
module akf_helpers

!================================================================================!
! Various helper subroutines for Kalman Filtering and Least-Squares Methods.
!
! Adapted from the book: 
! Advanced Kalman Filtering, Least-Squares and Modeling: A Practical Handbook
!
! Author(s): Bruce P. Gibbs
!
! Publisher: Wiley, Year: 2011
!
! ISBN: 0470529709,9780470529706,9780470890035,9780470890042
!** Author: B. Gibbs, 12/2009
!
!
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!********************************************************************************
! @Modified by Bernard Gingold on 08-05-2022 03:59 GMT+2, beniekg@gmail.com
!********************************************************************************
!================================================================================!

   ! Tab:5 col - Type and etc.. definitions
   ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds, only : i1,i4,sp,dp
    implicit none
    public

#if !defined(AKF_HELPERS_VERBOSE_ON)
#define AKF_HELPERS_VERBOSE_ON 0
#endif  

    interface cov2ud_m
        module procedure :: r8_cov2ud_m
        module procedure :: r4_cov2ud_m
    end interface cov2ud_m 

    interface rdx
        module procedure :: r4_rdx
        module procedure :: r8_rdx
    end interface rdx

    interface matInvGJ
        module procedure :: r4_matInvGJ
        module procedure :: r8_matInvGJ
    end interface matInvGJ

    contains

    subroutine bicgstab (ier,  xe,  a,b,n,nd)
      !dir$ attributes code_align : 32 :: bicgstab
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: bicgstab
      use omp_lib
!** preconditioned bi-conjugate gradient stabilized method for solving A * xe =b.
!** where A is n x n.  Note version assumes that matrix A is full.  For most problems of this
!** type matrix A is very sparse, so sparse matrix storage should be used for A and the
!** internal multiplications modified accordingly.
!** Reference: C.T. Kelley, Iterative Methods for Linear and Nonlinear Equations, SIAM, Philadelphia (1995)


      implicit none

      integer(i4),intent(in) :: n         !row and column dimension of A
      integer(i4),intent(in) :: nd        !row dimension of A used in the calling routine (nd >= n)
      real(dp),intent(in) :: a(nd,n)      !square n x n matrix
      real(dp),intent(in) :: b(n)         !n x 1 right-hand-side vector
      real(dp),intent(inout) :: xe(n)     !output n x 1 vector
      integer(i4),intent(out) :: ier      !error code: 0 =no error, 1=maximum iterations (200) reached

      integer(i4) i,j
      integer(i4) :: imax=200
      real(dp) d(n),hs(n,n),r(n),r0(n),rho0,rhol,rho,alpha,beta,w
      real(dp) p(n),v(n),s(n),t(n)
      !dir$ attribute align : 64 :: d
      !dir$ attribute align : 64 :: hs
      !dir$ attribute align : 64 :: r
      !dir$ attribute align : 64 :: r0
      !dir$ attribute align : 64 :: p
      !dir$ attribute align : 64 :: v
      !dir$ attribute align : 64 :: s
      !dir$ attribute align : 64 :: t
      real(dp) :: eps = 1.d-6             !tolerance test for convergence
      !_____________________________________

      ier =1
      !** compute pre-conditioning as diagonal
      !dir$ assume_aligned d:64
      !dir$ assume_aligned a:64
      !dir$ assume_aligned hs:64
      !dir$ ivdep
      !dir$ code_align(32)
       !$omp simd simdlen(8) linear(i:1)
      do i=1,n
        d(i)=sqrt(sum(a(:,i)**2))
        hs(:,i) =a(:,i)/d(i)
      enddo
      xe(:) =xe(:)/d(:)

      r(:) =b(:) -matmul(hs,xe)
      r0(:) =r(:)
      rho0 =sqrt(sum(b(:)**2))
      rhol =1.0_dp
      alpha =1.0_dp
      w =1.0_dp
      v(:) =0.0_dp
      p(:) =0.0_dp
      rho =dot_product(r0,r)
      !dir$ assume_aligned p:64
      !dir$ assume_aligned v:64
      !dir$ assume_aligned s:64
      !dir$ assume_aligned r:64
      !dir$ assume_aligned t:64
      !dir$ assume_aligned xe:64
      !dir$ ivdep
      do i=1,imax
        beta =(rho/rhol)*(alpha/w)
        p(:) =r(:) +beta*(p(:)-w*v(:))
        v(:) =matmul(hs,p)
        alpha =rho/dot_product(r0,v)
        s(:) =r(:) -alpha*v(:)
        t(:) =matmul(hs,s)
        w =dot_product(t,s)/sqrt(sum(t(:)**2))
        rho =w*dot_product(r0,t)
        xe(:) =xe(:) +alpha*p(:) +w*s(:)
        r(:) =s(:) -w*t(:)
        if (sqrt(sum(r(:)**2)) < eps*rho0) exit
        if (i >= imax) then
          ier =1
          return
        endif
      enddo

      ier =0
      xe(:) =xe(:)*d(:)

      end subroutine bicgstab


      subroutine cfactor(ier, a, n,eps)
        !dir$ attributes code_align : 32 :: cfactor
        !dir$ optimize : 3
        !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: cfactor

!** subroutine cfactor computes the Cholesky factor of a symmetric
!** positive definite matrix A, i.e., A =U^T * U where U is upper triangular.
!** Matrix A is stored as upper triangular by columns and the output U is stored in A.
!** If matrix A is singular at row i, the same row of U will be set to zero and
!** ier will be set to the first row that is found to be singular.  cfactor also tests
!** for a loss of precision or singularity and returns an error code indicating
!** at which row the loss occurred.

!** Author: B. Gibbs, 12/2009

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************


      implicit none

      integer(i4),intent(in) :: n               !Dimension of A
      real(dp),intent(in) :: eps                !Tolerance for loss of precision, e.g. eps =1d-4
      real(dp),intent(inout) :: a((n*(n+1))/2)  !Symmetric matrix to be factored, or output factor
!      real(8),intent(out) :: errm               !minimum remaining precision
      integer(i4),intent(out) :: ier            !Error flag: ier = -1 = n < 0, ier = -i means matrix
                                                ! is singular at row i, ier = +i, loss of precision
                                                ! exceeds eps at row i

      integer(i4) i,k,kpiv,ki,iflg,km1
      real(dp) tol,dsum,dpiv,work,wmax
      real(dp) errm   !minimum remaining precision
      !____________________________________________________

      iflg =0
!      if (ier == -2) iflg=1

      !**  test for invalid n
      if (n < 1) then
        ier =-1
        return
      endif

      ier =0
      kpiv =0          !initialize diagonal index
      wmax =0.d0

      do k=1,n         !row index
        kpiv =kpiv+k   !index of diagonal element of row
        ki =kpiv
        km1 =k-1
        !**  calculate tolerance for testing loss of significance
        tol =abs(eps*a(kpiv))

        !**   start factorization-loop over k-th row
        do i=k,n    !i is column index, ki is index of (k,i) element
          if (k > 1) then
            dsum =dot_product(a(kpiv-km1:kpiv-1),a(ki-km1:ki-1))
          else
            dsum =0.0_dp
          endif

          !**  compute difference for a(ki)
          dsum =a(ki)-dsum

          if (i == k) then
            !**  diagonal element: test for negative pivot element and for loss of significance
            wmax =max(wmax,abs(a(kpiv)/dsum))   !(a(kpiv)/dsum)**1.3 matches actual errors - WHY ??
            if (dsum <= tol) then
              if (dsum <= 0.d0) then
                write (6,'(/"MATRIX IS SINGULAR AT ROW ",I3)') i
                if (ier >= 0) ier =-i    !set for first row that is singular
                dpiv =0.0_dp              !set dpiv to zero elements in row k
                a(kpiv) =0.0_dp
                !dsum =1.d40  !when called by sinv, set diagonal to big number to get "pseudo-inverse" ?
                !return
              else
                work =log10(abs(a(kpiv)/dsum))
                write (6,100) k,work
  100           format(/'AT ROW',i5,',',f7.1,                           &
     &            ' DIGITS WERE LOST IN MATRIX FACTORIZATION')
                if (ier == 0) ier =k-1
              endif
            endif

            if (dsum > 0.0_dp) then
              !** compute pivot element
              a(kpiv) =sqrt(dsum)
              dpiv =1.d0/a(kpiv)
            endif

          else      !**  calculate terms in row
            a(ki) =dsum*dpiv
          endif

          ki =ki+i
        enddo       !____ end i loop
      enddo      !____ end k loop

!      errm =1.1d-16*sqrt(wmax)      !little difference between using max vs RSS
      errm =1.1d-16*wmax     !1.1d-16 is mantissa LSB (53) of IEEE S_floating on PC

     
      end subroutine cfactor

      subroutine cgnr (ier,xe,h,y,n,m,mmax)
          !dir$ attributes code_align : 32 :: cgnr
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: cgnr
          use omp_lib
!** preconditioned conjugate gradient method for solving the least squares normal equations
!** to minimize the residual.  The measurement equation is y =H*xe +r.
!** Least squares normal equations are xe = (H^T*H)^(-1) * H^T*y.
!** References:
!**    1) C.T. Kelley, Iterative Methods for Linear and Nonlinear Equations,
!**       SIAM, Philadelphia (1995),
!**    2) Å Björck, Numerical Methods for Least Squares Problems, SIAM, Philadelphia (1996)

!** Author: B. Gibbs, 12/2009

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      implicit none

      integer(i4),intent(in) :: n         !column dimension of matrix H
      integer(i4),intent(in) :: m         !number of actual measurements (rows) in H and y
      integer(i4),intent(in) :: mmax      !row dimension of H
      real(dp),intent(in) :: h(mmax,n)    !measurement partial matrix
      real(dp),intent(in) :: y(m)         !measurement vector
      real(dp),intent(inout) :: xe(n)     !state vector
      integer(i4),intent(out) :: ier      !error return: 0 =OK, 1 =not converged in 100 iterations

      integer(i4) i,j
      integer(i4) :: imax =100            !max allowed iterations
      real(dp) hs(m,n),r(m),q(m),rho0,tau,taul,alpha
      !dir$ attributes align : 64 :: hs
      !dir$ attributes align : 64 :: r
      !dir$ attributes align : 64 :: q
      real(dp) d(n),p(n),s(n)
      !dir$ attributes align : 64 :: d
      !dir$ attributes align : 64 :: p
      !dir$ attributes align : 64 :: s
      real(dp) :: eps =1.d-12
      !_____________________________________

      ier =0
      !** compute pre-conditioning as diagonal
      !dir$ assume_aligned d:64
      !dir$ assume_aligned h:64
      !dir$ assume_aligned hs:64
      !dir$ code_align(32)
      !$omp simd simdlen(8) linear(i:1)
      do i=1,n
        d(i) =sqrt(sum(h(:m,i)**2))
!        d(i) =1.d0      !### test (little difference)
        hs(:,i) =h(:m,i)/d(i)
      enddo
      xe(:) =xe(:)*d(:)

      r(:) =y(:)-matmul(hs,xe)
      s(:) =matmul(r(:),hs)
      p(:) =s(:)
      rho0 =sqrt(sum(s(:)**2))
      taul =1.0_dp
      !dir$ loop_count(100)
      do i=1,imax
        tau =sum(s(:)**2)
        if (i > 1) then
          p(:) =s(:) +(tau/taul)*p(:)
        endif
        taul =tau
        q(:) =matmul(hs,p)
        alpha=tau/sum(q(:)**2)
        xe(:)=xe(:)+alpha*p(:)
        s(:)=s(:)-alpha*matmul(q,hs)
!       write (6,'("cgnr: ",2i3,2g13.5)') n,i,sqrt(sum(s(:)**2)),rho0
        if (sqrt(sum(s(:)**2)) < eps*rho0) exit
        if (i >= imax) then
          ier =1
          xe(:) =xe(:)/d(:)
          return
        endif
      enddo

      xe(:) =xe(:)/d(:)

     
      end subroutine cgnr



      subroutine YW_spect (pwrDB,delf,g,x,nc,dt,nlag,nplot)
          !dir$ attributes code_align : 32 :: YW_spect
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: YW_spect
          use omp_lib
!***    YW_spect computes the power spectrum of real(4) data vector x
!***    using the Yule-Walker maximum entropy method (MEM).  The Yule-Walker normal
!***    equations, containing sample autocorrelation coefficients, are solved
!***    to estimate coefficients of an autoregressive (AR) prediction error filter.
!***    YW_spect first fits models and computes the Akaike final prediction error
!***    (FPE) for all model orders up to the maximum npmax.  It then re-computes the
!***    model for the order with minimum FPE and uses the AR coefficients to compute
!***    the power spectral density (PSD).

!***    Subroutine ywpr is adapted from Ulyrich and Bishop, Maximum Entropy Spectral Analysis and
!**     Autoregressive Decomposition", Rev. Geophysics and Space Physics, V13, pp. 183-200, Feb 1975

!**     Author: B. Gibbs, 12/2009

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      implicit none

      integer(i4),intent(in) :: nc      !number of data points in c
      integer(i4),intent(in) :: nlag    !# of lags for autocorrelation function.  nlag <= nc/2
      integer(i4),intent(in) :: nplot   !# of pwrDB values to be computed
      real(sp),intent(in) :: x(nc)      !input data vector of length n
      real(sp),intent(in) :: dt         !time step between points (sec)
      real(sp),intent(out) :: pwrDB(nplot)!power spectrum (dB) of x
      real(sp),intent(out) :: delf      !frequency increment for pwrDB

      integer(i4) n,k,i,j,isn,lext,lg,norder,ip
      real(sp) freq,dum,tspan,pmag,theta,psum
      real(sp) g(nc),phi(nc),fpe(nc),pm,fmax,xmean,xvar
      !dir$ attributes align : 64 :: g
      !dir$ attributes align : 64 :: phi
      !dir$ attributes align : 64 :: fpe
      real(dp), parameter :: pi = 3.1415926535897932384626_dp
      complex z,z1,p
      !________________________________________

      xmean =sum(x(:))/nc
      xvar =sum((x(:)-xmean)**2)/nc

      !** evaluate Yule-Walker AR model and FPE versus order
      lg =nlag     !max possible AR order
      lext =max(lg+1,nc/5)   !# of requested output autocorrelation coefficients in phi
      call ywpr (g,phi,fpe,pm,lg,lext,nc,x)
#if (AKF_HELPERS_VERBOSE_ON) == 1
      write (6,'("YW FPE =",5(/10g13.5))') fpe(:lg)
      write (6,'("YW g =",5(/10g13.5))') g(:lg)
      write (6,'("YW phi =",5(/10g13.5))') phi(:nlag+1)
#endif
      norder =minloc(fpe(2:lg),1)+1
#if (AKF_HELPERS_VERBOSE_ON) == 1
      write (6,'("YW-MEM optimal order =",i4)') norder
#endif
      !** recompute Yule-Walker AR model using the minimum FPE model order
      call ywpr (g,phi,fpe,pm,  norder,lext,nc,x)

      fmax =1.0_sp/(2.0_sp*dt)   !Nyquist frequency
      delf =fmax/nplot
      dum =2.0_sp*pm*dt       !factor 2 to account for negative frequencies
!     write (6,'(i3,", dt,tspan,dum =",4g13.5)')                        &
!    &                n,dt,tspan,dum
      psum =0.0_sp
      !dir$ assume_aligned g:64
      !dir$ assume_aligned pwrDB:64
      !dir$ code_align(32)
      !$omp simd simdlen(4) linear(i:1)
      do ip=1,nplot
        theta =(ip-1)*delf*2.0_dp*pi*dt
        z=cmplx(cos(theta),-sin(theta))

        !** compute z-transform of
        z1=cmplx(1.,0.)
        p=g(1)
        !dir$ code_align(32)
        !$omp simd reduction(+:z1) reduction(+:p)
        do i=2,norder
          z1=z1*z
          p=p+g(i)*z1
        enddo
        pmag =dum/(real(p)**2 +aimag(p)**2)
        psum =psum +pmag
        pwrDB(ip) =10.*log10(pmag)
      enddo
!     write (6,'("pwr(:) =",10(/10g13.5))') pwr(:)
      write (6,'("data variance =",g13.5,", YW summed power =",g13.5)') &
     &            xvar,psum*delf  !checks

      
      end subroutine YW_spect
! **********************************************************************
      subroutine ywpr (g,phi,fpe,pm,lg,lext,n,x)
          !dir$ attributes forceinline :: ywpr
          !dir$ attributes code_align : 32 :: ywpr
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: ywpr
!***   compute Yule-Walker power spectrum of x using AIC FPE to
!***   select model order (see Ulrych and Bishop, 1975)
      implicit none

      integer(i4),intent(in) :: n      !number of data points in x
      integer(i4),intent(in) :: lg     !# of lags used for computing direct autocorrelation
                                       ! function and fpe, i.e., max order of AR model
      integer(i4),intent(in) :: lext   !number of output correlation coeff (lext > lg)
      real(sp),intent(in) :: x(n)      !input data vector of length n

      real(sp),intent(out) :: g(lg)    !prediction error coefficients
      real(sp),intent(out) :: fpe(lg)  !final prediction error versus AR order
      real(sp),intent(out) :: phi(lext)!computed autocorrelation coefficients up to lag lext (lext > lg)
      real(sp),intent(out) :: pm       !updated variance

      integer(i4) i,j,k,nn
      real(dp) h(n),dphi(n),suma,vp,dp,ftemp,phi8(lext),g8(lg),xmean
      !dir$ attributes align : 64 :: h
      !dir$ attributes align : 64 :: dphi
      !dir$ attributes align : 64 :: phi8
      !dir$ attributes align : 64 :: g8
      !__________________________________________________________

      if (lext < lg) stop 'ywpr error: lext < lg'
      !dir$ assume_aligned x:64
      xmean =sum(x(:n))/n

      !** compute autocorrelation function phi
      !dir$ novector
      !dir$ assume_aligned x:64
      !dir$ assume_aligned phi8:64
      do i=1,lg
        j=min(n+i-1,n)-i+1
        phi8(i)=dot_product(x(i:i+j-1)-xmean,x(1:j)-xmean)/n
      enddo
      dphi(:lg)=phi8(:lg)

      g8(1)=1.0_dp
      g8(2)=-dphi(2)/dphi(1)
      ftemp=(real(n+1)/real(n-1))*phi8(1)     !scaled variance of x
      fpe(1) =0.0_sp

      !** recursively compute AR coefficients and fpe vs model order
      
      do nn=2,lg
        vp=dot_product(g8(1:nn),dphi(1:nn))
        dp=0.
        !dir$ assume_aligned g8:64
        !dir$ assume_aligned dphi:64
        !dir$ code_align:32
        !$omp simd simdlen(8) reduction(+:dp)
        do i=1,nn
          dp=dp+g8(nn+1-i)*dphi(i+1)   !dot_product(g(nn:1:-1),dphi(2:n+1))
        enddo
        pm=vp
        if(n /= nn) then
          fpe(nn)=(real(n+nn)/real(n-nn))*vp/ftemp
          fpe(nn)=log10(fpe(nn))
        endif

        if(nn == lg) exit
        g8(nn+1)=-dp/vp
        forall (i=2:nn) h(i)=g8(i)+g8(nn+1)*g8(nn+2-i)
        g8(2:nn)=h(2:nn)
      enddo

      !** compute extended autocorrelation function
      !dir$ assume_aligned dphi:64
      !dir$ assume_aligned g8:64
      !dir$ assume_aligned phi:64
       do j=lg+1,lext
        suma =0.
        !$omp simd simdlen(8) reduction(+:suma)
        do i=2,lg
          suma=suma-dphi(j+1-i)*g8(i)
        enddo
        dphi(j)=suma
        phi8(j)=suma
      enddo

      phi(:)=phi8(:)
      g(:)=g8(:)

   
      end subroutine ywpr


      subroutine r8_cov2ud_m (u,n)
          !dir$ attributes forceinline :: r8_cov2ud_m
          !dir$ attributes code_align : 32 :: r8_cov2ud_m
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r8_cov2ud_m
          
!
!        Computes the U-D factors of a positive semi-definite input matrix
!        (upper triangular by columns U). The input vector stored
!        matrix is overwritten by the output U-D factors, which are also vector stored.
!        Singular input covariances result in output matrices with zero columns

!        This is a modification of the JPL ESP routine COV2UD written by
!        G.J. Bierman & R.A.Jacobson (feb. 1977)

!        Modifications in the cov2ud_m version by B. Gibbs 8/27/09
!         1) upgrading to Fortran 90 syntax,
!         2) specifying input/output INTENT on all calling arguments,
!         3) using a separate module to specify whether arithmetic operations are single
!            or double precision

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      
      implicit  none

      integer(i4),intent(in) :: n                     !matrix dimension, n >= 2
      real(dp),intent(inout) :: u(n*(n+1)/2) !input vector stored covariance matrix.
                                                      ! on output it contains the vector
                                                      ! stored u-d covariance factors.

      integer(i4) i,j,k,l,none,np2,jj,ij,ik,kj,kk,jm1
      real(dp) zero,one,alpha,beta
      !_______________________________________________

      zero =0.0_dp
      one =1.0_dp
      none =1

      jj =n*(n+1)/2
      np2 =n+2
      !dir$ assume_aligned u:64
      do l=2,n
        j =np2-l
        alpha =zero
        if (u(jj) < zero) then
#if (AKF_HELPERS_VERBOSE_ON) == 1
          write (6,100) j,u(jj)
#endif
          u(jj) =zero
        endif
        if (u(jj) > zero) alpha =one/u(jj)
        jj =jj-j
        kk =0
        kj =jj
        jm1 =j-1
        !dir$ novector
        do k=1,jm1
          kj=kj+1
          beta=u(kj)
          u(kj)=alpha*u(kj)
          ij=jj
          ik=kk
          do i=1,k
            ik=ik+1
            ij=ij+1
            u(ik)=u(ik)-beta*u(ij)
          enddo
          kk=kk+k
        enddo
      enddo  !___ end l loop

      if (u(1) < zero) then
#if (AKF_HELPERS_VERBOSE_ON) == 1
        write (6,100) none, u(1)
#endif
        u(1) =zero
      endif

      return

  100 format (10x,'in cov2ud at step',i4,', diagonal entry =',e12.4)
      end subroutine 

      subroutine r4_cov2ud_m (u,n)
          !dir$ attributes forceinline :: r4_cov2ud_m
          !dir$ attributes code_align : 32 :: r4_cov2ud_m
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r4_cov2ud_m
          
!
!        Computes the U-D factors of a positive semi-definite input matrix
!        (upper triangular by columns U). The input vector stored
!        matrix is overwritten by the output U-D factors, which are also vector stored.
!        Singular input covariances result in output matrices with zero columns

!        This is a modification of the JPL ESP routine COV2UD written by
!        G.J. Bierman & R.A.Jacobson (feb. 1977)

!        Modifications in the cov2ud_m version by B. Gibbs 8/27/09
!         1) upgrading to Fortran 90 syntax,
!         2) specifying input/output INTENT on all calling arguments,
!         3) using a separate module to specify whether arithmetic operations are single
!            or double precision

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      
      implicit  none

      integer(i4),intent(in) :: n                     !matrix dimension, n >= 2
      real(sp),intent(inout) :: u(n*(n+1)/2) !input vector stored covariance matrix.
                                                      ! on output it contains the vector
                                                      ! stored u-d covariance factors.

      integer(i4) i,j,k,l,none,np2,jj,ij,ik,kj,kk,jm1
      real(sp) zero,one,alpha,beta
      !_______________________________________________

      zero =0.0_sp
      one =1.0_sp
      none =1

      jj =n*(n+1)/2
      np2 =n+2
      !dir$ assume_aligned u:64
      do l=2,n
        j =np2-l
        alpha =zero
        if (u(jj) < zero) then
#if (AKF_HELPERS_VERBOSE_ON) == 1
          write (6,100) j,u(jj)
#endif
          u(jj) =zero
        endif
        if (u(jj) > zero) alpha =one/u(jj)
        jj =jj-j
        kk =0
        kj =jj
        jm1 =j-1
        !dir$ novector
        do k=1,jm1
          kj=kj+1
          beta=u(kj)
          u(kj)=alpha*u(kj)
          ij=jj
          ik=kk
          do i=1,k
            ik=ik+1
            ij=ij+1
            u(ik)=u(ik)-beta*u(ij)
          enddo
          kk=kk+k
        enddo
      enddo  !___ end l loop

      if (u(1) < zero) then
#if (AKF_HELPERS_VERBOSE_ON) == 1
        write (6,100) none, u(1)
#endif
        u(1) =zero
      endif

      return

  100 format (10x,'in cov2ud at step',i4,', diagonal entry =',e12.4)
      end subroutine 



      subroutine marple_spect (pwrDB,delf,nc,mmax,nplot,x,dt,tol1,tol2)
          !dir$ attributes code_align : 32 :: marple_spect
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: marple_spect
          use omp_lib
!***    marple_spect computes the power spectrum of real(4) data vector x
!***    using the Marple (1980) AR modeling method.
!**     "A New Autogregressive Spectral Analysis Algorithm", IEEE Trans on Acoustics,
!**     Speech and Signal Processing, ASSP-28, pp. 441-454 (Aug. 1980)

!**     Author: B. Gibbs, 1/2010

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
      implicit none

      integer(i4),intent(in) :: nc      !number of data points in c
      integer(i4),intent(in) :: mmax    !maximum AR order.  mmax <= nc/2
      integer(i4),intent(in) :: nplot   !# of pwrDB values to be computed
      real(sp),intent(in) :: x(nc)      !input data vector of length n
      real(sp),intent(in) :: dt         !time step between points (sec)
      real(sp),intent(in) :: tol1       !0.01 to 0.001 use 0.01 for example
      real(sp),intent(in) :: tol2       !0.01 to 0.001 use 0.002 for example
      real(sp),intent(out) :: pwrDB(nplot)!power spectrum (dB) of x
      real(sp),intent(out) :: delf      !frequency increment for pwrDB

      integer(i4) i,norder,ip,status
      real(sp) dum,pmag,theta,psum,pvar,xm(nc)
      !dir$ attributes align : 64 :: xm
      real(sp) g(mmax),pm,fmax,xmean,xvar,e,e0
      !dir$ attributes align : 64 :: g
      real(dp), parameter :: pi = 3.1415926535897932384626_dp
      complex z,z1,p
      !________________________________________
      
      !dir$ assume_aligned x:64
      !dir$ assume_aligned xm:64
      xmean=sum(x(:))/nc
      xm(:)=x(:)-xmean
      xvar=sum(xm(:)**2)/nc

      !** evaluate Marple AR model
      call lstsqs (g,e,e0,norder,status,  nc,mmax,xm,tol1,tol2)
      pvar=e0/(2.0_dp*nc)
      pm=e/(2.0_dp*nc)
#if (AKF_HELPERS_VERBOSE_ON) == 1
      write (6,'("marple_spect lstsqs status =",i3)') status
#endif
      if (status < 4) then
#if (AKF_HELPERS_VERBOSE_ON) == 1
        write (6,'("marple_spect error: status =",i3)') status
#endif
        return
      endif
#if (AKF_HELPERS_VERBOSE_ON) == 1
      write (6,'("Marple data variance =",g13.5,                        &
     &     ", prediction error variance =",g13.5)') pvar,e/(2.*nc)
      write (6,'("Marple g =",5(/10g13.5))') g(:norder)
      write (6,'("Marple optimal order =",i4)') norder
#endif
      fmax=1.0_dp/(2.0_dp*dt)   !Nyquist frequency
      delf=fmax/nplot
      dum=2.0_sp*pm*dt       !factor 2 to account for negative frequencies
!     write (6,'(i3,", dt,tspan,dum =",4g13.5)')                        &
!    &                n,dt,tspan,dum
      psum=0.
      !dir$ assume_aligned g:64
      !dir$ assume_aligned pwrDB:64
      !dir$ code_align:32
      !$omp simd simdlen(4) linear(ip:1)
      do ip=1,nplot
        theta =(ip-1)*delf*2.0*pi*dt
        z=cmplx(cos(theta),-sin(theta))

        !** compute z-transform of
        z1=cmplx(1.,0.)
        p=1.0_dp !g(1)       !note difference in AR definition from Ulrich and Bishop
        !$omp simd simdlen(4) reduction(*:z1) reduction(+:p)
        do i=1,norder
          z1=z1*z
          p=p+g(i)*z1
        enddo
        pmag=dum/(real(p)**2 +aimag(p)**2)
        psum=psum+pmag
        pwrDB(ip)=10.*log10(pmag)
      enddo
!     write (6,'("pwr(:) =",10(/10g13.5))') pwr(:)
#if (AKF_HELPERS_VERBOSE_ON) == 1
      write (6,'("data variance =",g13.5,                               &
     &        ", Marple summed power =",g13.5)') xvar,psum*delf  !checks
#endif
    
      end subroutine 
!***********************************************************************************
      subroutine lstsqs (a,e,e0,m,status,n,mmax,x,tol1,tol2)
          !dir$ attributes code_align : 32 :: lstsqs
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: lstsqs
          use omp_lib
!**  lstsqs.f uses L. Marple's algorithms to compute parameters of an autoregressive model
!**  "A New Autogregressive Spectral Analysis Algorithm", IEEE Trans on Acoustics,
!**   Speech and Signal Processing, ASSP-28, pp. 441-454 (Aug. 1980)

      implicit none
      integer(i4),intent(in) :: n   !number of data samples
      integer(i4),intent(in) :: mmax!max allowed model order
      real(sp),intent(in) :: x(n)   !data samples
      real(sp),intent(in) :: tol1   !tolerance factor that stops recursion at order
                                    ! m when e(m)/e(0) < tol1
      real(sp),intent(in) :: tol2   !tolerance factor that stops recursion at order
                                    ! m when (e(m)-e(m-1)/e(m-1) < tol2

      integer(i4),intent(out) :: m  !model order at exit
      integer(i4),intent(out) :: status!error status: 1 =not converged at maximum order,
                                    ! 2 =, 3 = 4 =, 5 =OK
      real(sp),intent(out) :: e0     !total signal energy
      real(sp),intent(out) :: e      !prediction error energy (forward and backward) at order m
      real(sp),intent(out) :: a(mmax)!array of m autoregressive coefficients

      integer(i4) i,j,k,n1,nm,m1,k1,nk,m2,mk
      real(dp) c(mmax+1),d(mmax+1),r(mmax+1),f,b,h,s,u,v
      !dir$ attributes align : 64 :: c
      !dir$ attributes align : 64 :: d
      !dir$ attributes align : 64 :: r
      real(dp) save1,save2,save3,save4,delta,q1,q2,c1,c2,c3,c4,c5,c6
      real(dp) g,w,den,q3,q4,q5,q6,q7,alpha,eold,y1,y2,y3,y4
      !___________________________________________________________________

!**     initialization
      !dir$ assume_aligned x:64
      e0=2.*sum(x(:n)**2)
      q1=1.0_dp/e0
      q2=q1*x(1)
      g=q1*x(1)**2
      w=q1*x(n)**2
      den=1.d0-g-w
      q4=1.d0/den
      q5=1.d0-g
      q6=1.d0-w
      f=x(1)
      b=x(n)
      h=q2*x(n)
      s=q2*x(n)
      u=q1*x(n)**2
      v=q2*x(1)
      e=e0*den
      q1=1.d0/e
      c(1)=q1*x(1)
      d(1)=q1*x(n)
      save1=0.0_dp
      n1=n+1
      nm=n-1
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ code_align:32
      !$omp simd reduction(+:save1)
      do k=1,nm
        save1=save1 +x(k+1)*x(k)
      enddo
      r(1)=2.0_dp*save1
      a(1)=-q1*r(1)
      e=e*(1.0_dp-a(1)**2)

!**     loop for AR order
      m=1
      do
        !** prediction filter error update
        eold=e
        m1=m+1
        f=x(m1)
        b=x(nm)
        !dir$ assume_aligned x:64
        !dir$ assume_aligned a:64
        do k=1,m
          f=f+x(m1-k)*a(k)
          b=b+x(nm+k)*a(k)
        enddo

        !** auxilliary vectors order update
        q1=1.0_dp/e
        q2=q1*f
        q3=q1*b
        !dir$ assume_aligned c:64
        !dir$ assume_aligned d:64
        !dir$ assume_aligned a:64
        do k=m,1,-1
          k1=k+1
          c(k1)=c(k) +q2*a(k)
          d(k1)=d(k) +q3*a(k)
        enddo
        c(1)=q2
        d(1)=q3

        !** scalar order update
        q7 =s**2
        y1 =f**2
        y2 =v**2
        y3 =b**2
        y4 =u**2
        g =g+y1*q1+q4*(y2*q6 +q7*q5 +2.0_dp*v*h*s)
        w =w+y3*q1+q4*(y4*q5 +q7*q6 +2.0_dp*s*h*u)
        h =0.0_dp
        s =0.0_dp
        u =0.0_dp
        v =0.0_dp
        
        do k=0,m
          k1=k+1
          nk=n-k
          h=h+x(nm+k)*c(k1)
          s=s+x(nk)*c(k1)
          u=u+x(nk)*d(k1)
          v=v+x(k1)*c(k1)
        enddo

        !** denominator update
        q5=1.0_dp-g
        q6=1.0_dp-w
        de =q5*q6-h**2
        if (den <= 0.0_dp) then
          status=2
          return
        endif

        !** time shift variables update
        q4=1.0_dp/den
        q1=q1*q4
        alpha=1.0_dp/(1.0_dp+(y1*q6+y3*q5+2.0_dp*(h*f*b))*q1)
        e=alpha*e
        c1=q4*(f*q6+b*h)
        c2=q4*(b*q5+h*f)
        c3=q4*(v*q6+h*s)
        c4=q4*(s*q5+v*h)
        c5=q4*(s*q6+h*u)
        c6=q4*(u*q5+s*h)
        do k=1,m
          k1 =k+1
          a(k) =alpha*(a(k)+c1*c(k1)+c2*d(k1))
        enddo
        m2 =m/2+1
        do k=1,m2
          mk =m+2-k
          save1 =c(k)
          save2 =d(k)
          save3 =c(mk)
          save4 =d(mk)
          c(k) =c(k) +c3*save3+c4*save4
          d(k) =d(k) +c5*save3+c6*save4
          if (mk /= k) then
            c(mk) =c(mk) +c3*save1 +c4*save2
            d(mk) =d(mk) +c5*save1 +c6*save2
          endif
        enddo

        if (m >= mmax) then
          status =1
          return
        endif

        !** order update
        m=m+1
        nm=n-m
        m1=m-1
        delta=0.0_dp
        c1=x(n1-m)
        c2=x(m)
        do k=m1,1,-1
          r(k+1)=r(k) -x(n1-k)*c1 -x(k)*c2
          delta=delta +r(k+1)*a(k)
        enddo
        save1=0.0_dp
        !dir$ assume_aligned x:64
        !$omp simd reduction(+:save1)
        do k=1,nm
          save1=save1 +x(k+m)*x(k)
        enddo
        r(1)=2.0_dp*save1
        delta=delta+r(1)
        q2 =-delta/e
        a(m)=q2
        m2=m/2
        do k=1,m2
          mk =m-k
          save1 =a(k)
          a(k) =a(k) +q2*a(mk)
          if (k /= mk) a(mk) =a(mk) +q2*save1
        enddo

        y1=q2**2
        e=e*(1.0_dp-y1)

        if (y1 >= 1.d0) then
          status =3
          return
        endif
        if (e < e0*tol1) then
          status =4    !normal return
          return
        endif
        if ((eold-e) < eold*tol2) then
          status =5    !normal return
          return
        endif
      enddo   !___ end order loop

     
      end subroutine 


 subroutine r4_rdx(del,r,dx,n)
          !dir$ attributes forceinline :: r4_rdx
          !dir$ attributes code_align : 32 :: r4_rdx
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r4_rdx
!        rdx computes del =r*dx where r is the upper
!        triangular square-root information matrix

!        B. Gibbs 8/27/09

! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.

      
      implicit none

      integer(i4),intent(in) :: n                  !state dimension
      real(sp),intent(in) :: r(n*(n+1)/2) !srif r matrix
      real(sp),intent(in) :: dx(n)        !vector
      real(sp),intent(out) :: del(n)      !computed solution

      integer(i4) i,j,k
      real(sp), automatic :: rsum
      !_______________________________________________

      k=0
      !dir$ assume_aligned r:64
      !dir$ assume_aligned dx:64
      !dir$ assume_aligned del:64
      !dir$ novector
      do i=1,n
        rsum =0.0_sp
        k=(i*(i+1))/2
        do j=i,n
          rsum=rsum +r(k)*dx(j)
          k=k+j
        enddo
        del(i)=rsum
      enddo
   
      end subroutine 


      subroutine r8_rdx(del,r,dx,n)
          !dir$ attributes forceinline :: r8_rdx
          !dir$ attributes code_align : 32 :: r8_rdx
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r8_rdx

!        rdx computes del =r*dx where r is the upper
!        triangular square-root information matrix

!        B. Gibbs 8/27/09

! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.

      
      implicit none

      integer(i4),intent(in) :: n                  !state dimension
      real(dp),intent(in) :: r(n*(n+1)/2) !srif r matrix
      real(dp),intent(in) :: dx(n)        !vector
      real(dp),intent(out) :: del(n)      !computed solution

      integer(i4) i,j,k
      real(dp), automatic :: rsum
      !_______________________________________________

      k=0
      !dir$ assume_aligned r:64
      !dir$ assume_aligned dx:64
      !dir$ assume_aligned del:64
      !dir$ novector
      do i=1,n
        rsum =0.0_dp
        k=(i*(i+1))/2
        do j=i,n
          rsum=rsum +r(k)*dx(j)
          k=k+j
        enddo
        del(i)=rsum
      enddo
   
     end subroutine 


   
    subroutine r4_matInvGJ(ierr,a,b,np,n,m)
          !dir$ attributes code_align : 32 :: r4_matInvGJ
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r4_matInvGJ
          use omp_lib
!**  matrix inversion and linear equation solver using Gauss-Jordan elimination
!**  Based on gaussj in Numerical Recipes in Fortran, Version 2

!** Author: B. Gibbs, 12/2009

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
     
      implicit none

      integer(i4),intent(in) :: np        !row dimension of a and b (np >= n)
      integer(i4),intent(in) :: n         !number of rows and columns in a
      integer(i4),intent(in) :: m         !number of columns in b
      real(sp),intent(inout) :: a(np,n)!matrix to be inverted, and inverse
      real(sp),intent(inout) :: b(np,m)!right-hand-side matrix to be multiplied: b=inv(a)*b
      integer(i4),intent(out) :: ierr     !error return flag. 0=no error, 1=matrix singular

!****  local
      integer(i4) i,j,k,icol,irow,indxc(n),indxr(n),ipiv(n)
      !dir$ attributes align : 64 :: indxc
      !dir$ attributes align : 64 :: indxr
      !dir$ attributes align : 64 :: ipiv
      real(sp) biga,atmp,pivinv,atmp1(max(n,m))
      !dir$ attributes align : 64 :: atmp1
      !____________________________________________________________

      ierr =0
      ipiv(:) =0

      !***  compute pivot vector
      do i=1,n
        biga =0.0_sp

        do j=1,n
          if (ipiv(j) /= 1) then
            do k=1,n
              if (ipiv(k) == 0) then
                if (abs(a(j,k)) >= biga) then
                  biga =abs(a(j,k))
                  irow =j
                  icol =k
                endif
              else if (ipiv(k) > 1) then
                ierr =1       !stop 'matInvGJ error: singular matrix'
                return
              endif
            enddo
          endif
        enddo

        ipiv(icol) =ipiv(icol)+1

        !*** interchange rows if needed to put the pivot element on diagonal.
        !*** columns are relabeled, not physicaly interchanged
        !dir$ assume_aligned atmp1:64
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        if (irow /= icol) then
          atmp1(:n)=a(irow,:)
          a(irow,:)=a(icol,:)
          a(icol,:)=atmp1(:n)
          atmp1(:m)=b(irow,:)
          b(irow,:)=b(icol,:)
          b(icol,:)=atmp1(:m)
        endif

        indxr(i)=irow
        indxc(i)=icol

        if (a(icol,icol) == 0.0_sp) then
          ierr=1       !stop 'matInvGJ error: singular matrix'
          return
        endif
        pivinv =1.0_sp/a(icol,icol)
        a(icol,icol)=1.0_sp
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        a(icol,:)=a(icol,:)*pivinv
        b(icol,:)=b(icol,:)*pivinv

        !*** reduce the rows, except for pivot
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        !dir$ code_align:32
        do j=1,n
          if (j /= icol) then
            atmp =a(j,icol)
            a(j,icol)=0.0_sp
            a(j,:)=a(j,:)-a(icol,:)*atmp
            b(j,:)=b(j,:)-b(icol,:)*atmp
          endif
        enddo
      enddo!___ end do i loop

      !** now reset column order to original
      !dir$ assume_aligned a:64
      !dir$ assume_aligned indxc:64
      !dir$ assume_aligned indxr:64
      !dir$ code_align:32
      do j=n,1,-1
        if (indxr(j) /= indxc(j)) then
          !dir$ code_align:32
          !$omp simd simdlen(4) private(atmp)
          do k=1,n
            atmp =a(k,indxr(j))
            a(k,indxr(j)) =a(k,indxc(j))
            a(k,indxc(j)) =atmp
          enddo
        endif
      enddo

    end subroutine 


    subroutine r8_matInvGJ(ierr,a,b,np,n,m)
          !dir$ attributes code_align : 32 :: r8_matInvGJ
          !dir$ optimize : 3
          !dir$ attributes optimization_parameter: "TARGET_ARCH=skylake_avx512" :: r8_matInvGJ
          use omp_lib
!**  matrix inversion and linear equation solver using Gauss-Jordan elimination
!**  Based on gaussj in Numerical Recipes in Fortran, Version 2

!** Author: B. Gibbs, 12/2009

!**************************************
! The author grants the user a non-exclusive, worldwide, royalty-free copyright license to

! 1. reproduce and modify the software for your own purposes, and
! 2. to distribute the software provided that you give credit to the author,
!    do not make claims against the author or Wiley-Interscience,
!    and mark any modifications as your own.  If the software is incorporated in
!    a commercial product, you  agree to defend and indemnify this author and
!    Wiley-Interscience against any losses, damages and costs arising from claims,
!    lawsuits and other legal actions brought by a third party.

! The software is provided on an as is basis, without warranties or conditions
! of any kind, either express or implied (including any warranties or conditions
! of title, non-infringement, merchantability or fitness for a particular purpose).
! The user is solely responsible for determining the appropriateness of using and
! distributing the program and assumes all risks associated with its exercise of
! rights under this agreement.
!**************************************
     
      implicit none

      integer(i4),intent(in) :: np        !row dimension of a and b (np >= n)
      integer(i4),intent(in) :: n         !number of rows and columns in a
      integer(i4),intent(in) :: m         !number of columns in b
      real(dp),intent(inout) :: a(np,n)!matrix to be inverted, and inverse
      real(dp),intent(inout) :: b(np,m)!right-hand-side matrix to be multiplied: b=inv(a)*b
      integer(i4),intent(out) :: ierr     !error return flag. 0=no error, 1=matrix singular

!****  local
      integer(i4) i,j,k,icol,irow,indxc(n),indxr(n),ipiv(n)
      !dir$ attributes align : 64 :: indxc
      !dir$ attributes align : 64 :: indxr
      !dir$ attributes align : 64 :: ipiv
      real(dp) biga,atmp,pivinv,atmp1(max(n,m))
      !dir$ attributes align : 64 :: atmp1
      !____________________________________________________________

      ierr =0
      ipiv(:) =0

      !***  compute pivot vector
      do i=1,n
        biga =0.0_dp

        do j=1,n
          if (ipiv(j) /= 1) then
            do k=1,n
              if (ipiv(k) == 0) then
                if (abs(a(j,k)) >= biga) then
                  biga =abs(a(j,k))
                  irow =j
                  icol =k
                endif
              else if (ipiv(k) > 1) then
                ierr =1       !stop 'matInvGJ error: singular matrix'
                return
              endif
            enddo
          endif
        enddo

        ipiv(icol) =ipiv(icol)+1

        !*** interchange rows if needed to put the pivot element on diagonal.
        !*** columns are relabeled, not physicaly interchanged
        !dir$ assume_aligned atmp1:64
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        if (irow /= icol) then
          atmp1(:n)=a(irow,:)
          a(irow,:)=a(icol,:)
          a(icol,:)=atmp1(:n)
          atmp1(:m)=b(irow,:)
          b(irow,:)=b(icol,:)
          b(icol,:)=atmp1(:m)
        endif

        indxr(i)=irow
        indxc(i)=icol

        if (a(icol,icol) == 0.0_dp) then
          ierr=1       !stop 'matInvGJ error: singular matrix'
          return
        endif
        pivinv =1.0_dp/a(icol,icol)
        a(icol,icol)=1.0_dp
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        a(icol,:)=a(icol,:)*pivinv
        b(icol,:)=b(icol,:)*pivinv

        !*** reduce the rows, except for pivot
        !dir$ assume_aligned a:64
        !dir$ assume_aligned b:64
        !dir$ code_align:32
        do j=1,n
          if (j /= icol) then
            atmp =a(j,icol)
            a(j,icol)=0.0_dp
            a(j,:)=a(j,:)-a(icol,:)*atmp
            b(j,:)=b(j,:)-b(icol,:)*atmp
          endif
        enddo
      enddo!___ end do i loop

      !** now reset column order to original
      !dir$ assume_aligned a:64
      !dir$ assume_aligned indxc:64
      !dir$ assume_aligned indxr:64
      !dir$ code_align:32
      do j=n,1,-1
        if (indxr(j) /= indxc(j)) then
          !dir$ code_align:32
          !$omp simd simdlen(4) private(atmp)
          do k=1,n
            atmp =a(k,indxr(j))
            a(k,indxr(j)) =a(k,indxc(j))
            a(k,indxc(j)) =atmp
          enddo
        endif
      enddo

    end subroutine 




      subroutine errhand (message)
        !dir$ attributes forceinline :: errhand
      character(*),intent(in) :: message
      write (6,'(a)') trim(message)
      write (*,'(a)') trim(message)
      stop "error"
      end subroutine errhand



end module akf_helpers
