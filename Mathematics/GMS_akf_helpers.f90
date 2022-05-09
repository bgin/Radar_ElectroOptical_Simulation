
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









end module akf_helpers
