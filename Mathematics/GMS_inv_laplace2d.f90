

module inv_laplace2d

  !==============================================================================================!
  !
  ! Reference:
  !                         H. Stehfest, Algorithm 368, Numerical Inversion of Laplace 
  !                         Transforms, CACM Vol. 13, No.1, 47-49 (1970)
  !	                    J. Abate, W. Whitt, A Unified Approach for Numerically Inverting 
  !	                    Laplace Transforms, INFORNS Journal of Computing Vol 18, No. 4,
  !	                    408-421 (2006) 
  !
  !==============================================================================================!
  use mod_kinds, only : i4,sp,dp
  implicit none
  public

  abstract interface

     real(kind=dp) function Fx_r8(a,b)
       import :: dp
       implicit none
       real(kind=dp), intent(in) :: a
       real(kind=dp), intent(in) :: b
     end function Fx_r8

     real(kind=sp) function Fx_r4(a,b)
       import :: sp
       implicit none
       real(kind=sp), intent(in) :: a
       real(kind=dp), intent(in) :: b
     end function Fx_r4
     

  end interface

  contains

  
    subroutine gaver_stehfest_r8_1(Fx,y2d,al,om,t,    &
                                   c1,c2,nappr,ncoef, &
                                   niters)
      !dir$ optimize:3
      !dir$ attributes code_align : 32 :: gaver_stehfest_r8_1
      !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: gaver_stehfest_r8_1
      use omp_lib
      implicit none
      procedure(Fx_r8) :: Fx
      real(kind=dp), dimension(:,:), intent(out) :: y2d
      real(kind=dp), dimension(:),   intent(in)  :: al
      real(kind=dp), dimension(:),   intent(in)  :: om
      real(kind=dp), dimension(:),   intent(in)  :: t
      real(kind=dp),                 intent(in)  :: c1
      real(kind=dp),                 intent(in)  :: c2
      integer(kind=i4),              intent(in)  :: nappr
      integer(kind=i4),              intent(in)  :: ncoef
      integer(kind=i4),              intent(out) :: niters
      ! Locals
      real(kind=dp),    automatic :: ti,tj,sum1,sum2
      real(kind=dp),    automatic :: ei,ej,omii,t0
      integer(kind=i4), automatic :: i,j,ii,jj,n
      n=ncoef+ncoef
      ei=0.0_dp
      ej=ei
      omii=ei
      !dir$ assume_aligned y2d:64
      !dir$ assume_aligned al:64
      !dir$ assume_aligned om:64
      !dir$ assume_aligned t:64
!$omp parallel do default(none) schedule(runtime) &
!$omp firstprivate(ei,ej,omii)                    &
!$omp private(i,ti,j,tj,ii,jj,sum1,sum2)          &
!$omp shared(nappr,nc1,c2,n,y2d,al,om,t)
      do i=1,nappr
         ti=t(i)
         ei=exp(ti*c1)
         do j=1,nappr
            sum1=0.0_dp
            tj=t(j)
            ej=exp(tj*c2)
            !$omp simd reduction(+:sum1)
            do ii=1,n
               sum2=0.0_dp
               omii=om(ii)
               t0=c1+al(ii)/ti
               !$omp simd reduction(+:sum2)
               do jj=1,n
                  sum2 = sum2+om(jj)*Fx(t0,c2+al(jj)/tj)
               end do
               sum1 = sum1+omii*sum2
            end do
            y2d(j,i) = ei*ej*sum1
         end do
      end do
!$omp end parallel do
      niters = nappr*nappr*n*n
    end subroutine gaver_stehfest_r8_1
      

    subroutine gaver_stehfest_r4_1(Fx,y2d,al,om,t,    &
                                   c1,c2,nappr,ncoef, &
                                   niters)
      !dir$ optimize:3
      !dir$ attributes code_align : 32 :: gaver_stehfest_r4_1
      !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: gaver_stehfest_r4_1
      use omp_lib
      implicit none
      procedure(Fx_r4) :: Fx
      real(kind=sp), dimension(:,:), intent(out) :: y2d
      real(kind=sp), dimension(:),   intent(in)  :: al
      real(kind=sp), dimension(:),   intent(in)  :: om
      real(kind=sp), dimension(:),   intent(in)  :: t
      real(kind=sp),                 intent(in)  :: c1
      real(kind=sp),                 intent(in)  :: c2
      integer(kind=i4),              intent(in)  :: nappr
      integer(kind=i4),              intent(in)  :: ncoef
      integer(kind=i4),              intent(out) :: niters
      ! Locals
      real(kind=sp),    automatic :: ti,tj,sum1,sum2
      real(kind=sp),    automatic :: ei,ej,omii,t0
      integer(kind=i4), automatic :: i,j,ii,jj,n
      n=ncoef+ncoef
      ei=0.0_sp
      ej=ei
      omii=ei
      !dir$ assume_aligned y2d:64
      !dir$ assume_aligned al:64
      !dir$ assume_aligned om:64
      !dir$ assume_aligned t:64
!$omp parallel do default(none) schedule(runtime) &
!$omp firstprivate(ei,ej,omii)                    &
!$omp private(i,ti,j,tj,ii,jj,sum1,sum2)          &
!$omp shared(nappr,nc1,c2,n,y2d,al,om,t)
      do i=1,nappr
         ti=t(i)
         ei=exp(ti*c1)
         do j=1,nappr
            sum1=0.0_sp
            tj=t(j)
            ej=exp(tj*c2)
            !$omp simd reduction(+:sum1)
            do ii=1,n
               sum2=0.0_sp
               omii=om(ii)
               t0=c1+al(ii)/ti
               !$omp simd reduction(+:sum2)
               do jj=1,n
                  sum2 = sum2+om(jj)*Fx(t0,c2+al(jj)/tj)
               end do
               sum1 = sum1+omii*sum2
            end do
            y2d(j,i) = ei*ej*sum1
         end do
      end do
!$omp end parallel do
      niters = nappr*nappr*n*n
    end subroutine gaver_stehfest_r4_1
      

 
    
    

















end module inv_laplace2d
