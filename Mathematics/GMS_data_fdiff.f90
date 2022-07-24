

module data_fdiff


  !===========================================================
  ! Simple central difference numerical differentiation
  ! of tabulated data, i.e. generating function is unknown.
  ! Central difference scheme is used.
  !===========================================================
  
  use mod_kinds, only : i4,sp,dp
  public 
  implicit none
 
  interface data_fdiff
     module procedure :: data_fdiff_r4
     module procedure :: data_fdiff_r8
     module procedure :: data_fdiff_c4
     module procedure :: data_fdiff_c8
  end interface data_fdiff

  interface data_fdiff_omp
     module procedure :: data_fdiff_omp_r4
     module procedure :: data_fdiff_omp_r8
     module procedure :: data_fdiff_omp_c4
     module procedure :: data_fdiff_omp_c8
  end interface data_fdiff_omp

  contains

  subroutine data_fdiff_r4(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_r4
      !dir$ attributes code_align : 32 :: data_fdiff_r4
      implicit none
      real(kind=sp),  dimension(n), intent(in)  :: y
      real(kind=sp),  dimension(n), intent(in)  :: x
      real(kind=sp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_r4


  subroutine data_fdiff_r8(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_r8
      !dir$ attributes code_align : 32 :: data_fdiff_r8
      implicit none
      real(kind=dp),  dimension(n), intent(in)  :: y
      real(kind=dp),  dimension(n), intent(in)  :: x
      real(kind=dp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_r8


  subroutine data_fdiff_c4(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_c4
      !dir$ attributes code_align : 32 :: data_fdiff_c4
      implicit none
      complex(kind=sp),  dimension(n), intent(in)  :: y
      complex(kind=sp),  dimension(n), intent(in)  :: x
      complex(kind=sp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_c4


   subroutine data_fdiff_c8(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_c8
      !dir$ attributes code_align : 32 :: data_fdiff_c8
      implicit none
      complex(kind=dp),  dimension(n), intent(in)  :: y
      complex(kind=dp),  dimension(n), intent(in)  :: x
      complex(kind=dp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_c8


   subroutine data_fdiff_omp_r4(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_omp_r4
      !dir$ attributes code_align : 32 :: data_fdiff_omp_r4
      use omp_lib
      implicit none
      real(kind=sp),  dimension(n), intent(in)  :: y
      real(kind=sp),  dimension(n), intent(in)  :: x
      real(kind=sp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
!$omp parallel do default(none) schedule(static,16) &
!$omp private(i) shared(n,dy,y,x)
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_omp_r4


  subroutine data_fdiff_omp_r8(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_omp_r8
      !dir$ attributes code_align : 32 :: data_fdiff_omp_r8
      use omp_lib
      implicit none
      real(kind=dp),  dimension(n), intent(in)  :: y
      real(kind=dp),  dimension(n), intent(in)  :: x
      real(kind=dp),  dimension(n), intent(out) :: dy
      integer(kind=i4),             intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
!$omp parallel do default(none) schedule(static,8) &
!$omp private(i) shared(n,dy,y,x)
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_omp_r8


  subroutine data_fdiff_omp_c4(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_omp_c4
      !dir$ attributes code_align : 32 :: data_fdiff_omp_c4
      use omp_lib
      implicit none
      complex(kind=sp),  dimension(n), intent(in)  :: y
      complex(kind=sp),  dimension(n), intent(in)  :: x
      complex(kind=sp),  dimension(n), intent(out) :: dy
      integer(kind=i4),                intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
!$omp parallel do default(none) schedule(static,8) &
!$omp private(i) shared(n,dy,y,x)
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_omp_c4


   subroutine data_fdiff_omp_c8(y,x,dy,n)
      !dir$ optimize:3
      !dir$ attributes forceinline :: data_fdiff_omp_c8
      !dir$ attributes code_align : 32 :: data_fdiff_omp_c8
      use omp_lib
      implicit none
      complex(kind=dp),  dimension(n), intent(in)  :: y
      complex(kind=dp),  dimension(n), intent(in)  :: x
      complex(kind=dp),  dimension(n), intent(out) :: dy
      integer(kind=i4),                intent(in)  :: n   !the length of n must be length=n-1, data must have a total length of n!!
      ! Locals
      integer(kind=i4), automatic :: i
      Exec code .....
      dy(1)   = (y(2)-y(1))/(x(2)-x(1))
      dy(n+1) = (y(n+1)-y(n))/(x(n+1)-x(n))
      !dir$ assume_aligned dy:64
      !dir$ assume_aligned y:64
      !dir$ assume_aligned x:64
      !dir$ ivdep
      !dir$ vector aligned
      !dir$ vector vectorlength(4)
      !dir$ vector always
!$omp parallel do default(none) schedule(static,8) &
!$omp private(i) shared(n,dy,y,x)
      do i=2, n
         dy(i) = (y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      end do
  end subroutine data_fdiff_omp_c8






end module data_fdiff
