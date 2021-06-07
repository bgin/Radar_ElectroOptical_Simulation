
module mod_utility

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
! Modified by Bernard Gingold (beniekg@gmail.com) on 10/03/2019
! Removing an excessive usage of 'functional' style which will result
! in reduced performance by using extensive and expensive copy operations.

     use mod_kinds, only : i4, dp
     implicit none
     private

     public :: diff
     public :: diff_periodic
     public :: ones
     public :: range
     public :: tile
     public :: zeros

     interface diff
        module procedure :: diff_1d
        module procedure :: diff_2d
     end interface diff

     interface diff_periodic
        module procedure :: diff_periodic_1d
        module procedure :: diff_periodic_2d
     end interface diff_periodic

     interface ones
        module procedure :: ones_int
        module procedure :: ones_real
     end interface ones

     interface range
        module procedure :: range_int
        module procedure :: range_real
     end interface range

     interface tiles
        module procedure :: tile_1d_int
        module procedure :: tile_1d_real
        module procedure :: tile_2d_int
        module procedure :: tile_2d_real
        module procedure :: tile_3d_int
        module procedure :: tile_3d_real
     end interface tiles

     interface zeros
        module procedure :: zeros_int
        module procedure :: zeros_real
     end interface zeros

     contains

!============================================================================================95

!DIR$ ATTRIBUTES INLINE :: diff_1d
     subroutine diff_1d(x,dx,nx)
        !! Returns a centered-difference of a 1-d array, with first order
        !! differencing applied for the boundary points. This procedure is overloaded
        !! by the generic procedure `diff`.
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: diff_1d
        real(kind=dp), dimension(nx), intent(in)  :: x
        real(kind=dp), dimension(nx), intent(out) :: dx
        integer(kind=i4),        intent(in)  :: nx
        ! Exec code .....
!DIR$   VECTOR ALIGNED
        dx(2:nx-1) = 0.5_dp*(x(3:nx)-x(1:nx-2))
        dx(1)      = x(2)-x(1)
        dx(nx)     = x(nx)-x(nx-1)
     end subroutine diff_1d

!============================================================================================95

!DIR$ ATTRIBUTES INLINE :: diff_2d
     subroutine diff_2d(x,dx,nx,ny,dim)
         !! Returns a centered-difference of a 2-d array along dimension dim, with
         !! first order differencing applied for the boundary points. This procedure is
         !! overloaded by the generic procedure `diff`.
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: diff_2d
         real(kind=dp), dimension(nx,ny), intent(in)  :: x
         real(kind=dp), dimension(nx,ny), intent(out) :: dx
         integer(kind=i4),           intent(in)  :: nx
         integer(kind=i4),           intent(in)  :: ny
         integer(kind=i4),           intent(in)  :: dim
         ! Exec code .....
         if(1 == dim) then
!DIR$  VECTOR ALIGNED
              dx(2:nx-1,:)  = 0.5_dp*(x(3:nx,:)-x(1:nx-2,:))
              dx(1,:)       = x(2,:)-x(1,:)
              dx(nx,:)      = x(nx,:)-x(nx-1,:)
          elseif(2 == dim) then
!DIR$   VECTOR ALIGNED
              dx(:,2:nx-1)  = 0.5_dp*(:,3:nx)-x(:,1:nx-2))
              dx(:,1)       = x(2,:)-x(:,1)
              dx(:,nx)      = x(:,nx)-x(:,nx-1)
          end if
     end subroutine diff_2d

!============================================================================================95

!DIR$   ATTRIBUTES INLINE :: diff_periodic_1d
     subroutine diff_periodic_1d(x,dx,nx)
         !! Returns a centered-difference of a 1-d array with periodic boundary
         !! conditions. This procedure is overloaded by the generic procedure `diff`.
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: diff_periodic_1d
         real(kind=dp), dimension(nx),  intent(in)  :: x
         real(kind=dp), dimension(nx),  intent(out) :: dx
         integer(kind=int32_t),         intent(in)  :: nx
         ! Exec code ......
         dx(2:nx-1)     = 0.5_dp*(x(3:nx)-x(1:nx-2))
         dx(1)          = 0.5_dp*(x(2)-x(1))
         dx(nx)         = 0.5_dp*(x(1)-x(nx-1))
     end subroutine diff_periodic_1d

!============================================================================================95

!DIR$   ATTRIBUTES INLINE :: diff_periodic_2d
    subroutine diff_periodic_2d(x,dx,nx,ny,dim)
        !! Returns a centered-difference of a 2-d array along dimension dim, with
        !! periodic boundary conditions. This procedure is overloaded by the generic
        !! procedure `diff`.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: diff_periodic_2d
        real(kind=dp),  dimension(nx,ny),   intent(in)   :: x
        real(kind=dp),  dimension(nx,ny),   intent(out)  :: dx
        integer(kind=i4),              intent(in)   :: nx
        integer(kind=i4),              intent(in)   :: ny
        integer(kind=i4),              intent(in)   :: dim
        ! EXec code .....
        if(1 == dim) then
!DIR$   VECTOR ALIGNED
             dx(2:nx-1,:)   = 0.5_dp*(x(3:nx,:)-x(1:nx-2,:))
             dx(1,:)        = 0.5_dp*(x(2,:)-x(nx,:))
             dx(nx,:)       = 0.5_dp*(x(1,:)-x(nx-1,:))
        elseif(2 == dim) then
             dx(:,2:nx-1)   = 0.5_dp*(x(:,3:nx)-x(:,1:nx-2))
             dx(:,1)        = 0.5_dp*(x(:,2)-x(:,nx))
             dx(:,nx)       = 0.5_dp*(x(:,1)-x(:,nx-1))
        end if
    end subroutine diff_periodic_2d

!=============================================================================================95

!DIR$   ATTRIBUTES INLINE :: ones_int
    subroutine ones_int(ones,length,kflag)
        !! Returns a 1-d array of integer ones. This procedure is overloaded by the
        !! generic procedure `ones`.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: ones_int
!DIR$   ASSUME_ALIGNED ones:64
        integer(kind=i4),  dimension(length),  intent(out)   :: x
        integer(kind=i4),                      intent(in)    :: length
        integer(kind=i4),                      intent(in)    :: kflag
        ! Exec code
!DIR$   VECTOR ALIGNED
        ones = 1  ! Probably memset (optimized?) will be inserted here
    end subroutine ones_int

!=============================================================================================95

!DIR$   ATTRIBUTES INLINE :: ones_real
    subroutine ones_real(ones,length,kflag)
         !! Returns a 1-d array of floating-point ones. This procedure is overloaded by
         !! the generic procedure `ones`.
!DIR$    ATTRIBUTES CODE_ALIGN:32 :: ones_real
!DIR$    ASSUME_ALIGNED ones:64
         real(kind=dp), dimension(length),  intent(out) :: x
         integer(kind=i4),             intent(in)  :: length
         integer(kind=i4),             intent(in)  :: kflag
         ! EXec code ....
!DIR$    VECTOR ALIGNED
         ones = 1.0_dp  ! Probably memset (optimized?) will be inserted here
    end subroutine ones_real

!=============================================================================================95

    subroutine range_int(x,start,end,increment,aerr)
          !! Returns an array of integers given start, end, and increment values. If the
          !! increment argument is not passed, default increment is 1. This procedure is
          !! overloaded by the generic procedure `range`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: range_int
!DIR$     ATTRIBUTES ALIGN : 64 :: x
          integer(kind=int32_t), allocatable, dimension(:), intent(out)   :: x
          integer(kind=int32_t),                            intent(in)    :: start
          integer(kind=int32_t),                            intent(in)    :: end
          integer(kind=int32_t),                            intent(in), optional    :: increment
          integer(kind=int32_t),                            intent(inout) :: aerr
          ! Locals
          integer(kind=int32_t), automatic :: i
          integer(kind=int32_t), automatic :: incr
          integer(kind=int32_t), automatic :: length
          ! Exec code ....
          if(allocated(x)) then
             return
          end if
          if(present(increment)) then
             incr = increment
          else
             incr = 1
          end if
          length = (end-start)/incr+1
          allocate(x(length), STAT=aerr)
          if(aerr /= 0) then
             return
          end if
          do concurrent(i=1:length)
               x(i) = start+(i-1)*incr
          end do
    end subroutine range_int

!=============================================================================================95

    subroutine range_real(x,start,end,increment,aerr)
          !! Returns an array of reals given start, end, and increment values. If the
          !! increment argument is not passed, default increment is 1. This procedure is
          !! overloaded by the generic procedure `range`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: range_real
!DIR$     ATTRIBUTES ALIGN : 64 :: x
          real(kind=dp), allocatable, dimension(:),  intent(out)   :: x
          real(kind=dp),                             intent(in)    :: start
          real(kind=dp),                             intent(in)    :: end
          real(kind=dp),                             intent(in), optional :: increment
          integer(kind=int32_t),                     intent(inout) :: aerr
          ! Locals
          real(kind=dp), automatic :: incr
          integer(kind=int32_t), automatic :: i
          integer(kind=int32_t), automatic :: length
          ! EXec code .....
          if(allocated(x)) then
                return
          end if
          if(present(x)) then
                incr = increment
          else
                incr = 1.0_dp
          end if
          length = int((end-start)/incr)+1
          allocate(x(length),STAT=aerr)
          if(aerr /= 0) then
                return
          end if
          do concurrent(i=1:length)
              x(i) = start+(i-1)*incr
          end do
    end subroutine range_real

!=============================================================================================95

    subroutine tile_1d_int(in_arr,out_arr,nx,n,aerr)
         !! Tiles the input array `n` times. Returns a tiled array that has rank equal
         !! to `size(shape(array))+1` and that has values equal to values of `array`,
         !! repeated `n` times. This version is for 1-d input array of integers. This
         !! procedure is overloaded by the generic procedure `tile`.
!DIR$    ATTRIBUTES CODE_ALIGN:32 :: tile_1d_int
!DIR$    ASSUME_ALIGNED in_arr:64
         integer(kind=int32_t), dimension(nx),               intent(in)    :: in_arr
!DIR$    ATTRIBUTES ALIGN : 64 :: out_arr
         integer(kind=int32_t), allocatable, dimension(:,:), intent(inout) :: out_arr
         integer(kind=int32_t),                              intent(in)    :: nx
         integer(kind=int32_t),                              intent(in)    :: n
         integer(kind=int32_t),                              intent(inout) :: aerr
         ! LOcals
         integer(kind=int32_t), automatic :: i
         ! Exec code ....
         if(allocated(out_arr)) then
                return
         end if
         allocate(out_arr(nx,n), STAT=aerr)
         if(aerr /= 0) then
                return
         end if
         do concurrent(i=1:n)
            out_arr(:,i) = in_arr(i)
         end do
    end subroutine tile_1d_int

!=============================================================================================95

    subroutine tile_1d_real(in_arr,out_arr,nx,n,aerr)
          !! Tiles the input array `n` times. Returns a tiled array that has rank equal
          !! to `size(shape(array))+1` and that has values equal to values of `array`,
          !! repeated `n` times. This version is for 1-d input array of reals. This
          !! procedure is overloaded by the generic procedure `tile`
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: tile_1d_real
!DIR$     ASSUME_ALIGNED in_arr:64
          real(kind=dp),  dimension(nx),              intent(in)    :: in_arr
!DIR$     ATTRIBUTES ALIGN : 64 :: out_arr
          real(kind=dp), allocatable, dimension(:,:), intent(inout) :: out_arr
          integer(kind=int32_t),                      intent(in)    :: nx
          integer(kind=int32_t),                      intent(in)    :: n
          integer(kind=int32_t),                      intent(inout) :: aerr
          ! LOcals
          integer(kind=int32_t), automatic :: i
          ! Exec code ...
          if(allocated(out_arr)) then
                 return
          end if
          allocate(out_arr(nx,n), STAT=aerr)
          if(aerr /= 0) then
                  return
          end if
          do concurrent(i=1:n)
              out_arr(:,i) = in_arr(i)
          end do
    end subroutine tile_1d_real

!=============================================================================================95

    subroutine tile_2d_int(in_arr,out_arr,nx,ny,n,aerr)
          !! Tiles the input array `n` times. Returns a tiled array that has rank equal
          !! to `size(shape(array))+1` and that has values equal to values of `array`,
          !! repeated `n` times. This version is for 2-d input array of integers. This
          !! procedure is overloaded by the generic procedure `tile`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: tile_2d_int
!DIR$     ASSUME_ALIGNED in_arr:64
          integer(kind=int32_t),   dimension(nx,ny),              intent(in)    :: in_arr
!DIR$     ATTRIBUTES ALIGN : 64 :: out_arr
          integer(kind=int32_t),   allocatable, dimension(:,:,:), intent(inout) :: out_arr
          integer(kind=int32_t),                                  intent(in)    :: nx
          integer(kind=int32_t),                                  intent(in)    :: ny
          integer(kind=int32_t),                                  intent(in)    :: n
          integer(kind=int32_t),                                  intent(inout) :: aerr
          ! Locals
          integer(kind=int32_t), automatic :: i
          ! EXec code .....
          if(allocated(out_arr)) then
                 return
          end if
          allocate(out_arr(nx,ny,n), STAT=aerr)
          if(aerr /= 0) then
                return
          end if
          do concurrent(i=1:n)
                out_arr(:,:,i) = in_arr(:,:)
          end do
    end subroutine tile_2d_int

!=============================================================================================95

    subroutine tile_2d_real(in_arr,out_arr,nx,ny,n,aerr)
          !! Tiles the input array `n` times. Returns a tiled array that has rank equal
          !! to `size(shape(array))+1` and that has values equal to values of `array`,
          !! repeated `n` times. This version is for 2-d input array of reals. This
          !! procedure is overloaded by the generic procedure `tile`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: tile_2d_real
!DIR$     ASSUME_ALIGNED in_arr:64
          real(kind=dp),    dimension(nx,ny),                     intent(in)      :: in_arr
!DIR$     ATTRIBUTES ALIGN : 64 :: out_ar
          real(kind=dp),    allocatable,  dimension(:,:,:),       intent(inout)   :: out_arr
          integer(kind=int32_t),                                  intent(in)      :: nx
          integer(kind=int32_t),                                  intent(in)      :: ny
          integer(kind=int32_t),                                  intent(in)      :: n
          integer(kind=int32_t),                                  intent(inout)   :: aerr
          ! Locals
          integer(kind=int32_t), automatic :: i
          ! EXec code .....
          if(allocated(out_arr)) then
                 return
          end if
          allocate(out_arr(nx,ny,n), STAT=aerr)
          if(aerr /= 0) then
                return
          end if
          do concurrent(i=1:n)
             out_arr(:,:,i) = in_arr(:,:)
          end do
    end subroutine tile_2d_real

!=============================================================================================95

    subroutine tile_3d_int(in_arr,out_arr,nx,ny,nz,n,aerr)
          !! Tiles the input array `n` times. Returns a tiled array that has rank equal
          !! to `size(shape(array))+1` and that has values equal to values of `array`,
          !! repeated `n` times. This version is for 3-d input array of integers. This
          !! procedure is overloaded by the generic procedure `tile`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: tile_3d_int
!DIR$     ASSUME_ALIGNED : 64 :: in_arr
          integer(kind=int32_t),        dimension(nx,ny,nz),       intent(in)      :: in_arr
!DIR$     ATTRIBUTES ALIGN : 64 :: out_ar
          integer(kind=int32_t),  allocatable, dimension(:,:,:,:), intent(inout)   :: out_arr
          integer(kind=int32_t),                                   intent(in)      :: nx
          integer(kind=int32_t),                                   intent(in)      :: ny
          integer(kind=int32_t),                                   intent(in)      :: nz
          integer(kind=int32_t),                                   intent(in)      :: n
          integer(kind=int32_t),                                   intent(inout)   :: aerr
          ! Locals
          integer(kind=int32_t), automatic :: i
          ! Exec code ....
          if(allocated(out_arr)) then
                return
          end if
          allocate(out_arr(nx,ny,nz,n), STAT=aerr)
          if(aerr /= 0) then
                return
          end if
          do concurrent(i=1:n)
             out_arr(:,:,:,i) = in_arr(:,:,:)
          end do
    end subroutine tile_3d_int

!=============================================================================================95

    subroutine tile_3d_real(in_arr,out_arr,nx,ny,nz,n,aerr)
          !! Tiles the input array `n` times. Returns a tiled array that has rank equal
          !! to `size(shape(array))+1` and that has values equal to values of `array`,
          !! repeated `n` times. This version is for 3-d input array of reals. This
          !! procedure is overloaded by the generic procedure `tile`.
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: in_arr
!DIR$     ASSUME_ALIGNED : 64 :: in_arr
          real(kind=dp),        dimension(nx,ny,nz),               intent(in)        :: in_arr
!DIR$     ATTRIBUTES ALIGN : 64 :: out_arr
          real(kind=dp),    allocatable, dimension(:,:,:,:),       intent(inout)     :: out_arr
          integer(kind=int32_t),                                   intent(in)        :: nx
          integer(kind=int32_t),                                   intent(in)        :: ny
          integer(kind=int32_t),                                   intent(in)        :: nz
          integer(kind=int32_t),                                   intent(in)        :: n
          integer(kind=int32_t),                                   intent(inout)     :: aerr
          ! Locals
          integer(kind=int32_t), automatic :: i
          ! Exec code ....
          if(allocated(out_arr)) then
                return
          end if
          allocate(out_arr(nx,ny,nz,n), STAT=aerr)
          if(aerr /= 0) then
                return
          end if
          do concurrent(i=1:n)
              out_arr(:,:,:,i) = in_arr(:,:,:)
          end do
    end subroutine tile_3d_real

!=============================================================================================95

!DIR$ ATTRIBUTES INLINE :: zeros_int
    subroutine zeros_int(zeros,length,kflag)
         !! Returns a 1-d array of integer zeros. This procedure is overloaded by the
         !! generic procedure `zeros`.
!DIR$    ATTRIBUTES CODE_ALIGN:32 :: zeros_int
!DIR$    ASSUME_ALIGNED zeros:64
         integer(kind=int32_t),  dimension(length), intent(inout) :: zeros
         integer(kind=int32_t),                     intent(in)    :: length
         integer(kind=int32_t),                     intent(in)    :: kflag
         ! EXec code ....
         zeros = 0 ! Probably memset will be called here
    end subroutine zeros_int

!DIR$ ATTRIBUTES INLINE :: zeros_real
    subroutine zeros_real(zeros,length,kflag)
        !! Returns a 1-d array of floating-point zeros. This procedure is overloaded by
        !! the generic procedure `zeros`.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: zeros_real
!DIR$   ASSUME_ALIGNED zeros:64
        real(kind=dp),  dimension(length), intent(inout) :: zeros
        integer(kind=int32_t),             intent(in)    :: length
        integer(kind=int32_t),             intent(in)    :: kflag
        ! EXec code ....
        zeros = 0.0_dp
    end subroutine zeros_real

end module mod_utility
