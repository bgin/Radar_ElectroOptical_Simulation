!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
!===============================================================================
module mod_grid

!use mod_precision,only : intkind, realkind
use mod_kinds, only : int4, dp
use mod_utility,only : diff

implicit none

integer(kind=int4),parameter :: stdout = 6
integer(kind=int4),parameter :: stderr = 0

private

public :: grid_type

type :: grid_type

  private

  integer(kind=int4),dimension(2) :: lb
    !! Lower bounds of the grid
  integer(kind=int4),dimension(2) :: ub
    !! Upper bounds of the grid
!DIR$ ATTRIBUTES ALIGN : 64 :: x
  real(kind=dp),dimension(:,:),allocatable :: x
    !! Distance in x-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 :: y
  real(kind=dp),dimension(:,:),allocatable :: y
    !! Distance in y-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
  real(kind=dp),dimension(:,:),allocatable :: dx
    !! Grid spacing in x-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 :: dy
  real(kind=dp),dimension(:,:),allocatable :: dy
    !! Grid spacing in y-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 :: lon
  real(kind=dp),dimension(:,:),allocatable :: lon
    !! Longitude [rad]
!DIR$ ATTRIBUTES ALIGN : 64 :: lat
  real(kind=dp),dimension(:,:),allocatable :: lat
    !! Latitude [rad]
!DIR$ ATTRIBUTES ALIGN : 64 :: alpha
  real(kind=dp),dimension(:,:),allocatable :: alpha
    !! Grid rotation angle [rad]

  contains

  procedure,public,pass(self) :: getLowerBounds
  procedure,public,pass(self) :: getUpperBounds
  procedure,public,pass(self) :: getAxisX
  procedure,public,pass(self) :: getAxisY
  procedure,public,pass(self) :: getGridSpacingX
  procedure,public,pass(self) :: getGridSpacingY
  procedure,public,pass(self) :: getGridRotation
  procedure,public,pass(self) :: getLongitude
  procedure,public,pass(self) :: getLatitude

endtype grid_type

interface grid_type
  module procedure :: constructor_1d
  module procedure :: constructor_2d
endinterface grid_type

!===============================================================================
contains



!-------------------------------------------------------------------------------
type(grid_type) function constructor_1d(lb,ub,x,dx) result(grid)

  integer(kind=int4),intent(in) :: lb
    !! Lower bound indices of the grid array
  integer(kind=int4),intent(in) :: ub
    !! Upper bound indices of the grid array
  real(kind=dp),dimension(:),intent(in),optional :: x
    !! Distance in x-direction [m]
  real(kind=dp),dimension(:),intent(in),optional :: dx
    !! Grid spacing in x-direction [m]

  integer(kind=int4) :: i

  grid % lb(1) = lb
  grid % ub(1) = ub
  grid % lb(2) = 1
  grid % ub(2) = 1

  allocate(grid % x(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))
  allocate(grid % dx(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))

  if(present(x) .and. .not. present(dx))then

    ! x is given as input argument
    grid % x(:,1) = x

    ! Compute dx using centered differences
    grid % dx(:,1) = diff(x)

  elseif(.not. present(x) .and. present(dx))then

    ! dx is given as input argument
    grid % dx(:,1) = dx

    ! Compute x using dx provided as input argument
    grid % x(1,1) = 0.0_dp
!DIR$ VECTOR ALIGNED
    do i = grid % lb(1)+1,grid % ub(1)
      grid % x(i,1) = grid % x(i-1,1) + grid % dx(i,1)
    enddo

  endif

  allocate(grid % y(0,0))
  allocate(grid % dy(0,0))
  allocate(grid % lon(0,0))
  allocate(grid % lat(0,0))

endfunction constructor_1d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
type(grid_type) function constructor_2d(lb,ub,x,y,dx,dy,lon,lat) result(grid)

  integer(kind=int4),dimension(:),intent(in) :: lb
    !! Lower bound indices of the grid array
  integer(kind=int4),dimension(:),intent(in) :: ub
    !! Upper bound indices of the grid array
  real(kind=dp),dimension(:,:),intent(in),optional :: x
    !! Distance in x-direction [m]
  real(kind=dp),dimension(:,:),intent(in),optional :: y
    !! Distance in y-direction [m]
  real(kind=dp),dimension(:,:),intent(in),optional :: dx
    !! Grid spacing in x-direction [m]
  real(kind=dp),dimension(:,:),intent(in),optional :: dy
    !! Grid spacing in y-direction [m]
  real(kind=dp),dimension(:,:),intent(in),optional :: lon
    !! Longitude [rad]
  real(kind=dp),dimension(:,:),intent(in),optional :: lat
    !! Latitude [rad]

  integer(kind=int4) :: grid_rank
  integer(kind=int4) :: idm,jdm
  integer(kind=int4) :: i,j

  ! Raise error if lb and ub are not of matching ranks
  if(.not. size(lb) == size(ub))then
    write(unit=stderr,fmt='(a)')'Error in grid constructor: size(lb) must == '&
      //'size(ub)'
    stop 1
  endif

  grid % lb = lb
  grid % ub = ub

  allocate(grid % x(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))
  allocate(grid % y(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))
  allocate(grid % dx(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))
  allocate(grid % dy(grid % lb(1):grid % ub(1),grid % lb(2):grid % ub(2)))

  if(present(x) .and. present(y))then

    ! x and y are given as input arguments
    grid % x = x
    grid % y = y

    ! Compute dx and dy using centered differences
    grid % dx = diff(x,dim=1)
    grid % dy = diff(y,dim=2)

  elseif(present(dx) .and. present(dy))then

    ! dx and dy are given as input arguments
    grid % dx = dx
    grid % dy = dy

    ! Compute x using dx provided as input argument
    grid % x(1,:) = 0
!DIR$ VECTOR ALIGNED
    do i = grid % lb(1)+1,grid % ub(1)
      grid % x(i,:) = grid % x(i-1,:) + grid % dx(i,:)
    enddo

    ! Compute y using dy provided as input argument
    grid % y(:,1) = 0
!DIR$ VECTOR ALIGNED
    do j = grid % lb(2)+1,grid % ub(2)
      grid % y(:,j) = grid % y(:,j-1) + grid % dy(:,j)
    enddo

  endif

endfunction constructor_2d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getLowerBounds(self) result(lb)
  !! Returns the lower bounds of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  integer(kind=int4),dimension(2) :: lb !! Upper bound indices
  lb = self % lb
endfunction getLowerBounds
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getUpperBounds(self) result(ub)
  !! Returns the upper bounds of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  integer(kind=int4),dimension(2) :: ub !! Lower bound indices
  ub = self % ub
endfunction getUpperBounds
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getAxisX(self) result(x)
  !! Returns the x-coordinate of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=DP),dimension(:,:),allocatable :: x !! x-coordinate [m]
  x = self % x
endfunction getAxisX
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getAxisY(self) result(y)
  !! Returns the y-coordinate [m] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=DP),dimension(:,:),allocatable :: y !! y-coordinate [m]
  y = self % y
endfunction getAxisY
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGridSpacingX(self) result(dx)
  !! Returns the grid spacing in x [m] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=DP),dimension(:,:),allocatable :: dx !! Grid spacing in x [m]
  dx = self % dx
endfunction getGridSpacingX
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGridSpacingY(self) result(dy)
  !! Returns the grid spacing in y [m] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=DP),dimension(:,:),allocatable :: dy !! Grid spacing in y [m]
  dy = self % dy
endfunction getGridSpacingY
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGridRotation(self) result(alpha)
  !! Returns the grid rotation angle [rad] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=dp),dimension(:,:),allocatable :: alpha !! Grid rotation [rad]
  alpha = self % alpha
endfunction getGridRotation
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getLongitude(self) result(lon)
  !! Returns the longitude array [rad] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=dp),dimension(:,:),allocatable :: lon !! Longitude [rad]
  lon = self % lon
endfunction getLongitude
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getLatitude(self) result(lat)
  !! Returns the latitude array [rad] of the grid instance.
  class(grid_type),intent(in) :: self !! Grid instance
  real(kind=dp),dimension(:,:),allocatable :: lat !! Latitude [rad]
  lat = self % lat
endfunction getLatitude
!-------------------------------------------------------------------------------
endmodule mod_grid
