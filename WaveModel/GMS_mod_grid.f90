module mod_grid

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
! Modified by Bernard Gingold (beniekg@gmail.com) on 11/03/2019
! Removal of type :: grid_type member private access
! Using subroutine based constructors
! Removing getter functions
! Adding ATTRIBUTE ALIGNE Compiler directives
!===============================================================================

    use mod_kinds,   only :  int32_t, dp
    use mod_utility, only :  diff
    implicit none

    integer(kind=int32_t), parameter, private :: stdout = 6
    integer(kind=int32_t), parameter, private :: stderr = 0

    type, public :: grid_type

        public

        integer(kind=int32_t), dimension(2) :: lb
             !! Lower bounds of the grid
        integer(kind=int32_t), dimension(2) :: ub
             !! Upper bounds of the grid
!DIR$   ATTRIBUTES ALIGN : 64 :: x
        real(kind=dp), allocatable, dimension(:,:) :: x
             !! Distance in x-direction [m]
!DIR$   ATTRIBUTES ALIGN : 64 :: y
        real(kind=dp), allocatable, dimension(:,:) :: y
             !! Distance in y-direction [m]
!DIR$   ATTRIBUTES ALIGN : 64 :: dx
        real(kind=dp), allocatable, dimension(:,:) :: dx
             !! Grid spacing in x-direction [m]
!DIR$   ATTRIBUTES ALIGN : 64 :: dy
        real(kind=dp), allocatable, dimension(:,:) :: dy
             !! Grid spacing in y-direction [m]
!DIR$   ATTRIBUTES ALIGN : 64 :: lon
        real(kind=dp), allocatable, dimension(:,:) :: lon
             !! Longitude [rad]
!DIR$   ATTRIBUTES ALIGN : 64 :: lat
        real(kind=dp), allocatable, dimension(:,:) :: lat
             !! Latitude [rad]
!DIR$   ATTRIBUTES ALIGN : 64 :: alpha
        real(kind=dp), allocatable, dimension(:,:) :: alpha
             !! Grid rotation angle [rad]
    end type grid_type


    contains

    subroutine constructor_1d(grid,lb,ub,x,dx,errstate,iounit,logging, &
                              verbose,append,fname)
        use mod_print_error,  only : print_non_fatal_error,  &
                                     handle_fatal_memory_error
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: constructor_1d
        type(grid_type),              intent(inout)        :: grid
        integer(kind=int32_t),        intent(in)           :: lb
        integer(kind=int32_t),        intent(in)           :: ub
!DIR$ ASSUME_ALIGNED x:64
        real(kind=dp),  dimension(:), intent(in), optional :: x
!DIR$ ASSUME_ALIGNED dx:64
        real(kind=dp),  dimension(:), intent(in), optional :: dx
        logical(kind=int32_t),        intent(inout)        :: errstate
        integer(kind=int32_t),        intent(in)           :: iounit
        logical(kind=int32_t),        intent(in)           :: logging
        logical(kind=int32_t),        intent(in)           :: verbose
        logical(kind=int32_t),        intent(in)           :: append
        character(len=*),             intent(in)           :: fname
        ! LOcals
!DIR$   ATTRIBUTES ALIGN : 64 :: tmp
        real(kind=dp), allocatable,dimension(:) :: tmp
        integer(kind=int32_t), automatic :: i
        integer(kind=int32_t), automatic :: aerr
        character(len=256),    automatic :: emsg
        ! EXec code .....
        grid.lb(1)=lb; grid.ub(1)=ub
        grid.lb(2)=1;  grid.ub(2)=1
        if(allocated(grid.x)) then
                deallocate(grid.x)
                allocate(grid.x(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)),  &
                                     STAT=aerr,ERRMSG=emsg)
                if(aerr /= 0) goto 9999
        else
                allocate(grid.x(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)),  &
                                     STAT=aerr,ERRMSG=emsg)
                if(aerr /= 0) goto 9999
        end if
        if(allocated(grid.dx)) then
                deallocate(grid.dx)
                allocate(grid.dx(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                    STAT=aer,ERRMSG=emsg)
                if(aerr /= 0) goto 9999
         else
                allocate(grid.dx(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                    STAT=aer,ERRMSG=emsg)
                if(aerr /= 0) goto 9999
          end if
          if(present(x) .and. .not. present(dx)) then
                allocate(tmp(size(x))
                grid.x(:,1) = x
                call diff(x,tmp)
                grid.dx(:,1) = tmp
          elseif(.not. present(x) .and. present(dx)) then
                grid.dx(:,1) = dx
                grid.x(1,1)  = 0.0_dp
!DIR$   VECTOR ALIGNED
!DIR$   SIMD (VECTORLENGTHFOR(REAL(KIND=dp))
                do i = grid.lb(1)+1,grid.ub(1)
                       grid.x(i,1) = grid.x(i-1,1)+grid.dx(i,1)
                end do
          end if
          allocate(grid.y(0,0))
          allocate(grid.dy(0,0))
          allocate(grid.lon(0,0))
          allocate(grid.lat(0,0))
          errstate = .false.
          return
9999
          call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_grid, subroutine: constructor_1d -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_grid, subroutine: constructor_1d -- Memory Allocation Failure !!", &
                              emsg,128)
    end subroutine constructor_1d

    subroutine constructor_2d(grid,lb,ub,x,y,dx,dy,lon,lat,  &
                              errstate,iounit,logging,verbose,append,fname)
          use mod_print_error,  only : handle_fatal_memory_error,  &
                                       print_non_fatal_error
!DIR$     ATTRIBUTES CODE_ALIGN:32 :: constructor_2d
          type(grid_type),                        intent(inout) :: grid
          integer(kind=int32_t), dimension(2),    intent(in)    :: lb
          ! Arrays lb,ub must be of size = 2
          integer(kind=int32_t), dimension(2),    intent(in)    :: ub
!DIR$     ASSUME_ALIGNED x:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: x
!DIR$     ASSUME_ALIGNED y:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: y
!DIR$     ASSUME_ALIGNED dx:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: dx
!DIR$     ASSUME_ALIGNED dy:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: dy
!DIR$     ASSUME_ALIGNED lon:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: lon
!DIR$     ASSUME_ALIGNED lat:64
          real(kind=dp),         dimension(:,:),  intent(in), optional    :: lat
          logical(kind=int32_t),                  intent(inout)        :: errstate
          integer(kind=int32_t),                  intent(in)           :: iounit
          logical(kind=int32_t),                  intent(in)           :: logging
          logical(kind=int32_t),                  intent(in)           :: verbose
          logical(kind=int32_t),                  intent(in)           :: append
          character(len=*),                       intent(in)           :: fname
          ! LOcals
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp_dx, tmp_dy
          real(kind=dp), allocatable, dimension(:,:) :: tmp_dx,tmp_dy
          character(len=256),     automatic :: emsg
          integer(kind=int32_t),  automatic :: aerr
          integer(kind=int32_t),  automatic :: grid_rank
          integer(kind=int32_t),  automatic :: idm,jdm
          integer(kind=int32_t),  automatic :: i,j
          ! EXec code .....
          if(size(lb) /= size(ub)) then
               call print_non_fatal_error( " ================= Non-Fatal ================== " , &
                         " Module: mod_grid, subroutine: constructor_2d: Invalid size(lb) == size(ub) ",  &
                                        __LINE__,__FILE__ )
               errstate = .true.
               return
          end if
          grid.lb = lb
          grid.ub = ub
          if(allocated(grid.x)) then
                deallocate(grid.x)
                allocate(grid.x(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                    STAT=aerr, ERRMSG=emsg)
                if(aerr /= 0) goto 9999
          else
                allocate(grid.x(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                    STAT=aerr, ERRMSG=emsg)
                if(aerr /= 0) goto 9999
          end if
          if(allocated(grid.y)) then
                 deallocate(grid.y)
                 allocate(grid.y(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                     STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          else
                 allocate(grid.y(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                     STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          end if
          if(allocated(grid.dx)) then
                 deallocate(grid.dx)
                 allocate(grid.dx(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                      STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          else
                 allocate(grid.dx(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                      STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          end if
          if(allocated(grid.dy)) then
                 deallocate(grid.dy)
                 allocate(grid.dy(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                      STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          else
                 allocate(grid.dy(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)), &
                                      STAT=aerr, ERRMSG=emsg)
                 if(aerr /= 0) goto 9999
          end if
          if(present(x) .and. present(y)) then
                grid.x = x; grid.y = y
                allocate(tmp_dx(grid.lb(1):grid.ub(1),grid.lb(2):grid.ub(2)))
                allocate(tmp_dy(grid.lb(1):grid_ub(1),grid.lb(2):grid.ub(2)))
                call diff(x,tmp_dx,size(x,dim=1),size(x,dim=2),dim=1)
                grid.dx = tmp_dx
                call diff(y,tmp_dy,size(y,dim=1),size(y,dim=2),dim=2)
                grid.dy = tmp_dy
           elseif(present(dx) .and. present(dy)) then
                grid.dx = dx; grid.dy = dy
                grid.x(1,:) = 0.0_dp
!DIR$   VECTOR ALIGNED
!DIR$   SIMD VECTORLENGTHFOR(REAL(KIND=dp))
                do i = grid.lb(1)+1,grid.ub(1)
                       grid.x(i,:) = grid.x(i-1,:)+grid.dx(i,:)
                end do
                grid.y(:,1) = 0.0_dp
!DIR$   VECTOR ALIGNED
!DIR$   SIMD VECTORLENGTHFOR(REAL(KIND=dp))
                do j = grid.lb(2)+1,grid.ub(2)
                       grid.y(:,j) = grid.y(:,j-1)+grid.dy(:,j)
                end do
             end if
             errstate = .false.
             return
9999
             call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_grid, subroutine: constructor_2d -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_grid, subroutine: constructor_1d -- Memory Allocation Failure !!", &
                              emsg,243)
    end subroutine constructor_2d

end module mod_grid
