


module solver_pstd_3d
#include "GMS_config.fpp"


  !=================================================!
  ! This module solves the Maxwell EM equations
  ! by the means of the PSTD method in the 3D domain.
  ! This code is based on 'SeLaLib' implementation
  ! and was adapted to suit the 'GMS' project.
  !=================================================!

!**************************************************************
!  Copyright INRIA
!
!  This code SeLaLib (for Semi-Lagrangian-Library)
!  is a parallel library for simulating the plasma turbulence
!  in a tokamak.
!
!  This software is governed by the CeCILL-B license
!  under French law and abiding by the rules of distribution
!  of free software.  You can  use, modify and redistribute
!  the software under the terms of the CeCILL-B license as
!  circulated by CEA, CNRS and INRIA at the following URL
!  "http://www.cecill.info".
!**************************************************************


  use mod_kinds, only : i4,dp
  use ISO_C_BINDINGS, only : c_int
  use 
  public
  implicit none
  
  real(kind=dp), parameter, private :: pi = 3.1415926535897932384626433832795_dp

  !Solver module members
  complex(kind=dp), dimension(:), allocatable :: fftx
  complex(kind=dp), dimension(:), allocatable :: ffty
  complex(kind=dp), dimension(:), allocatable :: fftz
  real(kind=dp),    dimension(:), allocatable :: ddx
  real(kind=dp),    dimension(:), allocatable :: ddy
  real(kind=dp),    dimension(:), allocatable :: ddz
  real(kind=dp),    dimension(:), allocatable :: kx
  real(kind=dp),    dimension(:), allocatable :: ky
  real(kind=dp),    dimension(:), allocatable :: kz
  !dir$ attributes align : 64 :: fftx
  !dir$ attributes align : 64 :: ffty
  !dir$ attributes align : 64 :: fftz
  !dir$ attributes align : 64 :: ddx
  !dir$ attributes align : 64 :: ddy
  !dir$ attributes align : 64 :: ddz
  !dir$ attributes align : 64 :: kx
  !dir$ attributes align : 64 :: ky
  !dir$ attributes align : 64 :: kz
  
  real(kind=dp)    :: e     !conductivity
  real(kind=dp)    :: mu    !permeability
  integer(kind=i4) :: cnx   ! cells number x-dir
  integer(kind=i4) :: cny   ! cells number y-dir
  integer(kind=i4) :: cnz   ! cells number z-dir
  integer(kind=i4) :: pol   ! polarization type
  ! FFTW members data types
  type, public :: fftw_plan
        integer(kind=i4), dimension(:), allocatable :: pshape !FFTW number of points along each dimension
        !dir$ attributes align : 64 :: pshape
        integer(kind=i4) :: pfftw
        integer(kind=i4) :: dir
        integer(kind=i4) :: prank
        logical(kind=i4) :: normalize ! normalization of FFT value types
        integer(kind=i4) :: ttype !transform type
  end type fftw_plan
  ! FFTW flags
  integer(kind=i4), parameter :: fftw_r2c_1d = 1
  integer(kind=i4), parameter :: fftw_c2r_1d = 2

  contains

  ! Initialize the FFTW plan for FFT real-to-complex 1D
  subroutine init_r_to_c_1d(plan,n,r_in,c_out,norm,opt)
    implicit none
    type(fftw_plan),   intent(out)    :: plan
    integer(kind=i4),  intent(in)     :: n
    real(kind=dp),     intent(inout)  :: r_in ! input real data
    complex(kind=dp),  intent(out)    :: c_out ! output complex data
    logical(kind=i4),  optional, intent(in) :: norm
    integer(kind=i4),  optional, intent(in) :: optim
    !Locals
    integer(kind=i4), automatic :: err
    integer(kind=i4), automatic :: flag
    ! Exec code
    plan.ttype = fftw_r2c_1d
    plan.dir   = 0
    if(present(norm)) then
       plan.normalize = norm
    else
       plan.normalize = .false.
    end if
    if(present(optim)) then
       flag = optim
    else
       flag = FFTW_ESTIMATE
    end if
    plan.prank = 1
    allocate(plan.pshape(1))
    plan.pshape = nx
    call dfftw_plan_dft_r2c_1d(plan.pfftw,n,r_in,c_out,flag)
  end subroutine init_r_to_c_1d

  !Execute FFT real-to-complex 1D.
  subroutine exec_r_to_c_1d(plan,r_in,c_out)
    use omp_lib
    implicit none
    type(fftw_plan),   intent(in)    :: plan
    real(kind=dp),     intent(inout) :: r_in
    complex(kind=dp),  intent(out)   :: c_out
    ! Locals
    complex(kind=dp), automatic :: c0
    real(kind=dp),    automatic :: fac
    integer(kind=i4), automatic :: i
    ! Execute
    call fftw_execute_dft_r2c(plan.pfftw,r_in,c_out)
    if(plan.normalize == .true.) then
       fac = 1.0_dp/real(plan.pshape(1),kind=dp)
       c0  = cmplx(0.0_dp,0.0_dp)
       !dir$ assume_aligned c_out:64
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !$omp simd private(c0)
       do i = 1,size(c_out),8
          c0 = cmplx(fac,0.0_dp,kind=dp)
          c_out(i+0)  = c0
          c_out(i+1)  = c0
          c_out(i+2)  = c0
          c_out(i+3)  = c0
          c_out(i+4)  = c0
          c_out(i+5)  = c0
          c_out(i+6)  = c0
          c_out(i+7)  = c0
       end do
  end subroutine exec_r_to_c_1d


   ! Initialize the FFTW plan for FFT complex-to-real 1D
  subroutine init_c_to_r_1d(plan,n,r_in,c_out,norm,opt)
    implicit none
    type(fftw_plan),   intent(out)    :: plan
    integer(kind=i4),  intent(in)     :: n
    complex(kind=dp),  intent(inout)  :: c_in ! input complex data
    real(kind=dp),     intent(out)    :: r_out ! output real data
    logical(kind=i4),  optional, intent(in) :: norm
    integer(kind=i4),  optional, intent(in) :: optim
    !Locals
    integer(kind=i4), automatic :: err
    integer(kind=i4), automatic :: flag
    ! Exec code
    plan.ttype = fftw_c2r_1d
    plan.dir   = 0
    if(present(norm)) then
       plan.normalize = norm
    else
       plan.normalize = .false.
    end if
    if(present(optim)) then
       flag = optim
    else
       flag = FFTW_ESTIMATE
    end if
    plan.prank = 1
    allocate(plan.pshape(1))
    plan.pshape = nx
    call dfftw_plan_dft_c2r_1d(plan.pfftw,n,c_in,r_out,flag)
  end subroutine init_c_to_r_1d

  !Execute FFT complex-to-real 1D.
  subroutine exec_c_to_r_1d(plan,r_in,c_out)
    use omp_lib
    implicit none
    type(fftw_plan),   intent(in)    :: plan
    complex(kind=dp),  intent(inout) :: c_int
    real(kind=dp),     intent(out)   :: r_out
    ! Locals
    real(kind=dp),    automatic :: fac
    integer(kind=i4), automatic :: i
    ! Execute
    call fftw_execute_dft_c2r(plan.pfftw,c_in,r_out)
    if(plan.normalize == .true.) then
       fac = 1.0_dp/real(plan.pshape(1),kind=dp)
       r0  = 0.0_dp 
       !dir$ assume_aligned r_out:64
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !$omp simd private(r0)
       do i = 1,size(c_out),8
          r_out(i+0)  = fac
          r_out(i+1)  = fac
          r_out(i+2)  = fac
          r_out(i+3)  = fac
          r_out(i+4)  = fac
          r_out(i+5)  = fac
          r_out(i+6)  = fac
          r_out(i+7)  = fac
       end do
  end subroutine exec_c_to_r_1d


  !Initialize Maxwell equations solver
  subroutine init_solver_pstd_3d(fpx,bpx,fpy,bpy,fpz,bpz,&
                                 minx,maxx,miny,maxy, &
                                 minz,maxz,nx,ny,nz)
      type(fftw_plan), intent(in) :: fpx   !forward plan x-line 
      
      real(kind=dp),   intent(in) :: minx
      real(kind=dp),   intent(in) :: maxx
      real(kind=dp),   intent(in) :: miny
      real(kind=dp),   intent(in) :: maxy
      real(kind=dp),   intent(in) :: minz
      real(kind=dp),   intent(in) :: maxz
      integer(kind=i4),intent(in) :: nx
      integer(kind=i4),intent(in) :: ny
      integer(kind=i4),intent(in) :: nz
      ! Locals
      complex(kind=dp), automatic :: c0
      real(kind=dp),    automatic :: dx,dy,dz     !step size
      real(kind=dp),    automatic :: wx0,wy0,wz0  ! wave numbers
      real(kind=dp),    automatic :: r0
      integer(kind=i4), automatic :: i
      ! Execute
      cnx = nx
      cny = ny
      cnz = nz
      e   = 1.0_dp
      mu  = 1.0_dp
      allocate(fftx(1:cnx/2+1))
      allocate(ffty(1:cny/2+1))
      allocate(fftz(1:cnz/2+1))
      allocate(ddx(1:cnx))
      allocate(ddy(1:cny))
      allocate(ddz(1:cnz))
      allocate(kx(cnx/2+1))
      allocate(ky(cny/2+1))
      allocate(kz(cnz/2+1))
      c0 = cmplx(0.0_dp,0.0_dp)
      r0 = 0.0_dp
      fftx(:) = c0
      ddx(:)  = r0
      kx(:)   = r0
      ffty(:) = c0
      ddy(:)  = r0
      ky(:)   = r0
      fftz(:) = c0
      ddz(:)  = r0
      kz(:)   = r0
      call init_r_to_c_1d(plan.pff,
  end subroutine init_solver_pstd_3d

                                 

  
  
  
end module solver_pstd_3d
