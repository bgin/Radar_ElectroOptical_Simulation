


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
  subroutine init_solver_pstd_3d(plans,&
                                 minx,maxx,miny,maxy, &
                                 minz,maxz,nx,ny,nz)
      type(fftw_plan), dimension(6), intent(in) :: plans   !FFTW plans
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
      real(kind=dp),    automatic :: stx,sty,stz
      real(kind=dp),    automatic :: r0,t0
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
      stx = real(cnx,kind=dp)
      sty = real(cny,kind=dp)
      stz = real(cnz,kind=dp)
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
      dx      = (maxx-minx)/stx
      call init_r_to_c_1d(plans(1),cnx,ddx,fftx)
      dy      = (maxy-miny)/sty
      call init_c_to_r_1d(plans(2),cnx,fftx,ddx)
      dz      = (maxz-minz)/stz
      call init_r_to_c_1d(plans(3),cny,ddy,ffty)
      wx0     = 2.0*pi/(stx*dx)
      call init_c_to_r_1d(plans(4),cny,ffty,ddy)
      wy0     = 2.0*pi/(sty*dy)
      call init_r_to_c_1d(plans(5),cnz,ddz,fftz)
      wz0     = 2.0*pi/(stz*dz)
      call init_c_to_r_1d(plans(6),cnz,fftz,ddz)
      kx(1)   = 1.0_dp
      t0      = 0.0_dp
      !dir$ assume_aligned kx:64
      !dir$ vector aligned
      !dir$ vector vectorlength(8)
      !dir$ vector always
      do i=2, cnx/2+1
         t0 = real(i,kind=dp)
         kx(i) = t0*wx0
      end do
      !dir$ assume_aligned ky:64
      !dir$ vector aligned
      !dir$ vector vectorlength(8)
      !dir$ vector always
      do i=2, cny/2+1
         t0 = real(i,kind=dp)
         ky(i) = t0*wy0
      end do
      !dir$ assume_aligned kz:64
      !dir$ vector aligned
      !dir$ vector vectorlength(8)
      !dir$ vector always
      do i=2, cnz/2+1
         t0 = real(i,kind=dp)
         kz(i) = t0*wz0
      end do
  end subroutine init_solver_pstd_3d

#if 0

  complex(kind=dp), dimension(:), allocatable :: fftx
  complex(kind=dp), dimension(:), allocatable :: ffty
  complex(kind=dp), dimension(:), allocatable :: fftz
  real(kind=dp),    dimension(:), allocatable :: ddx
  real(kind=dp),    dimension(:), allocatable :: ddy
  real(kind=dp),    dimension(:), allocatable :: ddz
  real(kind=dp),    dimension(:), allocatable :: kx
  real(kind=dp),    dimension(:), allocatable :: ky
  real(kind=dp),    dimension(:), allocatable :: kz
#endif
  subroutine free_fields()
     implicit none
     ! Exec code ....
     deallocate(kz);deallocate(ky)
     deallocate(kx);deallocate(ddz)
     deallocate(ddy);deallocate(ddx)
     deallocate(fftz);deallocate(ffty)
     deallocate(fftz)
  end subroutine free_fields

  


  subroutine solve_faraday(plans,      &
                           Ex,Ey,Ez,   &
                           Hx,Hy,Hz,dt )
       use omp_lib
       implicit none
       type(fftw_plans), dimension(6),     intent(in)    :: plans
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ex ! Electric fields
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ey
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ez
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hx ! Magnetic fields
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hy
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hz
       real(kind=dp),                      intent(in)    :: dt ! time-step
       ! Locals
       complex(kind=dp), automatic :: c0,c1,c2
       real(kind=dp), automatic :: dtmu,inx,iny,inz
       integer(kind=i4), automatic :: nx,ny,nz
       integer(kind=i4), automatic :: j,k,i
       integer(kind=i4), automatic :: sx,sy,sz
       !Execute
       nx  = cnx
       inx = 1.0_dp/real(nx,kind=dp) 
       sx = nx/2+1
       c0 = cmplx(0.0_dp,0.0_dp)
       ny = cny
       iny = 1.0_dp/real(ny,kind=dp)
       sy = ny/2+1
       c1 = c0
       nz = cnz
       inz = 1.0_dp/real(nz,kind=dp)
       sz = nz/2+1
       c2 = c0
       dtmu = dt/mu
       !dir$ assume_aligned Ex:64
       !dir$ assume_aligned Hx:64
       !dir$ assume_aligned ddy:64
       !dir$ assume_aligned Hz:64
       !dir$ assume_aligned ddx:64
       !dir$ assume_aligned fftx:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(nz,nx,ny,ddy,Ez,ffty,sy,ky)          &
!$omp shared(dtmu,Hx,iny)
       do k=1,nz+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,nx+1
              ddy = Ez(i,1:ny,k)
              call exec_r_to_c1_1d(plans(3),ddy,ffty)
              c0   = ffty(2:sy)
              c1   = -cmplx(0.0_dp,ky(2:sy),kind=dp)*c0
              ffty(2:sy) = c1
              call exec_c_to_r1_1d(plans(4),ffty,ddy)
              ddy = ddy*iny
              Hx(i,1:ny,k) = Hx(i,1:ny,k)-dtmu*ddy
              ddy = Ex(i,1:ny,k)
              call exec_r_to_c1_1d(plans(3),ddy,ffty)
              c0   = ffty(2:sy)
              c1   = -cmplx(0.0_dp,ky(2:sy),kind=dp)*c0
              ffty(2:sy) = c1
              call exec_c_to_r1_1d(plans(4),ffty,ddy)
              ddy = ddy*iny
              Hz(i,1:ny,k) = Hz(i,1:ny,k)+dtmu*ddy
           end do
       end do
!$omp end parallel do

       Hx(:,ny+1,:) = Hx(:,1,:)
       Hz(:,ny+1,:) = Hz(:,1,:)

       !dir$ assume_aligned ddz:64
       !dir$ assume_aligned Ey:64
       !dir$ assume_aligned Ex:64
       !dir$ assume_aligned fftz:64
       !dir$ assume_aligned kz:64
       !dir$ assume_aligned Hx:64
       !dir$ assume_aligned Hy:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(ny,nx,nz,ddz,Ey,fftz,kz,Hx,Hy)       &
!$omp shared(sz,dtmu,inz,Ex)
       do k=1,ny+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,nx+1
              ddz = Ey(i,k,1:nz)
              call exec_r_to_c1_1d(plans(5),ddz,fftz)
              c0   = fftz(2:sz)
              c1   = -cmplx(0.0_dp,kz(2:sz),kind=dp)*c0
              fftz(2:sz) = c1
              call exec_c_to_r1_1d(plans(6),fftz,ddz)
              ddz = ddz*inz
              Hx(i,k,1:nz) = Hx(i,k,1:nz)-dtmu*ddz
              ddz = Ex(i,k,1:nz)
              call exec_r_to_c1_1d(plans(5),ddz,fftz)
              c0   = fftz(2:sz)
              c1   = -cmplx(0.0_dp,kz(2:sz),kind=dp)*c0
              fftz(2:sz) = c1
              call exec_c_to_r1_1d(plans(6),fftz,ddz)
              ddz = ddz*inz
              Hy(i,j,1:nz) = Hy(i,k,1:nz)+dtmu*ddz
           end do
       end do
!$omp end parallel do
 
       Hx(:,:,nz+1) = Hx(:,:,1)
       Hy(:,:,nz+1) = Hy(:,:,1)

       !dir$ assume_aligned ddx:64
       !dir$ assume_aligned Ez:64
       !dir$ assume_aligned Ey:64
       !dir$ assume_aligned fftx:64
       !dir$ assume_aligned kx:64
       !dir$ assume_aligned Hz:64
       !dir$ assume_aligned Hy:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(ny,nx,nz,ddx,Ey,fftx,kx,Hz,Hy)       &
!$omp shared(sx,dtmu,inx,Ez)
       do k=1,nz+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,ny+1
              ddx = Ez(1:nx,i,k)
              call exec_r_to_c1_1d(plans(1),ddx,fftx)
              c0   = fftx(2:sx)
              c1   = -cmplx(0.0_dp,kx(2:sx),kind=dp)*c0
              fftx(2:sx) = c1
              call exec_c_to_r1_1d(plans(2),fftx,ddx)
              ddx = ddx*inx
              Hy(1:nx,i,k) = Hy(1:nx,i,k)-dtmu*ddx
              ddx = Ey(1:nx,i,k)
              call exec_r_to_c1_1d(plans(1),ddx,fftx)
              c0   = fftx(2:sx)
              c1   = -cmplx(0.0_dp,kx(2:sx),kind=dp)*c0
              fftx(2:sx) = c1
              call exec_c_to_r1_1d(plans(2),fftx,ddx)
              ddx = ddx*inx
              Hz(1:nx,i,k) = Hz(1:nx,i,k)+dtmu*ddx
           end do
       end do
!$omp end parallel do
 
       Hy(nx+1,:,:) = Hy(1,:,:)
       Hz(nx+1,:,:) = Hz(1,:,:)

             
  end subroutine solve_faraday


  subroutine solve_ampere( plans,      &
                           Ex,Ey,Ez,   &
                           Hx,Hy,Hz,dt,&
                           Jx,Jy,Jz )
       use omp_lib
       implicit none
       type(fftw_plans), dimension(6),     intent(in)    :: plans
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ex ! Electric fields
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ey
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Ez
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hx ! Magnetic fields
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hy
       real(kind=dp),    dimension(:,:,:), intent(inout) :: Hz
       real(kind=dp),                      intent(in)    :: dt ! time-step
       real(kind=dp),    dimension(:,:,:), intent(in),optional :: Jx
       real(kind=dp),    dimension(:,:,:), intent(in),optional :: Jy
       real(kind=dp),    dimension(:,:,:), intent(in),optional :: Jz
       ! Locals
       complex(kind=dp), automatic :: c0,c1,c2
       real(kind=dp), automatic :: dte,inx,iny,inz
       integer(kind=i4), automatic :: nx,ny,nz
       integer(kind=i4), automatic :: j,k,i
       integer(kind=i4), automatic :: sx,sy,sz
       !Execute
       nx  = cnx
       inx = 1.0_dp/real(nx,kind=dp) 
       sx = nx/2+1
       c0 = cmplx(0.0_dp,0.0_dp)
       ny = cny
       iny = 1.0_dp/real(ny,kind=dp)
       sy = ny/2+1
       c1 = c0
       nz = cnz
       inz = 1.0_dp/real(nz,kind=dp)
       sz = nz/2+1
       c2 = c0
       dte = dt/e
       !dir$ assume_aligned Ex:64
       !dir$ assume_aligned Hx:64
       !dir$ assume_aligned Hz:64
       !dir$ assume_aligned ddy:64
       !dir$ assume_aligned Ez:64
       !dir$ assume_aligned ddx:64
       !dir$ assume_aligned fftx:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(nz,nx,ny,ddy,ffty,sy,ky)          &
!$omp shared(dte,Hx,Ez,iny,Hz,Ex)
       do k=1,nz+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,nx+1
              ddy = Hz(i,1:ny,k)
              call exec_r_to_c1_1d(plans(3),ddy,ffty)
              c0   = ffty(2:sy)
              c1   = -cmplx(0.0_dp,ky(2:sy),kind=dp)*c0
              ffty(2:sy) = c1
              call exec_c_to_r1_1d(plans(4),ffty,ddy)
              ddy = ddy*iny
              Ex(i,1:ny,k) = Hx(i,1:ny,k)-dte*ddy
              ddy = Hx(i,1:ny,k)
              call exec_r_to_c1_1d(plans(3),ddy,ffty)
              c0   = ffty(2:sy)
              c1   = -cmplx(0.0_dp,ky(2:sy),kind=dp)*c0
              ffty(2:sy) = c1
              call exec_c_to_r1_1d(plans(4),ffty,ddy)
              ddy = ddy*iny
              Ez(i,1:ny,k) = Ez(i,1:ny,k)+dte*ddy
           end do
       end do
!$omp end parallel do
       
      Ex(:,ny + 1,:) = Ex(:,1,:)
      Ez(:,ny + 1,:) = Ez(:,1,:)

       !dir$ assume_aligned ddz:64
       !dir$ assume_aligned Ey:64
       !dir$ assume_aligned Ex:64
       !dir$ assume_aligned fftz:64
       !dir$ assume_aligned kz:64
       !dir$ assume_aligned Hx:64
       !dir$ assume_aligned Hy:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(ny,nx,nz,ddz,Ey,fftz,kz,Hx,Hy)       &
!$omp shared(sz,dte,inz,Ex)
       do k=1,ny+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,nx+1
              ddz = Hy(i,k,1:nz)
              call exec_r_to_c1_1d(plans(5),ddz,fftz)
              c0   = fftz(2:sz)
              c1   = -cmplx(0.0_dp,kz(2:sz),kind=dp)*c0
              fftz(2:sz) = c1
              call exec_c_to_r1_1d(plans(6),fftz,ddz)
              ddz = ddz*inz
              Ex(i,k,1:nz) = Ex(i,k,1:nz)-dte*ddz
              ddz = Hx(i,k,1:nz)
              call exec_r_to_c1_1d(plans(5),ddz,fftz)
              c0   = fftz(2:sz)
              c1   = -cmplx(0.0_dp,kz(2:sz),kind=dp)*c0
              fftz(2:sz) = c1
              call exec_c_to_r1_1d(plans(6),fftz,ddz)
              ddz = ddz*inz
              Ey(i,j,1:nz) = Ey(i,k,1:nz)+dte*ddz
           end do
       end do
!$omp end parallel do
      
      Ex(:,:,nz+1) = Ex(:,:,1)
      Ey(:,:,nz+1) = Ey(:,:,1)


       !dir$ assume_aligned ddx:64
       !dir$ assume_aligned Ez:64
       !dir$ assume_aligned Ey:64
       !dir$ assume_aligned fftx:64
       !dir$ assume_aligned kx:64
       !dir$ assume_aligned Hz:64
       !dir$ assume_aligned Hy:64
!$omp parallel do default(none) schedule(runtime) &
!$omp private(k,i,c0,c1,plans)                    &
!$omp shared(ny,nx,nz,ddx,Ey,fftx,kx,Hz,Hy)       &
!$omp shared(sx,dte,inx,Ez)
       do k=1,nz+1
          !dir$ vector aligned
          !dir$ ivdep
          !dir$ vector vectorlength(8)
          !$omp simd
          do i=1,ny+1
              ddx = Hz(1:nx,i,k)
              call exec_r_to_c1_1d(plans(1),ddx,fftx)
              c0   = fftx(2:sx)
              c1   = -cmplx(0.0_dp,kx(2:sx),kind=dp)*c0
              fftx(2:sx) = c1
              call exec_c_to_r1_1d(plans(2),fftx,ddx)
              ddx = ddx*inx
              Ey(1:nx,i,k) = Ey(1:nx,i,k)-dte*ddx
              ddx = Hy(1:nx,i,k)
              call exec_r_to_c1_1d(plans(1),ddx,fftx)
              c0   = fftx(2:sx)
              c1   = -cmplx(0.0_dp,kx(2:sx),kind=dp)*c0
              fftx(2:sx) = c1
              call exec_c_to_r1_1d(plans(2),fftx,ddx)
              ddx = ddx*inx
              Ez(1:nx,i,k) = Ez(1:nx,i,k)+dte*ddx
           end do
       end do
!$omp end parallel do


      Ey(nx+1,:,:) = Ey(1,:,:)
      Ez(nx+1,:,:) = Ez(1,:,:)

      if(present(jx) .and. present(jy) .and. present(jz)) then
         ex = ex-dte*jx
         ey = ey-dte*jy
         ez = ez-dte*jz
      end if
      
  end subroutine solve_ampere                         

  
  
  
end module solver_pstd_3d
