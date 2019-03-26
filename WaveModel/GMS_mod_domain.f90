module GMS_mod_domain

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
! Modified by Bernard Gingold (beniekg@gmail.com) on 21/03/2019
! Removing most of the Object-Oriented crap
!
    use GMS_mod_kinds,    only : int32_t, dp
    use GMS_mod_spectrum, only : spectrum_type
    use GMS_mod_grid,     only : grid_type
    use GMS_mod_constants
    use GMS_mod_datetime, only : datetime,timedelta
    implicit none

    private

    type, public :: domain_type

          public
          character(len=:), allocatable, type_name
!DIR$     ATTRIBUTES ALIGN : 64 :: spectrum
          type(spectrum_type), allocatable, dimension(:,:) :: spectrum
          type(date_time) :: start_time
          type(date_time) :: end_time
          type(timedelta) :: time_step
!DIR$     ATTRIBUTES ALIGN : 64 :: dx
          real(kind=dp), allocatable, dimension(:,:)     :: dx
!DIR$     ATTRIBUTES ALIGN : 64 :: dy
          real(kind=dp), allocatable, dimension(:,:)     :: dy
!DIR$     ATTRIBUTES ALIGN : 64 :: u
          real(kind=dp), allocatable, dimension(:,:)     :: u
!DIR$     ATTRIBUTES ALIGN : 64 :: v
          real(kind=dp), allocatable, dimension(:,:)     :: v
!DIR$     ATTRIBUTES ALIGN : 64 :: u
          real(kind=dp), allocatable, dimension(:,:)     :: z
!DIR$     ATTRIBUTES ALIGN : 64 :: eta
          real(kind=dp), allocatable, dimension(:,:)     :: eta
!DIR$     ATTRIBUTES ALIGN : 64 :: depth
          real(kind=dp), allocatable, dimension(:,:)     :: depth
          integer(kind=int32_t), dimension(2) :: lb
          integer(kind=int32_t), dimension(2) :: ub
          integer(kind=int32_t) :: nfreqs
          integer(kind=int32_t) :: ndirs
          logical(kind=int32_t) :: shallow_water_mode

    end type domain_type

    contains

    subroutine constructor(domain,grid,spectrum,shallow_water_mode,  &
                           errstate,iounit,logging,verbose,append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: constructor
          use GMS_mod_print_error, only : handle_fatal_memory_error
          type(domain_type),        intent(inout) :: domain
          type(grid_type),          intent(in)    :: grid
          type(spectrum_type),      intent(in)    :: spectrum
          logical(kind=int32_t),    intent(in), optional    :: shallow_water_mode
          logical(kind=int32_t),    intent(inout) :: errstate
          integer(kind=int32_t),    intent(in)    :: iounit
          logical(kind=int32_t),    intent(in)    :: logging
          logical(kind=int32_t),    intent(in)    :: verbose
          logical(kind=int32_t),    intent(in)    :: append
          character(len=*),         intent(in)    :: fname
          ! LOcals
          character(len=256),    automatic :: emsg
          integer(kind=int32_t), autimatic :: aerr
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ....
          if(present(shallow_water_mode)) then
                domain.shallow_water_mode = shallow_water_mode
          else
                domain.shallow_water_mode = .false.
          end if
          domain.type_name = 'domain_type'
          domain.grid = grid
          domain.lb   = grid.lb
          domain.ub   = grid.ub
          domain.dx   = grid.dx
          domain.dy   = grid.dy
          if(allocated(domain.spectrum)) then
                deallocate(domain.spectrum)
                allocate(domain.spectrum(domain.lb(1):domain.ub(1), &
                                         domain.lb(2):domain.ub(2), &
                                         STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
          else
                allocate(domain.spectrum(domain.lb(1):domain.ub(1), &
                                         domain.lb(2):domain.ub(2), &
                                         STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
          end if
!DIR$     VECTOR ALIGNED
          do concurrent(i=domain.lb(1):domain.ub(1), &
                        j=domain.lb(2):domain.ub(2))
                    domain.spectrum(i,j) = spectrum
          end do
          if(allocated(domain.u)) then
                deallocate(domain.u)
                allocate(domain.u(domain.lb(1):domain.ub(1), &
                                  domain.lb(2):domain.ub(2), &
                                  STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           else
                allocate(domain.u(domain.lb(1):domain.ub(1), &
                                  domain.lb(2):domain.ub(2), &
                                  STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           end if
           domain.u = 0.0_dp
           if(allocated(domain.v)) then
                deallocate(domain.v)
                allocate(domain.v(domain.lb(1):domain.ub(1), &
                                  domain.lb(2):domain.ub(2), &
                                  STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           else
                allocate(domain.v(domain.lb(1):domain.ub(1), &
                                  domain.lb(2):domain.ub(2), &
                                  STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           end if
           domain.v = 0.0_dp
           if(allocated(domain.eta)) then
                deallocate(domain.eta)
                allocate(domain.eta(domain.lb(1):domain.ub(1), &
                                    domain.lb(2):domain.ub(2), &
                                    STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           else
                allocate(domain.eta(domain.lb(1):domain.ub(1), &
                                    domain.lb(2):domain.ub(2), &
                                    STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           end if
           domain.eta = 0.0_dp
           if(allocated(domain.eta)) then
                deallocate(domain.depth)
                allocate(domain.depth(domain.lb(1):domain.ub(1), &
                                    domain.lb(2):domain.ub(2), &
                                    STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           else
                allocate(domain.depth(domain.lb(1):domain.ub(1), &
                                    domain.lb(2):domain.ub(2), &
                                    STAT=aerr,ERRMSG=emsg))
                if(aerr /= 0) goto 9999
           end if
           domain.nfreqs = size(spectrum.f)
           domain.ndirs  = size(spectrum.th)
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_domain, subroutine: constructor -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_domain, subroutine: constructor -- Memory Allocation Failure !!", &
                              emsg,155)
    end subroutine constructor

    subroutine advec1dRank1(domain,advection_method,halo_width,directional_type)
 !! Computes the advective tendency for the domain instance given the desired
  !! advection method as an input function and the number of halo cells. This
  !! function works only in cases where `ndirs == 1`.
  !!
  !! This implementation accepts the methods that operate on spectrum arrays
  !! of rank 1 (omnidirectional) in 1-dimensional space:
  !!
  !!   * advectUpwind1stOrder1dRank1
  !!   * advectCentered2ndOrder1dRank1
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: advec1dRank1
          type(domain_type),       intent(inout) :: domain
          interface
                subroutine advection_method(f,u,dx,tendency)
                    import :: dp
                    !DIR$ ASSUME_ALIGNED f:64
                    real(kind=dp), dimension(:,:), intent(in)  :: f
                    !DIR$ ASSUME_ALIGNED u:64
                    real(kind=dp), dimension(:,:), intent(in)  :: u
                    !DIR$ ASSUME_ALIGNED dx:64
                    real(kind=dp), dimension(:),   intent(in)  :: dx
                    !DIR$ ASSUME_ALIGNED tendency:64
                    real(kind=dp), dimension(:,:), allocatable, intent(out) :: tendency
                end subroutine advection_mathod
           end interface
           integer(kind=int32_t),   intent(in) :: halo_width
           integer(kind=int32_t), dimension(:), intent(in) :: directional_type
           ! Locals
           integer(kind=int32_t), automatic :: idm
!DIR$   ATTRIBUTES ALIGN : 64 :: f
           real(kind=dp), allocatable, dimension(:,:)     :: f
!DIR$   ATTRIBUTES ALIGN : 64 :: cg
           real(kind=dp), allocatable, dimension(:,:)     :: cg
!DIR$   ATTRIBUTES ALIGN : 64 :: dx
           real(kind=dp), allocatable, dimension(:)       :: dx
!DIR$   ATTRIBUTES ALIGN : 64 :: tendency
           real(kind=dp), allocatable, dimension(:,:)     :: tendency
!DIR$   ATTRIBUTES ALIGN : 64 :: spectrum_array
           real(kind=dp), allocatable, dimension(:,:,:,:) :: spectrum_array
!DIR$   ATTRIBUTES ALIGN : 64 :: gs
           real(kind=dp), allocatable, dimension(:,:,:)   :: gs
!DIR$   ATTRIBUTES ALIGN : 64 :: gsxh
           real(kind=dp), allocatable, dimension(:,:)     :: gsxh
           associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                   idm = ub(1)-lb(1)+1+2*hw
                   call getSpectrumArray(domain,spectrum_array,[hw,0],.true.)
                   f = reshape(spectrum_array,[domain.nfreqs,idm])
                   call getGroupSpeed(domain,gs,[hw,0],.true.)
                   cg = reshape(gs,[domain.nfreqs,idm])
                   call getGridSpacingXWithHallo(domain,gshx,[hw,0],.true.)
                   dx = reshape(gshx,[idm])
                   call advection_method(f,u,dx,tendency)
                   call setSpectrumArray1d1d(domain,tendency)
            end associate
    end subroutine advec1dRank1

    subroutine advect1dRank2(domain,advection_method,halowidth,directional_type)
 !! Computes the advective tendency for the domain instance given the desired
  !! advection method as an input function and the number of halo cells. This
  !! function works both when `ndirs == 1` (omnidirectional) and when
  !! `ndirs > 1` (directional).
  !!
  !! This implementation accepts the methods that operate on spectrum arrays
  !! of rank 2 (directional) in 1-dimensional space:
  !!
  !!   * advectUpwind1stOrder1dRank2
  !!   * advectCentered2ndOrder1dRank2
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: advec1dRank2
          type(domain_type),        intent(inout) :: domain
          interface
                subroutine advection_method(f,u,dx,tendency)
                    import :: dp
                    !DIR$   ASSUME_ALIGNED f:64
                    real(kind=dp), dimension(:,:,:), intent(in) :: f
                    !DIR$   ASSUME_ALIGNED u:64
                    real(kind=dp), dimension(:,:,:), intent(in) :: u
                    !DIR$   ASSUME_ALIGNED dx:64
                    real(kind=dp), dimension(:),     intent(in) :: dx
                    !DIR$   ASSUME_ALIGNED tendency:64
                    real(kind=dp), dimension(:,:,:), intent(inout) :: tendency
                 end subroutine advection_method
           end interface
           integer(kind=int32_t),      intent(in)  :: halowidth
           integer(kind=int32_t), dimension(:,:), intent(in) :: directional_type
           ! Locals
           integer(kind=int32_t), automatic :: idm
!DIR$ ATTRIBUTES ALIGN : 64 :: f
           real(kind=dp),dimension(:,:,:),allocatable       :: f
!DIR$ ATTRIBUTES ALIGN : 64 :: cg
           real(kind=dp),dimension(:,:,:),allocatable       :: cg
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
           real(kind=dp),dimension(:),allocatable           :: dx
!DIR$   ATTRIBUTES ALIGN : 64 :: tendency
           real(kind=dp), allocatable, dimension(:,:,:)     :: tendency
!DIR$   ATTRIBUTES ALIGN : 64 :: spectrum_array
           real(kind=dp), allocatable, dimension(:,:,:,:) :: spectrum_array
!DIR$   ATTRIBUTES ALIGN : 64 :: gs
           real(kind=dp), allocatable, dimension(:,:,:)   :: gs
!DIR$   ATTRIBUTES ALIGN : 64 :: gsxh
           real(kind=dp), allocatable, dimension(:,:)     :: gsxh
           ! Exec code ...
           associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                idm = ub(1)-lb(1)+1+2*hw
                call getSpectrumArray(domain,spectrum_array,[hw,0],.true.)
                f = reshape(spectrum_array,[domain.nfreqs,domain.ndirs,idm])
                call getGroupSpeed(domain,gs,[hw,0],.true.)
                cg = reshape(gs,[domain.nfreqs,domain.ndirs,idm])
                call getGridSpacingXWithHalo(domain,gshx,[hw,0],.true.)
                dx = reshape(gshx,[idm])
                call advection_method(f,u,dx,tendency)
                call setSpectrumArray1d2d(domain,tendency)
            end associate
    end subroutine advect1dRank2

    subroutine advec2dRank2(domain,advection_method,halowidth)
 !! Computes the advective tendency for the domain instance given the desired
  !! advection method as an input function and the number of halo cells. This
  !! function works both when `ndirs == 1` (omnidirectional) and when
  !! `ndirs > 1` (directional).
  !!
  !! This implementation accepts the methods that operate on spectrum arrays
  !! of rank 2 (directional) in 2-dimensional space:
  !!
  !!   * advectUpwind1stOrder2dRank2
  !!   * advectCentered2ndOrder2dRank2
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: advec2dRank2
          type(domain_type),        intent(inout) :: domain
          interface
                subroutine advection_method(f,u,v,dx,dy,tendency)
                    import :: dp
!DIR$   ASSUME_ALIGNED f:64
                    real(kind=dp),  dimension(:,:,:,:), intent(in) :: f
!DIR$   ASSUME_ALIGNED u:64
                    real(kind=dp),  dimension(:,:,:,:), intent(in) :: u
!DIR$   ASSUME_ALIGNED v:64
                    real(kind=dp),  dimension(:,:,:,:), intent(in) :: v
!DIR$   ASSUME_ALIGNED dx:64
                    real(kind=dp),  dimension(:,:),     intent(in) :: dx
!DIR$   ASSUME_ALIGNED dy:64
                    real(kind=dp),  dimension(:,:),     intent(in) :: dy
                    real(kind=dp), allocatable, dimension(:,:,:,:), intent(out) :: tendency
                 end subroutine
          end interface
          integer(kind=int32_t), dimension(:), intent(in) :: halowidth
          ! LOcals
          integer(kind=int32_t), automatic :: idm,jdm,n
!DIR$ ATTRIBUTES ALIGN : 64 :: f
          real(kind=dp),dimension(:,:,:,:),allocatable :: f
!DIR$ ATTRIBUTES ALIGN : 64 :: cg
          real(kind=dp),dimension(:,:,:),allocatable :: cg
!DIR$ ATTRIBUTES ALIGN : 64 :: cgx
          real(kind=dp),dimension(:,:,:,:),allocatable :: cgx
!DIR$ ATTRIBUTES ALIGN : 64 :: cgy
          real(kind=dp),dimension(:,:,:,:),allocatable :: cgy
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
          real(kind=dp),dimension(:,:),allocatable :: dx
!DIR$ ATTRIBUTES ALIGN : 64 :: dy
          real(kind=dp),dimension(:,:),allocatable :: dy
!DIR$ ATTRIBUTES ALIGN : 64 :: theta
          real(kind=dp),dimension(:),allocatable :: theta
!DIR$ ATTIRBUTES ALIGN : 64 :: spectrum_array
          real(kind=dp), allocatable, dimension(:,:,:,:) ::  spectrum_array
          ! Exec code .....
          associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                theta = domain.spectrum(1,1).th
                idm   = ub(1)-lb(1)+1+2*hw(1)
                jdm   = ub(2)-lb(2)+1+2*hw(2)
                call getSpectrumArray(domain,spectrum_array,hw,.true.)
                f = reshape(spectrum_array,[domain.nfreqs,domain.ndirs,[idm,jdm]])
                allocate(cgx(domain.nfreqs,domain.ndirs,idm,jdm))
                allocate(cgy(domain.nfreqs,domain.ndirs,idm,jdm)
                call getGroupSpeed(domain,cg,hw,.true.)
                do concurrent(n=1:domain.ndirs)
                        cgx(:,n,:,:) = cos(theta(n))*cg
                        cgy(:,n,:,:) = sin(theta(n))*cg
                end do
                call getGridSpacingXWithHalo(domain,dx,hw,.true.)
                call getGridSpacingYWIthHalo(domain,dy,hw,.true.)
                call advection_mathod(f,cgx,cgy,dx,dy,tendency)
                call setSpectrumArray2d2d(domain,tendency)
           end associate
    end subroutine

!DIR$   ATTRIBUTES INLINE :: isAllocated
    pure logical(kind=int32_t) function isAllocated(domain)
!DIR$   ATTRIBUTES CODE_ALIGN:32
          type(domain_type),        intent(in) :: domain
          ! EXec code ....
          isAllocated = allocated(domain.spectrum)
    end function isAllocated

    subroutine getCurrent_u(domain,u,lb1,ub1,lb2,ub2,kdm)
  !! Returns the 3-d array with values of Eulerian velocity (mean current) in
  !! x-direction [m/s].
  !!
  !! Note: this implementation assumes that all u and v velocity arrays in
  !! the domain instance are of same length in depth, such that the resulting
  !! u and v arrays are regular 3-d arrays.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getCurrent_u
          type(domain_type),        intent(in) :: domain
!DIR$   ASSUME_ALIGNED u:64
          real(kind=dp), dimension(lb1:ub1,lb2:ub2,:kdm), intent(out) :: u
          integer(kind=int32_t),    intent(in) :: lb1
          integer(kind=int32_t),    intent(in) :: ub1
          integer(kind=int32_t),    intent(in) :: lb2
          integer(kind=int32_t),    intent(in) :: ub2
          integer(kind=int32_t),    intent(in) :: kdm
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! EXec code ....
          do concurrent(i=lb1:ub1,j=lb2:ub2)
                u(i,j,:) = domain.spectrum(i,j).u
          end do
    end subroutine getCurrent_u

    subroutine getCurrent_v(domain,v,lb1,ub1,lb2,ub2,kdm)
!! Returns the 3-d array with values of Eulerian velocity (mean current) in
  !! y-direction [m/s].
  !!
  !! Note: this implementation assumes that all u and v velocity arrays in
  !! the domain instance are of same length in depth, such that the resulting
  !! u and v arrays are regular 3-d arrays.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getCurrent_v
          type(domain_type),    intent(in) :: domain
!DIR$   ASSUME_ALIGNED v:64
          real(kind=dp), dimension(lb1:ub1,lb2:ub2,kdm), intent(out) :: v
          integer(kind=int32_t),    intent(in) :: lb1
          integer(kind=int32_t),    intent(in) :: ub1
          integer(kind=int32_t),    intent(in) :: lb2
          integer(kind=int32_t),    intent(in) :: ub2
          integer(kind=int32_t),    intent(in) :: kdm
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! EXec code ....
          do concurrent(i=lb1:ub1,j=lb2:ub2)
                v(i,j,:) = domain.spectrum(i,j).v
          end do
    end subroutine getCurrent_v

  subroutine getSpectrumArray(domain,spectrum_array,halowidth,periodic)
 !! Returns a 4-dimensional spectrum array, where the first two dimensions are
  !! frequency and directional dimensions and the second two are spatial x and y
  !! dimensions.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getSpectrumArray
          type(domain_type),                   intent(in) :: domain
          real(kind=dp), allocatable, dimension(:,:,:,:), intent(out) :: spectrum_array
          integer(kind=int32_t), dimension(2), intent(in) :: halowidth
          logical(kind=int32_t),               intent(in) :: periodic
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          integer(kind=int32_t), automatic :: nfreqs,ndirs
          ! EXec code ...
          if(allocated(spectrum_array)) return
          associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
              nfreqs = size(domain.spectrum(1,1).spec,dim=1)
              ndirs  = size(domain.spectrum(1,1).spec,dim=2)
              allocate(spectrum_array(nfreqs,ndirs,lb(1)-hw(1):ub(1)+hw(1), &
                                                   lb(2)-hw(2):ub(2)+hw(2)))
              do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                    spectrum_array(:,:,i,j) = domain.spectrum.spec
              end do
              if(periodic) then
                     spectrum_array(:,:,lb(1)-hw(1):lb(1)-1,:)&
                            = spectrum_array(:,:,ub(1)-hw(1)+1:ub(1),:)
                     spectrum_array(:,:,ub(1)+1:ub(1)+hw(1),:)&
                            = spectrum_array(:,:,lb(1):lb(1)+hw(1)-1,:)
                     spectrum_array(:,:,:,lb(2)-hw(2):lb(2)-1)&
                            = spectrum_array(:,:,:,ub(2)-hw(2)+1:ub(2))
                     spectrum_array(:,:,:,ub(2)+1:ub(2)+hw(2))&
                            = spectrum_array(:,:,:,lb(2):lb(2)+hw(2)-1)
              end if
          end associate
  end subroutine getSpectrumArray

  subroutine getGroupSpeed(domain,cg,halowidth,periodic)
!! Returns a 3-d array with group speed values [m/s].
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getGroupSpeed
          type(domain_type),                   intent(in) :: domain
          real(kind=dp), allocatable, dimension(:,:,:), intent(out) :: cg
          integer(kind=int32_t), dimension(2), intent(in) :: halowidth
          logical(kind=int32_t),               intent(in) :: periodic
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ....
          if(allocated(cg)) return
          associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                allocate(cg(domain.nfreqs,lb(1)-hw(1):ub(1)+hw(1), &
                                          lb(2)-hw(2):ub(2)+hw(2)))
                do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                        cg(:,i,j) = domain.spectrum(i,j).cg
                end do
                if(periodic) then
                      cg(:,lb(1)-hw(1):lb(1)-1,:) = cg(:,ub(1)-hw(1)+1:ub(1),:)
                      cg(:,ub(1)+1:ub(1)+hw(1),:) = cg(:,lb(1):lb(1)+hw(1)-1,:)
                      cg(:,:,lb(2)-hw(2):lb(2)-1) = cg(:,:,ub(2)-hw(2)+1:ub(2))
                      cg(:,:,ub(2)+1:ub(2)+hw(2)) = cg(:,:,lb(2):lb(2)+hw(2)-1)
                 end if
           end associate
  end subroutine getGroupSpeed

  subroutine getPhaseSpeed(domain,cg,nfreqs,lb1,ub1,lb2,ub2)
!! Returns a 3-d array with phase speed values [m/s].
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getPhaseSpeed
          type(domain_type),        intent(in) :: domain
!DIR$   ASSUME_ALIGNED cg:64
          real(kind=dp), dimension(nfreqs,lb1:ub1,lb2:ub2), intent(out) :: cg
          integer(kind=int32_t),    intent(in) :: nfreqs,lb1,ub1,lb2,ub2
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! EXec code ....
          do concurrent(i=lb1:ub1,j=lb2:ub2)
                cp(:,i,j) = domain.spectrum(i,j).cp
          end do
  end subroutine getPhaseSpeed

  subroutine getGridSpacingXWithHalo(domain,dx,halowidth,periodic)
 !! Returns grid spacing array in x-direction including halo cells.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getGridSpacingXWithHalo
          type(domain_type),                   intent(in) :: domain
          real(kind=dp), allocatable, dimension(:,:), intent(out) :: dx
          integer(kind=int32_t), dimension(2), intent(in) :: halowidth
          logical(kind=int32_t),               intent(in) :: periodic
          ! Exec code ...
          if(allocated(dx)) return
          associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                allocate(dx(lb(1)-hw(1):ub(1)+hw(1),lb(2)-hw(2):ub(2)+hw(2)))
                dx(lb(1):ub(1),lb(2):ub(2)) = domain.dx
                if(periodic) then
                       dx(lb(1)-hw(1):lb(1)-1,:) = dx(ub(1)-hw(1)+1:ub(1),:)
                       dx(ub(1)+1:ub(1)+hw(1),:) = dx(lb(1):lb(1)+hw(1)-1,:)
                       dx(:,lb(2)-hw(2):lb(2)-1) = dx(:,ub(2)-hw(2)+1:ub(2))
                       dx(:,ub(2)+1:ub(2)+hw(2)) = dx(:,lb(2):lb(2)+hw(2)-1)
                end if
          end associate
  end subroutine getGridSpacingXWithHalo

  subroutine getGridSpacingYWithHalo(domain,dy,halowidth,periodic)
 !! Returns grid spacing array in y-direction including halo cells.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: getGridSpacingYWithHalo
          type(domain_type),                   intent(in) :: domain
          real(kind=dp), allocatable, dimension(:,:), intent(out) :: dy
          integer(kind=int32_t), dimension(2), intent(in) :: halowidth
          logical(kind=int32_t),               intent(in) :: periodic
          ! Exec code ...
          if(allocated(dy)) return
          associate(lb=>domain.lb,ub=>domain.ub,hw=>halowidth)
                allocate(dy(lb(1)-hw(1):ub(1)+hw(1),lb(2)-hw(2):ub(2)+hw(2)))
                dy(lb(1):ub(1),lb(2):ub(2)) = domain.dy
                if(periodic) then
                    dy(lb(1)-hw(1):lb(1)-1,:) = dy(ub(1)-hw(1)+1:ub(1),:)
                    dy(ub(1)+1:ub(1)+hw(1),:) = dy(lb(1):lb(1)+hw(1)-1,:)
                    dy(:,lb(2)-hw(2):lb(2)-1) = dy(:,ub(2)-hw(2)+1:ub(2))
                    dy(:,ub(2)+1:ub(2)+hw(2)) = dy(:,lb(2):lb(2)+hw(2)-1)
                end if
          end associate
  end subroutine getGridSpacingYWithHalo

!DIR$   ATTRIBUTES INLINE :: setDepth
    subroutine setDepth(domain,depth,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setDepth

          type(domain_type),                       intent(inout)   :: domain
!DIR$   ASSUME_ALIGNED depth:64
          real(kind=dp),    dimension(lb1:ub1,lb2:ub2), intent(in) :: depth
          integer(kind=int32_t),          intent(in)               :: lb1,ub1, &
                                                                      lb2,ub2
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code....

         do i=lb1,ub1
            do j=lb2,ub2
                domain.spectrum(i,j).depth = depth(i,j)
           end do
         end do
    end subroutine setDepth

!DIR$   ATTRIBUTES INLINE :: setElevation
    subroutine setElevation(domain,elevation,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setElevation
          type(domain_type),              intent(inout) :: domain
!DIR$   ASSIME_ALIGNED elevation:64
          real(kind=dp),    dimension(lb1:ub1,lb2:ub2), intent(in)    :: elevation
          integer(kind=int32_t),          intent(in)    :: lb1,ub1, &
                                                           lb2,ub2
          ! LOcals
          integer(kind=int32_t), automatic :: i,j
          ! EXec code ....
          do i=lb1, ub1
               do j=lb2,ub2
                    domain.spectrum(i,j).elevation = elevation(i,j)
               end do
          end do
    end subroutine setElevation

!DIR$ ATTRIBUTES INLINE :: setGravity
    subroutine setGravity(domain,gravity,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setGravity
          type(domain_type),        intent(inout) :: domain
!DIR$   ASSUME_ALIGNED gravity:64
          real(kind=dp),    dimension(length), intent(in) :: gravity
          integer(kind=int32_t),               intent(in) :: lb1,ub1, &
                                                             lb2,ub2
          ! LOcals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ....
          do i=lb1, ub1
                do j=lb2,ub2
                    domain.spectrum(i,j).gravity = gravity(i,j)
                end do
          end do
    end subroutine setGravity

!DIR$   ATTRIBUTES INLINE :: setSurfaceTension
    subroutine setSurfaceTension(domain,st,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setSurfaceTension
          type(domain_type),        intent(inout) :: domain
!DIR$   ASSUME_ALIGNED st:64
          real(kind=dp),    dimension(lb1:ub1,lb2:ub2), intent(in) :: st
          integer(kind=int32_t),                        intent(in) :: lb1,ub1, &
                                                                      lb2,ub2
          ! LOcals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ....
          do i=lb1,ub1
                do j=lb2,ub2
                    domain.spectrum(i,j).surface_tension = st(i,j)
                end do
          end do
    end subroutine setSurfaceTension

!DIR$   ATTRIBUTES INLINE :: setAirDensity
    subroutine setAirDensity(domain,ad,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setAirDensity
          type(domain_type),            intent(inout) :: domain
!DIR$   ASSUME_ALIGNED ad:64
          real(kind=dp),    dimension(lb1:ub1,lb2:ub2), intent(in) :: ad

          integer(kind=int32_t),                        intent(in) :: lb1,ub1, &
                                                                      lb2,ub2
          ! LOcals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ...
          do i=lb1,ub1
                do j=lb2,ub2
                    domain.spectrum(i,j).air_density = ad(i,j)
                end do
          end do
    end subroutine setAirDensity

!DIR$   ATTRIBUTES INLINE :: setWaterDensity
    subroutine setWaterDensity(domain,wd,lb1,ub1,lb2,ub2)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setWaterDensity
          type(domain_type),            intent(inout) :: domain
!DIR$   ASSUME_ALIGNED wd:64
          real(kind=dp),      dimension(lb1:ub1,lb2:ub2), intent(in)   :: wd
          integer(kind=int32_t),                          intent(in)   :: lb1,ub1, &
                                                                           lb2,ub2
          ! LOcals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code ...
          do i=lb1,ub1
                do j=lb2,ub2
                      domain.spectrum(i,j).water_density = wd(i,j)
                end do
          end do
    end subroutine setWaterDensity

    subroutine setSpectrumArray1d1d(domain,spectrum_array)
 !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for omnidirectional spectrum in 1-d space (1d-1d)
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setSpectrumArray1d1d
          type(domain_type),            intent(inout) :: domain
!DIR$   ASSUME_ALIGNED spectrum_array:64
          real(kind=dp),    dimension(:,:),  intent(in) :: spectrum_array
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          ! Exec code....
          associate(lb=>domain.lb,ub=>domain.ub)
               do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                    domain.spectrum(i,j).spec(:,i) = spectrum_array(:,i)
               end do
          end associate
    end subroutine setSpectrumArray1d1d

    subroutine setSpectrumArray1d2d(domain,spectrum_array)
 !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for setting 1-d spectrum into 2-d physical space
  !! of 2-d spectrum into 1-d physical space.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: setSpectrumArray1d2d
          type(domain_type),        intent(inout)  :: domain
!DIR$   ASSUME_ALIGNED spectrum_array:64
          real(kind=dp),     dimension(:,:,:), intent(in) :: spectrum_array
          ! Locals
          integer(kind=int32_t), automatic :: i,j
          associate(lb=>domain.lb,ub=>domain.ub)
                if(lb(2) == ub(2)) then
                       do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                             domain.spectrum(i,j).spec(:,:) = spectrum_array(:,:,i)
                       end do
                else
                       do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                                domain.spectrum(i,j).spec(:,1) = spectrum_array(:,i,j)
                       end do
                end if
           end associate
    end subroutine setSpectrumArray1d2d

    subroutine setSpectrumArray2d2d(domain,spectrum_array)
 !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for directional spectrum in 2-d space (2d-2d)
!DIR$   ATTRIBUTES CODE_ALIGN:32
          type(domain_type),        intent(inout) :: domain
!DIR$   ASSUME_ALIGNED spectrum_array:64
          real(kind=dp),dimension(:,:,:,:),intent(in) :: spectrum_array
    !! Spectrum array
          integer(kind=32_t) :: i,j
          associate(lb => self % lb,ub => self % ub)
                do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
                    domain.spectrum(i,j).spec(:,:) = spectrum_array(:,:,i,j)
                end do
          end associate
    end subroutine setSpectrumArray2d2d

end module GMS_mod_domain
