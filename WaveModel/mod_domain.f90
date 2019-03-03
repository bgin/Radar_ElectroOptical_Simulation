!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.

module mod_domain

!use mod_precision,only : ik => intkind,rk => realkind
use mod_kinds, only : int4, dp
use mod_spectrum,only : spectrum_type
use mod_grid,only : grid_type
use mod_const
use mod_datetime ,only : datetime,timedelta
!use json_module,only : json_core,json_file,json_value

implicit none

private

public :: domain_type

type :: domain_type

  private

  character(len=:),allocatable :: type_name

  type(grid_type) :: grid
!DIR$ ATTRIBUTES ALIGN : 64 :: spectrum
  type(spectrum_type),dimension(:,:),allocatable :: spectrum

  logical :: shallow_water_mode

  type(datetime) :: start_time !! Simulation start time
  type(datetime) :: end_time   !! Simulation end time
  type(timedelta) :: time_step !! Time step [s]
!DIR$ ATTRIBUTES ALIGN : 64 ::  dx
  real(kind=dp),dimension(:,:),allocatable :: dx ! grid spacing in x-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 ::  dy
  real(kind=dp),dimension(:,:),allocatable :: dy ! grid spacing in y-direction [m]
!DIR$ ATTRIBUTES ALIGN : 64 ::  u
  real(kind=dp),dimension(:,:),allocatable :: u ! x-component of velocity [m/s]
!DIR$ ATTRIBUTES ALIGN : 64 :: v
  real(kind=dp),dimension(:,:),allocatable :: v ! y-component of velocity [m/s]
!DIR$ ATTRIBUTES ALIGN : 64 :: eta
  real(kind=dp),dimension(:,:),allocatable :: eta ! surface elevation [m]
!DIR$ ATTRIBUTES ALIGN : 64 :: depth
  real(kind=dp),dimension(:,:),allocatable :: depth ! mean water depth [m]

  integer(kind=int4),dimension(2) :: lb ! lower bounds in geographical space
  integer(kind=int4),dimension(2) :: ub ! upper bounds in geographical space

  integer(kind=int4) :: nfreqs ! number of frequencies
  integer(kind=int4) :: ndirs ! number of directions

  contains

  ! Public type-bound methods
  procedure,public,pass(self) :: frequencyMoment
  procedure,public,pass(self) :: getCurrent_u
  procedure,public,pass(self) :: getCurrent_v
  procedure,public,pass(self) :: getGravity
  procedure,public,pass(self) :: getGrid
  procedure,public,pass(self) :: getGridSpacingXWithHalo
  procedure,public,pass(self) :: getGridSpacingYWithHalo
  procedure,public,pass(self) :: getDepth
  procedure,public,pass(self) :: getElevation
  procedure,public,pass(self) :: getFrequency
  procedure,public,pass(self) :: getDirections
  procedure,public,pass(self) :: getLowerBounds
  procedure,public,pass(self) :: getUpperBounds
  procedure,public,pass(self) :: getSpectrum
  procedure,public,pass(self) :: getSpectrumArray
  procedure,public,pass(self) :: getPhaseSpeed
  procedure,public,pass(self) :: getGroupSpeed
  procedure,public,pass(self) :: getSurfaceTension
  procedure,public,pass(self) :: getAirDensity
  procedure,public,pass(self) :: getWaterDensity
  procedure,public,pass(self) :: isAllocated
  procedure,public,pass(self) :: meanPeriod
  procedure,public,pass(self) :: meanPeriodZeroCrossing
  procedure,public,pass(self) :: setDepth
  procedure,public,pass(self) :: setElevation
  procedure,public,pass(self) :: setGravity
  procedure,public,pass(self) :: setSurfaceTension
  procedure,public,pass(self) :: setAirDensity
  procedure,public,pass(self) :: setWaterDensity
  procedure,public,pass(self) :: significantWaveHeight
  procedure,public,pass(self) :: wavenumberMoment
  !procedure,public,pass(self) :: writeJSON

  ! Specific procedures overloaded by generic procedures and operators
  procedure,private,pass(self) :: advect1dRank1
  procedure,private,pass(self) :: advect1dRank2
  procedure,private,pass(self) :: advect2dRank2
  procedure,private,pass(self) :: assign_spectrum_array_1d
  procedure,private,pass(self) :: assign_spectrum_array_2d
  procedure,private,pass(self) :: domain_add_domain
  procedure,private,pass(self) :: domain_add_real
  procedure,private,pass(self) :: domain_sub_domain
  procedure,private,pass(self) :: domain_sub_real
  procedure,private,pass(self) :: domain_mult_domain
  procedure,private,pass(self) :: domain_mult_real
  procedure,private,pass(self) :: domain_div_domain
  procedure,private,pass(self) :: domain_div_real
  procedure,private,pass(self) :: domain_unary_minus
  procedure,private,pass(self) :: real_add_domain
  procedure,private,pass(self) :: real_sub_domain
  procedure,private,pass(self) :: real_mult_domain
  procedure,private,pass(self) :: real_div_domain
  procedure,private,pass(self) :: eq
  procedure,private,pass(self) :: neq
  procedure,private,pass(self) :: setSpectrum1d
  procedure,private,pass(self) :: setSpectrum2d
  procedure,private,pass(self) :: setSpectrumArray1d1d
  procedure,private,pass(self) :: setSpectrumArray1d2d
  procedure,private,pass(self) :: setSpectrumArray2d2d

  ! Generic procedures
  generic,public :: advect => advect1dRank1,&
                              advect1dRank2,&
                              advect2dRank2
  generic,public :: setSpectrum => setSpectrum1d,&
                                   setSpectrum2d
  generic,public :: setSpectrumArray => setSpectrumArray1d1d,&
                                        setSpectrumArray1d2d,&
                                        setSpectrumArray2d2d

  ! Generic operators
  generic :: assignment(=) => assign_spectrum_array_1d,&
                              assign_spectrum_array_2d
  generic :: operator(+) => domain_add_domain,&
                            domain_add_real,&
                            real_add_domain
  generic :: operator(-) => domain_sub_domain,&
                            domain_sub_real,&
                            domain_unary_minus,&
                            real_sub_domain
  generic :: operator(*) => domain_mult_domain,&
                            domain_mult_real,&
                            real_mult_domain
  generic :: operator(/) => domain_div_domain,&
                            domain_div_real,&
                            real_div_domain
  generic :: operator(==) => eq
  generic :: operator(/=) => neq

endtype domain_type

interface domain_type
  module procedure :: constructor
endinterface domain_type

contains
!-------------------------------------------------------------------------------
type(domain_type) function constructor(grid,spectrum,shallow_water_mode) result(domain)

  !! Constructor function for the domain object.

  type(grid_type),intent(in) :: grid
    !! Input `grid` instance
  type(spectrum_type),intent(in) :: spectrum
    !! Input `spectrum` instance
  logical,intent(in),optional :: shallow_water_mode
    !! Logical switch to enable shallow water solver

  integer(kind=int4) :: i,j

  if(present(shallow_water_mode))then
    domain % shallow_water_mode = shallow_water_mode
  else
    domain % shallow_water_mode = .false.
  endif

  domain % type_name = 'domain_type'
  domain % grid = grid

  domain % lb = grid % getLowerBounds()
  domain % ub = grid % getUpperBounds()
  domain % dx = grid % getGridSpacingX()
  domain % dy = grid % getGridSpacingY()

  allocate(domain % spectrum(domain % lb(1):domain % ub(1),&
                             domain % lb(2):domain % ub(2)))
!DIR$ VECTOR ALIGNED
  do concurrent(i = domain % lb(1):domain % ub(1),&
                j = domain % lb(2):domain % ub(2))
    domain % spectrum(i,j) = spectrum
  enddo

  allocate(domain % u(domain % lb(1):domain % ub(1),&
                      domain % lb(2):domain % ub(2)))
  domain % u = 0.0_dp
 
  allocate(domain % v(domain % lb(1):domain % ub(1),&
                      domain % lb(2):domain % ub(2)))
  domain % v = 0.0_dp

  allocate(domain % eta(domain % lb(1):domain % ub(1),&
                        domain % lb(2):domain % ub(2)))
  domain % eta = 0.0_dp

  allocate(domain % depth(domain % lb(1):domain % ub(1),&
                          domain % lb(2):domain % ub(2)))

  domain % nfreqs = size(spectrum % getFrequency())
  domain % ndirs = size(spectrum % getDirections())

endfunction constructor
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure type(domain_type) function advect1dRank1(self,advection_method,halowidth,&
  directional_type) result(adv)
  !! Computes the advective tendency for the domain instance given the desired
  !! advection method as an input function and the number of halo cells. This 
  !! function works only in cases where `ndirs == 1`.
  !!
  !! This implementation accepts the methods that operate on spectrum arrays
  !! of rank 1 (omnidirectional) in 1-dimensional space:
  !!
  !!   * advectUpwind1stOrder1dRank1
  !!   * advectCentered2ndOrder1dRank1
  class(domain_type),intent(in) :: self
    !! `domain` instance
  interface
    pure function advection_method(f,u,dx) result(tendency)
      import :: dp
      real(kind=dp),dimension(:,:),intent(in) :: f
      real(kind=dp),dimension(:,:),intent(in) :: u
      real(kind=dp),dimension(:),intent(in) :: dx
      real(kind=dp),dimension(:,:),allocatable :: tendency
    endfunction advection_method
  endinterface
    !! function with the requested advection method
  integer(kind=int4),intent(in) :: halowidth
    !! number of halo cells to use in the advection method
  integer(kind=int4),dimension(:),intent(in) :: directional_type
    !! A global constant that helps resolve the interface of this specific
    !! prodedure
  integer(kind=int4) :: idm
!DIR$ ATTRIBUTES ALIGN : 64 :: f
  real(kind=dp),dimension(:,:),allocatable :: f
!DIR$ ATTRIBUTES ALIGN : 64 :: cg
  real(kind=dp),dimension(:,:),allocatable :: cg
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
  real(kind=dp),dimension(:),  allocatable :: dx
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  adv = self
  idm = ub(1)-lb(1)+1+2*hw
  f = reshape(self % getSpectrumArray([hw,0],.true.),[self % nfreqs,idm])
  cg = reshape(self % getGroupSpeed([hw,0],.true.),[self % nfreqs,idm])
  dx = reshape(self % getGridSpacingXWithHalo([hw,0],.true.),[idm])
  call adv % setSpectrumArray(advection_method(f,cg,dx))
  endassociate
endfunction advect1dRank1
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure type(domain_type) function advect1dRank2(self,advection_method,halowidth,&
  directional_type) result(adv)
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
  class(domain_type),intent(in) :: self
    !! `domain` instance
  interface
    pure function advection_method(f,u,dx) result(tendency)
      import :: dp
      real(kind=dp),dimension(:,:,:),intent(in) :: f
      real(kind=dp),dimension(:,:,:),intent(in) :: u
      real(kind=dp),dimension(:),intent(in) :: dx
      real(kind=dp),dimension(:,:,:),allocatable :: tendency
    endfunction advection_method
  endinterface
    !! function with the requested advection method
  integer(kind=int4),intent(in) :: halowidth
    !! number of halo cells to use in the advection method
  integer(kind=int4),dimension(:,:),intent(in) :: directional_type
    !! A global constant that helps resolve the interface of this specific
    !! prodedure
  integer(kind=int4) :: idm
!DIR$ ATTRIBUTES ALIGN : 64 :: f
  real(kind=dp),dimension(:,:,:),allocatable :: f
!DIR$ ATTRIBUTES ALIGN : 64 :: cg
  real(kind=dp),dimension(:,:,:),allocatable :: cg
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
  real(kind=dp),dimension(:),allocatable :: dx
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  adv = self
  idm = ub(1)-lb(1)+1+2*hw
  f = reshape(self % getSpectrumArray([hw,0],.true.),[self % nfreqs,self % ndirs,idm])
  cg = reshape(self % getGroupSpeed([hw,0],.true.),[self % nfreqs,self % ndirs,idm])
  dx = reshape(self % getGridSpacingXWithHalo([hw,0],.true.),[idm])
  call adv % setSpectrumArray(advection_method(f,cg,dx))
  endassociate
endfunction advect1dRank2
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure type(domain_type) function advect2dRank2(self,advection_method,halowidth)&
  result(adv)
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
  class(domain_type),intent(in) :: self
    !! `domain` instance
  interface
    pure function advection_method(f,u,v,dx,dy) result(tendency)
      import :: dp
      real(kind=dp),dimension(:,:,:,:),intent(in) :: f
      real(kind=dp),dimension(:,:,:,:),intent(in) :: u
      real(kind=dp),dimension(:,:,:,:),intent(in) :: v
      real(kind=dp),dimension(:,:),intent(in) :: dx
      real(kind=dp),dimension(:,:),intent(in) :: dy
      real(kind=dp),dimension(:,:,:,:),allocatable :: tendency
    endfunction advection_method
  endinterface
    !! function with the requested advection method
  integer(kind=int4),dimension(:),intent(in) :: halowidth
    !! number of halo cells to use in the advection method
  integer(kind=int4) :: idm,jdm,n
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
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  theta = self % getDirections()
  adv = self
  idm = ub(1)-lb(1)+1+2*hw(1)
  jdm = ub(2)-lb(2)+1+2*hw(2)
  f = reshape(self % getSpectrumArray(hw,.true.),[self % nfreqs,self % ndirs,[idm,jdm]])
  allocate(cgx(self % nfreqs,self % ndirs,idm,jdm))
  allocate(cgy(self % nfreqs,self % ndirs,idm,jdm))
  cg = self % getGroupSpeed(hw,.true.)
!DIR$ VECTOR ALIGNED
  do concurrent(n = 1:self % ndirs)
    cgx(:,n,:,:) = cos(theta(n))*cg
    cgy(:,n,:,:) = sin(theta(n))*cg
  enddo
  dx = self % getGridSpacingXWithHalo(hw,.true.)
  dy = self % getGridSpacingYWithHalo(hw,.true.)
  call adv % setSpectrumArray(advection_method(f,cgx,cgy,dx,dy))
  deallocate(cgx,cgy)
  endassociate
endfunction advect2dRank2
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental logical function isAllocated(self)
  !! Returns the allocation status of the domains sub-components.
  class(domain_type),intent(in) :: self !! `domain` instance
  isAllocated = allocated(self % spectrum)
endfunction isAllocated
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine assign_spectrum_array_1d(self,spectrum_array)
  !! Assigns a 1-d array of `spectrum` instances to a `domain` instance. This 
  !! procedure overloads the assignment ('=') operator.
  class(domain_type),intent(inout) :: self
    !! l.h.s. `domain` instance
  class(spectrum_type),dimension(:),intent(in) :: spectrum_array 
    !! r.h.s. array of `spectrum` instances
  call self % setSpectrum(spectrum_array)
endsubroutine assign_spectrum_array_1d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine assign_spectrum_array_2d(self,spectrum_array)
  !! Assigns a 2-d array of `spectrum` instances to a `domain` instance. This 
  !! procedure overloads the assignment ('=') operator.
  class(domain_type),intent(inout) :: self 
    !! l.h.s. `domain` instance
  class(spectrum_type),dimension(:,:),intent(in) :: spectrum_array 
    !! r.h.s. array of `spectrum` instances
  call self % setSpectrum(spectrum_array)
endsubroutine assign_spectrum_array_2d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental logical function eq(self,d2)
  !! Logical equality comparison function. Overloads the `/=` operator.
  class(domain_type),intent(in) :: self
    !! l.h.s. `domain` instance
  class(domain_type),intent(in) :: d2
    !! r.h.s. `domain` instance
  eq = all(self % getSpectrum() == d2 % getSpectrum())
endfunction eq
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental logical function neq(self,d2)
  !! Logical inequality comparison function. Overloads the `/=` operator.
  class(domain_type),intent(in) :: self
    !! l.h.s. `domain` instance
  class(domain_type),intent(in) :: d2
    !! r.h.s. `domain` instance
  neq = .not. self == d2
endfunction neq
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_add_domain(self,d2) &
  result(domain)
  !! Returns a sum of two domain instances.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  class(domain_type),intent(in) :: d2 !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() + d2 % getSpectrum()
endfunction domain_add_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_sub_domain(self,d2)&
  result(domain)
  !! Returns a difference between two domain instances.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  class(domain_type),intent(in) :: d2 !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() - d2 % getSpectrum()
endfunction domain_sub_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_unary_minus(self) result(domain)
  !! Returns a negative domain instances.
  class(domain_type),intent(in) :: self !! domain instance
  domain = self
  domain = - self % getSpectrum()
endfunction domain_unary_minus
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_mult_domain(self,d2)&
  result(domain)
  !! Returns a product of two domain instances.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  class(domain_type),intent(in) :: d2 !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() * d2 % getSpectrum()
endfunction domain_mult_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_div_domain(self,d2)&
  result(domain)
  !! Returns a division of two domain instances.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  class(domain_type),intent(in) :: d2 !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() / d2 % getSpectrum()
endfunction domain_div_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_add_real(self,a)&
  result(domain)
  !! Returns a sum of a domain instance and a real number.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  real(kind=dp),intent(in) :: a !! r.h.s. real number
  domain = self
  domain = self % getSpectrum() + a
endfunction domain_add_real
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_sub_real(self,a) result(domain)
  !! Returns a difference between a domain instance and a real number.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  real(kind=dp),intent(in) :: a !! r.h.s. real number
  domain = self
  domain = self % getSpectrum() - a
endfunction domain_sub_real
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_mult_real(self,a)&
  result(domain)
  !! Returns a product of a domain instance and a real number.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  real(kind=dp),intent(in) :: a !! r.h.s. real number
  domain = self
  domain = self % getSpectrum() * a
endfunction domain_mult_real
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function domain_div_real(self,a) result(domain)
  !! Returns a division of a domain instance and a real number.
  class(domain_type),intent(in) :: self !! l.h.s. domain instance
  real(kind=dp),intent(in) :: a !! r.h.s. real number
  real(kind=dp) :: inva
  inva = 1.0_dp / a
  domain = self
  domain = self % getSpectrum() * inva
endfunction domain_div_real
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function real_add_domain(a,self) result(domain)
  !! Returns a sum of a real number and a domain instance.
  real(kind=dp),intent(in) :: a !! l.h.s. real number
  class(domain_type),intent(in) :: self !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() + a
endfunction real_add_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function real_sub_domain(a,self) result(domain)
  !! Returns a difference between a real number and a domain instance.
  real(kind=dp),intent(in) :: a !! l.h.s. real number
  class(domain_type),intent(in) :: self !! r.h.s. domain instance
  domain = self
  domain = a - self % getSpectrum()
endfunction real_sub_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function real_mult_domain(a,self) &
  result(domain)
  !! Returns a product of a real number and a domain instance.
  real(kind=dp),intent(in) :: a !! l.h.s. real number
  class(domain_type),intent(in) :: self !! r.h.s. domain instance
  domain = self
  domain = self % getSpectrum() * a
endfunction real_mult_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure elemental type(domain_type) function real_div_domain(a,self) result(domain)
  !! Returns a product of a real number and a domain instance.
  real(kind=dp),intent(in) :: a !! l.h.s. real number
  class(domain_type),intent(in) :: self !! r.h.s. domain instance
  real(kind=dp) :: inva
  inva = 1.0_dp / a
  domain = self
  domain = inva *  self % getSpectrum()
endfunction real_div_domain
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getCurrent_u(self) result(u)
  !! Returns the 3-d array with values of Eulerian velocity (mean current) in 
  !! x-direction [m/s].
  !!
  !! Note: this implementation assumes that all u and v velocity arrays in 
  !! the domain instance are of same length in depth, such that the resulting
  !! u and v arrays are regular 3-d arrays.
  class(domain_type),intent(in) :: self 
    !! Domain instance
!DIR$ ATTRIBUTE ALIGN : 64 :: u
  real(kind=dp),dimension(:,:,:),allocatable :: u
    !! Eulerian u-velocity [m/s]
  integer(kind=int4) :: i,j
  integer(kind=int4) :: kdm
  associate(lb => self % lb,ub => self % ub)
  kdm = size(self % spectrum(1,1) % getCurrent_u())
  allocate(u(lb(1):ub(1),lb(2):ub(2),kdm))
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    u(i,j,:) = self % spectrum(i,j) % getCurrent_u()
  enddo
  endassociate
endfunction getCurrent_u
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getCurrent_v(self) result(v)
  !! Returns the 3-d array with values of Eulerian velocity (mean current) in 
  !! y-direction [m/s].
  !!
  !! Note: this implementation assumes that all u and v velocity arrays in 
  !! the domain instance are of same length in depth, such that the resulting
  !! u and v arrays are regular 3-d arrays.
  class(domain_type),intent(in) :: self 
    !! Domain instance
!DIR$ ATTRIBUTES ALIGN : 64 :: v
  real(kind=dp),dimension(:,:,:),allocatable :: v
    !! Eulerian v-velocity [m/s]
  integer(kind=ik) :: i,j
  integer(kind=ik) :: kdm
  associate(lb => self % lb,ub => self % ub)
  kdm = size(self % spectrum(1,1) % getCurrent_v())
  allocate(v(lb(1):ub(1),lb(2):ub(2),kdm))
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    v(i,j,:) = self % spectrum(i,j) % getCurrent_v()
  enddo
  endassociate
endfunction getCurrent_v
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getDepth(self) result(depth)
  !! Returns the mean water depth [m] array.
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: depth 
    !! Mean water depth [m]
  depth = self % spectrum % getDepth()
endfunction getDepth
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getElevation(self) result(elevation)
  !! Returns the mean water elevation [m] array.
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: elevation
    !! Mean water elevation [m]
  elevation = self % spectrum % getElevation()
endfunction getElevation
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getFrequency(self) result(frequency)
  !! Returns the frequency [Hz] array.
  class(domain_type),intent(in) :: self
    !! Domain instance
  real(kind=dp),dimension(:),allocatable :: frequency
    !! Frequency [Hz]
  frequency = self % spectrum(1,1) % getFrequency()
endfunction getFrequency
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getDirections(self) result(directions)
  !! Returns the spectral direction bins [rad].
  class(domain_type),intent(in) :: self
    !! Domain instance
  real(kind=dp),dimension(:),allocatable :: directions
    !! Directions [rad]
  directions = self % spectrum(1,1) % getDirections()
endfunction getDirections
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGravity(self) result(grav)
  !! Returns the gravitational acceleration [m/s^2] array.
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: grav 
    !! Gravitational acceleration [m/s^2]
  grav = self % spectrum % getGravity()
endfunction getGravity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGrid(self) result(grid)
  !! Returns the grid instance that is the component of the domain.
  class(domain_type),intent(in) :: self !! Domain instance
  type(grid_type) :: grid !! Grid instance component
  grid = self % grid
endfunction getGrid
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getLowerBounds(self) result(lb)
  !! Returns the lower bounds of the domain instance.
  class(domain_type),intent(in) :: self !! Domain instance
  integer(kind=int4),dimension(2) :: lb !! Lower bound indices
  lb = self % lb
endfunction getLowerBounds
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getUpperBounds(self) result(ub)
  !! Returns the upper bounds of the domain instance.
  class(domain_type),intent(in) :: self !! Domain instance
  integer(kind=int4),dimension(2) :: ub !! Upper bound indices
  ub = self % ub
endfunction getUpperBounds
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getSpectrum(self) result(spectrum)
  !! Returns the array of spectrum instances.
  class(domain_type),intent(in) :: self
    !! Domain instance
  type(spectrum_type),dimension(:,:),allocatable :: spectrum
    !! Array of spectrum instances
  spectrum = self % spectrum
endfunction getSpectrum
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getSpectrumArray(self,halowidth,periodic) result(spectrum_array)
  !! Returns a 4-dimensional spectrum array, where the first two dimensions are
  !! frequency and directional dimensions and the second two are spatial x and y
  !! dimensions.
  class(domain_type),intent(in) :: self
    !! Domain instance
!DIR$ ATTRIBUTES ALIGN : 64 :: spectrum_array
  real(kind=dp),dimension(:,:,:,:),allocatable :: spectrum_array
    !! Spectrum array
  integer(kind=int4),dimension(2),intent(in) :: halowidth
    !! Integers indicating how many cells to allocate for halo points
  logical,intent(in) :: periodic
    !! If `.true.`, halo cells will be updated with values corresponding to 
    !! periodic boundary conditions 
  integer(kind=int4) :: i,j
  integer(kind=int4) :: ndirs,nfreqs
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  nfreqs = size(self % spectrum(1,1) % getSpectrum(),dim=1)
  ndirs = size(self % spectrum(1,1) % getSpectrum(),dim=2)
  allocate(spectrum_array(nfreqs,ndirs,lb(1)-hw(1):ub(1)+hw(1),&
                                       lb(2)-hw(2):ub(2)+hw(2)))
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    spectrum_array(:,:,i,j) = self % spectrum(i,j) % getSpectrum()
  enddo
  ! Set halo values for periodic boundary conditions
  if(periodic)then

    spectrum_array(:,:,lb(1)-hw(1):lb(1)-1,:)&
      = spectrum_array(:,:,ub(1)-hw(1)+1:ub(1),:)
    spectrum_array(:,:,ub(1)+1:ub(1)+hw(1),:)&
      = spectrum_array(:,:,lb(1):lb(1)+hw(1)-1,:)
    spectrum_array(:,:,:,lb(2)-hw(2):lb(2)-1)&
      = spectrum_array(:,:,:,ub(2)-hw(2)+1:ub(2))
    spectrum_array(:,:,:,ub(2)+1:ub(2)+hw(2))&
      = spectrum_array(:,:,:,lb(2):lb(2)+hw(2)-1)
  endif
  endassociate
endfunction getSpectrumArray
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getPhaseSpeed(self) result(cp)
  !! Returns a 3-d array with phase speed values [m/s].
  class(domain_type),intent(in) :: self
    !! Domain instance
!DIR$ ATTRIBUTES ALIGN : 64 :: cp
  real(kind=dp),dimension(:,:,:),allocatable :: cp 
    !! Phase speed [m/s] array
  integer(kind=int4) :: i,j
  integer(kind=int4) :: ndirs
  associate(lb => self % lb,ub => self % ub)
  allocate(cp(self % nfreqs,lb(1):ub(1),lb(2):ub(2)))
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    cp(:,i,j) = self % spectrum(i,j) % getPhaseSpeed()
  enddo
  endassociate
endfunction getPhaseSpeed
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGroupSpeed(self,halowidth,periodic) result(cg)
  !! Returns a 3-d array with group speed values [m/s].
  class(domain_type),intent(in) :: self
    !! Domain instance
!DIR$ ATTRIBUTES ALIGN : 64 :: cg
  real(kind=dp),dimension(:,:,:),allocatable :: cg
    !! Group speed [m/s] array
  integer(kind=int4),dimension(2),intent(in) :: halowidth
    !! Integers indicating how many cells to allocate for halo points
  logical,intent(in) :: periodic
    !! If `.true.`, halo cells will be updated with values corresponding to 
    !! periodic boundary conditions 
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  allocate(cg(self % nfreqs,lb(1)-hw(1):ub(1)+hw(1),lb(2)-hw(2):ub(2)+hw(2)))
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    cg(:,i,j) = self % spectrum(i,j) % getGroupSpeed()
  enddo
  ! Set halo values for periodic boundary conditions
  if(periodic)then
    cg(:,lb(1)-hw(1):lb(1)-1,:) = cg(:,ub(1)-hw(1)+1:ub(1),:)
    cg(:,ub(1)+1:ub(1)+hw(1),:) = cg(:,lb(1):lb(1)+hw(1)-1,:)
    cg(:,:,lb(2)-hw(2):lb(2)-1) = cg(:,:,ub(2)-hw(2)+1:ub(2))
    cg(:,:,ub(2)+1:ub(2)+hw(2)) = cg(:,:,lb(2):lb(2)+hw(2)-1)
  endif
  endassociate
endfunction getGroupSpeed
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGridSpacingXWithHalo(self,halowidth,periodic) result(dx)
  !! Returns grid spacing array in x-direction including halo cells.
  class(domain_type),intent(in) :: self
    !! Domain instance
  integer(kind=int4),dimension(2),intent(in) :: halowidth
    !! Integer width of halo region
  logical,intent(in) :: periodic
    !! If `.true.`, halo cells will be updated with values corresponding to 
    !! periodic boundary conditions 
!DIR$ ATTRIBUTES ALIGN : 64 :: dx
  real(kind=dp),dimension(:,:),allocatable :: dx
    !! Grid spacing in x [m]
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  allocate(dx(lb(1)-hw(1):ub(1)+hw(1),lb(2)-hw(2):ub(2)+hw(2)))
  dx = 0.0_dp
  dx(lb(1):ub(1),lb(2):ub(2)) = self % dx
  ! Set halo values for periodic boundary conditions
  if(periodic)then
    dx(lb(1)-hw(1):lb(1)-1,:) = dx(ub(1)-hw(1)+1:ub(1),:)
    dx(ub(1)+1:ub(1)+hw(1),:) = dx(lb(1):lb(1)+hw(1)-1,:)
    dx(:,lb(2)-hw(2):lb(2)-1) = dx(:,ub(2)-hw(2)+1:ub(2))
    dx(:,ub(2)+1:ub(2)+hw(2)) = dx(:,lb(2):lb(2)+hw(2)-1)
  endif
  endassociate
endfunction getGridSpacingXWithHalo
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getGridSpacingYWithHalo(self,halowidth,periodic) result(dy)
  !! Returns grid spacing array in y-direction including halo cells.
  class(domain_type),intent(in) :: self
    !! Domain instance
  integer(kind=int4),dimension(2),intent(in) :: halowidth
    !! Integer width of halo region
  logical,intent(in) :: periodic
    !! If `.true.`, halo cells will be updated with values corresponding to 
    !! periodic boundary conditions
!DIR$ ATTRIBUTES ALIGN : 64 :: dy
  real(kind=dp),dimension(:,:),allocatable :: dy
    !! Grid spacing in y [m]
  associate(lb => self % lb,ub => self % ub,hw => halowidth)
  allocate(dy(lb(1)-hw(1):ub(1)+hw(1),lb(2)-hw(2):ub(2)+hw(2)))
  dy = 0.0_dp
  dy(lb(1):ub(1),lb(2):ub(2)) = self % dy
  ! Set halo values for periodic boundary conditions
  if(periodic)then
    dy(lb(1)-hw(1):lb(1)-1,:) = dy(ub(1)-hw(1)+1:ub(1),:)
    dy(ub(1)+1:ub(1)+hw(1),:) = dy(lb(1):lb(1)+hw(1)-1,:)
    dy(:,lb(2)-hw(2):lb(2)-1) = dy(:,ub(2)-hw(2)+1:ub(2))
    dy(:,ub(2)+1:ub(2)+hw(2)) = dy(:,lb(2):lb(2)+hw(2)-1)
  endif
  endassociate
endfunction getGridSpacingYWithHalo
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getSurfaceTension(self) result(surface_tension)
  !! Returns the surface tension [N/m].
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: surface_tension
    !! Surface tension [N/m]
  surface_tension = self % spectrum % getSurfaceTension()
endfunction getSurfaceTension
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getAirDensity(self) result(air_density)
  !! Returns the air density [kg/m^3].
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: air_density
    !! Air density [kg/m^3]
  air_density = self % spectrum % getAirDensity()
endfunction getAirDensity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function getWaterDensity(self) result(water_density)
  !! Returns the water density [kg/m^3].
  class(domain_type),intent(in) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: water_density
    !! Water density [kg/m^3]
  water_density = self % spectrum % getWaterDensity()
endfunction getWaterDensity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setDepth(self,depth)
  !! Sets the mean water depth [m].
  class(domain_type),intent(inout) :: self !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: depth !! Mean water depth [m]
  call self % spectrum % setDepth(depth)
endsubroutine setDepth
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setElevation(self,elevation)
  !! Sets the mean water elevation [m].
  class(domain_type),intent(inout) :: self 
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: elevation 
    !! Mean water elevation [m]
  call self % spectrum % setElevation(elevation)
endsubroutine setElevation
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setGravity(self,grav)
  !! Sets the gravitational acceleration [m/s^2].
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: grav
    !! Gravitational acceleration [m/s^2]
  call self % spectrum % setGravity(grav)
endsubroutine setGravity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSurfaceTension(self,surface_tension)
  !! Sets the surface tension [N/m^2].
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: surface_tension
    !! Surface tension [N/m^2]
  call self % spectrum % setSurfaceTension(surface_tension)
endsubroutine setSurfaceTension
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setAirDensity(self,air_density)
  !! Sets the air density [kg/m^3].
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: air_density
    !! Air density [kg/m^3]
  call self % spectrum % setAirDensity(air_density)
endsubroutine setAirDensity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setWaterDensity(self,water_density)
  !! Sets the water density [kg/m^3].
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: water_density
    !! Water density [kg/m^3]
  call self % spectrum % setWaterDensity(water_density)
endsubroutine setWaterDensity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSpectrum1d(self,spectrum)
  !! Sets the 1-d spectrum array. This procedure is overloaded by the
  !! generic procedure setSpectrum.
  class(domain_type),intent(inout) :: self
    !! Domain instance
  type(spectrum_type),dimension(:),intent(in) :: spectrum
    !! Input 1-d array of spectrum object instances
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub)
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    self % spectrum(i,j) = spectrum(i)
  enddo
  endassociate
endsubroutine setSpectrum1d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSpectrum2d(self,spectrum)
  !! Sets the 2-d spectrum array. This procedure is overloaded by the
  !! generic procedure setSpectrum.
  class(domain_type),intent(inout) :: self
    !! Domain instance
  type(spectrum_type),dimension(:,:),intent(in) :: spectrum
    !! Input 2-d array of spectrum object instances
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub)
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    self % spectrum(i,j) = spectrum(i,j)
  enddo
  endassociate
endsubroutine setSpectrum2d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSpectrumArray1d1d(self,spectrum_array)
  !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for omnidirectional spectrum in 1-d space (1d-1d)
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),intent(in) :: spectrum_array
    !! Spectrum array
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub)
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    call self % spectrum(i,j) % setSpectrum(spectrum_array(:,i))
  enddo
  endassociate
endsubroutine setSpectrumArray1d1d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSpectrumArray1d2d(self,spectrum_array)
  !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for setting 1-d spectrum into 2-d physical space
  !! of 2-d spectrum into 1-d physical space.
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:,:),intent(in) :: spectrum_array
    !! Spectrum array
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub)
  if(lb(2) == ub(2))then
    ! Setting 2-d spectrum into 1-d physical space
    do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
      call self % spectrum(i,j) % setSpectrum(spectrum_array(:,:,i))
    enddo
  else
    ! Setting 1-d spectrum into 2-d physical space
    do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
      call self % spectrum(i,j) % setSpectrum(spectrum_array(:,i,j))
    enddo
  endif
  endassociate
endsubroutine setSpectrumArray1d2d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure subroutine setSpectrumArray2d2d(self,spectrum_array)
  !! Sets the spectrum instances based on input spectrum array.
  !! This implementation is for directional spectrum in 2-d space (2d-2d)
  class(domain_type),intent(inout) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:,:,:),intent(in) :: spectrum_array
    !! Spectrum array
  integer(kind=int4) :: i,j
  associate(lb => self % lb,ub => self % ub)
  do concurrent(i=lb(1):ub(1),j=lb(2):ub(2))
    call self % spectrum(i,j) % setSpectrum(spectrum_array(:,:,i,j))
  enddo
  endassociate
endsubroutine setSpectrumArray2d2d
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function frequencyMoment(self,n) result(moment)
  !! Returns the spectral frequency moment of order n.
  class(domain_type),intent(in) :: self !! Spectrum instance
  integer(kind=int4),intent(in) :: n !! Order
  real(kind=dp),dimension(:,:),allocatable :: moment
  moment = self % spectrum % frequencyMoment(n)
endfunction frequencyMoment
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function wavenumberMoment(self,n) result(moment)
  !! Returns the spectral frequency moment of order n.
  class(domain_type),intent(in) :: self !! Spectrum instance
  integer(kind=int4),intent(in) :: n !! Order
  real(kind=dp),dimension(:,:),allocatable :: moment
  moment = self % spectrum % wavenumberMoment(n)
endfunction wavenumberMoment
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function meanPeriod(self)
  !! Returns the mean wave period [s] for the whole domain.
  class(domain_type),intent(in) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: meanPeriod
    !! Mean period [s] array
  meanPeriod = self % spectrum % meanPeriod()
endfunction meanPeriod
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function meanPeriodZeroCrossing(self) 
  !! Returns the zero-crossing mean wave period [s] for the whole domain.
  class(domain_type),intent(in) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: meanPeriodZeroCrossing
    !! Mean period [s] array
  meanPeriodZeroCrossing = self % spectrum % meanPeriodZeroCrossing()
endfunction meanPeriodZeroCrossing
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
pure function significantWaveHeight(self) result(hs)
  !! Returns the significant wave height [m] for the whole domain.
  class(domain_type),intent(in) :: self
    !! Domain instance
  real(kind=dp),dimension(:,:),allocatable :: hs
    !! Significant wave height [m] array
  hs = self % spectrum % significantWaveHeight()
endfunction significantWaveHeight
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!subroutine writeJSON(self,filename,minify)
  !! Writes a spectrum instance to a JSON file.
 ! class(domain_type),intent(in) :: self !! `domain` instance
 ! character(len=*),intent(in) :: filename !! JSON file name
 ! logical,intent(in) :: minify !! Logical switch to minify the JSON file
 ! type(json_core) :: json
 ! type(json_value),pointer :: ptr
 ! call json % initialize(no_whitespace=minify,real_format='ES')
 ! call json % create_object(ptr,'')
 ! call json % add(ptr,'lb',self % grid % getLowerBounds())
  !call json % add(ptr,'ub',self % grid % getUpperBounds())
 ! call json % add(ptr,'x',pack(self % grid % getAxisX(),.true.))
 ! call json % add(ptr,'y',pack(self % grid % getAxisY(),.true.))
  !call json % add(ptr,'lon',pack(self % grid % getLongitude(),.true.))
  !call json % add(ptr,'lat',pack(self % grid % getLatitude(),.true.))
 ! call json % add(ptr,'u',pack(self % u,.true.))
  !call json % add(ptr,'v',pack(self % v,.true.))
 ! call json % add(ptr,'eta',pack(self % eta,.true.))
 ! call json % add(ptr,'depth',pack(self % depth,.true.))
  !call json % add(ptr,'frequency',self % getFrequency())
 ! call json % add(ptr,'directions',self % getDirections())
 ! call json % add(ptr,'spectrum',&
  !                pack(self % getSpectrumArray([0,0],.false.),.true.))
  !call json % print(ptr,trim(filename))
 ! call json % destroy(ptr)
!endsubroutine writeJSON
!-------------------------------------------------------------------------------
endmodule mod_domain
