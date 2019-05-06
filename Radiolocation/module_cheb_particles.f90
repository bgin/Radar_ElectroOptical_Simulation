
!#include "Config.fpp"

module  mod_cheb_particles

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cheb_particles'
 !          
 !          Purpose:
 !                      This module models 'Chebyshev Particles' aggregated concentration per number of bins
 !                      of size distribution.
 !                       
 !          History:
 !                        Date: 24-07-2018
 !                        Time: 11:17 GMT+2
 !                        Completely modified on: 03-05-2019, 13:49 GMT+2
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                           'Scattering of Radiation by Moderately Non-Spherical Particles' 
 !                            By A. Mugnai and W.J. Wiscombe (1986)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use module_kinds,    only : int1, int4, sp
    use mod_vectypes,    only : YMM8r4_t, Mask8_t,YMM8i4_t
    implicit none
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MAJOR = 1
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MINOR = 0
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_MICRO = 0
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_FULLVER = 1000*MOD_CHEB_PARTICLES_MAJOR + &
                                                                     100*MOD_CHEB_PARTICLES_MINOR  + &
                                                                     10*MOD_CHEB_PARTICLES_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_CREATE_DATE = "24-07-2018 11:17 +00200 (TUE 24 JUL 2018 GMT+2)"
    
    ! Module build date  (  should be set after successful compilation)
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_DESCRIPT = "Model of system of  chebyshev particles (hydrometeors)."
    
    ! Constants
    
    real(kind=sp),    parameter, private  :: Deg90Rad = 1.5708_sp
    type(YMM8r4_t),   parameter, private  :: vD90Rad = YMM8r4_t(Deg90Rad)
    type(YMM8r4_t),   parameter, private  :: vScaleToCm = YMM8r4_t(3000.0_sp)
    ! For loop-unrolling
    real(kind=sp), dimension(0:7), parameter, private :: VINC = [1.0_sp,2.0_sp,3.0_sp, &
                                                         4.0_sp,5.0_sp,6.0_sp,7.0_sp,8.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC2 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                         13.0_sp,14.0_sp,15.0_sp,16.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC3 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                          21.0_sp,22.0_sp,23.0_sp,24.0_sp]
    real(kind=sp), dimension(0:7), parameter, private :: VINC4 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                          29.0_sp,30.0_sp,31.0_sp]



    
    ! Low spacial and temporal frequency derived data type
    type, public :: ChebParticlesLTS_t
        
          
          public
          ! Dimensioning indices
          ! maximum parametrized points on curve (parametric checbyshev particles formula)
          integer(kind=int4)              :: nxpts
          integer(kind=int4)              :: nypts
          integer(kind=int4)              :: nzpts
          ! max values per particle's shape 
          integer(kind=int4)              :: nshpts
          integer(kind=int4)              :: np
          ! Time evolution steps
          integer(kind=int4)              :: nt
          
          !
          ! Particles types  (dust-like,grail,snow,hail)
          character(len=32)                :: particles_type
          !
          
         

       
      
         
          ! Chebyshev particles shapes  i.e. (r = r0[1+eTn(cos(theta))]
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_shape          
          type(YMM8r4_t), allocatable, dimension(:,:)  :: particles_shape

          
          ! Chebyshev particles radii  
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_radii          
          real(kind=sp), allocatable, dimension(:)      :: particles_radii

          ! 
          
       

         
          
          ! Chebyshev particles  surface (units of mm^2), (total)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_surface          
          real(kind=sp), allocatable, dimension(:)          :: particles_surf

          
          ! Chebyshev particles  parametric equation in x - dimension (non-dimensional)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_x          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramx

          
          ! Chebyshev particles   parametric equation in y  - dimension (non-dimensional)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_y          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramy

          
          ! Chebyshev particles  parametric equation in z - dimension (non-dimensional)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_param_z          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_paramz

          ! Particles per bin mass (kg) (time-invariant)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_mass          
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_mass

          ! Particles (coupled)  temperature (units of Celsius)
!DIR$     ATTRIBUTES ALIGN : 64 :: paricles_temp          
          type(YMM8r4_t), allocatable, dimension(:)       :: particles_temp
      
      end type ChebParticlesLTS_t      
      

          
       

          
    
!============================================================================================================!
       ! High temporal and spatial freuqency derived data type.
!============================================================================================================!
       type, public :: ChebParticlesHTS_t
          public

             ! Trajectory of Chebyshev particles (aggregated) range in (spherical coordinate system) per bin ( units of meters (m))
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_range          
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_range

          
          ! Trajectory of Chebyshev particles (aggregated) per bin theta spherical coordinates component (units of radian (rad))
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_theta
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_theta

          
          ! Trajectory of Chebyshev particles (aggregated)  per bin phi spehrical coordinates compoment (units of radian (rad))
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_phi          
          type(YMM8r4_t), allocatable, dimension(:,:)    :: particles_phi

              ! Chebyshev particles fall velocity (m/s^-1)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vfall          
          type(YMM8r4_t), allocatable, dimension(:,:)          :: particles_vfall

          
          ! Chebyshev particles (coupled) per bin fall velocity range component (spherical) (time evolution)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vr          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vr

          
          ! Chebyshev particles (coupled) velocity theta component  (spherical)  (time evolution)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vtheta          
          type(YMM8r4_t), allocatable, dimension(:,:)      :: particles_vth

          
          ! Chebyshev particles (coupled) velocity phi component  (spherical)   (time evolution)
!DIR$     ATTRIBUTES ALIGN : 64 :: particles_vphi          
          type(YMM8r4_t), allocatable, dimension(:,:)     :: particles_vphi

        
          
       end type ChebParticlesHTS_t
    
   contains

     subroutine InitChebParticles(PartLTS,PartHTS,nxpts,nypts,nzpts,nt,nparticles, &
                                  nshpts,part_type,errstate,iounit, &
                                  verbose,logging, filename,append )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitChebParticles
           use mod_print_error, only : handle_fatal_memory_error
                               
           type(ChebParticlesLTS_t),    intent(inout) :: PartLTS
           type(ChebParticlesHTS_t),    intent(inout) :: PartHTS
           integer(kind=int4),          intent(inout) :: nxpts
           integer(kind=int4),          intent(inout) :: nypts
           integer(kind=int4),          intent(inout) :: nzpts
           integer(kind=int4),          intent(in)    :: nt
           integer(kind=int4),          intent(inout) :: np
           integer(kind=int4),          intent(in)    :: nshpts
           character(len=32),           intent(in)    :: part_type
           logical(kind=int1),          intent(inout) :: errstate
           integer(kind=int4),          intent(in)    :: iounit
           logical(kind=int4),          intent(in)    :: logging
           logical(kind=int4),          intent(in)    :: verbose
           logical(kind=int4),          intent(in)    :: append
           character(len=*),            intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           integer(kind=int4), automatic :: rem
           ! Exec code .... 
           rem = 0
           rem = MOD(nxpts,8)
           if(rem) then
              nxpts = nxpts-rem
           end if
           rem = MOD(nypts,8)
           if(rem) then
              nypts = nypts-rem
           end if
           rem = MOD(nzpts,8)
           if(rem) then
              nzpts = nzpts-rem
           end if
           rem = MOD(np,8)
           if(rem) then
              np  =  np-rem
           end if
           rem = MOD(nt,8)
           if(rem) then
              nt = nt-rem
           end if
           rem = MOD(nshpts,8)
           if(rem) then
              nshpts = nshpts-rem
           end if
           PartLTS.nxpts = nxpts
           PartLTS.nypts = nypts
           PartLTS.nzpts = nzpts
           PartLTS.nt    = nt
           PartLTS.np    = np
           PartLTS.nshpts = nshpts
           PartLTS.part_type = part_type
           allocate(PartLTS.particles_shape(PartLTS.nshpts,PartLTS.np), &
                    PartLTS.particles_radii(PartLTS.np),                &
                    PartLTS.particles_surf(PartLTS.np),                 &
                    PartLTS.particles_paramx(PartLTS.nxpts,PartLTS.np), &
                    PartLTS.particles_paramy(PartLTS.nypts,PartLTS.np), &
                    PartLTS.particles_paramz(PartLTS.nzpts,PartLTS.np), &
                    PartLTS.particles_mass(PartLTS.np),                 &
                    PartLTS.particles_temp(PartLTS.np), STAT=aerr,ERRMSG=emsg) 
                                                                                
           if(aerr /= 0) goto 9999        
           aerr = 0
           emsg = " "
           allocate(PartHTS.particles_range(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_theta(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_phi(PartLTS.nt,  PartLTS.np),     &
                    PartHTS.particles_vfall(PartLTS.nt,PartLTS.np),     &
                    PartHTS.particles_vr(PartLTS.nt,   PartLTS.np),     &
                    PartHTS.particles_vth(PartLTS.nt,  PartLTS.np),     &
                    PartHTS.particles_vphi(PartLTS.nt, PartLTS.np),     &
                    STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
           errstate = .false.
           return
9999       call  handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_cheb_particles, subroutine: InitChebParticles -- Memory Allocation Failure !!", &                                                     
                              "module: mod_cheb_particles, subroutine: InitChebParticles -- Memory Allocation Failure !!", &
                                                    emsg, 304) 
          
     end subroutine InitChebParticles

 !===================================================================
    !                   Computational procedures 
    !  These procedures are not type bound in order to facilitate potential
    !  OpenMP parallelization (to be done later).
    !  Only Chebyshev Particles of type T2 and T4 should be used.
    !===================================================================
    
     subroutine ComputeShape_ymm8r4(PartLTS,np,acn,acdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeShape_ymm8r4
           use mod_vecconsts,     only : ymm8r4_one,ymm8r4_zero,ymm8r4_twopi
           type(ChebParticlesLTS_t),      intent(inout)  :: PartLTS
           integer(kind=int4),            intent(in)     :: np
           real(kind=sp), dimension(np),  intent(inout)  :: acn
           real(kind=sp), dimension(np),  intent(inout)  :: acdef
          
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
           end type Vtheta2CacheLines_t
           type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
           end type Vthinc2CacheLines_t
           type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
           end type Termx2CacheLines_t
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta2CacheLines_t) :: Vtheta2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
           type(Vthinc2CacheLines_t) :: Vthinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
           type(Termx2CacheLines_t) :: Term2CL
           !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
           type(YMM8r4_t), automatic :: cn_rand
           ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
           type(YMM8r4_t), automatic :: sphr_rand
           !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
           type(YMM8r4_t), automatic :: cdef_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic :: vC
           !DIR$ ATTRIBUTES ALIGN : 32 tmp
           type(YMM8r4_t), automatic :: tmp
           real(kind=sp),  automatic :: cn
           real(kind=sp),  automatic :: sphr
           real(kind=sp),  automatic :: cdef
           integer(kind=int4), automatic :: j,i
           
          
          
           ! Exec code ....
           cn_rand             = ymm8r4_zero
           sphr_rand           = ymm8r4_zero
           cdef_rand           = ymm8r4_zero
           Vtheta2CL.vtheta0   = ymm8r4_zero
           Vtheta2CL.vtheta1   = ymm8r4_zero
           Vtheta2CL.vtheta2   = ymm8r4_zero
           Vtheta2CL.vtheta3   = ymm8r4_zero
           Vthinc2CL.vthinc0   = ymm8r4_zero
           Vthinc2CL.vthinc1   = ymm8r4_zero
           Vthinc2CL.vthinc2   = ymm8r4_zero
           Vthinc2CL.vthinc3   = ymm8r4_zero
           vNPTS               = ymm8r4_zero
           tmp                 = ymm8r4_zero
           vC                  = ymm8r4_zero
           Term2CL.term0       = ymm8r4_zero
           Term2CL.term1       = ymm8r4_zero
           Term2CL.term2       = ymm8r4_zero
           Term2CL.term3       = ymm8r4_zero
           cn        = 0.0_sp
           sphr      = 0.0_sp
           cdef      = 0.0_sp
           vNPTS.v   = real(PartLTS.nshpts,kind=sp)
           ! First touch
           PartLTS.particles_shape = ymm8r4_zero
           PartLTS.particles_radii = 0.0_sp
           do j=1, PartLTS.np
              call RANDOM_SEED()
              call RANDOM_NUMBER(cn)
              if(0.0_sp == cn) cn = 0.1_sp
              cn = cn*10.0_sp
              acn(j) = cn ! caching values for the different proprties computations
              cn_rand.v = cn
              call RANDOM_NUMBER(sphr)
              sphr = sphr*3.0_sp
             ! asphr(j) = sphr
              sphr_rand.v = sphr
              PartLTS.particles_radii(j) = sphr ! caching not used here directly.
              call RANDOM_NUMBER(cdef)
              cdef_rand.v = cdef
              acdef(j) = cdef
              vC.v = ymm8r4_twopi.v*sphr_rand.v
              tmp.v = vC.v/vNPTS.v
              Vthinc2CL.vthinc0.v = tmp.v
              Vthinc2CL.vthinc0.v = Vthinc2CL.vthinc0.v*VINC.v
              Vthinc2CL.vthinc1.v = tmp.v
              Vthinc2CL.vthinc1.v = Vthinc2CL.vthinc1.v*VINC2.v
              Vthinc2CL.vthinc2.v = tmp.v
              Vthinc2CL.vthinc2.v = Vthinc2CL.vthinc2.v*VINC3.v
              Vthinc2CL.vthinc3.v = tmp.v
              Vthinc2CL.vthinc3.v = Vthinc2CL.vthinc3.v*VINC4.v
              Vtheta2CL.vtheta0   = ymm8r4_zero
              Term2CL.term0       = ymm8r4_zero
              Vtheta2CL.vtheta1   = ymm8r4_zero
              Term2CL.term1       = ymm8r4_zero
              Vtheta2CL.vtheta2   = ymm8r4_zero
              Term2CL.term2       = ymm8r4_zero
              Vtheta2CL.vtheta3   = ymm8r4_zero
              Term2CL.term3       = ymm8r4_zero
              !DIR$ VECTOR ALWAYS
              do i=1, PartLTS.nshpts-3, 4
                 Vtheta2CL.vtheta0.v   = Vtheta2CL.vtheta0.v+Vthinc2CL.vthinc0.v
                 Term2CL.term0.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta0.v)
                 PartLTS.particles_shape(i+0,j).v = sphr_rand.v*Term2CL.term0.v
                 Vtheta2CL.vtheta1.v   = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                 Term2CL.term1.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta1.v)
                 PartLTS.particles_shape(i+1,j).v = sphr_rand.v*Term2CL.term1.v
                 Vtheta2CL.vtheta2.v   = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                 Term2CL.term2.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta2.v)
                 PartLTS.particles_shape(i+2,j).v = sphr_rand.v*Term2CL.term2.v
                 Vtheta2CL.vtheta3.v   = Vtheta2CL.vtheta3.v+Vthinc2CL.vthinc3.v
                 Term2CL.term3.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta3.v)
                 PartLTS.particles_shape(i+3,j).v = sphr_rand.v*Term2CL.term3.v
              end do
           end do
     end subroutine ComputeShape_ymm8r4


     subroutine ComputeXparam_ymm8r4(PartLTS,np,acn,acdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeXparam_ymm8r4
           use mod_vecconsts, only : ymm8r4_twopi,ymm8r4_one,ymm8r4_zero
           type(ChebParticlesLST_t),     intent(inout) :: PartLST
           integer(kind=int4),           intent(in)    :: np
           real(kind=sp), dimension(np), intent(in)    :: acn
           real(kind=sp), dimension(np), intent(in)    :: acdef
          
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
           end type Vtheta2CacheLines_t
           type :: Vphi2CacheLines_t
              sequence
              type(YMM8r4_t) :: vphi0
              type(YMM8r4_t) :: vphi1
              type(YMM8r4_t) :: vphi2
              type(YMM8r4_t) :: vphi3
           end type Vphi2CacheLines_t
           type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
           end type Vthinc2CacheLines_t
           type :: Vphinc2CacheLines
              sequence
              type(YMM8r4_t) :: vphinc0
              type(YMM8r4_t) :: vphinc1
              type(YMM8r4_t) :: vphinc2
              type(YMM8r4_t) :: vphinc3
           end type Vphinc2CacheLines_t
           type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
           end type Termx2CacheLines_t
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta2CacheLines_t) :: Vtheta2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphi2CL
           type(Vphi2CacheLines_t)  :: Vphi2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
           type(Vthinc2CacheLines_t) :: Vthinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphinc2CL
           type(Vphinc2CacheLines_t) :: Vphinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
           type(Termx2CacheLines_t) :: Term2CL
           !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
           type(YMM8r4_t), automatic :: cn_rand
           ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
           type(YMM8r4_t), automatic :: sphr_rand
           !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
           type(YMM8r4_t), automatic :: cdef_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic :: vC
           !DIR$ ATTRIBUTES ALIGN : 32 tmp1,tmp2
           type(YMM8r4_t), automatic :: tmp1,tmp2
           integer(kind=int4), automatic :: j,i
          
        
           ! Exec code .....
           cn_rand             = ymm8r4_zero
           sphr_rand           = ymm8r4_zero
           cdef_rand           = ymm8r4_zero
           Vtheta2CL.vtheta0   = ymm8r4_zero
           Vtheta2CL.vtheta1   = ymm8r4_zero
           Vtheta2CL.vtheta2   = ymm8r4_zero
           Vtheta2CL.vtheta3   = ymm8r4_zero
           Vphi2CL.vphi0       = ymm8r4_zero
           Vphi2CL.vphi1       = ymm8r4_zero
           Vphi2CL.vphi2       = ymm8r4_zero
           Vphi2CL.vphi3       = ymm8r4_zero
           Vthinc2CL.vthinc0   = ymm8r4_zero
           Vthinc2CL.vthinc1   = ymm8r4_zero
           Vthinc2CL.vthinc2   = ymm8r4_zero
           Vthinc2CL.vthinc3   = ymm8r4_zero
           Vphinc2CL.vphinc0   = ymm8r4_zero
           Vphinc2CL.vphinc1   = ymm8r4_zero
           Vphinc2CL.vphinc2   = ymm8r4_zero
           Vphinc2CL.vphinc3   = ymm8r4_zero
           vNPTS               = ymm8r4_zero
           tmp1                = ymm8r4_zero
           tmp2                = ymm8r4_zero
           vC                  = ymm8r4_zero
           Term2CL.term0       = ymm8r4_zero
           Term2CL.term1       = ymm8r4_zero
           Term2CL.term2       = ymm8r4_zero
           Term2CL.term3       = ymm8r4_zero
           vNPTS.v   = real(PartLTS.nxpts,kind=sp)
           ! First touch
           PartLTS.particles_paramx = ymm8r4_zero
           do j=1, PartLTS.np
              cn_rand.v   = acn(j)
              sphr_rand.v = PartLTS.particles_radii(j)
              cdef_rand.v = acdef(j)
              vC.v        = ymm8r4_twopi.v*sphr_rand.v
              tmp1.v      = vC.v/vNPTS.v
              tmp2.v      = tmp1.v
              Vthinc2CL.vthinc0.v   = tmp1.v
              Vthinc2CL.vthinc0.v   = Vthinc2CL.vthinc0.v*VINC.v
              Vphinc2CL.vphinc0.v   = tmp2.v
              Vphinc2CL.vphinc0.v   = Vphinc2CL.vphinc0.v*VINC.v
              Vthinc2CL.vthinc1.v   = tmp1.v
              Vthinc2CL.vthinc1.v   = Vthinc2CL.vthinc1.v*VINC2.v
              Vphinc2CL.vphinc1.v   = tmp2.v
              Vphinc2CL.vphinc1.v   = Vphinc2CL.vphinc1.v*VINC2.v
              Vthinc2CL.vthinc2.v   = tmp1.v
              Vthinc2CL.vthinc2.v   = Vthinc2CL.vthinc2.v*VINC3.v
              Vphinc2CL.vphinc2.v   = tmp2.v
              Vphinc2CL.vphinc2.v   = Vphinc2CL.vphinc2.v*VINC3.v
              Vthinc2CL.vthinc3.v   = tmp1.v
              Vthinc2CL.vthinc3.v   = Vthinc2CL.vthinc3.v*VINC4.v
              Vphinc2CL.vphinc3.v   = tmp2.v
              Vphinc2CL.vphinc3.v   = Vphinc2CL.vphinc3.v*VINC4.v
              Vtheta2CL.vtheta0     = ymm8r4_zero
              Vphi2CL.vphi0         = ymm8r4_zero
              Term2CL.term0         = ymm8r4_zero
              Vtheta2CL.vtheta1     = ymm8r4_zero
              Vphi2CL.vphi1         = ymm8r4_zero
              Term2CL.term1         = ymm8r4_zero
              Vtheta2CL.vtheta2     = ymm8r4_zero
              Vphi2CL.vphi2         = ymm8r4_zero
              Term2CL.term2         = ymm8r4_zero
              Vtheta2CL.vtheta3     = ymm8r4_zero
              Vphi2CL.vphi3         = ymm8r4_zero
              Term2CL.term3         = ymm8r4_zero
              !DIR$ VECTOR ALWAYS
              do i=1, PartLTS.nxpts-3, 4
                 Vtheta2CL.vtheta0.v = Vtheta2CL.vtheta0.v+Vthinc2CL.vthinc0.v
                 Vphi2CL.vphi0.v   = Vphi2CL.vphi0.v+Vphinc2CL.vphinc0.v
                 Term2CL.term0.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta0.v))
                 Term2CL.term0.v   = Term2CL.term0.v*sin(Vtheta2CL.vtheta0.v)*cos(Vphi2CL.vphi0.v)
                 PartLTS.particles_paramx(i+0,j).v = Term2CL.term0.v
                 Vtheta2CL.vtheta1.v = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                 Vphi2CL.vphi1.v   = Vphi2CL.vphi1.v+Vphinc2CL.vphinc1.v
                 Term2CL.term1.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta1.v))
                 Term2CL.term1.v   = Term2CL.term1.v*sin(Vtheta2CL.vtheta1.v)*cos(Vphi2CL.vphi1.v)
                 PartLTS.particles_paramx(i+1,j).v = Term2CL.term1.v
                 Vtheta2CL.vtheta2.v = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                 Vphi2CL.vphi2.v   = Vphi2CL.vphi2.v+Vphinc2CL.vphinc2.v
                 Term2CL.term2.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta2.v))
                 Term2CL.term2.v   = Term2CL.term2.v*sin(Vtheta2CL.vtheta2.v)*cos(Vphi2CL.vphi2.v)
                 PartLTS.particles_paramx(i+2,j).v = Term2CL.term2.v
                 Vtheta2CL.vtheta3.v = Vtheta2CL.vtheta3.v+Vthinc2CL.vthinc3.v
                 Vphi2CL.vphi3.v   = Vphi2CL.vphi3.v+Vphinc2CL.vphinc3.v
                 Term2CL.term3.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta3.v))
                 Term2CL.term3.v   = Term2CL.term3.v*sin(Vtheta2CL.vtheta3.v)*cos(Vphi2CL.vphi3.v)
                 PartLTS.particles_paramx(i+3,j).v = Term2CL.term3.v
              end do
           end do
     end subroutine ComputeXparam_ymm8r4
       
     subroutine ComputeYparam_ymm8r4(PartLTS,np,acn,acdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeYparam_ymm8r4
           use mod_vecconsts, only : ymm8r4_twopi,ymm8r4_one,ymm8r4_zero
           type(ChebParticlesLST_t),     intent(inout) :: PartLST
           integer(kind=int4),           intent(in)    :: np
           real(kind=sp), dimension(np), intent(in)    :: acn
           real(kind=sp), dimension(np), intent(in)    :: acdef
          
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
           end type Vtheta2CacheLines_t
           type :: Vphi2CacheLines_t
              sequence
              type(YMM8r4_t) :: vphi0
              type(YMM8r4_t) :: vphi1
              type(YMM8r4_t) :: vphi2
              type(YMM8r4_t) :: vphi3
           end type Vphi2CacheLines_t
           type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
           end type Vthinc2CacheLines_t
           type :: Vphinc2CacheLines
              sequence
              type(YMM8r4_t) :: vphinc0
              type(YMM8r4_t) :: vphinc1
              type(YMM8r4_t) :: vphinc2
              type(YMM8r4_t) :: vphinc3
           end type Vphinc2CacheLines_t
           type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
           end type Termx2CacheLines_t
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta2CacheLines_t) :: Vtheta2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphi2CL
           type(Vphi2CacheLines_t)  :: Vphi2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
           type(Vthinc2CacheLines_t) :: Vthinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphinc2CL
           type(Vphinc2CacheLines_t) :: Vphinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
           type(Termx2CacheLines_t) :: Term2CL
           !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
           type(YMM8r4_t), automatic :: cn_rand
           ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
           type(YMM8r4_t), automatic :: sphr_rand
           !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
           type(YMM8r4_t), automatic :: cdef_rand
           !
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic :: vC
           !
           !DIR$ ATTRIBUTES ALIGN : 32 tmp1,tmp2
           type(YMM8r4_t), automatic :: tmp1,tmp2
           integer(kind=int4), automatic :: j,i
           ! Exec code .....
           cn_rand             = ymm8r4_zero
           sphr_rand           = ymm8r4_zero
           cdef_rand           = ymm8r4_zero
           Vtheta2CL.vtheta0   = ymm8r4_zero
           Vtheta2CL.vtheta1   = ymm8r4_zero
           Vtheta2CL.vtheta2   = ymm8r4_zero
           Vtheta2CL.vtheta3   = ymm8r4_zero
           Vphi2CL.vphi0       = ymm8r4_zero
           Vphi2CL.vphi1       = ymm8r4_zero
           Vphi2CL.vphi2       = ymm8r4_zero
           Vphi2CL.vphi3       = ymm8r4_zero
           Vthinc2CL.vthinc0   = ymm8r4_zero
           Vthinc2CL.vthinc1   = ymm8r4_zero
           Vthinc2CL.vthinc2   = ymm8r4_zero
           Vthinc2CL.vthinc3   = ymm8r4_zero
           Vphinc2CL.vphinc0   = ymm8r4_zero
           Vphinc2CL.vphinc1   = ymm8r4_zero
           Vphinc2CL.vphinc2   = ymm8r4_zero
           Vphinc2CL.vphinc3   = ymm8r4_zero
           vNPTS               = ymm8r4_zero
           tmp1                = ymm8r4_zero
           tmp2                = ymm8r4_zero
           vC                  = ymm8r4_zero
           Term2CL.term0       = ymm8r4_zero
           Term2CL.term1       = ymm8r4_zero
           Term2CL.term2       = ymm8r4_zero
           Term2CL.term3       = ymm8r4_zero
           vNPTS.v   = real(PartLTS.nypts,kind=sp)
           ! First touch
           PartLTS.particles_paramy = ymm8r4_zero
             do j=1, PartLTS.np
                cn_rand.v   = acn(j)
                sphr_rand.v = PartLTS.particles_radii(j)
                cdef_rand.v = acdef(j)
                vC.v        = ymm8r4_twopi.v*sphr_rand.v
                tmp1.v      = vC.v/vNPTS.v
                tmp2.v      = tmp1.v
                Vthinc2CL.vthinc0.v   = tmp1.v
                Vthinc2CL.vthinc0.v   = Vthinc2CL.vthinc0.v*VINC.v
                Vphinc2CL.vphinc0.v   = tmp2.v
                Vphinc2CL.vphinc0.v   = Vphinc2CL.vphinc0.v*VINC.v
                Vthinc2CL.vthinc1.v   = tmp1.v
                Vthinc2CL.vthinc1.v   = Vthinc2CL.vthinc1.v*VINC2.v
                Vphinc2CL.vphinc1.v   = tmp2.v
                Vphinc2CL.vphinc1.v   = Vphinc2CL.vphinc1.v*VINC2.v
                Vthinc2CL.vthinc2.v   = tmp1.v
                Vthinc2CL.vthinc2.v   = Vthinc2CL.vthinc2.v*VINC3.v
                Vphinc2CL.vphinc2.v   = tmp2.v
                Vphinc2CL.vphinc2.v   = Vphinc2CL.vphinc2.v*VINC3.v
                Vthinc2CL.vthinc3.v   = tmp1.v
                Vthinc2CL.vthinc3.v   = Vthinc2CL.vthinc3.v*VINC4.v
                Vphinc2CL.vphinc3.v   = tmp2.v
                Vphinc2CL.vphinc3.v   = Vphinc2CL.vphinc3.v*VINC4.v
                Vtheta2CL.vtheta0     = ymm8r4_zero
                Vphi2CL.vphi0         = ymm8r4_zero
                Term2CL.term0         = ymm8r4_zero
                Vtheta2CL.vtheta1     = ymm8r4_zero
                Vphi2CL.vphi1         = ymm8r4_zero
                Term2CL.term1         = ymm8r4_zero
                Vtheta2CL.vtheta2     = ymm8r4_zero
                Vphi2CL.vphi2         = ymm8r4_zero
                Term2CL.term2         = ymm8r4_zero
                Vtheta2CL.vtheta3     = ymm8r4_zero
                Vphi2CL.vphi3         = ymm8r4_zero
                Term2CL.term3         = ymm8r4_zero
                !DIR$ VECTOR ALWAYS
                do i=1, PartLTS.nypts-3, 4
                     Vtheta2CL.vtheta0.v = Vtheta2CL.vtheta0.v+Vthic2CL.vthinc0.v
                     Vphi2CL.vphi0.v   = Vphi2CL.vphi0.v+Vphinc2CL.vphinc0.v
                     Term2CL.term0.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta0.v))
                     Term2CL.term0.v   = Term2CL.term0.v*sin(Vtheta2CL.vtheta0.v)*sin(Vphi2CL.vphi0.v)
                     PartLTS.particles_paramy(i+0,j).v = Term2CL.term0.v
                     Vtheta2CL.vtheta1.v = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                     Vphi2CL.vphi1.v   = Vphi2CL.vphi1.v+Vphinc2CL.vphinc1.v
                     Term2CL.term1.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta1.v))
                     Term2CL.term1.v   = Term2CL.term1.v*sin(Vtheta2CL.vtheta1.v)*sin(Vphi2CL.vphi1.v)
                     PartLTS.particles_paramy(i+1,j).v = Term2CL.term1.v
                     Vtheta2CL.vtheta2.v = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                     Vphi2CL.vphi2.v   = Vphi2CL.vphi2.v+Vphi2CL.vphinc2.v
                     Term2CL.term2.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta2.v))
                     Term2CL.term2.v   = Term2CL.term2.v*sin(Vtheta2CL.vtheta2.v)*sin(Vphi2CL.vphi2.v)
                     PartLTS.particles_paramy(i+2,j).v = Term2CL.term2.v
                     Vtheta2CL.vtheta3.v = Vtheta2CL.vtheta3.v+Vtheta2CL.vthinc3.v
                     Vphi2CL.vphi3.v   = Vphi2CL.vphi3.v+Vphinc2CL.vphinc3.v
                     Term2CL.term3.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta3.v))
                     Term2CL.term3.v   = Term2CL.term3.v*sin(Vtheta2CL.vtheta3.v)*sin(Vtheta2CL.vphi3.v)
                     PartLTS.particles_paramy(i+3,j).v = Term2CL.term3.v
                  end do
               end do
     end subroutine ComputeYparam_ymm8r4

     subroutine ComputeZparam_ymm8r4(PartLTS,np,acn,acdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeZparam_ymm8r4
            use mod_vecconsts, only : ymm8r4_twopi,ymm8r4_one,ymm8r4_zero
            type(ChebParticlesLST_t),     intent(inout) :: PartLST
            integer(kind=int4),           intent(in)    :: np
            real(kind=sp), dimension(np), intent(in)    :: acn
            real(kind=sp), dimension(np), intent(in)    :: acdef
          
            ! Locals
            ! Error checking moved to the outside world
             type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
            end type Vtheta2CacheLines_t
            type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
            end type Vthinc2CacheLines_t
            type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
            end type Termx2CacheLines_t
             !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
            type(Vtheta2CacheLines_t) :: Vtheta2CL
             !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
            type(Vthinc2CacheLines_t) :: Vthinc2CL
             !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
            type(Termx2CacheLines_t) :: Term2CL
            !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
            type(YMM8r4_t), automatic :: cn_rand
            ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
            type(YMM8r4_t), automatic :: sphr_rand
            !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
            type(YMM8r4_t), automatic :: cdef_rand
            
            !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
            type(YMM8r4_t), automatic :: vNPTS
            !DIR$ ATTRIBUTES ALIGN : 32 :: vC
            type(YMM8r4_t), automatic :: vC
            !
            !DIR$ ATTRIBUTES ALIGN : 32 tmp1
            type(YMM8r4_t), automatic :: tmp1
            integer(kind=int4), automatic :: j,i
            ! Exec code .....
            cn_rand             = ymm8r4_zero
            sphr_rand           = ymm8r4_zero
            cdef_rand           = ymm8r4_zero
            Vtheta2CL.vtheta0   = ymm8r4_zero
            Vtheta2CL.vtheta1   = ymm8r4_zero
            Vtheta2CL.vtheta2   = ymm8r4_zero
            Vtheta2CL.vtheta3   = ymm8r4_zero
            Vthinc2CL.vthinc0   = ymm8r4_zero
            Vthinc2CL.vthinc1   = ymm8r4_zero
            Vthinc2CL.vthinc2   = ymm8r4_zero
            Vthinc2CL.vthinc3   = ymm8r4_zero
            vNPTS               = ymm8r4_zero
            tmp1                = ymm8r4_zero
            !
            vC                  = ymm8r4_zero
            Term2CL.term0       = ymm8r4_zero
            Term2CL.term1       = ymm8r4_zero
            Term2CL.term2       = ymm8r4_zero
            Term2CL.term3       = ymm8r4_zero
            vNPTS.v   = real(PartLTS.nzpts,kind=sp)
             ! First touch
            PartLTS.particles_paramz = ymm8r4_zero
            do j=1, PartLTS.np
                cn_rand.v   = acn(j)
                sphr_rand.v = PartLTS.particles_radii(j)
                cdef_rand.v = acdef(j)
                vC.v        = ymm8r4_twopi.v*sphr_rand.v
                tmp1.v      = vC.v/vNPTS.v
                Vthinc2CL.vthinc0.v   = tmp1.v
                Vthinc2CL.vthinc0.v   = Vthinc2CL.vthinc0.v*VINC.v
                Vthinc2CL.vthinc1.v   = tmp1.v
                Vthinc2CL.vthinc1.v   = Vthinc2CL.vthinc1.v*VINC2.v
                Vthinc2CL.vthinc2.v   = tmp1.v
                Vthinc2CL.vthinc2.v   = Vthinc2CL.vthinc2.v*VINC3.v
                Vthinc2CL.vthinc3.v   = tmp1.v
                Vthinc2CL.vthinc3.v   = Vthinc2CL.vthinc3.v*VINC4.v
                Vtheta2CL.vtheta0     = ymm8r4_zero
                Term2CL.term0         = ymm8r4_zero
                Vtheta2CL.vtheta1     = ymm8r4_zero
                Term2CL.term1         = ymm8r4_zero
                Vtheta2CL.vtheta2     = ymm8r4_zero
                Term2CL.term2         = ymm8r4_zero
                Vtheta2CL.vtheta3     = ymm8r4_zero
                Term2CL.term3         = ymm8r4_zero
                !DIR$ VECTOR ALWAYS
                do i=1, PartLTS.nzpts-3, 4
                     Vtheta2CL.vtheta0.v = Vtheta2CL.vtheta0.v+Vthinc2CL.vthinc0.v
                     Term2CL.term0.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta0.v))
                     Term2CL.term0.v   = term0.v*cos(Vtheta2CL.vtheta0.v)
                     PartLTS.particles_paramz(i+0,j).v = Term2CL.term0.v
                     Vtheta2CL.vtheta1.v = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                     Term2CL.term1.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta1.v))
                     Term2CL.term1.v   = term1.v*cos(Vtheta2CL.vtheta1.v)
                     PartLTS.particles_paramz(i+1,j).v = Term2CL.term1.v
                     Vtheta2CL.vtheta2.v = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                     Term2CL.term2.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta2.v))
                     Term2CL.term2.v   = term2.v*cos(Vtheta2CL.vtheta2.v)
                     PartLTS.particles_paramz(i+2,j).v = Term2CL.term2.v
                     Vtheta2CL.vtheta3.v = Vtheta2CL.vtheta3.v+Vthinc2CL.vthinc3.v
                     Term2CL.term3.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta3.v))
                     Term2CL.term3.v   = term3.v*cos(Vtheta2CL.vtheta3.v)
                     PartLTS.particles_paramz(i+3,j).v = Term2CL.term3.v
                 end do
              end do 
     end subroutine ComputeZparam_ymm8r4              
                    
     !==========================================================================80
     !            Computes Chebyshev particles surface area
     !            Scalar version only.
    !==========================================================================80
     subroutine ComputeSurface_scalar(PartLST,np,ac,acdef)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeSurface_scalar
          use mod_constants, only : pi2r4_const
          type(ChebParticlesLST_t),     intent(inout) :: PartLST
          integer(kind=int4),           intent(in)    :: np
          real(kind=sp), dimension(np), intent(in)    :: ac
          real(kind=sp), dimension(n),  intent(in)    :: acdef
          ! Locals
          integer(kind=int4) :: i,cd
          ! Keeping hot data aligned to L1D Cache line
          type :: CacheLineAligned_t
             real(kind=sp) :: term1,term2,term3,term4, &
                              term5,term5b,term6,term7,&
                              term8,totev,totod,tmp1,  &
                              tmp2,tmp3
             integer(kind=int1), dimension(0:7) :: pad
          end type CacheLineAligned_t
          type(CacheLineAligned_t) :: cla
          ! Exec code ....
          cla.term1  = 0.0_sp
          cla.term2  = 0.0_sp
          cla.term3  = 0.0_sp
          cla.term4  = 0.0_sp
          cla.term5  = 0.0_sp
          cla.term5b = 0.0_sp
          cla.term6  = 0.0_sp
          cla.term7  = 0.0_sp
          cla.term8  = 0.0_sp
          cla.totev  = 0.0_sp
          cla.totod  = 0.0_sp
          cla.tmp1   = 0.0_sp
          cla.tmp2   = 0.0_sp
          cla.tmp3   = 0.0_sp
          PartLTS.particles_surf = 0.0_sp
          do i=1, PartLST.np
             cla.term1 = 4.0_sp*pi2r_const*PartLST.particles_radii(i)**2     
             cd = int(ac(i),kind=int4)
             tmp1 = ac(i)
             tmp2 = acdef(i)
             tmp3 = tmp1**2-1.0_sp
             if(iand(cd,1) == 0) then     
                cla.term2 = 1.0_sp-2.0_sp*tmp2/(tmp1**2-1.0_sp)
                cla.term3 = tmp2**2*(tmp1**4+2.0_sp*tmp1**2-1.0_sp) / &
                            (4.0_sp*tmp1**2-1.0_sp)
                cla.term4 = 3.0_sp*tmp2**4*tmp1**8 / &
                            (64.0_sp*tmp1**4-12.0_sp*tmp1**2+1.0_sp)
                
                cla.term5 = -6.0_sp*tmp2**5*tmp1**8
                cla.term5b = 1.0_sp/tmp3*9.0_sp*tmp3*25.0*tmp3
                cla.totev  = cla.term1*(cla.term2+cla.term3-cla.term4-cla.term5*cla.term5b)
                PartLTS.particles_surf(i) = cla.totev
             else              
                cla.term2 = 1.0_sp+tmp2**2*(tmp1**4+2.0_sp*tmp3)/(4.0_sp*tmp3)         
                cla.term3 = 3.0_sp*tmp2**4*tmp1**4*0.015625_sp
                cla.term4 = 1.0_sp+20.0_sp*tmp3/(16.0_sp*tmp3*4.0_sp*tmp3)
                cla.totod = cla.term1*(cla.term2-cla.term3*cla.term4)
                PartLTS.particles_surf(i) = cla.totod
             end if
          end do
     end subroutine ComputeSurface_scalar    
                  
                                        
            
             
              
              
               
               
               
               
          
    
          
  


     
    
    
  
  
 
    

 
 

  
    
   
          
          
          
          
          
          
          
          
          
          
          
          


    
  

                  
  
                           
                       
      
  
    
    
   
 
 
   
    
    
    
    
    
    
end module mod_cheb_particles
