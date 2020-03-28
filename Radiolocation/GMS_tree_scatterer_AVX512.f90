

module mod_tree_scatterer_AVX512

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_tree_scatterer_AVX'
 !          
 !          Purpose:
  !                     Simplified non-geometrically acurrate plant (tree) representation.
  !                      For the modeling and simulation of radar backscattering.
  !                     Suitable for AVX512 explicit vectorization.
 !          History:
 !                        
 !                          Date: 26-04-2019
  !                         Time: 16:31 GMT+2
  !                         Modified:
  !                         Date: 16-12-2019
  !                         Time: 10:23 GMT+2
  !                         Modified
  !                         Date: 24-01-2020
  !                         Time: 15:11 AM GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 2
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
      use mod_kinds,    only : int1, int4, sp
      use mod_vectypes, only : ZMM16r4_t,Mask16_t
      implicit none
      public
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    ! Major version
  
     integer(kind=int4), parameter :: MOD_TREE_SCATTERER_AVX512_MAJOR = 1

     ! Minor version
     integer(kind=int4), parameter :: MOD_TREE_SCATTERER_AVX512_MINOR = 0

     ! Micro version
     integer(kind=int4), parameter :: MOD_TREE_SCATTERER_AVX512_MICRO = 0

     ! Module full version
     integer(kind=int4), parameter :: MOD_TREE_SCATTERER_AVX512_FULLVER = 1000*MOD_TREE_SCATTERER_AVX512_MAJOR + &
                                                                           100*MOD_TREE_SCATTERER_AVX512_MINOR  + &
                                                                           10*MOD_TREE_SCATTERER_AVX512_MICRO
     ! Module creation date
     character(*),      parameter  :: MOD_TREE_SCATTERER_AVX512_CREATION_DATE = "26-04-2019 16:31 +00200 (FRI 16 APR 2019 GMT+2)"

     ! Module build date
     character(*),      parameter  :: MOD_TREE_SCATTERER_AVX512_BUILD_DATE =  __DATE__ " " __TIME__

     ! Module short description
     character(*),      parameter  :: MOD_TREE_SCATTERER_AVX512_SYNOPSIS = "Plant(tree) model suitable for computation of the radar backscatter"

     ! Module version ID
     character(*),      parameter  :: MOD_TREE_SCATTERER_AVX512_VERSION_ID = &
          "$Id: GMS_tree_scatterer_AVX512.f90 1000 +00200 2020-01-24 15:24 beniekg@gmail.com $"

     real(kind=sp), dimension(0:15), parameter, private :: VINC0 = [1.0_sp,2.0_sp,3.0_sp,4.0_sp, &
                                                                    5.0_sp,6.0_sp,7.0_sp,8.0_sp, &
                                                                    9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                                    13.0_sp,14.0_sp,15.0_sp,16.0_sp]
                                                                    
     real(kind=sp), dimension(0:15), parameter, private :: VINC1 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                                    21.0_sp,22.0_sp,23.0_sp,24.0_sp, &
                                                                    25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                                    29.0_sp,30.0_sp,31.0_sp,32.0_sp]
     real(kind=sp), dimension(0:15), parameter, private :: VINC2 = [33.0_sp,34.0_sp,35.0_sp,36.0_sp, &
                                                                    37.0_sp,38.0_sp,39.0_sp,40.0_sp, &
                                                                    41.0_sp,42.0_sp,43.0_sp,44.0_sp, &
                                                                    45.0_sp,46.0_sp,47.0_sp,48.0_sp]
     real(kind=sp), dimension(0:15), parameter, private :: VINC3 = [49.0_sp,50.0_sp,51.0_sp,52.0_sp, &
                                                                    53.0_sp,54.0_sp,55.0_sp,56.0_sp, &
                                                                    57.0_sp,58.0_sp,59.0_sp,60.0_sp, &
                                                                    61.0_sp,62.0_sp,63.0_sp,64.0_sp]
     
     !integer(kind=int4), parameter, private :: PADR4 = 16
     !integer(kind=int4), parameter, private :: PADR8 = 8

#if !defined(GMS_TREE_SCATTERER_ADD_PADDING)
#define GMS_TREE_SCATTERER_AVX512_ADD_PADDING 1
#endif

     ! This is low termporal and spatial locality data type
     type, public :: TSColdAVX512_t
        public
        ! Number of leaves (represented as an ellipsoidal surface)
        integer(kind=int4) :: nleaves
        ! Number of branches (represented as an cylindrical volumes)
        integer(kind=int4) :: nbranches
        ! Number of simulation steps it is equal to Radar PRF (pulse repetetive frequency)
        integer(kind=int4) :: nsteps
        ! Tree scatterer ordinal number (for the forest simulation)
        integer(kind=int4) :: ordinal
        ! Number of parametric equation evaluation 'points' for the trunk cylindrical approximation
        integer(kind=int4) :: trunk_param_npoints
        ! Number of parametric equation evaluation 'points' for the leaves elliptical approximation
        integer(kind=int4) :: leaves_param_npoints
        ! Number of parametric equation evaluation 'points' for the branches cylindrical approximation
        integer(kind=int4) :: branches_param_npoints
        ! Total height of the tree
        real(kind=sp)      :: tree_height
        ! Height of the trunk only
        real(kind=sp)      :: trunk_height
        ! Radius of trunk  (averaged)
        real(kind=sp)      :: trunk_radius
        ! Height of crown only
        real(kind=sp)      :: crown_height
        ! Total crown area (approximated) as sum of leaves area
        real(kind=sp)      :: crown_area
        ! Trunk area (cylinder area)
        real(kind=sp)      :: trunk_area
        ! Total tree area
        real(kind=sp)      :: tree_area
        ! Tree geo-location latitude
        real(kind=sp)      :: tree_lat
        ! Tree geo-location longtitude
        real(kind=sp)      :: tree_lon ! 64-bytes (1st cache line)
        ! Tree elevation (above the sea level) (meters)
        real(kind=sp)      :: tree_elevation
                                       ! 64-bytes (2nd cache line)
        ! Is water or rather moistness present or not on the leaf surface (per n-leaves) (true or false) PAOS type number of elements
        ! must be nleaves/16
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_moistness
        logical(kind=int4), allocatable, dimension(:)   :: leaves_moistness
         ! Is water or rather moistness present or not on the branch surface (per n-branches) (true or false)
        !  PAOS type number of elements  ! must be nbranches/16
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_moistness
        logical(kind=int4), allocatable, dimension(:)   :: branches_moistness
         ! Trunk parametric equation (approximated as a cylindrical object)
        ! PAOS type size of array -- npoints/16
!DIR$ ATTRIBUTES ALIGN : 64 :: trunk_xparam
        type(ZMM16r4_t),      allocatable, dimension(:)   :: trunk_xparam
 ! PAOS type size of array -- npoints/16

!DIR$ ATTRIBUTES ALIGN : 64 :: trunk_yparam
        type(ZMM16r4_t),      allocatable, dimension(:)   :: trunk_yparam
 ! PAOS type size of array -- npoints/16
 
!DIR$ ATTRIBUTES ALIGN : 64 :: trunk_zparam
        type(ZMM16r4_t),      allocatable, dimension(:)   :: trunk_zparam
  ! Leaves thicknes (micron) per leaf
        ! PAOS type size of array nleaves/16

!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_thickness
        type(ZMM16r4_t),     allocatable, dimension(:)   :: leaves_thickness
          ! Leaves density (g/cm^3) per leaf
        ! PAOS type size of array nleaves/16

!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_density
        type(ZMM16r4_t),     allocatable, dimension(:)   :: leaves_density



        ! Leaves parameteric equation (approximated as an ellipses)
        ! Parameter x,(a*cos(t))
        
        ! PAOS type size of arrays  1st dim (evaluation of x) ,
        !  2nd dim (number of leaves)

!DIR$   ATTRIBUTES ALIGN : 64 :: leaves_xparam
        type(ZMM16r8_t),      allocatable, dimension(:,:) :: leaves_xparam
 ! Leaves parametric equation (approximated as an ellipses)
        ! Parameter y, (b*sin(t))
         ! PAOS type size of arrays is -- npoints/16 1st dim (evaluation of y) ,
        ! nleaves/8 2nd dim (number of leaves)

!DIR$   ATTRIBUTES ALIGN : 64 :: leaves_yparam
        type(ZMM16r8_t),      allocatable, dimension(:,:) :: leaves_yparam
!DIR$   ATTRIBUTES ALIGN : 64 :: branches_thickness
        type(ZMM16r4_t),      allocatable, dimension(:)   :: branches_thickness
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_density
        type(ZMM16r4_t),      allocatable, dimension(:)   :: branches_density

   ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter x, (r*cos(t))
        ! PAOS type size of arrays is -- npoints/16 1st dim (evaluation of x) ,
        ! nbranches/8 2nd dim (number of leaves)

!DIR$   ATTRIBUTES ALIGN : 64 :: branches_xparam
        type(ZMM16r4_t),      allocatable, dimension(:,:) :: branches_xparam
  ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter y, (r*sin(t))
        !2nd dimension is a branches number, 1st dimension evaluation of parameter y

!DIR$   ATTRIBUTES ALIGN : 64 :: branches_yparam
        type(ZMM16r4_t),      allocatable, dimension(:,:) :: branches_yparam
  ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter z, (height)
        !2nd dimension is a branch  number, 1st dimension evaluation of parameter z

!DIR$ ATTRIBUTES ALIGN : 64 :: branches_zparam
        type(ZMM16r4_t),      allocatable, dimension(:,:) :: branches_zparam         
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        logical(kind=int4), allocatable, dimension(:)   :: leaves_moistness  !GCC$ ATTRIBUTES aligned(64) :: leaves_moistness
        logical(kind=int4), allocatable, dimension(:)   :: branches_moistness  !GCC$ ATTRIBUTES aligned(64) :: branches_moistness
        type(ZMM16r4_t),    allocatable, dimension(:)   :: trunk_xparam    !GCC$ ATTRIBUTES aligned(64) :: trunk_xparam
        type(ZMM16r4_t),    allocatable, dimension(:)   :: trunk_yparam    !GCC$ ATTRIBUTES aligned(64) :: trunk_yparam
        type(ZMM16r4_t),    allocatable, dimension(:)   :: trunk_zparam   !GCC$ ATTRIBUTES aligned(64) :: trunk_zparam
        type(ZMM16r4_t),    allocatable, dimension(:)   :: leaves_thickness !GCC$ ATTRIBUTES aligned(64) :: leaves thickness
        type(ZMM16r4_t),    allocatable, dimension(:)   :: leaves_density  !GCC$ ATTRIBUTES aligned(64) :: leaves_density
        type(ZMM16r8_t),    allocatable, dimension(:,:) :: leaves_xparam !GCC$ ATTRIBUTES aligned(64) :: leaves_xparam
        type(ZMM16r8_t),    allocatable, dimension(:,:) :: leaves_yparam !GCC$ ATTIRBUTES aligned(64) :: leaves_yparam
        type(ZMM16r4_t),    allocatable, dimension(:)   :: branches_thickness !GCC$ ATTRIBUTES aligned(64) :: branches_thickness
        type(ZMM16r4_t),    allocatable, dimension(:)   :: branches_density !GCC$ ATTRIBUTES aligned(64) :: branches_density
        type(ZMM16r4_t),    allocatable, dimension(:,:) :: branches_xparam !GCC$ ATTRIBUTES aligned(64) :: branches_xparam
        type(ZMM16r4_t),    allocatable, dimension(:,:) :: branches_yparam  !GCC$ ATTRIBUTES aligned(64) :: branches_yparam
        type(ZMM16r4_t),    allocatable, dimension(:,:) :: branches_zparam  !GCC$ ATTRIBUTES aligned(64) :: branches_zparam
#endif
     end type TSColdAVX512_t

    
      
       
      
       
      
      
     ! This is a high termporal and spatial locality data type
     ! These data type members characteristics are varying between each sample of Radar PRF.
     type, public :: TSHotAVX512_t
        public
       
        ! Whole tree vibration in x-axis (radians)
        real(kind=sp)      :: tree_dphi
                !
        ! Whole tree vibration in y-axis (radians)
        real(kind=dp)      :: tree_dheta
       
        ! Tree total cross section (dimensionless)
        real(kind=sp)      :: tree_cross_section
        ! Crown cross section approximated as sum of leaves cross section.
        real(kind=sp)      :: crown_cross_section
        ! Trunk cross section
        real(kind=sp)      :: trunk_cross_section
#if   (GMS_TREE_SCATTERER_AVX512_ADD_PADDING) == 1
        integer(kind=int1), dimension(0:43), private :: pad2
#endif
        ! Leaves cross section (varying due to leaves vibrations)
        ! ( 1st dimension cross section variation per leaf,2nd dimension is PRF/s )
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_cross_section
        type(ZMM16r4_t), allocatable, dimension(:,:) :: leaves_cross_section
        ! Leaves reflectivity (varying due to leaves vibration)
        ! ( 1st dimension reflectivity(dbm) pers leaf, 2nd dimension is PRF/s,)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_reflectivity
        type(ZMM16r4_t), allocatable, dimension(:,:) :: leaves_reflectivity
        ! Branches cross section (varying due to branches vibrations)
        ! ( 1st dimension cross section variation per branch, 2nd dimension is PRF/s))
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_cross_section
        type(ZMM16r4_t), allocatable, dimension(:,:)  :: branches_cross_section
        ! Branches reflectivity (varying due to leaves vibration)
        ! ( 1st dimension reflectivity(dbm) pers branch, 2nd dimension is PRF/s))
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_reflectivity
        type(ZMM16r4_t), allocatable, dimension(:,:)  :: branches_reflectivity
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
        type(ZMM16r4_t), allocatable, dimension(:,:) :: leaves_cross_section !GCC$ ATTRIBUTES aligned(64) :: leaves_cross_section
        type(ZMM16r4_t), allocatable, dimension(:,:) :: leaves_reflectivity !GCC$ ATTRIBUTES aligned(64) :: leaves_reflectivity
        type(ZMM16r4_t), allocatable, dimension(:,:) :: branches_cross_section  !GCC$ ATTRIBUTES aligned(64) :: branches_cross_section
        type(ZMM16r4_t), allocatable, dimension(:,:) :: branches_reflectivity  !GCC$ ATTRIBUTES aligned(64) :: branches_reflectivity
#endif

    
     end type TSHotAVX512_t

     ! Leaves phase derived type
     type, public :: LeavesPhase_t
        public
#if defined __ICC || defined __INTEL_COMPILER
        ! Leaves theta angle of incidence (rad) per each leaf
        !DIR$ ATTRIBUTES ALIGN : 64 :: theta_inc
        real(kind=sp), allocatable, dimension(:) :: theta_inc
        ! Leaves phi angle of incidence
        !DIR$ ATTRIBUTES ALIGN : 64 :: phi_inc
        real(kind=sp), allocatable, dimension(:) :: phi_inc
        !DIR$ ATTRIBUTES ALIGN : 64 :: theta_scat
        real(kind=sp), allocatable, dimension(:) :: theta_scat
         !DIR$ ATTRIBUTES ALIGN : 64 :: phi_scat
        real(kind=sp), allocatable, dimension(:) :: phi_scat
          !DIR$ ATTRIBUTES ALIGN : 64 :: theta_dir
        real(kind=sp), allocatable, dimension(:) :: theta_dir
          !DIR$ ATTRIBUTES ALIGN : 64 :: phi_dir
        real(kind=sp), allocatable, dimension(:) :: phi_dir
          ! Allocate this array as (4,4,4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: l4x4phm
        real(kind=sp), allocatable, dimension(:,:,:,:) :: l4x4phm
         ! Allocate this array as (2,2,2,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: sm2x2avg
        complex(kind=sp), allocatable, dimension(:,:,:,:) :: sm2x2avg
          ! Allocate this as (2,2, number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: l2x2mp
        complex(kind=sp), allocatable, dimension(:,:,:) :: l2x2mp
          !DIR$ ATTRIBUTES ALIGN : 64 :: l2x2mn
        complex(kind=sp), allocatable, dimension(:,:,:) :: l2x2mn
        ! Allocate this array as a(4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig1x4lp
        complex(kind=sp), allocatable, dimension(:,:) :: eig1x4lp
        ! Allocate this array as a (4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig1x4ln
        complex(kind=sp), allocatable, dimension(:,:) :: eig1x4ln
        !Allocate this array as a (4,4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig4x4mp
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mp
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig4x4mn
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mn
        !Allocate this array as a (4,4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig4x4mpi
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mpi
        !DIR$ ATTRIBUTES ALIGN : 64 :: eig4x4mni
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mni
        ! Allocate this array as a (4,4,number of leaves)
        !DIR$ ATTRIBUTES ALIGN : 64 :: expa4x4mp
        real(kind=sp),    allocatable, dimension(:,:,:) :: expa4x4mp
        !DIR$ ATTRIBUTES ALIGN : 64 :: expa4x4mn
        real(kind=sp),    allocatable, dimension(:,:,:) :: expa4x4mn
        ! Allocate this array as (4,4,number of leaves)  (output)
        !DIR$ ATTRIBUTES ALIGN : 64 :: stokes4x4m
        real(kind=sp),    allocatable, dimension(:,:,:) :: stokes4x4m
        ! Allocate this array as (2,2,number of leaves)  (output)
        ! DIR$ ATTRIBUTES ALIGN : 64 :: scat2x2m
        complex(kind=sp), allocatable, dimension(:,:,:) :: scat2x2m
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        real(kind=sp), allocatable, dimension(:) :: theta_inc !GCC$ ATTRIBUTES aligned(64) :: theta_inc
        real(kind=sp), allocatable, dimension(:) :: phi_inc !GCC$ ATTRIBUTES aligned(64) :: phi_inc
        real(kind=sp), allocatable, dimension(:) :: theta_scat !GCC$ ATTRIBUTES aligned(64) :: theta_scat
        real(kind=sp), allocatable, dimension(:) :: phi_scat !GCC$ ATTRIBUTES aligned(64) :: phi_scat
        real(kind=sp), allocatable, dimension(:) :: theta_dir !GCC$ ATTRIBUTES aligned(64) :: theta_dir
        real(kind=sp), allocatable, dimension(:) :: phi_dir !GCC$ ATTRIBUTES aligned(64) :: phi_dir
        real(kind=sp), allocatable, dimension(:,:,:,:) :: l4x4phm !GCC$ ATTRIBUTES aligned(64) :: l4x4phm
        complex(kind=sp), allocatable, dimension(:,:,:,:) :: sm2x2avg !GCC$ ATTRIBUTES aligned(64) :: sm2x2avg
        complex(kind=sp), allocatable, dimension(:,:,:) :: l2x2mp !GCC$ ATTRIBUTES aligned(64) :: l2x2mp
        complex(kind=sp), allocatable, dimension(:,:,:) :: l2x2mn !GCC$ ATTRIBUTES aligned(64) :: l2x2mn
        complex(kind=sp), allocatable, dimension(:,:) :: eig1x4lp !GCC$ ATTRIBUTES aligned(64) :: eig1x4lp
        complex(kind=sp), allocatable, dimension(:,:) :: eig1x4ln !GCC$ ATTRIBUTES aligned(64) :: eig1x4ln
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mp !GCC$ ATTRIBUTES aligned(64) :: eig4x4mp
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mn !GCC$ ATTRIBUTES aligned(64) :: eig4x4mn
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mpi !GCC$ ATTRIBUTES aligned(64) :: eig4x4mpi
        complex(kind=sp), allocatable, dimension(:,:,:) :: eig4x4mni !GCC$ ATTRIBUTES aligned(64) :: eig4x4mni
        real(kind=sp),    allocatable, dimension(:,:,:) :: expa4x4mp !GCC$ ATTRIBUTES aligned(64) :: expa4x4mp
        real(kind=sp),    allocatable, dimension(:,:,:) :: expa4x4mn !GCC$ ATTRIBUTES aligned(64) :: expa4x4mn
        real(kind=sp),    allocatable, dimension(:,:,:) :: stokes4x4m !GCC$ ATTRIBUTES aligned(64) :: stokes4x4m
        complex(kind=sp), allocatable, dimension(:,:,:) :: scat2x2m   !GCC$ ATTRIBUTES aligned(64) :: scat2x2m
#endif
     end type LeavesPhase_t

   contains

#if defined __GFORTRAN__
     subroutine InitTreeScatterer_AVX512(DataCold,DataHot,LPhase,nleaves,nbranches,nsteps,ordinal,trunk_param_npoints, &
                                  leaves_param_npoints,branches_param_npoints,tree_height,          &
                                  trunk_height,trunk_radius,crown_height,         &
                                  tree_lat,tree_lon,tree_elevation,dphi,dtheta,  &
                                  errstate,iounit,logging,    &
                                  verbose,append,fname )                 !GCC$ ATTRIBUTES cold :: InitTreeScatterer_AVX512  !GCC$ ATTRIBUTES aligned(32) :: InitTreeScatterer_AVX512
#elif defined __INTEL_COMPILER
     subroutine InitTreeScatterer_AVX512(DataCold,DataHot,LPhase,nleaves,nbranches,nsteps,ordinal,trunk_param_npoints, &
                                  leaves_param_npoints,branches_param_npoints,tree_height,          &
                                  trunk_height,trunk_radius,crown_height,         &
                                  tree_lat,tree_lon,tree_elevation, dphi,dtheta,  &
                                  errstate,iounit,logging,    &
                                  verbose,append,fname )
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: InitTreeScatterer_AVX512
#endif
           use mod_constants,   only : INITVAL
           use mod_print_error, only : print_non_fatal_error, &
                                       handle_fatal_memory_error
           type(TSColdAVX512_t),        intent(inout) :: DataCold
           type(TSHotAVX512_t),         intent(inout) :: DataHot
           type(LeavesPhase_t),         intent(inout) :: LPhase
           integer(kind=int4),          intent(in)    :: nleaves
           integer(kind=int4),          intent(in)    :: nbranches
           integer(kind=int4),          intent(in)    :: nsteps
           integer(kind=int4),          intent(in)    :: ordinal
           integer(kind=int4),          intent(in)    :: trunk_param_npoints 
           integer(kind=int4),          intent(in)    :: leaves_param_npoints 
           integer(kind=int4),          intent(in)    :: branches_param_npoints
           real(kind=sp),               intent(in)    :: tree_height
           real(kind=sp),               intent(in)    :: trunk_height
           real(kind=sp),               intent(in)    :: trunk_radius
           real(kind=sp),               intent(in)    :: crown_height
           real(kind=sp),               intent(in)    :: tree_lat
           real(kind=sp),               intent(in)    :: tree_lon
           real(kind=sp),               intent(in)    :: tree_elevation
           real(kind=sp),               intent(in)    :: dphi
           real(kind=sp),               intent(in)    :: dtheta
           logical(kind=int1),          intent(inout) :: errstate
           integer(kind=int4),          intent(in)    :: iounit
           logical(kind=int4),          intent(in)    :: logging
           logical(kind=int4),          intent(in)    :: verbose
           logical(kind=int4),          intent(in)    :: append
           character(len=*),            intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           integer(kind=int4), automatic :: remainder
           ! Exec code ....
           if(nleaves <= 0 .or. nbranches <= 0) then
              errstate = .true.
              call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_tree_scatterer_AVX512, subroutine: InitTreeScatterer_AVX512: Invalid 'nleaves' or 'nbranches' argument!! ",  &
                                          __LINE__,__FILE__ )
              return
           end if
           remainder = 0
           remainder = MOD(nleaves,16)
           if(remainder) then
              nleaves = nleaves-remainder
           end if
           remainder = MOD(nbranches,16)
           if(remainder) then
              nbranches = nbranches-remainder
           end if
           remainder = MOD(trunk_param_npoints,16)
           if(remainder) then
              trunk_param_npoints = trunk_param_npoints-remainder
           end if
           remainder = MOD(leaves_param_npoints,16)
           if(remainder) then
              leaves_param_npoints = leaves_param_npoints-remainder
           end if
           remainder = MOD(branches_param_npoints,16)
           if(remainder) then
              branches_param_npoints = branches_param_npoints-remainder
           end if
           DataCold.nleaves     = nleaves
           DataCold.nbranches   = nbranches
           DataCold.nsteps      = nsteps
           DataCold.ordinal     = ordinal
           DataCold.trunk_param_npoints = trunk_param_npoints
           DataCold.leaves_param_npoints = leaves_param_npoints
           DataCold.branches_param_npoints = branches_param_npoints
           DataCold.tree_height = tree_height
           DataCold.trunk_height = trunk_height
           DataCold.trunk_radius = trunk_radius
           DataCold.crown_height = crown_height
           DataCold.crown_area   = INITVAL
           DataCold.trunk_area   = INITVAL
           DataCold.tree_area    = INITVAL
           DataCold.tree_lat     = tree_lat
           DataCold.tree_lon     = tree_lon
           DataCold.tree_elevation = tree_elevation
           DataHot.tree_dphi     = dphi
           DataHot.tree_dtheta   = dtheta
           allocate(DataCold.leaves_moistness(DataCold.nleaves), &
                    DataCold.branches_moistness(DataCold.nbranches), & 
                    DataCold.trunk_xparam(DataCold.trunk_param_npoints), &
                    DataCold.trunk_yparam(DataCold.trunk_param_npoints), &
                    DataCold.trunk_zparam(DataCold.trunk_param_npoints), &
                    DataCold.leaves_thickness(DataCold.nleaves), &
                    DataCold.leaves_density(DataCold.nleaves), &
                    DataCold.leaves_xparam(DataCold.leaves_param_npoints,DataCold.nleaves), &
                    DataCold.leaves_yparam(DataCold.leaves_param_npoints,DataCold.nleaves), &
                    DataCold.branches_thickness(DataCold.nbranches), &
                    DataCold.branches_density(DataCold.nbranches), &
                    DataCold.branches_xparam(DataCold.branches_param_npoints,DataCold.nbranches), &
                    DataCold.branches_yparam(DataCold.branches_param_npoints,DataCold.nbranches), &
                    DataCold.branches_zparam(DataCold.branches_param_npoints,DataCold.nbranches), &
                    DataHot.leaves_cross_section(DataCold.nsteps,DataCold.nleaves), &
                    DataHot.leaves_reflectivity(DataCold.nsteps,DataCold.nleaves), &
                    DataHot.branches_cross_section(DataCold.nsteps,DataCold.nbranches), &
                    DataHot.branches_reflectivity(DataCold.nsteps,DataCold.nbranches), &
                    LPhase.theta_inc(DataCold.nleaves), &
                    LPhase.phi_inc(DataCold.nleaves),   &
                    LPhase.theta_scat(DataCold.nleaves), &
                    LPhase.phi_scat(DataCold.nleaves),   &
                    LPhase.theta_dir(DataCold.nleaves),  &
                    LPhase.phi_dir(DataCold.nleaves),    &
                    LPhase.l4x4phm(4,4,4,DataCold.nleaves), &
                    LPhase.sm2x2avg(2,2,2,DataCold.nleaves), &
                    LPhase.l2x2mp(2,2,DataCold.nleaves),     &
                    LPhase.l2x2mn(2,2,DataCold.nleaves),     &
                    LPhase.eig1x4lp(4,DataCold.nleaves),     &
                    LPhase.eig1x4ln(4,DataCold.nleaves),     &
                    LPhase.eig4x4mp(4,4,DataCold.nleaves),   &
                    LPhase.eig4x4mn(4,4,DataCold.nleaves),   &
                    LPhase.eig4x4mpi(4,4,DataCold.nleaves),  &
                    LPhase.eig4x4mni(4,4,DataCold.nleaves),  &
                    LPhase.expa4x4mp(4,4,DataCold.nleaves),  &
                    LPhase.expa4x4mn(4,4,DataCold.nleaves),  &
                    LPhase.stokes4x4m(4,4,DataCold.nleaves), &
                    LPhase.scat2x2m(2,2,DataCold.nleaves),   &
                                  STAT=aerr,ERRMSG=emsg)
                    if(aerr /= 0) goto 9999  
                  
         
                    errstate = .false.
                    return
9999       call handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_tree_scatterer_AVX512, subroutine: InitTreeScatterer_AVX512 -- Memory Allocation Failure !!", &                                                     
                              "module: mod_tree_scatterer_AVX512, subroutine: InitTreeScatterer_AVX512 -- Memory Allocation Failure !!", &
                                                    emsg, 550) 
         
         
     end subroutine InitTreeScatterer_AVX512 
        
          
             
         
         
         
      
          
          
         
          
         
          
        
        
            
          
         
      
             
        
         
         
        
          
         
        
          
         
          
          
          
         
        
 
    

!====================================================================================================================!
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine SetMoistness_scalar(le_moist,br_moist,nleave,nbranch,iflag)  !GCC$ ATTRIBUTES cold :: SetMoistness_scalar !GCC$ ATTRIBUTES aligned(32) :: SetMoistness_scalar
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine SetMoistness_scalar(le_moist,br_moist,nleave,nbranch,iflag)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: SetMoistness_scalar
#endif
           
           logical(kind=int4),  contiguous, dimension(:), intent(inout) :: le_moist
           logical(kind=int4),  contiguous, dimension(:), intent(inout) :: br_moist 
           integer(kind=int4),                            intent(in)    :: nleave
           integer(kind=int4),                            intent(in)    :: nbranch
           integer(kind=int4),                            intent(in), optional    :: iflag
           ! Locals
          
           integer(kind=int4), parameter :: cutoff1  = 8191
           integer(kind=int4), parameter :: cutoff2  = 1073741823
           integer(kind=int4), automatic :: i
           logical(kind=int4), automatic :: bval
           ! Exec code ....
           ! First memory touch
           le_moist   = .false.
           br_moist = .false.
           bval = .false.
           if(present(iflag)) then
              do i=0, nleave
                 bval = irand(iflag)
                 if(cutoff1 < bval) then
                    le_moist(i) = .true.
                 else
                    le_moist(i) = .false.
                 end if
              end do
              bval = .false.
              do i=0, nbranch
                 bval = irand(iflag)
                 if(cutoff1 < bval) then
                    br_moist(i) = .true.
                 else
                    br_moist(i) = .false.
                 end if
              end do
           else
               do i=0, nleave
                 bval = irand()
                 if(cutoff2 > bval) then
                    le_moist(i) = .true.
                 else
                    le_moist(i) = .false.
                 end if
              end do
              bval = .false.
              do i=0, nbranch
                 bval = irand()
                 if(cutoff2 > bval) then
                    br_moist(i) = .true.
                 else
                    br_moist(i) = .false.
                 end if
              end do
           end if
           
            
     end subroutine SetMoistness_scalar
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeTrunkParamEq_zmm16r4(tr_xparam,tr_yparam,tr_zparam,radius, &
                                           height,npoints,zpoints) !GCC$ ATTRIBUTES hot :: ComputeTrunkParamEq_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeTrunkParamEq_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine ComputeTrunkParamEq_zmm16r4(tr_xparam,tr_yparam,tr_zparam,radius &
                                           height,npoints,zpoints)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeTrunkParametricEq_zmm16r4
#endif
           use mod_vecconsts, only : zmm16r4_twopi
           type(ZMM16r4_t),  contiguous, dimension(:), intent(inout) :: tr_xparam
           type(ZMM16r4_t),  contiguous, dimension(:), intent(inout) :: tr_yparam
           type(ZMM16r4_t),  contiguous, dimension(:), intent(inout) :: tr_zparam
           real(kind=sp),                              intent(in)    :: radius
           real(kind=sp),                              intent(in)    :: height
           integer(kind=int4),                         intent(in)    :: npoints
           integer(kind=int4),                         intent(in)    :: zpoints
           ! Locals
         
#if defined __ICC || defined __INTEL_COMPILER           
           !DIR$ ATTRIBUTES ALIGN : 64 :: vtheta0,vtheta1,vtheta2,vtheta3
           type(ZMM16r4_t),     automatic :: vtheta0,vtheta1,vtheta2,vtheta3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vtheta0 !GCC$ ATTRIBUTES aligned(64) :: vtheta0
           type(ZMM16r4_t),     automatic :: vtheta1 !GCC$ ATTRIBUTES aligned(64) :: vtheta1
           type(ZMM16r4_t),     automatic :: vtheta2 !GCC$ ATTRIBUTES aligned(64) :: vtheta2
           type(ZMM16r4_t),     automatic :: vtheta3 !GCC$ ATTRIBUTES aligned(64) :: vtheta3
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vthinc0,vthinc1,vthinc2,vthinc3
           type(ZMM16r4_t),     automatic :: vthinc0,vthinc1,vthinc2,vthinc3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vthinc0 !GCC$ ATTIRBUTES aligned(64) :: vthinc0
           type(ZMM16r4_t),     automatic :: vthinc1 !GCC$ ATTIRBUTES aligned(64) :: vthinc1
           type(ZMM16r4_t),     automatic :: vthinc2 !GCC$ ATTRIBUTES aligned(64) :: vthinc2
           type(ZMM16r4_t),     automatic :: vthinc3 !GCC$ ATTRIBUTES aligned(64) :: vthinc3
#endif
#if defined  __ICC || defined  __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vrad
           type(ZMM16r4_t),     automatic :: vrad
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vrad !GCC$ ATTRIBUTES aligned(64) :: vrad
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vhinc0,vhinc1,vhinc2,vhinc3
           type(ZMM16r4_t),     automatic :: vhinc0,vhinc1,vhinc2,vhinc3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vhinc0 !GCC$ ATTRIBUTES aligned(64) :: vhinc0
           type(ZMM16r4_t),     automatic :: vhinc1 !GCC$ ATTRIBUTES aligned(64) :: vhinc1
           type(ZMM16r4_t),     automatic :: vhinc2 !GCC$ ATTRIBUTES aligned(64) :: vhinc2
           type(ZMM16r4_t),     automatic :: vhinc3 !GCC$ ATTRIBUTES aligned(64) :: vhinc3
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vhinit0,vhinit1,vhinit2,vhinit3
           type(ZMM16r4_t),     automatic :: vhinit0,vhinit1,vhinit2,vhinit3
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vhinit0 !GCC$ ATTRIBUTES aligned(64) :: vhinit0
           type(ZMM16r4_t),     automatic :: vhinit1 !GCC$ ATTRIBUTES aligned(64) :: vhinit1
           type(ZMM16r4_t),     automatic :: vhinit2 !GCC$ ATTRIBUTES aligned(64) :: vhinit2
           type(ZMM16r4_t),     automatic :: vhinit3 !GCC$ ATTRIBUTES aligned(64) :: vhinit3
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: tmp1,tmp2
           type(ZMM16r4_t),     automatic :: tmp1,tmp2
#elif defined __GFORTRAN__
           type(ZMM16r4_t),     automatic :: tmp1 !GCC$ ATTRIBUTES aligned(64) :: tmp1
           type(ZMM16r4_t),     automatic :: tmp2 !GCC$ ATTRIBUTES aligned(64) :: tmp2
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
           type(ZMM16r4_t),     automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: vNPTS !GCC$ ATTRIBUTES aligned(32) :: vNPTS
#endif
           integer(kind=int4), automatic :: i
           !integer(kind=int4), automatic :: niter
           !Exec code .....
           
           ! First touch
           vtheta0.v = 0.0_sp;vtheta1.v = 0.0_sp
           vtheta2.v = 0.0_sp;vtheta3.v = 0.0_sp
           vthinc0.v = 0.0_sp;vthinc1.v = 0.0_sp
           vthinc2.v = 0.0_sp;vthinc3.v = 0.0_sp
           vhinc0.v  = 0.0_sp;vhinc1.v  = 0.0_sp
           vhinc2.v  = 0.0_sp;vhinc3.v  = 0.0_sp
           vhinit0.v = 0.0_sp;vhinit1.v = 0.0_sp
           vhinit2.v = 0.0_sp;vhinit3.v = 0.0_sp
           vrad.v   = 0.0_sp
           vNPTS.v  = 0.0_sp
           tmp1.v   = 0.0_sp
           tmp.v    = 0.0_sp
          ! vhinc.v  = 0.0_sp
          ! vhinit.v = 0.0_sp
           vNPTS.v  = real(npoints,kind=sp)
           tmp1.v    =  zmm16r4_twopi.v/vNPTS.v
           vthinc0.v = tmp1.v
           vthinc0.v = vthinc0.v*VINC0.v
           vthinc1.v = tmp1.v
           vthinc1.v = vthinc1.v*VINC1.v
           vthinc2.v = tmp1.v
           vthinc2.v = vthinc2.v*VINC2.v
           vthinc3.v = tmp1.v
           vthinc3.v = vthinc3.v*VINC3.v
           vrad.v    = radius
           zpoints   = zpoints + npoints ! skew the Z points in order to be not symmetric to x,y points
           tmp2.v    = height/real(zpoints,kind=sp)
           vhinc0.v  = tmp2.v
           vhinc0.v  = vhinc0.v*VINC0.v
           vhinc1.v  = tmp2.v
           vhinc1.v  = vhinc1.v*VINC1.v
           vhinc2.v  = tmp2.v
           vhinc2.v  = vhinc2.v*VINC2.v
           vhinc3.v  = tmp2.v
           vhinc3.v  = vhinc3.v*VINC3.v
           !niter = tsLTS.trunk_param_npoints
           ! First memory touch
           tr_xparam = zmm16r4_zero
           tr_yparam = zmm16r4_zero
           tr_zparam = zmm16r4_zero
#if defined __INTEL_COMPILER          
           !DIR$ VECTOR  ALIGNED
           !DIR$ VECTOR  ALWAYS
           !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__
           !GCC$ VECTOR
#endif
           do i=1, npoints-3, 4
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED tr_xparam:64
              !DIR$ ASSUME_ALIGNED tr_yparam:64
              !DIR$ ASSUME_ALIGNED tr_zparam:64
#endif
#if defined __GFORTRAN__
              !GCC$ builtin (cos) attributes simd
              !GCC$ builtin (sin) attributes simd
#endif
              vtheta0.v = vtheta0.v+vthinc0.v
              tr_xparam(i+0).v = vrad.v*cos(vtheta0.v)
              tr_yparam(i+0).v = vrad.v*sin(vtheta0.v)
              vtheta1.v = vtheta1.v+vthinc1.v
              tr_xparam(i+1).v = vrad.v*cos(vtheta1.v)
              tr_yparam(i+1).v = vrad.v*sin(vtheta1.v)
              vtheta2.v = vtheta2.v+vthinc2.v
              tr_xparam(i+2).v = vrad.v*cos(vtheta2.v)
              tr_yparam(i+2).v = vrad.v*sin(vtheta2.v)
              vtheta3.v = vtheta3.v+vthinc3.v
              tr_xparam(i+3).v = vrad.v*cos(vtheta3.v)
              tr_yparam(i+3).v = vrad.v*sin(vtheta3.v)
              vhinit0.v = vhinit0.v+vhinc0.v
              tr_zparam(i+0).v = vhinit0.v
              vhinit1.v = vhinit1.v+vhinc1.v
              tr_zparam(i+1).v = vhinit1.v
              vhinit2.v = vhinit2.v+vhinc2.v
              tr_zparam(i+2).v = vhinit2.v
              vhinit3.v = vhinit3.v+vhinc3.v
              tr_zparam(i+3).v = vhinit3.v
           end do
           
     end subroutine ComputeTrunkParamEq_zmm16r4
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine SetThickDensAng_zmm16r4(leaves_thick,leaves_dens,leaves_incang, &
                                       branch_thick,branch_dens,branch_incang, &
                                       bradii,nleaves,nbranches)                  !GCC$ ATTRIBUTES hot :: SetThickDensAng_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: SetThickDensAng_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine SetThickDensAng_zmm16r4(leaves_thick,leaves_dens,leaves_incang, &
                                       branch_thick,branch_dens,branch_incang, &
                                       bradii,nleaves,nbranches) 
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: SetThickDensAng_zmm16r4
#endif
           use mod_vectypes,  only : Mask8_t
           use mod_vecconsts, only : zmm16r4_zero,zmm16r4_one
           use mod_fpcompare, only : zmm16r4_rgt_zmm16r4, zmm16r4_rlt_zmm16r4,zmm16r4_equalto_zmm16r4
           type(ZMM16r4_t), contiguous, dimension(:),   intent(inout) :: leaves_thick
           type(ZMM16r4_t), contiguous, dimension(:),   intent(inout) :: leaves_dens
           type(ZMM16r4_t), contiguous, dimension(:,:), intent(inout) :: leaves_incang
           type(ZMM16r4_t), contiguous, dimension(:),   intent(inout) :: branch_thick
           type(ZMM16r4_t), contiguous, dimension(:),   intent(inout) :: branch_dens
           type(ZMM16r4_t), contiguous, dimension(:,:), intent(inout) :: branch_incang
           type(ZMM16r4_t), contiguous, dimension(:),   intent(in)    :: bradii
           integer(kind=int4),                          intent(in)    :: nleaves
           integer(kind=int4),                          intent(in)    :: nbranches
           ! Locals
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vScale
           type(ZMM16r4_t), parameter :: vScale = ZMM16r4_t(1000.0_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vScale = ZMM16r4_t(1000.0_sp) !GCC$ ATTRIBUTES aligned(64) :: vScale
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vLTlo
           type(ZMM16r4_t), parameter :: vLTlo = ZMM16r4_t(0.1_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vLTlo = ZMM16r4_t(0.1_sp)     !GCC$ ATTRIBUTES aligned(32) :: vLTlo
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vLThi
           type(ZMM16r4_t), parameter :: vLThi = ZMM16r4_t(0.7_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vLThi = ZMM16r4_t(0.7_sp)     !GCC$ ATTRIBUTES aligned(32) :: vLThi
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vLDlo
           type(ZMM16r4_t), parameter :: vLDlo = ZMM16r4_t(0.1_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vLDlo = ZMM16r4_t(0.1_sp)     !GCC$ ATTRIBUTES aligned(32) :: vLDlo
#endif
#if defined  __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vLDhi
           type(ZMM16r4_t), parameter :: vLDhi = ZMM16r4_t(0.6_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vLDhi = ZMM16r4_t(0.6_sp)     !GCC$ ATTIRBUTES aligned(32) :: vLDhi
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vPhlo
           type(ZMM16r4_t), parameter :: vPhlo = ZMM16r4_t(0.3_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vPhlo = ZMM16r4_t(0.3_sp)     !GCC$ ATTRIBUTES aligned(32) :: vPhlo
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vPhhi
           type(ZMM16r4_t), parameter :: vPhhi = ZMM16r4_t(0.7_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vPhhi = ZMM16r4_t(0.7_sp)     !GCC$ ATTRIBUTES aligned(32) :: vPhhi
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vThlo
           type(ZMM16r4_t), parameter :: vThlo = ZMM16r4_t(0.75_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vThlo = ZMM16r4_t(0.75_sp)    !GCC$ ATTIRBUTES aligned(32) :: vThlo
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vThhi
           type(ZMM16r4_t), parameter :: vThhi = ZMM16r4_t(1.5_sp)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), parameter :: vThhi = ZMM16r4_t(1.5_sp)     !GCC$ ATTRIBUTES aligned(32) :: vThhi
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: t1
           type(ZMM16r4_t),     automatic :: t1
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: t1  !GCC$ ATTRIBUTES aligned(32) :: t1
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: t2
           type(ZMM16r4_t),     automatic :: t2
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),     automatic :: t2  !GCC$ ATTRIBUTES aligned(32) :: t2
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vmt1gt
           type(Mask16_t),      automatic :: vmt1gt
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Mask16_t),      automatic :: vmt1gt  !GCC$ ATTRIBUTES aligned(32) :: vmt1gt
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vmt1lt
           type(Mask16_t),      automatic :: vmt1lt
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Mask16_t),      automatic :: vmt1lt  !GCC$ ATTRIBUTES aligned(32) :: vmt1lt
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vmt2gt
           type(Mask16_t),      automatic :: vmt2gt
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Mask16_t),      automatic :: vmt2gt  !GCC$ ATTRIBUTES aligned(32) :: vmt2gt
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vmt2lt
           type(Mask16_t),      automatic :: vmt2lt
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Mask16_t),      automatic :: vmt2lt  !GCC$ ATTRIBUTES aligned(32) :: vmt2lt
#endif
           integer(kind=int4), automatic :: i
           ! Exec code ....
           t1.v = 0.0_sp; t2.v = 0.0_sp
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           leaves_thick       = zmm16r4_zero
           leaves_dens        = zmm16r4_zero
          
           call RANDOM_SEED()
#if defined __INTEL_COMPILER
           !DIR$ VECTOR ALIGNED
           !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           !GCC$ VECTOR
#endif
           do i=1, nleaves
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED leaves_thick:64
              !DIR$ ASSUME_ALIGNED leaves_dense:64
#endif
              call RANDOM_NUMBER(t1.v)
              vmt1gt = zmm16r4_rgt_zmm16r4(t1,vLThi)
              vmt1lt = zmm16r4_tlt_zmm16r4(t1,vLTlo)
              if(ALL(vmt1gt.m)) then
                 t1.v = vLThi.v
              else if(ALL(vmt1lt.m)) then
                 t1.v = vLTlo.v
              end if
              leaves_thick(i).v = vScale.v*t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2gt = zmm16r4_rgt_zmm16r4(t2,vLDhi)
              vmt2lt = zmm16r4_rlt_zmm16r4(t2,vLDlo)
              if(ALL(vmt2gt.m)) then
                 t2.v = vLDhi.v
              else if(ALL(vmt2lt.m)) then
                 t2.v = vLDlo.v
              end if
              leaves_dens(i).v = t2.v
           end do
          ! t1 = ymm8r4_zero
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           leaves_incang = zmm16r4_zero
           call RANDOM_SEED()
           t1 = zmm16r4_zero
           t2 = zmm16r4_zero
#if defined __INTEL_COMPILER
           !DIR$ VECTOR ALIGNED
           !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           !GCC$ VECTOR
#endif           
           do i=1, nleaves
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED leaves_incang(1,1):64
#endif
              call RANDOM_NUMBER(t1.v)
              vmt1lt = zmm16r4_rlt_zmm16r4(t1,vPhlo)
              vmt1gt = zmm16r4_rgt_zmm16r4(t1,vPhhi)
              if(ALL(vmt1lt.m)) then
                 t1.v = vPhlo.v
              else if(ALL(vmt1gt.m)) then
                 t1.v = vPhhi.v
              end if
              leaves_incang(i,1).v = t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2lt = zmm16r4_rlt_zmm16r4(t2,vThlo)
              vmt2gt = zmm16r4_equalto_zmm16r4(t2,zmm16r4_one)
              if(ALL(vmt2lt.m)) then
                 t2.v = vThlo.v
              end if
              if(ALL(vmt2gt.m)) then
                 t2.v = zmm16r4_one
              end if
              leaves_incang(i,2).v = t2.v
           end do
           ! Similar procedure for branches
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           branch_incang = zmm16r4_zero
           call RANDOM_SEED()
           t1 = zmm16r4_zero
           t2 = zmm16r4_zero
#if defined __INTEL_COMPILER
           !DIR$ VECTOR ALIGNED
           !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           !GCC$ VECTOR
#endif           
           do i=1, nbranches
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED branch_incang(1,1):64
#endif
              call RANDOM_NUMBER(t1.v)
              vmt1gt = zmm16rr4_rgt_zmm16r4(t1,vLThi)
              vmt1lt = zmm16r4_tlt_zmm16r4(t1,vLTlo)
              if(ALL(vmt1gt.m)) then
                 t1.v = vLThi.v
              else if(ALL(vmt1lt.m)) then
                 t1.v = vLTlo.v
              end if
              branch_incang(i,1).v = t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2lt = zmm16r4_rlt_zmm16r4(t2,vThlo)
              vmt2gt = zmm16r4_equalto_zmm16r4(t2,zmm16r4_one)
              if(ALL(vmt2lt.m)) then
                 t2.v = vThlo.v
              end if
              if(ALL(vmt2gt.m)) then
                 t2.v = zmm16r4_one
              end if
              branch_incang(i,2).v = t2.v
            end do
              
           ! Density set to 0.0 (must find the exact data)
           ! Setting only the radii
           ! First touch
           branch_dens   = zmm16r4_zero
           branch_thick  = zmm16r4_zero
#if defined __INTEL_COMPILER
           !DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           !GCC$ VECTOR
#endif
           do i=1, nbranches
#if defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED branch_thick:64,bradii:64
#endif
              branch_thick(i).v = bradii(i).v
           end do
              
     end subroutine SetThickDensAng_zmm16r4
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeLeavesParamEq_zmm16r4(le_xparam,le_yparam,npoints,nleaves,va,vb) !GCC$ ATTRIBUTES hot :: ComputeLeavesParamEq_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeLeavesParamEq_zmm16r4
#elif defined __INTEL_COMPILER
     subroutine ComputeLeavesParamEq_zmm16r4(le_xparam,le_yparam,npoints,nleaves,va,vb)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeLeavesParamEq_zmm16r4
#endif           
           use mod_vecconsts,    only : zmm16r4_zero,zmm16r4_pi,zmm16r4_two
           type(ZMM16r4_t),  contiguous, dimension(:,:), intent(inout) :: le_xparam
           type(ZMM16r4_t),  contiguous, dimension(:,:), intent(inout) :: le_yparam
           integer(kind=int4),                           intent(in)    :: npoints
           integer(kind=int4),                           intent(in)    :: nleaves
           type(ZMM16r4_t),                              intent(in)    :: va
           type(ZMM16r4_t),                              intent(in)    :: vb
           ! Locals
           !
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: va_rand
           type(ZMM16r4_t), automatic   :: va_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic   :: va_rand   !GCC$ ATTRIBUTES aligned(64) :: va_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vb_rand
           type(ZMM16r4_t), automatic   :: vb_rand
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic   :: vb_rand   !GCC$ ATTRIBUTES aligned(64) :: vb_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vsqrt
           type(ZMM16r4_t), automatic   :: vsqrt
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic   :: vsqrt     !GCC$ ATTRIBUTES aligned(64) :: vsqrt
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 ::  vsqrt_arg
           type(ZMM16r4_t), automatic  ::  vsqrt_arg
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic  ::  vsqrt_arg !GCC$ ATTRIBUTES aligned(64) :: vsqrt_arg
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vC
           type(ZMM16r4_t), automatic ::   vC
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic ::   vC        !GCC$ ATTRIBUTES aligned(64) :: vC
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: tmp
           type(ZMM16r4_t), automatic ::   tmp
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic ::   tmp       !GCC$ ATTRIBUTES aligned(64) :: tmp
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vthinc0,vthinc1,vthinc2,vthinc3
           type(ZMM16r4_t), automatic ::   vthinc0,vthinc1,vthinc2,vthinc3
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic ::   vthinc0   !GCC$ ATTRIBUTES aligned(64) :: vthinc0
           type(ZMM16r4_t), automatic ::   vthinc1   !GCC$ ATTRIBUTES aligned(64) :: vthinc1
           type(ZMM16r4_t), automatic ::   vthinc2   !GCC$ ATTRIBUTES aligned(64) :: vthinc2
           type(ZMM16r4_t), automatic ::   vthinc3   !GCC$ ATTRIBUTES aligned(64) :: vthinc3
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
           type(ZMM16r4_t), automatic ::   vNPTS
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic ::   vNPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vtheta0,vtheta1,vtheta2,vtheta3
           type(ZMM16r4_t), automatic ::   vtheta0,vtheta1,vtheta2,vtheta3
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic ::   vtheta0  !GCC$ ATTRIBUTES aligned(64) :: vtheta0
           type(ZMM16r4_t), automatic ::   vtheta1  !GCC$ ATTRIBUTES aligned(64) :: vtheta1
           type(ZMM16r4_t), automatic ::   vtheta2  !GCC$ ATTRIBUTES aligned(64) :: vtheta2
           type(ZMM16r4_t), automatic ::   vtheta3  !GCC$ ATTRIBUTES aligned(64) :: vtheta3
#endif
           integer(kind=int4), automatic :: j,i
           !
           ! Exec code.....
           !
                     
          !
           va_rand    = zmm16r4_zero
           vb_rand    = zmm16r4_zero
           vsqrt      = zmm16r4_zero
           vsqrt_arg  = zmm16r4_zero
           vC         = zmm16r4_zero
           vthinc0    = zmm16r4_zero
           vthinc1    = zmm16r4_zero
           vthinc2    = zmm16r4_zero
           vthinc3    = zmm16r4_zero
           vNPTS      = zmm16r4_zero
           vtheta0    = zmm16r4_zero
           vtheta1    = zmm16r4_zero
           vtheta2    = zmm16r4_zero
           vtheta3    = zmm16r4_zero
           tmp        = zmm16r4_zero
           vNPTS.v    = real(npoints,kind=sp)
           ! First touch
           le_xparam = zmm16r4_zero
           le_yparam = zmm16r4_zero
           do j=1, nleaves
              call RANDOM_SEED()
              call RANDOM_NUMBER(va_rand.v)
              call RANDOM_NUMBER(vb_rand.v)
              ! Check for 0.0_sp will be too costly (horizontal operation)
              ! leaving it out
              va.v        = va.v+va_rand.v
              vb.v        = vb.v+vb_rand.v
              vsqrt_arg.v = zmm16r4_two.v*(va.v**2+vb.v**2)
              vsqrt.v     = sqrt(vsqrt_arg.v) 
              vC.v        = zmm16r4_pi.v * vsqrt.v
              tmp.v       = vC.v/vNPTS.v
              vthinc0.v   = tmp.v
              vthinc0.v   = vthinc0.c*VINC0.v
              vthinc1.v   = tmp.v
              vthinc1.v   = vthinc1.v*VINC1.v
              vthinc2.v   = tmp.v
              vthinc2.v   = vthinc2.v*VINC2.v
              vthinc3.v   = tmp.v
              vthinc3.v   = vthinc3.v*VINC3.v
              vtheta0     = zmm16r4_zero
              vtheta1     = zmm16r4_zero
              vtheta2     = zmm16r4_zero
              vtheta3     = zmm16r4_zero
#if defined __INTEL_COMPILER
              !DIR$ VECTOR ALIGNED
              !DIR$ VECTOR ALWAYS
              !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__
              !GCC$ VECTOR
#endif
              do i=1, npoints-3,4
#if defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED le_xparam:64
                 !DIR$ ASSUME_ALIGNED le_yparam:64
#endif
#if defined __GFORTRAN__
                 !GCC$ builtin (sin) attributes simd
                 !GCC$ builtin (cos) attributes simd
#endif
                 vtheta0.v = vtheta0.v+vthinc0.v
                 le_xparam(i+0,j).v = va.v*cos(vtheta0.v)
                 le_yparam(i+0,j).v = vb.v*sin(vtheta0.v)
                 vtheta1.v = vtheta1.v+vthinc1.v
                 le_xparam(i+1,j).v = va.v*cos(vtheta1.v)
                 le_yparam(i+1,j).v = vb.v*sin(vtheta1.v)
                 vtheta2.v = vtheta2.v+vthinc2.v
                 le_xparam(i+2,j).v = va.v*cos(vtheta2.v)
                 le_yparam(i+2,j).v = vb.v*sin(vtheta2.v)
                 vtheta3.v = vtheta3.v+vthinc3.v
                 le_xparam(i+3,j).v = va.v*cos(vtheta3.v)
                 le_yparam(i+3,j).v = vb.v*sin(vtheta3.v)
              end do
           end do
     end subroutine ComputeLeavesParamEq_zmm16r4      

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeBranchesParamEq_zmm16r4(br_xparam,br_yparam,br_zparam,nbranches,npoints, &
          vrad,vz,nzpts)     !GCC$ ATTRIBUTES hot :: ComputeBranchesParamEq_zmm16r4  !GCC$ ATTRIBUTES aligned(32) :: ComputeBranchesParamEq_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
       subroutine ComputeBranchesParamEq_zmm16r4(br_xparam,br_yparam,br_zparam,nbranches,npoints, &
         vrad,vz,nzpts)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeBranchesParamEq_zmm16r4
#endif
           use mod_vecconsts, only : zmm16r4_zero
           type(ZMM16r4_t),  contiguous, dimension(:,:),  intent(inout) :: br_xparam
           type(ZMM16r4_t),  contiguous, dimension(:,:),  intent(inout) :: br_yparam
           type(ZMM16r4_t),  contiguous, dimension(;,:),  intent(inout) :: br_zparam
           integer(kind=int4),                            intent(in)    :: nbranches
           integer(kind=int4),                            intent(in)    :: npoints
           type(ZMM16r4_t),                               intent(in)    :: vrad
           type(ZMM16r4_t),                               intent(in)    :: vz
           integer(kind=int4),                            intent(in)    :: nzpts
           ! Locals
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 ::  vr_rand
           type(ZMM16r4_t),  automatic :: vr_rand
#elif defined __GFORTRAN__
           type(ZMM16r4_t),  automatic :: vr_rand     !GCC$ ATTRIBUTES aligned(64) :: vr_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 ::  vz_rand
           type(ZMM16r4_t),  automatic :: vz_rand
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER  
           type(ZMM16r4_t),  automatic :: vz_rand     !GCC$ ATTRIBUTES aligned(64) :: vz_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 ::  vtheta0,vtheta1,vtheta2,vtheta3
           type(ZMM16r4_t),  automatic :: vtheta0,vtheta1,vtheta2,vtheta3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vtheta0     !GCC$ ATTRIBUTES aligned(64) :: vtheta0
           type(ZMM16r4_t),  automatic :: vtheta1     !GCC$ ATTRIBUTES aligned(64) :: vtheta1
           type(ZMM16r4_t),  automatic :: vtheta2     !GCC$ ATTRIBUTES aligned(64) :: vtheta2
           type(ZMM16r4_t),  automatic :: vtheta3     !GCC$ ATTRIBUTES aligned(64) :: vtheta3
#endif
#if defined __ICC || defined __INTEL_COMPILER           
           !DIR$ ATTRIBUTES ALIGN : 64 ::  vthinc0,vthinc1,vthinc2,vthinc3
           type(ZMM16r4_t),  automatic :: vthinc0,vthinc1,vthinc2,vthinc3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vthinc0     !GCC$ ATTRIBUTES aligned(64) :: vthinc0
           type(ZMM16r4_t),  automatic :: vthinc1     !GCC$ ATTRIBUTES aligned(64) :: vthinc1
           type(ZMM16r4_t),  automatic :: vthinc2     !GCC$ ATTRIBUTES aligned(64) :: vthinc2
           type(ZMM16r4_t),  automatic :: vthinc3     !GCC$ ATTRIBUTES aligned(64) :: vthinc3
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vhinc0,vhinc1,vhinc2,vhinc3
           type(ZMM16r4_t),  automatic :: vhinc0,vhinc1,vhinc2,vhinc3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vhinc0      !GCC$ ATTRIBUTES aligned(64) :: vhinc0
           type(ZMM16r4_t),  automatic :: vhinc1      !GCC$ ATTRIBUTES aligned(64) :: vhinc1
           type(ZMM16r4_t),  automatic :: vhinc2      !GCC$ ATTRIBUTES aligned(64) :: vhinc2
           type(ZMM16r4_t),  automatic :: vhinc3      !GCC$ ATTRIBUTES aligned(64) :: vhinc3
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vhinit0,vhinit1,vhinit2,vhinit3
           type(ZMM16r4_t),  automatic :: vhinit0,vhinit1,vhinit2,vhinit3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vhinit0     !GCC$ ATTRIBUTES aligned(64) :: vhinit0
           type(ZMM16r4_t),  automatic :: vhinit1     !GCC$ ATTRIBUTES aligned(64) :: vhinit1
           type(ZMM16r4_t),  automatic :: vhinit2     !GCC$ ATTRIBUTES aligned(64) :: vhinit2
           type(ZMM16r4_t),  automatic :: vhinit3     !GCC$ ATTRIBUTES aligned(64) :: vhinit3
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
           type(ZMM16r4_t),  automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vNPTS       !GCC$ ATTRIBUTES aligned(64) :: vNPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNZPTS
           type(ZMM16r4_t),  automatic :: vNZPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: vNZPTS      !GCC$ ATTRIBUTES aligned(64) :: vNZPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: tmp1,tmp2
           type(ZMM16r4_t),  automatic :: tmp1,tmp2
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t),  automatic :: tmp1        !GCC$ ATTRIBUTES aligned(64) :: tmp1
           type(ZMM16r4_t),  automatic :: tmp2        !GCC$ ATTRIBUTES aligned(64) :: tmp2
#endif
           integer(kind=int4), automatic :: j,i
           ! Exec code......
           ! First touch
           vr_rand  = zmm16r4_zero
           vz_rand  = zmm16r4_zero
           vtheta0  = zmm16r4_zero
           vtheta1  = zmm16r4_zero
           vtheta2  = zmm16r4_zero
           vtheta3  = zmm16r4_zero
           vthinc0  = zmm16r4_zero
           vthinc1  = zmm16r4_zero
           vthinc2  = zmm16r4_zero
           vthinc3  = zmm16r4_zero
           vhinc0   = zmm16r4_zero
           vhinc1   = zmm16r4_zero
           vhinc2   = zmm16r4_zero
           vhinc3   = zmm16r4_zero
           vhinit0  = zmm16r4_zero
           vhinit1  = zmm16r4_zero
           vhinit2  = zmm16r4_zero
           vhinit3  = zmm16r4_zero
           tmp1     = zmm16r4_zero
           tmp2     = zmm16r4_zero
           vNPTS   = zmm16r4_zero
           vNZPTS  = zmm16r4_zero
           vNPTS.v = real(npoints,kind=sp)
           ! 
           vNZPTS.v  = real(npoints+nzpts,kind=sp)
           tmp1.v    = zmm16r4_twopi.v/vNPTS.v
           vthinc0.v = tmp1.v
           vthinc0.v = vthinc0.v*VINC0.v
           vthinc1.v = tmp1.v
           vthinc1.v = vthinc1.v*VINC1.v
           vthinc2.v = tmp1.v
           vthinc2.v = vthinc2.v*VINC2.v
           vthinc3.v = tmp1.v
           vthinc3.v = vthinc3.v*VINC3.v
           br_xparam = zmm16r4_zero
           br_yparam = zmm16r4_zero
           br_zparam = zmm16r4_zero
           do j=1, nbranches
              call RANDOM_SEED()
              vr_rand  = zmm16r4_zero
              vz_rand  = zmm16r4_zero
              call RANDOM_NUMBER(vr_rand.v)
              vrad.v   = vrad.v+vr_rand.v
              call RANDOM_NUMBER(vz_rand.v)
              vz.v      = vz.v+vz_rand.v
              tmp2.v    = vz.v/vNZPTS.v
              vhinc0.v  = tmp2.v
              vhinc0.v  = vhinc0.v*VINC0.v
              vhinc1.v  = tmp2.v
              vhinc1.v  = vhinc1.v*VINC1.v
              vhinc2.v  = tmp2.v
              vhinc2.v  = vhinc2.v*VINC2.v
              vhinc3.v  = tmp2.v
              vhinc3.v  = vhinc3.v*VINC3.v
              vtheta0  = zmm16r4_zero
              vhinit0  = zmm16r4_zero
              vtheta1  = zmm16r4_zero
              vhinit1  = zmm16r4_zero
              vtheta2  = zmm16r4_zero
              vhinit2  = zmm16r4_zero
              vtheta3  = zmm16r4_zero
              vhinit3  = zmm16r4_zero
#if defined __INTEL_COMPILER
              !DIR$ VECTOR ALIGNED
              !DIR$ VECTOR ALWAYS
              !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__
              !GCC$ VECTOR
#endif
              do i=1, npoints-3, 4
#if defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED br_xparam(1,1):64
                 !DIR$ ASSUME_ALIGNED br_yparam(1,1):64
                 !DIR$ ASSUME_ALIGNED br_zparam(1,1):64
#endif
#if defined __GFORTRAN__
                 !GCC$ builtin (sin) attributes simd
                 !GCC$ builtin (cos) attributes simd
#endif
                 vtheta0.v = vtheta0.v+vthinc0.v
                 br_xparam(i+0,j).v = vrad.v*cos(vtheta0.v)
                 br_yparam(i+0,j).v = vrad.v*sin(vtheta0.v)
                 vhinit0.v = vhinit0.v+vhinc0.v
                 br_zparam(i+0,j).v = vhinit0.v
                 vtheta1.v = vtheta1.v+vthinc1.v
                 br_xparam(i+1,j).v = vrad.v*cos(vtheta1.v)
                 br_yparam(i+1,j).v = vrad.v*sin(vtheta1.v)
                 vhinit1.v = vhinit1.v+vhinc1.v
                 br_zparam(i+1,j).v = vhinit1.v
                 vtheta2.v = vtheta2.v+vthinc2.v
                 br_xparam(i+2,j).v = vrad.v*cos(vtheta2.v)
                 br_yparam(i+2,j).v = vrad.v*sin(vtheta2.v)
                 vhinit2.v = vhinit2.v+vhinc2.v
                 br_zparam(i+2,j).v = vhinit2.v
                 vtheta3.v = vtheta3.v+vthinc3.v
                 br_xparam(i+3,j).v = vrad.v*cos(vtheta3.v)
                 br_yparam(i+3,j).v = vrad.v*sin(vtheta3.v)
                 vhinit3.v = vhinit3.v+vhinc3.v
                 br_zparam(i+3,j).v = vhinit3.v
              end do
           end do
     end subroutine ComputeBranchesParamEq_zmm16r4

         
         
             
     
         
           
         

          
            
        

           
         

         
            
         

        
            
         

          
             
        

         
            
          

           
            
         
        

        
           
         

          
           

            
          
          

          
            
        

         
             
          

          
            
         

        
            
        

         
             
         
        

         
     
   end module mod_tree_scatterer_AVX512
