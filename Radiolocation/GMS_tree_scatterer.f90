
module mod_tree_scatterer

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_tree_scatterer'
 !          
 !          Purpose:
  !                     Simplified non-geometrically acurrate plant (tree) representation.
 !                      For the modeling and simulation of radar backscattering.
 !          History:
 !                        
 !                        Date: 26-04-2019
 !                        Time: 16:31 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
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
      use mod_vectypes, only : YMM8r4_t,YMM4r8_t,Mask8_t
     implicit none

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    ! Major version
  
     integer(kind=int4), parameter, public :: MOD_TREE_SCATTERER_MAJOR = 1

     ! Minor version
     integer(kind=int4), parameter, public :: MOD_TREE_SCATTERER_MINOR = 0

     ! Micro version
     integer(kind=int4), parameter, public :: MOD_TREE_SCATTERER_MICRO = 0

     ! Module full version
     integer(kind=int4), parameter, public :: MOD_TREE_SCATTERER_FULLVER = 1000*MOD_TREE_SCATTERER_MAJOR + &
                                                                           100*MOD_TREE_SCATTERER_MINOR  + &
                                                                           10*MOD_TREE_SCATTERER_MICRO
     ! Module creation date
     character(*),      parameter, public  :: MOD_TREE_SCATTERER_CREATION_DATE = "26-04-2019 16:31 +00200 (FRI 16 APR 2019 GMT+2)"

     ! Module build date
     character(*),      parameter, public  :: MOD_TREE_SCATTERER_BUILD_DATE = "00-00-0000 00:00"

     ! Module short description
     character(*),      parameter, public  :: MOD_TREE_SCATTERER_SYNOPSIS = "Plant(tree) model suitable for computation of the radar backscatter"

     real(kind=sp), dimension(0:7), parameter :: VINC = [1.0_sp,2.0_sp,3.0_sp, &
                                                         4.0_sp,5.0_sp,6.0_sp,7.0_sp,8.0_sp]
     real(kind=sp), dimension(0:7), parameter :: VINC2 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                         13.0_sp,14.0_sp,15.0_sp,16.0_sp]
     real(kind=sp), dimension(0:7), parameter :: VINC3 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                          21.0_sp,22.0_sp,23.0_sp,24.0_sp]
     real(kind=sp), dimension(0:7), parameter :: VINC4 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                          29.0_sp,30.0_sp,31.0_sp]
     
     integer(kind=int4), parameter, private :: PADR4 = 16
     integer(kind=int4), parameter, private :: PADR8 = 8

     !DIR$ IF .NOT. DEFINED (GMS_MOD_TREE_SCATTERER_ADD_PADDING)
             !DIR$ DEFINE GMS_MOD_TREE_SCATTERER_ADD_PADDING = 1
     !DIR$ ENDIF
     ! Adding Fortran array descriptor type alignment
     ! GFortran sources
     
     !struct descriptor_dimension {
     !    ! size of ptrdiff_t (64-bit) system is 8 bytes
     !     ptrdiff_t stride;
     !     ptrdiff_t lower_bound;
     !     ptrdiff_t upper_bound;
     ! }

     ! template<int Rank, typename Type>
     ! struct descriptor {
     !     Type* base_addr;
     !     size_t offset;
     !     ptrdiff_t dtype;
     !     descriptor_dimension dim[Rank]; array dimension
     ! }
     ! For example for 1D allocatable array sizeof(descriptor) == 48-bytes.

     ! This is low termporal and spatial locality data type
     type, public :: TreeScattererLTS_t
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
        real(kind=sp)      ;: trunk_radius
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
        ! must be nleaves/8
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_moistness
        logical(kind=int4), allocatable, dimension(:)   :: leaves_moistness 
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad1               
!DIR$ ENDIF
        ! Is water or rather moistness present or not on the branch surface (per n-branches) (true or false)
        !  PAOS type number of elements  ! must be nbranches/8
       
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_moistness
        logical(kind=int4), allocatable, dimension(:)   :: branches_moistness 
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad2                 
!DIR$ ENDIF
      
        ! Trunk parametric equation (approximated as a cylindrical object)
         ! PAOS type size of array -- npoints/8
!DIR$ ATTRIBUTES ALIGN : 64 :: trunk_xparam
        type(YMM8r4_t),      allocatable, dimension(:)   :: trunk_xparam          
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad5                         
!DIR$ ENDIF
         ! PAOS type size of array -- npoints/8
!DIR$ ATTRIBUTES ALIGN : 64 :: trunk_yparam
        type(YMM8r4_t),      allocatable, dimension(:)   :: trunk_yparam          
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad6                        
!DIR$ ENDIF
        ! PAOS type size of array -- npoints/8
!DIR$ ATTRIBUTES ALIGN ; 64 :: trunk_zparam
        type(YMM8r4_t),      allocatable, dimension(:)   :: trunk_zparam            
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad7                    
        !DIR$ ENDIF
        ! Leaves thicknes (micron) per leaf
        ! PAOS type size of array nleaves/8
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_thickness
        type(YMM8r4_t),     allocatable, dimension(:)   :: leaves_thickness
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private  :: pad8
!DIR$ ENDIF
        ! Leaves density (g/cm^3) per leaf
        ! PAOS type size of array nleaves/8
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_density
        type(YMM8r4_t),     allocatable, dimension(:)   :: leaves_density
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private  :: pad9
!DIR$ ENDIF
        ! Leaves surface angle to inpinging Radar waveform (rad)
        ! PAOS type size of array nleaves/8
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_incident_angle (theta,phi angles)
        type(YMM8r4_t),     allocatable, dimension(:,:)   :: leaves_incident_angle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15),  private :: pad10
!DIR$ ENDIF
        ! Leaves parameteric equation (approximated as an ellipses)
        ! Parameter x,(a*cos(t))
        
        ! PAOS type size of arrays  1st dim (evaluation of x) ,
        !  2nd dim (number of leaves)
!DIR$   ATTRIBUTES ALIGN : 64 :: leaves_xparam
        type(YMM4r8_t),      allocatable, dimension(:,:) :: leaves_xparam
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55),  private :: pad11
!DIR$ ENDIF
        ! Leaves parametric equation (approximated as an ellipses)
        ! Parameter y, (b*sin(t))
         ! PAOS type size of arrays is -- npoints/4 1st dim (evaluation of y) ,
        ! nleaves/4 2nd dim (number of leaves)  
!DIR$   ATTRIBUTES ALIGN : 64 :: leaves_yparam
        type(YMM4r8_t),      allocatable, dimension(:,:) :: leaves_yparam
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55),  private :: pad12                  
!DIR$ ENDIF
       
      
!DIR$   ATTRIBUTES ALIGN : 64 :: branches_thickness
        type(YMM8r4_t),      allocatable, dimension(:)   :: branches_thickness
!DIR$   IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15), private :: pad13
!ENDIF
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_density
        type(YMM8r4_t),      allocatable, dimension(:)   :: branches_density
!DIR$   IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15), private :: pad14
!DIR$   ENDIF
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_incident_angle
        type(YMM8r4_t),      allocatable, dimension(:,:)   :: branches_incident_angle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:15), private :: pad15
 !DIR$ ENDIF
        ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter x, (r*cos(t))
        ! PAOS type size of arrays is -- npoints/4 1st dim (evaluation of x) ,
        ! nbranches/4 2nd dim (number of leaves)  
!DIR$   ATTRIBUTES ALIGN : 64 :: branches_xparam
        type(YMM4r8_t),      allocatable, dimension(:,:) :: branches_xparam
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55),  private :: pad16              
!DIR$ ENDIF
        ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter y, (r*sin(t))
        !2nd dimension is a branches number, 1st dimension evaluation of parameter y
!DIR$   ATTRIBUTES ALIGN : 64 :: branches_yparam
        type(YMM4r8_t),      allocatable, dimension(:,:) :: branches_yparam
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad17                 
!DIR$ ENDIF
        ! Branches parametric equation (approximated as a cylindrical objects)
        ! Parameter z, (height)
        !2nd dimension is a branch  number, 1st dimension evaluation of parameter z
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_zparam
        type(YMM4r8_t),      allocatable, dimension(:,:) :: branches_zparam 
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad18            
!DIR$ ENDIF
     end type TreeScattererLTS_t

    
      
       
      
       
      
      
     ! This is a high termporal and spatial locality data type
     ! These data type members characteristics are varying between each sample of Radar PRF.
     type, public :: TreeScattererHTS_t
        public
       
        ! Whole tree vibration in x-axis (radians)
        real(kind=sp)      :: tree_xangle
        !DIR$   IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:3), private :: pad0
        !DIR$ ENDIF
        !
        ! Whole tree vibration in y-axis (radians)
        ! Sine of x-axis angle
        real(kind=dp)      :: sin_xangle
        ! Cosine of x-axis angle
        real(kind=dp)      :: cos_xangle
        real(kind=sp)      :: tree_yangle
        !DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:3), private :: pad1
        !DIR$ ENDIF
        ! Sine of y-axis angle
        real(kind=dp)      :: sin_yangle
        ! Cosine of y-axis angle
        real(kind=dp)      :: cos_yangle
        ! Tree total cross section (dimensionless)
        real(kind=sp)      :: tree_cross_section
        ! Crown cross section approximated as sum of leaves cross section.
        real(kind=sp)      :: crown_cross_section
        ! Trunk cross section
        real(kind=sp)      :: trunk_cross_section
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:3), private :: pad2
!DIR$ ENDIF
        ! Leaves cross section (varying due to leaves vibrations)
        ! ( 1st dimension cross section variation per leaf,2nd dimension is PRF/s )
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_cross_section
        type(YMM8r4_t), allocatable, dimension(:,:) :: leaves_cross_section
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad3
!DIR$ ENDIF
        ! Leaves reflectivity (varying due to leaves vibration)
        ! ( 1st dimension reflectivity(dbm) pers leaf, 2nd dimension is PRF/s,)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_reflectivity
        type(YMM8r4_t), allocatable, dimension(:,:) :: leaves_reflectivity
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad4
!DIR$ ENDIF
        ! 
        ! Branches cross section (varying due to branches vibrations)
        ! ( 1st dimension cross section variation per branch, 2nd dimension is PRF/s))
        ! 
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_cross_section
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_cross_section
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad5
!DIR$ ENDIF        
        ! Branches reflectivity (varying due to leaves vibration)
        ! ( 1st dimension reflectivity(dbm) pers branch, 2nd dimension is PRF/s))
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_reflectivity
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_reflectivity
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad6
!DIR$ ENDIF
        ! Leaves angle of vibration in x-axis per PRF/s
        ! 1st dimension angle values (rad),  2nd dimension PRF/s,
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad7
!DIR$ ENDIF        
        ! Leaves sine of vibration angle in x-axis per PRF/s
        ! 1st  dimension sine of vibrational angle (rad),  2nd dimension PRF/s,
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_sin_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_sin_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad8
!DIR$ ENDIF        
        ! Leaves sine of vibration angle in x-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension sine of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_cos_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_cos_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad9
!DIR$ ENDIF        
        ! Leaves angle of vibration in y-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension angle values (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_yangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad10
!DIR$ ENDIF        
        ! Leaves sine of vibration angle in y-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension angle of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_sin_yangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_sin_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad11
!DIR$ ENDIF        
        ! Leaves sine of vibration angle in y-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension sine of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: leaves_cos_angle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: leaves_cos_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad12
!DIR$ ENDIF        
        ! Branches angle of vibration in x-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension angle values (rad)
!DIR$ ATTRIBUTES ALIGN ; 64 :: branches_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad13
!DIR$ ENDIF        
        ! Branches sine of vibration angle in x-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension angle  of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_sin_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_sin_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad14
!DIR$ ENDIF        
        ! Branches cosine of vibration angle in x-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension cosine of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_cos_xangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_cos_xangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad15
!DIR$ ENDIF
        ! Branches angle of vibration in y-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension angle values (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_yangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad16
!DIR$ ENDIF        
        ! Branches sine of vibration angle in y-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension sine  of vibrational angle (rad)
!DIR$ ATTRIBUTES ALIGN : 64 :: branches_sin_yangle
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_sin_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad17
!DIR$ ENDIF        
        ! Branches cosine of vibration angle in x-axis per PRF/s
        ! 1st dimension PRF/s, 2nd dimension cosine  of vibrational angle (rad)
        type(YMM8r4_t), allocatable, dimension(:,:)  :: branches_cos_yangle
!DIR$ IF (GMS_MOD_TREE_SCATTERER_ADD_PADDING .EQ. 1)
        integer(kind=int1), dimension(0:55), private :: pad18
!DIR$ ENDIF        
     end type TreeScattererHTS_t

   contains

     subroutine InitTreeScatterer(tsLTS,tsHTS,nleaves,nbranches,nsteps,ordinal,trunk_param_npoints, &
                                  leaves_param_npoints,branches_param_npoints,tree_height,          &
                                  trunk_height,trunk_radius,crown_height,         &
                                  tree_lat,tree_lon,tree_elevation,  &
                                  errstate,iounit,logging,    &
                                  verbose,append,fname )
 !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: InitTreeScatterer
           use mod_constants,   only : INITVAL, INITVALR4
           use mod_print_error, only : print_non_fatal_error, &
                                       handle_fatal_memory_error
           type(TreeScattererLTS_t),    intent(inout) :: tsLTS
           type(TreeScattererHTS_t),    intent(inout) :: tsHTS
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
                                          " Module: mod_tree_scatterer, subroutine: InitTreeScatterer: Invalid 'nleaves' or 'nbranches' argument!! ",  &
                                          __LINE__,__FILE__ )
              return
           end if
           remainder = 0
           remainder = MOD(nleaves,8)
           if(remainder) then
              nleaves = nleaves-remainder
           end if
           remainder = MOD(nbranches,8)
           if(remainder) then
              nbranches = nbranches-remainder
           end if
           remainder = MOD(trunk_param_npoints,8)
           if(remainder) then
              trunk_param_npoints = trunk_param_npoints-remainder
           end if
           remainder = MOD(leaves_param_npoints,8)
           if(remainder) then
              leaves_param_npoints = leaves_param_npoints-remainder
           end if
           remainder = MOD(branches_param_npoints,8)
           if(remainder) then
              branches_param_npoints = branches_param_npoints-remainder
           end if
           tsLTS.nleaves     = nleaves
           tsLTS.nbranches   = nbranches
           tsLTS.nsteps      = nsteps
           tsLTS.ordinal     = ordinal
           tsLTS.trunk_param_npoints = trunk_param_npoints
           tsLTS.leaves_param_npoints = leaves_param_npoints
           tsLTS.branches_param_npoints = branches_param_npoints
           tsLTS.tree_height = tree_height
           tsLTS.trunk_height = trunk_height
           tsLTS.trunk_radius = trunk_radius
           tsLTS.crown_height = crown_height
           tsLTS.crown_area   = INITVAL
           tsLTS.trunk_area   = INITVAL
           tsLTS.tree_area    = INITVAL
           tsLTS.tree_lat     = tree_lat
           tsLTS.tree_lon     = tree_lon
           tsLTS.tree_elevation = tree_elevation
          
          
            
           allocate(tsLTS.leaves_moistness(tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999

           allocate(tsLTS.branches_moistness(tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
          
             
           allocate(tsLTS.trunk_xparam(tsLTS.trunk_param_npoints), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
      
           allocate(tsLTS.trunk_yparam(tsLTS.trunk_param_npoints), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
             
           allocate(tsLTS.trunk_zparam(tsLTS.trunk_param_npoints), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
         
           allocate(tsLTS.leaves_thickness(tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
            
           allocate(tsLTS.leaves_density(tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
           allocate(tsLTS.leaves_incident_angle(tsLTS.nleaves,2), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
        
           allocate(tsLTS.leaves_xparam(tsLTS.leaves_param_npoints+PADR8,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
           
           allocate(tsLTS.leaves_yparam(tsLTS.leaves_param_npoints+PADR8,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
           allocate(tsLTS.branches_thickness(tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999

           allocate(tsLTS.branches_density(tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
         
           allocate(tsLTS.branches_incident_angle(tsLTS.nbranches,2), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999  
        
           allocate(tsLTS.branches_xparam(tsLTS.branches_param_npoints+PADR8,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
             
           allocate(tsLTS.branches_yparam(tsLTS.branches_param_npoints+PADR8,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999  
          
           allocate(tsLTS.branches_zparam(tsLTS.branches_param_npoints+PADR8,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
         
           tsHTS.tree_xangle = INITVALR4
           tsHTS.tree_sin_xangle = INITVAL
           tsHTS.tree_cos_xangle = INITVAL
           tsHTS.tree_yangle = INITVALR4
           tsHTS.tree_sin_yangle = INITVAL
           tsHTS.tree_cos_yangle = INITVAL
           tsHTS.tree_cross_section = INITVAL
           tsHTS.crown_cross_section = INITVAL
           tsHTS.trunk_cross_section = INITVAL   
         
           allocate(tsHTS.leaves_cross_section(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
           allocate(tsHTS.leaves_reflectivity(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999  
          
           allocate(tsHTS.branches_cross_section(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
        
           allocate(tsHTS.branches_reflectivity(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999  
        
           allocate(tsHTS.leaves_xangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
            
           allocate(tsHTS.leaves_sin_xangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
         
           allocate(tsHTS.leaves_cos_xangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
            
           allocate(tsHTS.leaves_yangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999

           allocate(tsHTS.leaves_sin_yangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
             
           allocate(tsHTS.leaves_cos_yangle(tsLTS.nsteps,tsLTS.nleaves), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
         
           allocate(tsHTS.branches_xangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
        
           allocate(tsHTS.branches_sin_xangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
           allocate(tsHTS.branches_cos_xangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
        
           allocate(tsHTS.branches_yangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999   
         
           allocate(tsHTS.branches_sin_yangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999
          
           allocate(tsHTS.branches_cos_yangle(tsLTS.nsteps,tsLTS.nbranches), &
                   STAT=aerr,ERRMSG=emsg)
           if(aerr /= 0) goto 9999  
         
           errstate = .false.
           return
9999       call handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_tree_scatterer, subroutine: InitTreeScatterer -- Memory Allocation Failure !!", &                                                     
                              "module: mod_tree_scatterer, subroutine: InitTreeScatterer -- Memory Allocation Failure !!", &
                                                    emsg, 703) 
 
     end subroutine InitTreeScatterer

!====================================================================================================================!

     subroutine SetMoistness_scalar(tsLTS,nleaves,nbranches,iflag)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: SetMoistness
           use IFPORT
           type(TreeScaterrerLTS_t),     intent(inout) :: tsLTS
           integer(kind=int4),           intent(in)    :: nleaves
           integer(kind=int4),           intent(in)    :: nbranches
           integer(kind=int4),           intent(in), optional    :: iflag
           ! Locals
          
           integer(kind=int4), parameter :: cutoff1  = 8191
           integer(kind=int4), parameter :: cutoff2  = 1073741823
           integer(kind=int4), automatic :: i
           logical(kind=int4), automatic :: bval
           ! Exec code ....
           ! First memory touch
           tsLTS.leaves_moistness   = .false.
           tsLTS.branches_moistness = .false.
           bval = .false.
           if(present(iflag)) then
              do i=0, nleaves
                 bval = irand(iflag)
                 if(cutoff1 < bval) then
                    tsLTS.leaves_moistness(i) = .true.
                 else
                    tsLTS.leaves_moistness(i) = .false.
                 end if
              end do
              bval = .false.
              do i=0, nbranches
                 bval = irand(iflag)
                 if(cutoff1 < bval) then
                    tsLTS.branches_moistness(i) = .true.
                 else
                    tsLTS.branches_moistness(i) = .false.
                 end if
              end do
           else
               do i=0, nleaves
                 bval = irand()
                 if(cutoff2 > bval) then
                    tsLTS.leaves_moistness(i) = .true.
                 else
                    tsLTS.leaves_moistness(i) = .false.
                 end if
              end do
              bval = .false.
              do i=0, nbranches
                 bval = irand()
                 if(cutoff2 > bval) then
                    tsLTS.branches_moistness(i) = .true.
                 else
                    tsLTS.branches_moistness(i) = .false.
                 end if
              end do
           end if
           
            
     end subroutine SetMoistness_scalar

     subroutine ComputeTrunkParamEq_ymm8r4(tsLTS,zpoints)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeTrunkParametricEq
           use mod_vecconsts, only : ymm8r4_twopi
           type(TreeScattererLTS_t),    intent(inout) :: tsLTS
           integer(kind=int4),          intent(in)    :: zpoints
           ! Locals
           !DIR$ ATTRIBUTES ALIGN : 32 :: v2PI
           !type(YMM8r4_t),     parameter :: v2PI = YMM8r4_t(6.28_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vtheta0,vtheta1,vtheta2,vtheta3
           type(YMM8r4_t),     automatic :: vtheta0,vtheta1,vtheta2,vtheta3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vthinc0,vthinc1,vthinc2,vthinc3
           type(YMM8r4_t),     automatic :: vthinc0,vthinc1,vthinc2,vthinc3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vrad
           type(YMM8r4_t),     automatic :: vrad
           !DIR$ ATTRIBUTES ALIGN : 32 :: vhinc0,vhinc1,vhinc2,vhinc3
           type(YMM8r4_t),     automatic :: vhinc0,vhinc1,vhinc2,vhinc3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vhinit0,vhinit1,vhinit2,vhinit3
           type(YMM8r4_t),     automatic :: vhinit0,vhinit1,vhinit2,vhinit3
           !DIR$ ATTRIBUTES ALIGN : 32 :: tmp1,tmp2
           type(YMM8r4_t),     automatic :: tmp1,tmp2
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t),     automatic :: vNPTS
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
           vNPTS.v  = real(tsLTS.trunk_param_npoints,kind=sp)
           tmp1.v    =  ymm8r4_twopi.v/vNPTS.v
           vthinc0.v = tmp1.v
           vthinc0.v = vthinc0.v*VINC.v
           vthinc1.v = tmp1.v
           vthinc1.v = vthinc1.v*VINC2.v
           vthinc2.v = tmp1.v
           vthinc2.v = vthinc2.v*VINC3.v
           vthinc3.v = tmp1.v
           vthinc3.v = vthinc3.v*VINC4.v
           vrad.v    = tsLTS.trunk_radius
           zpoints   = zpoints + tsLTS.trunk_param_npoints ! skew the Z points in order to be not symmetric to x,y points
           tmp2.v    = tsLTS.trunk_height/real(zpoints,kind=sp)
           vhinc0.v  = tmp2.v
           vhinc0.v  = vhinc0.v*VINC.v
           vhinc1.v  = tmp2.v
           vhinc1.v  = vhinc1.v*VINC2.v
           vhinc2.v  = tmp2.v
           vhinc2.v  = vhinc2.v*VINC3.v
           vhinc3.v  = tmp2.v
           vhinc3.v  = vhinc3.v*VINC4.v
           !niter = tsLTS.trunk_param_npoints
           ! First memory touch
           tsLTS.trunk_xparam = ymm8r4_zero
           tsLTS.trunk_yparam = ymm8r4_zero
           tsLTS.trunk_zparam = ymm8r4_zero
          
           !DIR$ VECTOR  ALWAYS
           do i=1, tsLTS.trunk_param_npoints-3, 4
              vtheta0.v = vtheta0.v+vthinc0.v
              tsLTS.trunk_xparam(i+0).v = vrad.v*cos(vtheta0.v)
              tsLTS.trunk_yparam(i+0).v = vrad.v*sin(vtheta0.v)
              vtheta1.v = vtheta1.v+vthinc1.v
              tsLTS.trunk_xparam(i+1).v = vrad.v*cos(vtheta1.v)
              tsLTS.trunk_yparam(i+1).v = vrad.v*sin(vtheta1.v)
              vtheta2.v = vtheta2.v+vthinc2.v
              tsLTS.trunk_xparam(i+2).v = vrad.v*cos(vtheta2.v)
              tsLTS.trunk_yparam(i+2).v = vrad.v*sin(vtheta2.v)
              vtheta3.v = vtheta3.v+vthinc3.v
              tsLTS.trunk_xparam(i+3).v = vrad.v*cos(vtheta3.v)
              tsLTS.trunk_yparam(i+3).v = vrad.v*sin(vtheta3.v)
              vhinit0.v = vhinit0.v+vhinc0.v
              tsLTS.trunk_zparam(i+0).v = vhinit0.v
              vhinit1.v = vhinit1.v+vhinc1.v
              tsLTS.trunk_zparam(i+1).v = vhinit1.v
              vhinit2.v = vhinit2.v+vhinc2.v
              tsLTS.trunk_zparam(i+2).v = vhinit2.v
              vhinit3.v = vhinit3.v+vhinc3.v
              tsLTS.trunk_zparam(i+3).v = vhinit3.v
           end do
           
     end subroutine ComputeTrunkParamEq_ymm8r4

     subroutine SetThickDensAng_ymm8r4(tsLTS,bradii,nbranches)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: SetLeavesThicknessDensity
           use mod_vectypes,  only : Mask8_t
           use mod_vecconsts, only : ymm8r4_zero,ymm8r4_one
           use mod_fpcompare, only : ymm8r4_rgt_ymm8r4, ymm8r4_rlt_ymm8r4,ymm8r4_equalto_ymm8r4
           type(TreeScattererLTS_t),             intent(inout) :: tsLTS
           type(YMM8r4_t), dimension(nbranches), intent(in)    :: bradii
           ! Locals
           !DIR$ ATTRIBUTES ALIGN : 32 :: vScale
           type(YMM8r4_t), parameter :: vScale = YMM8r4_t(1000.0_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vLTlo
           type(YMM8r4_t), parameter :: vLTlo = YMM8r4_t(0.1_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vLThi
           type(YMM8r4_t), parameter :: vLThi = YMM8r4_t(0.7_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vLDlo
           type(YMM8r4_t), parameter :: vLDlo = YMM8r4_t(0.1_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vLDhi
           type(YMM8r4_t), parameter :: vLDhi = YMM8r4_t(0.6_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vPhlo
           type(YMM8r4_t), parameter :: vPhlo = YMM8r4_t(0.3_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vPhhi
           type(YMM8r4_t), parameter :: vPhhi = YMM8r4_t(0.7_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vThlo
           type(YMM8r4_t), parameter :: vThlo = YMM8r4_t(0.75_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: vThhi
           type(YMM8r4_t), parameter :: vThhi = YMM8r4_t(1.5_sp)
           !DIR$ ATTRIBUTES ALIGN : 32 :: t1
           type(YMM8r4_t),     automatic :: t1
           !DIR$ ATTRIBUTES ALIGN : 32 :: t2
           type(YMM8r4_t),     automatic :: t2
           !DIR$ ATTRIBUTES ALIGN : 32 :: vmt1gt
           type(Mask8_t),      automatic :: vmt1gt
           !DIR$ ATTRIBUTES ALIGN : 32 :: vmt1lt
           type(Mask8_t),      automatic :: vmt1lt
           !DIR4 ATTRIBUTES ALIGN : 32 :: vmt2gt
           type(Mask8_t),      automatic :: vmt2gt
           !DIR$ ATTRIBUTES ALIGN : 32 :: vmt2lt
           type(Mask8_t),      automatic :: vmt2lt
           integer(kind=int4), automatic :: i
           ! Exec code ....
           t1.v = 0.0_sp; t2.v = 0.0_sp
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           tsLTS.leaves_thickness      = ymm8r4_zero
           tsLTS.leaves_density        = ymm8r4_zero
          
           call RANDOM_SEED()
           !DIR$ VECTOR ALWAYS
           do i=1, tsLTS.nleaves
              call RANDOM_NUMBER(t1.v)
              vmt1gt = ymm8r4_rgt_ymm8r4(t1,vLThi)
              vmt1lt = ymm8r4_tlt_ymm8r4(t1,vLTlo)
              if(ALL(vmt1gt.m)) then
                 t1.v = vLThi.v
              else if(ALL(vmt1lt.m)) then
                 t1.v = vLTlo.v
              end if
              tsLTS.leaves_thickness(i).v = vScale.v*t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2gt = ymm8r4_rgt_ymm8r4(t2,vLDhi)
              vmt2lt = ymm8r4_rlt_ymm8r4(t2,vLDlo)
              if(ALL(vmt2gt.m)) then
                 t2.v = vLDhi.v
              else if(ALL(vmt2lt.m)) then
                 t2.v = vLDlo.v
              end if
              tsLTS.leaves_density(i).v = t2.v
           end do
          ! t1 = ymm8r4_zero
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           tsLTS.leaves_incident_angle = ymm8r4_zero
           call RANDOM_SEED()
           t1 = ymm8r4_zero
           t2 = ymm8r4_zero
           do i=1, tsLTS.nleaves
              call RANDOM_NUMBER(t1.v)
              vmt1lt = ymm8r4_rlt_ymm8r4(t1,vPhlo)
              vmt1gt = ymm8r4_rgt_ymm8r4(t1,vPhhi)
              if(ALL(vmt1lt.m)) then
                 t1.v = vPhlo.v
              else if(ALL(vmt1gt.m)) then
                 t1.v = vPhhi.v
              end if
              tsLTS.leaves_incident_angle(i,1).v = t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2lt = ymm8r4_rlt_ymm8r4(t2,vThlo)
              vmt2gt = ymm8r4_equalto_ymm8r4(t2,ymm8r4_one)
              if(ALL(vmt2lt.m)) then
                 t2.v = vThlo.v
              end if
              if(ALL(vmt2gt.m)) then
                 t2.v = ymm8r4_one
              end if
              tsLTS.leaves_incident_angle(i,2).v = t2.v
           end do
           ! Similar procedure for branches
           vmt1gt.m = .false.
           vmt1lt.m = .false.
           vmt2gt.m = .false.
           vmt2lt.m = .false.
           tsLTS.branches_incident_angle = ymm8r4_zero
           call RANDOM_SEED()
           t1 = ymm8r4_zero
           t2 = ymm8r4_zero
           do i=1, tsLTS.nbranches
              call RANDOM_NUMBER(t1.v)
              vmt1gt = ymm8r4_rgt_ymm8r4(t1,vLThi)
              vmt1lt = ymm8r4_tlt_ymm8r4(t1,vLTlo)
              if(ALL(vmt1gt.m)) then
                 t1.v = vLThi.v
              else if(ALL(vmt1lt.m)) then
                 t1.v = vLTlo.v
              end if
              tsLTS.branches_incident_angle(i,1).v = t1.v
              call RANDOM_NUMBER(t2.v)
              vmt2lt = ymm8r4_rlt_ymm8r4(t2,vThlo)
              vmt2gt = ymm8r4_equalto_ymm8r4(t2,ymm8r4_one)
              if(ALL(vmt2lt.m)) then
                 t2.v = vThlo.v
              end if
              if(ALL(vmt2gt.m)) then
                 t2.v = ymm8r4_one
              end if
              tsLTS.branches_incident_angle(i,2).v = t2.v
            end do
              
           ! Density set to 0.0 (must find the exact data)
           ! Setting only the radii
           ! First touch
           tsLTS.branches_density   = ymm8r4_zero
           tsLTS.branches_thickness = ymm8r4_zero
           !DIR$ VECTOR ALWAYS
           do i=1, tsLTS.nbranches
              tsLTS.branches_thickness(i).v = bradii(i).v
           end do
              
     end subroutine SetThickDensAng_ymm8r4

     subroutine ComputeLeavesParamEq_ymm8r4(tsLTS,va,vb)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeLeavesParamEq_ymm8r4
           
           use mod_vecconsts,    only : ymm8r4_zero,ymm8r4_pi,ymm8r4_two
           type(TreeScattererLTS_t),    intent(inout) :: tsLTS
           type(YMM8r4_t),              intent(in)    :: va
           type(YMM8r4_t),              intent(in)    :: vb
           ! Locals
           !
           !DIR$ ATTRIBUTES ALIGN : 32 :: va_rand
           type(YMM8r4_t), automatic   :: va_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vb_rand
           type(YMM8r4_t), automatic   :: vb_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vsqrt
           type(YMM8r4_t), automatic   :: vsqrt
           !DIR$ ATTRIBUTES ALIGN :32 ::  vsqrt_arg
           type(YMM8r4_t), automatic  ::  vsqrt_arg
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic ::   vC
           !DIR$ ATTRIBUTES ALIGN : 32 :: tmp
           type(YMM8r4_t), automatic ::   tmp
           !DIR$ ATTRIBUTES ALIGN : 32 :: vthinc0,vthinc1,vthinc2,vthinc3
           type(YMM8r4_t), automatic ::   vthinc0,vthinc1,vthinc2,vthinc3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic ::   vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vtheta0,vtheta1,vtheta2,vtheta3
           type(YMM8r4_t), automatic ::   vtheta0,vtheta1,vtheta2,vtheta3
           integer(kind=int4), automatic :: j,i
           !
           ! Exec code.....
           !
                     
          !
           va_rand    = ymm8r4_zero
           vb_rand    = ymm8r4_zero
           vsqrt      = ymm8r4_zero
           vsqrt_arg  = ymm8r4_zero
           vC         = ymm8r4_zero
           vthinc0    = ymm8r4_zero
           vthinc1    = ymm8r4_zero
           vthinc2    = ymm8r4_zero
           vthinc3    = ymm8r4_zero
           vNPTS      = ymm8r4_zero
           vtheta0    = ymm8r4_zero
           vtheta1    = ymm8r4_zero
           vtheta2    = ymm8r4_zero
           vtheta3    = ymm8r4_zero
           tmp        = ymm8r4_zero
           vNPTS.v    = real(tsLTS.leaves_param_npoints,kind=sp)
           ! First touch
           tsLTS.leaves_xparam = ymm8r4_zero
           tsLTS.leaves_zparam = ymm8r4_zero
           do j=1, tsLTS.nleaves
              call RANDOM_SEED()
              call RANDOM_NUMBER(va_rand.v)
              call RANDOM_NUMBER(vb_rand.v)
              ! Check for 0.0_sp will be too costly (horizontal operation)
              ! leaving it out
              va.v        = va.v+va_rand.v
              vb.v        = vb.v+vb_rand.v
              vsqrt_arg.v = ymm8r4_two.v*(va.v**2+vb.v**2)
              vsqrt.v     = sqrt(vsqrt_arg.v) 
              vC.v        = ymm8r4_pi.v * vsqrt.v
              tmp.v       = vC.v/vNPTS.v
              vthinc0.v   = tmp.v
              vthinc0.v   = vthinc0.c*VINC.v
              vthinc1.v   = tmp.v
              vthinc1.v   = vthinc1.v*VINC2.v
              vthinc2.v   = tmp.v
              vthinc2.v   = vthinc2.v*VINC3.v
              vthinc3.v   = tmp.v
              vthinc3.v   = vthinc3.v*VINC4.v
              vtheta0     = ymm8r4_zero
              vtheta1     = ymm8r4_zero
              vtheta2     = ymm8r4_zero
              vtheta3     = ymm8r4_zero
             
              !DIR$ VECTOR ALWAYS
              do i=1, tsLTS.leaves_param_npoints-3,4
                 vtheta0.v = vtheta0.v+vthinc0.v
                 tsLTS.leaves_xparam(i+0,j).v = va.v*cos(vtheta0.v)
                 tsLTS.leaves_yparam(i+0,j).v = vb.v*sin(vtheta0.v)
                 vtheta1.v = vtheta1.v+vthinc1.v
                 tsLTS.leaves_xparam(i+1,j).v = va.v*cos(vtheta1.v)
                 tsLTS.leaves_yparam(i+1,j).v = vb.v*sin(vtheta1.v)
                 vtheta2.v = vtheta2.v+vthinc2.v
                 tsLTS.leaves_xparam(i+2,j).v = va.v*cos(vtheta2.v)
                 tsLTS.leaves_yparam(i+2,j).v = vb.v*sin(vtheta2.v)
                 vtheta3.v = vtheta3.v+vthinc3.v
                 tsLTS.leaves_xparam(i+3,j).v = va.v*cos(vtheta3.v)
                 tsLTS.leaves_yparam(i+3,j).v = vb.v*sin(vtheta3.v)
              end do
           end do
     end subroutine ComputeLeavesParamEq_ymm8r4      
                 
     subroutine ComputeBranchesParamEq_ymm8r4(tsLTS,vrad,vz,nzpts)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeBranchesParamEq_ymm8r4
           use mod_vecconsts, only : ymm8r4_zero
           type(TreeScattererLTS_t),     intent(inout) :: tsLTS
           type(YMM8r4_t),               intent(in)    :: vrad
           type(YMM8r4_t),               intent(in)    :: vz
           integer(kind=int4),           intent(in)    :: nzpts
           ! Locals
           !DIR$ ATTRIBUTES ALIGN : 32 ::  vr_rand
           type(YMM8r4_t),  automatic :: vr_rand
           !DIR$ ATTRIBUTES ALIGN : 32 ::  vz_rand
           type(YMM8r4_t),  automatic :: vz_rand
           !DIR$ ATTRIBUTES ALIGN : 32 ::  vtheta0,vtheta1,vtheta2,vtheta3
           type(YMM8r4_t),  automatic :: vtheta0,vtheta1,vtheta2,vtheta3
           !DIR4 ATTRIBUTES ALIGN : 32 ::  vthinc0,vthinc1,vthinc2,vthinc3
           type(YMM8r4_t),  automatic :: vthinc0,vthinc1,vthinc2,vthinc3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vhinc0.vhinc1,vhinc2,vhinc3
           type(YMM8r4_t),  automatic :: vhinc0,vhinc1,vhinc2,vhinc3
           !DIR4 ATTRIBUTES ALIGN : 32 :: vhinit0,vhinit1,vhinit2,vhinit3
           type(YMM8r4_t),  automatic :: vhinit0,vhinit1,vhinit2,vhinit3
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t),  automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNZPTS
           type(YMM8r4_t),  automatic :: vNZPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: tmp1,tmp2
           type(YMM8r4_t),  automatic :: tmp1,tmp2
           integer(kind=int4), automatic :: j,i
           ! Exec code......
           ! First touch
           vr_rand  = ymm8r4_zero
           vz_rand  = ymm8r4_zero
           vtheta0  = ymm8r4_zero
           vtheta1  = ymm8r4_zero
           vtheta2  = ymm8r4_zero
           vtheta3  = ymm8r4_zero
           vthinc0  = ymm8r4_zero
           vthinc1  = ymm8r4_zero
           vthinc2  = ymm8r4_zero
           vthinc3  = ymm8r4_zero
           vhinc0   = ymm8r4_zero
           vhinc1   = ymm8r4_zero
           vhinc2   = ymm8r4_zero
           vhinc3   = ymm8r4_zero
           vhinit0  = ymm8r4_zero
           vhinit1  = ymm8r4_zero
           vhinit2  = ymm8r4_zero
           vhinit3  = ymm8r4_zero
           tmp1     = ymm8r4_zero
           tmp2     = ymm8r4_zero
           vNPTS   = ymm8r4_zero
           vNZPTS  = ymm8r4_zero
           vNPTS.v = real(tsLTS.branches_param_npoints,kind=sp)
           ! 
           vNZPTS.v  = real(tsLTS.branches_param_npoints+nzpts,kind=sp)
           tmp1.v    = ymm8r4_twopi.v/vNPTS.v
           vthinc0.v = tmp1.v
           vthinc0.v = vthinc0.v*VINC.v
           vthinc1.v = tmp1.v
           vthinc1.v = vthinc1.v*VINC2.v
           vthinc2.v = tmp1.v
           vthinc2.v = vthinc2.v*VINC3.v
           vthinc3.v = tmp1.v
           vthinc3.v = vthinc3.v*VINC4.v
           tsLTS.branches_xparam = ymm8r4_zero
           tsLTS.branches_yparam = ymm8r4_zero
           tsLTS.branches_zparam = ymm8r4_zero
           do j=1, tsLTS.nbranches
              call RANDOM_SEED()
              vr_rand  = ymm8r4_zero
              vz_rand  = ymm8r4_zero
              call RANDOM_NUMBER(vr_rand.v)
              vrad.v   = vrad.v+vr_rand.v
              call RANDOM_NUMBER(vz_rand.v)
              vz.v      = vz.v+vz_rand.v
              tmp2.v    = vz.v/vNZPTS.v
              vhinv0.v  = tmp2.v
              vhinv0.v  = vhinv0.v*VINC.v
              vhinv1.v  = tmp2.v
              vhinv1.v  = vhinv1.v*VINC2.v
              vhinv2.v  = tmp2.v
              vhinv2.v  = vhinv2.v*VINC3.v
              vhinv3.v  = vhinv3.v*VINC4.v
              vtheta0  = ymm8r4_zero
              vhinit0  = ymm8r4_zero
              vtheta1  = ymm8r4_zero
              vhinit1  = ymm8r4_zero
              vtheta2  = ymm8r4_zero
              vhinit2  = ymm8r4_zero
              vtheta3  = ymm8r4_zero
              vhinit3  = ymm8r4_zero
              
              !DIR$ VECTOR ALWAYS
              do i=1, tsLTS.branches_param_points-3, 4
                 vtheta0.v = vtheta0.v+vthinc0.v
                 tsLTS.branches_xparam(i+0,j).v = vrad.v*cos(vtheta0.v)
                 tsLTS.branches_yparam(i+0,j).v = vrad.v*sin(vtheta0.v)
                 vhinit0.v = vhinit0.v+vhinv0.v
                 tsLTS.branches_zparam(i+0,j).v = vhinit0.v
                 vtheta1.v = vtheta1.v+vthinc1.v
                 tsLTS.branches_xparam(i+1,j).v = vrad.v*cos(vtheta1.v)
                 tsLTS.branches_yparam(i+1,j).v = vrad.v*sin(vtheta1.v)
                 vhinit1.v = vhinit1.v+vhinv1.v
                 tsLTS.branches_zparam(i+1,j).v = vhinit1.v
                 vtheta2.v = vtheta2.v+vthinc2.v
                 tsLTS.branches_xparam(i+2,j).v = vrad.v*cos(vtheta2.v)
                 tsLTS.branches_yparam(i+2,j).v = vrad.v*sin(vtheta2.v)
                 vhinit2.v = vhinit2.v+vhinv2.v
                 tsLTS.branches_zparam(i+2,j).v = vhinit2.v
                 vtheta3.v = vtheta3.v+vthinc3.v
                 tsLTS.branches_xparam(i+3,j).v = vrad.v*cos(vtheta3.v)
                 tsLTS.branches_yparam(i+3,j).v = vrad.v*sin(vtheta3.v)
                 vhinit3.v = vhinit3.v+vhinv3.v
                 tsLTS.branches_zparam(i+3,j).v = vhinit3.v
              end do
           end do
     end subroutine ComputeBranchesParamEq_ymm8r4

         
         
             
     
         
           
         

          
            
        

           
         

         
            
         

        
            
         

          
             
        

         
            
          

           
            
         
        

        
           
         

          
           

            
          
          

          
            
        

         
             
          

          
            
         

        
            
        

         
             
         
        

         
     
   end module mod_tree_scatterer
