

#include "GMS_config.fpp"


module urban_model

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         urban_model
 !          
 !          Purpose:
 !                        Module data based description of urban built up area.
 !                        Various characteristics of different urban building being
 !                        a model for EM-wave diffraction and scattering computations.
 !                        
 !          History:
 !                        Date: 18-12-2023
 !                        Time: 06:28 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                      Own design.
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
    use mod_kinds,    only : i4,sp
    public
    implicit none

#if !defined(URBAN_MODEL_USE_FLAT_ARRAYS)
#define URBAN_MODEL_USE_FLAT_ARRAYS 0
#endif

     ! Major version
    integer(kind=i4),  parameter :: URBAN_MODEL_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: URBAN_MODEL_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: URBAN_MODEL_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: URBAN_MODEL_FULLVER =   &
            1000*URBAN_MODEL_MAJOR+100*URBAN_MODEL_MINOR+10*URBAN_MODEL_MICRO
    ! Module creation date
    character(*),        parameter :: URBAN_MODEL_CREATE_DATE = "12-12-2023 15:32 +00200 (TUE 12 DEC 2023 GMT+2)"
    ! Module build date
    character(*),        parameter :: URBAN_MODEL_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: URBAN_MODEL_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: URBAN_MODEL_SYNOPSIS    = "Data describing simplified geometry of urban built-up area."

    
    !************************************************************************************************!
    !************************************* Arrays size parameters ***********************************!
    ! Number of rows
    integer(i4) :: nrows
    ! Number of columns
    integer(i4) :: ncols
    ! Number of latitude   values (deg), per building
    integer(i4) :: nblatd
    ! Number of longtitude values (deg), per building
    integer(i4) :: nblond
    
    ! Number of latitude   values (rad), per building
    integer(i4) :: nblatr
    ! Number of longtitude values (rad), per building
    integer(i4) :: nblonr
    
    ! Number of ellipsoidal (radar waveform irradiating field) cells for building column
    integer(i4) :: nellb
    
    ! Parametric equation x (acos(t)) values (building)
    integer(i4) :: npxb
    
    ! Parametric equation y (b(sin(t)) values (building)
    integer(i4) :: npyb

    ! Number of building units per column
    integer(i4) :: nbpc
    ! Number of building units per row
    integer(i4) :: nbpr
    
    ! Number of streets
    integer(i4) :: nstr
    ! Length of every street
    integer(i4) :: nlstr
    ! Width of every street
    integer(i4) :: nwstr
    ! An area of every street
    integer(i4) :: narstr
    ! Moisture of every street  at each ellipsoidal cell
    integer(i4) :: nwtstr
    ! Percent of moist to dry area of evey street at each ellipsoidal cell
    integer(i4) :: nphds
    ! Coverage of every street (like: snow,mud, ...etc)
    integer(i4) :: ncstr
    ! Percent of covered to non-covered portion of every street
    integer(i4) :: npcns
    ! Average thickness of each layer (cover) of every street at each irradiated cell
    integer(i4) :: natcstr
    ! Thickness of cover along street (number of values) each irradiated cell
    integer(i4) :: ntcstr
    ! Mu for 'clean' street interpolated along the street length at each irradiated cell
    integer(i4) :: nmustr1
    ! Eps for 'clean' street street length interpolated at each irradiated cell
    integer(i4) :: nepsstr1
    ! Mu for covered (i.e. by mud,snow,clay, ..etc) street interpolated along the street length
    integer(i4) :: nmustr2
    ! Eps for covered (i.e. by mud,snow,clay, ..etc) street street length interpolated
    integer(i4) :: nepsstr2
    ! Street curvature parametric equation u-parameter
    integer(i4) :: nupstr
    ! Street curvature parametric equation v-parameter
    integer(i4) :: nvpstr
    ! Street surface normal vectors x-components along the street length at each irradiated cell
    integer(i4) :: nnxstr
    ! Street surface normal vectors y-components along the street length at each irradiated cell
    integer(i4) :: nnystr
    ! Street surface normal vectors z-components along the street length at each irradiated cell
    integer(i4) :: nnzstr
     ! Number of latitude   values (deg), per street length (at irradiance point)
    integer(i4) :: nslatd
    ! Number of longtitude values (deg), per street length (at irradiance point)
    integer(i4) :: nslond
    ! Number of latitude   values (rad), per street length (at irradiance point)
    integer(i4) :: nslatr
    ! Number of longtitude values (rad), per street length (at irradiance point)
    integer(i4) :: nslonr
    ! Urban area height map (at single building resolution), x-coordinate
    integer(i4) :: nxbh
    ! Urban area height map (at single building resolution), y-coordinate
    integer(i4) :: nybh
    ! ! Urban area height map (at single building resolution), x-coordinate, its first derivative
    integer(i4) :: ndxbh
    ! Urban area height map (at single building resolution), y-coordinate, its first derivative
    integer(i4) :: ndybh
    ! Height field gradient x-components
    integer(i4) :: ngrdxbh
    ! Height field gradient y-components
    integer(i4) :: ngrdybh
    ! Number of values of smoothing and approximating curve for linearly-piecewise height function (x-coordinate)
    integer(i4) :: nxsmbh
    ! Number of values of smoothing and approximating curve for linearly-piecewise height function (y-coordinate)
    integer(i4) :: nysmbh
    !! ********************* Building geometry ***************************** //
    
    ! Number of values of empty space in-between of buildings (per single column)
    integer(i4) :: nesbb
    ! Number of area values of in-between buildings empty spaces (per single column)
    integer(i4) :: naesbb
    ! Number of area values of each building (per single building column)
    integer(i4) :: nabc
    ! Number of south-facing walls (per each column)
    integer(i4) :: nswpc
    ! Number of east-facing walls (per each column)
    integer(i4) :: newpc
    ! Number of west-facing walls (per each column)
    integer(i4) :: nwwpc
    ! Number of north-facing walls (per each column)
    integer(i4) :: nnwpc
    ! Number of building [flat] roofs (per each column)
    integer(i4) :: nbrpc
    ! Number of area of every building [flat] roof (per each column)
    integer(i4) :: nbrapc
    ! Number of angled roof -- south facing roof wall (per each column)
    integer(i4) :: nsrwc
    ! Number of angled roof -- east facing roof wall (per each column)
    integer(i4) :: nerwc
    ! Number of angled roof -- west facing roof wall (per each column)
    integer(i4) :: nwrwc
    ! Number of angled roof -- north facing roof wall (per each column)
    integer(i4) :: nnrwc
    ! Number of angled roof inclination (deg) -- south facing roof wall (per each column)
    integer(i4) :: nidsrw
    ! Number of angled roof inclination (deg) -- east facing roof wall (per each column)
    integer(i4) :: niderw
    ! Number of angled roof inclination (deg) -- west facing roof wall (per each column)
    integer(i4) :: nidwrw
    ! Number of angled roof inclination (deg) -- north facing roof wall (per each column)
    integer(i4) :: nidnrw
    ! Number of angled roof inclination (rad) -- south facing roof wall (per each column)
    integer(i4) :: nirsrw
    ! Number of angled roof inclination (rad) -- east facing roof wall (per each column)
    integer(i4) :: nirerw
    ! Number of angled roof inclination (rad) -- west facing roof wall (per each column)
    integer(i4) :: nirwrw
    ! Number of angled roof inclination (rad) -- north facing roof wall (per each column)
    integer(i4) :: nirnrw
    ! Number of angled roof inclination surface area -- south facing roof wall (per each column)
    integer(i4) :: nisra
    ! Number of angled roof inclination surface area -- east facing roof wall (per each column)
    integer(i4) :: niera
    ! Number of angled roof inclination surface area -- west facing roof wall (per each column)
    integer(i4) :: niwra
     ! Number of angled roof inclination surface area -- north facing roof wall (per each column)
    integer(i4) :: ninra
    ! Number of south wall upper-facing edge inclination (rad) -- (per each column)
    integer(i4) :: nswue
     ! Number of east wall upper-facing edge inclination (rad) -- (per each column)
    integer(i4) :: newue
     ! Number of west wall upper-facing edge inclination (rad) -- (per each column)
    integer(i4) :: nwwue
     ! Number of north wall upper-facing edge inclination (rad) -- (per each column)
    integer(i4) :: nnwue
     ! Number of shared right edges between the south wall and east wall inclination (rad) -- (per each column)
    integer(i4) :: nsewe
    ! Number of shared left edges between the south wall and west wall inclination (rad) -- (per each column)
    integer(i4) :: nswwe
    ! Number of shared right edges between the north wall and east wall inclination (rad) -- (per each column)
    integer(i4) :: nnewe
    ! Number of shared left edges between the north wall and west wall inclination (rad) -- (per each column)
    integer(i4) :: nnwwe
    ! Number of south walls surface area (for every building, per column) 
    integer(i4) :: nswsa
    ! Number of east walls surface area (for every building, per column) 
    integer(i4) :: newsa
    ! Number of west walls surface area (for every building, per column) 
    integer(i4) :: nwwsa
    ! Number of north walls surface area (for every building, per column) 
    integer(i4) :: nnwsa
    ! Number of south walls being either moist or non moist  (per column) x number of columns
    integer(i4) :: nmnmsw
    ! Number of east walls being either moist or non moist  (per column) x number of columns
    integer(i4) :: nmnmew
    ! Number of west walls being either moist or non moist  (per column) x number of columns
    integer(i4) :: nmnmww
    ! Number of north walls being either moist or non moist  (per column) x number of columns
    integer(i4) :: nmnmnw
    ! Number of values describing the ratio (percentage) of south wall moisture to dryness (per each column) 
    integer(i4) :: nmdswr
    ! Number of values describing the ratio (percentage) of east wall moisture to dryness (per each column) 
    integer(i4) :: nmdewr
    ! Number of values describing the ratio (percentage) of west wall moisture to dryness (per each column) 
    integer(i4) :: nmdwwr
     ! Number of values describing the ratio (percentage) of north wall moisture to dryness (per each column) 
    integer(i4) :: nmdnwr
    ! Number of logical values of flat roof moistness (being either moist or dry) (per column)
    integer(i4) :: nmdr
    ! Number of values describing the ratio (percentage) of flat roof moisture to dryness (per each column) 
    integer(i4) :: nmdrr
    ! Number of values describing the surface of moist part of the flat roof (per each column) 
    integer(i4) :: nmpfr
    ! Number of values describing the surface of dry part of the flat roof (per each column) 
    integer(i4) :: ndpfr
    ! Number of values describing the surface of moist part of the south wall (per each column) 
    integer(i4) :: nmpsw
    ! Number of values describing the surface of dry part of the south wall (per each column) 
    integer(i4) :: ndpsw
     ! Number of values describing the surface of moist part of the east wall (per each column) 
    integer(i4) :: nmpew
    ! Number of values describing the surface of dry part of the east wall (per each column) 
    integer(i4) :: ndpew
     ! Number of values describing the surface of moist part of the west wall (per each column) 
    integer(i4) :: nmpww
    ! Number of values describing the surface of dry part of the west wall (per each column) 
    integer(i4) :: ndpww
     ! Number of values describing the surface of moist part of the north wall (per each column) 
    integer(i4) :: nmpnw
    ! Number of values describing the surface of dry part of the north wall (per each column) 
    integer(i4) :: ndpnw
    ! Number of RCS values for south wall (per each column) 
    integer(i4) :: nrcssw
    ! Number of RCS values for east wall (per each column) 
    integer(i4) :: nrcsew
    ! Number of RCS values for west wall (per each column) 
    integer(i4) :: nrcsww
    ! Number of RCS values for north wall (per each column) 
    integer(i4) :: nrcsnw
    ! Number of RCS values for flat roof (per each column) 
    integer(i4) :: nrcsfr
    ! Number of RCS values for southern angled roof (per each column) 
    integer(i4) :: nrcssar
    ! Number of RCS values for eastern angled roof (per each column) 
    integer(i4) :: nrcsear
    ! Number of RCS values for western angled roof (per each column) 
    integer(i4) :: nrcswar
    ! Number of RCS values for northern angled roof (per each column) 
    integer(i4) :: nrcsnar
    ! Number of values describing the surface of moist part of the angled south roof wall
                               !// ! (per each column)
    integer(i4) :: nmpsar
    ! Number of values describing the surface of dry part of the angled south roof wall
                               !// ! (per each column)
    integer(i4) :: ndpsar
    ! Number of values describing the surface of moist part of the angled east roof wall
                               !// ! (per each column) 
    integer(i4) :: nmpear
    ! Number of values describing the surface of dry part of the angled east roof wall
                               !// ! (per each column) 
    integer(i4) :: ndpear
     ! Number of values describing the surface of moist part of the angled west roof wall
                               !// ! (per each column) 
    integer(i4) :: nmpwar
    ! Number of values describing the surface of dry part of the angled west roof wall
                               !// ! (per each column) 
    integer(i4) :: ndpwar
     ! Number of values describing the surface of moist part of the angled north roof wall
                               !// ! (per each column) 
    integer(i4) :: nmpnar
    ! Number of values describing the surface of dry part of the angled north roof wall
                               !// ! (per each column) 
    integer(i4) :: ndpnar
    ! Number of values describing the complex permittivity of south walls
                               !// ! (per each column) 
    integer(i4) :: ncesw
    ! Number of values describing the complex permeabillity of south walls
                              ! // ! (per each column) x number of columns  
    integer(i4) :: ncmsw
     ! Number of values describing the complex permittivity of west walls
                               !// ! (per each column) 
    integer(i4) :: nceww
    ! Number of values describing the complex permeabillity of west walls
                              ! // ! (per each column) x number of columns  
    integer(i4) :: ncmww
     ! Number of values describing the complex permittivity of east walls
                               !// ! (per each column) 
    integer(i4) :: nceew
    ! Number of values describing the complex permeabillity of east walls
                              ! // ! (per each column) x number of columns  
    integer(i4) :: ncmew
    ! Number of values describing the complex permittivity of north walls
                               !// ! (per each column) 
    integer(i4) :: ncenw
    ! Number of values describing the complex permeabillity of north walls
                              ! // ! (per each column) x number of columns  
    integer(i4) :: ncmnw
    ! Number of wire antennas per every building (per each column)
    integer(i4) :: nwant
    ! Number of parabolic antennas per every building (per each column)
    integer(i4) :: npant
    ! Number of yagi type antennas per every building (per each column)
    integer(i4) :: nyant
    ! Number of log-periodic dipole array antennas per every building (per each column)
    integer(i4) :: nlpda
    ! Number of cell phone sector bars antennas per every building (per each column)
    integer(i4) :: ncant
    ! Number of cylindrical objects (e.g. ventillation) per every building (per each column)
    integer(i4) :: ncylo
    
!! ****************************************  Array data types ***************************************** //
 

    ! Simple cell-based mesh to be used
    ! as an array of derived types per every surface.
    type, public :: CellMesh_t
          
          !  // Number of divisions along the x,y,z
          integer(i4), dimension(3) :: ndiv
          ! Number of cells
          integer(i4) :: nL
          ! Numerical integration to be computed?
          logical(i4) :: nint
          ! Coordinates (x,y,z) of the center
          !        // of Lth cell.
          real(sp), dimension(:), allocatable :: cx
          !dir$ attributes align : 64 :: cx
          real(sp), dimension(:), allocatable :: cy
          !dir$ attributes align : 64 :: cy
          real(sp), dimension(:), allocatable :: cz
          !dir$ attributes align : 64 :: cz
          ! The volume of each cell
          real(sp), dimension(:), allocatable :: dv
          !dir$ attributes align : 64 :: dv
          ! // (X,Y,Z) dimensions of the Ith
          !        // rectangular volume cell (this is needed for
          !        // the numerical integration)
          real(sp), dimension(:), allocatable :: dx
          !dir$ attributes align : 64 :: dx
          real(sp), dimension(:), allocatable :: dy
          !dir$ attributes align : 64 :: dy
          real(sp), dimension(:), allocatable :: dz
          !dir$ attributes align : 64 :: dz
    end type CellMesh_t
    
    
    ! latitude   values (deg), per building
    real(sp), dimension(:), allocatable :: blatd
    !dir$ attributes align : 64 :: blatd
   
    ! longtitude values (deg), per building
    real(sp), dimension(:), allocatable :: blond
    !dir$ attributes align : 64 :: blond
    
    ! latitude   values (rad), per building
     real(sp), dimension(:), allocatable :: blatr
    !dir$ attributes align : 64 :: blatr
    
    ! longtitude values (rad), per building
    real(sp), dimension(:), allocatable :: blonr
    !dir$ attributes align : 64 :: blonr
    
#if (URBAN_MODEL_USE_FLAT_ARRAYS) == 1
    ! ellipsoidal (radar waveform irradiating field) cells for building column
    integer(i4), dimension(:), allocatable :: ellpb
    !dir$ attributes align : 64 :: ellpb
    
    ! Parametric equation x (acos(t)) values (building) of (irradiating waveform)
    ! 1st dimension building column, 2nd dimension 'x' parameter values
    real(sp), dimension(:),    allocatable :: pxb
    !dir$ attributes align : 64 :: pxb
    
    ! Parametric equation y (b(sin(t)) values (building) of (irradiating waveform)
    ! 1st dimension building column, 2nd dimension 'y' parameter values
    real(sp), dimension(:),    allocatable :: pyb
    !dir$ attributes align : 64 :: pyb
#else
    
    ! ellipsoidal (radar waveform irradiating field) cells for building column
    integer(i4), dimension(:,:), allocatable :: ellpb
    !dir$ attributes align : 64 :: ellpb
    
    ! Parametric equation x (acos(t)) values (building)
    ! 1st dimension building column, 2nd dimension 'x' parameter values
    real(sp), dimension(:,:),    allocatable :: pxb
    !dir$ attributes align : 64 :: pxb
    
    ! Parametric equation y (b(sin(t)) values (building)
    ! 1st dimension building column, 2nd dimension 'y' parameter values
    real(sp), dimension(:,:),    allocatable :: pyb
    !dir$ attributes align : 64 :: pyb
#endif
   
    ! building units per column
    integer(i4), dimension(:), allocatable :: bpc
    !dir$ attributes align : 64 :: bpc
    
    ! building units per raw
    integer(i4), dimension(:), allocatable :: bpr
    !dir$ attributes align : 64 :: bpr
    
        
    ! Length of every street
    real(sp), dimension(:), allocatable :: lstr
    !dir$ attributes align : 64 :: lstr
    
    ! Width of every street
    real(sp), dimension(:), allocatable :: wstr
    !dir$ attributes align : 64 :: wstr
    
    ! An area of every street
    real(sp), dimension(:), allocatable :: arstr
    !dir$ attributes align :: 64 :: arstr
#if (URBAN_MODEL_USE_FLAT_ARRAYS) == 1
    ! Moisture of every street (2D array)
    ! 1st dimension humidity values (per street), 2nd dimension street numbers
    real(sp), dimension(:), allocatable :: wtstr
    !dir$ attributes align : 64 :: wtstr
    
    ! Percent of moist to dry area of evey street at each cell
    real(sp), dimension(:), allocatable :: phds
    !dir$ attributes align : 64 :: phds
    
    !Coverage of every street (like: snow,mud, ...etc)
    integer(i4), dimension(:), allocatable :: cstr
    !dir$ attributes align : 64 :: cstr
    
    ! Percent of covered to non-covered portion of every street (at irradiated cell)
    real(sp),  dimension(:),  allocatable :: pcns
    !dir$ attributes align : 64 :: pcns
    
    ! Average thickness of each layer (cover) of every street at each irradiated cell
    real(sp),  dimension(:),  allocatable :: atcstr
    !dir$ attributes align : 64 :: atcstr
    
    ! Thickness of cover along street (number of values) at each irradiated cell
    real(sp),  dimension(:),  allocatable :: tcstr
    !dir$ attributes align : 64 :: tcstr
    
    ! Mu values for 'clean' street interpolated along the street length at each irradiated cell
    complex(sp), dimension(:), allocatable :: mustr1
    !dir$ attributes align : 64 :: mustr1
    
    ! Eps for 'clean' street street length interpolated at each irradiated cell
    complex(sp), dimension(:), allocatable :: epsstr1
    !dir$ attributes align : 64 :: epsstr1
    
    ! Mu for covered (i.e. by mud,snow,clay, ..etc) street interpolated along the street length at each irradiated cell
    complex(sp), dimension(:), allocatable :: mustr2
    !dir$ attributes align : 64 :: mustr2
    
    ! Eps for covered (i.e. by mud,snow,clay, ..etc) street  length interpolated at each irradiated cell
    complex(sp), dimension(:), allocatable :: epsstr2
    !dir$ attributes align : 64 :: epsstr2
    
    ! ! Street curvature parametric equation u-parameter
    real(sp),  dimension(:), allocatable :: upstr
    !dir$ attributes align : 64 :: upstr
    
    ! ! Street curvature parametric equation v-parameter
    real(sp), dimension(:), allocatable :: vpstr
    !dir$ attributes align : 64 :: vpstr
    
    ! Street surface normal vectors x-components along the street length at each irradiated cell
    real(sp), dimension(:), allocatable :: nxstr
    !dir$ attributes align : 64 :: nxstr
    
    ! Street surface normal vectors y-components along the street length at each irradiated cell
    real(sp), dimension(:), allocatable :: nystr
    !dir$ attributes align : 64 :: nystr
    
    ! Street surface normal vectors z-components along the street length at each irradiated cell
    real(sp), dimension(:), allocatable :: nzstr
    !dir$ attributes align : 64 :: nzstr
    
    ! latitude   values (deg), per street length (at irradiance point)
    real(sp), dimension(:), allocatable :: slatd
    !dir$ attributes align : 64 :: slatd
    
    ! longtitude values (deg), per street length (at irradiance point)
    real(sp), dimension(:), allocatable :: slond
    !dir$ attributes align : 64 :: slond
    
    ! Smoothing and approximating curve for linearly-piecewise height function (x-coordinate)
    ! Smoothing and approximating curve for linearly-piecewise height function (y-coordinate)
    real(sp), dimension(:), allocatable :: xysmbh
    !dir$ attributes align : 64 :: xysmbh
    
    ! Empty space in-between of buildings (per single column) x number columns
    integer(i4), dimension(:), allocatable :: esbb
    !dir$ attributes align : 64 :: esbb
    
    ! An area values of in-between buildings empty spaces (per single column) x number of columns
    real(sp), dimension(:), allocatable :: aesbb
    !dir$ attributes align : 64 :: aesbb
    
    ! An area values of each building (per single building column) x number of columns
    real(sp), dimension(:), allocatable :: abc
    !dir$ attributes align : 64 :: abc
    
    ! Number of south-facing walls (per each column) x number of columns
    integer(i4), dimension(:), allocatable :: swpc
    !dir$ attributes align : 64 :: swpc
    
    ! Number of east-facing walls (per each column) x number of columns
    integer(i4), dimension(:), allocatable :: ewpc
    !dir$ attributes align : 64 :: ewpc
    
    ! Number of west-facing walls (per each column) x number of columns
    integer(i4), dimension(:), allocatable :: wwpc
    !dir$ attributes align : 64 :: wwpc
    
     ! Number of north-facing walls (per each column) x number of columns
    integer(i4), dimension(:), allocatable :: nwpc
    !dir$ attributes align : 64 :: nwpc
    
    ! Number of building roofs (per each column) x number of columns
    integer(i4), dimension(:), allocatable :: brpc
    !dir$ attributes align : 64 :: brpc
    
    ! An area of every building [flat] roof (per each column) x number of columns
    real(sp), dimension(:), allocatable :: brapc
    !dir$ attributes align : 64 :: brapc
    
    ! Number of angled roof -- south facing roof wall (per each column)  x number of columns
    integer(i4), dimension(:), allocatable :: srwc
    !dir$ attributes align : 64 :: srwc
    
    ! Number of angled roof -- east facing roof wall (per each column)  x number of columns
    integer(i4), dimension(:), allocatable :: erwc
    !dir$ attributes align : 64 :: erwc
    
    ! Number of angled roof -- west facing roof wall (per each column)  x number of columns
    integer(i4), dimension(:), allocatable :: wrwc
    !dir$ attributes align : 64 :: wrwc
    
    ! Number angled roof -- north facing roof wall (per each column)
    integer(i4), dimension(:), allocatable :: nrwc
    !dir$ attributes align : 64 :: nrwc
    
    ! An angled roof inclination (deg) -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: idsrw
    !dir$ attributes align : 64 :: idsrw
    
    ! An angled roof inclination (deg) -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: iderw
    !dir$ attributes align : 64 :: iderw
    
     ! An angled roof inclination (deg) -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: idwrw
    !dir$ attributes align : 64 :: idwrw
    
      ! An angled roof inclination (deg) -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: idnrw
    !dir$ attributes align : 64 :: idnrw
    
     ! An angled roof inclination (rad) -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: irsrw
    !dir$ attributes align : 64 :: irsrw
    
    ! An angled roof inclination (rad) -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: irerw
    !dir$ attributes align : 64 :: irerw
    
     ! An angled roof inclination (rad) -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: irwrw
    !dir$ attributes align : 64 :: irwrw
    
     ! An angled roof inclination (rad) -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:),  allocatable :: irnrw
    !dir$ attributes align : 64 :: irnrw
    
    ! An angled roof inclination surface area -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: isra
    !dir$ attributes align : 64 :: isra
    
     ! An angled roof inclination surface area -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: iwra
    !dir$ attributes align : 64 :: iwra
    
     ! An angled roof inclination surface area -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: iera
    !dir$ attributes align : 64 :: iera
    
    ! An angled roof inclination surface area -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: inra
    !dir$ attributes align : 64 :: inra
    
    ! South wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:), allocatable :: swue
    !dir$ attributes align : 64 :: swue
    
     ! East wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:), allocatable :: ewue
    !dir$ attributes align : 64 :: swue
    
     ! West wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:), allocatable :: wwue
    !dir$ attributes align : 64 :: wwue
    
      ! North wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:), allocatable :: nwue
    !dir$ attributes align : 64 :: nwue
    
    ! Shared right edges between the south wall and east wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:), allocatable :: sewe
    !dir$ attributes align : 64 :: sewe 
    
    ! Shared left edges between the south wall and west wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:), allocatable :: swwe
     !dir$ attributes align : 64 :: swwe
    
    ! Shared right edges between the north wall and east wall inclination (rad) -- 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:), allocatable :: nwee
    !dir$ attributes align : 64 :: nwee
    
    ! Shared left edges between the north wall and west wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:), allocatable :: nwwe
    !dir$ attributes align : 64 :: nwwe
    
    ! South walls surface area (for every building, per column) x number of columns
    real(sp), dimension(:), allocatable :: swsa
    !dir$ attributes align : 64 :: swsa
    
    ! East walls surface area (for every building, per column) 
    real(sp), dimension(:), allocatable :: ewsa
    !dir$ attributes align : 64 :: ewsa
    
     ! West walls surface area (for every building, per column) 
    real(sp), dimension(:), allocatable :: wwsa
    !dir$ attributes align : 64 :: wwsa
    
     ! North walls surface area (for every building, per column) 
    real(sp), dimension(:), allocatable :: nwsa
    !dir$ attributes align : 64 :: nwsa
    
    ! South walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:), allocatable :: mnmsw
    !dir$ attributes align : 64 :: mnmsw
    
     ! East walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:), allocatable :: mnmew
    !dir$ attributes align : 64 :: mnmew
    
     ! West walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:), allocatable :: mnmww
    !dir$ attributes align : 64 :: mnmww
    
     ! North walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:), allocatable :: mnmnw
    !dir$ attributes align : 64 :: mnmnw
    
    ! The values describing the ratio (percentage) of south wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mdswr
    !dir$ attributes align : 64 :: mdswr
    
     ! The values describing the ratio (percentage) of east wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mdewr
    !dir$ attributes align : 64 :: mdewr
    
    ! The values describing the ratio (percentage) of west wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mdwwr
    !dir$ attributes align : 64 :: mdwwr
    
    ! The values describing the ratio (percentage) of north wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mdnwr
    !dir$ attributes align : 64 :: mdnwr
    
    ! The logical values of flat roof moistness (being either moist or dry) 
                               ! (per column) x number of columns
    logical(i4), dimension(:), allocatable :: mdr
    !dir$ attributes align : 64 :: mdr
    
    ! The values describing the ratio (percentage) of flat roof moisture to dryness 
                               ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mdrr
    !dir$ attributes align : 64 :: mdrr
    
    ! The values describing the surface of moist part of the flat roof 
                                ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpfr
    !dir$ attributes align : 64 :: mpfr
    
     ! The values describing the surface of dry part of the flat roof 
                                ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpfr
    !dir$ attributes align : 64 :: dpfr
    
    ! The values describing the surface of moist part of the south wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpsw
    !dir$ attributes align : 64 :: mpsw
    
    ! The values describing the surface of dry part of the south wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpsw
    !dir$ attributes align : 64 :: dpsw
    
    ! The values describing the surface of moist part of the east wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpew
    !dir$ attributes align : 64 :: mpew
    
    ! The values describing the surface of dry part of the east wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpew
    !dir$ attributes align : 64 :: dpew
    
     ! The values describing the surface of moist part of the west wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpww
    !dir$ attributes align : 64 :: mpww
    
    ! The values describing the surface of dry part of the west wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpww
    !dir$ attributes align : 64 :: dpww
    
    ! The values describing the surface of moist part of the north wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpnw
    !dir$ attributes align : 64 :: mpnw
    
    ! The values describing the surface of dry part of the north wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpnw
    !dir$ attributes align : 64 :: dpnw
    
    ! RCS values for south wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcssw
    !dir$ attributes align : 64 :: rcssw
    
    ! RCS values for east wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsew
    !dir$ attributes align : 64 :: rcsew
    
     ! RCS values for west wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsww
    !dir$ attributes align : 64 :: rcsww
    
     ! RCS values for north wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsnw
    !dir$ attributes align : 64 :: rcsnw
    
   ! RCS values for flat roof (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsfr
     !dir$ attributes align : 64 :: rcsfr
     
   ! Number of RCS values for southern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcssar
     !dir$ attributes align : 64 :: rcssar
    
    ! Number of RCS values for eastern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsear
     !dir$ attributes align : 64 :: rcsear
     
     ! Number of RCS values for western angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcswar
     !dir$ attributes align : 64 :: rcswar
     
     ! Number of RCS values for northern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcsnar
     !dir$ attributes align : 64 :: rcsnar
     
    ! The values describing the surface of moist part of the angled south roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpsar
    !dir$ attributes align : 64 :: mpsar
    
     ! The values describing the surface of dry part of the angled south roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpsar
    !dir$ attributes align : 64 :: dpsar
    
     ! The values describing the surface of moist part of the angled east roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpear
    !dir$ attributes align : 64 :: mpear
    
     ! The values describing the surface of dry part of the angled east roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpear
    !dir$ attributes align : 64 :: dpear
    
     ! The values describing the surface of moist part of the angled west roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpwar
    !dir$ attributes align : 64 :: mpwar
    
     ! The values describing the surface of dry part of the angled west roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpwar
    !dir$ attributes align : 64 :: dpwar
    
     ! The values describing the surface of moist part of the angled north roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: mpnar
    !dir$ attributes align : 64 :: mpnar
    
     ! The values describing the surface of dry part of the angled north roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:), allocatable :: dpnar
    !dir$ attributes align : 64 :: dpnar
    
   ! The values describing the complex permittivity of south walls
                               !// ! (per each column) 
    complex(sp), dimension(:), allocatable :: cesw
    !dir$ attributes align : 64 :: cesw
    
   ! The values describing the complex permeabillity of south walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:), allocatable :: cmsw
    !dir$ attributes align : 64 :: cmsw
    
     ! The values describing the complex permittivity of west walls
                               !// ! (per each column) 
    complex(sp), dimension(:), allocatable :: ceww
    !dir$ attributes align : 64 :: ceww
    
   ! The values describing the complex permeabillity of west walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:), allocatable :: cmww
    !dir$ attributes align : 64 :: cmww
    
     ! The values describing the complex permittivity of east walls
                               !// ! (per each column) 
    complex(sp), dimension(:), allocatable :: ceew
    !dir$ attributes align : 64 :: ceew
    
   ! The values describing the complex permeabillity of east walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:), allocatable :: cmew
    !dir$ attributes align : 64 :: cmew
    
     ! The values describing the complex permittivity of north walls
                               !// ! (per each column) 
    complex(sp), dimension(:), allocatable :: cenw
    !dir$ attributes align : 64 :: cenw
    
   ! The values describing the complex permeabillity of north walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:), allocatable :: cmnw
    !dir$ attributes align : 64 :: cmnw
    
    ! Number of wire antennas per every building (per each column)
    integer(i4), dimension(:), allocatable :: want
    !dir$ attributes align : 64 :: want
    
     ! Number of parabollic antennas per every building (per each column)
    integer(i4), dimension(:), allocatable :: pant
    !dir$ attributes align : 64 :: pant
    
      ! Number of parabollic antennas per every building (per each column)
    integer(i4), dimension(:), allocatable :: yant
    !dir$ attributes align : 64 :: yant
    
    ! Number of log-periodic dipole array antennas per every building (per each column)
    integer(i4), dimension(:), allocatable :: lpda
     !dir$ attributes align : 64 :: lpda
     
    !  Number of cell phone sector bars antennas per every building (per each column)
    integer(i4), dimension(:), allocatable :: cant
     !dir$ attributes align : 64 :: cant
     
    ! Number of cylindrical objects (e.g. ventillation) per every building (per each column)
    integer(i4), dimension(:), allocatable :: cylo
     !dir$ attributes align : 64 :: cylo
     
#else    
    ! Moisture of every street (2D array)
    ! 1st dimension humidity values (per street), 2nd dimension street numbers
    real(sp), dimension(:,:), allocatable :: wtstr
    !dir$ attributes align : 64 :: wtstr
    
    ! Percent of moist to dry area of evey street at each cell
    real(sp), dimension(:,:), allocatable :: phds
    !dir$ attributes align : 64 :: phds
    
    !Coverage of every street (like: snow,mud, ...etc)
    integer(i4), dimension(:,:), allocatable :: cstr
    !dir$ attributes align : 64 :: cstr
    
    ! Percent of covered to non-covered portion of every street (at irradiated cell)
    real(sp),  dimension(:,:),  allocatable :: pcns
    !dir$ attributes align : 64 :: pcns
    
    ! Average thickness of each layer (cover) of every street at each irradiated cell
    real(sp),  dimension(:,:),  allocatable :: atcstr
    !dir$ attributes align : 64 :: atcstr
    
    ! Thickness of cover along street (number of values) at each irradiated cell
    real(sp),  dimension(:,:),  allocatable :: tcstr
    !dir$ attributes align : 64 :: tcstr
    
    ! Mu values for 'clean' street interpolated along the street length at each irradiated cell
    complex(sp), dimension(:,:), allocatable :: mustr1
    !dir$ attributes align : 64 :: mustr1
    
    ! Eps for 'clean' street street length interpolated at each irradiated cell
    complex(sp), dimension(:,:), allocatable :: epsstr1
    !dir$ attributes align : 64 :: epsstr1
    
    ! Mu for covered (i.e. by mud,snow,clay, ..etc) street interpolated along the street length at each irradiated cell
    complex(sp), dimension(:,:), allocatable :: mustr2
    !dir$ attributes align : 64 :: mustr2
    
    ! Eps for covered (i.e. by mud,snow,clay, ..etc) street  length interpolated at each irradiated cell
    complex(sp), dimension(:,:), allocatable :: epsstr2
    !dir$ attributes align : 64 :: epsstr2
    
    ! ! Street curvature parametric equation u-parameter
    real(sp),  dimension(:,:), allocatable :: upstr
    !dir$ attributes align : 64 :: upstr
    
    ! ! Street curvature parametric equation v-parameter
    real(sp), dimension(:,:), allocatable :: vpstr
    !dir$ attributes align : 64 :: vpstr
    
    ! Street surface normal vectors x-components along the street length at each irradiated cell
    real(sp), dimension(:,:), allocatable :: nxstr
    !dir$ attributes align : 64 :: nxstr
    
    ! Street surface normal vectors y-components along the street length at each irradiated cell
    real(sp), dimension(:,:), allocatable :: nystr
    !dir$ attributes align : 64 :: nystr
    
    ! Street surface normal vectors z-components along the street length at each irradiated cell
    real(sp), dimension(:,:), allocatable :: nzstr
    !dir$ attributes align : 64 :: nzstr
    
    ! latitude   values (deg), per street length (at irradiance point)
    real(sp), dimension(:,:), allocatable :: slatd
    !dir$ attributes align : 64 :: slatd
    
    ! longtitude values (deg), per street length (at irradiance point)
    real(sp), dimension(:,:), allocatable :: slond
    !dir$ attributes align : 64 :: slond
    
    ! latitude   values (rad), per street length (at irradiance point)
    real(sp), dimension(:,:), allocatable :: slatr
    !dir$ attributes align : 64 :: slatr
    
    ! longtitude values (rad), per street length (at irradiance point)
    real(sp), dimension(:,:), allocatable :: slonr
    !dir$ attributes align : 64 :: slonr
    
    !Urban area height map (at single building resolution)
    real(sp), dimension(:,:), allocatable :: bhmap
    !dir$ attributes align : 64 :: bhmap
    
    !Urban area height map (at single building resolution) -- 1st derivative
    real(sp), dimension(:,:), allocatable :: bhdxdy
    !dir$ attributes align : 64 :: bhdxdy
    
    ! Urban area height map (at single building resolution) -- gradient x-component
    real(sp), dimension(:,:), allocatable :: bhgradx
    !dir$ attributes align : 64 :: bhgradx
    
    ! Urban area height map (at single building resolution) -- gradient y-component
    real(sp), dimension(:,:), allocatable :: bhgrady
    !dir$ attributes align : 64 :: bhgrady
    
     ! Smoothing and approximating curve for linearly-piecewise height function (x-coordinate)
    ! Smoothing and approximating curve for linearly-piecewise height function (y-coordinate)
    real(sp), dimension(:,:), allocatable :: xysmbh
    !dir$ attributes align : 64 :: xysmbh
    
    ! Empty space in-between of buildings (per single column) x number columns
    integer(i4), dimension(:,:), allocatable :: esbb
    !dir$ attributes align : 64 :: esbb
    
     ! An area values of in-between buildings empty spaces (per single column) x number columns
    real(sp), dimension(:,:), allocatable :: aesbb
    !dir$ attributes align : 64 :: aesbb
    
    ! An area values of each building (per single building column) x number columns
    real(sp), dimension(:,:), allocatable :: abc
    !dir$ attributes align : 64 :: abc
    
     ! Number of south-facing walls (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: swpc
    !dir$ attributes align : 64 :: swpc
    
     ! Number of east-facing walls (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: ewpc
    !dir$ attributes align : 64 :: ewpc
    
    ! Number of west-facing walls (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: wwpc 
    !dir$ attributes align : 64 :: wwpc
    
     ! Number of north-facing walls (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: nwpc
    !dir$ attributes align : 64 :: nwpc
    
     ! Number of building roofs per each column
    integer(i4), dimension(:,:), allocatable :: brpc
    !dir$ attributes align : 64 :: brpc
    
     ! An area of every building [flat] roof (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: brapc
    !dir$ attributes align : 64 :: brapc
    
      ! Number of angled roof -- south facing roof wall (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: srwc
    !dir$ attributes align : 64 :: srwc
    
    ! Number of angled roof -- east facing roof wall (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: erwc
    !dir$ attributes align : 64 :: erwc
    
    ! Number of angled roof -- west facing roof wall (per each column)  x number of columns
    integer(i4), dimension(:,:), allocatable :: wrwc
    !dir$ attributes align : 64 :: wrwc
    
    ! Number angled roof -- north facing roof wall (per each column) x number of columns
    integer(i4), dimension(:,:), allocatable :: nrwc
    !dir$ attributes align : 64 :: nrwc
    
    !An angled roof inclination (deg) -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: idsrw
    !dir$ attributes align : 64 :: idsrw
    
    ! An angled roof inclination (deg) -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: iderw
    !dir$ attributes align : 64 :: iderw
    
     ! An angled roof inclination (deg) -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: idwrw
    !dir$ attributes align : 64 :: idwrw
    
      ! An angled roof inclination (deg) -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: idnrw
    !dir$ attributes align : 64 :: idnrw
    
     ! An angled roof inclination (rad) -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: irsrw
    !dir$ attributes align : 64 :: irsrw
    
    ! An angled roof inclination (rad) -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: irerw
    !dir$ attributes align : 64 :: irerw
    
     ! An angled roof inclination (rad) -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: irwrw
    !dir$ attributes align : 64 :: irwrw
    
      ! An angled roof inclination (rad) -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:),  allocatable :: irnrw
    !dir$ attributes align : 64 :: irnrw
    
     ! An angled roof inclination surface area -- south facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: isra
    !dir$ attributes align : 64 :: isra
    
     ! An angled roof inclination surface area -- west facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: iwra
    !dir$ attributes align : 64 :: iwra
    
      ! An angled roof inclination surface area -- east facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: iera
    !dir$ attributes align : 64 :: iera
    
      ! An angled roof inclination surface area -- north facing roof wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: inra
    !dir$ attributes align : 64 :: inra
    
     ! South wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:,:), allocatable :: swue
    !dir$ attributes align : 64 :: swue
    
     ! East wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:,:), allocatable :: ewue
    !dir$ attributes align : 64 :: swue
    
     ! West wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:,:), allocatable :: wwue
    !dir$ attributes align : 64 :: wwue
    
      ! North wall upper-facing edge inclination (rad) -- (per each column)  x number of columns
    real(sp), dimension(:,:), allocatable :: nwue
    !dir$ attributes align : 64 :: nwue
    
     ! Shared right edge between the south wall and east wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: sewe
    !dir$ attributes align : 64 :: sewe 
    
    ! Shared left edges between the south wall and west wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: swwe
     !dir$ attributes align : 64 :: swwe
     
    ! Shared right edges between the north wall and east wall inclination (rad) -- 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: nwee
    !dir$ attributes align : 64 :: nwee
    
     ! Shared left edges between the north wall and west wall inclination (rad) 
                                     ! -- (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: nwwe
    !dir$ attributes align : 64 :: nwwe
    
    ! South walls surface area (for every building, per column) x number of columns
    real(sp), dimension(:,:), allocatable :: swsa
    !dir$ attributes align : 64 :: swsa
    
    ! East walls surface area (for every building, per column) 
    real(sp), dimension(:,:), allocatable :: ewsa
    !dir$ attributes align : 64 :: ewsa
    
     ! West walls surface area (for every building, per column) 
    real(sp), dimension(:,:), allocatable :: wwsa
    !dir$ attributes align : 64 :: wwsa
    
      ! North walls surface area (for every building, per column) 
    real(sp), dimension(:,:), allocatable :: nwsa
    !dir$ attributes align : 64 :: nwsa
    
     ! South walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:,:), allocatable :: mnmsw
    !dir$ attributes align : 64 :: mnmsw
    
     ! East walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:,:), allocatable :: mnmew
    !dir$ attributes align : 64 :: mnmew
    
     ! West walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:,:), allocatable :: mnmww
    !dir$ attributes align : 64 :: mnmww
    
    ! North walls being either moist or non moist  (per column) x number of columns
    logical(i4), dimension(:,:), allocatable :: mnmnw
    !dir$ attributes align : 64 :: mnmnw
    
     ! The values describing the ratio (percentage) of south wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mdswr
    !dir$ attributes align : 64 :: mdswr
    
     ! The values describing the ratio (percentage) of east wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mdewr
    !dir$ attributes align : 64 :: mdewr
    
     ! The values describing the ratio (percentage) of west wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mdwwr
    !dir$ attributes align : 64 :: mdwwr
    
     ! The values describing the ratio (percentage) of north wall 
                             ! moisture to dryness (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mdnwr
    !dir$ attributes align : 64 :: mdnwr
    
    ! The logical values of flat roof moistness (being either moist or dry) 
                               ! (per column) x number of columns
    logical(i4), dimension(:,:), allocatable :: mdr
    !dir$ attributes align : 64 :: mdr
    
    ! The values describing the ratio (percentage) of flat roof moisture to dryness 
                               ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mdrr
    !dir$ attributes align : 64 :: mdrr
    
    ! The values describing the surface of moist part of the flat roof 
                                ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpfr
    !dir$ attributes align : 64 :: mpfr
    
       ! The values describing the surface of dry part of the flat roof 
                                ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpfr
    !dir$ attributes align : 64 :: dpfr
    
    ! The values describing the surface of moist part of the south wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpsw
    !dir$ attributes align : 64 :: mpsw
    
    ! The values describing the surface of dry part of the south wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpsw
    !dir$ attributes align : 64 :: dpsw
    
     ! The values describing the surface of moist part of the east wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpew
    !dir$ attributes align : 64 :: mpew
    
    ! The values describing the surface of dry part of the east wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpew
    !dir$ attributes align : 64 :: dpew
    
      ! The values describing the surface of moist part of the west wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpww
    !dir$ attributes align : 64 :: mpww
    
    ! The values describing the surface of dry part of the west wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpww
    !dir$ attributes align : 64 :: dpww
    
    ! The values describing the surface of moist part of the north wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpnw
    !dir$ attributes align : 64 :: mpnw
    
    ! The values describing the surface of dry part of the north wall 
                                 ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpnw
    !dir$ attributes align : 64 :: dpnw
    
    ! RCS values for south wall (per each column) x number of columns
    real(sp), dimension(:), allocatable :: rcssw
    !dir$ attributes align : 64 :: rcssw
    
    ! RCS values for east wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsew
    !dir$ attributes align : 64 :: rcsew
    
    ! RCS values for west wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsww
    !dir$ attributes align : 64 :: rcsww
    
     ! RCS values for north wall (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsnw
    !dir$ attributes align : 64 :: rcsnw
    
     ! RCS values for flat roof (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsfr
     !dir$ attributes align : 64 :: rcsfr
     
    ! Number of RCS values for southern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcssar
     !dir$ attributes align : 64 :: rcssar
     
    ! Number of RCS values for eastern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsear
     !dir$ attributes align : 64 :: rcsear
     
     ! Number of RCS values for western angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcswar
     !dir$ attributes align : 64 :: rcswar
     
      ! Number of RCS values for northern angled roof 
                     !(per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: rcsnar
     !dir$ attributes align : 64 :: rcsnar
     
    ! The values describing the surface of moist part of the angled south roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpsar
    !dir$ attributes align : 64 :: mpsar
    
     ! The values describing the surface of dry part of the angled south roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpsar
    !dir$ attributes align : 64 :: dpsar
    
       ! The values describing the surface of moist part of the angled east roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpear
    !dir$ attributes align : 64 :: mpear
    
     ! The values describing the surface of dry part of the angled east roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpear
    !dir$ attributes align : 64 :: dpear
    
      ! The values describing the surface of moist part of the angled west roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpwar
    !dir$ attributes align : 64 :: mpwar
    
     ! The values describing the surface of dry part of the angled west roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpwar
    !dir$ attributes align : 64 :: dpwar
    
      ! The values describing the surface of moist part of the angled north roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: mpnar
    !dir$ attributes align : 64 :: mpnar
    
     ! The values describing the surface of dry part of the angled north roof wall
                               !// ! (per each column) x number of columns
    real(sp), dimension(:,:), allocatable :: dpnar
    !dir$ attributes align : 64 :: dpnar
    
    ! The values describing the complex permittivity of south walls
                               !// ! (per each column) 
    complex(sp), dimension(:,:), allocatable :: cesw
    !dir$ attributes align : 64 :: cesw
    
    ! The values describing the complex permeabillity of south walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:,:), allocatable :: cmsw
    !dir$ attributes align : 64 :: cmsw
    
     ! The values describing the complex permittivity of west walls
                               !// ! (per each column) 
    complex(sp), dimension(:,:), allocatable :: ceww
    !dir$ attributes align : 64 :: ceww
    
   ! The values describing the complex permeabillity of west walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:,:), allocatable :: cmww
    !dir$ attributes align : 64 :: cmww
    
     ! The values describing the complex permittivity of east walls
                               !// ! (per each column) 
    complex(sp), dimension(:,:), allocatable :: ceew
    !dir$ attributes align : 64 :: ceew
    
   ! The values describing the complex permeabillity of east walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:,:), allocatable :: cmew
    !dir$ attributes align : 64 :: cmew
    
     ! The values describing the complex permittivity of north walls
                               !// ! (per each column) 
    complex(sp), dimension(:,:), allocatable :: cenw
    !dir$ attributes align : 64 :: cenw
    
   ! The values describing the complex permeabillity of north walls
                               !// ! (per each column) x number of columns 
    complex(sp), dimension(:,:), allocatable :: cmnw
    !dir$ attributes align : 64 :: cmnw
    
     ! Number of wire antennas per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: want
    !dir$ attributes align : 64 :: want
    
     ! Number of parabollic antennas per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: pant
    !dir$ attributes align : 64 :: pant
    
    ! Number of parabollic antennas per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: yant
    !dir$ attributes align : 64 :: yant
    
    ! Number of log-periodic dipole array antennas per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: lpda
     !dir$ attributes align : 64 :: lpda
     
     !  Number of cell phone sector bars antennas per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: cant
     !dir$ attributes align : 64 :: cant
     
    ! Number of cylindrical objects (e.g. ventillation) per every building (per each column)
    integer(i4), dimension(:,:), allocatable :: cylo
     !dir$ attributes align : 64 :: cylo
    
#endif
    
end module urban_model
