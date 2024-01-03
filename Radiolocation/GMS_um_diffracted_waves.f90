

#include "GMS_config.fpp"

module um_diffracted_waves


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         um_diffracted_fields
 !          
 !          Purpose:
 !                        Computation of diffracted EM waves by the methods of GTD
 !                        and PTD as stated by the cited reference sources.
 !                        The theory developed by the reference source is a firm
 !                        foundation of the computational methods for the EM wave
 !                        diffraction and scattering calculation.
 !                        
 !                        
 !          History:
 !                        Date: 01-01-2024
 !                        Time: 09:46AM GMT+2
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
 !                     Method of Edge Waves in the Physical Theory of Diffraction, Ufimtsev P. Ya.
 !                     Fundamentals of the Physical Theory of Diffraction, Ufimtsev P. Ya
 !                     Геометрическая теория дифракции, Боровиков В.А., Кинбер Б.Е.
 !                     Дифракция на многоугольниках и многогранниках, Боровиков В.А.
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use mod_kinds,    only : i4,sp
    
    public
    implicit none
    


     ! Major version
    integer(kind=i4),  parameter :: UM_DIFFRACTED_WAVES_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: UM_DIFFRACTED_WAVES_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: UM_DIFFRACTED_WAVES_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: UM_DIFFRACTED_WAVES_FULLVER =   &
            1000*UM_DIFFRACTED_WAVES_MAJOR+100*UM_DIFFRACTED_WAVES_MINOR+10*UM_DIFFRACTED_WAVES_MICRO
    ! Module creation date
    character(*),        parameter :: UM_DIFFRACTED_WAVES_CREATE_DATE = "01-01-2024 10:01AM +00200 (MON 01 01 2024 GMT+2)"
    ! Module build date
    character(*),        parameter :: UM_DIFFRACTED_WAVES_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: UM_DIFFRACTED_WAVES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: UM_DIFFRACTED_WAVES_SYNOPSIS    = "Computation of diffracted EM WAVES by the methods of PTD and GTD."   

    
    !  Helper type, a holder of module arrays size variables
    !  serves the purpose for the whole data initialization.
    
    type, public :: UMDataHolder_t
    
          sequence
           ! Number of rows
          integer(i4) :: xnrows
          ! Number of columns
          integer(i4) :: xncols
          ! 
          integer(i4) :: xnblatd
          ! Number of longtitude values (deg), per building
          integer(i4) :: xnblond
          ! Number of latitude   values (rad), per building
          integer(i4) :: xnblatr
          ! Number of longtitude values (rad), per building
          integer(i4) :: xnblonr
          ! Number of ellipsoidal (radar waveform irradiating field) cells for building column
          integer(i4) :: xnellb
          ! Parametric equation x (acos(t)) values (building)
          integer(i4) :: xnpxb
          ! Parametric equation y (b(sin(t)) values (building)
          integer(i4) :: xnpyb
           ! Number of building units per column
          integer(i4) :: xnbpc
          ! Number of building units per row
          integer(i4) :: xnbpr
          ! Number of streets
          integer(i4) :: xnstr
          ! Length of every street
          integer(i4) :: xnlstr
          ! Width of every street
          integer(i4) :: xnwstr
          ! An area of every street
          integer(i4) :: xnarstr
          ! Moisture of every street  at each ellipsoidal cell
          integer(i4) :: xnwtstr
          ! Percent of moist to dry area of evey street at each ellipsoidal cell
          integer(i4) :: xnphds
          ! Coverage of every street (like: snow,mud, ...etc)
          integer(i4) :: xncstr
          ! Percent of covered to non-covered portion of every street
          integer(i4) :: xnpcns
           ! Average thickness of each layer (cover) of every street at each irradiated cell
          integer(i4) :: xnatcstr
           ! Thickness of cover along street (number of values) each irradiated cell
          integer(i4) :: xntcstr
          ! Mu for 'clean' street interpolated along the street length at each irradiated cell
          integer(i4) :: xnmustr1
          ! Eps for 'clean' street street length interpolated at each irradiated cell
          integer(i4) :: xnepsstr1
          ! Mu for covered (i.e. by mud,snow,clay, ..etc) street interpolated along the street length
          integer(i4) :: xnmustr2
          ! Eps for covered (i.e. by mud,snow,clay, ..etc) street street length interpolated
          integer(i4) :: xnepsstr2
          ! Street curvature parametric equation u-parameter
          integer(i4) :: xnupstr
          ! Street curvature parametric equation v-parameter
          integer(i4) :: xnvpstr
          ! Street surface normal vectors x-components along the street length at each irradiated cell
          integer(i4) :: xnnxstr
          ! Street surface normal vectors y-components along the street length at each irradiated cell
          integer(i4) :: xnnystr
          ! Street surface normal vectors z-components along the street length at each irradiated cell
          integer(i4) :: xnnzstr
          ! Number of latitude   values (deg), per street length (at irradiance point)
          integer(i4) :: xnslatd
          ! Number of longtitude values (deg), per street length (at irradiance point)
          integer(i4) :: xnslond
          ! Number of latitude   values (rad), per street length (at irradiance point)
          integer(i4) :: xnslatr
          ! Number of longtitude values (rad), per street length (at irradiance point)
          integer(i4) :: xnslonr
          ! Urban area height map (at single building resolution), x-coordinate
          integer(i4) :: xnxbh
          ! Urban area height map (at single building resolution), y-coordinate
          integer(i4) :: xnybh
          ! ! Urban area height map (at single building resolution), x-coordinate, its first derivative
          integer(i4) :: xndxbh
          ! Urban area height map (at single building resolution), y-coordinate, its first derivative
          integer(i4) :: xndybh
          ! Height field gradient x-components
          integer(i4) :: xngradxbh
           ! Height field gradient y-components
          integer(i4) :: xngradybh
          ! Number of values of smoothing and approximating curve for linearly-piecewise height function (x-coordinate)
          integer(i4) :: xnxsmbh
          ! Number of values of smoothing and approximating curve for linearly-piecewise height function (y-coordinate)
          integer(i4) :: xnysmbh
    !! ********************* Building geometry ***************************** //
    
          ! Number of values of empty space in-between of buildings (per single column)
          integer(i4) :: xnesbb
           ! Number of area values of in-between buildings empty spaces (per single column)
          integer(i4) :: xnaesbb
          ! Number of area values of each building (per single building column)
          integer(i4) :: xnabc
          ! Number of area of every building [flat] roof (per each column)
          integer(i4) :: xnbrapc
          ! Number of angled roof inclination (deg) -- south facing roof wall (per each column)
          integer(i4) :: xnidsrw
           ! Number of angled roof inclination (deg) -- east facing roof wall (per each column)
          integer(i4) :: xniderw
           ! Number of angled roof inclination (deg) -- west facing roof wall (per each column)
          integer(i4) :: xnidwrw
           ! Number of angled roof inclination (deg) -- north facing roof wall (per each column)
          integer(i4) :: xnidnrw
           ! Number of angled roof inclination (rad) -- south facing roof wall (per each column)
          integer(i4) :: xnirsrw
          ! Number of angled roof inclination (rad) -- east facing roof wall (per each column)
          integer(i4) :: xnirerw
           ! Number of angled roof inclination (rad) -- west facing roof wall (per each column)
          integer(i4) :: xnirwrw
           ! Number of angled roof inclination (rad) -- north facing roof wall (per each column)
          integer(i4) :: xnirnrw
           ! Number of angled roof inclination surface area -- south facing roof wall (per each column)
          integer(i4) :: xnisra
           ! Number of angled roof inclination surface area -- east facing roof wall (per each column)
          integer(i4) :: xniera
           ! Number of angled roof inclination surface area -- west facing roof wall (per each column)
          integer(i4) :: xniwra
           ! Number of angled roof inclination surface area -- north facing roof wall (per each column)
          integer(i4) :: xninra
            ! Number of south wall upper-facing edge inclination (rad) -- (per each column)
          integer(i4) :: xnswue
            ! Number of east wall upper-facing edge inclination (rad) -- (per each column)
          integer(i4) :: xnewue
             ! Number of west wall upper-facing edge inclination (rad) -- (per each column)
          integer(i4) :: xnwwue
            ! Number of north wall upper-facing edge inclination (rad) -- (per each column)
          integer(i4) :: xnnwue
            ! Number of shared right edges between the south wall and east wall inclination (rad) -- (per each column)
          integer(i4) :: xnsewe
            ! Number of shared left edges between the south wall and west wall inclination (rad) -- (per each column)
          integer(i4) :: xnswwe
            ! Number of shared right edges between the north wall and east wall inclination (rad) -- (per each column)
          integer(i4) :: xnnewe
            ! Number of shared left edges between the north wall and west wall inclination (rad) -- (per each column)
          integer(i4) :: xnnwwe
            ! Number of south walls surface area (for every building, per column) 
          integer(i4) :: xnswsa
            ! Number of east walls surface area (for every building, per column) 
          integer(i4) :: xnewsa
            ! Number of west walls surface area (for every building, per column) 
          integer(i4) :: xnwwsa
            ! Number of north walls surface area (for every building, per column) 
          integer(i4) :: xnnwsa
            ! Number of south walls being either moist or non moist  (per column) x number of columns
          integer(i4) :: xnmnmsw
            ! Number of east walls being either moist or non moist  (per column) x number of columns
          integer(i4) :: xnmnmew
            ! Number of west walls being either moist or non moist  (per column) x number of columns
          integer(i4) :: xnmnmww
            ! Number of north walls being either moist or non moist  (per column) x number of columns
          integer(i4) :: xnmnmnw
            ! Number of values describing the ratio (percentage) of south wall moisture to dryness (per each column) 
          integer(i4) :: xnmdswr
            ! Number of values describing the ratio (percentage) of east wall moisture to dryness (per each column) 
          integer(i4) :: xnmdewr
             ! Number of values describing the ratio (percentage) of west wall moisture to dryness (per each column) 
          integer(i4) :: xnmdwwr
             ! Number of values describing the ratio (percentage) of north wall moisture to dryness (per each column) 
          integer(i4) :: xnmdnwr
             ! Number of logical values of flat roof moistness (being either moist or dry) (per column)
          integer(i4) :: xnmdr
             ! Number of values describing the ratio (percentage) of flat roof moisture to dryness (per each column) 
          integer(i4) :: xnmdrr
             ! Number of values describing the surface of moist part of the flat roof (per each column) 
          integer(i4) :: xnmpfr
             ! Number of values describing the surface of dry part of the flat roof (per each column) 
          integer(i4) :: xndpfr
             ! Number of values describing the surface of moist part of the south wall (per each column) 
          integer(i4) :: xnmpsw
             ! Number of values describing the surface of dry part of the south wall (per each column) 
          integer(i4) :: xndpsw
             ! Number of values describing the surface of moist part of the east wall (per each column) 
          integer(i4) :: xnmpew
             ! Number of values describing the surface of dry part of the east wall (per each column) 
          integer(i4) :: xndpew
              ! Number of values describing the surface of moist part of the west wall (per each column) 
          integer(i4) :: xnmpww
              ! Number of values describing the surface of dry part of the west wall (per each column) 
          integer(i4) :: xndpww
              ! Number of values describing the surface of moist part of the north wall (per each column) 
          integer(i4) :: xnmpnw
             ! Number of values describing the surface of dry part of the north wall (per each column) 
          integer(i4) :: xndpnw
             ! Number of RCS values for south wall (per each column) 
          integer(i4) :: xnrcssw
             ! Number of RCS values for east wall (per each column) 
          integer(i4) :: xnrcsew
             ! Number of RCS values for west wall (per each column) 
          integer(i4) :: xnrcsww
             ! Number of RCS values for north wall (per each column) 
          integer(i4) :: xnrcsnw
             ! Number of RCS values for flat roof (per each column) 
          integer(i4) :: xnrcsfr
             ! Number of RCS values for southern angled roof (per each column) 
          integer(i4) :: xnrcssar
             ! Number of RCS values for eastern angled roof (per each column) 
          integer(i4) :: xnrcsear
              ! Number of RCS values for western angled roof (per each column) 
          integer(i4) :: xnrcswar
             ! Number of RCS values for northern angled roof (per each column) 
          integer(i4) :: xnrcsnar
            ! Number of values describing the surface of moist part of the angled south roof wall
                               !// ! (per each column)
          integer(i4) :: xnmpsar
           ! Number of values describing the surface of dry part of the angled south roof wall
                               !// ! (per each column)
          integer(i4) :: xndpsar
            ! Number of values describing the surface of moist part of the angled east roof wall
                               !// ! (per each column) 
          integer(i4) :: xnmpear
          ! Number of values describing the surface of dry part of the angled east roof wall
                               !// ! (per each column) 
          integer(i4) :: xndpear
           ! Number of values describing the surface of moist part of the angled west roof wall
                               !// ! (per each column) 
          integer(i4) :: xnmpwar
          ! Number of values describing the surface of dry part of the angled west roof wall
                               !// ! (per each column) 
          integer(i4) :: xndpwar
           ! Number of values describing the surface of moist part of the angled north roof wall
                               !// ! (per each column) 
          integer(i4) :: xnmpnar
           ! Number of values describing the surface of dry part of the angled north roof wall
                               !// ! (per each column) 
          integer(i4) :: xndpnar
          ! Number of values describing the complex permittivity of south walls
                               !// ! (per each column) 
          integer(i4) :: xncesw
          ! Number of values describing the complex permeabillity of south walls
                              ! // ! (per each column) x number of columns  
          integer(i4) :: xncmsw
           ! Number of values describing the complex permittivity of west walls
                               !// ! (per each column) 
          integer(i4) :: xnceww
          ! Number of values describing the complex permeabillity of west walls
                              ! // ! (per each column) x number of columns  
          integer(i4) :: xncmww
          ! Number of values describing the complex permittivity of east walls
                               !// ! (per each column) 
          integer(i4) :: xnceew
          ! Number of values describing the complex permeabillity of east walls
                              ! // ! (per each column) x number of columns  
          integer(i4) :: xncmew
          ! Number of values describing the complex permittivity of north walls
                               !// ! (per each column) 
          integer(i4) :: xncenw
          ! Number of values describing the complex permeabillity of north walls
                              ! // ! (per each column) x number of columns  
          integer(i4) :: xncmnw
          ! Number of wire antennas per every building (per each column)
          integer(i4) :: xnwant
          ! Number of parabolic antennas per every building (per each column)
          integer(i4) :: xnpant
          ! Number of yagi type antennas per every building (per each column)
          integer(i4) :: xnyant
           ! Number of log-periodic dipole array antennas per every building (per each column)
          integer(i4) :: xnlpda
          ! Number of cell phone sector bars antennas per every building (per each column)
          integer(i4) :: xncant
          ! Number of cylindrical objects (e.g. ventillation) per every building (per each column)
          integer(i4) :: xncylo
    end type UMDataHolder_t
    
    
    contains
    
    
       subroutine init_data_length(umdh) 
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: init_data_length
           use urban_model
           type(UMDataHolder_t),       intent(in) :: umdh
           ! Executable code
           nrows    = umdh.xnrows
           ncols    = umdh.xncols
           nblatd   = umdh.xnblatd
           nblond   = umdh.xnblond
           nblatr   = umdh.xnblatr
           nblonr   = umdh.xnblonr
           nellb    = umdh.xnellb 
           npxb     = umdh.xnpxb
           npyb     = umdh.xnypb
           nbpc     = umdh.xnbpc
           nbpr     = umdh.xnbpr
           nstr     = umdh.xnstr
           nlstr    = umdh.xnlstr
           nswtr    = umdh.xnwstr
           narstr   = umdh.xnarstr
           nwtstr   = umdh.xnwtstr
           nphds    = umdh.xnphds
           ncstr    = umdh.xncstr
           npcns    = umdh.xnpcns
           natcstr  = umdh.xnatcstr
           ntcstr   = umdh.xntcstr
           nmustr1  = umdh.xnmustr1
           nepsstr1 = umdh.xnepsstr1
           nmustr2  = umdh.xnmustr2
           nepsstr2 = umdh.xnepsstr2
           nupstr   = umdh.xnupstr
           nvpstr   = umdh.xnvpstr
           nnxstr   = umdh.xnnxstr
           nnystr   = umdh.xnnystr
           nnzstr   = umdh.xnnzstr
           nslatd   = umdh.xnslatd
           nslond   = umdh.xnslond
           nslatr   = umdh.xnslatr
           nslonr   = umdh.xnslonr
           nxbh     = umdh.xnxbh
           nybh     = umdh.xnybh
           ndxbh    = umdh.xndxbh
           ndybh    = umdh.xndybh
           ngrdxbh  = umdh.xngrdxbh
           ngrdybh  = umdh.xngrdybh
           nxsmbh   = umdh.xnxsmbh
           nysmbh   = umdh.xnysmbh
           nesbb    = umdh.xnesbb
           naesbb   = umdh.xnaesbb
           nabc     = umdh.xnabc
           nbrapc   = umdh.xnbrapc
           nidsrw   = umdh.xnidsrw
           niderw   = umdh.xniderw
           nidwrw   = umdh.xnidwrw
           nidnrw   = umdh.xnidnrw
           nirsrw   = umdh.xnirsrw
           nirerw   = umdh.xnirerw
           nirwrw   = umdh.xnirwrw
           nirnrw   = umdh.xnirnrw
           nisra    = umdh.xnisra
           niera    = umdh.xniera
           niwra    = umdh.xniwra
           ninra    = umdh.xninra
           nswue    = umdh.xnswue
           newue    = umdh.xnewue
           nnwue    = umdh.xnnwue
           nsewe    = umdh.xnsewe
           nswwe    = umdh.xnswwe
           nnewe    = umdh.xnnewe
           nnwwe    = umdh.xnnwwe
           nswsa    = umdh.xnswsa
           newsa    = umdh.xnewsa
           nwwsa    = umdh.xnwwsa
           nmnmsw   = undh.xnmnmsw
           nmnmew   = umdh.xnmnmsw
           nmnmww   = umdh.xnmnmww
           nmdswr   = umdh.xnmdswr
           nmdewr   = umdh.xnmdewr
           nmdwwr   = umdh.xnmdwwr
           nmdnwr   = umdh.xnmdnwr
           nmdr     = umdh.xnmdr
           nmdrr    = umdh.xnmdrr
           nmpfr    = umdh.xnmpfr
           ndpfr    = umdh.xndpfr
           nmpsw    = umdh.xnmpsw
           ndpsw    = umdh.xndpsw
           nmpew    = umdh.xnmpew
           ndpew    = umdh.xndpew
           nmpww    = umdh.xnmpww
           ndpww    = umdh.xndpww
           nmpnw    = umdh.xnmpnw
           ndpnw    = umdh.xndpnw
           nrcssw   = umdh.xnrcssw
           nrcsew   = umdh.xnrcsew
           nrcsww   = umdh.xnrcsww
           nrcsnw   = umdh.xnrcsnw
           nrcsfr   = umdh.xnrcsfr
           nrcssar  = umdh.xnrcssar
           nrcsear  = umdh.xnrcsear
           nrcswar  = umdh.xnrcswar
           nrcsnar  = umdh.xnrcsnar
           nmpsar   = umdh.xnmpsar
           ndpsar   = umdh.xndpsar
           nmpear   = umdh.xnmpear
           ndpear   = umdh.xndpear
           nmpwar   = umdh.xnmpwar
           ndpwar   = umdh.xndpwar
           nmpnar   = umdh.xnmpnar
           ndpnar   = umdh.xndpnar
           ncesw    = umdh.xncesw
           ncmsw    = umdh.xncmsw
           nceww    = umdh.xnceww
           ncmww    = umdh.xncmww
           nceew    = umdh.xnceew
           ncmew    = umdh.xncmew
           ncenw    = umdh.xncenw
           ncmnw    = umdh.xncmnw
           nwant    = umdh.xnwant
           npant    = umdh.xnpant
           nyant    = umdh.xnyant
           nlpda    = umdh.xnlpda
           ncant    = umdh.xncant
           ncylo    = umdh.xncylo
                         
       end subroutine init_data_length


       subroutine alloc_um_arrays()
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: init_data_length
           use urban_model
           ! Executable code
           allocate(blatd(1:nblatd))
           allocate(blond(1:nblond))
           allocate(blatr(1:nblatr))
           allocate(blonr(1:nblonr))
           allocate(bpc(1:nbpc))
           allocate(bpr(1:nbpr))
           allocate(lstr(1:nstr))
           allocate(wstr(1:nstr))
           allocate(arstr(1:nstr))
           allocate(ellpb(1:nellpb,1:nbpc))
           allocate(pxb(1:npxb,1:nbpc))
           allocate(pyb(1:npyb,1:nbpc))
           allocate(wtstr(1:nwtstr,1:nstr))
           allocate(phds(1:nphds,1:nstr))
           allocate(cstr(1:ncstr,1:nstr))
           allocate(pcns(1:npcns,1:nstr))
           allocate(atcstr(1:natcstr,1:nstr))
           allocate(tcstr(1:ntcstr,1:nstr))
           allocate(mustr1(1:nmustr1,1:nstr))
           allocate(epsstr1(1:nepsstr1,1:nstr))
           allocate(mustr2(1:nmustr2,1:nstr))
           allocate(epsstr2(1:nepsstr2,1:nstr))
           allocate(upstr(1:nupstr,1:nstr))
           allocate(vpstr(1:nvpstr,1:nstr))
           allocate(nxstr(1:nnxstr,1:nstr))
           allocate(nystr(1:nnystr,1:nstr))
           allocate(nzstr(1:nnzstr,1:nstr))
           allocate(slatd(1:nslatd,1:nstr))
           allocate(slond(1:nslond,1:nstr))
           allocate(slatr(1:nslatr,1:nstr))
           allocate(slonr(1:nslonr,1:nstr))
           allocate(bhmap(1:nxbh,1:nybh))
           allocate(bhdxdy(1:ndxbh,1:ndybh))
           allocate(bhgrdx(1:ngrdxbh,1:ngrdybh))
           allocate(xysmbh(1:nxsmbh,1:nysmbh))
           allocate(esbb(1:nesbb,1:ncols))
           allocate(aesbb(1:naesbb,1:ncols))
           allocate(abc(1:nabc,1:ncols))
           allocate(swpc(1:ncols))
           allocate(ewpc(1:ncols))
           allocate(wwpc(1:ncols))
           allocate(nwpc(1:ncols))
           allocate(brpc(1:ncols))
           allocate(brapc(1:nbrapc,1:nbpc))
           allocate(srwc(1:ncols))
           allocate(erwc(1:ncols))
           allocate(wrwc(1:ncols))
           allocate(nrwc(1:ncols))
           allocate(idsrw(1:nidsrw,1:nbpc))
           allocate(iderw(1:niderw,1:nbpc))
           allocate(idwrw(1:nidwrw,1:nbpc))
           allocate(idnrw(1:nidnrw,1:nbpc))
           allocate(irsrw(1:nirsrw,1:nbpc))
           allocate(irerw(1:nirerw,1:nbpc))
           allocate(irwrw(1:nirwrw,1:nbpc))
           allocate(irnrw(1:nirnrw,1:nbpc))
           allocate(isra(1:nisra,1:nbpc))
           allocate(iwra(1:niwra,1:nbpc))
           allocate(iera(1:niera,1:nbpc))
           allocate(inra(1:ninra,1:nbpc))
           
       end subroutine alloc_um_arrays

























end module um_diffracted_fields
