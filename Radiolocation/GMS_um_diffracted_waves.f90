

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
    
    use mod_kinds,    only : i1,i4,sp
    
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
           !dir$ attributes code_align : 32 :: alloc_um_arrays
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
           allocate(swue(1:nswue,1:nbpc))
           allocate(ewue(1:newue,1:nbpc))
           allocate(wwue(1:nwwue,1:nbpc))
           allocate(nwue(1:nnwue,1:nbpc))
           allocate(sewe(1:nsewe,1:nbpc))
           allocate(swwe(1:nswwe,1:nbpc))
           allocate(nwee(1:nnwee,1:nbpc))
           allocate(swsa(1:nswsa,1:nbpc))
           allocate(ewsa(1:newsa,1:nbpc))
           allocate(wwsa(1:nwwsa,1:nbpc))
           allocate(nwsa(1:nnwsa,1:nbpc))
           allocate(mnmsw(1:nmnmsw,1:nbpc))
           allocate(mnmew(1:nmnmew,1:nbpc))
           allocate(mnmww(1:nmnmww,1:nbpc))
           allocate(mnmnw(1:nmnmnw,1:nbpc))
           allocate(mdswr(1:nmdswr,1:nbpc))
           allocate(mdewr(1:nmdewr,1:nbpc))
           allocate(mdwwr(1:nmdwwr,1:nbpc))
           allocate(mdnwr(1:nmdnwr,1:nbpc))
           allocate(mdr(1:nmdr,1:nbpc))
           allocate(mdrr(1:nmdrr,1:nbpc))
           allocate(mpfr(1:nmpfr,1:nbpc))
           allocate(dpfr(1:ndpfr,1:nbpc))
           allocate(mpsw(1:nmpsw,1:nbpc))
           allocate(dpsw(1:ndpsw,1:nbpc))
           allocate(mpew(1:nmpew,1:nbpc))
           allocate(dpew(1:ndpew,1:nbpc))
           allocate(mpww(1:nmpww,1:nbpc))
           allocate(dpww(1:ndpww,1:nbpc))
           allocate(mpnw(1:nmpnw,1:nbpc))
           allocate(dpnw(1:ndpnw,1:nbpc))
           allocate(rcssw(1:nrcssw,1:nbpc))
           allocate(rcsew(1:nrcsew,1:nbpc))
           allocate(rcsww(1:nrcsww,1:nbpc))
           allocate(rcsnw(1:nrcsnw,1:nbpc))
           allocate(rcsfr(1:nrcsfr,1:nbpc))
           allocate(rcssar(1:nrcssar,1:nbpc))
           allocate(rcsear(1:nrcsear,1:nbpc))
           allocate(rcswar(1:nrcswar,1:nbpc))
           allocate(rcsnar(1:nrcsnar,1:nbpc))
           allocate(mpsar(1:nmpsar,1:nbpc))
           allocate(dpsar(1:ndpsar,1:nbpc))
           allocate(mpear(1:nmpear,1:nbpc))
           allocate(dpear(1:ndpear,1:nbpc))
           allocate(mpwar(1:nmpwar,1:nbpc))
           allocate(dpwar(1:ndpwar,1:nbpc))
           allocate(mpnar(1:nmpnar,1:nbpc))
           allocate(dpnar(1:ndpnar,1:nbpc))
           allocate(cesw(1:ncesw,1:nbpc))
           allocate(cmsw(1:ncmsw,1:nbpc))
           allocate(ceww(1:nceww,1:nbpc))
           allocate(cmww(1:ncmww,1:nbpc))
           allocate(ceew(1:nceew,1:nbpc))
           allocate(cmew(1:ncmew,1:nbpc))
           allocate(cenw(1:ncenw,1:nbpc))
           allocate(cmnw(1:mcmnw,1:nbpc))
           allocate(want(1:nwant,1:nbpc))
           allocate(pant(1:npant,1:nbpc))
           allocate(yant(1:nyant,1:nbpc))
           allocate(lpda(1:nlpda,1:nbpc))
           allocate(cant(1:ncant,1:nbpc))
           allocate(cylo(1:ncylo,1:nbpc))
       end subroutine alloc_um_arrays
       
       
       subroutine alloc_um_arrays_omp_sec()
           !dir$ attributes code_align : 32 :: alloc_um_arrays_omp_sec
           use urban_model
           use omp_lib
           ! Executable code
           integer(kind=i4) :: nthds
           nthds = omp_get_num_threads()
           if(nthds<2) return
!$omp parallel sections    
!$omp section       
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
           
!$omp section
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
!$omp section

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
           allocate(swue(1:nswue,1:nbpc))
           allocate(ewue(1:newue,1:nbpc))
           allocate(wwue(1:nwwue,1:nbpc))
!$omp section

           allocate(nwue(1:nnwue,1:nbpc))
           allocate(sewe(1:nsewe,1:nbpc))
           allocate(swwe(1:nswwe,1:nbpc))
           allocate(nwee(1:nnwee,1:nbpc))
           allocate(swsa(1:nswsa,1:nbpc))
           allocate(ewsa(1:newsa,1:nbpc))
           allocate(wwsa(1:nwwsa,1:nbpc))
           allocate(nwsa(1:nnwsa,1:nbpc))
           allocate(mnmsw(1:nmnmsw,1:nbpc))
           allocate(mnmew(1:nmnmew,1:nbpc))
           allocate(mnmww(1:nmnmww,1:nbpc))
           allocate(mnmnw(1:nmnmnw,1:nbpc))
           allocate(mdswr(1:nmdswr,1:nbpc))
           allocate(mdewr(1:nmdewr,1:nbpc))
           allocate(mdwwr(1:nmdwwr,1:nbpc))
           allocate(mdnwr(1:nmdnwr,1:nbpc))
           allocate(mdr(1:nmdr,1:nbpc))
           allocate(mdrr(1:nmdrr,1:nbpc))
           allocate(mpfr(1:nmpfr,1:nbpc))
           allocate(dpfr(1:ndpfr,1:nbpc))
           allocate(mpsw(1:nmpsw,1:nbpc))
!$omp section

           allocate(dpsw(1:ndpsw,1:nbpc))
           allocate(mpew(1:nmpew,1:nbpc))
           allocate(dpew(1:ndpew,1:nbpc))
           allocate(mpww(1:nmpww,1:nbpc))
           allocate(dpww(1:ndpww,1:nbpc))
           allocate(mpnw(1:nmpnw,1:nbpc))
           allocate(dpnw(1:ndpnw,1:nbpc))
           allocate(rcssw(1:nrcssw,1:nbpc))
           allocate(rcsew(1:nrcsew,1:nbpc))
           allocate(rcsww(1:nrcsww,1:nbpc))
           allocate(rcsnw(1:nrcsnw,1:nbpc))
           allocate(rcsfr(1:nrcsfr,1:nbpc))
           allocate(rcssar(1:nrcssar,1:nbpc))
           allocate(rcsear(1:nrcsear,1:nbpc))
           allocate(rcswar(1:nrcswar,1:nbpc))
           allocate(rcsnar(1:nrcsnar,1:nbpc))
           allocate(mpsar(1:nmpsar,1:nbpc))
           allocate(dpsar(1:ndpsar,1:nbpc))
           allocate(mpear(1:nmpear,1:nbpc))
           allocate(dpear(1:ndpear,1:nbpc))
           allocate(mpwar(1:nmpwar,1:nbpc))
           
!$omp section
           allocate(dpwar(1:ndpwar,1:nbpc))
           allocate(mpnar(1:nmpnar,1:nbpc))
           allocate(dpnar(1:ndpnar,1:nbpc))
           allocate(cesw(1:ncesw,1:nbpc))
           allocate(cmsw(1:ncmsw,1:nbpc))
           allocate(ceww(1:nceww,1:nbpc))
           allocate(cmww(1:ncmww,1:nbpc))
           allocate(ceew(1:nceew,1:nbpc))
           allocate(cmew(1:ncmew,1:nbpc))
           allocate(cenw(1:ncenw,1:nbpc))
           allocate(cmnw(1:mcmnw,1:nbpc))
           allocate(want(1:nwant,1:nbpc))
           allocate(pant(1:npant,1:nbpc))
           allocate(yant(1:nyant,1:nbpc))
           allocate(lpda(1:nlpda,1:nbpc))
           allocate(cant(1:ncant,1:nbpc))
           allocate(cylo(1:ncylo,1:nbpc))
!$omp end parallel sections
       end subroutine alloc_um_arrays_omp_sec
       
       
       subroutine dealloc_um_arrays(check_alloc)
          !dir$ attributes code_align : 32 :: dealloc_um_arrays
          use urban_model
          logical(i4),   intent(in) :: check_alloc
          
          if(check_alloc) then
             if(allocated(cylo))   deallocate(cylo)
             if(allocated(cant))   deallocate(cant)
             if(allocated(lpda))   deallocate(lpda)
             if(allocated(yant))   deallocate(yant)
             if(allocated(pant))   deallocate(pant)
             if(allocated(want))   deallocate(want)
             if(allocated(cmnw))   deallocate(cmnw)
             if(allocated(cenw))   deallocate(cenw)
             if(allocated(cmew))   deallocate(cmew)
             if(allocated(ceew))   deallocate(ceew)
             if(allocated(cmww))   deallocate(cmww)
             if(allocated(ceww))   deallocate(ceww)
             if(allocated(cmsw))   deallocate(cmsw)
             if(allocated(cesw))   deallocate(cesw)
             if(allocated(dpnar))  deallocate(dpnar)
             if(allocated(mpnar))  deallocate(mpnar)
             if(allocated(dpwar))  deallocate(dpwar)
             if(allocated(mpwar))  deallocate(mpwar)
             if(allocated(dpear))  deallocate(dpear)
             if(allocated(mpear))  deallocate(mpear)
             if(allocated(dpsar))  deallocate(dpsar)
             if(allocated(mpsar))  deallocate(mpsar)
             if(allocated(rcsnar)) deallocate(rcsnar)
             if(allocated(rcswar)) deallocate(rcswar)
             if(allocated(rcsear)) deallocate(rcsear)
             if(allocated(rcssar)) deallocate(rcssar)
             if(allocated(rcsfr))  deallocate(rcsfr)
             if(allocated(rcsnw))  deallocate(rcsnw)
             if(allocated(rcsww))  deallocate(rcsww)
             if(allocated(rcsew))  deallocate(rcsew)
             if(allocated(rcssw))  deallocate(rcssw)
             if(allocated(dpnw))   deallocate(dpnw)
             if(allocated(mpnw))   deallocate(mpnw)
             if(allocated(dpew))   deallocate(dpew)
             if(allocated(mpew))   deallocate(mpew)
             if(allocated(dpsw))   deallocate(dpsw)
             if(allocated(mpsw))   deallocate(mpsw)
             if(allocated(dpfr))   deallocate(dpfr)
             if(allocated(mpfr))   deallocate(mpfr)
             if(allocated(mdrr))   deallocate(mdrr)
             if(allocated(mdr))    deallocate(mdr)
             if(allocated(mdnwr))  deallocate(mdnwr)
             if(allocated(mdwwr))  deallocate(mdwwr)
             if(allocated(mdewr))  deallocate(mdewr)
             if(allocated(mdswr))  deallocate(mdswr)
             if(allocated(mnmnw))  deallocate(mnmnw)
             if(allocated(mnmww))  deallocate(mnmww)
             if(allocated(mnmew))  deallocate(mnmew)
             if(allocated(mnmsw))  deallocate(mnmsw)
             if(allocated(nwsa))   deallocate(nwsa)
             if(allocated(wwsa))   deallocate(wwsa)
             if(allocated(ewsa))   deallocate(ewsa)
             if(allocated(swsa))   deallocate(swsa)
             if(allocated(nwwe))   deallocate(nwwe)
             if(allocated(nwee))   deallocate(nwee)
             if(allocated(swwe))   deallocate(swwe)
             if(allocated(sewe))   deallocate(sewe)
             if(allocated(nwue))   deallocate(nwue)
             if(allocated(wwue))   deallocate(wwue)
             if(allocated(ewue))   deallocate(ewue)
             if(allocated(swue))   deallocate(swue)
             if(allocated(inra))   deallocate(inra)
             if(allocated(iera))   deallocate(iera)
             if(allocated(iwra))   deallocate(iwra)
             if(allocated(isra))   deallocate(isra)
             if(allocated(irnrw))  deallocate(irnrw)
             if(allocated(irwrw))  deallocate(irwrw)
             if(allocated(irerw))  deallocate(irerw)
             if(allocated(irsrw))  deallocate(irsrw)
             if(allocated(idnrw))  deallocate(idnrw)
             if(allocated(idwrw))  deallocate(idwrw)
             if(allocated(iderw))  deallocate(iderw)
             if(allocated(idsrw))  deallocate(idsrw)
             if(allocated(nrwc))   deallocate(nrwc)
             if(allocated(wrwc))   deallocate(wrwc)
             if(allocated(erwc))   deallocate(erwc)
             if(allocated(srwc))   deallocate(srwc)
             if(allocated(brapc))  deallocate(brapc)
             if(allocated(brpc))   deallocate(brpc)
             if(allocated(nwpc))   deallocate(nwpc)
             if(allocated(wwpc))   deallocate(wwpc)
             if(allocated(ewpc))   deallocate(ewpc)
             if(allocated(swpc))   deallocate(swpc)
             if(allocated(abc))    deallocate(abc)
             if(allocated(aesbb))  deallocate(aesbb)
             if(allocated(esbb))   deallocate(esbb)
             if(allocated(xysmbh)) deallocate(xysmbh)
             if(allocated(bhgrdy)) deallocate(bhgrdy)
             if(allocated(bhgrdx)) deallocate(bhgrdx)
             if(allocated(bhdxdy)) deallocate(bhdxdy)
             if(allocated(bhmap))  deallocate(bhmap)
             if(allocated(slonr))  deallocate(slonr)
             if(allocated(slanr))  deallocate(slanr)
             if(allocated(slond))  deallocate(slond)
             if(allocated(slatd))  deallocate(slatd)
             if(allocated(nzstr))  deallocate(nzstr)
             if(allocated(nystr))  deallocate(nystr)
             if(allocated(nxstr))  deallocate(nxstr)
             if(allocated(vpstr))  deallocate(vpstr)
             if(allocated(upstr))  deallocate(upstr)
             if(allocated(epsstr2))deallocate(epsstr2)
             if(allocated(mustr2)) deallocate(mustr2)
             if(allocated(epsstr1))deallocate(epsstr1)
             if(allocated(mustr1)) deallocate(mustr1)
             if(allocated(tcstr))  deallocate(tcstr)
             if(allocated(atcstr)) deallocate(atcstr)
             if(allocated(pcns))   deallocate(pcns)
             if(allocated(cstr))   deallocate(cstr)
             if(allocated(phds))   deallocate(phds)
             if(allocated(wtstr))  deallocate(wtstr)
             if(allocated(arstr))  deallocate(arstr)
             if(allocated(wstr))   deallocate(wstr)
             if(allocated(lstr))   deallocate(lstr)
             if(allocated(bpr))    deallocate(bpr)
             if(allocated(bpc))    deallocate(bpc)
             if(allocated(pyb))    deallocate(pyb)
             if(allocated(pxb))    deallocate(pxb)
             if(allocated(ellpb))  deallocate(ellpb)
             if(allocated(blonr))  deallocate(blonr)
             if(allocated(blatr))  deallocate(blatr)
             if(allocated(blond))  deallocate(blond)
             if(allocated(blatd))  deallocate(blatd)
          else 
             deallocate(cylo)
             deallocate(cant)
             deallocate(lpda)
             deallocate(yant)
             deallocate(pant)
             deallocate(want)
             deallocate(cmnw)
             deallocate(cenw)
             deallocate(cmew)
             deallocate(ceew)
             deallocate(cmww)
             deallocate(ceww)
             deallocate(cmsw)
             deallocate(cesw)
             deallocate(dpnar)
             deallocate(mpnar)
             deallocate(dpwar)
             deallocate(mpwar)
             deallocate(dpear)
             deallocate(mpear)
             deallocate(dpsar)
             deallocate(mpsar)
             deallocate(rcsnar)
             deallocate(rcswar)
             deallocate(rcsear)
             deallocate(rcssar)
             deallocate(rcsfr)
             deallocate(rcsnw)
             deallocate(rcsww)
             deallocate(rcsew)
             deallocate(rcssw)
             deallocate(dpnw)
             deallocate(mpnw)
             deallocate(dpew)
             deallocate(mpew)
             deallocate(dpsw)
             deallocate(mpsw)
             deallocate(dpfr)
             deallocate(mpfr)
             deallocate(mdrr)
             deallocate(mdr)
             deallocate(mdnwr)
             deallocate(mdwwr)
             deallocate(mdewr)
             deallocate(mdswr)
             deallocate(mnmnw)
             deallocate(mnmww)
             deallocate(mnmew)
             deallocate(mnmsw)
             deallocate(nwsa)
             deallocate(wwsa)
             deallocate(ewsa)
             deallocate(swsa)
             deallocate(nwwe)
             deallocate(nwee)
             deallocate(swwe)
             deallocate(sewe)
             deallocate(nwue)
             deallocate(wwue)
             deallocate(ewue)
             deallocate(swue)
             deallocate(inra)
             deallocate(iera)
             deallocate(iwra)
             deallocate(isra)
             deallocate(irnrw)
             deallocate(irwrw)
             deallocate(irerw)
             deallocate(irsrw)
             deallocate(idnrw)
             deallocate(idwrw)
             deallocate(iderw)
             deallocate(idsrw)
             deallocate(nrwc)
             deallocate(wrwc)
             deallocate(erwc)
             deallocate(srwc)
             deallocate(brapc)
             deallocate(brpc)
             deallocate(nwpc)
             deallocate(wwpc)
             deallocate(ewpc)
             deallocate(swpc)
             deallocate(abc)
             deallocate(aesbb)
             deallocate(esbb)
             deallocate(xysmbh)
             deallocate(bhgrdy)
             deallocate(bhgrdx)
             deallocate(bhdxdy)
             deallocate(bhmap)
             deallocate(slonr)
             deallocate(slanr)
             deallocate(slond)
             deallocate(slatd)
             deallocate(nzstr)
             deallocate(nystr)
             deallocate(nxstr)
             deallocate(vpstr)
             deallocate(upstr)
             deallocate(epsstr2)
             deallocate(mustr2)
             deallocate(epsstr1)
             deallocate(mustr1)
             deallocate(tcstr)
             deallocate(atcstr)
             deallocate(pcns)
             deallocate(cstr)
             deallocate(phds)
             deallocate(wtstr)
             deallocate(arstr)
             deallocate(wstr)
             deallocate(lstr)
             deallocate(bpr)
             deallocate(bpc)
             deallocate(pyb)
             deallocate(pxb)
             deallocate(ellpb)
             deallocate(blonr)
             deallocate(blatr)
             deallocate(blond)
             deallocate(blatd)
          end if
       end subroutine dealloc_um_arrays


       subroutine dealloc_um_arrays_omp_sec(check_alloc)
          !dir$ attributes code_align : 32 :: dealloc_um_arrays_omp_sec
          use urban_model
          use omp_lib
          logical(i4),   intent(in) :: check_alloc
          integer(i4) :: nthds
          nthds = omp_get_num_threads()
          if(nthds<2) return
          
          if(check_alloc) then
!$omp parallel section
!$omp section
             if(allocated(cylo))   deallocate(cylo)
             if(allocated(cant))   deallocate(cant)
             if(allocated(lpda))   deallocate(lpda)
             if(allocated(yant))   deallocate(yant)
             if(allocated(pant))   deallocate(pant)
             if(allocated(want))   deallocate(want)
             if(allocated(cmnw))   deallocate(cmnw)
             if(allocated(cenw))   deallocate(cenw)
             if(allocated(cmew))   deallocate(cmew)
             if(allocated(ceew))   deallocate(ceew)
             if(allocated(cmww))   deallocate(cmww)
             if(allocated(ceww))   deallocate(ceww)
             if(allocated(cmsw))   deallocate(cmsw)
             if(allocated(cesw))   deallocate(cesw)
             if(allocated(dpnar))  deallocate(dpnar)
             if(allocated(mpnar))  deallocate(mpnar)
             if(allocated(dpwar))  deallocate(dpwar)
             if(allocated(mpwar))  deallocate(mpwar)
             if(allocated(dpear))  deallocate(dpear)
             if(allocated(mpear))  deallocate(mpear)
             if(allocated(dpsar))  deallocate(dpsar)
             
!$omp section
             if(allocated(mpsar))  deallocate(mpsar)
             if(allocated(rcsnar)) deallocate(rcsnar)
             if(allocated(rcswar)) deallocate(rcswar)
             if(allocated(rcsear)) deallocate(rcsear)
             if(allocated(rcssar)) deallocate(rcssar)
             if(allocated(rcsfr))  deallocate(rcsfr)
             if(allocated(rcsnw))  deallocate(rcsnw)
             if(allocated(rcsww))  deallocate(rcsww)
             if(allocated(rcsew))  deallocate(rcsew)
             if(allocated(rcssw))  deallocate(rcssw)
             if(allocated(dpnw))   deallocate(dpnw)
             if(allocated(mpnw))   deallocate(mpnw)
             if(allocated(dpew))   deallocate(dpew)
             if(allocated(mpew))   deallocate(mpew)
             if(allocated(dpsw))   deallocate(dpsw)
             if(allocated(mpsw))   deallocate(mpsw)
             if(allocated(dpfr))   deallocate(dpfr)
             if(allocated(mpfr))   deallocate(mpfr)
             if(allocated(mdrr))   deallocate(mdrr)
             if(allocated(mdr))    deallocate(mdr)
             if(allocated(mdnwr))  deallocate(mdnwr)
!$omp section
             if(allocated(mdwwr))  deallocate(mdwwr)
             if(allocated(mdewr))  deallocate(mdewr)
             if(allocated(mdswr))  deallocate(mdswr)
             if(allocated(mnmnw))  deallocate(mnmnw)
             if(allocated(mnmww))  deallocate(mnmww)
             if(allocated(mnmew))  deallocate(mnmew)
             if(allocated(mnmsw))  deallocate(mnmsw)
             if(allocated(nwsa))   deallocate(nwsa)
             if(allocated(wwsa))   deallocate(wwsa)
             if(allocated(ewsa))   deallocate(ewsa)
             if(allocated(swsa))   deallocate(swsa)
             if(allocated(nwwe))   deallocate(nwwe)
             if(allocated(nwee))   deallocate(nwee)
             if(allocated(swwe))   deallocate(swwe)
             if(allocated(sewe))   deallocate(sewe)
             if(allocated(nwue))   deallocate(nwue)
             if(allocated(wwue))   deallocate(wwue)
             if(allocated(ewue))   deallocate(ewue)
             if(allocated(swue))   deallocate(swue)
             if(allocated(inra))   deallocate(inra)
             if(allocated(iera))   deallocate(iera)
!$omp section

             if(allocated(iwra))   deallocate(iwra)
             if(allocated(isra))   deallocate(isra)
             if(allocated(irnrw))  deallocate(irnrw)
             if(allocated(irwrw))  deallocate(irwrw)
             if(allocated(irerw))  deallocate(irerw)
             if(allocated(irsrw))  deallocate(irsrw)
             if(allocated(idnrw))  deallocate(idnrw)
             if(allocated(idwrw))  deallocate(idwrw)
             if(allocated(iderw))  deallocate(iderw)
             if(allocated(idsrw))  deallocate(idsrw)
             if(allocated(nrwc))   deallocate(nrwc)
             if(allocated(wrwc))   deallocate(wrwc)
             if(allocated(erwc))   deallocate(erwc)
             if(allocated(srwc))   deallocate(srwc)
             if(allocated(brapc))  deallocate(brapc)
             if(allocated(brpc))   deallocate(brpc)
             if(allocated(nwpc))   deallocate(nwpc)
             if(allocated(wwpc))   deallocate(wwpc)
             if(allocated(ewpc))   deallocate(ewpc)
             if(allocated(swpc))   deallocate(swpc)
             if(allocated(abc))    deallocate(abc)
!$omp section

             if(allocated(aesbb))  deallocate(aesbb)
             if(allocated(esbb))   deallocate(esbb)
             if(allocated(xysmbh)) deallocate(xysmbh)
             if(allocated(bhgrdy)) deallocate(bhgrdy)
             if(allocated(bhgrdx)) deallocate(bhgrdx)
             if(allocated(bhdxdy)) deallocate(bhdxdy)
             if(allocated(bhmap))  deallocate(bhmap)
             if(allocated(slonr))  deallocate(slonr)
             if(allocated(slanr))  deallocate(slanr)
             if(allocated(slond))  deallocate(slond)
             if(allocated(slatd))  deallocate(slatd)
             if(allocated(nzstr))  deallocate(nzstr)
             if(allocated(nystr))  deallocate(nystr)
             if(allocated(nxstr))  deallocate(nxstr)
             if(allocated(vpstr))  deallocate(vpstr)
             if(allocated(upstr))  deallocate(upstr)
             if(allocated(epsstr2))deallocate(epsstr2)
             if(allocated(mustr2)) deallocate(mustr2)
             if(allocated(epsstr1))deallocate(epsstr1)
             if(allocated(mustr1)) deallocate(mustr1)
             if(allocated(tcstr))  deallocate(tcstr)
!$omp section

             if(allocated(atcstr)) deallocate(atcstr)
             if(allocated(pcns))   deallocate(pcns)
             if(allocated(cstr))   deallocate(cstr)
             if(allocated(phds))   deallocate(phds)
             if(allocated(wtstr))  deallocate(wtstr)
             if(allocated(arstr))  deallocate(arstr)
             if(allocated(wstr))   deallocate(wstr)
             if(allocated(lstr))   deallocate(lstr)
             if(allocated(bpr))    deallocate(bpr)
             if(allocated(bpc))    deallocate(bpc)
             if(allocated(pyb))    deallocate(pyb)
             if(allocated(pxb))    deallocate(pxb)
             if(allocated(ellpb))  deallocate(ellpb)
             if(allocated(blonr))  deallocate(blonr)
             if(allocated(blatr))  deallocate(blatr)
             if(allocated(blond))  deallocate(blond)
             if(allocated(blatd))  deallocate(blatd)
!$omp end parallel sections
          else 
!$omp parallel sections
!$omp section
             deallocate(cylo)
             deallocate(cant)
             deallocate(lpda)
             deallocate(yant)
             deallocate(pant)
             deallocate(want)
             deallocate(cmnw)
             deallocate(cenw)
             deallocate(cmew)
             deallocate(ceew)
             deallocate(cmww)
             deallocate(ceww)
             deallocate(cmsw)
             deallocate(cesw)
             deallocate(dpnar)
             deallocate(mpnar)
             deallocate(dpwar)
             deallocate(mpwar)
             deallocate(dpear)
             deallocate(mpear)
             deallocate(dpsar)
!$omp section
             deallocate(mpsar)
             deallocate(rcsnar)
             deallocate(rcswar)
             deallocate(rcsear)
             deallocate(rcssar)
             deallocate(rcsfr)
             deallocate(rcsnw)
             deallocate(rcsww)
             deallocate(rcsew)
             deallocate(rcssw)
             deallocate(dpnw)
             deallocate(mpnw)
             deallocate(dpew)
             deallocate(mpew)
             deallocate(dpsw)
             deallocate(mpsw)
             deallocate(dpfr)
             deallocate(mpfr)
             deallocate(mdrr)
             deallocate(mdr)
             deallocate(mdnwr)
!$omp section
             deallocate(mdwwr)
             deallocate(mdewr)
             deallocate(mdswr)
             deallocate(mnmnw)
             deallocate(mnmww)
             deallocate(mnmew)
             deallocate(mnmsw)
             deallocate(nwsa)
             deallocate(wwsa)
             deallocate(ewsa)
             deallocate(swsa)
             deallocate(nwwe)
             deallocate(nwee)
             deallocate(swwe)
             deallocate(sewe)
             deallocate(nwue)
             deallocate(wwue)
             deallocate(ewue)
             deallocate(swue)
             deallocate(inra)
             deallocate(iera)
!$omp section
             deallocate(iwra)
             deallocate(isra)
             deallocate(irnrw)
             deallocate(irwrw)
             deallocate(irerw)
             deallocate(irsrw)
             deallocate(idnrw)
             deallocate(idwrw)
             deallocate(iderw)
             deallocate(idsrw)
             deallocate(nrwc)
             deallocate(wrwc)
             deallocate(erwc)
             deallocate(srwc)
             deallocate(brapc)
             deallocate(brpc)
             deallocate(nwpc)
             deallocate(wwpc)
             deallocate(ewpc)
             deallocate(swpc)
             deallocate(abc)
!$omp section
             deallocate(aesbb)
             deallocate(esbb)
             deallocate(xysmbh)
             deallocate(bhgrdy)
             deallocate(bhgrdx)
             deallocate(bhdxdy)
             deallocate(bhmap)
             deallocate(slonr)
             deallocate(slanr)
             deallocate(slond)
             deallocate(slatd)
             deallocate(nzstr)
             deallocate(nystr)
             deallocate(nxstr)
             deallocate(vpstr)
             deallocate(upstr)
             deallocate(epsstr2)
             deallocate(mustr2)
             deallocate(epsstr1)
             deallocate(mustr1)
             deallocate(tcstr)
!$omp section
             deallocate(atcstr)
             deallocate(pcns)
             deallocate(cstr)
             deallocate(phds)
             deallocate(wtstr)
             deallocate(arstr)
             deallocate(wstr)
             deallocate(lstr)
             deallocate(bpr)
             deallocate(bpc)
             deallocate(pyb)
             deallocate(pxb)
             deallocate(ellpb)
             deallocate(blonr)
             deallocate(blatr)
             deallocate(blond)
             deallocate(blatd)
!$omp end parallel sections
          end if
       end subroutine dealloc_um_arrays_omp_sec
       
       ! local helper
       function nidxrw_diff() result(bdif)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  nidxrw_diff
            !dir$ attributes forceinline ::  nidxrw_diff
            logical(i1) :: bdif
            integer(i4), automatic :: diff1,diff2,diff3
            diff1 = nidsrw-niderw
            diff2 = nidwrw-nidnrw
            diff3 = diff1-diff2
            if(diff3==0) then
               bdif=.true.
            else 
               bdif=.false.
            endif
       end function nidxrw_diff
       
       subroutine rand_norm_init_idxrw_unroll4x()
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  rand_norm_init_idxrw_unroll4x
            !dir$ attributes forceinline ::  rand_norm_init_idxrw_unroll4x
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rand_norm_init_idxrw_unroll4x
            use rand_scalar_distributions, only : random_normal_clamped
            use urban_model
            ! Locals
            real(sp), automatic :: r00,r10,r20,r30
            real(sp), automatic :: r01,r11,r21,r31
            real(sp), automatic :: r02,r12,r22,r32
            real(sp), automatic :: r03,r13,r23,r33
            integer(i4), automatic :: j,i,m,m1
            if(nidxrw_diff()) then
               m = mod(nidsrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ assume_aligned iderw:32
                   !dir$ assume_aligned idwrw:32
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                          r10 = random_normal_clamped()
                          iderw(j+0,i) = r10
                          r20 = random_normal_clamped()
                          idwrw(j+0,i) = r20
                          r30 = random_normal_clamped()
                          idnrw(j+0,i) = r30
                      end do
                   end do
                   if(nidsrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ assume_aligned iderw:32
               !dir$ assume_aligned idwrw:32
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,4
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+0,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+0,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+0,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+1,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+1,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+1,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+1,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+2,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+2,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+2,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+2,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+3,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+3,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+3,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+3,i) = r33
                   end do
               end do
            else
               m = mod(nidsrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                      end do
                   end do
                   if(nidsrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,4
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+3,i) = r30
                  end do
               end do
               m = mod(niderw,4)
               if(m/=0) then
                   !dir$ assume_aligned iderw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          iderw(j+0,i) = r00
                      end do
                   end do
                   if(niderw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned iderw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, niderw,4
                     r00 = random_normal_clamped()
                     iderw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+3,i) = r30
                  end do
               end do
               m = mod(nidwrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idwrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idwrw(j+0,i) = r00
                      end do
                   end do
                   if(nidwrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idwrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidwrw,4
                     r00 = random_normal_clamped()
                     idwrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+3,i) = r30
                  end do
               end do
               m = mod(nidnrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idnrw(j+0,i) = r00
                      end do
                   end do
                   if(nidnrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidnrw,4
                     r00 = random_normal_clamped()
                     idnrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+3,i) = r30
                  end do
               end do
            end if 
       end subroutine rand_norm_init_idxrw_unroll4x
       
       
       subroutine rand_norm_init_idxrw_unroll8x()
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  rand_norm_init_idxrw_unroll8x
            !dir$ attributes forceinline ::  rand_norm_init_idxrw_unroll8x
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rand_norm_init_idxrw_unroll8x
            use rand_scalar_distributions, only : random_normal_clamped
            use urban_model
            ! Locals
            real(sp), automatic :: r00,r10,r20,r30
            real(sp), automatic :: r01,r11,r21,r31
            real(sp), automatic :: r02,r12,r22,r32
            real(sp), automatic :: r03,r13,r23,r33
            integer(i4), automatic :: j,i,m,m1
            if(nidxrw_diff()) then
               m = mod(nidsrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ assume_aligned iderw:32
                   !dir$ assume_aligned idwrw:32
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                          r10 = random_normal_clamped()
                          iderw(j+0,i) = r10
                          r20 = random_normal_clamped()
                          idwrw(j+0,i) = r20
                          r30 = random_normal_clamped()
                          idnrw(j+0,i) = r30
                      end do
                   end do
                   if(nidsrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ assume_aligned iderw:32
               !dir$ assume_aligned idwrw:32
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,8
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+0,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+0,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+0,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+1,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+1,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+1,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+1,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+2,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+2,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+2,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+2,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+3,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+3,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+3,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+3,i) = r33
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+4,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+4,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+4,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+5,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+5,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+5,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+5,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+6,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+6,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+6,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+6,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+7,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+7,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+7,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+7,i) = r33
                   end do
               end do
            else
               m = mod(nidsrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                      end do
                   end do
                   if(nidsrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,8
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+7,i) = r30
                  end do
               end do
               m = mod(niderw,8)
               if(m/=0) then
                   !dir$ assume_aligned iderw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          iderw(j+0,i) = r00
                      end do
                   end do
                   if(niderw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned iderw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, niderw,8
                     r00 = random_normal_clamped()
                     iderw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     iderw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+7,i) = r30
                  end do
               end do
               m = mod(nidwrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idwrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idwrw(j+0,i) = r00
                      end do
                   end do
                   if(nidwrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idwrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidwrw,8
                     r00 = random_normal_clamped()
                     idwrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idwrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+7,i) = r30
                  end do
               end do
               m = mod(nidnrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idnrw(j+0,i) = r00
                      end do
                   end do
                   if(nidnrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidnrw,8
                     r00 = random_normal_clamped()
                     idnrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idnrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+7,i) = r30
                  end do
               end do
            end if 
       end subroutine rand_norm_init_idxrw_unroll8x


       subroutine rand_norm_init_idxrw_unroll16x()
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  rand_norm_init_idxrw_unroll16x
            !dir$ attributes forceinline ::  rand_norm_init_idxrw_unroll16x
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rand_norm_init_idxrw_unroll16x
            use rand_scalar_distributions, only : random_normal_clamped
            use urban_model
            ! Locals
            real(sp), automatic :: r00,r10,r20,r30
            real(sp), automatic :: r01,r11,r21,r31
            real(sp), automatic :: r02,r12,r22,r32
            real(sp), automatic :: r03,r13,r23,r33
            integer(i4), automatic :: j,i,m,m1
            if(nidxrw_diff()) then
               m = mod(nidsrw,16)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ assume_aligned iderw:32
                   !dir$ assume_aligned idwrw:32
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                          r10 = random_normal_clamped()
                          iderw(j+0,i) = r10
                          r20 = random_normal_clamped()
                          idwrw(j+0,i) = r20
                          r30 = random_normal_clamped()
                          idnrw(j+0,i) = r30
                      end do
                   end do
                   if(nidsrw<16) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ assume_aligned iderw:32
               !dir$ assume_aligned idwrw:32
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,16
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+0,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+0,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+0,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+1,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+1,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+1,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+1,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+2,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+2,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+2,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+2,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+3,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+3,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+3,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+3,i) = r33
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+4,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+4,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+4,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+5,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+5,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+5,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+5,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+6,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+6,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+6,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+6,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+7,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+7,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+7,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+7,i) = r33
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+8,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+8,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+8,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+8,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+9,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+9,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+9,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+9,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+10,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+10,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+10,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+10,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+11,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+11,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+11,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+11,i) = r33
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+12,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+12,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+12,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+12,i) = r30
                     r01 = random_normal_clamped()
                     idsrw(j+13,i) = r01
                     r11 = random_normal_clamped()
                     iderw(j+13,i) = r11
                     r21 = random_normal_clamped()
                     idwrw(j+13,i) = r21
                     r31 = random_normal_clamped()
                     idnrw(j+13,i) = r31
                     r02 = random_normal_clamped()
                     idsrw(j+14,i) = r02
                     r12 = random_normal_clamped()
                     iderw(j+14,i) = r12
                     r22 = random_normal_clamped()
                     idwrw(j+14,i) = r22
                     r32 = random_normal_clamped()
                     idnrw(j+14,i) = r32
                     r03 = random_normal_clamped()
                     idsrw(j+15,i) = r03
                     r13 = random_normal_clamped()
                     iderw(j+15,i) = r13
                     r23 = random_normal_clamped()
                     idwrw(j+15,i) = r23
                     r33 = random_normal_clamped()
                     idnrw(j+15,i) = r33
                   end do
               end do
            else
               m = mod(nidsrw,16)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idsrw(j+0,i) = r00
                      end do
                   end do
                   if(nidsrw<16) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,16
                     r00 = random_normal_clamped()
                     idsrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+7,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+8,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+9,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+10,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+11,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idsrw(j+12,i) = r00
                     r10 = random_normal_clamped()
                     idsrw(j+13,i) = r10
                     r20 = random_normal_clamped()
                     idsrw(j+14,i) = r20
                     r30 = random_normal_clamped()
                     idsrw(j+15,i) = r30
                  end do
               end do
               m = mod(niderw,16)
               if(m/=0) then
                   !dir$ assume_aligned iderw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          iderw(j+0,i) = r00
                      end do
                   end do
                   if(niderw<16) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned iderw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, niderw,16
                     r00 = random_normal_clamped()
                     iderw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     iderw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+7,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     iderw(j+8,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+9,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+10,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+11,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     iderw(j+12,i) = r00
                     r10 = random_normal_clamped()
                     iderw(j+13,i) = r10
                     r20 = random_normal_clamped()
                     iderw(j+14,i) = r20
                     r30 = random_normal_clamped()
                     iderw(j+15,i) = r30
                  end do
               end do
               m = mod(nidwrw,16)
               if(m/=0) then
                   !dir$ assume_aligned idwrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idwrw(j+0,i) = r00
                      end do
                   end do
                   if(nidwrw<16) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idwrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidwrw,16
                     r00 = random_normal_clamped()
                     idwrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idwrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+7,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idwrw(j+8,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+9,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+10,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+11,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idwrw(j+12,i) = r00
                     r10 = random_normal_clamped()
                     idwrw(j+13,i) = r10
                     r20 = random_normal_clamped()
                     idwrw(j+14,i) = r20
                     r30 = random_normal_clamped()
                     idwrw(j+15,i) = r30
                  end do
               end do
               m = mod(nidnrw,16)
               if(m/=0) then
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_normal_clamped()
                          idnrw(j+0,i) = r00
                      end do
                   end do
                   if(nidnrw<16) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidnrw,16
                     r00 = random_normal_clamped()
                     idnrw(j+0,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+1,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+2,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+3,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idnrw(j+4,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+5,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+6,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+7,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idnrw(j+8,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+9,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+10,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+11,i) = r30
                     ! 
                     r00 = random_normal_clamped()
                     idnrw(j+12,i) = r00
                     r10 = random_normal_clamped()
                     idnrw(j+13,i) = r10
                     r20 = random_normal_clamped()
                     idnrw(j+14,i) = r20
                     r30 = random_normal_clamped()
                     idnrw(j+15,i) = r30
                  end do
               end do
            end if 
       end subroutine rand_norm_init_idxrw_unroll16x
       
!////////////////////////////////////////////////////////////////!


       subroutine rand_gamma2_init_idxrw_unroll4x(s)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  rand_gamma1_init_idxrw_unroll4x
            !dir$ attributes forceinline ::  rand_gamma1_init_idxrw_unroll4x
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rand_gamma1_init_idxrw_unroll4x
            use rand_scalar_distributions, only : random_gamma2
            use urban_model
            real(kind=sp), dimension(4), intent(in) :: s
            
            ! Locals
            real(sp), automatic :: r00,r10,r20,r30
            real(sp), automatic :: r01,r11,r21,r31
            real(sp), automatic :: r02,r12,r22,r32
            real(sp), automatic :: r03,r13,r23,r33
            integer(i4), automatic :: j,i,m,m1
            if(nidxrw_diff()) then
               m = mod(nidsrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ assume_aligned iderw:32
                   !dir$ assume_aligned idwrw:32
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(1))
                          idsrw(j+0,i) = r00
                          r10 = random_gamma2(s(2))
                          iderw(j+0,i) = r10
                          r20 = random_gamma2(s(3))
                          idwrw(j+0,i) = r20
                          r30 = random_gamma2(s(4))
                          idnrw(j+0,i) = r30
                      end do
                   end do
                   if(nidsrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ assume_aligned iderw:32
               !dir$ assume_aligned idwrw:32
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,4
                     r00 = random_gamma2(s(1))
                     idsrw(j+0,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+0,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+0,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+0,i) = r30
                     r01 = random_gamma2(s(1))
                     idsrw(j+1,i) = r01
                     r11 = random_gamma2(s(2))
                     iderw(j+1,i) = r11
                     r21 = random_gamma2(s(3))
                     idwrw(j+1,i) = r21
                     r31 = random_gamma2(s(4))
                     idnrw(j+1,i) = r31
                     r02 = random_gamma2(s(1))
                     idsrw(j+2,i) = r02
                     r12 = random_gamma2(s(2))
                     iderw(j+2,i) = r12
                     r22 = random_gamma2(s(3))
                     idwrw(j+2,i) = r22
                     r32 = random_gamma2(s(4))
                     idnrw(j+2,i) = r32
                     r03 = random_gamma2(s(1))
                     idsrw(j+3,i) = r03
                     r13 = random_gamma2(s(2))
                     iderw(j+3,i) = r13
                     r23 = random_gamma2(s(3))
                     idwrw(j+3,i) = r23
                     r33 = random_gamma2(s(4))
                     idnrw(j+3,i) = r33
                   end do
               end do
            else
               m = mod(nidsrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(1))
                          idsrw(j+0,i) = r00
                      end do
                   end do
                   if(nidsrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,4
                     r00 = random_gamma2(s(1))
                     idsrw(j+0,i) = r00
                     r10 = random_gamma2(s(1))
                     idsrw(j+1,i) = r10
                     r20 = random_gamma2(s(1))
                     idsrw(j+2,i) = r20
                     r30 = random_gamma2(s(1))
                     idsrw(j+3,i) = r30
                  end do
               end do
               m = mod(niderw,4)
               if(m/=0) then
                   !dir$ assume_aligned iderw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(2))
                          iderw(j+0,i) = r00
                      end do
                   end do
                   if(niderw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned iderw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, niderw,4
                     r00 = random_gamma2(s(2))
                     iderw(j+0,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+1,i) = r10
                     r20 = random_gamma2(s(2))
                     iderw(j+2,i) = r20
                     r30 = random_gamma2(s(2))
                     iderw(j+3,i) = r30
                  end do
               end do
               m = mod(nidwrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idwrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(3))
                          idwrw(j+0,i) = r00
                      end do
                   end do
                   if(nidwrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idwrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidwrw,4
                     r00 = random_gamma2(s(3))
                     idwrw(j+0,i) = r00
                     r10 = random_gamma2(s(3))
                     idwrw(j+1,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+2,i) = r20
                     r30 = random_gamma2(s(3))
                     idwrw(j+3,i) = r30
                  end do
               end do
               m = mod(nidnrw,4)
               if(m/=0) then
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(4))
                          idnrw(j+0,i) = r00
                      end do
                   end do
                   if(nidnrw<4) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidnrw,4
                     r00 = random_gamma2(s(4))
                     idnrw(j+0,i) = r00
                     r10 = random_gamma2(s(4))
                     idnrw(j+1,i) = r10
                     r20 = random_gamma2(s(4))
                     idnrw(j+2,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+3,i) = r30
                  end do
               end do
            end if 
       end subroutine rand_gamma2_init_idxrw_unroll4x  
       
       
       subroutine rand_gamma2_init_idxrw_unroll8x(s)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  rand_gamma2_init_idxrw_unroll8x
            !dir$ attributes forceinline ::  rand_gamma2_init_idxrw_unroll8x
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rand_gamma2_init_idxrw_unroll8x
            use rand_scalar_distributions, only : random_gamma2
            use urban_model
            real(kind=sp), dimension(4), intent(in) :: s
          
            ! Locals
            real(sp), automatic :: r00,r10,r20,r30
            real(sp), automatic :: r01,r11,r21,r31
            real(sp), automatic :: r02,r12,r22,r32
            real(sp), automatic :: r03,r13,r23,r33
            integer(i4), automatic :: j,i,m,m1
            if(nidxrw_diff()) then
               m = mod(nidsrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ assume_aligned iderw:32
                   !dir$ assume_aligned idwrw:32
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(1))
                          idsrw(j+0,i) = r00
                          r10 = random_gamma2(s(1))
                          iderw(j+0,i) = r10
                          r20 = random_gamma2(s(1))
                          idwrw(j+0,i) = r20
                          r30 = random_gamma2(s(1))
                          idnrw(j+0,i) = r30
                      end do
                   end do
                   if(nidsrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ assume_aligned iderw:32
               !dir$ assume_aligned idwrw:32
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,8
                     r00 = random_gamma2(s(1))
                     idsrw(j+0,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+0,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+0,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+0,i) = r30
                     r01 = random_gamma2(s(1))
                     idsrw(j+1,i) = r01
                     r11 = random_gamma2(s(2))
                     iderw(j+1,i) = r11
                     r21 = random_gamma2(s(3))
                     idwrw(j+1,i) = r21
                     r31 = random_gamma2(s(4))
                     idnrw(j+1,i) = r31
                     r02 = random_gamma2(s(1))
                     idsrw(j+2,i) = r02
                     r12 = random_gamma2(s(2))
                     iderw(j+2,i) = r12
                     r22 = random_gamma2(s(3))
                     idwrw(j+2,i) = r22
                     r32 = random_gamma2(s(4))
                     idnrw(j+2,i) = r32
                     r03 = random_gamma2(s(1))
                     idsrw(j+3,i) = r03
                     r13 = random_gamma2(s(2))
                     iderw(j+3,i) = r13
                     r23 = random_gamma2(s(3))
                     idwrw(j+3,i) = r23
                     r33 = random_gamma2(s(4))
                     idnrw(j+3,i) = r33
                     ! 
                     r00 = random_gamma2(s(1))
                     idsrw(j+4,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+4,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+4,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+4,i) = r30
                     r01 = random_gamma2(s(1))
                     idsrw(j+5,i) = r01
                     r11 = random_gamma2(s(2))
                     iderw(j+5,i) = r11
                     r21 = random_gamma2(s(3))
                     idwrw(j+5,i) = r21
                     r31 = random_gamma2(s(4))
                     idnrw(j+5,i) = r31
                     r02 = random_gamma2(s(1))
                     idsrw(j+6,i) = r02
                     r12 = random_gamma2(s(2))
                     iderw(j+6,i) = r12
                     r22 = random_gamma2(s(3))
                     idwrw(j+6,i) = r22
                     r32 = random_gamma2(s(4))
                     idnrw(j+6,i) = r32
                     r03 = random_gamma2(s(1))
                     idsrw(j+7,i) = r03
                     r13 = random_gamma2(s(2))
                     iderw(j+7,i) = r13
                     r23 = random_gamma2(s(3))
                     idwrw(j+7,i) = r23
                     r33 = random_gamma2(s(4))
                     idnrw(j+7,i) = r33
                   end do
               end do
            else
               m = mod(nidsrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idsrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(1))
                          idsrw(j+0,i) = r00
                      end do
                   end do
                   if(nidsrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idsrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidsrw,8
                     r00 = random_gamma2(s(1))
                     idsrw(j+0,i) = r00
                     r10 = random_gamma2(s(1))
                     idsrw(j+1,i) = r10
                     r20 = random_gamma2(s(1))
                     idsrw(j+2,i) = r20
                     r30 = random_gamma2(s(1))
                     idsrw(j+3,i) = r30
                     ! 
                     r00 = random_gamma2(s(1))
                     idsrw(j+4,i) = r00
                     r10 = random_gamma2(s(1))
                     idsrw(j+5,i) = r10
                     r20 = random_gamma2(s(1))
                     idsrw(j+6,i) = r20
                     r30 = random_gamma2(s(1))
                     idsrw(j+7,i) = r30
                  end do
               end do
               m = mod(niderw,8)
               if(m/=0) then
                   !dir$ assume_aligned iderw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(2))
                          iderw(j+0,i) = r00
                      end do
                   end do
                   if(niderw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned iderw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, niderw,8
                     r00 = random_gamma2(s(2))
                     iderw(j+0,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+1,i) = r10
                     r20 = random_gamma2(s(2))
                     iderw(j+2,i) = r20
                     r30 = random_gamma2(s(2))
                     iderw(j+3,i) = r30
                     ! 
                     r00 = random_gamma2(s(2))
                     iderw(j+4,i) = r00
                     r10 = random_gamma2(s(2))
                     iderw(j+5,i) = r10
                     r20 = random_gamma2(s(2))
                     iderw(j+6,i) = r20
                     r30 = random_gamma2(s(2))
                     iderw(j+7,i) = r30
                  end do
               end do
               m = mod(nidwrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idwrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(3))
                          idwrw(j+0,i) = r00
                      end do
                   end do
                   if(nidwrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idwrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidwrw,8
                     r00 = random_gamma2(s(3))
                     idwrw(j+0,i) = r00
                     r10 = random_gamma2(s(3))
                     idwrw(j+1,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+2,i) = r20
                     r30 = random_gamma2(s(3))
                     idwrw(j+3,i) = r30
                     ! 
                     r00 = random_gamma2(s(3))
                     idwrw(j+4,i) = r00
                     r10 = random_gamma2(s(3))
                     idwrw(j+5,i) = r10
                     r20 = random_gamma2(s(3))
                     idwrw(j+6,i) = r20
                     r30 = random_gamma2(s(3))
                     idwrw(j+7,i) = r30
                  end do
               end do
               m = mod(nidnrw,8)
               if(m/=0) then
                   !dir$ assume_aligned idnrw:32
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i=1, nbpc
                      do j=1,m
                          r00 = random_gamma2(s(4))
                          idnrw(j+0,i) = r00
                      end do
                   end do
                   if(nidnrw<8) return
               end if
               m1 = m+1
               do i=1, nbpc
               !dir$ assume_aligned idnrw:32
               !dir$ vector aligned
               !dir$ ivdep
               !dir$ vector vectorlength(4)
               !dir$ vector always
                  do j=m1, nidnrw,8
                     r00 = random_gamma2(s(4))
                     idnrw(j+0,i) = r00
                     r10 = random_gamma2(s(4))
                     idnrw(j+1,i) = r10
                     r20 = random_gamma2(s(4))
                     idnrw(j+2,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+3,i) = r30
                     ! 
                     r00 = random_gamma2(s(4))
                     idnrw(j+4,i) = r00
                     r10 = random_gamma2(s(4))
                     idnrw(j+5,i) = r10
                     r20 = random_gamma2(s(4))
                     idnrw(j+6,i) = r20
                     r30 = random_gamma2(s(4))
                     idnrw(j+7,i) = r30
                  end do
               end do
            end if 
       end subroutine rand_gamma2_init_idxrw_unroll8x
     
       





















end module um_diffracted_fields
