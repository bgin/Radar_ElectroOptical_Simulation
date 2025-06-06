
#include "GMS_config.fpp"

module mod_wsm6_driver

    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsm6_driver'
 !          
 !          Purpose:
 !                       This is a driver for --> 
 !                       6-class GRAUPEL phase microphyiscs scheme (WSM6) of the 
 !                       Single-Moment MicroPhyiscs (WSMMP)
 !          History:
 !                        Date: 26-05-2018
 !                        Time: 10:26 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 1
 !                      Micro: 0
 !
 !          Author:  
 !                   Song-You Hong and Jeong-Ock Jade Lim (Yonsei Univ.)
 !          Modified:
 !                   Bernard Gingold on 26-05-2018
 !                 
 !          References:
 !         
 !                
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    
    use mod_kinds,   only : i4,sp
    

    use module_mp_wsm6, only : firstTouch,wsm62D,                    &
                               readarray2,readarray3,readarray4,     &
                               writearray2,writearray3,writearray4,  &
                               pi, xlv1,                             &
                               qc0, qck1,                            &
                               bvtr1, bvtr2, bvtr3,                  &
                               bvtr4, bvtr6, g1pbr, g3pbr,           &
                               g4pbr, g6pbr, g5pbro2, pvtr,          &
                               eacrr, pacrr,                         &
                               precr1, precr2, roqimax,              &
                               bvts1, bvts2, bvts3, bvts4,           &
                               g1pbs, g3pbs, g4pbs,                  &
                               g5pbso2, pvts, pacrs, precs1,         & 
                               precs2, pidn0r, pidn0s, pacrc,        &
                               bvtg1, bvtg2, bvtg3, bvtg4,           &
                               g1pbg, g3pbg, g4pbg,                  &
                               pacrg, g5pbgo2, pvtg, precg1,         &
                               precg2, pidn0g ,rslopermax,           &
                               rslopesmax, rslopegmax, rsloperbmax,  &
                               rslopesbmax, rslopegbmax,             &
                               rsloper2max, rslopes2max,             &
                               rslopeg2max, rsloper3max,             &
                               rslopes3max, rslopeg3max

    
    implicit none
    
    public :: wsm6D_driver
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4), parameter, public :: MOD_WSM6_DRIVER_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_WSM6_DRIVER_MINOR = 1
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_WSM6_DRIVER_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_WSM6_DRIVER_FULLVER = 1000*MOD_WSM6_DRIVER_MAJOR + &
                                                                  100*MOD_WSM6_DRIVER_MINOR  + &
                                                                  10*MOD_WSM6_DRIVER_MICRO
    ! Module creation date
    character(*),  parameter, public :: MOD_WSM6_DRIVER_CREATE_DATE = "26-05-2018 10:26 +00200 (SAT 26 MAY 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSM6_DRIVER_BUILD_DATE = __DATE__ 
    
    character(*),  parameter, public :: MOD_WSM6_DRIVER_BUILD_TIME = __TIME__
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSM6_DRIVER_AUTHOR = "Song-You Hong and Jeong-Ock Jade Lim, modified by: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSM6_DRIVER_DESCRIPT = "Driver for -class GRAUPEL phase microphyiscs scheme (WSM6) "
    




    
    contains


    subroutine wsm6D_driver(  ids,ide,jds,jde,kds,kde      &
                             ,ims,ime,jms,jme,kms,kme      &
                             ,its,ite,jts,jte,kts,kte      &
                             ,t,qci,qrs,q,den,p,delz       &
                             ,rain,rainncv,sr,snow         &
                             ,snowncv,graupel,graupelncv)
                                                         
                                    
     

           use ISO_C_BINDING, only : c_size_t
           use omp_lib        

          integer(kind=i4), intent(inout) :: ids,ide,jds,jde,kds,kde,  &
                                               ims,ime,jms,jme,kms,kme,  &
                                               its,ite,jts,jte,kts,kte  
          real(kind=sp), allocatable, dimension(:,:,:),   intent(inout)   :: t 
          real(kind=sp), allocatable, dimension(:,:,:,:), intent(inout)   :: qci
          real(kind=sp), allocatable, dimension(:,:,:,:), intent(inout)   :: qrs
          real(kind=sp), allocatable, dimension(:,:,:),   intent(inout)   :: q
          real(kind=sp), allocatable, dimension(:,:,:),   intent(inout)   :: den
          real(kind=sp), allocatable, dimension(:,:,:),   intent(inout)   :: p
          real(kind=sp), allocatable, dimension(:,:,:),   intent(inout)   :: delz
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: rain
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: rainncv
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: sr
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: snow
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: snowncv
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: graupel
          real(kind=sp), allocatable, dimension(:,:),     intent(inout)   :: graupelncv
          

         
          character(*), parameter :: fcn = "wsm6_constants.dat"
          character(*), parameter :: fin = "wsm6_input.dat"
          character(*), parameter :: fon = "wsm6_output.txt"
          real(kind=sp) :: delt,g,rd,rv,t0c,den0,cpd,cpv,ep1,   &
               ep2,qmin,XLS,XLV0,XLF0,cliq,cice,psat,denr

         

          integer(kind=i4) :: i,j,k,CHUNK,num_tiles_C
          integer(kind=i4) :: ios,unitno
         
          ! So called 'chunk indices'
          integer(kind=i4) :: iids,iide,jjds,jjde, &
                           iims,iime,jjms,jjme, &
                           iits,iite,jjts,jjte
          
          ! Exec code ....
         
          ! Check allocation status, if allocated return immediately
          if( ALLOCATED(t)       .OR.  &
              ALLOCATED(qci)     .OR.  &
              ALLOCATED(qrs)     .OR.  &
              ALLOCATED(q)       .OR.  &
              ALLOCATED(den)     .OR.  &
              ALLOCATED(p)       .OR.  &
              ALLOCATED(delz)    .OR.  &
              ALLOCATED(rain)    .OR.  &
              ALLOCATED(rainncv) .OR.  &
              ALLOCATED(sr)      .OR.  &
              ALLOCATED(snow)    .OR.  &
              ALLOCATED(snowncv) .OR.  &
              ALLOCATED(graupel) .OR.  &
              ALLOCATED(graupelncv)     ) then
                print*, "In File: ", __FILE__," at line: ", __LINE__, " Non-Fatal error: -- allocated array in wsm6D_driver!!"
                return
          end if
          !fn = "wsm6_constants.dat"
          unitno = 31
          open(unitno,file = trim(fcn),form = "unformatted",action = "read", &
               iostat=ios )
          if(ios /= 0) then
              print*, "In File: ", __FILE__, " at line: ",__LINE__, & 
                      " FATAL-ERROR: Failed to open file: ",trim(fcn)
              ERROR STOP "FATAL-ERROR: Failed to open file: "//trim(fcn)
          end if
          read(unitno) pi,xlv1
          read(unitno) qc0,qck1
          read(unitno) bvtr1, bvtr2, bvtr3, bvtr4, bvtr6, g1pbr, g3pbr, &
                       g4pbr, g6pbr, g5pbro2, pvtr, eacrr, pacrr, &
                       precr1, precr2, roqimax
          read(unitno) bvts1, bvts2, bvts3, bvts4, g1pbs, g3pbs, g4pbs,  &
                       g5pbso2, pvts, pacrs, precs1, precs2, pidn0r, pidn0s
          read(unitno) pacrc
          read(unitno) bvtg1, bvtg2, bvtg3, bvtg4, g1pbg, g3pbg, g4pbg,  &
                       pacrg, g5pbgo2, pvtg, precg1, precg2, pidn0g
          read(unitno) rslopermax, rslopesmax, rslopegmax, rsloperbmax,  &
                       rslopesbmax, rslopegbmax, rsloper2max, rslopes2max,  &
                       rslopeg2max, rsloper3max, rslopes3max, rslopeg3max
          close(unitno) 
          ! Read input data
          !fn = "wsm6_input.dat"
          unitno = 31
          open(unitno,file=trim(fin),form="unformatted",action="read", &
               access="stream",iostat=ios )
          if(ios /= 0) then
              print*, "In File: ",__FILE__, "at line: ",__LINE__, &
                      "FATAL-ERROR: Failed to open file: ",trim(fin)
              ERROR STOP "FATAL-ERROR: Failed to open file: "//trim(fin)
          end if
          read(unitno) ids,ide,jds,jde,kds,kde, &
                       ims,ime,jms,jme,kms,kme, &
                       its,ite,jts,jte,kts,kte
          ! Check indices
          if((ims/=its) .OR.   &
             (ime/=ite) .OR.   &
             (jms/=jts) .OR.   &
             (jme/=jte)         ) then
                print*, " In File: ",__FILE__, "at line: ",__LINE__, &
                        " FATAL-ERROR: Index mismatch found in file: ", trim(fcn)
                ERROR STOP " FATAL-ERROR: Index mismatch found in file: "//trim(fcn)
          end if
          if((ims/=1) .OR. (jms/=1)) then
               print*, " In File: ",__FILE__, "at line: ",__LINE__, &
                        " FATAL-ERROR: Incorrect start index found in file: ", trim(fcn)
               ERROR STOP " FATAL-ERROR: Incorrect start index found in file: "//trim(fcn)
          end if
           ! set default values of "chunk" indices
#if 1
          print*, "ids=",ids,"ide=",ide,"jds=",jds,"jde=",jde,"kds=",kds,"kde=",kde 
          print*, "ims=",ims,"ime=",ime,"jms=",jms,"jme=",jme,"kms=",kms,"kme=",kme 
          print*, "its=",its,"ite=",ite,"jts=",jts,"jte=",jte,"kts=",kts,"kte=",kte 
#endif
          iids = ids
          iide = ide
          iims = ims
          iime = ime
          iits = its
          iite = ite
          jjds = jds
          jjde = jde
          jjms = jms
          jjme = jme
          jjts = jts
          jjte = jte
          CHUNK = iite-iits+1
          num_tiles_C = (iite-iits+1) / CHUNK
          if(mod((iite-iits+1),CHUNK) > 0) then
              num_tiles_C = num_tiles_C + 1
          end if
          iime = CHUNK
          iite = CHUNK
          jjme = num_tiles_C
          jjte = num_tiles_C
          !omp_set_num_threads(nthreads)
          ! Array allocation
          allocate(t(iits:iite,kts:kte,jjts:jjte),     &
                   qci(iits:iite,kts:kte,2,jjts:jjte), &
                   qrs(iits:iite,kts:kte,3,jjts:jjte), &
                   q(iims:iime,kms:kme,jjms:jjme),     &
                   den(iims:iime,kms:kme,jjms:jjme),   &
                   p(iims:iime,kms:kme,jjms:jjme),     &
                   delz(iims:iime,kms:kme,jjms:jjme),  &
                   rain(iims:iime,jjms:jjme),          &
                   rainncv(iims:iime,jjms:jjme),       &
                   sr(iims:iime,jjms:jjme),            &
                   snow(iims:iime,jjms:jjme),          &
                   snowncv(iims:iime,jjms:jjme),       &
                   graupel(iims:iime,jjms:jjme),       &
                   graupelncv(iims:iime,jjms:jjme))   

                  
                      
!$OMP PARALLEL DO PRIVATE(j) SCHEDULE(runtime)
          do j = jjts, jjte
              call firstTouch(t(iits,kts,j),     q(iims,kms,j),     &
                              qci(iits,kts,1,j), qrs(iits,kts,1,j), &
                              den(iims,kms,j),                      &
                              p(iims,kms,j),     delz(iims,kms,j),  &
                              j,                                    &
                              rain(iims,j),      rainncv(iims,j),   &
                              sr(iims,j),                           &
                              iids,iide, jjds,jjde, kds,kde,        &
                              iims,iime, jjms,jjme, kms,kme,        &
                              iits,iite, jjts,jjte, kts,kte,        &
                              snow,snowncv,                         &
                              graupel,graupelncv                    )

          end do
!$OMP END PARALLEL DO
           ! read remaining input data
           call readarray3(t,'t',unitno,its,ite)
           call readarray4(qci,'qci',unitno,its,ite)
           call readarray4(qrs,'qrs',unitno,its,ite)
           call readarray3(q,'q',unitno,its,ite)
           call readarray3(den,'den',unitno,its,ite)
           call readarray3(p,'p',unitno,its,ite)
           call readarray3(delz,'delz',unitno,its,ite)
           read(unitno) delt,g,cpd,cpv,t0c,den0,  &
                        rd,rv,ep1,ep2,qmin,XLS,   &
                        XLV0,XLF0,cliq,cice,psat, &
                        denr
                        
 
           call readarray2(rain,'rain',unitno,its,ite)
           call readarray2(rainncv,'rainncv',unitno,its,ite)
           call readarray2(sr,'sr',unitno,its,ite)
           call readarray2(snow,'snow',unitno,its,ite)
           call readarray2(snowncv,'snowncv',unitno,its,ite)
           call readarray2(graupel,'graupel',unitno,its,ite)
           call readarray2(graupelncv,'graupelncv',unitno,its,ite)
           
      

           ! Kernel warmp-up run
!$OMP PARALLEL DO PRIVATE(j) SCHEDULE(runtime)
        do j = jjts, jjte
             
               call wsm62D( t(iits,kts,j),     q(iims,kms,j),      &
                            qci(iits,kts,1,j), qrs(iits,kts,1,j),  &
                            den(iims,kms,j),                       &
                            p(iims,kms,j),     delz(iims,kms,j),   &
                            delt,g,cpd,cpv,rd,rv,t0c,              &
                            ep1,ep2,qmin,                          &
                            XLS,XLV0,XLF0,den0,denr,               &
                            cliq,cice,psat,                        &
                            j,                                     &
                            rain(iims,j),      rainncv(iims,j),    &
                            sr(iims,j),                            &
                            iids,iide, jjds,jjde, kds,kde,         &
                            iims,iims, jjms,jjme, kms,kme,         &
                            iits,iite, jjts,jjte, kts,kte,         &
                            snow,snowncv,                          &
                            graupel,graupelncv                    )
           
           end do
!$OMP END PARALLEL DO           


         unitno=31
         open (unitno,file=trim(fon),form="unformatted",action='write', &
               iostat=ios)
         if (ios /= 0) then
             write(6,*) 'ERROR: failed to open output file ',trim(fon), &
                        ' . stopping'
             stop
         endif
         call writearray3(t,'t',unitno,its,ite)
         call writearray4(qci,'qci',unitno,its,ite)
         call writearray4(qrs,'qrs',unitno,its,ite)
         call writearray3(q,'q',unitno,its,ite)
         call writearray2(rain,'rain',unitno,its,ite)
         call writearray2(rainncv,'rainncv',unitno,its,ite)
         call writearray2(sr,'sr',unitno,its,ite)
         call writearray2(snow,'snow',unitno,its,ite)
         call writearray2(snowncv,'snowncv',unitno,its,ite)
         call writearray2(graupel,'graupel',unitno,its,ite)
         call writearray2(graupelncv,'graupelncv',unitno,its,ite)
         close(unitno)
end subroutine wsm6D_driver
           
  

end module mod_wsm6_driver



