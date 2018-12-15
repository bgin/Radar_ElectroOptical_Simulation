


module mod_targetrp

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_targetrp'
 !          
 !          Purpose:
 !                      Target radiation pattern as function of angle -- computed by NEC program.
 !          History:
 !                        
 !                        Date: 19-11-2018
 !                        Time: 18:51 GMT+2
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
    use mod_kinds, only : int1, int4, dp
    implicit none
    
    public
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_TARGETRP_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_TARGETRP_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_TARGETRP_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_TARGETRP_FULLVER = 1000_int4*MOD_TARGETRP_MAJOR + &
                                                                    100_int4*MOD_TARGETRP_MINOR  + &
                                                                    10_int4*MOD_TARGETRP_MINOR
    
    ! Module creation date
    character(*),       parameter, public :: MOD_TARGETRP_CREATE_DATE = "19-11-2018 19:18 +00200 (MON 19 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_TARGETRP_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_TARGETRP_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_TARGETRP_SYNOPSIS = "  Target radiation pattern as function of angle -- computed by NEC program."
    
!DIR$ IF .NOT. DEFINED (GMS_TARGETRP_ADD_PADDING)
    !DIR$ DEFINE GMS_TARGETRP_ADD_PADDING = 1
!DIR$ ENDIF
    
        type, public :: NECTargetRadPattern_t
            
              
              ! Number of data columns (per angle)
              integer(kind=int4) :: m_ncols
!DIR$   IF (GMS_TARGETRP_ADD_PADDING .EQ. 1 )
              integer(kind=int1), dimension(0:3) :: pad0
!DIR$   ENDIF
              ! Angle theta
!DIR$         ATTRIBUTES ALIGN : 64 :: m_angth
              real(kind=dp),    allocatable, dimension(:) :: m_angth
              ! Angle phi
!DIR$         ATTRIBUTES ALIGN : 64 :: m_angphi
              real(kind=dp),    allocatable, dimension(:) :: m_angphi
              ! Cross section: vertical (db)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_csvert
              real(kind=dp),    allocatable, dimension(:) :: m_csvert
              ! Cross section: horizontal  (db)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_cshor
              real(kind=dp),    allocatable, dimension(:) :: m_cshor
              ! Cross section total    (db)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_cstot
              real(kind=dp),    allocatable, dimension(:) :: m_cstot
              ! Polarization: axial ratio unitless
!DIR$         ATTRIBUTES ALIGN : 64 :: m_axratio
              real(kind=dp),    allocatable, dimension(:) :: m_axratio
              ! Polarization: tilt (deg)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tilt
              real(kind=dp),    allocatable, dimension(:) :: m_tilt
              ! E-field theta magnitude (volts)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_Ethmag
              real(kind=dp),    allocatable, dimension(:) :: m_Ethmag
              ! E-field theta phase (deg)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_Ethphase
              real(kind=dp),    allocatable, dimension(:) :: m_Ethphase
              ! E-field phi magnitude (volts)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_Ephimag
              real(kind=dp),    allocatable, dimension(:) :: m_Ephimag
              ! E-field phi phase (deg)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_Ephiphase
              real(kind=dp),    allocatable, dimension(:) :: m_Ephiphase
        end type NECTargetRadPattern_t
        
    contains
    
    subroutine initNECTargetRadPattern(rp,ncols,errstate,iounit,logging,verbose,append,fname)
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECTargetRadPattern_t),          intent(inout) :: rp
          integer(kind=int4),                   intent(in)    :: ncols
          logical(kind=int4),                   intent(inout) :: errstate
          integer(kind=int4),                   intent(in)    :: iounit
          logical(kind=int4),                   intent(in)    :: logging,   &
                                                                 verbose,   &
                                                                 append
          character(len=*),                     intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          errstate = .false.
          if(ncols <= 0) then
               call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_targetrp, subroutine: initNECTargetRadPattern: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
               errstate = .true.
               return
          end if
          rp.m_ncols = ncols
          if( allocated(rp.m_angth)) then
              deallocate(rp.m_angth)
              allocate(rp.m_angth(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_angth(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_angphi)) then
              deallocate(rp.m_angphi)
              allocate(rp.m_angphi(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_angphi(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_csvert)) then
              deallocate(rp.m_csvert)
              allocate(rp.m_csvert(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_csvert(rp.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_cshor)) then
              deallocate(rp.m_cshor)
              allocate(rp.m_cshor(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_cshor(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_cstot)) then
              deallocate(rp.m_cstot)
              allocate(rp.m_cstot(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_cstot(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_axratio)) then
              deallocate(rp.m_axratio)
              allocate(rp.m_axratio(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_axratio(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_tilt)) then
              deallocate(rp.m_tilt)
              allocate(rp.m_tilt(rp.m_ncols),    &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_tilt(rp.m_ncols),    &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_Ethmag)) then
              deallocate(rp.m_Ethmag)
              allocate(rp.m_Ethmag(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ethmag(rp.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_Ethphase)) then
              deallocate(rp.m_Ethphase)
              allocate(rp.m_Ethphase(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ethphase(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_Ephimag)) then
              deallocate(rp.m_Ephimag)
              allocate(rp.m_Ephimag(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ephimag(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_Ephiphase)) then
              deallocate(rp.m_Ephiphase)
              allocate(rp.m_Ephiphase(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ephiphase(rp.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          rp.m_angth(:)     = INITVAL
          rp.m_angphi(:)    = INITVAL
          rp.m_csvert(:)    = INITVAL
          rp.m_cshor(:)     = INITVAL
          rp.m_cstot(:)     = INITVAL
          rp.m_axratio(:)   = INITVAL
          rp.m_tilt(:)      = INITVAL
          rp.m_Ethmag(:)    = INITVAL
          rp.m_Ethphase(:)  = INITVAL
          rp.m_Ephimag(:)   = INITVAL
          rp.m_Ephiphase(:) = INITVAL
          return
9999      call  handle_fatal_memory_error(iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_targetrp, subroutine: initNECTargetRadPattern -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_targetrp, subroutine: initNECTargetRadPattern -- Memory Allocation Failure !!", &
                                                    emsg,273 ) 
    end subroutine initNECTargetRadPattern
    
    subroutine readNECTargetRadPattern(rp,iounit,filename,errmsg,ioerr,iounit2,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECTargetRadPattern_t),      intent(inout) :: rp
          integer(kind=int4),               intent(in)    :: iounit
          character(len=*),                 intent(in)    :: filename
          character(len=256),               intent(inout) :: errmsg
          integer(kind=int4),               intent(inout) :: ioerr
          integer(kind=int4),               intent(in)    :: iounit2
          logical(kind=int4),               intent(in)    :: logging,  &
                                                             verbose,  &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
                call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_targetrp, subroutine: readNECTargetRadPattern: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_targetrp, subroutine: readNECTargetRadPattern -- File Does Not Exist!! ",  &
                                            errmsg,296)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                 call handle_fatal_fileio_error(iounit2, logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_targetrp, subroutine: readNECTargetRadPattern: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_targetrp, subroutine: readNECTargetRadPattern -- File Open I/O Failure !! ",  &
                                            errmsg,308)
          end if
          do idx = 1,  rp.m_ncols   
                read(iounit, '(11F22.15)', IOMSG=errmsg,IOSTAT=ioerr)       &
                            rp.m_angth(idx),                                &
                            rp.m_angphi(idx),                               &
                            rp.m_csvert(idx),                               &
                            rp.m_cshor(idx),                                &
                            rp.m_cstot(idx),                                &
                            rp.m_axratio(idx),                              &
                            rp.m_tilt(idx),                                 &
                            rp.m_Ethmag(idx),                               &
                            rp.m_Ethphase(idx),                             &
                            rp.m_Ephimag(idx),                              &
                            rp.m_Ephiphase(idx)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call   handle_fatal_fileio_error( iounit2, logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_targetrp, subroutine: readNECTargetRadPattern, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_targetrp, subroutine: readNECTargetRadPattern -- File READ I/O Failure !! ",  &
                                           errmsg, 333)
    end subroutine readNECTargetRadPattern
    
    subroutine copyNECTargetRadPattern_ymm4r8(rp,      &
                                       ymm4r8_angth,    &
                                       ymm4r8_angphi,   &
                                       ymm4r8_csvert,   &
                                       ymm4r8_cshor,    &
                                       ymm4r8_cstot,    &
                                       ymm4r8_axratio,  &
                                       ymm4r8_tilt,     &
                                       ymm4r8_Ethmag,   &
                                       ymm4r8_Ethphase, &
                                       ymm4r8_Ephimag,  &
                                       ymm4r8_Ephiphase,    &
                                       errstate  )
          use mod_print_error, only : print_non_fatal_error
          use mod_vectypes,    only :  YMM4r8_t
          use mod_copypaos,    only : copy_r8_ymm4r8
          type(NECTargetRadPattern_t),              intent(in)    :: rp
          type(YMM4r8_t), contiguous, dimension(:), intent(inout) :: ymm4r8_angth,    &
                                                                     ymm4r8_angphi,   &
                                                                     ymm4r8_csvert,   &
                                                                     ymm4r8_cshor,    &
                                                                     ymm4r8_cstot,    &
                                                                     ymm4r8_axratio,  &
                                                                     ymm4r8_tilt,     &
                                                                     ymm4r8_Ethmag,   &
                                                                     ymm4r8_Ethphase, &
                                                                     ymm4r8_Ephimag,  &
                                                                     ymm4r8_Ephiphase
          logical(kind=int4),                       intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          ! Exec code ....
          errstate = .false.
          is_conforming = (4*size(rp.m_angth)     == size(ymm4r8_angth))    .and.  &
                          (4*size(rp.m_angphi)    == size(ymm4r8_angphi))   .and.  &
                          (4*size(rp.m_csvert)    == size(ymm4r8_csvert))   .and.  &
                          (4*size(rp.m_cshor)     == size(ymm4r8_cshor))    .and.  &
                          (4*size(rp.m_cstot)     == size(ymm4r8_cstot))    .and.  &
                          (4*size(rp.m_axratio)   == size(ymm4r8_axratio))  .and.  &
                          (4*size(rp.m_tilt)      == size(ymm4r8_tilt))     .and.  &
                          (4*size(rp.m_Ethmag)    == size(ymm4r8_Ethmag))   .and.  &
                          (4*size(rp.m_Ethphase)  == size(ymm4r8_Ethphase)) .and.  &
                          (4*size(rp.m_Ephimag)   == size(ymm4r8_Ephimag))  .and.  &
                          (4*size(rp.m_Ephiphase) == size(ymm4r8_Ephiphase))
          if(.not. is_conforming) then 
                 call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_targetrp, subroutine: copyNECTargetRadPattern_ymm4r8: Nonconforming array(s) detected!! ",  &
                                        __LINE__,__FILE__ )
                 errstate = .true.
                 return
          end if
          call copy_r8_ymm4r8(ymm4r8_angth,    rp.m_angth)
          call copy_r8_ymm4r8(ymm4r8_angphi,   rp.m_angphi)
          call copy_r8_ymm4r8(ymm4r8_csvert,   rp.m_csvert)
          call copy_r8_ymm4r8(ymm4r8_cshor,    rp.m_cshor)
          call copy_r8_ymm4r8(ymm4r8_cstot,    rp.m_cstot)
          call copy_r8_ymm4r8(ymm4r8_axratio,  rp.m_axratio)
          call copy_r8_ymm4r8(ymm4r8_tilt,     rp.m_tilt)
          call copy_r8_ymm4r8(ymm4r8_Ethmag,   rp.m_Ethmag)
          call copy_r8_ymm4r8(ymm4r8_Ethphase, rp.m_Ethphase)
          call copy_r8_ymm4r8(ymm4r8_Ephimag,  rp.m_Ephimag)
          call copy_r8_ymm4r8(ymm4r8_Ephiphase,rp.m_Ephiphase)
          
    end subroutine copyNECTargetRadPattern_ymm4r8

end module mod_targetrp