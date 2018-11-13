
#include "Config.fpp"

module mod_antennarp

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_antennarp'
 !          
 !          Purpose:
 !                      Antenna radiation pattern as computed by NEC program.
 !          History:
 !                        
 !                        Date: 12-11-2018
 !                        Time: 15:40 GMT+2
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
     use mod_kinds,  only : int1, int4, dp
     implicit none
     
     public :: initNECRadPattern,   &
               readNECRadPattern
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_ANTENNARP_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_ANTENNARP_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_ANTENNARP_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_ANTENNARP_FULLVER = 1000_int4*MOD_ANTENNARP_MAJOR+100_int4*MOD_ANTENNARP_MINOR + &
                                                                     10_int4*MOD_ANTENNARP_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_ANTENNARP_CREATION_DATE = "12-11-2018 15:42 +00200 (MON 12 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_ANTENNARP_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_ANTENNARP_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_ANTENNARP_SYNOPSIS = " Antenna radiation pattern as computed by NEC program."
    
!DIR$ IF .NOT. DEFINED (GMS_ANTENNARP_ADD_PADDING)
    !DIR$ DEFINE GMS_ANTENNARP_ADD_PADDING = 1
!DIR$ ENDIF
    
    type, public :: NECRadPattern_t
        
          sequence
          public
          ! Number of data columns
          integer(kind=int4) :: m_ncols
!DIR$ IF (GMS_ANTENNARP_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad
!DIR$ ENDIF
          ! Angle theta (deg)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_angth
          real(kind=dp), allocatable, dimension(:) :: m_angth
          ! Angle phi (deg)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_angphi
          real(kind=dp), allocatable, dimension(:) :: m_angphi
          ! Cross Section vertical (db)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_csvert
          real(kind=dp), allocatable, dimension(:) :: m_csvert
          ! Cross Section horizontal (db)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_cshor
          real(kind=dp), allocatable, dimension(:) :: m_cshor
          ! Cross Section total (db)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_cstot
          real(kind=dp), allocatable, dimension(:) :: m_cstot
          ! Polarization axial ratio
!DIR$     ATTRIBUTES ALIGN : 64 :: m_polax
          real(kind=dp), allocatable, dimension(:) :: m_polax
          ! Polarization tilt (deg)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_poltilt
          real(kind=dp), allocatable, dimension(:) :: m_poltilt
          ! E-theta magnitude (volts)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Ethmag
          real(kind=dp), allocatable, dimension(:) :: m_Ethmag
          ! E-theta phase (deg)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Ethphi
          real(kind=dp), allocatable, dimension(:) :: m_Ethphi
          ! E-phi magnitude (volts)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Ephimag
          real(kind=dp), allocatable, dimension(:) :: m_Ephimag
          ! E-phi phase (deg)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_Ephiphase
          real(kind=dp), allocatable, dimension(:) :: m_Ephphase
    end type NECRadPattern_t
    
    contains
    
    subroutine initNECRadPattern(rp,ncols,errstate,logging,verbose,append,fname)
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECRadPattern_t),    intent(inout) :: rp
          integer(kind=int4),       intent(in)    :: ncols
          logical(kind=int4),       intent(inout) :: errstate
          logical(kind=int4),       intent(in)    :: logging,   &
                                                     verbose,   &
                                                     append
          character(len=*),         intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code .....
          if(errstate) errstate = .false.
          if(ncols <= 0) then
               call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_antennarp, subroutine: initNECRadPattern: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
               errstate = .true.
               return
          end if
          rp.m_ncols = ncols
          if( allocated(ARRAY=rp.m_angth)) then
              deallocate(rp.m_angth)
              allocate(rp.m_angth(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_angth(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_angphi)) then
              deallocate(rp.m_angphi)
              allocate(rp.m_angphi(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_angphi(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_csvert)) then
              deallocate(rp.m_csvert)
              allocate(rp.m_csvert(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_csvert(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_cshor)) then
              deallocate(rp.m_cshor)
              allocate(rp.m_cshor(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_cshor(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_cstot)) then
              deallocate(rp.m_cstot)
              allocate(rp.m_cstot(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_cstot(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_polax)) then
              deallocate(rp.m_polax)
              allocate(rp.m_polax(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(rp.m_polax(rp.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_poltilt)) then
              deallocate(rp.m_poltilt)
              allocate(rp.m_poltilt(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_poltilt(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_Ethmag)) then
              deallocate(rp.m_Ethmag)
              allocate(rp.m_Ethmag(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ethmag(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_Ethphi)) then
              deallocate(rp.m_Ethphi)
              allocate(rp.m_Ethphi(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ethphi(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_Ephimag)) then
              deallocate(rp.m_Ephimag)
              allocate(rp.m_Ephimag(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ephimag(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(ARRAY=rp.m_Ephphase)) then
              deallocate(rp.m_Ephphase)
              allocate(rp.m_Ephphase(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_Ephphase(rp.m_ncols), &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          rp.m_angth(:)    = INITVAL
          rp.m_angphi(:)   = INITVAL
          rp.m_csvert(:)   = INITVAL
          rp.m_cshor(:)    = INITVAL
          rp.m_cstot(:)    = INITVAL
          rp.m_polax(:)    = INITVAL
          rp.m_poltilt(:)  = INITVAL
          rp.m_Ethmag(:)   = INITVAL
          rp.m_Ethphi(:)   = INITVAL
          rp.m_Ephimag(:)  = INITVAL
          rp.m_Ephphase(:) = INITVAL
          errstate = .false.
          return
9999      call handle_fatal_memory_error( logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_antennarp, subroutine: initNECRadPattern -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_antennarp, subroutine: initNECRadPattern -- Memory Allocation Failure !!", &
                                                    emsg,__LINE__ ) 
    end subroutine initNECRadPattern
    
    subroutine readNECRadPattern(rp,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECRadPattern_t),        intent(inout) :: rp
          integer(kind=int4),           intent(in)    :: iounit
          character(len=*),             intent(in)    :: filename
          character(len=256),           intent(inout) :: errmsg
          integer(kind=int4),           intent(inout) :: ioerr
          logical(kind=int4),           intent(in)    :: logging, &
                                                         verbose, &
                                                         append
          character(len=*),             intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code .....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
               call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_antennarp, subroutine: readNECRadPattern: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_antennarp, subroutine: readNECRadPattern-- File Does Not Exist!! ",  &
                                            errmsg,__LINE__)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_antennarp, subroutine: readNECRadPattern: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_antennarp, subroutine: readNECRadPattern -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          do idx = 0,  rp.m_ncols
              read(iounit,'(11F22.15)',IOSTAT=ioerr,IOMSG=errmsg)     &
                            rp.m_angth(idx),                          &
                            rp.m_angphi(idx),                         &
                            rp.m_csvert(idx),                         &
                            rp.m_cshor(idx),                          &
                            rp.m_cstot(idx),                          &
                            rp.m_polax(idx),                          &
                            rp.m_poltilt(idx),                        &
                            rp.m_Ethmag(idx),                         &
                            rp.m_Ethphi(idx),                         &
                            rp.m_Ephimag(idx),                        &
                            rp.m_Ephphase(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call     handle_fatal_fileio_error( logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_antennarp, subroutine: readNECRadPattern, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_antennarp, subroutine: readNECRadPattern -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
    end subroutine readNECRadPattern
    
    subroutine copyNECRadPattern_ymm4r8(rp,               &
                                        ymm4r8_angth,     &
                                        ymm4r8_angphi,    &
                                        ymm4r8_csvert,    &
                                        ymm4r8_cshor,     &
                                        ymm4r8_cstot,     &
                                        ymm4r8_polax,     &
                                        ymm4r8_poltilt,   &
                                        ymm4r8_Ethmag,    &
                                        ymm4r8_Ethphi,    &
                                        ymm4r8_Ephimag,   &
                                        ymm4r8_Ephphase ,
                                        errstate            )
                                       
          use mod_print_error, only : print_non_fatal_error
          use mod_vectypes,    only : YMM4r8_t
          use mod_copypaos,    only : copy_r8_ymm4r8
          type(NECRadPattern_t),                     intent(in) :: rp
          type(YMM4r8_t), contiguous,  dimension(:), intent(inout) ::  ymm4r8_angth,     &
                                                                       ymm4r8_angphi,    &
                                                                       ymm4r8_csvert,    &
                                                                       ymm4r8_cshor,     &
                                                                       ymm4r8_cstot,     &
                                                                       ymm4r8_polax,     &
                                                                       ymm4r8_poltilt,   &
                                                                       ymm4r8_Ethmag,    &
                                                                       ymm4r8_Ethphi,    &
                                                                       ymm4r8_Ephimag,   &
                                                                       ymm4r8_Ephphase
          logical(kind=int4),                        intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          ! Exec code
          is_conforming  = (size(rp.m_angth)    == 4*size(ymm4r8_angth))   .and.  &
                           (size(rp.m_angphi)   == 4*size(ymm4r8_angphi))  .and.  & 
                           (size(rp.m_csvert)   == 4*size(ymm4r8_csvert))  .and.  &
                           (size(rp.m_cshor)    == 4*size(ymm4r8_cshor))   .and.  &
                           (size(rp.m_cstot)    == 4*size(ymm4r8_cstot))   .and.  &
                           (size(rp.m_polax)    == 4*size(ymm4r8_polax))   .and.  &
                           (size(rp.m_poltilt)  == 4*size(ymm4r8_poltilt)) .and.  &
                           (size(rp.m_Ethmag)   == 4*size(ymm4r8_Ethmag))  .and.  &
                           (size(rp.m_Ethphi)   == 4*size(ymm4r8_Ethphi))  .and.  &
                           (size(rp.m_Ephimag)  == 4*size(ymm4r8_Ephimag)) .and.  &
                           (size(rp.m_Ephphase) == 4*size(ymm4r8_Ephphase))
          if(.not. is_conforming) then
              call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_antennarp, subroutine: copyNECRadPattern_ymm4r8: Nonconforming array(s) detected!! ",  &
                                        __LINE__,__FILE__ )
               errstate = .true.
               return
          end if
          call copy_r8_ymm4r8(ymm4r8_angth,   rp.m_angth)
          call copy_r8_ymm4r8(ymm4r8_angphi,  rp.m_angphi)
          call copy_r8_ymm4r8(ymm4r8_csvert,  rp.m_csvert)
          call copy_r8_ymm4r8(ymm4r8_cshor,   rp.m_cshor)
          call copy_r8_ymm4r8(ymm4r8_cstot,   rp.m_cstot)
          call copy_r8_ymm4r8(ymm4r8_polax,   rp.m_polax)
          call copy_r8_ymm4r8(ymm4r8_poltilt, rp.m_poltilt)
          call copy_r8_ymm4r8(ymm4r8_Ethmag,  rp.m_Ethmag)
          call copy_r8_ymm4r8(ymm4r8_Ethphi,  rp.m_Ethphi)
          call copy_r8_ymm4r8(ymm4r8_Ephimag, rp.m_Ephimag)
          call copy_r8_ymm4r8(ymm4r8_Ephphase,rp.m_Ephphase)
          errstate = .false.
    end subroutine copyNECRadPattern_ymm4r8
                                        
                                        
                                        
                                        
end module mod_antennarp