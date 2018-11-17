
#include "Config.fpp"

module mod_surfcurrents


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_excitation'
 !          
 !          Purpose:
 !                      NEC output target surface currents(segmented) and their location.
 !          History:
 !                        
 !                        Date: 17-11-2018
 !                        Time: 14:18 GMT+2
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
    
    public
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_SURFCURRENTS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_SURFCURRENTS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_SURFCURRENTS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_SURFCURRENTS_FULLVER = 1000_int4*MOD_SURFCURRENTS_MAJOR + &
                                                                        100_int4*MOD_SURFCURRENTS_MINOR  + &
                                                                        10_int4*MOD_SURFCURRENTS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SURFCURRENTS_CREATE_DATE = "17-11-2018 14:42 +00200 (SAT 17 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SURFCURRENTS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author infor
    character(*),       parameter, public :: MOD_SURFCURRENTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short info
    character(*),       parameter, public :: MOD_SURFCURRENTS_SYNOPSIS = "NEC output target surface(segmented) currents and their location."
    
!DIR$ IF .NOT. DEFINED (GMS_SURFCURRENTS_ADD_PADDING)
    !DIR$ DEFINE GMS_SURFCURRENTS_ADD_PADDING = 1
!DIR$ ENDIF
    
        type, public :: NECTargetCurrents_t
            
              sequence
              public
              ! Number of data elemts
              integer(kind=int4) :: m_ncols
!DIR$ IF (GMS_SURFCURRENTS_ADD_PADDING .EQ. 1)
              integer(kind=int1), dimension(0:3) :: pad0
!DIR$ ENDIF
              ! Segment number
!DIR$         ATTRIBUTES ALIGN : 64 :: m_segnum
              integer(kind=int4), allocatable, dimension(:) :: m_segnum
              ! Tag number
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tagnum
              integer(kind=int4), allocatable, dimension(:) :: m_tagnum
              ! Segment center x-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xcoord
              real(kind=dp),      allocatable, dimension(:) :: m_xcoord
              ! Segment center y-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_ycoord
              real(kind=dp),      allocatable, dimension(:) :: m_ycoord
              ! Segment center z-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zcoord
              real(kind=dp),      allocatable, dimension(:) :: m_zcoord
              ! Segment length
!DIR$         ATTRIBUTES ALIGN : 64 :: m_seglen
              real(kind=dp),      allocatable, dimension(:) :: m_seglen
              ! Current real part (amps)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_re
              real(kind=dp),      allocatable, dimension(:) :: m_re
              ! Current imag part (amps)
!DIR$         ATTRIBUTES ALIGN : 64 :: m_im
              real(kind=dp),      allocatable, dimension(:) :: m_im
              ! Magnitude
!DIR$         ATTRIBUTES ALIGN : 64 :: m_mag
              real(kind=dp),      allocatable, dimension(:) :: m_mag
              ! Phase
!DIR$         ATTRIBUTES ALIGN : 64 :: m_phase
              real(kind=dp),      allocatable, dimension(:) :: m_phase
              
        end type NECTargetCurrents_t
        
    contains
    
    subroutine initNECTargetCurrents(tc,ncols,errstate,logging,verbose,append,fname)
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECTargetCurrents_t),        intent(inout) :: tc
          integer(kind=int4),               intent(in)    :: ncols
          logical(kind=int4),               intent(inout) :: errstate
          logical(kind=int4),               intent(in)    :: logging,     &
                                                             verbose,     &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          errstate = .false.
          if(ncols <= 0) then
              call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_surfcurrents, subroutine: initNECTargetCurrents: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
              errstate = .true.
              return
          end if
          tc.m_ncols = ncols
          if( allocated(tc.m_segnum)) then
              deallocate(tc.m_segnum)
              allocate(tc.m_segnum(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_segnum(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_tagnum)) then
              deallocate(tc.m_tagnum)
              allocate(tc.m_tagnum(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_tagnum(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_xcoord)) then
              deallocate(tc.m_xcoord)
              allocate(tc.m_xcoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_xcoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_ycoord)) then
              deallocate(tc.m_ycoord)
              allocate(tc.m_ycoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_ycoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_zcoord)) then
              deallocate(tc.m_zcoord)
              allocate(tc.m_zcoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_zcoord(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_seglen)) then
              deallocate(tc.m_seglen)
              allocate(tc.m_seglen(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_seglen(tc.m_ncols),  &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_re)) then
              deallocate(tc.m_re)
              allocate(tc.m_re(tc.m_ncols),      &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_re(tc.m_ncols),      &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_im)) then
              deallocate(tc.m_im)
              allocate(tc.m_im(tc.m_ncols),     &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_im(tc.m_ncols),     &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_mag)) then
              deallocate(tc.m_mag)
              allocate(tc.m_mag(tc.m_ncols),    &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_mag(tc.m_ncols),    &
                        STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_phase)) then
              deallocate(tc.m_phase)
              allocate(tc.m_phase(tc.m_ncols),  &
                         STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_phase(tc.m_ncols),  &
                         STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          tc.m_segnum(:) = -1
          tc.m_tagnum(:) = -1
          tc.m_xcoord(:) = INITVAL
          tc.m_ycoord(:) = INITVAL
          tc.m_zcoord(:) = INITVAL
          tc.m_seglen(:) = INITVAL
          tc.m_re(:)     = INITVAL
          tc.m_im(:)     = INITVAL
          tc.m_mag(:)    = INITVAL
          tc.m_phase(:)  = INITVAL
          return
9999      call handle_fatal_memory_error( logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_surfcurrents, subroutine: initNECTargetCurrents -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_surfcurrents, subroutine: initNECTargetCurrents -- Memory Allocation Failure !!", &
                                                    emsg,__LINE__ ) 
    end subroutine iniNECTargetCurrents
    
    subroutine readNECTargetCurrents(tc,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
           use mod_print_error, only : handle_fatal_fileio_error
           type(NECTargetCurrents_t),           intent(inout) :: tc
           integer(kind=int4),                  intent(in)    :: iounit
           character(len=*),                    intent(in)    :: filename
           character(len=256),                  intent(inout) :: errmsg
           integer(kind=int4),                  intent(inout) :: ioerr
           logical(kind=int4),                  intent(in)    :: logging,   &
                                                                 verbose,   &
                                                                 append
           character(len=*),                    intent(in)    :: fname
           ! Locals
           logical(kind=int4) :: is_present = .false.
           integer(kind=int4) :: idx
           ! Exec code ....
           inquire(FILE=trim(filename),EXIST=is_present)
           if(.not. is_present) then
               call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfcurrents, subroutine: readNECTargetCurrents: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_surfcurrents, subroutine: readNECTargetCurrents-- File Does Not Exist!! ",  &
                                            errmsg,__LINE__)
           end if
           open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
           if(ioerr > 0) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfcurrents, subroutine: readNECTargetCurrents: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_surfcurrents, subroutine: readNECTargetCurrents -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
           end if
           do idx = 1,  tc.m_ncols
               read(iounit,'(i6,i6,8F22.15)',IOMSG=errmsg,IOSTAT=ioerr)     &
                            tc.m_segnum(idx),                               &
                            tc.m_tagnum(idx),                               &
                            tc.m_xcoord(idx),                               &
                            tc.m_ycoord(idx),                               &
                            tc.m_zcoord(idx),                               &
                            tc.m_seglen(idx),                               &
                            tc.m_re(idx),                                   &
                            tc.m_im(idx),                                   &
                            tc.m_mag(idx),                                  &
                            tc.m_phase(idx)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end if
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       close(UNIT=iounit,STATUS='KEEP')
           call     handle_fatal_fileio_error( logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_surfcurrents, subroutine: readNECTargetCurrents, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_surfcurrents, subroutine: readNECTargetCurrents -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
           
    end subroutine readNECTargetCurrents
    
    subroutine copyNECTargetCurrents_ymm8i4_ymm4r8(tc,              &
                                                   ymm8i4_segnum,   &
                                                   ymm8i4_tagnum,   &
                                                   ymm4r8_xcoord,   &
                                                   ymm4r8_ycoord,   &
                                                   ymm4r8_zcoord,   &
                                                   ymm4r8_seglen,   &
                                                   ymm4c8_signal,   &
                                                   ymm4r8_mag,      &
                                                   ymm4r8_phase,    &
                                                   errstate  )
          use mod_print_error, only : print_non_fatal_error
          use mod_vectypes,    only : YMM8i4_t, YMM4r8_t, YMM4c8_t
          use mod_copypaos,    only : copy_r8_ymm4r8,   &
                                      copy_i4_ymm8i4,   &
                                      copy_r8_ymm4c8
          type(NECTargetCurrents_t),        intent(in) :: tc
          type(YMM8i4_t),  contiguous, dimension(:), intent(inout) :: ymm8i4_segnum,  &
                                                                      ymm8i4_tagnum
          type(YMM4r8_t),  contiguous, dimension(:), intent(inout) :: ymm4r8_xcoord,  &
                                                                      ymm4r8_ycoord,  &
                                                                      ymm4r8_zcoord,  &
                                                                      ymm4r8_seglen
          type(YMM4c8_t),  contiguous, dimension(:), intent(inout) :: ymm4c8_signal
          type(YMM4r8_t),  contiguous, dimension(:), intent(inout) :: ymm4r8_mag,     &
                                                                      ymm4r8_phase
          logical(kind=int4),                        intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          is_conforming = (8*size(tc.m_segnum) == size(ymm8i4_segnum)) .and.   &
                          (8*size(tc.m_tagnum) == size(ymm8i4_tagnum)) .and.   &
                          (4*size(tc.m_xcoord) == size(ymm4r8_xcoord)) .and.   &
                          (4*size(tc.m_ycoord) == size(ymm4r8_ycoord)) .and.   &
                          (4*size(tc.m_zcoord) == size(ymm4r8_zcoord)) .and.   &
                          (4*size(tc.m_seglen) == size(ymm4r8_seglen)) .and.   &
                          (4*size(tc.m_re)     == size(ymm4c8_signal)) .and.   &
                          (4*size(tc.m_im)     == size(ymm4c8_signal)) .and.   &
                          (4*size(tc.m_mag)    == size(ymm4r8_mag))    .and.   &
                          (4*size(tc.m_phase)  == size(ymm4r8_phase))
          if(.not. is_conforming) then
                 call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_surfcurrents, subroutine: copyNECTargetCurrents_ymm8i4_ymm4r8: Nonconforming array(s) detected!! ",  &
                                        __LINE__,__FILE__ )
                 errstate = .true.
                 return
          end if
          
          call copy_i4_ymm8i4(ymm8i4_segnum, tc.m_segnum)
          call copy_i4_ymm8i4(ymm8i4_tagnum, tc.m_tagnum)
          call copy_r8_ymm4r8(ymm4r8_xcoord, tc.m_xcoord)
          call copy_r8_ymm4r8(ymm4r8_ycoord, tc.m_ycoord)
          call copy_r8_ymm4r8(ymm4r8_zcoord, tc.m_zcoord)
          call copy_r8_ymm4r8(ymm4r8_seglen, tc.m_seglen)
          call copy_r8_ymm4c8(ymm4r8_seglen, tc.m_re,tc.m_im)
          call copy_r8_ymm4r8(ymm4r8_mag,    tc.m_mag)
          call copy_r8_ymm4r8(ymm4r8_phase,  tc.m_phase)
          
    end subroutine copyNECTargetCurrents_ymm8i4_ymm4r8
                                                   
                                                   
end module surfcurrents