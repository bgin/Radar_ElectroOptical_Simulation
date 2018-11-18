
#inlude "Config.fpp"

module mod_surfpatchcurrents


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_surfpatchcurrents'
 !          
 !          Purpose:
 !                      NEC output target surface currents(surface patch) and their location.
 !          History:
 !                        
 !                        Date: 18-11-2018
 !                        Time: 11:18 GMT+2
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
    integer(kind=int4), parameter, public :: MOD_SURFPATCHCURRENTS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_SURFPATCHCURRENTS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_SURFPATCHCURRENTS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_SURFPATCHCURRENTS_FULLVER = 1000_int4*MOD_SURFPATCHCURRENTS_MAJOR + &
                                                                             100_int4*MOD_SURFPATCHCURRENTS_MINOR  + &
                                                                             10_int4*MOD_SURFPATCHCURRENTS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_CREATE_DATE = "18-11-2018 11:18 +00200 (SUN 18 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_SYNOPSIS = " NEC output target surface currents(surface patch) and their location."
    
!DIR$ IF .NOT. DEFINED (GMS_SURFPATCHCURRENTS_ADD_PADDING)
    !DIR$ DEFINE GMS_SURFPATCHCURRENTS_ADD_PADDING = 1
!DIR$ ENDIF
    
        type, public :: NECTargetSPCurrents_t
            
              sequence
              public
              ! Number of data columns
              integer(kind=int4) :: m_ncols
!DIR$   IF (GMS_SURFPATCHCURRENTS_ADD_PADDING .EQ. 1)
              integer(kind=int1), dimension(0:3) :: pad0
!DIR$   ENDIF
              ! Patch center x-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xcoord
              real(kind=dp),    allocatable, dimension(:) :: m_xcoord
              ! Patch center y-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_ycoord
              real(kind=dp),    allocatable, dimension(:) :: m_ycoord
              ! Patch center z-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zcoord
              real(kind=dp),    allocatable, dimension(:) :: m_zcoord
              ! Tangent vector 1 magnitude
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv1mag
              real(kind=dp),    allocatable, dimension(:) :: m_tv1mag
              ! Tangent vector 1 phase
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv1phase
              real(kind=dp),    allocatable, dimension(:) :: m_tv1phase
              ! Tangent vector 2 magnitude
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv2mag
              real(kind=dp),    allocatable, dimension(:) :: m_tv2mag
              ! Tangent vector 2 phase
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv2phase
              real(kind=dp),    allocatable, dimension(:) :: m_tv2phase
              ! Rectangular components: x-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xre
              real(kind=dp),    allocatable, dimension(:) :: m_xre
              ! Rectangular components: x-imaginary part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xim
              real(kind=dp),    allocatable, dimension(:) :: m_xim
              ! Rectangular components: y-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_yre
              real(kind=dp),    allocatable, dimension(:) :: m_yre
              ! Rectangular components: y-imaginary part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_yim
              real(kind=dp),    allocatable, dimension(:) :: m_yim
              ! Rectangular components: z-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zre
              real(kind=dp),    allocatable, dimension(:) :: m_zre
              ! Rectangular components: z-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zim              
              real(kind=dp),    allocatable, dimension(:) :: m_zim
        end type NECTargetSPCurrents_t
        
    contains
    
    subroutine initNECTargetSPCurrents(tc,ncols,errstate,logging,verbose,append,fname)
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECTargetSPCurrents_t),       intent(inout) :: tc
          integer(kind=int4),                intent(in)    :: ncols
          logical(kind=int4),                intent(inout) :: errstate
          logical(kind=int4),                intent(in)    :: logging,  &
                                                              verbose,  &
                                                              append
          character(*),                      intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code .....
          errstate = .false.
          if(ncols <= 0) then
               call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_surfpatchcurrents, subroutine: initNECTargetSPCurrents: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
               errstate = .true.
               return
          end if
          tc.m_ncols = ncols
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
          if( allocated(tc.m_tv1mag)) then
              deallocate(tc.m_tv1mag)
              allocate(tc.m_tv1mag(tc.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_tv1mag(tc.m_ncols),  &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_tv1phase)) then
              deallocate(tc.m_tv1phase)
              allocate(tc.m_tv1phase(tc.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_tv1phase(tc.m_ncols), &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_tv2mag)) then
              deallocate(tc.m_tv2mag)
              allocate(tc.m_tv2mag(tc.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_tv2mag(tc.m_ncols),   &
                            STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_tv2phase)) then
              deallocate(tc.m_tv2phase)
              allocate(tc.m_tv2phase(tc.m_ncols), &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tc.m_tv2phase(tc.m_ncols), &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_xre)) then
              deallocate(tc.m_xre)
              allocate(tc.m_xre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_xre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_xim)) then
              deallocate(tc.m_xim)
              allocate(tc.m_xim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_xim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
         if( allocated(tc.m_yre)) then
              deallocate(tc.m_yre)
              allocate(tc.m_yre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_yre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_yim)) then
              deallocate(tc.m_yim)
              allocate(tc.m_yim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_yim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_zre)) then
              deallocate(tc.m_zre)
              allocate(tc.m_zre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_zre(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tc.m_zim)) then
              deallocate(tc.m_zim)
              allocate(tc.m_zim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else 
              allocate(tc.m_zim(tc.m_ncols),      &
                             STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          tc.m_xcoord(:)    = INITVAL
          tc.m_ycoord(:)    = INITVAL
          tc.m_zcoord(:)    = INITVAL
          tc.m_tv1mag(:)    = INITVAL
          tc.m_tv1phase(:)  = INITVAL
          tc.m_tv2mag(:)    = INITVAL
          tc.m_tv2phase(:)  = INITVAL
          tc.m_xre(:)       = INITVAL
          tc.m_xim(:)       = INITVAL
          tc.m_yre(:)       = INITVAL
          tc.m_yim(:)       = INITVAL
          tc.m_zre(:)       = INITVAL
          tc.m_zim(:)       = INITVAL
          return
9999      call  handle_fatal_memory_error( logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_surfpatchcurrents, subroutine: initNECTargetSPCurrents -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_surfpatchcurrents, subroutine: initNECTargetSPCurrents -- Memory Allocation Failure !!", &
                                                    emsg,__LINE__ ) 
    end subroutine initNECTargetSPCurrents
    
    subroutine readNECTargetSPCurrents(tc,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECTargetSPCurrents_t),      intent(inout) :: tc
          integer(kind=int4),               intent(in)    :: iounit
          character(len=*),                 intent(in)    :: filename
          character(len=256),               intent(inout) :: errmsg
          integer(kind=int4),               intent(in)    :: ioerr
          logical(kind=int4),               intent(in)    :: logging,   &
                                                             verbose,   &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code .....
          inquire(FILE=trim(filename), EXIST=is_present)
          if(.not. is_present) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents-- File Does Not Exist!! ",  &
                                            errmsg,__LINE__)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                 call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          do idx = 1,  tc.m_ncols
              read(iounit,'(13F22.15)',IOMSG=errmsg,IOSTAT=ioerr)    &
                            tc.m_xcoord(idx),                        &
                            tc.m_ycoord(idx),                        &
                            tc.m_zcoord(idx),                        &
                            tc.m_tv1mag(idx),                        &
                            tc.m_tv1phase(idx),                      &
                            tc.m_tv2mag(idx),                        &
                            tc.m_tv2phase(idx),                      &
                            tc.m_xre(idx),                           &
                            tc.m_xim(idx),                           &
                            tc.m_yre(idx),                           &
                            tc.m_yim(idx),                           &
                            tc.m_zre(idx),                           &
                            tc.m_zim(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call   handle_fatal_fileio_error( logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
    end subroutine readNECTargetSPCurrents
    
    subroutine copyNECTargetSPCurrents_ymm4r8_ymm4c8(tc,              &
                                                     ymm4r8_xcoord,   &
                                                     ymm4r8_ycoord,   &
                                                     ymm4r8_zcoord,   &
                                                     ymm4r8_tv1mag,   &
                                                     ymm4r8_tv1phase, &
                                                     ymm4r8_tv2mag,   &
                                                     ymm4r8_tv2phase, &
                                                     ymm4c8_xcomp,    &
                                                     ymm4c8_ycomp,    &
                                                     ymm4c8_zcomp,    &
                                                     errstate       )
          use mod_print_error, only : print_non_fatal_error
          use mod_vectypes,    only :  YMM4r8_t, YMM4c8_t
          use mod_copypaos,    only : copy_r8_ymm4r8,   &
                                      copy_r8_ymm4c8
          type(NECTargetSPCurrents_t),                intent(in)    :: tc
          type(YMM4r8_t),   contiguous, dimension(:), intent(inout) :: ymm4r8_xcoord,   &
                                                                       ymm4r8_ycoord,   &
                                                                       ymm4r8_zcoord,   &
                                                                       ymm4r8_tv1mag,   &
                                                                       ymm4r8_tv1phase, &
                                                                       ymm4r8_tv2mag,   &
                                                                       ymm4r8_tv2phase
          type(YMM4c8_t),   contiguous, dimension(:), intent(in)   :: ymm4c8_xcomp,  &
                                                                      ymm4c8_ycomp,  &
                                                                      ymm4c8_zcomp
          logical(kind=int4),                         intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          ! Exec code ....
          errstate = .false.
          is_conforming = (4*size(ymm4r8_xcoord)   == size(tc.m_xcoord))   .and.   &
                          (4*size(ymm4r8_ycoord)   == size(tc.m_ycoord))   .and.   &
                          (4*size(ymm4r8_zcoord)   == size(tc.m_zcoord))   .and.   &
                          (4*size(ymm4r8_tv1mag)   == size(tc.m_tv1mag))   .and.   &
                          (4*size(ymm4r8_tv1phase) == size(tc.m_tv1phase)) .and.   &
                          (4*size(ymm4r8_tv2mag)   == size(tc.m_tv2mag))   .and.   &
                          (4*size(ymm4r8_tv2phase) == size(tc.m_tv2phase)) .and.   &
                          (4*size(ymm4c8_xcomp)    == size(tc.m_xre))      .and.   &
                          (4*size(ymm4r8_xcomp)    == size(tc.m_xim))      .and.   &
                          (4*size(ymm4r8_ycomp)    == size(tc.m_yre))      .and.   &
                          (4*size(ymm4r8_ycomp)    == size(tc.m_yim))      .and.   &
                          (4*size(ymm4r8_zcomp)    == size(tc.m_zre))      .and.   &
                          (4*size(ymm4r8_zcomp)    == size(tc.m_zim)) 
          if(.not. is_conforming) then
                 call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_surfpatchcurrents, subroutine: copyNECTargetSPCurrents_ymm8i4_ymm4r8: Nonconforming array(s) detected!! ",  &
                                        __LINE__,__FILE__ )
                 errstate = .true.
                 return
          end if
          call copy_r8_ymm4r8(ymm4r8_xcoord, tc.m_xcoord)
          call copy_r8_ymm4r8(ymm4r8_ycoord, tc.m_ycoord)
          call copy_r8_ymm4r8(ymm4r8_zcoord, tc.m_zcoord)
          call copy_r8_ymm4r8(ymm4r8_tv1mag, tc.m_tv1mag)
          call copy_r8_ymm4r8(ymm4r8_tv1phase, tc.m_tv1phase)
          call copy_r8_ymm4r8(ymm4r8_tv2mag, tc.m_tv2mag)
          call copy_r8_ymm4r8(ymm4r8_tv2phase, tc.m_tv2phase)
          call copy_r8_ymm4c8(ymm4c8_xcomp, tc.m_xre, tc.m_xim)
          call copy_r8_ymm4c8(ymm4c8_ycomp, tc.m_yre, tc.m_yim)
          call copy_r8_ymm4c8(ymm4c8_zcomp, tc.m_zre, tc.m_zim)
          
    end subroutine copyNECTargetSPCurrents
    
end module mod_surfpatchcurrents