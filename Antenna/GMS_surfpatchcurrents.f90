


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
    use mod_kinds,  only : i1, i4, sp
    implicit none
    
    public
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MOD_SURFPATCHCURRENTS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_SURFPATCHCURRENTS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_SURFPATCHCURRENTS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_SURFPATCHCURRENTS_FULLVER = 1000*MOD_SURFPATCHCURRENTS_MAJOR + &
                                                                             100*MOD_SURFPATCHCURRENTS_MINOR  + &
                                                                             10*MOD_SURFPATCHCURRENTS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_CREATE_DATE = "18-11-2018 11:18 +00200 (SUN 18 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_SURFPATCHCURRENTS_SYNOPSIS = " NEC output target surface currents(surface patch) and their location."
    
!DIR$ IF .NOT. DEFINED (GMS_SURFPATCHCURRENTS_ADD_PADDING)
    !DIR$ DEFINE GMS_SURFPATCHCURRENTS_ADD_PADDING = 1
!DIR$ ENDIF
    
        type, public :: NECTargetSPCurrents_t
            
             
              ! Number of data columns
              integer(kind=i4) :: m_ncols
!DIR$   IF (GMS_SURFPATCHCURRENTS_ADD_PADDING .EQ. 1)
              integer(kind=i1), dimension(0:3) :: pad0
!DIR$   ENDIF
              ! Patch center x-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xcoord
              real(kind=sp),    allocatable, dimension(:) :: m_xcoord
              ! Patch center y-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_ycoord
              real(kind=sp),    allocatable, dimension(:) :: m_ycoord
              ! Patch center z-coordinate
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zcoord
              real(kind=sp),    allocatable, dimension(:) :: m_zcoord
              ! Tangent vector 1 magnitude
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv1mag
              real(kind=sp),    allocatable, dimension(:) :: m_tv1mag
              ! Tangent vector 1 phase
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv1phase
              real(kind=sp),    allocatable, dimension(:) :: m_tv1phase
              ! Tangent vector 2 magnitude
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv2mag
              real(kind=sp),    allocatable, dimension(:) :: m_tv2mag
              ! Tangent vector 2 phase
!DIR$         ATTRIBUTES ALIGN : 64 :: m_tv2phase
              real(kind=sp),    allocatable, dimension(:) :: m_tv2phase
              ! Rectangular components: x-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xre
              real(kind=sp),    allocatable, dimension(:) :: m_xre
              ! Rectangular components: x-imaginary part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_xim
              real(kind=sp),    allocatable, dimension(:) :: m_xim
              ! Rectangular components: y-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_yre
              real(kind=sp),    allocatable, dimension(:) :: m_yre
              ! Rectangular components: y-imaginary part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_yim
              real(kind=sp),    allocatable, dimension(:) :: m_yim
              ! Rectangular components: z-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zre
              real(kind=sp),    allocatable, dimension(:) :: m_zre
              ! Rectangular components: z-real part
!DIR$         ATTRIBUTES ALIGN : 64 :: m_zim              
              real(kind=sp),    allocatable, dimension(:) :: m_zim
        end type NECTargetSPCurrents_t
        
    contains
    
    subroutine initNECTargetSPCurrents(tc,ncols,errstate,iounit,logging,verbose,append,fname)
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECTargetSPCurrents_t),       intent(inout) :: tc
          integer(kind=i4),                intent(in)    :: ncols
          logical(kind=i4),                intent(inout) :: errstate
          integer(kind=i4),                intent(in)    :: iounit
          logical(kind=i4),                intent(in)    :: logging,  &
                                                              verbose,  &
                                                              append
          character(*),                      intent(in)    :: fname
          ! Locals
          integer(kind=i4) :: aerr
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
9999      call  handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_surfpatchcurrents, subroutine: initNECTargetSPCurrents -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_surfpatchcurrents, subroutine: initNECTargetSPCurrents -- Memory Allocation Failure !!", &
                                                    emsg,304) 
    end subroutine initNECTargetSPCurrents
    
    subroutine readNECTargetSPCurrents(tc,iounit,filename,errmsg,ioerr,iounit2,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECTargetSPCurrents_t),      intent(inout) :: tc
          integer(kind=i4),               intent(inout)    :: iounit
          character(len=*),                 intent(inout)    :: filename
          character(len=256),               intent(inout) :: errmsg
          integer(kind=i4),               intent(inout)    :: ioerr
          integer(kind=i4),               intent(in)       :: iounit2
          logical(kind=i4),               intent(in)    :: logging,   &
                                                             verbose,   &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=i4) :: idx
          logical(kind=i4) :: is_present = .false.
          ! Exec code .....
          inquire(FILE=trim(filename), EXIST=is_present)
          if(.not. is_present) then
                call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents-- File Does Not Exist!! ",  &
                                            errmsg,329)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                 call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents -- File Open I/O Failure !! ",  &
                                            errmsg,337)
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
          call   handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_surfpatchcurrents, subroutine: readNECTargetSPCurrents -- File READ I/O Failure !! ",  &
                                           errmsg,363)
    end subroutine readNECTargetSPCurrents
    
    subroutine copyNECTargetSPCurrents_ymm8r4_ymm8c4(tc,              &
                                                     ymm8r4_xcoord,   &
                                                     ymm8r4_ycoord,   &
                                                     ymm8r4_zcoord,   &
                                                     ymm8r4_tv1mag,   &
                                                     ymm8r4_tv1phase, &
                                                     ymm8r4_tv2mag,   &
                                                     ymm8r4_tv2phase, &
                                                     ymm8c4_xcomp,    &
                                                     ymm8c4_ycomp,    &
                                                     ymm8c4_zcomp,    &
                                                     errstate       )
          use mod_print_error, only : print_non_fatal_error
          use mod_vectypes,    only :  YMM8r4_t, YMM8c4_t
          use mod_copypaos,    only : copy_r4_ymm8r4,   &
                                      copy_r4_ymm8c4
          type(NECTargetSPCurrents_t),                intent(in)    :: tc
          type(YMM8r4_t),   contiguous, dimension(:), intent(inout) :: ymm8r4_xcoord,   &
                                                                       ymm8r4_ycoord,   &
                                                                       ymm8r4_zcoord,   &
                                                                       ymm8r4_tv1mag,   &
                                                                       ymm8r4_tv1phase, &
                                                                       ymm8r4_tv2mag,   &
                                                                       ymm8r4_tv2phase
          type(YMM8c4_t),   contiguous, dimension(:), intent(inout)   :: ymm8c4_xcomp,  &
                                                                      ymm8c4_ycomp,  &
                                                                      ymm8c4_zcomp
          logical(kind=i4),                         intent(inout) :: errstate
          ! Locals
          logical(kind=i4) :: is_conforming = .false.
          ! Exec code ....
          errstate = .false.
          is_conforming = (8*size(ymm8r4_xcoord)   == size(tc.m_xcoord))   .and.   &
                          (8*size(ymm8r4_ycoord)   == size(tc.m_ycoord))   .and.   &
                          (8*size(ymm8r4_zcoord)   == size(tc.m_zcoord))   .and.   &
                          (8*size(ymm8r4_tv1mag)   == size(tc.m_tv1mag))   .and.   &
                          (8*size(ymm8r4_tv1phase) == size(tc.m_tv1phase)) .and.   &
                          (8*size(ymm8r4_tv2mag)   == size(tc.m_tv2mag))   .and.   &
                          (8*size(ymm8r4_tv2phase) == size(tc.m_tv2phase)) .and.   &
                          (8*size(ymm8c4_xcomp)    == size(tc.m_xre))      .and.   &
                          (8*size(ymm8c4_xcomp)    == size(tc.m_xim))      .and.   &
                          (8*size(ymm8c4_ycomp)    == size(tc.m_yre))      .and.   &
                          (8*size(ymm8c4_ycomp)    == size(tc.m_yim))      .and.   &
                          (8*size(ymm8c4_zcomp)    == size(tc.m_zre))      .and.   &
                          (8*size(ymm8c4_zcomp)    == size(tc.m_zim)) 
          if(.not. is_conforming) then
                 call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                          " Module: mod_surfpatchcurrents, subroutine: copyNECTargetSPCurrents_ymm8i4_ymm4r8: Nonconforming array(s) detected!! ",  &
                                        __LINE__,__FILE__ )
                 errstate = .true.
                 return
          end if
          call copy_r4_ymm8r4(ymm8r4_xcoord, tc.m_xcoord)
          call copy_r4_ymm8r4(ymm8r4_ycoord, tc.m_ycoord)
          call copy_r4_ymm8r4(ymm8r4_zcoord, tc.m_zcoord)
          call copy_r4_ymm8r4(ymm8r4_tv1mag, tc.m_tv1mag)
          call copy_r4_ymm8r4(ymm8r4_tv1phase, tc.m_tv1phase)
          call copy_r4_ymm8r4(ymm8r4_tv2mag, tc.m_tv2mag)
          call copy_r4_ymm8r4(ymm8r4_tv2phase, tc.m_tv2phase)
          call copy_r4_ymm8c4(ymm4c8_xcomp, tc.m_xre, tc.m_xim)
          call copy_r4_ymm8c4(ymm4c8_ycomp, tc.m_yre, tc.m_yim)
          call copy_r4_ymm8c4(ymm4c8_zcomp, tc.m_zre, tc.m_zim)
          
    end subroutine copyNECTargetSPCurrents_ymm8r4_ymm8c4
    
end module mod_surfpatchcurrents
