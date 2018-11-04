
#include "Config.fpp"

module mod_necsp

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_necsp'
 !          
 !          Purpose:
 !                     NEC Surface Patch (SP) target mesh geometry representation.
 !
 !          History:
 !                        
 !                        Date: 03-11-2018
 !                        Time: 10:42 GMT+2
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
    use mod_kinds, int1, int4, dp
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_NECSP_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_NECSP_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_NECSP_MICRO = 0_int4
    
    ! Module ful version
    integer(kind=int4), parameter, public :: MOD_NECSP_FULLVER = 1000_int4*MOD_NECSP_MAJOR+100_int4*MOD_NECSP_MINOR+ &
                                                                 10_int4*MOD_NECSP_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_NECSP_CREATE_DATE =   "03-11-2018 11:09 +00200 (SAT 03 NOV 2018 GMT+2) "
    
    ! Module build date
    character(*),       parameter, public :: MOD_NECSP_BUILD_DATE = " 00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_NECSP_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_NECSP_SYNOPSIS = "NEC Surface Patch (SP) target mesh geometry representation. "
    
    real(kind=dp), parameter, private :: INITVAL = HUGE(1.0E+00_dp)
    
!DIR$   IF .NOT. DEFINED (GMS_NECSP_ADD_PADDING)
    !DIR$ DEFINE GMS_NECSP_ADD_PADDING = 1
!DIR$ ENDIF
    
    type, public :: NECParamPatches_t
        
          
          SEQUENCE
          
          integer(kind=int4), public :: m_nspatches
!DIR$ IF (GMS_NECSP_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad1
!DIR$ ENDIF
          ! Shapeless patch type (always 0)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_sptype
          integer(kind=int4), allocatable, dimension(:), public :: m_sptype
          ! x coordinate of the patch center
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcoord
          real(kind=dp),      allocatable, dimension(:), public :: m_xcoord
          ! y coordinate of the patch center
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycoord
          real(kind=dp),      allocatable, dimension(:), public :: m_ycoord
          ! z coordinate of the patch center
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcoord
          real(kind=dp),      allocatable, dimension(:), public :: m_zcoord
          ! elevation angle of the outward normal vector above the x-y plane (degrees).
!DIR$     ATTRIBUTES ALIGN : 64 :: m_elevang
          real(kind=dp),      allocatable, dimension(:), public :: m_elevang
          ! azimuth angle of the outward normal vector from the x axis (degrees). 
!DIR$     ATTRIBUTES ALIGN : 64 :: m_azimang
          real(kind=dp),      allocatable, dimension(:), public :: m_azimang
          !patch area (square of units used)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_parea
          real(kind=dp),      allocatable, dimension(:), public :: m_parea
    end type NECParamPatches_t
    
    type, public :: NECRectangPatches_t
        
          SEQUENCE
          
          integer(kind=int4), public :: m_nrpatches
!DIR$ IF (GMS_NECSP_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad1
!DIR$ ENDIF
          !   rectangular patch  type (always 1)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_rptype
          integer(kind=int4), allocatable, dimension(:), public :: m_rptype
          !   x coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner1
          !   y coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner1
          !  z coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner1
          !  x coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner2
          !  y coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner2
          ! z  coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner2
          !  x coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner3
          !  y coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner3
          !  z coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner3
    end type NECRectangPatches_t
    
    type, public :: NECTriangPatches_t
        
          SEQUENCE
          
          integer(kind=int4), public :: m_ntpatches
!DIR$ IF (GMS_NECSP_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad1
!DIR$ ENDIF
          ! Traingular patch type (always 2)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_tptype
          integer(kind=int4), allocatable, dimension(:), public :: m_tptype
          !   x coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner1
          !   y coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner1
          !  z coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner1
          !  x coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner2
          !  y coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner2
          ! z  coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner2
          !  x coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner3
          !  y coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner3
          !  z coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner3         
    end type  NECTriangPatches_t
    
    type, public :: NECQuadPatches_t 
        
          SEQUENCE
          integer(kind=int4), public :: m_nqpatches
!DIR$ IF (GMS_NECSP_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad1
!DIR$ ENDIF
          ! Quadrilateral patch type (always 3)
!DIR$     ATTRIBUTES ALIGN : 64 :: m_qptype
          integer(kind=int4), allocatable, dimension(:), public :: m_qptype
            !   x coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner1
          !   y coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner1
          !  z coordinate of corner 1
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner1
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner1
          !  x coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner2
          !  y coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner2
          ! z  coordinate of corner 2
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner2
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner2
          !  x coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner3
          !  y coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner3
          !  z coordinate of corner 3
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner3
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner3  
          ! x coordinate of corner 4
!DIR$     ATTRIBUTES ALIGN : 64 :: m_xcorner4
          real(kind=dp),      allocatable, dimension(:), public :: m_xcorner4
          ! y coordinate of corner 4
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ycorner4
          real(kind=dp),      allocatable, dimension(:), public :: m_ycorner4
          ! z coordinate of corner 4
!DIR$     ATTRIBUTES ALIGN : 64 :: m_zcorner4
          real(kind=dp),      allocatable, dimension(:), public :: m_zcorner4
    end type NECQuadPatches_t
    
    contains
    
    subroutine initNECParamPatches(pp,nsp,errstate,logging,verbose,append,fname)
          use mod_print_error, only : print_non_fatal_error,  &
                                      handle_fatal_memory_error
          type(NECParamPatches_t),           intent(inout) :: pp
          integer(kind=int4),                intent(in)    :: nsp
          logical(kind=int4),                intent(inout) :: errstate
          logical(kind=int4),                intent(in)    :: logging,verbose, &
                                                              append
          character(len=*),                  intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          if(errstate) errstate = .false.
          if(nsp <= 0) then
              call print_non_fatal_error( "================ Non-Fatal ===================", &
                                          "Module: mod_necsp, subroutine: initNECParamPatches: -- nsp <= 0 !!", &
                                          __LINE__,__FILE__)
              errstate = .true.
              return
          end if
          pp.m_nspatches = nsp
          if( allocated(pp.m_sptype)) then
              deallocate(pp.m_sptype)
              allocate(pp.m_sptype(pp.m_nspatches), &
                                   STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_sptype(pp.m_nspatches), &
                                    STAT=aerr, ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_xcoord)) then
              deallocate(pp.m_xcoord)
              allocate(pp.m_xcoord(pp.m_nspatches), &
                                    STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
               allocate(pp.m_xcoord(pp.m_nspatches), &
                                    STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_ycoord)) then
              deallocate(pp.m_ycoord)
              allocate(pp.m_ycoord(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_ycoord(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_zcoord)) then
              deallocate(pp.m_zcoord)
              allocate(pp.m_zcoord(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_zcoord(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_elevang)) then
              deallocate(pp.m_elevang)
              allocate(pp.m_elevang(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_elevang(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_azimang)) then
              deallocate(pp.m_azimang)
              allocate(pp.m_azimang(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_azimang(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(pp.m_parea)) then
              deallocate(pp.m_parea)
              allocate(pp.m_parea(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(pp.m_parea(pp.m_nspatches), &
                                     STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          pp.m_sptype(:)  =  -1
          pp.m_xcoord(:)  =  0.0E+00_dp
          pp.m_ycoord(:)  =  0.0E+00_dp
          pp.m_zcoord(:)  =  0.0E+00_dp
          pp.m_elevang(:) =  0.0E+00_dp
          pp.m_azimang(:) =  0.0E+00_dp
          pp.m_parea(:)   = -1.0E+00_dp
          errstate = .false.
          return
9999      call handle_fatal_memory_error( logging,verbose,append,fname, &
                        "logger: "// __FILE__ // " Module: mod_necsp, sub: initNECParamPatches: -- Memory Allocation Failure!!!", &
                        "Module: mod_necsp, sub: initNECParamPatches: -- Memory Allocation Failure!!!",  &
                                                    emsg,__LINE__)
    end subroutine initNECParamPatches
    
    subroutine readNECParamPatches(pp,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECParamPatches_t),      intent(in)    :: pp
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
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECParamPatches: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                                            "module: mod_necsp, subroutine: read_NECParamPatches -- File Does Not Exist!! ",  &
                                            errmsg,__LINE__)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
              call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECParamPatches: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                                            "module: mod_necsp, subroutine: read_NECParamPatches -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          do idx = 1, pp.m_nspatches
              
              read(iounit,'(i6,6F22.15)',IOSTAT=ioerr,IOMSG=errmsg)      &
                           pp.m_sptype(idx),  pp.m_xcoord(idx),          &
                           pp.m_ycoord(idx),  pp.m_zcoord(idx),          &
                           pp.m_elevang(idx), pp.m_azimang(idx),         &
                           pp.m_parea(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
              
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call handle_fatal_fileio_error( logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_necsp, subroutine: read_NECParamPatches, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                                          "module: mod_necsp, subroutine: read_NECParamPatches -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
    end subroutine readNECParamPatches
    
    subroutine initNECRectangPatches(rp,nrp,errstate,logging,verbose,append,fname)
          use mod_print_error, only : print_non_fatal_error,  &
                                      handle_fatal_memory_error
          type(NECRectangPatches_t),        intent(inout) :: rp
          integer(kind=int4),               intent(in)    :: nrp
          logical(kind=int4),               intent(in)    :: errstate
          logical(kind=int4),               intent(in)    :: logging,  &
                                                             verbose,  &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          if(errstate) errstate = .false.
          if(nrp <= 0) then
              call print_non_fatal_error(  "================ Non-Fatal ===================", &
                                          "Module: mod_necsp, subroutine: initNECRectangPatches: -- nrp <= 0 !!", &
                                          __LINE__,__FILE__)
              errstate = .true.
              return
          end if
          rp.m_nrpatches = nrp
          if( allocated(rp.m_rptype)) then
              deallocate(rp.m_rptype)
              allocate(rp.m_rptype(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_rptype(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_xcorner1)) then
              deallocate(rp.m_xcorner1)
              allocate(rp.m_xcorner1(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_xcorner1(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_ycorner1)) then
              deallocate(rp.m_ycorner1)
              allocate(rp.m_ycorner1(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_ycorner1(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_zcorner1)) then
              deallocate(rp.m_zcorner1)
              allocate(rp.m_zcorner1(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_zcorner1(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
           if( allocated(rp.m_xcorner2)) then
              deallocate(rp.m_xcorner2)
              allocate(rp.m_xcorner2(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_xcorner2(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_ycorner2)) then
              deallocate(rp.m_ycorner)
              allocate(rp.m_ycorner2(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_ycorner2(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_zcorner2)) then
              deallocate(rp.m_zcorner2)
              allocate(rp.m_zcorner2(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_zcorner2(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
           if( allocated(rp.m_xcorner3)) then
              deallocate(rp.m_xcorner3)
              allocate(rp.m_xcorner3(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_xcorner3(rp.m_nrpatches), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_ycorner3)) then
              deallocate(rp.m_ycorner3)
              allocate(rp.m_ycorner3(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_ycorner3(rp.m_nrpatches), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(rp.m_zcorner3)) then
              deallocate(rp.m_zcorner3)
              allocate(rp.m_zcorner3(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(rp.m_zcorner3(rp.m_nrpatches), &
                              STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          rp.m_rptype(:)   = -1
          rp.m_xcorner1(:) = INITVAL
          rp.m_ycorner1(:) = INITVAL
          rp.m_zcorner1(:) = INITVAL
          rp.m_xcorner2(:) = INITVAL
          rp.m_ycorner2(:) = INITVAL
          rp.m_zcorner2(:) = INITVAL
          rp.m_xcorner3(:) = INITVAL
          rp.m_ycorner3(:) = INITVAL
          rp.m_zcorner3(:) = INITVAL
          errstate = .false.
          return
9999      call handle_fatal_memory_error(  logging,verbose,append,fname, &
                        "logger: "// __FILE__ // " Module: mod_necsp, sub: initNECRectangPatches: -- Memory Allocation Failure!!!", &
                        "Module: mod_necsp, sub: initNECRectangPatches: -- Memory Allocation Failure!!!",  &
                                                    emsg,__LINE__) 
    end subroutine initNECRectangPatches
    
    subroutine readNECRectangPatches(rp,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECRectangPatches_t),        intent(inout) :: rp
          integer(kind=int4),               intent(in)    :: iounit
          character(len=*),                 intent(in)    :: filename
          character(len=256),               intent(inout) :: errmsg
          integer(kind=int4),               intent(inout) :: ioerr
          logical(kind=int4),               intent(in)    :: logging,   &
                                                             verbose,   &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
              call handle_fatal_fileio_error( logging,verbose,append,fname,   &
                                         "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECRectangPatches: File"//filename//" Does Not Exist!!", &
                                          filename,                        &
                                         "module: mod_necsp, subroutine: read_NECRectangPatches -- File Does Not Exist!! ",  &
                                        errmsg,__LINE__         )
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
               call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECRectangPatches: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                                            "module: mod_necsp, subroutine: read_NECRectangPatches -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          do idx = 1, rp.m_nrpatches
              read(iounit,'(i6,9F22.15)',IOSTAT=ioerr,IOMSG=errmsg)   &
                            rp.m_rptype(idx),   rp.m_xcorner1(idx),   &
                            rp.m_ycorner1(idx), rp.m_zcorner1(idx),   &
                            rp.m_xcorner2(idx), rp.m_ycorner2(idx),   &
                            rp.m_zcorner2(idx), rp.m_xcorner3(idx),   &
                            rp.m_ycorner3(idx), rp.m_zcorner3(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
          end if
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call  handle_fatal_fileio_error( logging,verbose,append,fname,    &
                                "logger:"//__FILE__ //"module: mod_necsp, subroutine: read_NECRectangPatches, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                                "module: mod_necsp, subroutine: read_NECRectangPatches -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__) 
    end subroutine readNECRectangPatches
    
    subroutine initNECTriangPatches(tp,ntp,errstate,logging,verbose,append,fname)
          use mod_print_error, only : print_non_fatal_error,    &
                                      handle_fatal_memory_error
          type(NECTriangPatches_t),         intent(inout) :: tp
          integer(kind=int4),               intent(in)    :: ntp
          logical(kind=int4),               intent(inout) :: errstate
          logical(kind=int4),               intent(in)    :: logging,   &
                                                             verbose,   &
                                                             append
          character(len=*),                 intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          if(errstate) errstate = .false.
          if(ntp <= 0) then
              call print_non_fatal_error( "================= Non-Fatal ==================",  &
                                           "Module: mod_necsp, subroutine: initNECTriangPatches: -- ntp <= 0 !!", &
                                          __LINE__,__FILE__)
              errstate = .true.
              return
          end if
          tp.m_ntpatches = ntp
          if( allocated(tp.m_tptype)) then
              deallocate(tp.m_tptype)
              allocate(tp.m_tptype(tp.m_ntpatches),  &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_tptype(tp.m_ntpatches),  &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_xcorner1)) then
              deallocate(tp.m_xcorner1)
              allocate(tp.m_xcorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_xcorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_ycorner1)) then
              deallocate(tp.m_ycorner1)
              allocate(tp.m_ycorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_ycorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_zcorner1)) then
              deallocate(tp.m_zcorner1)
              allocate(tp.m_zcorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_zcorner1(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
           if( allocated(tp.m_xcorner2)) then
              deallocate(tp.m_xcorner2)
              allocate(tp.m_xcorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_xcorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_ycorner2)) then
              deallocate(tp.m_ycorner2)
              allocate(tp.m_ycorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_ycorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_zcorner2)) then
              deallocate(tp.m_zcorner2)
              allocate(tp.m_zcorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_zcorner2(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_xcorner3)) then
              deallocate(tp.m_xcorner3)
              allocate(tp.m_xcorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_xcorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_ycorner3)) then
              deallocate(tp.m_ycorner3)
              allocate(tp.m_ycorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_ycorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(tp.m_zcorner3)) then
              deallocate(tp.m_zcorner3)
              allocate(tp.m_zcorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(tp.m_zcorner3(tp.m_ntpatches), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          tp.m_tptype(:)   = -1
          tp.m_xcorner1(:) = INITVAL
          tp.m_ycorner1(:) = INITVAL
          tp.m_zcorner1(:) = INITVAL
          tp.m_xcorner2(:) = INITVAL
          tp.m_ycorner2(:) = INITVAL
          tp.m_zcorner2(:) = INITVAL
          tp.m_xcorner3(:) = INITVAL
          tp.m_ycorner3(:) = INITVAL
          tp.m_zcorner3(:) = INITVAL
          errstate = .false.
          return
9999      call handle_fatal_memory_error(  logging,verbose,append,fname, &
                        "logger: "// __FILE__ // " Module: mod_necsp, sub: initNECTriangPatches: -- Memory Allocation Failure!!!", &
                        "Module: mod_necsp, sub: initNECTriangPatches: -- Memory Allocation Failure!!!",  &
                                                    emsg,__LINE__) 
    end subroutine initNECTriangPatches
    
    subroutine readNECTriangPatches(tp,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error,   only : handle_fatal_fileio_error
          type(NECTriangPatches_t),      intent(inout) :: tp
          integer(kind=int4),            intent(in)    :: iounit
          character(len=*),              intent(in)    :: filename
          character(len=256),            intent(inout) :: errmsg
          integer(kind=int4),            intent(inout) :: ioerr
          logical(kind=int4),            intent(in)    :: logging,   &
                                                          verbose,   &
                                                          append
          character(len=*),              intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
                 call handle_fatal_fileio_error( logging,verbose,append,fname,   &
                            "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECTriangPatches: File"//filename//" Does Not Exist!!", &
                                          filename,                        &
                            "module: mod_necsp, subroutine: read_NECTriangPatches -- File Does Not Exist!! ",  &
                                        errmsg,__LINE__         )
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECTriangPatches: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_necsp, subroutine: read_NECTriangPatches -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__     )
          end if
          do idx = 1, tp.m_ntpatches
              read(iounit,'(i6,9F22.15)',IOSTAT=ioerr,IOMSG=errmsg)       &
                           tp.m_tptype(idx),    tp.m_xcorner1(idx),       &
                           tp.m_ycorner1(idx),  tp.m_zcorner1(idx),       &
                           tp.m_xcorner2(idx),  tp.m_ycorner2(idx),       &
                           tp.m_zcorner2(idx),  tp.m_xcorner3(idx),       &
                           tp.m_ycorner3(idx),  tp.m_zcorner3(idx)
              if(ierr > 0 .or. ierr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call  handle_fatal_fileio_error( logging,verbose,append,fname,    &
                                "logger:"//__FILE__ //"module: mod_necsp, subroutine: read_NECTriangPatches, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                                "module: mod_necsp, subroutine: read_NECTriangPatches -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__) 
    end subroutine readNECTriangPatches
    
    subroutine initNECQuadPatches(qp,nqp,errstate,logging,verbose,append,fname)
           use mod_print_error, only : print_non_fatal_error,    &
                                       handle_fatal_memory_error
           type(NECQuadPatches_t),          intent(inout) :: qp
           integer(kind=int4),              intent(in)    :: nqp
           logical(kind=int4),              intent(inout) :: errstate
           logical(kind=int4),              intent(in)    :: logging,   &
                                                             verbose,   &
                                                             append
           character(len=*),                intent(in)    :: fname
           ! Locals
           integer(kind=int4) :: aerr
           character(len=256) :: emsg
           ! Exec code
           if(errstate) errstate = .false.
           if(nqp <= 0) then
               call print_non_fatal_error( "================= Non-Fatal ==================",  &
                                           "Module: mod_necsp, subroutine: initNECQuadPatches: -- nqp <= 0 !!", &
                                          __LINE__,__FILE__)
               errstate = .true.
               return
           end if
           qp.m_nqpatches = nqp
           if( allocated(qp.m_qptype)) then
               deallocate(qp.m_qptype)
               allocate(qp.m_qptype(qp.m_nqpatches),  &
                                STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_qptype(qp.m_nqpatches),  &
                                STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_xcorner1)) then
               deallocate(qp.m_xcorner1)
               allocate(qp.m_xcorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_xcorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_ycorner1)) then
               deallocate(qp.m_ycorner1)
               allocate(qp.m_ycorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_ycorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_zcorner1)) then
               deallocate(qp.m_zcorner1)
               allocate(qp.m_zcorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_zcorner1(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
            if( allocated(qp.m_xcorner2)) then
               deallocate(qp.m_xcorner2)
               allocate(qp.m_xcorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_xcorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_ycorner2)) then
               deallocate(qp.m_ycorner2)
               allocate(qp.m_ycorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_ycorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_zcorner2)) then
               deallocate(qp.m_zcorner2)
               allocate(qp.m_zcorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_zcorner2(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
            if( allocated(qp.m_xcorner3)) then
               deallocate(qp.m_xcorner3)
               allocate(qp.m_xcorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_xcorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_ycorner3)) then
               deallocate(qp.m_ycorner3)
               allocate(qp.m_ycorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_ycorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_zcorner3)) then
               deallocate(qp.m_zcorner3)
               allocate(qp.m_zcorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_zcorner3(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
             if( allocated(qp.m_xcorner4)) then
               deallocate(qp.m_xcorner4)
               allocate(qp.m_xcorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_xcorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_ycorner4)) then
               deallocate(qp.m_ycorner4)
               allocate(qp.m_ycorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_ycorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           if( allocated(qp.m_zcorner4)) then
               deallocate(qp.m_zcorner4)
               allocate(qp.m_zcorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           else
               allocate(qp.m_zcorner4(qp.m_nqpatches), &
                                 STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
           end if
           ! Memory first touch
           qp.m_qptype(:)   = -1
           qp.m_xcorner1(:) = INITVAL
           qp.m_ycorner1(:) = INITVAL
           qp.m_zcorner1(:) = INITVAL
           qp.m_xcorner2(:) = INITVAL
           qp.m_ycorner2(:) = INITVAL
           qp.m_zcorner2(:) = INITVAL
           qp.m_xcorner3(:) = INITVAL
           qp.m_ycorner3(:) = INITVAL
           qp.m_zcorner3(:) = INITVAL
           qp.m_xcorner4(:) = INITVAL
           qp.m_ycorner4(:) = INITVAL
           qp.m_zcorner4(:) = INITVAL
           errstate = .false.
           return
9999      call handle_fatal_memory_error(  logging,verbose,append,fname, &
                        "logger: "// __FILE__ // " Module: mod_necsp, sub: initNECQuadPatches: -- Memory Allocation Failure!!!", &
                        "Module: mod_necsp, sub: initNECQuadPatches: -- Memory Allocation Failure!!!",  &
                                                    emsg,__LINE__) 
    end subroutine initNECQuadPatches
    
    subroutine readNECQuadPatches(qp,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error  
          type(NECQuadPatches_t),       intent(inout) :: qp
          integer(kind=int4),           intent(in)    :: iounit
          character(len=*),             intent(in)    :: filename
          character(len=256),           intent(inout) :: errmsg
          integer(kind=int4),           intent(inout) :: ioerr
          logical(kind=int4),           intent(in)    :: logging,   &
                                                         verbose,   &
                                                         append
          character(len=*),             intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ...
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
               call handle_fatal_fileio_error( logging,verbose,append,fname,   &
                            "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECQuadPatches: File"//filename//" Does Not Exist!!", &
                                          filename,                        &
                            "module: mod_necsp, subroutine: read_NECTriangPatches -- File Does Not Exist!! ",  &
                                        errmsg,__LINE__         )
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                call handle_fatal_fileio_error( logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_necsp, subroutine: read_NECQuadPatches: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_necsp, subroutine: read_NECQuadPatches -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__     )
          end if
          do idx = 1, qp.m_nqpatches
              read(iounit,'(i6,12F22.15)',IOSTAT=ioerr,IOMSG=errmsg)   &
                            qp.m_qptype(idx),   qp.m_xcorner1(idx),    &
                            qp.m_ycorner1(idx), qp.m_zcorner1(idx),    &
                            qp.m_xcorner2(idx), qp.m_ycorner2(idx),    &
                            qp.m_zcorner2(idx), qp.m_xcorner3(idx),    &
                            qp.m_ycorner3(idx), qp.m_zcorner3(idx),    &
                            qp.m_xcorner4(idx), qp.m_ycorner4(idx),    &
                            qp.m_zcorner4(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
         end do
         close(UNIT=iounit,STATUS='KEEP')
         return
         close(UNIT=iounit,STATUS='KEEP')
9999     call  handle_fatal_fileio_error( logging,verbose,append,fname,    &
                                "logger:"//__FILE__ //"module: mod_necsp, subroutine: read_NECQuadPatches, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                                "module: mod_necsp, subroutine: read_NECQuadPatches -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__) 
    end subroutine readNECQuadPatches
    
    
    

end module mod_necsp