


module mod_segdataout

   !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_segdataout'
 !          
 !          Purpose:
 !                     NEC segementation data output for target mesh geometry representation.
 !                     This data will be used to represent distrubution of segmentation surface target currents
 !          History:
 !                        
 !                        Date: 05-11-2018
  !                       Time: 15:37 GMT+2
  !                       Modified on
  !                       Date: 10-12-2019
  !                       Time: 16:42 GMT+2
  !
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
    use mod_kinds, only : int1, int4, sp
    
    implicit none
    
    public ::   initNECWireDataOut,    &
                readNECWireDataOut,    &
                initNECSegmentDataOut, &
                readNECSegmentDataOut
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_SEGDATAOUT_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_SEGDATAOUT_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_SEGDATAOUT_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_SEGDATAOUT_FULLVER = 1000_int4*MOD_SEGDATAOUT_MAJOR+100_int4*MOD_SEGDATAOUT_MINOR+ &
                                                                      10_int4*MOD_SEGDATAOUT_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SEGDATAOUT_CREATION_DATE = "05-11-2018 15:39 +00200 (MON 05 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SEGDATAOUT_BUILD_DATE =  __DATE__ " " __TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_SEGDATAOUT_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_SEGDATAOUT_SYNOPSIS = "NEC segmentation data output for target mesh geometry representation." 
    

#if !defined(GMS_SEGDATAOUT_ADD_PADDING)
#define GMS_SEGDATAOUT_ADD_PADDING 1
#endif
    
    type, public :: NECWireDataOut_t
        
          sequence
          ! Number of data columns
          integer(kind=int4)  :: m_ncols
#if (GMS_SEGDATAOUT_ADD_PADDING) == 1
          integer(kind=int1), dimension(0:3) :: pad
#endif
          ! Wire number
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_wirenum
#endif
          integer(kind=int4), allocatable, dimension(:) :: m_wirenum
          ! X1 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_x1
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_x1
          ! Y1 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_y1
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_y1
          ! Z1 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_z1
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_z1
          ! X2 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_x2
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_x2
          ! Y2 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_y2
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_y2
          ! Z2 coordinate
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_z2
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_z2
          ! Radius
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_radius
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_radius
          ! Number of segments per single wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_nsegs
#endif
          integer(kind=int4), allocatable, dimension(:) :: m_nsegs
          ! First segment value (interval in number of segments per wire)
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_firstseg
#endif
          integer(kind=int4), allocatable, dimension(:) :: m_firstseg
          ! Last segment value (interval in number of segments per wire)
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_lastseg
#endif
          integer(kind=int4), allocatable, dimension(:) :: m_lastseg
          ! Tag number
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_tagnum
#endif
          integer(kind=int4), allocatable, dimension(:) :: m_tagnum
    end type NECWireDataOut_t
    
    type, public :: NECSegmentDataOut_t
          
          SEQUENCE
        
          ! Number of data columns
          integer(kind=int4)  :: m_ncols
#if (GMS_SEGDATAOUT_ADD_PADDING) == 1
          integer(kind=int1), dimension(0:3) :: pad
#endif
          ! Segment number
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_segnum
#endif
          integer(kind=int4),   allocatable, dimension(:) :: m_segnum
          !  X -- COORDINATE OF SEG. CENTER
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_xcoord
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_xcoord
          !  Y -- COORDINATE OF SEG. CENTER
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_ycoord
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_ycoord
          !  Z -- COORDINATE OF SEG. CENTER
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_zcoord
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_zcoord
          ! Segment length
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_seglen
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_seglen
          ! ORIENTATION ANGLES: alpha
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_alpha
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_alpha
          ! ORIENTATION ANGLES: beta
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_beta
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_beta
          ! Wire radius
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_wradius
#endif
          real(kind=sp),        allocatable, dimension(:) :: m_wradius
          ! I-   -- CONNECTION DATA
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_Iminus
#endif
          integer(kind=int4),   allocatable, dimension(:) :: m_Iminus
          ! I    -- CONNECTION DATA
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_I
#endif
          integer(kind=int4),   allocatable, dimension(:) :: m_I
          ! I+   -- CONNECTION DATA
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_Iplus
#endif
          integer(kind=int4),   allocatable, dimension(:) :: m_Iplus
          ! Tag number
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_tagnum
#endif
          integer(kind=int4),   allocatable, dimension(:) :: m_tagnum
    end type NECSegmentDataOut_t
    
    contains
    
      subroutine initNECWireDataOut(wo,ncols,errstate,iounit,logging,verbose,append,fname)
#if defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: initNECWireDataOut
#endif
          use mod_constants, only : INITVAL
          use mod_print_error, only : print_non_fatal_error,  &
                                      handle_fatal_memory_error
          type(NECWireDataOut_t),      intent(inout) :: wo
          integer(kind=int4),          intent(in)    :: ncols
          logical(kind=int4),          intent(inout) :: errstate
          integer(kind=int4),          intent(in)    :: iounit
          logical(kind=int4),          intent(in)    :: logging,  &
                                                        verbose,  &
                                                        append
          character(len=*),         intent(in)       :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          if(errstate) errstate = .false.
          if(ncols <= 0) then
              call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                        " Module: mod_segdataout, subroutine: initNECWireDataOut: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
              errstate = .true.
              return
          end if
          wo.m_ncols =  ncols
          if( allocated(wo.m_wirenum)) then
              deallocate(wo.m_wirenum)
              allocate(wo.m_wirenum(wo.m_ncols), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_wirenum(wo.m_ncols), &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_x1)) then
              deallocate(wo.m_x1)
              allocate(wo.m_x1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_x1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_y1)) then
              deallocate(wo.m_y1)
              allocate(wo.m_y1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_y1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_z1)) then
              deallocate(wo.m_z1)
              allocate(wo.m_z1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_z1(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_x2)) then
              deallocate(wo.m_x2)
              allocate(wo.m_x2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_x2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_y2)) then
              deallocate(wo.m_y2)
              allocate(wo.m_y2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_y2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_z2)) then
              deallocate(wo.m_z2)
              allocate(wo.m_z2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_z2(wo.m_ncols),      &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_radius)) then
              deallocate(wo.m_radius)
              allocate(wo.m_radius(wo.m_ncols),   &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_radius(wo.m_ncols),   &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_nsegs)) then
              deallocate(wo.m_nsegs)
              allocate(wo.m_nsegs(wo.m_ncols),    &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_nsegs(wo.m_ncols),    &
                            STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_firstseg)) then
              deallocate(wo.m_firstseg)
              allocate(wo.m_firstseg(wo.m_ncols), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_firstseg(wo.m_ncols), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_lastseg)) then
              deallocate(wo.m_lastseg)
              allocate(wo.m_lastseg(wo.m_ncols), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_lastseg(wo.m_ncols), &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wo.m_tagnum)) then
              deallocate(wo.m_tagnum)
              allocate(wo.m_tagnum(wo.m_ncols),  &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wo.m_tagnum(wo.m_ncols),  &
                             STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory first touch
          wo.m_wirenum(:)   = -1
          wo.m_x1(:)        = INITVAL
          wo.m_y1(:)        = INITVAL
          wo.m_z1(:)        = INITVAL
          wo.m_x2(:)        = INITVAL
          wo.m_y2(:)        = INITVAL
          wo.m_z2(:)        = INITVAL
          wo.m_radius(:)    = INITVAL
          wo.m_nsegs(:)     = -1
          wo.m_firstseg(:)  = -1
          wo.m_lastseg(:)   = -1
          wo.m_tagnum(:)    = -1
          errstate = .true.
          return
9999      call handle_fatal_memory_error( iounit,logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_segdataout, subroutine: initNECWireDataOut -- Memory Allocation Failure !!", &
                              "module: mod_segdataout, subroutine: initNECWireDataOut -- Memory Allocation Failure !!", &
                                                    emsg,__LINE__ )
    end subroutine initNECWireDataOut
    
    subroutine readNECWireDataOut(wirenum,x1,y1,z1,x2,y2,z2,   &
                                  radius,nsegs,firstseg,lastseg,tagnum, &
                                  ncols,iounit,filename,errmsg,ioerr,&
                                  iounit2,logging,verbose,append,fname)
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES ALIGN : 32 :: readNECWireDataOut
#endif
          use mod_print_error,          only : handle_fatal_fileio_error
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: wirenum
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: x1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: y1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: z1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: x2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: y2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: z2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: radius
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: msegs
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: firstseg
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: lastseg
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: tagnum
          integer(kind=int4),                           intent(in)    :: ncols
          integer(kind=int4),                           intent(in)    :: iounit
          character(len=*),                             intent(in)    :: filename
          character(len=256),                           intent(inout) :: errmsg
          integer(kind=int4),                           intent(inout) :: ioerr
          integer(kind=int4),                           intent(in)    :: iounit2
          logical(kind=int4),                           intent(in)    :: logging,   &
                                                                          verbose,   &
                                                                         append
          character(len=*),                             intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
                  call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_segdataout, subroutine: readNECWireDataOut: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_segdataout, subroutine: readNECWireDataOut -- File Does Not Exist!! ",  &
                                            errmsg,367)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
               call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_segdataout, subroutine: readNECWireDataOut: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_segdataout, subroutine: readNECWireDataOut -- File Open I/O Failure !! ",  &
                                            errmsg,375)
          end if
          do idx = 1, ncols
              read(iounit,'(i6,7F10.7,i6,i6,i6,i6)',IOSTAT=ioerr,IOMSG=errmsg)         &
                           wirenum(idx),   x1(idx),       &
                           y1(idx),        z1(idx),       &
                           x2(idx),        y2(idx),       &
                           z2(idx),        radius(idx),   &
                           nsegs(idx),     firstseg(idx), &
                           lastseg(idx),   tagnum(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_segdataout, subroutine: readNECWireDataOut, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_segdataout, subroutine: readNECWireDataOut -- File READ I/O Failure !! ",  &
                                           errmsg,394)
    end subroutine readNECWireDataOut
    
    subroutine copyNECWireDataOut_ymm8r4_ymm8i4(wirenum,          &
                                                x1,               &
                                                y1,               &
                                                z1,               &
                                                x2,               &
                                                y2,               &
                                                z2,               &
                                                radius,           &
                                                nsegs,            &
                                                firstseg,         &
                                                lastseg,          &
                                                tagnum,           &
                                                ymm8i4_wirenum,   &
                                                ymm8r4_x1,        &
                                                ymm8r4_y1,        &
                                                ymm8r4_z1,        &
                                                ymm8r4_x2,        &
                                                ymm8r4_y2,        &
                                                ymm8r4_z2,        &
                                                ymm8r4_radius,    &
                                                ymm8i4_nsegs,     &
                                                ymm8i4_firstseg,  &
                                                ymm8i4_lastseg,   &
                                                ymm8i4_tagnum,    &
                                                errstate            )
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copyNECWireDataOut_ymm8r4_ymm8i4
#endif
          use mod_print_error,      only : print_non_fatal_error
          use mod_vectypes,         only : YMM8i4_t, YMM8r4_t
          use mod_copypaos,         only : copy_i4_ymm8i4,   &
                                           copy_r4_ymm8r4
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: wirenum
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: x1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: y1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: z1
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: x2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: y2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: z2
          real(kind=sp),      contiguous,dimension(:),  intent(out)   :: radius
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: msegs
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: firstseg
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: lastseg
          integer(kind=int4), contiguous,dimension(:),  intent(out)   :: tagnum
          type(YMM8i4_t),     contiguous, dimension(:), intent(inout) :: ymm8i4_wirenum
          type(YMM8r4_t),     contiguous, dimension(:), intent(inout) :: ymm8r4_x1,       &
                                                                     ymm8r4_y1,       &
                                                                     ymm8r4_z1,       &
                                                                     ymm8r4_x2,       &
                                                                     ymm8r4_y2,       &
                                                                     ymm8r4_z2,       &
                                                                     ymm8r4_radius    
          type(YMM8i4_t), contiguous, dimension(:), intent(inout) :: ymm8i4_nsegs,    &
                                                                     ymm8i4_firstseg, &
                                                                     ymm8i4_lastseg,  &
                                                                     ymm8i4_tagnum
          logical(kind=int4),                        intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          ! Exec code ....
          errstate = .false.
          is_conforming = (8*size(wirenum)   == size(ymm8i4_wirenum))  .and.  &
                          (8*size(x1)        == size(ymm8r4_x1))       .and.  &
                          (8*size(y1)        == size(ymm8r4_y1))       .and.  &
                          (8*size(z1)        == size(ymm8r4_z1))       .and.  &
                          (8*size(x2)        == size(ymm8r4_x2))       .and.  &
                          (8*size(y2)        == size(ymm8r4_y2))       .and.  &
                          (8*size(z2)        == size(ymm8r4_z2))       .and.  &
                          (8*size(radius)    == size(ymm8r4_radius))   .and.  &
                          (8*size(nsegs)     == size(ymm8i4_nsegs))    .and.  &
                          (8*size(firstseg)  == size(ymm8i4_firstseg)) .and.  &
                          (8*size(lastseg)   == size(ymm8i4_lastseg))  .and.  &
                          (8*size(tagnum)    == size(ymm8i4_tagnum)) 
              if(.not. is_conforming) then
                    call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                        " Module: mod_segdataout, subroutine: copyNECWireDataOut_ymm4r8_ymm8i4: Nonconforming arrays!! ",  &
                                        444,"GMS_segdataout.f90" )
                    errstate = .true.
                    return
              end if
              
         call copy_i4_ymm8i4(ymm8i4_wirenum ,wirenum)
         call copy_r4_ymm8r4(ymm8r4_x1,     x1)
         call copy_r4_ymm8r4(ymm8r4_y1,     y1)
         call copy_r4_ymm8r4(ymm8r4_z1,     z1)
         call copy_r4_ymm8r4(ymm8r4_x2,     x2)
         call copy_r4_ymm8r4(ymm8r4_y2,     y2)
         call copy_r4_ymm8r4(ymm8r4_z2,     z2)
         call copy_r4_ymm8r4(ymm8r4_radius, radius)
         call copy_i4_ymm8i4(ymm8i4_nsegs,  nsegs)
         call copy_i4_ymm8i4(ymm8i4_firstseg, firstseg)
         call copy_i4_ymm8i4(ymm8i4_lastseg,  lastseg)
         call copy_i4_ymm8i4(ymm8i4_tagnum,   tagnum)
          
    end subroutine copyNECWireDataOut_ymm8r4_ymm8i4
    
    subroutine initNECSegmentDataOut(so,ncols,errstate,iounit,logging,verbose,append,fname)
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: initNECSegmentDataOut
#endif
          use mod_constants,    only : INITVAL
          use mod_print_error,  only : print_non_fatal_error,   &
                                       handle_fatal_memory_error
          type(NECSegmentDataOut_t),            intent(inout) :: so
          integer(kind=int4),                   intent(in)    :: ncols
          integer(kind=int4),                   intent(in)    :: iounit
          logical(kind=int4),                   intent(inout) :: errstate
          logical(kind=int4),                   intent(in)    :: logging,   &
                                                                 verbose,   &
                                                                 append
          character(len=*),                     intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code .....
          if(errstate) errstate = .false.
          if(ncols <= 0) then
              call print_non_fatal_error(  "================ Non-Fatal ===================", &
                                          "Module: mod_segdataout, subroutine: initNECSegmentDataOut: -- ncols <= 0 !!", &
                                          __LINE__,__FILE__)
              errstate = .true.
              return
          end if
          so.m_ncols = ncols
          if( allocated(so.m_segnum)) then
              deallocate(so.m_segnum)
              allocate(so.m_segnum(so.m_ncols), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_segnum(so.m_ncols), &
                                STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_xcoord)) then
              deallocate(so.m_xcoord)
              allocate(so.m_xcoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_xcoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_ycoord)) then
              deallocate(so.m_ycoord)
              allocate(so.m_ycoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_ycoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_zcoord)) then
              deallocate(so.m_zcoord)
              allocate(so.m_zcoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_zcoord(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_seglen)) then
              deallocate(so.m_seglen)
              allocate(so.m_seglen(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_seglen(so.m_ncols), &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_alpha)) then
              deallocate(so.m_alpha)
              allocate(so.m_alpha(so.m_ncols),  &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_alpha(so.m_ncols),  &
                                 STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_beta)) then
              deallocate(so.m_beta)
              allocate(so.m_beta(so.m_ncols),   &
                                  STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_beta(so.m_ncols),   &
                                  STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_wradius)) then
              deallocate(so.m_wradius)
              allocate(so.m_wradius(so.m_ncols), &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_wradius(so.m_ncols), &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_Iminus)) then
              deallocate(so.m_Iminus)
              allocate(so.m_Iminus(so.m_ncols), &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_Iminus(so.m_ncols), &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_I)) then
              deallocate(so.m_I)
              allocate(so.m_I(so.m_ncols),       &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_I(so.m_ncols),       &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_Iplus)) then
              deallocate(so.m_Iplus)
              allocate(so.m_Iplus(so.m_ncols),   &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_Iplus(so.m_ncols),   &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(so.m_tagnum)) then
              deallocate(so.m_tagnum)
              allocate(so.m_tagnum(so.m_ncols),  &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(so.m_tagnum(so.m_ncols),  &
                                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          ! Memory forst touch
          so.m_segnum(:)    = -1
          so.m_xcoord(:)    = INITVAL
          so.m_ycoord(:)    = INITVAL
          so.m_zcoord(:)    = INITVAL
          so.m_seglen(:)    = INITVAL
          so.m_alpha(:)     = INITVAL
          so.m_beta(:)      = INITVAL
          so.m_wradius(:)   = INITVAL
          so.m_Iminus(:)    = -999999
          so.m_I(:)         = -999999
          so.m_Iplus(:)     = -999999
          so.m_tagnum(:)    = -1
          errstate = .false.
          return
9999      call handle_fatal_memory_error( iounit, logging,verbose,append,fname, &
                        "logger: "// __FILE__ // " Module: mod_segdataout, sub: initNECSegmentDataOut: -- Memory Allocation Failure!!!", &
                        "Module: mod_segdataout, sub: initNECSegmentDataOut: -- Memory Allocation Failure!!!",  &
                                                    emsg,__LINE__) 
    end subroutine initNECSegmentDataOut
    
    subroutine readNECSegmentDataOut(segnum,xcoord,ycoord,zcoord,       &
                                     seglen,alpha,beta,wradius,         &
                                     Iminus,I,Iplus,tagnum,
                                     ncols,iounit,filename,errmsg,iounit2,&
                                     ioerr,logging,verbose,append,fname)
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: readNECSegmentDataOut
#endif
          use mod_print_error,  only : handle_fatal_fileio_error
          integer(kind=int4), contiguous,dimension(:), intent(out) :: segnum
          real(kind=sp),      contiguous,dimension(:), intent(out) :: xcoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: ycoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: zcoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: seglen
          real(kind=sp),      contiguous,dimension(:), intent(out) :: alpha
          real(kind=sp),      contiguous,dimension(:), intent(out) :: beta
          real(kind=sp),      contiguous,dimension(:), intent(out) :: wradius
          integer(kind=int4), contiguous,dimension(:), intent(out) :: Iminus
          integer(kind=int4), contiguous,dimension(:), intent(out) :: I
          integer(kind=int4), contiguous,dimension(:), intent(out) :: Iplus
          integer(kind=int4), contiguous,dimension(:), intent(out) :: tagnum
          integer(kind=int4),                          intent(in)  :: ncols
          integer(kind=int4),                          intent(in)    :: iounit
          character(len=*),                            intent(in)    :: filename
          character(len=256),                          intent(inout) :: errmsg
          integer(kind=int4),                          intent(in)    :: iounit2
          integer(kind=int4),                          intent(inout) :: ioerr
          logical(kind=int4),                          intent(in)    :: logging,   &
                                                                        verbose,   &
                                                                        append
          character(len=*),                            intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: idx
          logical(kind=int4) :: is_present = .false.
          ! Exec code ....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
                  call handle_fatal_fileio_error(iounit2, logging,verbose,append,fname,   &
                                "logger: " //__FILE__//"module: mod_segdataout, subroutine: readNECSegmentDataOut: File"//filename//" Does Not Exist!!", &
                                          filename,                        &
                                "module: mod_segdataout, subroutine: readNECSegmentDataOut -- File Does Not Exist!! ",  &
                                        errmsg, 656        )
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
                  call handle_fatal_fileio_error( iounit2, logging,verbose,append,fname,   &
                                "logger: " //__FILE__//"module: mod_segdataout, subroutine: readNECSegmentDataOut: File"//filename//" Open I/O Failure !!", &
                                          filename,                        &
                                "module: mod_segdataout, subroutine: readNECSegmentDataOut -- File Open I/O Failure!! ",  &
                                        errmsg, 664         )
          end if
          do idx = 1, so.m_ncols
              read(iounit,'(i6,7F10.7,i6,i6,i6,i6)',IOSTAT=ioerr,IOMSG=errmsg)     &
                            segnum(idx),   xcoord(idx),                   &
                            ycoord(idx),   zcoord(idx),                   &
                            seglen(idx),   alpha(idx),                    &
                            beta(idx),     wradius(idx),                  &
                            Iminus(idx),   I(idx),                        &
                            Iplus(idx),    tagnum(idx)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
          end do
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call handle_fatal_fileio_error( iounit2,logging,verbose,append,fname,   &
                                "logger: " //__FILE__//"module: mod_segdataout, subroutine: readNECSegmentDataOut: File"//filename//" Read I/O Failure !!", &
                                          filename,                        &
                                "module: mod_segdataout, subroutine: readNECSegmentDataOut -- File Read I/O Failure!! ",  &
                                        errmsg, 682         )
    end subroutine readNECSegmentDataOut
    
    subroutine copyNECSegmentDataOut_ymm8i4_ymm8r4( segnum,           &
                                                    xcoord,           &
                                                    ycoord,           &
                                                    zcoord,           &
                                                    seglen,           &
                                                    alpha,            &
                                                    beta,             &
                                                    wradius,          &
                                                    Iminus,           &
                                                    I,                &
                                                    Iplus,            &
                                                    tagnum,           &
                                                    ymm8i4_segnum,    &
                                                    ymm8r4_xcoord,    &
                                                    ymm8r4_ycoord,    &
                                                    ymm8r4_zcoord,    &
                                                    ymm8r4_seglen,    &
                                                    ymm8r4_alpha,     &
                                                    ymm8r4_beta,      &
                                                    ymm8r4_wradius,   &
                                                    ymm8i4_Iminus,    &
                                                    ymm8i4_I,         &
                                                    ymm8i4_Iplus,     &
                                                    ymm8i4_tagnum,    &
                                                    errstate     )
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copyNECSegmentDataOut_ymm8i4_ymm8r4
#endif
          use mod_print_error,      only : print_non_fatal_error
          use mod_vectypes,         only : YMM8i4_t, YMM8r4_t
          use mod_copypaos,         only : copy_i4_ymm8i4,   &
                                           copy_r8_ymm8r4
          integer(kind=int4), contiguous,dimension(:), intent(out) :: segnum
          real(kind=sp),      contiguous,dimension(:), intent(out) :: xcoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: ycoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: zcoord
          real(kind=sp),      contiguous,dimension(:), intent(out) :: seglen
          real(kind=sp),      contiguous,dimension(:), intent(out) :: alpha
          real(kind=sp),      contiguous,dimension(:), intent(out) :: beta
          real(kind=sp),      contiguous,dimension(:), intent(out) :: wradius
          integer(kind=int4), contiguous,dimension(:), intent(out) :: Iminus
          integer(kind=int4), contiguous,dimension(:), intent(out) :: I
          integer(kind=int4), contiguous,dimension(:), intent(out) :: Iplus
          integer(kind=int4), contiguous,dimension(:), intent(out) :: tagnum
          type(YMM8i4_t),     contiguous,dimension(:), intent(inout) :: ymm8i4_segnum
          type(YMM8r4_t),     contiguous,dimension(:), intent(inout) :: ymm8r4_xcoord,  &
                                                                     ymm8r4_ycoord,  &
                                                                     ymm8r4_zcoord,  &
                                                                     ymm8r4_seglen,  &
                                                                     ymm8r4_alpha,   &
                                                                     ymm8r4_beta,    &
                                                                     ymm8r4_wradius
          type(YMM8i4_t), contiguous, dimension(:), intent(inout) :: ymm8i4_Iminus,  &
                                                                     ymm8i4_I,       &
                                                                     ymm8i4_Iplus,   &
                                                                     ymm8i4_tagnum
          logical(kind=int4),                        intent(inout) :: errstate
          ! Locals
          logical(kind=int4) :: is_conforming = .false.
          ! Exec code....
          errstate = .false.
          is_conforming = (8*size(segnum,dim=1)  == size(ymm8i4_segnum,dim=1))  .and. &
                          (8*size(xcoord,dim=1)  == size(ymm8r4_xcoord,dim=1))  .and. &
                          (8*size(ycoord,dim=1)  == size(ymm8r4_ycoord,dim=1))  .and. &
                          (8*size(zcoord,dim=1)  == size(ymm8r4_zcoord,dim=1))  .and. &
                          (8*size(seglen,dim=1)  == size(ymm8r4_seglen,dim=1))  .and. &
                          (8*size(alpha,dim=1)   == size(ymm8r4_alpha, dim=1))  .and. &
                          (8*size(beta,dim=1)    == size(ymm8r4_beta,  dim=1))  .and. &
                          (8*size(wradius,dim=1) == size(ymm8r4_wradius,dim=1)) .and. &
                          (8*size(Iminus,dim=1)  == size(ymm8i4_Iminus,dim=1))  .and. &
                          (8*size(I,  dim=1)     == size(ymm8i4_I,  dim=1))     .and. &
                          (8*size(Iplus,dim=1)   == size(ymm8i4_Iplus,dim=1))   .and. &
                          (8*size(tagnum,dim=1)  == size(ymm8i4_tagnum,dim=1))
          if(.not. is_conforming) then
                call print_non_fatal_error(  "================ Non-Fatal ===================", &
                                          "Module: mod_segdataout, subroutine: copyNECSegmentDataOut_ymm8i4_ymm4r8: -- Nonconforming arrays !!", &
                                          __LINE__,__FILE__)
                errstate = .true.
                return
          end if
          call copy_i4_ymm8i4( ymm8i4_segnum,  segnum)
          call copy_r4_ymm8r4( ymm8r4_xcoord,  xcoord)
          call copy_r4_ymm8r4( ymm8r4_ycoord,  ycoord)
          call copy_r4_ymm8r4( ymm8r4_zcoord,  zcoord)
          call copy_r4_ymm8r4( ymm8r4_seglen,  seglen)
          call copy_r4_ymm8r4( ymm8r4_alpha,   alpha)
          call copy_r4_ymm8r4( ymm8r4_beta,    beta)
          call copy_r4_ymm8r4( ymm8r4_wradius, wradius)
          call copy_i4_ymm8i4( ymm8i4_Iminus,  Iminus)
          call copy_i4_ymm8i4( ymm8i4_I,       I)
          call copy_i4_ymm8i4( ymm8i4_Iplus,   Iplus)
          call copy_i4_ymm8i4( ymm8i4_tagnum,  tagnum)
          
   end subroutine copyNECSegmentDataOut_ymm8i4_ymm8r4
                                                    
end module mod_segdataout
