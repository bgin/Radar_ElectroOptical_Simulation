
    



module mod_necgw

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_necgw'
 !          
 !          Purpose:
 !                     NEC ground wire (GW) target mesh geometry representation.
 !
 !          History:
 !                        
 !                        Date: 31-10-2018
 !                        Time: 15:06 GMT+2
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
    use mod_kinds, only : i1, i4, sp
    implicit none
    
    public :: initNECGWires,   &
              readNECGWires
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MOD_NECGW_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_NECGW_MINOR = 0_int4
    
    !Micro version
    integer(kind=i4), parameter, public :: MOD_NECGW_MICRO = 0_int4
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_NECGW_FULLVER = 1000_int4*MOD_NECGW_MAJOR+100_int4*MOD_NECGW_MINOR + &
                                                                 10_int4*MOD_NECGW_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_NECGW_CREATION_DATE = "31-10-2018 15:09 +00200 (WED 31 OCT 2018 GMT+2) "
    
    ! Module build date (should be set after successful compilation)
    character(*),       parameter, public :: MOD_NECGW_BUILD_DATE = __DATE__ ":" __TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_NECGW_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_NECGW_SYNOPSIS = "NEC ground wire (GW) target mesh geometry representation."
    

#if !defined(GMS_NECGW_ADD_PADDING)
  #define GMS_NECGW_ADD_PADDING 1
#endif
    
    type, public :: NECGWires_t
        
         
          ! Number of GW columns
          integer(kind=i4) :: m_ncols
#if (GMS_NECGW_ADD_PADDING) == 1
          integer(kind=i1), dimension(0:3) :: pad0
#endif
          ! Wire ordinal
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_ordinal
#endif
          integer(kind=i4), allocatable, dimension(:) :: m_ordinal
          ! Number of segments per wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_nsegs
#endif
          integer(kind=i4), allocatable, dimension(:) :: m_nsegs
          ! Start_x coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_startx
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_startx
          ! Stop_x coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_stopx
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_stopx
          ! Start_y coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_starty
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_starty
          ! Stop_y coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_stopy
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_stopy
          ! Start_z coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_startz
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_startz
          ! Stop_z coordinate of every wire
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_stopz
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_stopz
          ! Wire radius
#if defined __INTEL_COMPILER
          !DIR$     ATTRIBUTES ALIGN : 64 :: m_radii
#endif
          real(kind=sp),      allocatable, dimension(:) :: m_radii
    end type NECGWires_t
    
    contains
    
      subroutine initNECGWires(wf,ncols,errstate,iounit,logging,verbose,append,fname)
#if defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: initNECGWires
#endif
          use mod_print_error, only : print_non_fatal_error,  &
                                      handle_fatal_memory_error
          type(NECGWires_t),            intent(inout) :: wf
          integer(kind=i4),           intent(in)    :: ncols
          logical(kind=i4),           intent(inout)    :: errstate
          integer(kind=i4),           intent(in)       :: iounit
          logical(kind=i4),           intent(in)    :: logging,verbose,append
          character(len=*),             intent(in)    :: fname
          ! Locals
          integer(kind=i4) :: aerr
          character(len=256) :: emsg
          ! Exec code
          errstate = .false.
          if(ncols <= 0) then
             call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                        " Module: mod_necgw, subroutine: init_NECGWires: Invalid 'ncols' argument!! ",  &
                                        __LINE__,__FILE__ )
             errstate = .true.
             return
          end if
          wf.m_ncols = ncols
          if(  allocated(wf.m_ordinal)) then
               deallocate(wf.m_ordinal)
               allocate(wf.m_ordinal(wf.m_ncols),     &
                                    STAT=aerr,        &
                                    ERRMSG=emsg)
               if(aerr /= 0) goto 9999   
          else   
               allocate(wf.m_ordinal(wf.m_ncols),      &
                                     STAT=aerr,        &
                                     ERRMSG=emsg)
               if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_nsegs)) then
              deallocate(wf.m_nsegs)
              allocate(wf.m_nsegs(wf.m_ncols),         &
                                  STAT=aerr,           &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_nsegs(wf.m_ncols),         &
                                  STAT=aerr,           &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_startx)) then
              deallocate(wf.m_startx)
              allocate(wf.m_startx(wf.m_ncols),        &
                                   STAT=aerr,          &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_startx(wf.m_ncols),        &
                                   STAT=aerr,          &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_stopx)) then
              deallocate(wf.m_stopx)
              allocate(wf.m_stopx(wf.m_ncols),         &
                                  STAT=aerr,           &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_stopx(wf.m_ncols),         &
                                  STAT=aerr,           &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_starty)) then
              deallocate(wf.m_starty)
              allocate(wf.m_starty(wf.m_ncols),       &
                                  STAT=aerr,          &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_starty(wf.m_ncols),       &
                                  STAT=aerr,          &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_stopy)) then
              deallocate(wf.m_stopy)
              allocate(wf.m_stopy(wf.m_ncols),        &
                                  STAT=aerr,          &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_stopy(wf.m_ncols),        &
                                  STAT=aerr,          &
                                  ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_startz)) then
              deallocate(wf.m_startz)
              allocate(wf.m_startz(wf.m_ncols),       &
                                   STAT=aerr,         &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_startz(wf.m_ncols),       &
                                   STAT=aerr,         &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_stopz)) then
              deallocate(wf.m_stopz)
              allocate(wf.m_stopz(wf.m_ncols),        &
                                   STAT=aerr,         &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
               allocate(wf.m_stopz(wf.m_ncols),        &
                                   STAT=aerr,          &
                                   ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          if( allocated(wf.m_radii)) then
              deallocate(wf.m_radii)
              allocate(wf.m_radii(wf.m_ncols),         &
                                    STAT=aerr,         &
                                    ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          else
              allocate(wf.m_radii(wf.m_ncols),         &
                                    STAT=aerr,         &
                                    ERRMSG=emsg)
              if(aerr /= 0) goto 9999
          end if
          !! Memory first touch
          wf.m_ordinal(:) = 0
          wf.m_nsegs(:)   = 0
          wf.m_startx(:)  = 0.0E+00_sp
          wf.m_stopx(:)   = 0.0E+00_sp
          wf.m_starty(:)  = 0.0E+00_sp
          wf.m_stopy(:)   = 0.0E+00_sp
          wf.m_startz(:)  = 0.0E+00_sp
          wf.m_stopz(:)   = 0.0E+00_sp
          wf.m_radii(:)   = 0.0E+00_sp
          errstate = .false.
          return
9999      call handle_fatal_memory_error( iounit, logging,verbose,append,fname,                                                    &
                             "logger: "// __FILE__ // "module: mod_necgw, subroutine: init_NECGWires -- Memory Allocation Failure !!", &                                                        &
                              "module: mod_necgw, subroutine: init_NECGWires -- Memory Allocation Failure !!", &
                                                    emsg, 266 )
    end subroutine initNECGWires
    
    subroutine readNECGWires(ordinal,nsegs,startx,stopx,                 &
                             starty,stopy,startz,stopz,radii,ncols,      &
                             iounit,filename,errmsg,ioerr,iounit2,       &
                             logging,verbose,append,fname)
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: readNECGWires
#endif
          use mod_print_error, only : handle_fatal_fileio_error
          integer(kind=i4), contiguous,dimension(:), intent(out)      :: ordinal
          integer(kind=i4), contiguous,dimension(:), intent(out)      :: nsegs
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: startx
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: stopx
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: starty
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: stopy
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: startz
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: stopz
          real(kind=sp),      contiguous,dimension(:), intent(out)      :: radii
          integer(kind=i4),                          intent(in)       :: ncols
          integer(kind=i4),                          intent(in)       :: iounit
          character(len=*),                            intent(in)       :: filename
          character(len=256),                          intent(inout)    :: errmsg
          integer(kind=i4),                          intent(inout)    :: ioerr
          integer(kind=i4),                          intent(in)       :: iounit2
          logical(kind=i4),                          intent(in)       :: logging,verbose,append
          character(len=*),                            intent(in)       :: fname
          ! Locals
          integer(kind=i4) :: idx
          logical(kind=i4) :: is_present = .false.
          ! Exec code
          
          inquire(FILE=trim(filename), EXIST=is_present)
          if(.not. is_present)  then
                call handle_fatal_fileio_error(iounit2, logging,verbose,append,fname,    &
                                            "logger: " //__FILE__//"module: mod_necgw, subroutine: read_NECGWires, File"//filename//" Does Not Exist !! ",      &
                                            filename,                        &
                                            "module: mod_necgw, subroutine: read_NECGWires -- File Does Not Exist !! ",  &
                                            errmsg, 289)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
             call handle_fatal_fileio_error(iounit2,logging,verbose,append,fname,    &
                                            "logger: " //__FILE__//"module: mod_necgw, subroutine: read_NECGWires, File"//filename//" Open I/O Failure !! " ,     &
                                            filename,                        &
                                            "module: mod_necgw, subroutine: read_NECGWires -- File Open I/O Failure !! ",  &
                                            errmsg, 298)
          end if
          do idx = 1, ncols
              read(iounit,'(i6,i6,7F10.7)',IOSTAT=ioerr,IOMSG=errmsg)      &
                                       ordinal(idx),nsegs(idx),   &
                                       startx(idx), stopx(idx),   &
                                       starty(idx), stopy(idx),   &
                                       startz(idx), stopz(idx),   &
                                       radii(idx)
              
              if(ioerr > 0 .or. ioerr < 0) goto 9999
                  
             
          end do
          close(UNIT=iounit, STATUS='KEEP')
          return
9999      close(UNIT=iounit, STATUS='KEEP')
          call handle_fatal_fileio_error(iounit2, logging,verbose,append,fname,    &
                             "logger:"//__FILE__ //"module: mod_necgw, subroutine: read_NECGWires, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                                          "module: mod_necgw, subroutine: read_NECGWires -- File READ I/O Failure !! ",  &
                                           errmsg, 319)
    end subroutine readNECGWires
    
    subroutine copyNECGWires_ymm8i4_ymm4r8(ordinal,
                                           nsegs,             &
                                           startx,            &
                                           stopx,             &
                                           starty,            &
                                           stopy,             &
                                           startz,            &
                                           stopz,             &
                                           ymm8i4_ordinal,    &
                                           ymm8i4_nsegs,      &
                                           ymm4r8_startx,     &
                                           ymm4r8_stopx,      &
                                           ymm4r8_starty,     &
                                           ymm4r8_stopy,      &
                                           ymm4r8_startz,     &
                                           ymm4r8_stopz,      &
                                           ymm4r8_radii,      &
                                           errstate  )
#if defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copyNECGWires_ymm8i4_ymm4r8
#endif
          use mod_print_error,      only : print_non_fatal_error
          use mod_vectypes,         only : YMM8i4_t, YMM4r8_t
          use mod_copypaos,         only : copy_i4_ymm8i4,   &
                                           copy_r8_ymm4r8
          integer(kind=i4), contiguous,dimension(:), intent(in)      :: ordinal
          integer(kind=i4), contiguous,dimension(:), intent(in)      :: nsegs
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: startx
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: stopx
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: starty
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: stopy
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: startz
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: stopz
          real(kind=sp),      contiguous,dimension(:), intent(in)      :: radii
          type(YMM8i4_t),     contiguous, dimension(:),intent(inout)   :: ymm8i4_ordinal,   &
                                                                          ymm8i4_nsegs
          type(YMM4r8_t),     contiguous, dimension(:),intent(inout)   ::  ymm4r8_startx,     &
                                                                          ymm4r8_stopx,      &
                                                                          ymm4r8_starty,     &
                                                                          ymm4r8_stopy,      &
                                                                          ymm4r8_startz,     &
                                                                          ymm4r8_stopz,      &
                                                                          ymm4r8_radii
          logical(kind=i4),                         intent(inout) :: errstate
          ! Locals
          logical(kind=i4) :: is_conforming = .false.
          ! Exec code ....
          errstate = .false.
          is_conforming = (size(ordinal) == 8*size(ymm8i4_ordinal)) .and.  &
                          (size(nsegs)   == 8*size(ymm8i4_nsegs))   .and.  &
                          (size(startx)  == 4*size(ymm4r8_startx))  .and.  &
                          (size(stopx)   == 4*size(ymm4r8_stopx))   .and.  &
                          (size(starty)  == 4*size(ymm4r8_starty))  .and.  &
                          (size(stopy)   == 4*size(ymm4r8_stopy))   .and.  &
                          (size(startz)  == 4*size(ymm4r8_stopz))   .and.  &
                          (size(radii)   == 4*size(ymm4r8_radii))
          if(.not. is_conforming) then
              call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                        " Module: mod_necgw, subroutine: copyNECGWires_ymm8i4_ymm4r8: Nonconforming arrays!! ",  &
                                        __LINE__,__FILE__ )
              errstate = .true.
              return
          end if
          call copy_i4_ymm8i4(ymm8i4_ordinal, ordinal)
          call copy_i4_ymm8i4(ymm8i4_nsegs,   nsegs)
          call copy_r8_ymm4r8(ymm4r8_startx,  startx)
          call copy_r8_ymm4r8(ymm4r8_stopx,   stopx)
          call copy_r8_ymm4r8(ymm4r8_starty,  starty)
          call copy_r8_ymm4r8(ymm4r8_stopy,   stopy)
          call copy_r8_ymm4r8(ymm4r8_startz,  startz)
          call copy_r8_ymm4r8(ymm4r8_stopz,   stopz)
          call copy_r8_ymm4r8(ymm4r8_radii,   radii)
          
    end subroutine copyNECGWires_ymm8i4_ymm4r8
                                           
                                           

end module mod_necgw
