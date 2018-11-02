
    
#include "Config.fpp"


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
    use mod_kinds, only : int1, int4, dp
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_NECGW_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_NECGW_MINOR = 0_int4
    
    !Micro version
    integer(kind=int4), parameter, public :: MOD_NECGW_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_NECGW_FULLVER = 1000_int4*MOD_NECGW_MAJOR+100_int*MOD_NECGW_MINOR + &
                                                                 10_int4*MOD_NECGW_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_NECGW_CREATION_DATE = "31-10-2018 15:09 +00200 (WED 31 OCT 2018 GMT+2) "
    
    ! Module build date (should be set after successful compilation)
    character(*),       parameter, public :: MOD_NECGW_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_NECGW_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_NECGW_SYNOPSIS = "NEC ground wire (GW) target mesh geometry representation."
    
!DIR$ IF .NOT. DEFINED (GMS_NECGW_ADD_PADDING)
    !DIR$ DEFINE GMS_NECGW_ADD_PADDING = 1
!DIR$ ENDIF
    
    type, public :: NECGWires_t
        
          public
          SEQUENCE
          ! Number of GW columns
          integer(kind=int4) :: m_ncols
!DIR$     IF (GMS_NECGW_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(0:3) :: pad0
!DIR$     ENDIF
          ! Wire ordinal
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ordinal
          integer(kind=int4), allocatable, dimension(:) :: m_ordinal
          ! Number of segments per wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_nsegs
          integer(kind=int4), allocatable, dimension(:) :: m_nsegs
          ! Start_x coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_startx
          real(kind=dp),      allocatable, dimension(:) :: m_startx
          ! Stop_x coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_stopx
          real(kind=dp),      allocatable, dimension(:) :: m_stopx
          ! Start_y coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_starty
          real(kind=dp),      allocatable, dimension(:) :: m_starty
          ! Stop_y coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_stopy
          real(kind=dp),      allocatable, dimension(:) :: m_stopy
          ! Start_z coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_startz
          real(kind=dp),      allocatable, dimension(:) :: m_startz
          ! Stop_z coordinate of every wire
!DIR$     ATTRIBUTES ALIGN : 64 :: m_stopz
          real(kind=dp),      allocatable, dimension(:) :: m_stopz
          ! Wire radius
!DIR$     ATTRIBUTES ALIGN : 64 :: m_radii
          real(kind=dp),      allocatable, dimension(:) :: m_radii
    end type NECGWires_t
    
    contains
    
    subroutine init_NECGWires(wf,ncols,errstate,logging,verbose,append,fname)
          use mod_print_error, only : print_non_fatal_error,  &
                                      handle_fatal_memory_error
          type(NECGWires_t),            intent(inout) :: wf
          integer(kind=int4),           intent(in)    :: ncols
          logical(kind=int4),           intent(in)    :: errstate
          logical(kind=int4),           intent(in)    :: logging,verbose,append
          character(len=*),             intent(in)    :: fname
          ! Locals
          integer(kind=int4) :: aerr
          character(len=256) :: emsg
          ! Exec code
          if(errstate) errstate = .false.
          if(ncols <= 0) then
             call print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                        " logger:130 --> init_NECGWires: Invalid 'ncols' argument!! ",  &
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
          wf.m_startx(:)  = 0.0E+00_dp
          wf.m_stopx(:)   = 0.0E+00_dp
          wf.m_starty(:)  = 0.0E+00_dp
          wf.m_stopy(:)   = 0.0E+00_dp
          wf.m_startz(:)  = 0.0E+00_dp
          wf.m_stopz(:)   = 0.0E+00_dp
          wf.m_radii(:)   = 0.0E+00_dp
          errstate = .false.
          return
          9999      call handle_fatal_memory_error( logging,verbose,append,fname,                                                    &
                                     "logger: 259" // __FILE__,                                                       &
                                     "module: mod_necgw, subroutine: init_NECGWires -- Memory Allocation Failure !!", &
                                                    emsg,__LINE__ )
    end subroutine init_NECGWires
    
    subroutine read_NECGWires(wf,iounit,filename,errmsg,ioerr,logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECGWires_t),        intent(inout)    :: wf
          integer(kind=int4),       intent(in)       :: iounit
          character(len=*),         intent(in)       :: filename
          character(len=256),       intent(inout)    :: errmsg
          integer(kind=int4),       intent(inout)    :: ioerr
          logical(kind=int4),       intent(in)       :: logging,verbose,append
          character(len=*),         intent(in)       :: fname
          ! Locals
          integer(kind=int4) :: idx
          ! Exec code
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
             call handle_fatal_fileio_error(logging,verbose,append,fname,    &
                                            "logger: 278 " // __FILE__,      &
                                            filename,                        &
                                            "module: mod_necgw, subroutine: read_NECGWires -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          do idx = 1, wf.m_ncols
              read(iounit,'(i6,i6,7F22.15)',IOSTAT=ioerr,IOMSG=errmsg)      &
                                       wf.m_ordinal(idx),wf.m_nsegs(idx),   &
                                       wf.m_startx(idx), wf.m_stopx(idx),   &
                                       wf.m_starty(idx), wf.m_stopy(idx),   &
                                       wf.m_startz(idx), wf.m_stopz(idx),   &
                                       wf.m_radii(idx)
              
              if(ioerr > 0 .or. ioerr < 0) goto 9999
                  
             
          end do
          close(UNIT=iounit, STATUS='KEEP')
          return
9999      close(UNIT=iounit, STATUS='KEEP')
          call handle_fatal_fileio_error(logging,verbose,append,fname,    &
                                         "logger: 288 " // __FILE__,      &
                                          filename,                        &
                                          "module: mod_necgw, subroutine: read_NECGWires -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
    end subroutine read_NECGWires
    
    


end module mod_necgw