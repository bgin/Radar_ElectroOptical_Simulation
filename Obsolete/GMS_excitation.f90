


module mod_excitation

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_excitation'
 !          
 !          Purpose:
 !                      NEC input wave excitation.
 !          History:
 !                        
 !                        Date: 17-11-2018
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
     use mod_kinds,     only : int1, int4, sp
     implicit none
     
     public  ::  initNECExcitation,   &
                 readNECExcitation,   &
                 copyNECExcitation_AoS_SoA
                 
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_EXCITATION_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_EXCITATION_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_EXCITATION_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_EXCITATION_FULLVER = 1000_int4*MOD_EXCITATION_MAJOR + &
                                                                      100_int4*MOD_EXCITATION_MINOR  + &
                                                                      10_int4*MOD_EXCITATION_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_EXCITATION_CREATE_DATE = "17-11-2018 11:18 +00200 (SAT 17 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_EXCITATION_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_EXCITATION_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_EXCITATION_SYNOPSIS = "NEC input waves excitation."

!DIR$ IF .NOT. DEFINED (GMS_EXCITATION_ADD_PADDING)
    !DIR$ DEFINE GMS_EXCITATION_ADD_PADDING = 1
!DIR$ ENDIF
    
        type, public :: NECExcitation_t
              
              ! Wave type
              character(len=32) :: m_wname

              ! Theta component (deg)
              real(kind=sp)     :: m_theta
              ! Phi component (deg)
              real(kind=int4)   :: m_phi
              ! Eta component (deg)
              real(kind=int4)   :: m_eta
              ! Type
              character(len=32) :: m_wtype

               ! Axial ration
              real(kind=sp)     :: m_axratio

        end type NECExcitation_t
        
    contains
    
    subroutine initNECExcitation(nex)
          use mod_constants,  only : INITVAL
          type(NECExcitation_t),        intent(inout) :: nex
          ! Exec code .....
          nex.m_wname   = " "
          nex.m_theta   = 0.0_sp
          nex.m_phi     = 0.0_sp
          nex.m_eta     = 0.0_sp
          nex.m_wtype   = " "
          nex.m_axratio = 0.0_sp
    end subroutine initNECExcitation
    
    subroutine readNECExcitation(nex,iounit,filename,errmsg,ioerr,     &
                                 logging,verbose,append,fname)
          use mod_print_error, only : handle_fatal_fileio_error
          type(NECExcitation_t),        intent(inout) :: nex
          integer(kind=int4),           intent(in)    :: iounit
          character(len=*),             intent(in)    :: filename
          character(len=256),           intent(inout) :: errmsg
          integer(kind=int4),           intent(inout) :: ioerr
          logical(kind=int4),           intent(in)    :: logging,   &
                                                         verbose,   &
                                                         append
          character(len=*),             intent(in)    :: fname
          ! Locals
          logical(kind=int4) :: is_present = .false.
          ! Exec code .....
          inquire(FILE=trim(filename),EXIST=is_present)
          if(.not. is_present) then
               call handle_fatal_fileio_error( iounit,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_excitation, subroutine: readNECExcitation: File"//filename//" Does Not Exist!!", &
                                            filename,                        &
                     "module: mod_excitation, subroutine: readNECExcitation-- File Does Not Exist!! ",  &
                                            errmsg,__LINE__)
          end if
          open(UNIT=iounit,FILE=trim(filename),ACTION='READ',STATUS='OLD',IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
               call handle_fatal_fileio_error( iounit,logging,verbose,append,fname,     &
                     "logger: " //__FILE__//"module: mod_excitation, subroutine: readNECExcitation: File"//filename//" Open I/O Failure !! ", &
                                            filename,                        &
                     "module: mod_excitation, subroutine: readNECExcitation -- File Open I/O Failure !! ",  &
                                            errmsg,__LINE__)
          end if
          read(iounit,'(a,F10.7,F10.7,F10.7,a,F10.7)',IOMSG=errmsg,IOSTAT=ioerr) &
                       nex.m_wname, nex.m_theta, nex.m_phi, nex.m_eta, nex.m_wtype, nex.m_axratio
          if(ioerr > 0 .or. ioerr < 0) goto 9999
          close(UNIT=iounit,STATUS='KEEP')
          return
9999      close(UNIT=iounit,STATUS='KEEP')
          call     handle_fatal_fileio_error( iounit, logging,verbose,append,fname,  &
                            "logger:"//__FILE__ //"module: mod_excitation, subroutine: readNECExcitation, File"//filename//" READ I/O Failure!!",   &
                                          filename,                        &
                            "module: mod_excitation, subroutine: readNECExcitation -- File READ I/O Failure !! ",  &
                                           errmsg,__LINE__)
    end subroutine readNECExcitation
                                 
    subroutine copyNECExcitation_AoS_SoA(nex_aos,                  &
                                         nelems,                   &
                                         soa_wname,                &
                                         soa_theta,                &
                                         soa_phi,                  &
                                         soa_eta,                  &
                                         soa_wtype,                &
                                         soa_axratio                 )
          type(NECExcitation_t),    dimension(1:nelems),         intent(in)    :: nex_aos
          integer(kind=int4),                                    intent(in)    :: nelems
          character(len=32),    contiguous, dimension(:), intent(inout) :: soa_wname
          real(kind=sp),        contiguous, dimension(:), intent(inout) :: soa_theta
          real(kind=sp),        contiguous, dimension(:), intent(inout) :: soa_phi
          real(kind=sp),        contiguous, dimension(:), intent(inout) :: soa_eta
          character(len=32),    contiguous, dimension(:), intent(inout) :: soa_wtype
          real(kind=sp),        contiguous, dimension(:), intent(inout) :: soa_axratio
          ! Locals
          integer(kind=int4) :: idx
          ! Exec code ....
          
          do idx = 1, nelems
                soa_wname(idx)   = nex_aos(idx).m_wname
                soa_theta(idx)   = nex_aos(idx).m_theta
                soa_phi(idx)     = nex_aos(idx).m_phi
                soa_eta(idx)     = nex_aos(idx).m_eta
                soa_wtype(idx)   = nex_aos(idx).m_wtype
                soa_axratio(idx) = nex_aos(idx).m_axratio
          end do
          
    end subroutine copyNECExcitation_AoS_SoA
                                         
                                         

end module mod_excitation
