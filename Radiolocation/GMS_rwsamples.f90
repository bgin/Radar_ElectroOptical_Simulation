
module rwsamples



         
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'rwsamples'
 !          
 !          Purpose:
 !                   Mathematical representation and computation
 !                   of power weather sample of rect 
 !                    wave radar signal.
 !                   This module also contains subroutines which
 !                   perform computation of signal statistics.
 !          History:
 !                        Date: 11-11-2017
 !                        Time: 10:21 GMT+2
 !
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                      Doppler Radar and Weather Observations
 !                      Richard L. Dvorak, Dusan S. Zrnic
 !                      pages 67-72 
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    public
    use mod_kinds,       only : i4,r4
     
    
    
    public :: assignment (=)
      
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(i4), parameter :: MOD_RWSAMPLES_MAJOR = 1
    
    ! Minor version
    integer(i4), parameter :: MOD_RWSAMPLES_MINOR = 0
    
    ! Micro version
    integer(i4), parameter :: MOD_RWSAMPLES_MICRO = 0
    
    ! Module full version
    integer(i4), parameter :: MOD_RWSAMPLES_FULLVER = 1000*MOD_RWSAMPLES_MAJOR+100*MOD_RWSAMPLES_MINOR+ &
                                                        10*MOD_RWSAMPLES_MICRO
    
    ! Module creation date
    character(*),  parameter :: MOD_RWSAMPLES_CREATE_DATE = "11-11-2017 10:21 +00200 (SAT 11 NOV 2017 GMT+2)"
    
    ! Module build date  (should be set after successful build date)
    character(*),  parameter :: MOD_RWSAMPLES_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter :: MOD_RWSAMPLES_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter :: MOD_RWSAMPLES_DESCRIPT = " Rect wave signal weather samples"
    
    !========================================
    !  Type: RSWSamples_t
    !========================================
    
    type, public :: RSWSamples_t
        
        
        
        ! Number of echoes per sampples
        integer(i4) :: m_nechoes
        
        ! Number of composed samples in this sample train
        integer(i4) :: m_size
        
        ! Number of scaterers
        integer(i4) :: m_scatnum
        
        ! Time duration of samples interval
        real(r4)    :: m_Ts
        
        ! Time duration of single sample
        real(r4)    :: m_ths
        
        ! Samples indexing integral array
        integer(i4), allocatable, dimension(:) :: m_sampID
!DIR$   ATTRIBUTES ALIGN : 64 :: m_sampID
        
        ! Cached sample signal components
        ! Complex amplitude
        complex(r4), allocatable, dimension(:) :: m_Ai
!DIR$   ATTRIBUTES ALIGN : 64 :: m_Ai
        
        ! Samples (IQ) components one per sample
        complex(r4), allocatable, dimension(:) :: m_Wi
!DIR$   ATTRIBUTES ALIGN : 64 :: m_Wi
        
        ! Complex exponential term itself beign a single complex value
        ! of function related to scaterrer distance and to specific bandwidth
        complex(r4), allocatable, dimension(:,:) :: m_cexpt
!DIR$   ATTRIBUTES ALIGN : 64 :: m_cexpt
        
        ! Output members
        ! Power signal composition of sample train (per single sample)
        complex(r4), allocatable, dimension(:) :: m_Vsamp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_Vsamp
        
         ! Averaged power specific frequencies of sample train
        complex(r4), allocatable, dimension(:) :: m_Vsampavg
!DIR$   ATTRIBUTES ALIGN : 64 :: m_Vsampavg
        
        ! built indicator
        logical(i4) :: m_isbuilt
        
        contains
    
        !=====================================================
        !  Construction, copying, destruction
        !=====================================================
        
        procedure, pass(this), public :: init
        
        procedure, pass(this), public :: copy
        
        procedure, pass(this), public :: destroy
        
        
      
        
      
        
        !================================================ 
        !  Computational procedures
        !================================================
        
        procedure, pass(this), public :: compute_Vsamp
        
        procedure, pass(this), public :: compute_Vsampavg
        
        !=================================================
        !    Read/write procedures
        !=================================================
        
        procedure, pass(this), public :: read_samples
        
        procedure, pass(this), public :: write_samples
        
    end type RSWSamples_t
        
        !==================================================
        !  Module operators
        !==================================================
    
        interface assignment (=)
            module procedure assign_samples
        end interface
        
            
    contains
    
    !========================================!
    !    Implementation                      !
    !========================================!
    
    !=================================================!
    !  @subroutine: init                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of complex arrays (output)
    !  Copying of cached samples component arrays.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   2) -2 -- Invalid argument (any of them)
    !   3) -3 -- Floating-point arithmentic error 
    !            (general error) (not in this scope)
    !   4) -4 -- Overflow
    !   5) -5 -- Underflow
    !   6) -6 -- Inexact
    !   7) -7 -- Denormal
    !   8) -8 -- Cancellation error (cadna)
    !=================================================!
    subroutine init(this,nechoes,size,scatnum,Ts,Ai,Wi,cexpt,ierr)
          implicit none
          class(RSWSamples_t),               intent(out)   :: this
          integer(i4),                       intent(in)    :: nechoes,size,scatnum
          real(r4),                          intent(in)    :: Ts
          complex(r4), dimension(size),      intent(in)    :: Ai
!DIR$     ASSUME_ALIGNED Ai:64
          complex(r4), dimension(size),      intent(in)    :: Wi
!DIR$     ASSUME_ALIGNED Wi:64
          complex(r4), dimension(size,size), intent(in)    :: cexpt
          integer(i4),                       intent(inout) :: ierr
!DIR$     ASSUME_ALIGNED cexpt:64
          ! Locals
          integer(i4)      :: i,j
          ! Start of executable sttements
          if(this%m_isbuilt == .true.) then
              ierr = -1
              return
          end if
          if(nechoes< 1 .OR. &
             size  <= 1 .OR. &
             Ts    <= 0._r4 ) then
              ierr = -2
              return
          end if
          ! Begin construction
          this%m_nechoes = nechoes
          this%m_size    = size
          this%m_scatnum = scatnum
          this%m_Ts      = Ts
          this%m_ths     = this%m_Ts/REAL(size,kind=r4)
          ! Array members
          associate(dim1=>this%m_size)
              allocate(this%m_sampID(dim1),     &
                       this%m_Ai(dim1),         &
                       this%m_Wi(dim1),         &
                       this%m_cexpt(dim1,dim1), &
                       this%m_Vsamp(dim1),      &
                       this%m_Vsampavg(dim1))
          end associate
          ! Arrays initialization
          do i = 1, this%m_size
              this%m_sampID(i) = i
              this%m_Ai(i) = Ai(i)
              this%m_Wi(i) = Wi(i)
!DIR$     SIMD
              do j = 1, this%m_size
                  this%m_cexpt(i,j) = cexpt(i,j)
              end do
          end do
          this%m_isbuilt = .true.
    end subroutine              
    
    !=================================================!
    !  @subroutine: copy                                          
    !  @Purpose:
    !            Copying of object state.
    !            Deep copy semantics in use.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !  
    !=================================================!                
    subroutine copy(this,other,ierr)
          implicit none
          class(RSWSamples_t), intent(out) :: this
          class(RSWSamples_t), intent(in)  :: other
          integer(i4),       intent(inout) :: err
          ! Locals
          if(this%m_isbuilt == .true.) then
              ierr = -1
              return
          end if
          ! Begin copy-construction
          this%m_nechoes = other%m_nechoes
          this%m_size    = other%m_size
          this%m_scatnum = other%m_scatnum
          this%m_Ts      = other%m_Ts
          this%m_ths     = other%m_ths
          this%m_sampID  = other%m_sampID
          this%m_Ai      = other%m_Ai
          this%m_Wi      = other%m_Wi
          this%m_cexpt   = other%m_cexpt
          this%m_Vsamp   = other%m_Vsamp
          this%m_Vsampavg = other%m_Vsampavg
          this%m_isbuilt = .true.
    end subroutine
    
    !=================================================!
    !  @subroutine: destroy                                          
    !  @Purpose:
    !            Destroys object state by allocatable
    !            arrays deallocation and setting
    !            member scalar variables to default values.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   
    !=================================================!
    subroutine destroy(this,ierr)
          implicit none
          class(RSWSamples_t), intent(inout) :: this
         
          ! Start of executable statements
         
          if(this%m_isbuilt == .false.) then
              ierr = -1
              return
          end if
          this%m_nechoes = 0
          this%m_size    = 0
          this%m_scatnum = 0
          this%m_Ts      = 0.0_r4
          this%m_ths     = 0.0_r4
          deallocate(this%m_sampID,   &
                     this%m_Ai,       &
                     this%m_Wi,       &
                     this%m_cexpt,    &
                     this%m_Vsamp,    &
                     this%m_Vsampavg)
                    
         this%m_isbuilt = .false.
    end subroutine
    
  
    !=================================================!
    !  @subroutine: compute_Vsamp                                         
    !  @Purpose:
    !            Performs computation of weather radar
    !            signal samples and stores them in
    !            member array m_Vsamp
    ! 
    !=================================================! 
    subroutine compute_Vsamp(this,ierr)
          !use mod_constants, only : LAM_ISQRT2,LAM_ZC

          implicit none
          class(RSWSamples_t),       intent(inout)          :: this
          integer(i4),               intent(inout)          :: ierr
          logical(i4), dimension(5), intent(inout),optional :: fpflags
          logical(i4),               intent(in)             :: verbose
          ! Locals
          complex(r4) :: tmp
          integer(i4) :: i,j

          ! Start of executable statements
          tmp = (0.0_r4,0.0_r4)
          do j = 1, this%m_size
!DIR$     SIMD
              do i = 1, this%m_size
                  tmp = tmp+(this%m_Ai(j)*this%m_Wi(j)*this%m_cexpt(i,j))
                  tmp = tmp*0.70710678118654752440084_r4
                  this%m_Vsamp(j) = tmp
              end do
          end do
    end subroutine
    
    !=================================================!
    !  @subroutine: compute_Vsampavg                                         
    !  @Purpose:
    !            Performs computation of weather radar
    !            signal samples averaged over frequency cycle
    !            and stores them in member array mVsampavg.
    !
    !=================================================!
    subroutine compute_Vsampavg(this,ierr)
         
          implicit none
          class(RSWSamples_t),         intent(inout)         :: this
          integer(i4),               intent(inout)           :: ierr
          ! locals
          complex(r4) :: tmp
          integer(i4) :: i,j
          tmp = (0.0_r4,0.0_r4)
          do j = 1, this%m_size
!DIR$     SIMD
              do i = 1, this%m_size
                  tmp = tmp+(this%m_Ai(j)*CONJG(this%m_Ai(j))* &
                      this%m_Wi(j)*CONJG(this%m_Wi(j))*this%m_cexpt(i,j))
                  tmp = tmp*0.5_r4
                  this%m_Vsampavg(j) = tmp
              end do
          end do
    end subroutine
    
      !=================================================!
    !  @subroutine: read_samples                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine read_samples(this,form,unit,ioerr)
          implicit none
          class(RSWSamples_t), intent(in)     :: this
          character(len=*),     intent(in)    :: form
          integer(i4),        intent(in)    :: unit
          integer(i4),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
     !=================================================!
    !  @subroutine: write_samples                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine write_samples(this,form,unit,ioerr)
          implicit none
          class(RSWSamples_t), intent(in)     :: this
          character(len=*),     intent(in)    :: form
          integer(i4),        intent(in)    :: unit
          integer(i4),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !===========================================
    !    Module operators
    !===========================================
    
    !==============================================
    !   subroutine: assignment (=)
    !==============================================
    subroutine assign_samples(this,other)
          implicit none
          type(RSWSamples_t), intent(inout) :: this
          type(RSWSamples_t), intent(in)    :: other
          ! Locals
          if(LOC(this) == LOC(other)) then
             return
          end if
          this%m_nechoes    = other%m_nechoes
          this%m_size       = other%m_size
          this%m_scatnum    = other%m_scatnum
          this%m_Ts         = other%m_Ts
          this%m_ths        = other%m_ths
          this%m_sampID     = other%m_sampID
          this%m_Ai         = other%m_Ai
          this%m_Wi         = other%m_Wi
          this%m_cexpt      = other%m_cexpt
          this%m_Vsamp      = other%m_Vsamp
          this%m_Vsampavg   = other%m_Vsampavg
          this%m_isbuilt    = other%m_isbuilt
    end subroutine
    
   
    
end  module rwsamples
    
