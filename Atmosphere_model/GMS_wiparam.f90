
module wiparam

       
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'wiparam'
 !          
 !          Purpose:
 !                   Parametrization of Water and Ice.
 !                   
 !                     
 !          History:
 !                        Date: 23-09-2017
 !                        Time: 12:15 GMT+2
 !
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          References:
 !         
 !                     "Thermodynamics, kinetics and microphysics of clouds"
 !                      by Vitaliy I. Khvorostyanov & Judith A. Curry
 !                      Page 108, chapter 4.4
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    public
    use module_kinds,    only : i4,sp
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
           
  
   
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(i4), parameter, public :: WIPARAM_MAJOR = 1
    
    ! Version minor
    integer(i4), parameter, public :: WIPARAM_MINOR = 0
    
    ! Version micro
    integer(i4), parameter, public :: WIPARAM_MICRO = 0
    
    ! Module full version
    integer(i4), parameter, public :: WIPARAM_FULLVER = 1000*WIPARAM_MAJOR+100*WIPARAM_MINOR+ &
                                                              10*WIPARAM_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: WIPARAM_CREATE_DATE = "23-09-2017 12:15 +00200 (SAT 23 SEP 2017 GMT+2)"
    
    ! Module compilation date (should be set after successful build date/time)
    character(*),  parameter, public :: WIPARAM_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: WIPARAM_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: WIPARAM_DESCRIPT = "Water and Ice saturated pressure computation."
    
   
    
    
   
    
   
    
    !============================================50
    !   Type: water_ice_thermodynamic_t
    !============================================50  
    type, public :: water_ice_thermodynamic_t
        
          private
          
          ! indices  (conforms to WRF model)
          ! m_ims,m_ime,m_kms,m_kme
          ! m_jms,m_jme,m_ids,m_ide
          ! m_kds,m_kde,m_jds,m_jde
          ! m_its,m_ite,m_kts,m_kte
          ! m_jts,m_jte
          integer(sp), dimension(A3DNidx) :: m_idx
          
          ! Saturated pressure of water i.e. m_ews in (Pa)
          real(sp), allocatable, dimension(:,:,:) :: m_ews
!DIR$     ATTRIBUTES ALIGN : 32 :: m_ews
          
          ! Saturated over ice vapour pressure i.e. m_eis in (Pa)
          real(sp), allocatable, dimension(:,:,:) :: m_eis
!DIR$     ATTRIBUTES ALIGN : 32 :: m_eis
          
          ! Heat capacity of water i.e. m_cw in (cal/g)
          real(sp), allocatable, dimension(:,:,:) :: m_cw
!DIR$     ATTRIBUTES ALIGN : 32 :: m_cw
          
          ! Isobaric molar heat capacity of ice i.e. m_cpim in (J mol^-1 K^-1)
          real(sp), allocatable, dimension(:,:,:) :: m_cpim
!DIR$     ATTRIBUTES ALIGN : 32 :: m_cpim
          
          ! Difference in molar heat capacity betweem water and ice m_dcpm in (J mol^-1 K^-1)
          real(sp), allocatable, dimension(:,:,:) :: m_dcpm
!DIR$     ATTRIBUTES ALIGN : 32 :: m_dpcm
          
          ! Latent heat of evaporation i.e. m_Lv in (cal/g) , T(C)
          real(sp), allocatable, dimension(:,:,:) :: m_Lv
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Lv
          
          ! Molar latent heat of sublimation i.e. m_Ls in (J mol^-1), T(K)
          ! fitted to integral for T(K) > 30.0
          real(sp), allocatable, dimension(:,:,:) :: m_Ls
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Ls
          
          ! Specific melting heat i.e. m_Lm  in (cal/g)
          ! for range T(K) > 229
          real(sp), allocatable, dimension(:,:,:) :: m_Lm
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Lm
          
          ! Surface tension between water and air in (erg/cm^2) i.e. m_gamwa at
          ! T(C) > -40 , Prupacher and Klent (1997)
          real(sp), allocatable, dimension(:,:) :: m_gamwa
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamwa
          
          ! Surface tension between ice and water in (erg/cm^2) i.e. m_gamwi
          ! for following ranges:
          ! 1) -36 < T(C) < 0
          ! 2) -44 < T(C) < -36
          real(sp), allocatable, dimension(:,:) :: m_gamwi
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamwi
          
          ! Surface tension between ice and air/vapour in (erg/cm^2) i.e. m_gamiv
          real(sp), allocatable, dimension(:,:) :: m_gamiv
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamiv
          
          ! Density of water in (g/cm^3) i.e. m_psiw
          ! for 0 <= T(C) < 100, Kell (1975)
          ! for -33 <= T(C) <= 0, Hare & Sorensen (1987)
          real(sp), allocatable, dimension(:,:,:) :: m_psiw
!DIR$     ATTRIBUTES ALIGN : 32 :: m_psiw
          
          ! Density of ice (hexagonal ice, Ih) in (g/cm^3) i.e. m_psii
          ! for -180 <= T(C) <= 0, Pruppacher & Klett (1997)
          real(sp), allocatable, dimension(:,:,:) :: m_psii
!DIR$     ATTRIBUTES ALIGN : 32 :: m_psii
          
          ! Built indicator (logical)
          logical(i4) :: m_isbuilt
          
    contains
          
          !======================================50
          !  Constructor and Destructor
          !======================================50
          
          procedure, pass(this), public :: init
          
          procedure, pass(this), public :: copy
          
          procedure, pass(this), public :: destroy
          
         
          
          !============================================56
          !     Computational procedures
          !============================================56
          
          procedure, pass(this), public :: compute_ews
          
          procedure, pass(this), public :: compute_eis
          
          procedure, pass(this), public :: compute_cw
          
          procedure, pass(this), public :: compute_cpim
          
          procedure, pass(this), public :: compute_dcpm
          
          procedure, pass(this), public :: compute_Lv
          
          procedure, pass(this), public :: compute_Ls
          
          procedure, pass(this), public :: compute_Lm
          
          procedure, pass(this), public :: compute_gamwa
          
          procedure, pass(this), public :: compute_gamwi
          
          procedure, pass(this), public :: compute_gamiv
          
          procedure, pass(this), public :: compute_psiw
          
          procedure, pass(this), public :: compute_psii
          
    end type water_ice_thermodynamic_t
    
    contains
    
    !============================================50
    !      Implementation
    !============================================50
    
    !========================================================62
    !  @subroutine:
    !               init
    !  @Purpose:
    !               Default initialization of type
    !               bound members scalars and allocata-
    !               ble arrays
    ! 
    !========================================================62
    subroutine init(this,idx,ierr,init)
          implicit none
          class(water_ice_thermodynamic_t),  intent(inout) :: this
          integer(sp), dimension(A3DNidx),   intent(in)    :: idx
          integer(i4),                       intent(inout) :: ierr
          logical(i4),                       intent(in)    :: init
          ! Locals
          integer(sp)      :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(sp)      :: jj,kk,ii
!DIR$     ENDIF
          ! Start of executable statements
          
          if(this%m_isbuilt .EQ. .true. ) then
              ierr = -1
              return
          end if
          ! Begin construction
          this%m_idx(:) = idx(:)
          ! Begin arrays allocation
          associate(d1s=>this%m_idx(1), & ! ims
                    d1e=>this%m_idx(2), & ! ime
                    d2s=>this%m_idx(3), & ! kms
                    d2e=>this%m_idx(4), & ! kme
                    d3s=>this%m_idx(5), & ! jms
                    d3e=>this%m_idx(6) )  ! jme
              allocate(this%m_ews(d1s:d1e,d2s:d2e,d3s:d3e),   &
                       this%m_eis(d1s:d1e,d2s:d2e,d3s:d3e),   &
                       this%m_cw(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_cpim(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_dcpm(d2s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_Lv(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_Ls(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_Lm(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_gamwa(d1s:d1e,d3s:d3e),         &
                       this%m_gamwi(d1s:d1e,d3s:d3e),         &
                       this%m_gamiv(d1s:d1e,d3s:d3e),         &
                       this%m_psiw(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_psii(d1s:d1e,d2s:d2e,d3s:d3e))
                       
         end associate
         if(init) then
                 ! Arrays initialization
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
                do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE ! jts,jte
                   do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE ! kts,kte
                      do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE ! its, ite
                         do jj = j, DEFAULT_BLOCK_SIZE
                            do kk = k, DEFAULT_BLOCK_SIZE
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=4))                            
                               do ii = i, DEFAULT_BLOCK_SIZE
                                       this%m_ews(ii,kk,jj)   = 0._sp
                                       this%m_eis(ii,kk,jj)   = 0._sp
                                       this%m_cw(ii,kk,jj)    = 0._sp
                                       this%m_cpim(ii,kk,jj)  = 0._sp
                                       this%m_dcpm(ii,kk,jj)  = 0._sp
                                       this%m_Lv(ii,kk,jj)    = 0._sp
                                       this%m_Ls(ii,kk,jj)    = 0._sp
                                       this%m_Lm(ii,kk,jj)    = 0._sp
                                       this%m_gamwa(ii,jj)    = 0._sp
                                       this%m_gamwi(ii,jj)    = 0._sp
                                       this%m_gamiv(ii,jj)    = 0._sp
                                       this%m_psiw(ii,kk,jj)  = 0._sp
                                       this%m_psii(ii,kk,jj)  = 0._sp
                               end do
                             end do
                          end do
                       end do
                    end do
                end do
!DIR$    ELSE
               do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))                 
                      do i = this%m_idx(13), this%m_idx(14)
                             this%m_ews(i,k,j)   = 0._sp
                             this%m_eis(i,k,j)   = 0._sp
                             this%m_cw(i,k,j)    = 0._sp
                             this%m_cpim(i,k,j)  = 0._sp
                             this%m_dcpm(i,k,j)  = 0._sp
                             this%m_Lv(i,k,j)    = 0._sp
                             this%m_Ls(i,k,j)    = 0._sp
                             this%m_Lm(i,k,j)    = 0._sp
                             this%m_gamwa(i,j)   = 0._sp
                             this%m_gamwi(i,j)   = 0._sp
                             this%m_gamiv(i,j)   = 0._sp
                             this%m_psiw(i,k,j)  = 0._sp
                             this%m_psii(i,k,j)  = 0._sp
                      end do
                  end do
               end do
!DIR$    ENDIF
          end if
              this%m_isbuilt = .true.
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               copy
    !  @Purpose:
    !               Copy-constructs  derived type state
    ! 
    !               
    !========================================================62
    subroutine copy(this,other,ierr)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          class(water_ice_thermodynamic_t), intent(in)    :: other
          integer(i4),                    intent(inout) :: ierr
          ! Locals
         ! Start of executable statemetns
         
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true. ) then
             ierr = -1
              return
          end if
          ! Begin construction   
          this%m_idx(:) = other%m_idx(:)
          this%m_ews    = other%m_ews
          this%m_eis    = other%m_eis
          this%m_cw     = other%m_cw
          this%m_cpim   = other%m_cpim
          this%m_dcpm   = other%m_dcpm
          this%m_Lv     = other%m_Lv
          this%m_Ls     = other%m_Ls
          this%m_Lm     = other%m_Lm
          this%m_gamwa  = other%m_gamwa
          this%m_gamwi  = other%m_gamwi
          this%m_gamiv  = other%m_gamiv
          this%m_psiw   = other%m_psiw
          this%m_psii   = other%m_psii
          this%m_isbuilt = other%m_isbuilt
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               destroy
    !  @Purpose:
    !               Destroys (deallocates)  derived type arrays
    !               and sets isbuilt indicator to false.
    !               indexing static array is nullified.
    !  
    !========================================================62
    subroutine destroy(this,ierr)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          integer(i4),                    intent(in)    :: ierr
          ! Locals
          ! Start of executable statements
       
          if(this%m_isbuilt .EQ. .false.) then
             ierr = -1
             return
          end if
          ! Begin destruction
          this%m_idx(:) = 0
          deallocate(this%m_ews,   &
                     this%m_eis,   &
                     this%m_cw,    &
                     this%m_cpim,  &
                     this%m_dcpm,  &
                     this%m_Lv,    &
                     this%m_Ls,    &
                     this%m_Lm,    &
                     this%m_gamwa, &
                     this%m_gamwi, &
                     this%m_gamiv, &
                     this%m_psiw,  &
                     this%m_psii)
                   
        this%m_isbuilt = .false.
    end subroutine
    
    
    !======================================================60
    !               Computational procedures
    !======================================================60
    
    !========================================================62
    !  @subroutine:
    !               compute_ews
    !  @Purpose:
    !               Performs computation of saturated pressure
    !               of water.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    !  
    !               
    !========================================================62
    subroutine compute_ews(this,t3d,ews,use_blocking )
          implicit none
          class(water_ice_thermodynamic_t),      intent(inout) :: this
          real(sp), contiguous ,dimension(:,:,:),intent(in)    :: t3d
          real(sp), contiguous ,dimension(:,:,:),intent(out)   :: ews
          logical(kind=i4),                      intent(in)    :: use_blocking
          ! Locals
         
          real(sp) :: T1,T2,T3
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
         ! Wexler (1976) calculated coefficients of saturated pressure of water  i.e. e_ws
          real(sp), parameter, private :: a0 =  -2991.2729_sp
          real(sp), parameter, private :: a1 =  -6017.0128_sp
          real(sp), parameter, private :: a2 =   18.87643854_sp
          real(sp), parameter, private :: a3 =  -0.028354721_sp
          real(sp), parameter, private :: a4 =   0.000017838301_sp
          real(sp), parameter, private :: a5 =  -0.00000000084150417_sp
          real(sp), parameter, private :: a6 =   0.00000000000044412543_sp
          real(sp), parameter, private :: a7 =   2.858487_sp
          ! Start of executable statements
         
          T1 = 0._sp
          T2 = 0._sp
          T3 = 0._sp
          if(use_blocking==.true.) then
          !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
          !DIR$     ASSUME_ALIGNED ews(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                  do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                              !dir$ vector aligned
                              !dir$ ivdep
                              !dir$ vector vectorlength(4)
                              !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                     call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                     T1 = t3d(ii,kk,jj)
                                     T2 = t3d(ii,kk,jj)
                                     T3 = t3d(ii,kk,jj)
                                     T1 = (a0*T1**-2)+(a1*T1**-1)
                                     T2 = a2+(a3+T2*(a4+T2*(a5+T2*(a6+T2))))
                                     T3 = a7*LOG10(T3)
                                     ews(ii,kk,jj) = EXP(T1+T2+T3)
                                  end do
                               end do
                           end do
                       end do
                   end do
               end do
          else
                !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
                !DIR$     ASSUME_ALIGNED ews(1,1,1) : 64
               do j = this%m_idx(17), this%m_idx(18)   ! jts, jte
                  do k = this%m_idx(15), this%m_idx(16)    ! kts, kte
                     !dir$ vector aligned
                     !dir$ ivdep
                     !dir$ vector vectorlength(4)
                     !dir$ vector always         
                     do i = this%m_idx(13), this%m_idx(14)

                         call MM_PREFETCH(t3d(i+2,k,j),1)
                         T1 = t3d(i,k,j)
                         T2 = t3d(i,k,j)
                         T3 = t3d(i,k,j)
                         T1 = (a0*T1**-2)+(a1*T1**-1)
                         T2 = a2+(a3+T2*(a4+T2*(a5+T2*(a6+T2))))
                         T3 = a7*LOG10(T3)
                         ews(i,k,j) = EXP(T1+T2+T3)
                   end do
                end do
            end do
        endif 
    end subroutine
                           
    !========================================================62
    !  @subroutine:
    !               compute_eis
    !  @Purpose:
    !               Performs computation of saturated over
    !               ice vapour pressure.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    ! 
    !               
    !========================================================62                       
    subroutine compute_eis(this,t3d,eis,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),       intent(inout)    :: this
          real(sp), contiguous, dimension(:,:,:), intent(in)       :: t3d
          real(sp), contiguous, dimension(:,:,:), intent(out)      :: eis
          logical(kind=i4),                       intent(in)       :: use_blocking     
        
          real(sp) :: T1,T2,T3
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii

          ! Hyland and Wexler (1983) coefficients of saturated over ice vapour pressure i.e. e_is
          ! for T (K) range 173.16<T<273.16
          real(sp), parameter, private :: is_a0 = -5674.5359_sp
          real(sp), parameter, private :: is_a1 = 6.3925247_sp
          real(sp), parameter, private :: is_a2 = -0.009677843_sp
          real(sp), parameter, private :: is_a3 =  0.00000062215701_sp
          real(sp), parameter, private :: is_a4 =  0.0000000020747825_sp
          real(sp), parameter, private :: is_a5 =  -0.0000000000009484024_sp
          real(sp), parameter, private :: is_a6 =  4.1635019_sp
          ! Murphy and Koop (2005) parametrization of e_is for T(K) > 110K
          real(sp), parameter, private :: is2_a0 = 9.550426_sp
          real(sp), parameter, private :: is2_a1 = -5723.265_sp
          real(sp), parameter, private :: is2_a2 = 3.53068_sp
          real(sp), parameter, private :: is2_a3 = -0.00728332_sp
          ! Start of executable statements
         
         T1 = 0._sp
         T2 = 0._sp
         T3 = 0._sp
       
         if(use_blocking) then
             !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
             !DIR$     ASSUME_ALIGNED eis(1,1,1) : 64
            do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
               do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                   do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                       do jj = j, DEFAULT_BLOCK_SIZE
                           do kk = k, DEFAULT_BLOCK_SIZE
                              !dir$ vector aligned
                              !dir$ ivdep
                              !dir$ vector vectorlength(4)
                              !dir$ vector always
                               do ii = i, DEFAULT_BLOCK_SIZE
                                call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                if(t3d(ii,kk,jj)>=173.16_sp .AND. t3d(ii,kk,jj)<=273.16_sp) then
                                    T1 = t3d(ii,kk,jj)
                                    T2 = t3d(ii,kk,jj)
                                    T3 = t3d(ii,kk,jj)
                                    T1 = is_a0/T1
                                    T2 = is_a1+(is_a2+T2*(is_a3+T2*(is_a4+T2*(is_a5+T2))))
                                    T3 = is_a6*DLOG10(T3)
                                    eis(ii,kk,jj) = EXP(T1+T2+T3)
                                  else if(t3d(ii,kk,jj)>110._sp .AND. t3d(ii,kk,jj)<173.16_sp) then
                                     T1 = t3d(ii,kk,jj)
                                     T2 = t3d(ii,kk,jj)
                                     T1 = is2_a0+(is2_a1/T1)
                                     T2 = (is2_a2*LOG(T2))+(is2_a3*T2)
                                     eis(ii,kk,jj) = EXP(T1+T2)
                                  end if
                               end do
                            end do
                        end do
                    end do
                end do
            end do
        else
            !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
            !DIR$     ASSUME_ALIGNED eis(1,1,1) : 64
           do j = this%m_idx(17), this%m_idx(18)
               do k = this%m_idx(15), this%m_idx(16)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                   do i = this%m_idx(13), this%m_idx(14)
                      call MM_PREFETCH(t3d(i+2,k,j),1)
                      if(t3d(i,k,j)>=173.16_sp .AND. t3d(i,k,j)<=273.16_sp) then
                         T1 = t3d(i,k,j)
                         T2 = t3d(i,k,j)
                         T3 = t3d(i,k,j)
                         T1 = is_a0/T1
                         T2 = is_a1+(is_a2+T2*(is_a3+T2*(is_a4+T2*(is_a5+T2))))
                         T3 = is_a6*DLOG10(T3)
                         eis(i,k,j) = EXP(T1+T2+T3)
                      else if(t3d(i,k,j)>110._sp .AND. t3d(i,k,j)<173.16_sp) then
                          T1 = t3d(ii,kk,jj)
                          T2 = t3d(ii,kk,jj)
                          T1 = is2_a0+(is2_a1/T1)
                          T2 = (is2_a2*LOG(T2))+(is2_a3*T2)
                          eis(i,k,j) = EXP(T1+T2)
                      end if
                  end do
              end do
            end do
        end if 
   end subroutine
    
    !========================================================62
    !  @subroutine:
    !               compute_cw
    !  @Purpose:
    !               Performs computation of water heat
    !               capacity
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    !  
    !              
    !========================================================62
    subroutine compute_cw(this,t3d,cw,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(sp), contiguous,dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous,dimension(:,:,:),     intent(out)   :: cw
          logical(kind=i4),                          intent(in)    :: use_blocking
          ! Locals
          real(sp) :: T1,T2
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: cw0 = 0.9979_sp,       &
                                   a1  = 0.0000031_sp,    &
                                   a2  = 0.0000000038_sp, &
                                   Tc1 = 35.0_sp
          real(sp), parameter :: a00 = 1.000938_sp,      &
                                   a11 = -0.0270052_sp,    &
                                   a22 = -0.000023235_sp,  &
                                   a33 =  0.0000043778_sp, &
                                   a44 =  0.00000027316_sp
          ! Start of executable satements
         T1 = 0._sp
         T2 = 0._sp
         if(use_blocking==.true.) then
            !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
            !DIR$     ASSUME_ALIGNED cw(1,1,1) : 64
            do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                    do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                        do jj = j, DEFAULT_BLOCK_SIZE
                            do kk = k, DEFAULT_BLOCK_SIZE
                                 !dir$ vector aligned
                                 !dir$ ivdep
                                 !dir$ vector vectorlength(4)
                                 !dir$ vector always
                                do ii = i, DEFAULT_BLOCK_SIZE
                                    call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                    if(t3d(ii,kk,jj)>=0._sp .AND. t3d(ii,kk,jj)<=Tc1) then
                                        T1 = a1*(t3d(ii,kk,jj)-Tc1)**2
                                        T2 = a2*(t3d(ii,kk,jj)-Tc1)**4
                                        cw(ii,kk,jj) = cw0+T1+T2
                                    else if(t3d(ii,kk,jj)>= -37._sp .AND. t3d(ii,kk,jj)<=0._sp) then
                                        T1 = t3d(ii,kk,jj)
                                        cw(ii,kk,jj) = a00+T1*(a11+T1*(a22+T1*(a33+T1)))
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end do
      else
            !DIR$     ASSUME_ALIGNED t3d(1,1,1) : 64
            !DIR$     ASSUME_ALIGNED eis(1,1,1) : 64
            do j = this%m_idx(17), this%m_idx(18)
                do k = this%m_idx(15), this%m_idx(16)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                    do i = this%m_idx(13), this%m_idx(14)
                        call MM_PREFETCH(t3d(i+2,k,j),1)
                        if(t3d(i,k,j)>=0._sp .AND. t3d(i,k,j)<=Tc1) then
                            T1 = a1*(t3d(i,k,j)-Tc1)**2
                            T2 = a2*(t3d(i,k,j)-Tc1)**4
                            cw(i,k,j) = cw0+T1+T2
                         else if(t3d(i,k,j)>= -37._sp .AND. t3d(i,k,j)<=0._sp) then
                            T1 = t3d(i,k,j)
                            cw(i,k,j) = a00+T1*(a11+T1*(a22+T1*(a33+T1)))
                         end if
                    end do
                 end do
            end do
       end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_cpim
    !  @Purpose:
    !               Performs computation of molar ice heat capacity.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_cpim.
    !               This is not checked.
    !  
    !              
    !========================================================62                      
    subroutine compute_cpim(this,t3d,cpim,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(sp), contiguous,dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous,dimension(:,:,:),     intent(out)   :: cpim
          logical(i4),                               intent(in)    :: use_blocking
               
          real(sp) :: T1,T2
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii

          real(sp), parameter :: a1 = -2.0572_sp
          real(sp), parameter :: a2 = 0.14644_sp
          real(sp), parameter :: a3 = 0.06163_sp
          real(sp), parameter :: a4 = 125.1_sp
          ! Start of executable statements
          T1 = 0._sp
          T2 = 0._sp
       
          if(use_blocking==.true.) then
             !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
             !DIR$     ASSUME_ALIGNED cpim(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                  do k = this%m_idx(15), this%m_idx(16), DEFUALT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                !dir$ vector aligned
                                !dir$ ivdep
                                !dir$ vector vectorlength(4)
                                !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                     call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                     T1 = t3d(ii,kk,jj)
                                     T2 = t3d(ii,kk,jj)
                                     T1 = a1+a2*T1+a3*T1
                                     T2 = EXP(-(T2/a4)**2)
                                     cpim(ii,kk,jj) = T1+T2
                                  end do
                              end do
                           end do
                       end do
                   end do
               end do
        else
               do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
                   !dir$ vector aligned
                   !dir$ ivdep
                   !dir$ vector vectorlength(4)
                   !dir$ vector always
                      do i = this%m_idx(13), this%m_idx(14)
                                  call MM_PREFETCH(t3d(i+2,k,j),1)
                                  T1 = t3d(i,k,j)
                                  T2 = t3d(i,k,j)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = DEXP(-(T2/a4)**2)
                                  this%m_cpim(i,k,j) = T1+T2   
                      end do
                   end do
               end do
        end if
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_dcpm
    !  @Purpose:
    !               Performs computation of difference in
    !               in molar heat capacity between vapour and ice 
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    ! 
    !              
    !========================================================62  
    subroutine compute_dcpm(this,t3d,dcpm,use_blocking )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(sp), contiguous,dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous,dimension(:,:,:),     intent(out)   :: dcpm
          logical(kind=i4),                          intent(in)    :: use_blocking
          ! Locals
          real(sp) :: T1,T2
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: a1 = -35.319_sp
          real(sp), parameter :: a2 = 0.14457_sp
          real(sp), parameter :: a3 = 0.06155_sp
          real(sp), parameter :: a4 = 129.85_sp
          ! Strat of executable statements
         
          T1 = 0._sp
          T2 = 0._sp
          if(use_blocking==.true.) then
             !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
             !DIR$     ASSUME_ALIGNED dcpm(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                 do k = this%m_idx(15), this%m_idx(16), DEFUALT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                    call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                    T1 = t3d(ii,kk,jj)
                                    T2 = t3d(ii,kk,jj)
                                    T1 = a1+a2*T1+a3*T1
                                    T2 = EXP(-(T2/a4)**2)
                                    dcpm(ii,kk,jj) = T1+T2
                                 end do
                              end do
                          end do
                      end do
                  end do
              end do
         else
              !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
              !DIR$     ASSUME_ALIGNED dcpm(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
                       !dir$ vector aligned
                       !dir$ ivdep
                       !dir$ vector vectorlength(4)
                       !dir$ vector always
                      do i = this%m_idx(13), this%m_idx(14)
                                  call MM_PREFETCH(t3d(i+2,k,j),1)
                                  T1 = t3d(i,k,j)
                                  T2 = t3d(i,k,j)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = EXP(-(T2/a4)**2)
                                  cpim(i,k,j) = T1+T2   
                       end do
                   end do
               end do
         end if
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_Lv
    !  @Purpose:
    !               Performs computation of evaporation latent heat
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    ! 
    !              
    !========================================================62                          
    subroutine compute_Lv(this,t3d,Lv,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)   :: Lv
          logical(kind=i4),                           intent(in)    :: use_blocking
          ! Locals
      
          real(sp) :: tmp
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: c1 = 597.3_sp, &
                                 c2 = 0.561_sp
          real(sp), parameter :: a0 = -1412.3_sp,   &
                                   a1 = -338.82_sp,   &
                                   a2 = -122.347_sp,  &
                                   a3 = -0.7526_sp,   &
                                   a4 = -0.011595_sp, &
                                   a5 = -0.00007313_sp
          real(sp), parameter :: t1 = -44._sp, &   ! All those constants(t1,t2,t3) expresed in Celsius
                                   t2 = -20._sp, &
                                   t3 =  40._sp
       
          if(use_blocking==.true.) then 
             !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
             !DIR$     ASSUME_ALIGNED Lv(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                 do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                     call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                     if(t3d(ii,kk,jj)>=t1 .AND. t3d(ii,kk,jj)<=t2) then
                                         tmp = t3d(ii,kk,jj)
                                         tmp = a0+tmp*(a1+tmp*(a2+tmp*(a3+tmp*(a4+tmp*(a5+tmp)))))
                                         Lv(ii,kk,jj) = tmp
                                      else if(t3d(ii,kk,jj)>t2 .AND. t3d(ii,kk,jj)<=t3) then
                                         Lv(ii,kk,jj) = c1-c2*t3d(ii,kk,jj)
                                      end if
                                  end do
                               end do
                            end do
                         end do
                      end do
               end do
         else
                  !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                  !DIR$     ASSUME_ALIGNED Lv(1,1,1) : 64
               do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
                       !dir$ vector aligned
                       !dir$ ivdep
                       !dir$ vector vectorlength(4)
                       !dir$ vector always
                      do i = this%m_idx(13), this%m_idx(14)
                             call MM_PREFETCH(t3d(i+2,k,j),1)
                             if(t3d(i,k,j)>=t1 .AND. t3d(i,k,j)<=t2) then
                                 tmp = t3d(i,k,j)
                                 tmp = a0+tmp*(a1+tmp*(a2+tmp*(a3+tmp*(a4+tmp*(a5+tmp)))))
                                 Lv(i,k,j) = tmp
                             else if(t3d(i,k,j)>t2 .AND. t3d(i,k,j)<=t3) then
                                 Lv(i,k,j) = c1-c2*t3d(i,k,j)
                             end if   
                        end do
                     end do
                 end do
          end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_Ls
    !  @Purpose:
    !               Performs computation of sublimation
    !               molar latent heat.       
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    !  
    !              
    !========================================================62 
    subroutine compute_Ls(this,t3d,Ls,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),           intent(inout) :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)     :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)    :: Ls
          logical(i4),                                intent(in)     :: use_blocking
          real(sp) :: tmp1,tmp2
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: b0 = 46782.5_sp, &
                                   b1 = 35.8925_sp, &
                                   b2 = -0.07414_sp, &
                                   b3 = 541.5_sp,    &
                                   b4 = 0.00808080808080808080808_sp
          tmp1 = 0._sp
          tmp2 = 0._sp
          if(use_blocking=.true.) then
              !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
              !DIR$     ASSUME_ALIGNED Ls(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_LOOP_BLOCKING
                 do k = this%m_idx(15), this%m_idx(16), DEFAULT_LOOP_BLOCKING
                     do k = this%m_idx(13), this%m_idx(14), DEFAULT_LOOP_BLOCKING
                         do jj = j, DEFAULT_LOOP_BLOCKING
                             do kk = k, DEFAULT_LOOP_BLOCKING
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                 do ii = i, DEFAULT_LOOP_BLOCKING
                                     call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                     tmp1 = t3d(ii,kk,jj)
                                     tmp2 = t3d(ii,kk,jj)
                                     tmp1 = b0+b1+tmp1*(b2+tmp2)
                                     tmp2 = b3*EXP(-(tmp2*b4)**2)
                                     Ls(ii,kk,jj) = tmp1+tmp2
                                 end do
                              end do
                          end do
                      end do
                  end do
              end do
           else
                   !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                   !DIR$     ASSUME_ALIGNED Ls(1,1,1) : 64
                do j = this%m_idx(17), this%m_idx(18)
                    do k = this%m_idx(15), this%m_idx(16)
                            !dir$ vector aligned
                            !dir$ ivdep
                            !dir$ vector vectorlength(4)
                            !dir$ vector always
                        do i = this%m_idx(13), this%m_idx(14)
                           call MM_PREFETCH(t3d(i+2,k,j),1)
                           tmp1 = t3d(i,k,j)
                           tmp2 = t3d(i,k,j)
                           tmp1 = b0+b1+tmp1*(b2+tmp2)
                           tmp2 = b3*EXP(-(tmp2*b4)**2)
                           Ls(i,k,j) = tmp1+tmp2  
                        end do
                    end do
                end do
       endif
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_Lm
    !  @Purpose:
    !               Performs computation of melting heat point.
    !                    
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    ! 
    !              
    !========================================================62                        
    subroutine compute_Lm(this,t3d,Lm,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),           intent(inout)  :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)     :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)    :: Lm
          logical(i4),                                intent(in)     :: use_blocking
          ! Locals
          
          real(sp) :: tmp1
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: a0 = 79.7_sp, &
                                   a1 = -0.12_sp,&
                                   a2 = -0.080481_sp, &
                                   a3 = -0.0032376_sp, &
                                   a4 = -0.0000425533_sp
          ! Start of executable statements
         tmp1 = 0._sp
         if(use_blocking=.true.) then
              !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
              !DIR$     ASSUME_ALIGNED Lm(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                    do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                        do jj = j, DEFAULT_BLOCK_SIZE
                            do kk = k, DEFAULT_BLOCK_SIZE
                                 !dir$ vector aligned
                                 !dir$ ivdep
                                 !dir$ vector vectorlength(4)
                                 !dir$ vector always
                                do ii = i, DEFAULT_BLOCK_SIZE
                                    call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                    tmp1 = t3d(ii,kk,jj)
                                    tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1))))
                                    Lm(ii,kk,jj) = tmp1
                                end do
                            end do
                         end do
                     end do
                  end do
              end do
         else
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED Lm(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18)
                 do k = this%m_idx(15), this%m_idx(16)
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(4)
                        !dir$ vector always
                    do i = this%m_idx(13), this%m_idx(14)
                        call MM_PREFETCH(t3d(i+2,k,j),1)
                        tmp1 = t3d(i,k,j)
                        tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1))))
                        Lm(i,k,j) = tmp1      
                    end do
                 end do
               end do
         endif
       
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_gamwa
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem water and air.     
    !               
    !  
    !              
    !========================================================62  
    subroutine compute_gamwa(this,t3d,gamwa,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),          intent(inout)  :: this
          real(sp), contiguous,dimension(:,:,:),     intent(in)     :: t3d
          real(sp), contiguous,dimension(:,:,:),     intent(out)    :: gamwa
          logical(i4),                               intent(in)     :: use_blocking
          ! Locals
          
          real(sp) :: tmp1
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: a0 = 75.93_sp, &
                                   a1 = 0.115_sp, &
                                   a2 = 0.06818_sp, &
                                   a3 = 0.006511_sp, &
                                   a4 = 0.0002933_sp, &
                                   a5 = 0.000006283_sp, &
                                   a6 = 0.00000005285_sp
          ! Start of executable statements
          tmp1 = 0._sp
          if(use_blocking=.true.) then
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED gamwa(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                 do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                    call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                    tmp1 = t3d(ii,kk,jj)
                                    tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1*(a5+tmp1*(a6+tmp1))))))
                                    gamwa(ii,jj) = tmp1
                                 end do
                             end do
                         end do
                     end do
                  end do
               end do
      else
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED gamwa(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18)
                 do k = this%m_idx(15), this%m_idx(16)
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(4)
                        !dir$ vector always
                     do i = this%m_idx(13), this%m_idx(14)
                          call MM_PREFETCH(t3d(i+2,k,j),1)
                          tmp1 = t3d(i,k,j)
                          tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1*(a5+tmp1*(a6+tmp1))))))
                          gamwa(i,j) = tmp1  
                     end do
                  end do
              end do
       endif
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               compute_gamiw
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem water and ice.     
    !              
    ! 
    !              
    !========================================================62 
    subroutine compute_gamiw(this,t3d,gamiw,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)   :: gamiw
          logical(i4),                                intent(in)    :: use_blocking
          
       
          real(sp) :: tmp1
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter :: c0 = 28._sp, &
                                   c1 = 0.25_sp
          real(sp), parameter :: a0 = 189.081_sp, &
                                   a1 = 13.1625_sp, &
                                   a2 = 0.3469_sp,  &
                                   a3 = 0.003125_sp
          real(sp), parameter :: t1 = -36._sp, &
                                   t2 = 0._sp,   &
                                   t3 = -44._sp
          ! Start of executable satements
         
          tmp1 = 0._sp
         
          if(use_blocking==.true.) then
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED gamiw(1,1,1) : 64
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                 do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                 do ii = i, DEFAULT_BLOCK_SIZE
                                      call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                      if(t3d(ii,kk,jj)>=t1 .AND. t3d(ii,kk,jj)<=t2) then
                                          gamiw(ii,jj) = c0+c1*t3d(ii,kk,jj)
                                      else if(t3d(ii,kk,jj)>=t3 .AND. t3d(ii,kk,jj)<=t1) then
                                          tmp1 = t3d(ii,kk,jj)
                                          tmp1 = a0+a1+tmp1*(a2+tmp1*(a3+tmp1))
                                          gamiw(ii,jj) = tmp1
                                      end if
                                  end do
                               end do
                           end do
                      end do
                  end do
               end do
        else
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED gamiw(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18)
                   do k = this%m_idx(15), this%m_idx(16)
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(4)
                        !dir$ vector always
                      do i = this%m_idx(13), this%m_idx(14)
                          call MM_PREFETCH(t3d(i+2,k,j),1)
                          if(t3d(i,k,j)>=t1 .AND. t3d(i,k,j)<=t2) then
                             gamiw(i,j) = c0+c1*t3d(i,k,j)
                          else if(t3d(i,k,j)>=t3 .AND. t3d(i,k,j)<=t1) then
                             tmp1 = t3d(i,k,j)
                             tmp1 = a0+a1+tmp1*(a2+tmp1*(a3+tmp1))
                             gamiw(i,j) = tmp1
                          end if  
                       end do
                   end do
               end do
        endif
    end subroutine
                             
    !========================================================62
    !  @subroutine:
    !               compute_gamiv
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem ice and air or vapour.     
    !               This subroutine must be called only
    !               after saubroutines:
    !               1) compute_gamwa
    !               2) compute_gamiw
    !               finished to execute successfully,
    !               otherwise result will be obvioulsy wrong.          
    ! 
    !              
    !========================================================62 
#if 0
    subroutine compute_gamiv(this,logging,filename,append,  &
                             dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          logical(i4),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(i4),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(i4),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
         
          integer(sp) :: j,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(sp) :: jj,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(i4) :: bfail
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false.)  then
               
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:2008, In->mod_wiparam/compute_gamiv,: water_ice_thermodynamic_t in invalid state")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_gamv:2008, water_ice_thermodynamic_t in invalid state)"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_gamiv: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                  do jj = j, DEFAULT_BLOCK_SIZE
!DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))                      
                      do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                 IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(this%m_gamwa(ii+2,jj),1)
                          call MM_PREFETCH(this%m_gamiw(ii+2,jj),1)
!DIR$                 ENDIF
                          this%m_gamiv(ii,jj) = this%m_gamwa(ii,jj) + &
                                                this%m_gamiw(ii,jj)
                      end do
                  end do
              end do
          enddo
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_idx(13), this%m_idx(14)
!DIR$         IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(this%m_gamwa(i+2,j),1)
                  call MM_PREFETCH(this%m_gamiw(i+2,j),1)
!DIR$         ENDIF
                  this%m_gamiv(i,j) = this%m_gamwa(i,j) + &
                                      this%m_gamiw(i,j) 
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_gamiV: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_gamiV: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of m_gamiv: ", this%m_gamiv
          end if
   end subroutine
#endif                             
     
    !========================================================62
    !  @subroutine:
    !               compute_psiw
    !  @Purpose:
    !               Performs computation of water density   
    !               at pressure = 1 atm.
    !               Parameter array 't3d' i.e. temperature
    !                of water must conform to array m_psiw.        
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62                           
    subroutine compute_psiw(this,t3d,psiw,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),           intent(inout)  :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)     :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)    :: psiw
          logical(i4),                                intent(in)     :: use_blocking
        
          real(sp)    :: tmp1,tmp2
          integer(sp) :: j,k,i
          integer(sp) :: jj,kk,ii
          real(sp), parameter ::  t33 = -33._sp, &
                                   t0  = 0._sp,   &
                                   t100 = 100._sp
          real(sp), parameter :: a0 = 0.9998396_sp, &
                                   a1 = 0.018224944_sp, &
                                   a2 = -0.000007922210_sp, &
                                   a3 = -0.00000005544846_sp, &
                                   a4 = 0.0000000001497562_sp, &
                                   a5 = -0.0000000000003932952_sp
                                   Bpw = 0.018159725_sp
          real(sp), parameter :: a00 = 0.99986_sp, &
                                   a11 = 0.00006690_sp, &
                                   a22 = -0.000008486_sp, &
                                   a33 = 0.0000001518_sp, &
                                   a44 = -0.0000000069984_sp, &
                                   a55 = -0.00000000036449_sp, &
                                   a66 = -0.000000000007497_sp
           ! Start of executable statements
         
          if(use_blocking==.true.) then
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED psiw(1,1,1) : 64
               do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                   do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                       do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                           do jj = j, DEFAULT_BLOCK_SIZE
                               do kk = k, DEFAULT_BLOCK_SIZE
                                   !dir$ vector aligned
                                   !dir$ ivdep
                                   !dir$ vector vectorlength(4)
                                   !dir$ vector always
                                   do ii = i, DEFAULT_BLOCK_SIZE
                                      call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                      if(t3d(ii,kk,jj)>=t33 .AND. t3d(ii,kk,jj)<=t0) then
                                          tmp1 = t3d(ii,kk,jj)
                                          tmp1 = a00+tmp1*(a11+tmp1*(a22+tmp1*(a33+tmp1*(a44+tmp1*(a55+tmp1*(a66+tmp1))))))
                                          psiw(ii,kk,jj) = tmp1
                                      else if(t3d(ii,kk,jj)>=t0 .AND. t3d(ii,kk,jj)<t100) then
                                          tmp1 = 1._sp/(1._sp+Bpw*t3d(ii,kk,jj))
                                          tmp2 = t3d(ii,kk,jj)
                                          tmp2 = a0+tmp2*(a1+tmp2*(a2+tmp2*(a3+tmp2*(a4+tmp2*(a5+tmp2)))))
                                          psiw(ii,kk,jj) = tmp1*tmp2
                                      end if
                                   end do
                                end do
                            end do
                        end do
                     end do
                 end do
          else
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED psiw(1,1,1) : 64
              do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(4)
                        !dir$ vector always
                      do i = this%m_idx(13), this%m_idx(14)
                          call MM_PREFETCH(t3d(i+2,k,j),1)
                          if(t3d(i,k,j)>=t33 .AND. t3d(i,k,j)<=t0) then
                              tmp1 = t3d(i,k,j)
                              tmp1 = a00+tmp1*(a11+tmp1*(a22+tmp1*(a33+tmp1*(a44+tmp1*(a55+tmp1*(a66+tmp1))))))
                              psiw(i,k,j) = tmp1
                          else if(t3d(i,k,j)>=t0 .AND. t3d(i,k,j)<t100) then
                              tmp1 = 1._sp/(1._sp+Bpw*t3d(i,k,j))
                              tmp2 = t3d(i,k,j)
                              tmp2 = a0+tmp2*(a1+tmp2*(a2+tmp2*(a3+tmp2*(a4+tmp2*(a5+tmp2)))))
                              psiw(i,k,j) = tmp1*tmp2
                          end if 
                      end do
                  end do
              end do
          endif
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_psii
    !  @Purpose:
    !               Performs computation of ice density   
    !               
    !               Parameter array 't3d' i.e. reduced temperature
    !                of ice must conform to array m_psii.        
    !  
    !              
    !========================================================62  
    subroutine compute_psii(this,t3d,psii,use_blocking)
          implicit none
          class(water_ice_thermodynamic_t),           intent(inout) :: this
          real(sp), contiguous, dimension(:,:,:),     intent(in)    :: t3d
          real(sp), contiguous, dimension(:,:,:),     intent(out)   :: psii
          logical(i4),                                intent(in)    :: use_blocking
          real(sp)    :: Tr 
          integer(sp) :: j,k,i

          integer(sp) :: jj,kk,ii
          real(sp), parameter :: pi0 = 0.9167_sp
          real(sp), parameter :: T0  = 0.00366099212886692293611568735127_sp
          real(sp), parameter :: av0 = 1._sp, &
                                   av1 = -0.05294_sp, &
                                   av2 = -0.05637_sp, &
                                   av3 = -0.002913_sp
        
          if(use_blocking==.true.) then
                !DIR$     ASSUME_ALIGNED t3d(1,1,1)  : 64
                !DIR$     ASSUME_ALIGNED psii(1,1,1) : 64
               do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                    do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                        do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                            do jj = j, DEFAULT_BLOCK_SIZE
                                  !dir$ vector aligned
                                  !dir$ ivdep
                                  !dir$ vector vectorlength(4)
                                  !dir$ vector always
                                do kk = k, DEFAULT_BLOCK_SIZE
                                   do ii = i, DEFAULT_BLOCK_SIZE
                                       call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                                       Tr =  (t3d(ii,kk,jj)-T0)*T0
                                       psii(ii,kk,jj) = &
                                                av0+Tr*(av1+Tr*(av2+Tr*(av3+Tr)))
                                    end do
                                 end do
                             end do
                         end do
                     end do
                end do
         else
             do j = this%m_idx(17), this%m_idx(18)
                 do k = this%m_idx(15), this%m_idx(16)
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(4)
                        !dir$ vector always
                     do i = this%m_idx(13), this%m_idx(14)
                          call MM_PREFETCH(t3d(ii+2,kk,jj),1)
                           Tr =  (t3d(ii,kk,jj)-T0)*T0
                           psii(ii,kk,jj) = &
                                    av0+Tr*(av1+Tr*(av2+Tr*(av3+Tr)))   
                     end do
                 end do
             end do
         endif           
          
    end subroutine
                            
end module wiparam
