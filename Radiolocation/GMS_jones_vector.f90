
module jonesvec

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'jonesvec'
 !          
 !          Purpose:
 !                    Implements so called 'Jones Vector'
 !                   
 !                     
 !          History:
 !                        Date: 08-08-2017
 !                        Time: 12:22 GMT+2
 !
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    implicit none
    use mod_kinds, only : i4,sp
    
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(i4), parameter, public :: JONESVEC_MAJOR = 1
    
    ! Minor version
    integer(i4), parameter, public :: JONESVEC_MINOR = 0
    
    ! Micro version
    integer(i4), parameter, public :: JONESVEC_MICRO = 0
    
    ! Module/file full version
    integer(i4), parameter, public :: JONESVEC_FULLVER = 1000*JONESVEC_MAJOR+100*JONESVEC_MINOR + &
                                                               10*JONESVEC_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: JONESVEC_CREATE_DATE = "08-08-2017 12:30 +00200 (TUE 08 AUG 2017 GMT+2)"
    
    ! Module/file build date/time (should be set to appropriate values after successful build date)
    character(*),  parameter, public :: JONESVEC_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: JONESVEC_AUTHOR = "Programmer: Bernard Gingold contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: JONESVEC_DESCRIPT = "Implementation of Jones Vector"
    
    complex(sp), parameter, private :: ZC = CMPLX(0._sp,0._sp)
    
    !============================================50
    ! Type: JonesVector_t
    !============================================50
    
    type :: JonesVector_t
        
         public
         
         ! Horizontal component of Electric Field Hx
         complex(sp) :: m_h
         
         ! Vertical component of Electric Field Hy
         complex(sp) :: m_v
         
        
        !============================================================70
        !    Type-bound procedures
        !    Decalaration
        !============================================================70
         
        contains
        
        ! Destructor
         
        
        procedure, pass(this), public :: destroy_jvec
        
        ! Getters
        
        procedure, pass(this), public :: get_h
        
        procedure, pass(this), public :: get_v
        
                
        ! Computational procedures
        
        procedure, pass(this), public :: field_intensity
        
        procedure, pass(this), public :: field_atan
        
        procedure, pass(this), public :: field_pdiff
        
        procedure, pass(this), public :: polarization_degree
        
        procedure, pass(this), public :: jvec_conj
        
        ! helpers
        procedure, nopass, public :: print_jvec
        
        end type JonesVector_t
    !============================================50
    ! Declaration of module operators
    !============================================50
    
    ! Constructor bound to name JonesVector_t
        
    interface  JonesVector_t
    
        procedure :: default_jvec
        
        procedure :: create_jvec
        
        procedure :: copy_jvec  
    
    end interface
        
    ! Operator assignment
        
    interface assignment (=)
    
          module procedure assign_jvec
    
    end interface
    
    ! Operator multiplication
    
    interface operator (*)
    
          module procedure jvec_mul_jvec
          module procedure jvec_mul_complex
         
          
   end interface       
   
    
    ! Operator division
    
    interface operator (/)
    
          module procedure jvec_div_complex
         
          
    end interface
    
    ! Operator addition
    
    interface operator (+)
    
          module procedure jvec_add_jvec
         
    
    end interface
    
    ! Operator subtraction
    
    interface operator (-)
    
          module procedure jvec_negate
          module procedure jvec_sub_jvec
         
    
    end interface
    
   
    
    contains
    
    !======================================================60
    !  function: default_jvec
    !            Creates JonesVector_t with its two fields
    !            set to zero.
    !======================================================60
    type(JonesVector_t) function default_jvec()
          implicit none
          ! Start of executable statements
          default_jvec%m_h = CMPLX(0._sp,0._sp)
          default_jvec%m_v = CMPLX(0._sp,0._sp)
    end function
    
    !======================================================60
    !  function: create_jvec
    !            Creates JonesVector_t with its two fields
    !            set to specific polarization values.
    !======================================================60
    type(JonesVector_t) function create_jvec(h,v)
          implicit none
          complex(sp), intent(in) :: h,v
          ! Start of executable statements
          create_jvec%m_h = h
          create_jvec%m_v = v
    end function
    
    !======================================================60
    !  function: copy_jvec
    !            Creates JonesVector_t with its two fields
    !            set to copy of its argument.
    !======================================================60
    type(JonesVector_t) function copy_jvec(other)
          !use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          implicit none
          class(JonesVector_t), intent(in) :: other
          ! Start of executable statements
          copy_jvec%m_h = other%m_h
          copy_jvec%m_v = other%m_v
    end function

    !======================================================60
    !  subroutine: destroy_jvec
    !              Destroys JonesVector_t by settings its
    !              members to invalid values.
    !======================================================60
    subroutine destroy_jvec(this)
          !use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          use IEEE_ARITHMETIC, only : IEEE_VALUE
          implicit none
          class(JonesVector_t), intent(inout) :: this
          ! Locals
          real(sp) :: nan
          ! Start of executable statements
          nan = IEEE_VALUE(1._sp,IEEE_QUIET_NAN)
          this%m_h = CMPLX(nan,nan)
          this%m_v = CMPLX(nan,nan)
     end subroutine
    
    !======================================================60
    ! function: get_h
    !======================================================60
    pure function get_h(this) result(h)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          complex(sp) :: h
          ! Start of executable statements
          h = this%m_h
    end function
    
    !======================================================60
    ! function: get_v
    !======================================================60
    pure function get_v(this) result(v)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          complex(sp) :: v
          ! Start of executable statements
          v = this%m_v
    end function
    
   
    
    !======================================================60
    ! function: field_intensity
    !======================================================60
    pure function field_intensity(this) result(norm)
          !use mod_complex_arithm, only : cnorm
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          real(sp) :: norm,th,vh,hr,hi,vr,vi
          ! Start of executable statements
          vr   = real(this%m_v,kind=sp)
          hr   = real(this%m_h,kind=sp)
          vi   = aimag(this%m_v,kind=sp)
          vh   = sqrt(vr*vr+vi*vi)
          hi   = aimag(this%m_h,kind=sp)
          th   = sqrt(hr*hr+hi*hi)
          norm = th+vh
    end function
    
    !======================================================60
    ! function: field_atan
    !======================================================60
    pure function field_atan(this) result(ang)
          use mod_complex_arithm, only : cmag
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          real(sp) :: vr,vi,hr,hi
          real(sp) :: ang,tmp,mh,mv
          ! Start of executable statements
          vr = real(this%m_v,kind=sp)
          vi = aimag(this%m_v,kind=sp)
          mv = sqrt(vr*vr+vi*vi)
          hr = real(this%m_h,kind=sp)
          hi = aimag(this%m_h,kind=sp)
          mh = sqrt(hr*hr+hi*hi)
          tmp = mv/mh
          ang = ATAN(tmp)
    end function
    
    !======================================================60
    ! function: field_pdiff
    !======================================================60
    pure function field_pdiff(this) result(pdiff)
         use mod_complex_arithm, only : carg
         implicit none
         class(JonesVector_t), intent(in) :: this
         ! Locals
         real(sp) :: pdiff,hr,hi,vr,vi,ch,cv
         ! Start of executable statements
         vr = real(this%m_v,kind=sp)
         vi = aimag(this%m_v,kind=sp)
         cv = atan2(vr,vi)
         hr = real(this%m_h,kind=sp)
         hi = aimag(this%m_h,kind=sp) 
         ch = atan2(hr,hi)
         pdiff = cv-ch
    end function
    
    !======================================================60
    ! function: polarization_degree
    !======================================================60
     pure function polarization_degree() return(val)
          implicit none
          ! Locals
          real(sp) :: val
          val = 1._sp
    end function
    
    !======================================================60
    ! function: jvec_conj
    !======================================================60
    pure function jvec_conj(jv1) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: jv1
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(CONJG(jv1%m_h),CONJG(jv1%m_v))
    end function
    
    !======================================================60
    ! subroutine: print_jvec
    !======================================================60
    subroutine print_jvec(this)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Start of executable statements
          print*, "==============================================="
          print*, " Vertical component   'v'   ", this%m_v
          print*, " Horizontal component 'h'   ", this%m_h
          print*, "================================================"
    end subroutine
    
    !======================================================60
    ! subroutine:  assignmemt (=)
    ! 
    !======================================================60
    subroutine assign_jvec(this,other)
          !use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          implicit none
          class(JonesVector_t), intent(inout) :: this
          class(JonesVector_t), intent(in)    :: other
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
             return
          end if
          this%m_h = other%m_h
          this%m_v = other%m_v
    end subroutine
    
    !======================================================60
    ! function: jvec_mul_jvec
    ! Remark: valid only for: conjg(this)*jv1
    !======================================================60
    pure function jvec_mul_jvec(this,jv1) result(c)
          implicit none
          class(JonesVector_t), intent(in) :: this
          class(JonesVector_t), intent(in) :: jv1
          ! Locals
          complex(sp) :: c
          ! Start of executable statements
          c = this%m_h*jv1%m_h+this%m_v*jv1%m_v
    end function
    
    !======================================================60
    !  function: jvec_mul_complex
    !======================================================60
    pure function jvec_mul_complex(this,c) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this
          complex(sp),        intent(in) :: c
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec%m_h = this%m_h*c
          njvec%m_v = this%m_v*c
          njvec%m_isbuilt = .true.
    end function
    
    
    
    !======================================================60
    !  function: jvec_div_complex
    !======================================================60
    pure function jvec_div_complex(this,c) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this
          complex(sp),        intent(in) :: c
          ! Start of executable statements
         njvec%m_h = this%m_h/c 
         njvec%m_v = this%m_v/c
         njvec%m_isbuilt = .true.
    end function
    
   
    
    !======================================================60
    ! function: jvec_add_jvec
    !======================================================60
    pure function jvec_add_jvec(this,other) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this,other
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(this%m_h+other%m_h, &
                                this%m_v+other%m_v )
    end function
    
   
    
    !======================================================60
    ! function: jvec_negate
    !======================================================60
    pure function jvec_negate(this) result(njvec)
          implicit none
          class(JonesVector_t), intent(inout) :: this
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(-this%m_h,-this%m_v)
    end function
    
    !======================================================60
    ! function: jvec_sub_jvec
    !======================================================60
    pure function jvec_sub_jvec(this,other) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this,other
          ! Lcals
          class(JonesVector) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(this%m_h-other%m_h, &
                                this%m_v-other%m_v)
    end function      
    
   
   
end module jonesvec
