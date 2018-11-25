
#include "Config.hpp"
    
module mod_quaternion



             
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_quaternion'
 !          
 !          Purpose:
 !                    Modeling of quaternions and their
 !                    corresponding mathematical operations.
 !          History:
 !                        Date: 02-12-2017
 !                        Time: 09:38 GMT+2
 !          Modified:     
 !                        Date: 11-24-2018
 !                        Time: 17:45 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 1
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                          Wikipedia.org
 !                     
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use mod_kinds,    only : int1, int4, dp
    use mod_vectypes, only : YMM4r8_t
    implicit none
    private
   
    ! public operators
    
    public :: assignment (=)
    public :: operator (+)
    public :: operator (-)
    public :: operator (*)
    public :: operator (/)
    public :: operator (==)
    public :: operator (/=)
    public :: operator (>)
    public :: operator (<)
    public :: operator (.idotp.)
    public :: operator (.icrossp.)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_QUATERNION_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_QUATERNION_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_QUATERNION_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_QUATERNION_FULLVER = 1000_int4*MOD_QUATERNION_MAJOR+100_int4*MOD_QUATERNION_MINOR+ &
                                                                 10_int4*MOD_QUATERNION_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_QUATERNION_CREATE_DATE = "02-12-2017 09:38 +00200 (SAT 02 DEC 2017 GMT+2)"
    
    ! Module build date (should be set after successfull compilation)
    character(*),  parameter, public :: MOD_QUATERNION_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_QUATERNION_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module description
    character(*),  parameter, public :: MOD_QUATERNION_DESCRIPT = " Implementation of Quaternions."
    
    !====================================
    !   Type: Quaternion_t
    !====================================
    
   
        
       
      type, public :: Quaternion_t
          
        type(YMM4r8_t) :: m_q
        
       
        
        contains
    
        ! Constructors
        
        ! default init to {0.0,0.0,0.0,0.0}
        procedure, pass(this), public :: default_init
        
        ! element-wise initialization
        procedure, pass(this), public :: elemwise_init
        
        ! initialize from complex number pair
        procedure, pass(this), public :: complex_init
        
        ! initialize from array of size 4
        procedure, pass(this), public :: array_init
        
        ! initialize scalar part only (vector part is set to 0.0)
        procedure, pass(this), public :: scpart_init
        
        ! initialize vector part only (scalar part is set to 0.0)
        procedure, pass(this), public :: vpart_init
        
        ! initialize by copy of rhs
        procedure, pass(this), public :: copy_init
        
       
        
        ! read/write procedures
        
        procedure, nopass, public :: read_quaternion
        
        procedure, nopass, public :: write_quaternion
        
        ! Computational procedures
        
        procedure, pass(this), public :: conjugate
        
        procedure, pass(this), public :: norm
        
        procedure, pass(this), public :: vnorm
        
        procedure, pass(this), public :: distance
        
        procedure, pass(this), public :: unit
        
        procedure, pass(this), public :: polar_decomp
        
        procedure, pass(this), public :: reciprocal
        
        procedure, pass(this), public :: mat4x4
        
        procedure, pass(this), public :: q_exp
        
        procedure, pass(this), public :: q_ln
        
        
        
    end type :: Quaternion_t   
        
        
        !======================================
        !   Module operators
        !======================================
        
        interface assignment (=)
            module procedure q_assign_q
        end interface
        
        interface operator (+)
            module procedure q_add_q
            module procedure q_add_c
            module procedure q_add_s
            module procedure c_add_q
            module procedure s_add_q
        end interface
        
        interface operator (-)
            module procedure q_sub_q
            module procedure q_sub_c
            module procedure q_sub_s
            module procedure c_sub_q
            module procedure s_sub_q
        end interface
        
        interface operator (*)
            module procedure q_mul_q
            module procedure q_mul_c
            module procedure q_mul_s
            module procedure c_mul_q
            module procedure s_mul_q
        end interface
        
        interface operator (/)
            module procedure q_div_q
            module procedure q_div_c
            module procedure q_div_s
            module procedure c_div_q
            module procedure s_div_q
        end interface
        
        interface operator (==)
            module procedure q_eq_q
            module procedure q_eq_c
            module procedure q_eq_s
            module procedure c_eq_q
            module procedure s_eq_q
        end interface
        
        interface operator (/=)
            module procedure q_neq_q
            module procedure q_neq_c
            module procedure q_neq_s
            module procedure c_neq_q
            module procedure s_neq_q
        end interface
        
        interface operator (>)
            module procedure q_gt_q
            module procedure q_gt_c
            module procedure q_gt_s
            module procedure c_gt_q
            module procedure s_gt_q
        end interface
        
        interface operator (<)
            module procedure q_lt_q
            module procedure q_lt_c
            module procedure q_lt_s
            module procedure c_lt_q
            module procedure s_lt_q
        end interface
        
        interface operator (.idotp.)
            module procedure q_idotp_q
        end interface
        
        interface operator (.icrossp.)
            module procedure q_icrossp_q
        end interface
        
    contains
    
    !========================================!
    !    Implementation                      !
    !========================================!    
    
    !=================================================
    !  @subroutine: default_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its default
    !            initialization to {0.0,0.0,0.0,0.0}
    !=================================================
    subroutine default_init(this)
          use mod_vecconsts , only : v4_pinf
          class(Quaternion_t), intent(inout) :: this
          ! Start of executable statements
          this.m_q = v4_pinf
    end subroutine
    
    !=================================================
    !  @subroutine: elemwise_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its element-wise
    !            initialization.
    !=================================================
    subroutine elemwise_init(this,x,y,z,w)
          
          class(Quaternion_t),    intent(out) :: this
          real(kind=dp),          intent(in)  :: x,y,z,w
          ! Start of executable statements
          this.m_q.v(0) = x
          this.m_q.v(1) = y
          this.m_q.v(2) = z
          this.m_w.v(3) = w
    end subroutine
    
    !=================================================
    !  @subroutine: complex_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and member initialization
    !            by user passed a pair of complex numbers
    !=================================================
    subroutine complex_init(this,c1,c2)
         
          class(Quaternion_t),    intent(out) :: this
          complex(kind=dp),       intent(in)  :: c1,c2
          ! Start of executable statements
          this.m_q.v(0) = REAL(c1,KIND=dp)
          this.m_q.v(1) = AIMAG(c1,KIND=dp)
          this.m_q.v(2) = REAL(c2,KIND=dp)
          this%m_q.v(3) = AIMAG(c2,KIND=dp)
    end subroutine
    
    !=================================================
    !  @subroutine: array_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member initialization
    !            by user passed array of real numbers.
    !=================================================
    subroutine array_init(this,a)
         
          class(Quaternion_t),         intent(out) :: this
          real(kind=dp), dimension(4), intent(in)  :: a
          ! Start of executable statements
          this.m_q.v = a
         
    end subroutine
    
    !=================================================
    !  @subroutine: scpart_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x
    !            initialzation by user passed scalar
    !================================================= 
    subroutine scpart_init(this,x)
          implicit none
          class(Quaternion_t),    intent(out) :: this
          real(kind=dp),          intent(in)  :: x
          ! Start of executable statements
          this.m_q.v    = 0.0_dp
          this.m_q.v(0) = x
         
    end subroutine
    
     
    !=================================================
    !  @subroutine: vpart_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x,m_y,m_z
    !            initialzation by user passed scalars.
    !================================================= 
    subroutine vpart_init(this,y,z,w)
         
          class(Quaternion_t),    intent(out) :: this
          real(kind=dp),          intent(in)  :: y,z,w
          ! Start of executable statements
          this.m_q.v(0) = 0.0_dp
          this.m_q.v(1) = y
          this.m_q.v(2) = z
          this.m_q.v(3) = w
    end subroutine
    
    !=================================================
    !  @subroutine: copy_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x,m_y,m_z,m_w
    !            initialization by user passed Quaternion_t
    !================================================= 
    subroutine copy_init(this,rhs)
          
          class(Quaternion_t), intent(out) :: this
          class(Quaternion_t), intent(in)  :: rhs
          ! Start of executable statements
          this.m_q.v = rhs.m_q.v
    end subroutine
    
   
    
   
    
   
    
   
    
   
    
   
    
   
    
     
  
    
    !=================================================!
    !  @subroutine: read_quaternion                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine read_quaternion(this,form,unit,ioerr)
          
          class(Quaternion_t),       intent(in)    :: this
          character(len=*),          intent(in)    :: form
          integer(kind=int4),        intent(in)    :: unit
          integer(kind=int4),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !=================================================!
    !  @subroutine: write_quaternion                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine write_quaternion(this,form,unit,ioerr)
          
          class(Quaternion_t),       intent(in)    :: this
          character(len=*),          intent(in)    :: form
          integer(kind=int4),        intent(in)    :: unit
          integer(kind=int4),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !=============================================
    ! @function: 
    !               conjugate
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: conjugate
!DIR$ ENDIF    
   pure function conjugate(this) result(q)
         
          class(Quaternion_t), intent(in) :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: q
          type(Quaternion_t) :: q
          ! Start of executable statements
          q.m_q.v    =  -this.m_q.v
          q.m_q.v(0) =  -1.0_dp*q.m_q.v(0)
         
    end function
    
    !=============================================
    ! @function: 
    !               norm
    ! @attributes: pure
    !=============================================
  pure  function norm(this) result(n)
          
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(kind=dp)  :: n
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM4r8_t) :: tmp
          ! Start of executable statements
          tmp.v = this.m_q.v*this.m_q.v
          n = SQRT(tmp.v(0)+tmp.v(1)+tmp.v(2)+tmp.v(3))
    end function
    
    !=============================================
    ! @function: 
    !               vnorm
    ! @attributes: pure
    !=============================================
  pure  function vnorm(this) result(n)
         
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(kind=dp)  :: n
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM4r8_t) :: tmp
          ! Start of executable statements
          tmp.v = this.m_q.v*this.m_q.v
          n = SQRT(tmp.v(1)+tmp.v(2)+tmp.v(3))
  end function
  
  !=============================================
  ! @function: 
  !               distance
  ! @attributes: pure
  !=============================================
  pure function distance(this,other) result(dist)
        
          class(Quaternion_t), intent(in) :: this,other
          ! Locals
          real(kind=dp)  :: dist
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(YMM4r8_t) :: tmp
          ! Start of executable statements
          tmp.v = (this.m_q.v-other.m_q.v)**2
          dist = SQRT(tmp.v(0)+tmp.v(1)+tmp.v(2)+tmp.v(3))
  end function 
  
  !=============================================
  ! @function: 
  !              unit
  ! @attributes: pure
  !=============================================
  pure function unit(this) result(uq)
          
          class(Quaternion_t), intent(in) :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 ::  uq
          type(Quaternion_t) :: uq
          real(kind=dp)      :: t
          ! Start of executable statements
          
          t = norm(this)
          uq.m_q.v = this.m_q.v / t
          
  end function
  
  !=============================================
  ! @function: 
  !              polar_decomp
  ! @attributes: pure
  !=============================================
  pure function polar_decomp(this) result(pd)
         
          class(Quaternion_t), intent(in) :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: pd,q
          type(Quaternion_t) :: pd,q
          real(kind=dp)      :: t
          ! Start of executable statements
          q = unit(this)
          t = norm(this)
          pd.m_q.v = t * q.m_q.v
         
  end function
  
  !=============================================
  ! @function: 
  !              reciprocal
  ! @attributes: pure
  !=============================================
  pure function reciprocal(this) result(r)
         
          class(Quaternion_t), intent(in)  :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: r,q
          type(Quaternion_t) :: r,q
          real(kind=dp)      :: n
          ! Start of executable statemetns
          q = conjugate(this)
          n = norm(this)
          r.m_q.v = q.m_q.v / n**2
          
  end function
  
  !=============================================
  ! @function: 
  !              mat4x4
  ! @attributes: pure
  !=============================================
  pure function mat4x4(this,mtype) result(m4x4)
         
          class(Quaternion_t), intent(in) :: this
          integer(I32P),       intent(in) :: mtype
          ! Locals
          
          real(kind=dp) :: ty,tw,tz
!DIR$     ATTRIBUTES ALIGN : 64 :: m4x4
          real(R64P), dimension(0:3,0:3) :: m4x4
          
          ! Start of executable statements
          ty = -this.m_q.v(1)
          tz = -this.m_q.v(2)
          tw = -this.m_q.v(3)
          m4x4(0,:) = this.m_q.v
          m4x4(1,:) = this.m_q.v
          m4x4(2,:) = this.m_q.v
          m4x4(3,:) = this.m_q.v
          if(mtype == 0) then
             
              m4x4(1,2) =  ty
              m4x4(1,3) =  tz
              m4x4(1,4) =  tw
              m4x4(2,3) =  tw
              m4x4(3,4) =  ty
              m4x4(4,2) =  tz
           else if (mtype == 1) then  
              m4x4(2,1) =  ty
              m4x4(2,3) = tw
              m4x4(3,1) =  tz
              m4x4(3,4) =  ty
              m4x4(4,1) =  tw
              m4x4(4,2) =  tz
            end if
  end function
  
          
            
            
            
            
            
             
           
            
              
  !=============================================
  ! @function: 
  !              q_exp
  ! @attributes: pure
  !=============================================
  pure function q_exp(this) result(qe)
         
         
          class(Quaternion_t), intent(in) :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 ::  qe
          type(Quaternion_t) :: qe
!DIR$     ATTRIBUTES ALIGN : 8 :: t1,t2,t3
          real(kind=dp) :: t1,t2,t3
!DIR$     ATTRIBUTES ALIGN : 64 ::  v
          real(kind=dp), dimension(3) :: v
          ! Start of executable statemetns
          t1 = EXP(this.m_q.v(0)))  ! e^a
          t2 = vnorm(this)
          v(1:3) = this.m_q.v(1:3)
          t3 = SIN(t2)
          v(1:3) = v(1:3) / t2 * t3
          qe.m_q.v(0) = t1*COS(t2)
          qe.m_q.v(1:3) = t1*v(1:3)
    end function        
      
          
  !=============================================
  ! @function: 
  !              q_ln
  ! @attributes: pure
  !=============================================
  pure function q_ln(this) result(lq)
         
          class(Quaternion_t), intent(in) :: this
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: lq
          type(Quaternion_t) :: lq
!DIR$     ATTRIBUTES ALIGN : 8 :: t1,t2,a
          real(kind=dp)      :: t1,t2,a
!DIR$     ATTRIBUTES ALIGN : 64 :: v
          real(kind=dp), dimension(3) :: v
          ! Start of executable statements
          t1 = LOG(norm(this))   ! ln||q||
          a  = this.m_q.v(0)
          t2 = vnorm(this)  ! ||vecpart of q||
          a  = ACOS(a/t2)
          v(1:3) = v(1:3) / t2 * a
          lq.m_q.v(0) = t1
          lq.m_q.v(1:3) = v(1:3)
    end function      
 
  
  !=============================================
  ! @subroutine: 
  !             q_assign_q
  ! @attributes: 
  !=============================================
  subroutine q_assign_q(this,other)
          
          type(Quaternion_t), intent(out) :: this
          type(Quaternion_t), intent(in)  :: other
          ! Start of executable statements
          if(LOC(this) == LOC(other)) then
              WRITE(*,*) "assignment (=) -- Self assignment!!"
              return
          end if
          this.m_q.v = other.m_q.v
          
  end subroutine
  
  !=============================================
  ! @function: 
  !              q_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_a(lhs,rhs) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: lhs,rhs
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq.m_q.v = lhs.m_q.v + rhs.m_q.v
  end function       
  
  
  !=============================================
  ! @function: 
  !              q_add_c (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_c(lhs,rhs) result(nq)
        
          type(Quaternion_t),    intent(in) :: lhs
          complex(kind=dp),      intent(in) :: rhs
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_q.v(0) = lhs%m_q.v(0) + REAL(rhs,KIND=dp)
          nq%m_q.v(1) = lhs%m_q.v(1) + AIMAG(rhs,KIND=dp)
          nq%m_q.v(2) = lhs%m_q.v(2) + REAL(rhs,KIND=dp)
          nq%m_q.v(3) = lhs%m_q.v(3) + AIMAG(rhs,KIND=dp)
  end function
  
  !=============================================
  ! @function: 
  !              q_add_s (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_s(lhs,rhs) result(nq)
        
          type(Quaternion_t),    intent(in) :: lhs
          real(kind=dp),         intent(in) :: rhs
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq.m_q.v = lhs.m_q.v + rhs
        
  end function
  
  !=============================================
  ! @function: 
  !              c_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function c_add_q(lhs,rhs) result(nq)
        
          complex(kind=dp),      intent(in) :: lhs
          type(Quaternion_t),    intent(in) :: rhs
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_q.v(0) = REAL(lhs,KIND=dp)  + rhs%m_q.v(0)
          nq%m_q.v(1) = AIMAG(lhs,KIND=dp) + rhs%m_q.v(1)
          nq%m_q.v(2) = REAL(lhs,KIND=dp)  + rhs%m_q.v(2)
          nq%m_q.v(3) = AIMAG(lhs,KIND=dp) + rhs%m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !              s_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function s_add_q(s,q) result(nq)
       
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq.m_q.v = s + q.m_q.v
         
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_q(q1,q2) result(nq)
        
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq.m_q.v = q1.m_q.v - q2.m_q.v
         
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_c (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_c(q,c) result(nq)
         
          type(Quaternion_t),    intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_q.v(0) = q%m_q.v(0) * REAL(c,KIND=dp)
          nq%m_q.v(1) = q%m_q.v(1) * AIMAG(c,KIND=dp)
          nq%m_q.v(2) = q%m_q.v(2) * REAL(c,KIND=dp)
          nq%m_q.v(3) = q%m_q.v(3) * AIMAG(c,KIND=dp)
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_s (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_s(q,s) result(nq)
       
          type(Quaternion_t), intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_q.v = q%m_q.v - s
         
  end function
  
  !=============================================
  ! @function: 
  !              c_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function c_sub_q(c,q) result(nq)
       
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_q.v(0) = REAL(c, KIND=dp) - q%m_q.v(0)
          nq%m_q.v(1) = AIMAG(c,KIND-dp) - q%m_q.v(1)
          nq%m_q.v(2) = REAL(c,KIND=dp)  - q%m_q.v(2)
          nq%m_q.v(3) = AIMAG(c,KIND=dp) - q%m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !              s_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function s_sub_q(s,q) result(nq)
        
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$      ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v = s - q.m_q.v
        
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_q(q1,q2) result(nq)
          
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v(0) = q1.m_q.v(0)*q2.m_q.v(0)-q1.m_q.v(1)*q2.m_q.v(1)- &
                   q1.m_q.v(2)*q2.m_q.v(2)-q1.m_q.v(3)*q2.m_q.v(3)
          nq%m_q.v(1) = q1.m_q.v(0)*q2.m_q.v(1)+q1.m_q.v(1)*q2.m_q.v(0)+ &
                   q1.m_q.v(2)*q2.m_q.v(3)-q1.m_q.v(3)*q2.m_q.v(2)
          nq%m_q.v(2) = q1.m_q.v(0)*q2.m_q.v(2)-q1.m_q.v(1)*q2%m_q.v(3)+ &
                   q1.m_q.v(2)*q2.m_q.v(0)+q1.m_q.v(3)*q2.m_q.v(1)
          nq%m_q.v(3) = q1.m_q.v(0)*q2.m_q.v(2)+q1.m_q.v(1)*q2.m_q.v(2)- &
                   q1.m_q.v(2)*q2.m_q.v(1)+q1.m_q.v(2)*q2.m_q.v(0)
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_c (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_c(q,c) result(nq)
         
          type(Quaternion_t),    intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
          type(Quaternion_t) :: nq
          real(kind=dp)      :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          nq.m_q.v(0) =  q.m_q.v(0)*re-q.m_q.v(1)*im
          nq.m_q.v(1) =  q.m_q.v(0)*im+q.m_q.v(1)*re
          nq.m_q.v(2) =  q.m_q.v(2)*re+q.m_q.v(3)*im
          nq.m_q.v(3) = -q.m_q.v(2)*im+q.m_q.v(3)*re
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_s (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_s(q,s) result(nq)
!DIR$     ATTRIBUTES VECTOR :: q_mul_q         
          type(Quaternion_t),    intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v = q.m_q.v * s
         
  end function
  
  !=============================================
  ! @function: 
  !              c_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function c_mul_q(c,q) result(nq)
         
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 ::  nq
          type(Quaternion_t) :: nq
          real(kind=dp) :: re,im
          ! Start of executable statements
          re = REAL(c,KIND=dp)
          im = AIMAG(c, KIND=dp)
          nq.m_q.v(0) = re*q.m_q.v(0)-im*q.m_q.v(1)
          nq.m_q.v(1) = im*q.m_q.v(0)+re*q.m_q.v(1)
          nq.m_q.v(2) = re*q.m_q.v(2)+im*q.m_q.v(3)
          nq%m_q.v(3) = im*(-q.m_q.v(2))+re*q.m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !              s_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function s_mul_q(s,q) result(nq)
!DIR$     ATTRIBUTES AVECTOR :: s_mul_q        
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v = s* q.m_q.v
        
  end function
  
  !=============================================
  ! @function: 
  !              q_div_q (operator (/))
  ! @attributes: 
  !=============================================
  pure function q_div_q(q1,q2) result(nq)
       
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 ::  nq
          type(Quaternion_t) :: nq
          real(kind=dp)      :: denom,ivdenom
          ! Start of executable statemetns
          denom =  q2.m_q.v(0)*q2.m_q.v(0)+q2.m_q.v(1)+q2.m_q.v(1)+ &
                   q2.m_q.v(2)*q2.m_q.v(2)+q2.m_q.v(3)*q2.m_q.v(3)
          ivdenom = 1.0_dp/denom
          nq.m_q.v(0) = (q1.m_q.v(0)*q2.m_q.v(0)+q1.m_q.v(1)*q2.m_q.v(1)+ &
                   q1.m_q.v(2)*q2.m_q.v(2)+q1.m_q.v(3)*q2.m_q.v(3) )*ivdenom
          nq.m_q.v(1) = (-q1.m_q.v(0)*q2.m_q.v(1)+q1.m_q.v(1)*q2.m_q.v(0)- &
                   q1.m_q.v(2)*q2.m_q.v(3)+q1.m_q.v(3)*q2.m_q.v(2))*ivdenom
          nq.m_q.v(2) = (-q1.m_q.v(0)*q2.m_q.v(2)+q1.m_q.v(1)*q2.m_q.v(3)+ &
                    q1.m_q.v(2)*q2.m_q.v(1)-q1.m_q.v(3)*q2.m_q.v(1))*ivdenom
          nq.m_q.v(3) = (-q1.m_q.v(0)*q2.m_q.v(3)-q1.m_q.v(1)*q2.m_q.v(2)+ &
                    q1.m_q.v(2)*q2.m_q.v(1)+q1.m_q.v(3)*q2.m_q.v(0))*ivdenom
  end function
  
   
  !=============================================
  ! @function: 
  !              q_div_c (operator (/))
  ! @attributes: 
  !============================================= 
  pure function q_div_c(q,c) result(nq)
        
          type(Quaternion_t), intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          real(kind=dp)      :: denom,re,im,ivdenom
          ! Start of executable statements
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          denom = re*re+im*im
          ivdenom = 1.0_dp/denom
          nq.m_q.v(0) = (q.m_q.v(0)*re+q.m_q.v(1)*im)*ivdenom
          nq.m_q.v(1) = (-q%m_q.v(0)*im+q.m_q.v(1)*re)*ivdenom
          nq.m_q.v(2) = (q.m_q.v(2)*re-q.m_q.v(3)*im)*ivdenom
          nq.m_q.v(3) = (q.m_q.v(2)*im+q.m_q.v(3)*re)*ivdenom
  end function
  
  !=============================================
  ! @function: 
  !              q_div_s (operator (/))
  ! @attributes: 
  !============================================= 
  pure function q_div_s(q,s) result(nq)
!DIR$     ATTRIBUTES VECTOR :: q_div_q          
          type(Quaternion_t),    intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: n
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v = q.m_q.v / s
         
  end function
  
  !=============================================
  ! @function: 
  !              c_div_q (operator (/))
  ! @attributes: 
  !============================================= 
  pure function c_div_q(c,q) result(nq)
         
          complex(kind=dp),   intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          real(kind=dp) :: re,im,denom,ivdenom
          ! Start of executable statements
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          denom = re*re+im*im
          ivdenom = 1.0_dp/denom
          nq%m_x = (re*q.m_q.v(0)+im*q.m_q.v(1))*ivdenom
          nq%m_y = (im*(-q.m_q.v(0))+re*q.m_q.v(1))*ivdenom
          nq%m_z = (re*q.m_q.v(2)-im*q.m_q.v(3))*ivdenom
          nq%m_w = (im*q.m_q.v(2)+re*q.m_q.v(3))*ivdenom
  end function
  
   
  !=============================================
  ! @function: 
  !              s_div_q (operator (/))
  ! @attributes: 
  !============================================= 
  pure function s_div_q(s,q) result(nq)
!DIR$     ATTRIBUTES VECTOR :: s_div_q         
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t),    intent(in) :: q
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: nq
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq.m_q.v = s / q.m_q.v
         
  end function
  
    
  !=============================================
  ! @function: 
  !             q_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_q(q1,q2) result(eq)
         
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          logical(kind=int4) :: eq
          eq =  q1.m_q.v(0) == q2.m_q.v(0) .and. &
                q1.m_q.v(1) == q2.m_q.v(1) .and. &
                q1.m_q.v(2) == q2.m_q.v(2) .and. &
                q1.m_q.v(3) == q2.m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !             q_eq_c (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_c(q,c) result(eq)
         
          type(Quaternion_t), intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
          real(kind=dp) :: re,im
          logical(kind=int4) :: eq
          
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          eq =  q.m_q.v(0) == re       .AND. &
                q.m_q.v(1) == im      .AND. &
                q.m_q.v(2) == 0.0_dp  .AND. &
                q.m_q.v(3) == 0.0_dp 
  end function
  
  !=============================================
  ! @function: 
  !             q_eq_s (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_s(q,s) result(eq)
        
          type(Quaternion_t), intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
          logical(kind=int4) :: eq
          ! Start of executable statemetns
          eq = q.m_q.v(0) == s       .AND. &
               q.m_q.v(1) == 0.0_dp  .AND. &
               q.m_q.v(2) == 0.0_dp  .AND. &
               q.m_q.v(3) == 0.0_dp  
  end function
  
  !=============================================
  ! @function: 
  !             c_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function c_eq_q(c,q) result(eq)
        
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t),    intent(in) :: q
          ! Locals
          logical(kind=dint4) :: eq
          real(kind=dp)       :: re,im
          ! Start of executable statement
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          eq =  re      ==  q.m_q.v(0)      .AND. &
                im      ==  q%m_q.v(1)      .AND. &
                0.0_dp  ==  q%m_q.v(2)      .AND. &
                0.0_dp  ==  q%m_q.v(3)   
  end function
  
  !=============================================
  ! @function: 
  !             s_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function s_eq_q(s,q) result(eq)
         
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t),    intent(in) :: q
          ! Locals
          logical(kind=int4) :: eq
          ! Start of executable statements
          eq =  s == q.m_q.v(0)      .AND. &
                0.0_dp == q.m_q.v(1) .AND. &
                0.0_dp == q.m_q.v(2) .AND. &
                0.0_dp == q.m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_q(q1,q2) result(neq)
         
          type(Quaternion_t), intent(in) :: q1,q2
          ! locals
          logical(kind=int4) :: neq
          ! Start of executable statements
          neq =  q1.m_q.v(0) /= q2.m_q.v(0) .AND. &
                 q1.m_q.v(1) /= q2.m_q.v(1) .AND. &
                 q1.m_q.v(2) /= q2.m_q.v(2) .AND. &
                 q1.m_q.v(3) /= q2.m_q.v(3)   
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_c (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_c(q,c) result(neq)
         
          type(Quaternion_t), intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
          logical(kind=int4) :: neq
          real(kind=dp) :: re,im
          ! Start of executale statemnts
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          neq = q.m_q.v(0) /= re      .AND. &
                 q.m_q.v(1) /= im      .AND. &
                 q.m_q.v(2) /= 0.0_dp  .AND. &
                 q.m_q.v(3) /= 0.0_dp 
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_s (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_s(q,s) result(neq)
         
          type(Quaternion_t), intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
          logical(kind=int4) :: neq
          ! Start of excutable statments
          neq =  q%m_q.v(0) /= s       .AND. &
                 q%m_q.v(1) /= 0._dp .AND. &
                 q%m_q.v(2) /= 0._dp .AND. &
                 q%m_q.v(3) /= 0._dp 
  end function
  
  !=============================================
  ! @function: 
  !             c_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function c_neq_q(c,q) result(neq)
         
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(kind=int4) :: neq
          real(kind=dp) :: re,im
          ! Start of executable sattaements
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          neq = re /= q%m_q.v(0)        .AND. &
                 im /= q%m_q.v(1)       .AND. &
                 0._dp /= q%m_q.v(2)    .AND. &
                 0._dp /= q%m_q.v(3)  
  end function
  
  !=============================================
  ! @function: 
  !             s_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function s_neq_q(s,q) result(neq)
         
          real(kind=dp),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(kind=int4) :: neq
          ! Start of executable statements
          neq = s /= q%m_q.v(0)   .AND. &
                 s /= 0._dp .AND. &
                 s /= 0._dp .AND. &
                 s /= 0._dp  
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_q(q1,q2) result(gt)
         
          type(Quaternion_t), intent(in) :: q1,q2
          !Locals
          logical(kind=int4) :: gt
          ! Start of executable statements
          gt =  q1%m_q.v(0) > q2%m_q.v(0)  .AND. &
                q1%m_q.v(1) > q2%m_q.v(1)  .AND. &
                q1%m_q.v(2) > q2%m_q.v(2)  .AND. &
                q1%m_q.v(3) > q2%m_q.v(3)   
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_c (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_c(q,c) result(gt)
         
          type(Quaternion_t), intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
          logical(kind=int4) :: gt
          real(kind=dp) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          gt =  q%m_q.v(0) > re      .AND. &
                q%m_q.v(1) > im      .AND. &
                q%m_q.v(2) > 0._dp   .AND. &
                q%m_q.v(3) > 0._dp  
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_s (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_s(q,s) result(gt)
         
          type(Quaternion_t), intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
          logical(kind=int4) :: gt
          ! Start of executable statemetns
          gt =  q%m_q.v(0) > s       .AND. &
                q%m_q.v(1) > 0._dp   .AND. &
                q%m_q.v(2) > 0._dp   .AND. &
                q%m_q.v(3) > 0._dp   
  end function
  
  !=============================================
  ! @function: 
  !             c_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function c_gt_q(c,q) result(gt)
         
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          real(kind=dp) :: re,im
          logical(kind=int4) :: gt
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          gt =  re > q%m_q.v(0)      .AND. &
                im > q%m_q.v(1)      .AND. &
                0._dp > q%m_q.v(2)   .AND. &
                0._dp > q%m_q.v(3)  
  end function
  
  !=============================================
  ! @function: 
  !             s_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function s_gt_q(s,q) result(gt)
         
          real(kind=dp),  intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(kind=int4) :: gt
          ! Start of executable statemetns
          gt =  s > q%m_q.v(0)          .AND. &
                0._dp > q%m_q.v(1)      .AND. &
                0._dp > q%m_q.v(2)      .AND. &
                0._dp > q%m_q.v(3)       
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_q(q1,q2) result(lt)
        
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          logical(kind=int4) :: lt
          ! Strat of executable statements
          lt =  q1%m_q.v(0) < q2%m_q.v(0)  .AND. &
                q1%m_q.v(1) < q2%m_q.v(1)  .AND. &
                q1%m_q.v(2) < q2%m_q.v(2)  .AND. &
                q1%m_q.v(3) < q2%m_q.v(3)   
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_c (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_c(q,c) result(lt)
        
          type(Quaternion_t), intent(in) :: q
          complex(kind=dp),      intent(in) :: c
          ! Locals
          logical(kind=int4) :: lt
          real(kind=dp) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          lt = q%m_q.v(0) < re       .AND. &
                q%m_q.v(1) < im      .AND. &
                q%m_q.v(2) < 0._dp   .AND. &
                q%m_q.v(3) < 0._dp  
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_s (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_s(q,s) result(lt)
         
          type(Quaternion_t), intent(in) :: q
          real(kind=dp),         intent(in) :: s
          ! Locals
          logical(kind=int4) :: lt
          ! Start of executable statemetns
          lt =  q%m_q.v(0) < s       .AND.  &
                q%m_q.v(1) < 0._dp .AND. &
                q%m_q.v(2) < 0._dp .AND. &
                q%m_q.v(3) < 0._dp  
  end function
  
  !=============================================
  ! @function: 
  !             c_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function c_lt_q(c,q) result(lt)
         
          complex(kind=dp),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(kind=int4) :: lt
          real(kind=dp) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=dp)
          im = AIMAG(c,KIND=dp)
          lt = re < q%m_q.v(0)      .AND. &
                im < q%m_q.v(1)      .AND. &
                0._dp < q%m_q.v(2) .AND. &
                0._dp < q%m_q.v(3) 
  end function
  
  !=============================================
  ! @function: 
  !             s_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function s_lt_q(s,q) result(lt)
         
          real(kind=dp), intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(kind=int4) :: lt
          ! Start of executable sattemets
          lt =  s < q%m_q.v(0)       .AND. &
                 0._dp < q%m_q.v(1) .AND. &
                 0._dp < q%m_q.v(2) .AND. &
                 0._dp < q%m_q.v(3) 
  end function
  
  !=============================================
  ! @function: 
  !             q_idotp_q   (imaginary part
  !             dot product.
  ! @attributes: 
  !============================================= 
  pure function q_idotp_q(q1,q2) result(dp)
          
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          real(kind=dp) :: dotp
          ! Start of executable statements
          dotp = q1.m_q.v(1)*q2.m_q.v(1)+q1.m_q.v(2)*q2.m_q.v(2)+ &
               q1.m_q.v(3)*q2.m_q.v(3)
  end function
  
  !=============================================
  ! @function: 
  !             q_icrossp_q   (imaginary part
  !             cross product.
  ! @attributes: 
  !============================================= 
  pure function q_icrossp_q(q1,q2) result(cp)
         
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: cp
          type(Quaternion_t) :: cp
          ! Strat of executable statemetns
          cp.m_q.v(0) = 0.0_dp
          cp.m_q.v(1) = q1%m_z*q2%m_w-q1%m_w*q2%m_z
          cp.m_q.v(2) = q1%m_w*q2%m_y-q1%m_y*q2%m_w
          cp.m_q.v(3) = q1%m_y*q2%m_z-q1%m_z*q2%m_y
  end function
  
  
end module mod_quaternion