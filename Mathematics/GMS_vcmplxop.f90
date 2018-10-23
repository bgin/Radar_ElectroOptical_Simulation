

module mod_vcmplxop

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_vcmplxop'
 !          
 !          Purpose:
 !                     SIMD-like operations on derived types
 !                     which represent decomposed (re,im) parts
 !                     complex numbers
 !
 !          History:
 !                        
 !                        Date: 18-10-2018
 !                        Time: 11:23 GMT+2
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
    use mod_kinds,    only : int4,dp
    use mod_vectypes, only : YMM4c8_t,ZMM8c8_t
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_VCMPLXOP_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_VCMPLXOP_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_VCMPLXOP_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_VCMPLXOP_FULLVER = 1000_int4*MOD_VCMPLXOP_MAJOR+100_int4*MOD_VCMPLXOP_MINOR+ &
                                                                    10_int4*MOD_VCMPLXOP_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VCMPLXOP_CREATE_DATE = "18-10-2018 11:23 +00200 (THR 18 OCT 2018 GMT+2) "
    
    ! Module build date
    character(*),       parameter, public :: MOD_VCMPLXOP_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_VCMPLXOP_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_VCMPLXOP_SYNOPSIS = "SIMD complex arithmetic operations."
    
    contains
    
    ! 4-wide complex vector addition
    
    pure function v4c_ymm4c8_add_ymm4c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Local 
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ....
           z.re = x.re + y.re
           z.im = x.im + y.im
    end function v4c_ymm4c8_add_ymm4c8
    
    pure function v4c_ymm4c8_add_c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          complex(kind=dp),     intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ....
            z.re = x.re + real(y)
            z.im = x.im + aimag(y)
    end function v4c_ymm4c8_add_c8
    
    pure function v4c_ymm4c8_add_r8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          real(kind=dp),        intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Excec code ...
           z.re = x.re + y
           z.im = x.im + y
    end function v4c_ymm4c8_add_r8
    
     ! 4-wide complex vector subtraction

     pure function v4c_ymm4c8_sub_ymm4c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ...
          z.re = x.re - y.re
          z.im = x.im - y.im
     end function v4c_ymm4c8_sub_ymm4c8
     
     pure function v4c_ymm4c8_sub_c8(x,y) result(z)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ...
          z.re = x.re - real(y)
          z.im = x.im - aimag(y)
     end function v4c_ymm4c8_sub_c8
     
     pure function v4c_ymm4c8_sub_c8(x,y) result(z)
           type(YMM4c8_t),       intent(in) :: x
           real(kind=dp),        intent(in) :: y
           ! Locals
!DIR$      ATTRIBUTES ALIGN : 32 :: z
           type(YMM4c8_t) :: z
           ! Exec code ...
           z.re = x.re - y
           z.im = x.im - y
     end function v4c_ymm4c8_sub_c8
     
       ! 4-wide complex vector multiplication
     
     pure function v4c_ymm4c8_mul_ymm4c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ....
          z.re = (x.re * y.re) - (x.im * y.im)
          z.im = (x.im * y.re) + (x.re * y.im)
     end function v4c_ymm4c8_mul_ymm4c8
     
     pure function v4c_ymm4c8_mul_c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          complex(kind=dp),     intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ...
          z.re =  x.re * real(y) - x.im * aimag(y)
          z.im =  x.im * real(y) + x.re * aimag(y)
     end function v4c_ymm4c8_mul_c8
     
     pure function v4c_ymm4c8_mul_r8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          real(kind=dp),        intent(in) :: y
           ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ....
          z.re = x.re * y
          z.im = x.im * y
     end function v4c_ymm4c8_mul_r8
     
     ! 4-wide complex vector multiplication
     
     pure function v4c_ymm4c8_div_ymm4c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z,tmp,den
          type(YMM4c8_t) :: z,tmp,den
          ! Exec code ....
          den.im = 0.0E+00_dp
          tmp.re = x.re * y.re + x.im * y.im
          tmp.im = x.im * y.re - x.re * y.im
          den.re = y.re * y.re + y.im * y.im
          z.re = tmp.re / den.re
          z.im = tmp.im / den.re
     end function v4c_ymm4c8_div_ymm4c8
     
     pure function v4c_ymm4c8_div_c8(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          complex(kind=dp),     intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z,tmp,den          
          type(YMM4c8_t) :: z,tmp,den
          ! Exec code ...
          den.im = 0.0E+00_dp
          tmp.re = x.re * real(y)  + x.im * aimag(y)
          tmp.im = x.im * real(y)  - x.re * aimag(y)
          den.re = real(y) * real(y) + aimag(y) * aimag(y)
          z.re = tmp.re / den.re
          z.im = tmp.im / den.re
     end function  v4c_ymm4c8_div_c8
     
     pure function v4c_ymm4c8_div_r8(x,y) result(z)
          type(YMM4c8_t),        intent(in) :: x
          real(kind=dp),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM4c8_t) :: z
          ! Exec code ....
          z.re = x.re / y
          z.im = x.im / y
     end function  v4c_ymm4c8_div_r8
     
     ! Horizontal comparison
     
     pure function v4c_ymm4c8_eq_ymm4c8(x,y) result(eq)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: eq
          ! Exec code ...
          eq(0) = x.re(0) == y.re(0) .and.  &
                  x.re(1) == y.re(1) .and.  &
                  x.re(2) == y.re(2) .and.  &
                  x.re(3) == y.re(3)
          eq(1) = x.im(0) == y.im(0) .and.  &
                  x.im(1) == y.im(1) .and.  &
                  x.im(2) == y.im(2) .and   &
                  x.im(3) == y.im(3)
     end function v4c_ymm4c8_eq_ymm4c8
     
     pure function v4c_ymm4c8_eq_c8(x,y) result(eq)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: eq
          ! Exec code ....
          eq(0) = x.re(0) == real(y) .and. &
                  x.re(1) == real(y) .and. &
                  x.re(2) == real(y) .and. &
                  x.re(2) == real(y)
          eq(1) = x.im(0) == aimag(y) .and. &
                  x.im(1) == aimag(y) .and. &
                  x.im(2) == aimag(y) .and. &
                  x.im(3) == aimag(y)
     end function v4c_ymm4c8_eq_c8
     
     pure function v4c_ymm4c8_eq_r8(x,y) result(eq)
          type(YMM4c8_t),         intent(in) :: x
          real(kind=dp),          intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: eq
          ! Exec code ...
          eq(0) = x.re(0) == y .and. &
                  x.re(1) == y .and. &
                  x.re(2) == y .and. &
                  x.re(3) == y 
          eq(1) = x.im(0) == y .and. &
                  x.im(1) == y .and. &
                  x.im(2) == y .and. &
                  x.im(3) == y
     end function v4c_ymm4c8_eq_r8
     
     pure function v4c_ymm4c8_neq_ymm4c8(x,y) result(neq)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: neq
          ! Exec code ...
          neq(0) = x.re(0)  /= y.re(0) .and.  &
                   x.re(1)  /= y.re(1) .and.  &
                   x.re(2)  /= y.re(2) .and.  &
                   x.re(3)  /= y.re(3)
          neq(1) = x.im(0) /= y.im(0) .and.  &
                   x.im(1) /= y.im(1) .and.  &
                   x.im(2) /= y.im(2) .and.  &
                   x.im(3) /= y.im(3)
     end function v4c_ymm4c8_neq_ymm4c8
     
     pure function v4c_ymm4c8_neq_c8(x,y) result(neq)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: neq
          ! Exec code ....
          neq(0) = x.re(0) /= real(y) .and. &
                  x.re(1) /= real(y) .and. &
                  x.re(2) /= real(y) .and. &
                  x.re(2) /= real(y)
          neq(1) = x.im(0) /= aimag(y) .and. &
                  x.im(1) /= aimag(y) .and. &
                  x.im(2) /= aimag(y) .and. &
                  x.im(3) /= aimag(y)
     end function v4c_ymm4c8_neq_c8
     
     pure function v4c_ymm4c8_neq_r8(x,y) result(neq)
            type(YMM4c8_t),      intent(in) :: x
            real(kind=dp),       intent(in) :: y
            ! Locals
            logical(kind=int4) :: neq
            ! Exec code ....
            neq = x.re(0) == y .and. &
                  x.re(1) == y .and. &
                  x.re(2) == y .and. &
                  x.re(3) == y
     end function v4c_ymm4c8_neq_r8
     
     pure function v4c_ymm4c8_gt_ymm4c8(x,y) result(gt)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: gt
          ! Exec code ...
          gt(0) =  x.re(0)  > y.re(0) .and.  &
                   x.re(1)  > y.re(1) .and.  &
                   x.re(2)  > y.re(2) .and.  &
                   x.re(3)  > y.re(3)
          gt(1) =  x.im(0)  > y.im(0) .and.  &
                   x.im(1)  > y.im(1) .and.  &
                   x.im(2)  > y.im(2) .and.  &
                   x.im(3)  > y.im(3)
     end function v4c_ymm4c8_gt_ymm4c8
     
     pure function v4c_ymm4c8_gt_c8(x,y) result(gt)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: gt
          ! Exec code ....
          gt(0) = x.re(0) > real(y) .and. &
                  x.re(1) > real(y) .and. &
                  x.re(2) > real(y) .and. &
                  x.re(2) > real(y)
          gt(1) = x.im(0) > aimag(y) .and. &
                  x.im(1) > aimag(y) .and. &
                  x.im(2) > aimag(y) .and. &
                  x.im(3) > aimag(y)
     end function v4c_ymm4c8_gt_c8
     
     pure function v4c_ymm4c8_r8(x,y) result(gt)
          type(YMM4c8_t),      intent(in) :: x
          real(kind=dp),       intent(in) :: y
            ! Locals
            logical(kind=int4) :: gt
            ! Exec code ....
            gt = x.re(0) ==  y .and. &
                  x.re(1) == y .and. &
                  x.re(2) == y .and. &
                  x.re(3) == y
     end function v4c_ymm4c8_c8
     
     pure function v4c_ymm4c8_ge_ymm4c8(x,y) result(ge)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: ge
          ! Exec code ...
          ge(0) =  x.re(0)  >= y.re(0) .and.  &
                   x.re(1)  >= y.re(1) .and.  &
                   x.re(2)  >= y.re(2) .and.  &
                   x.re(3)  >= y.re(3)
          ge(1) =  x.im(0)  >= y.im(0) .and.  &
                   x.im(1)  >= y.im(1) .and.  &
                   x.im(2)  >= y.im(2) .and.  &
                   x.im(3)  >= y.im(3)
     end function v4c_ymm4c8_ge_ymm4c8
     
     pure function v4c_ymm4c8_ge_c8(x,y) result(ge)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: ge
          ! Exec code ....
          ge(0) = x.re(0) >= real(y) .and. &
                  x.re(1) >= real(y) .and. &
                  x.re(2) >= real(y) .and. &
                  x.re(2) >= real(y)
          ge(1) = x.im(0) >= aimag(y) .and. &
                  x.im(1) >= aimag(y) .and. &
                  x.im(2) >= aimag(y) .and. &
                  x.im(3) >= aimag(y)
     end function v4c_ymm4c8_ge_c8
     
     pure function v4c_ymm4c8_ge_r8(x,y) result(ge)
          type(YMM4c8_t),      intent(in) :: x
          real(kind=dp),       intent(in) :: y
            ! Locals
            logical(kind=int4) :: ge
            ! Exec code ....
            gt  = x.re(0) >=  y .and. &
                  x.re(1) >=  y .and. &
                  x.re(2) >=  y .and. &
                  x.re(3) >=  y
     end function  v4c_ymm4c8_ge_r8
     
     pure function v4c_ymm4c8_lt_ymm4c8(x,y) result(lt)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: lt
          ! Exec code ...
          lt(0) =  x.re(0)  < y.re(0) .and.  &
                   x.re(1)  < y.re(1) .and.  &
                   x.re(2)  < y.re(2) .and.  &
                   x.re(3)  < y.re(3)
          lt(1) =  x.im(0)  < y.im(0) .and.  &
                   x.im(1)  < y.im(1) .and.  &
                   x.im(2)  < y.im(2) .and.  &
                   x.im(3)  < y.im(3)
     end function v4c_ymm4c8_lt_ymm4c8
     
     pure function v4c_ymm4c8_lt_c8(x,y) result(lt)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: lt
          ! Exec code ....
          lt(0) = x.re(0) < real(y) .and. &
                  x.re(1) < real(y) .and. &
                  x.re(2) < real(y) .and. &
                  x.re(2) < real(y)
          lt(1) = x.im(0) < aimag(y) .and. &
                  x.im(1) < aimag(y) .and. &
                  x.im(2) < aimag(y) .and. &
                  x.im(3) < aimag(y)
     end function v4c_ymm4c8_lt_c8
     
     pure function v4c_ymm4c8_lt_r8(x,y) result(lt)
          type(YMM4c8_t),      intent(in) :: x
          real(kind=dp),       intent(in) :: y
            ! Locals
            logical(kind=int4) :: lt
            ! Exec code ....
            lt  = x.re(0) <  y .and. &
                  x.re(1) <  y .and. &
                  x.re(2) <  y .and. &
                  x.re(3) <  y
     end function v4c_ymm4c8_lt_r8
     
     pure function v4c_ymm4c8_le_ymm4c8(x,y) result(le)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          logical(kind=int4), dimension(2) :: le
          ! Exec code ...
          le(0) =  x.re(0)  <= y.re(0) .and.  &
                   x.re(1)  <= y.re(1) .and.  &
                   x.re(2)  <= y.re(2) .and.  &
                   x.re(3)  <= y.re(3)
          le(1) =  x.im(0)  <= y.im(0) .and.  &
                   x.im(1)  <= y.im(1) .and.  &
                   x.im(2)  <= y.im(2) .and.  &
                   x.im(3)  <= y.im(3)
     end function v4c_ymm4c8_le_ymm4c8
     
     pure function v4c_ymm4c8_le_c8(x,y) result(le)
          type(YMM4c8_t),        intent(in) :: x
          complex(kind=dp),      intent(in) :: y
          ! Locals
          
          logical(kind=int4), dimension(2) :: le
          ! Exec code ....
          le(0) = x.re(0) <= real(y) .and. &
                  x.re(1) <= real(y) .and. &
                  x.re(2) <= real(y) .and. &
                  x.re(2) <= real(y)
          le(1) = x.im(0) <= aimag(y) .and. &
                  x.im(1) <= aimag(y) .and. &
                  x.im(2) <= aimag(y) .and. &
                  x.im(3) <= aimag(y)
     end function v4c_ymm4c8_le_c8
     
     pure function v4c_ymm4c8_le_r8(x,y) result(le)
          type(YMM4c8_t),      intent(in) :: x
          real(kind=dp),       intent(in) :: y
            ! Locals
          logical(kind=int4) :: le
            ! Exec code ....
            le  = x.re(0) <=  y .and. &
                  x.re(1) <=  y .and. &
                  x.re(2) <=  y .and. &
                  x.re(3) <=  y
     end function  v4c_ymm4c8_le_r8
     
     ! Complex operations over complex field 1D
     
     subroutine v4c_c4field1D_add_c4field1D(x,y,z,length)
          type(YMM4c8_t), dimension(1:length), intent(in)  :: x
          type(YMM4c8_t), dimension(1:length), intent(in)  :: y
          type(YMM4c8_t), dimension(1:length), intent(out) :: z
          integer(kind=int4),                  intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code
!DIR$ VECTOR ALWAYS
          do i = 1, length
             z(i).re = x(i).re + y(i).re
             z(i).im = x(i).re + y(i).re
          end do
     end subroutine v4c_c4field1D_add_c4field1D
     
     subroutine v4c_c4field1D_add_c8(x,y,z,length)
          type(YMM4c8_t),  dimension(1:length), intent(in)  :: x
          complex(kind=dp),                     intent(in)  :: y
          type(YMM4c8_t),  dimension(1:length), intent(out) :: z
          integer(kind=int4),                   intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code ....
!DIR$ VECTOR ALWAYS          
          do i = 1, length
              z(i).re = x(i).re + real(y)
              z(i).im = x(i).im + aimag(y)
          end do
     end subroutine v4c_c4field1D_add_c8
     
     subroutine v4c_c4field1D_add_r8(x,y,z,length)
          type(YMM4c8_t),   dimension(1:length), intent(in)  :: x
          real(kind=dp),                         intent(in)  :: y
          type(YMM4c8_t),   dimension(1:length), intent(out) :: z
          integer(kind=int4),                    intent(in)  :: length
          ! Locals
          type(YMM4c8_t)     :: t
          integer(kind=int4) :: i
          ! Exec code ....
          t.re = y
!DIR$ VECTOR ALWAYS
          do i = 1, length
             z(i).re = x(i).re + t.re 
             z(i).im = x(i).im + t.re
          end do
     end subroutine v4c_c4field1D_add_r8
     
     subroutine v4c_c4field1D_sub_c4field1D(x,y,z,length)
          type(YMM4c8_t),   dimension(1:length), intent(in)  :: x
          type(YMM4c8_t),   dimension(1:length), intent(in)  :: y
          type(YMM4c8_t),   dimension(1:length), intent(out) :: z
          integer(kind=int4),                    intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code ...
!DIR$ VECTOR ALWAYS
          do i = 1, length
              z(i).re = x(i).re - y(i).re
              z(i).im = x(i).im - y(i).im
          end do
     end subroutine v4c_c4field1D_sub_c4field1D
     
     subroutine v4c_c4field1D_sub_c8(x,y,z,length)
          type(YMM4c8_t),   dimension(1:length), intent(in)  :: x
          complex(kind=dp),                      intent(in)  :: y
          type(YMM4c8_t),   dimension(1:length), intent(out) :: z
          integer(kind=int4),                    intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code ....
!DIR$ VECTOR ALWAYS
          do i = 1, length
              z(i).re = x(i).re - real(y)
              z(i).im = x(i).im - aimag(y)
          end do
     end subroutine v4c_c4field1D_sub_c8
     
    subroutine v4c_c4field1D_sub_r8(x,y,z,length)
          
          type(YMM4c8_t),   dimension(1:length), intent(in)  :: x
          real(kind=dp),                         intent(in)  :: y
          type(YMM4c8_t),   dimension(1:length), intent(out) :: z
          integer(kind=int4),                    intent(in)  :: length
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          type(YMM4r8_t) :: tmp
          integer(kind=int4) :: i
          ! Exec code ....
          tmp.v = y
!DIR$ VECTOR ALWAYS
          do i = 1, length
              z(i).re = x(i).re - tmp.v
          end do
    end subroutine v4c_c4field1D_sub_r8
    
    subroutine v4c_c4field1D_mul_c4field1D(x,y,z,length)
         type(YMM4c8_t),    dimension(1:length), intent(in)  :: x
         type(YMM4c8_t),    dimension(1:length), intent(in)  :: y
         type(YMM4c8_t),    dimension(1:length), intent(out) :: z
         integer(kind=int4),                     intent(in)  :: length
         ! Locals
         integer(kind=int4) :: i
!DIR$  VECTOR ALWAYS
         do i = 1, length
              z(i).re = x(i).re * y(i).re - x(i).im * y(i).im
              z(i).im = x(i).im * y(i).re + x(i).re * y(i).im
         end do
    end subroutine v4c_c4field1D_mul_c4field1D
    
    subroutine v4c_c4field1D_mul_c8(x,y,z,length)
         type(YMM4c8_t),    dimension(1:length), intent(in)  :: x
         complex(kind=dp),                       intent(in)  :: y
         type(YMM4c8_t),    dimension(1:length), intent(out) :: z
         integer(kind=int4),                     intent(in)  :: length
         ! Locals
         integer(kind=int4) :: i
         ! Exec code ...
!DIR$    VECTOR ALWAYS
         do i = 1, length
             z(i).re = x(i).re *  real(y)  - x(i).im * aimag(y)
             z(i).im = x(i).im *  real(y)  + x(i).re * aimag(y)
         end do
    end subroutine v4c_c4field1D_mul_c8
    
    subroutine v4c_c4field1D_mul_r8(x,y,z,length)
         type(YMM4c8_t),    dimension(1:length), intent(in)  :: x
         real(kind=dp),                          intent(in)  :: y
         type(YMM4c8_t),    dimension(1:length), intent(out) :: z
         integer(kind=int4),                     intent(in)  :: length
         ! locals
!DIR$    ATTRIBUTES ALIGN : 32 :: tmp
         type(YMM4r8_t)     :: tmp
         integer(kind=int4) :: i
         ! Exec code ....
         tmp.v = y
!DIR$    VECTOR ALWAYS
         do i = 1, length
             z(i).re = x(i).re * tmp.v
             z(i).im = x(i).im * tmp.v
         end do
    end subroutine v4c_c4field1D_mul_r8
    
    subroutine v4c_c4field1D_div_c4field1D(x,y,z,length)
         type(YMM4c8_t),     dimension(1:length), intent(in)  :: x
         type(YMM4c8_t),     dimension(1:length), intent(in)  :: y
         type(YMM4c8_t),     dimension(1:length), intent(out) :: z
         integer(kind=int4),                      intent(in)  :: length
         ! Locals
!DIR$    ATTRIBUTES ALIGN : 32 :: tmp,den
         type(YMM4c8_t)     :: tmp
         type(YMM4r8_t)     :: den
         integer(kind=int4) :: i
         ! Exec code ...
!DIR$  VECTOR ALWAYS
         do i = 1, length
             
             tmp(i).re = x(i).re * y(i).re + x(i).im * y(i).im
             tmp(i).im = x(i).im * y(i).re - x(i).re * y(i).im
             den.v     = y(i).re * y(i).re + y(i).im * y(i).im
             z(i).re = tmp(i).re / den.v
             z(i).im = tmp(i).im / den.v
        end do
    end subroutine v4c_c4field1D_div_c4field1D
    
    subroutine v4c_c4field1D_div_c8(x,y,z,length)
          type(YMM4c8_t),       dimension(1:length), intent(in)  :: x
          complex(kind=dp),                          intent(in)  :: y
          type(YMM4c8_t),       dimension(1:length), intent(out) :: z
          integer(kind=int4),                        intent(in)  :: length
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          type(YMM4c8_t) :: tmp
          type(YMM4r8_t) :: den
          ! Exec code ....
!DIR$   VECTOR ALWAYS
          do i = 1, length
              tmp(i).re = x(i).re * real(y) + x(i).im * aimag(y)
              tmp(i).im = x(i).im * real(y) - x(i).re * aimag(y)
              den.v     = real(y) * real(y) + aimag(y) * aimag(y)
              z(i).re   = tmp(i).re / dev.v
              z(i).im   = tmp(i).im / dev.v
          end do
    end subroutine v4c_c4field1D_div_c8
    
    subroutine v4c_c4field1D_div_r8(x,y,z,length)
           type(YMM4c8_t),     dimension(1:length), intent(in)  :: x
           real(kind=dp),                           intent(in)  :: y
           type(YMM4c8_t),     dimension(1:length), intent(out) :: z
           integer(kind=int4),                      intent(in)  :: length
           ! Locals
!DIR$      ATTRIBUTES ALIGN : 32 :: tmp
           type(YMM4r8_t) :: tmp
           integer(kind=int4) :: i
           ! Exec code ...
           tmp.v = y
!DIR$    VECTOR ALWAYS
           do i = 1, length
               z(i).re = x(i).re / tmp.v
               z(i).im = x(i).im / tmp.v
           end do
    end subroutine v4c_c4field1D_div_r8  
    
    subroutine v4c_c4field1D_eq_c4field1D(x,y,eq,length)
          type(YMM4c8_t),     dimension(1:length),      intent(in)  :: x
          type(YMM4c8_t),     dimension(1:length),      intent(in)  :: y
          logical(kind=int4), dimension(1:nlength,0:7), intent(out) :: eq 
          integer(kind=int4),                           intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code
          do i = 1, length
              eq(i,0) = x(i).re(0) == y(i).re(0) 
              eq(i,1) = x(i).re(1) == y(i).re(1) 
              eq(i,2) = x(i).re(2) == y(i).re(2) 
              eq(i,3) = x(i).re(3) == y(i).re(3) 
              eq(i,4) = x(i).im(0) == y(i).im(0) 
              eq(i,5) = x(i).im(1) == y(i).im(1)
              eq(i,6) = x(i).im(2) == y(i).im(2)
              eq(i,7) = x(i).im(3) == y(i).im(3)
          end do
    end subroutine v4c_c4field1D_eq_c4field1D
    
    subroutine v4c_c4field1D_eq_c8(x,y,eq,length)
         type(YMM4c8_t),        dimension(1:length),      intent(in)  :: x
         complex(kind=dp),                                intent(in)  :: y
         logical(kind=int4),     dimension(1:length,0:7), intent(out) :: eq
         integer(kind=int4),                              intent(in)  :: length
         ! Locals
         real(kind=dp) :: re,im
         integer(kind=int4) :: i
         ! Exec code ...
         re = real(y); im = aimag(y)
         do i = 1, length
             eq(i,0) = x(i).re(0) == re
             eq(i,1) = x(i).re(1) == re
             eq(i,2) = x(i).re(2) == re
             eq(i,3) = x(i).re(3) == re
             eq(i,4) = x(i).im(0) == im
             eq(i,5) = x(i).im(1) == im
             eq(i,6) = x(i).im(2) == im
             eq(i,6) = x(i).im(3) == im
             eq(i,7) = x(i).im(4) == im
          end do
    end subroutine v4c_c4field1D_eq_c8
    
    subroutine v4c_c4field1D_eq_r8(x,y,eq,length)
          type(YMM4c8_t),     dimension(1:length),     intent(in)  :: x
          real(kind=dp),                               intent(in)  :: y
          logical(kind=int4), dimension(1:length,0:7), intent(out) :: eq
          integer(kind=int4),                          intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code
          do i = 1, length
             eq(i,0) = x(i).re(0) == y
             eq(i,1) = x(i).re(1) == y
             eq(i,2) = x(i).re(2) == y
             eq(i,3) = x(i).re(3) == y
             eq(i,4) = x(i).im(0) == y
             eq(i,5) = x(i).im(1) == y
             eq(i,6) = x(i).im(2) == y
             eq(i,6) = x(i).im(3) == y
             eq(i,7) = x(i).im(4) == y
           end do
    end subroutine v4c_c4field1D_eq_r8
    
    subroutine v4c_c4field1D_neq_c4field1D(x,y,neq,length)
          type(YMM4c8_t),     dimension(1:length),      intent(in)  :: x
          type(YMM4c8_t),     dimension(1:length),      intent(in)  :: y
          logical(kind=int4), dimension(1:nlength,0:7), intent(out) :: neq 
          integer(kind=int4),                           intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code
          do i = 1, length
              neq(i,0) = x(i).re(0) /= y(i).re(0) 
              neq(i,1) = x(i).re(1) /= y(i).re(1) 
              neq(i,2) = x(i).re(2) /= y(i).re(2) 
              neq(i,3) = x(i).re(3) /= y(i).re(3) 
              neq(i,4) = x(i).im(0) /= y(i).im(0) 
              neq(i,5) = x(i).im(1) /= y(i).im(1)
              neq(i,6) = x(i).im(2) /= y(i).im(2)
              neq(i,7) = x(i).im(3) /= y(i).im(3)
          end do
    end subroutine v4c_c4field1D_neq_c4field1D
        
    subroutine v4c_c4field1D_neq_c8(x,y,neq,length)
          type(YMM4c8_t),        dimension(1:length),      intent(in)  :: x
          complex(kind=dp),                                intent(in)  :: y
          logical(kind=int4),     dimension(1:length,0:7), intent(out) :: neq
          integer(kind=int4),                              intent(in)  :: length
         ! Locals
          real(kind=dp) :: re,im
          integer(kind=int4) :: i
         ! Exec code ...
          re = real(y); im = aimag(y)
          do i = 1, length
             neq(i,0) = x(i).re(0) /= re
             neq(i,1) = x(i).re(1) /= re
             neq(i,2) = x(i).re(2) /= re
             neq(i,3) = x(i).re(3) /= re
             neq(i,4) = x(i).im(0) /= im
             neq(i,5) = x(i).im(1) /= im
             neq(i,6) = x(i).im(2) /= im
             neq(i,6) = x(i).im(3) /= im
             neq(i,7) = x(i).im(4) /= im
           end do
    end subroutine v4c_c4field1D_neq_c8
    
    subroutine v4c_c4field1D_neq_r8(x,y,neq,length)
          type(YMM4c8_t),     dimension(1:length),     intent(in)  :: x
          real(kind=dp),                               intent(in)  :: y
          logical(kind=int4), dimension(1:length,0:7), intent(out) :: neq
          integer(kind=int4),                          intent(in)  :: length
          ! Locals
          integer(kind=int4) :: i
          ! Exec code
          do i = 1, length
             neq(i,0) = x(i).re(0) /= y
             neq(i,1) = x(i).re(1) /= y
             neq(i,2) = x(i).re(2) /= y
             neq(i,3) = x(i).re(3) /= y
             neq(i,4) = x(i).im(0) /= y
             neq(i,5) = x(i).im(1) /= y
             neq(i,6) = x(i).im(2) /= y
             neq(i,6) = x(i).im(3) /= y
             neq(i,7) = x(i).im(4) /= y
           end do
    end subroutine v4c_c4field1D_neq_r8
    
    subroutine v4c_c4field1D_gte_c4field1D(x,y,gte,length,op)
          type(YMM4c8_t),       dimension(1:length),     intent(in)  :: x
          type(YMM4c8_t),       dimension(1:length),     intent(in)  :: y
          logical(kind=int4),   dimension(1:length,0:7), intent(out) :: gte
          integer(kind=int4),                            intent(in)  :: length
          character(len=*),                              intent(in)  :: op
          ! Locals
          integer(kind=int4) :: i
          ! Exec code ....
          select case(op)
          case ("GT")
              do i = 1, length
                  gte(i,0) = x(i).re(0) > y(i).re(0)
                  gte(i,1) = x(i).re(1) > y(i).re(1)
                  gte(i,2) = x(i).re(2) > y(i).re(2)
                  gte(i,3) = x(i).re(3) > y(i).re(3)
                  gte(i,4) = x(i).im(0) > y(i).im(0)
                  gte(i,5) = x(i).im(1) > y(i).im(1)
                  gte(i,6) = x(i).im(2) > y(i).im(2)
                  gte(i,7) = x(i).im(3) > y(i).im(3)
              end do
          case ("GTE")
              do i = 1, length
                  gte(i,0) = x(i).re(0) >= y(i).re(0)
                  gte(i,1) = x(i).re(1) >= y(i).re(1)
                  gte(i,2) = x(i).re(2) >= y(i).re(2)
                  gte(i,3) = x(i).re(3) >= y(i).re(3)
                  gte(i,4) = x(i).im(0) >= y(i).im(0)
                  gte(i,5) = x(i).im(1) >= y(i).im(1)
                  gte(i,6) = x(i).im(2) >= y(i).im(2)
                  gte(i,7) = x(i).im(3) >= y(i).im(3)
              end do
          case default
              print*, "v4c_c4field1D_gte_c4field1D: -- Invalid select case "
              return
          end select
          
    end subroutine v4c_c4field1D_gte_c4field1D
    
    subroutine v4c_c4field1D_lte_c4field1D(x,y,lte,length,op)
          type(YMM4c8_t),       dimension(1:length),     intent(in)  :: x
          type(YMM4c8_t),       dimension(1:length),     intent(in)  :: y
          logical(kind=int4),   dimension(1:length,0:7), intent(out) :: lte
          integer(kind=int4),                            intent(in)  :: length
          character(len=*),                              intent(in)  :: op
          ! Locals
          integer(kind=int4) :: i
           ! Exec code ....
          select case(op)
          case ("LT")
              do i = 1, length
                  lte(i,0) = x(i).re(0) < y(i).re(0)
                  lte(i,1) = x(i).re(1) < y(i).re(1)
                  lte(i,2) = x(i).re(2) < y(i).re(2)
                  lte(i,3) = x(i).re(3) < y(i).re(3)
                  lte(i,4) = x(i).im(0) < y(i).im(0)
                  lte(i,5) = x(i).im(1) < y(i).im(1)
                  lte(i,6) = x(i).im(2) < y(i).im(2)
                  lte(i,7) = x(i).im(3) < y(i).im(3)
              end do
          case ("LTE")
              do i = 1, length
                  lte(i,0) = x(i).re(0) <= y(i).re(0)
                  lte(i,1) = x(i).re(1) <= y(i).re(1)
                  lte(i,2) = x(i).re(2) <= y(i).re(2)
                  lte(i,3) = x(i).re(3) <= y(i).re(3)
                  lte(i,4) = x(i).im(0) <= y(i).im(0)
                  lte(i,5) = x(i).im(1) <= y(i).im(1)
                  lte(i,6) = x(i).im(2) <= y(i).im(2)
                  lte(i,7) = x(i).im(3) <= y(i).im(3)
              end do
          case default
              print*, "v4c_c4field1D_lte_c4field1D: -- Invalid select case "
              return
          end select
    end subroutine v4c_c4field1D_lte_c4field1D
    
    subroutine v4c_c4field1D_gte_c8(x,y,gte,length,op)
         type(YMM4c8_t),    dimension(1:length),      intent(in)  :: x
         complex(kind=dp),                            intent(in)  :: y
         logical(kind=int4), dimension(1:length,0:7), intent(out) :: gte
         integer(kind=int4),                          intent(in)  :: length
         character(len=*),                            intent(in)  :: op
         ! Locals
         real(kind=dp) :: re,im
         integer(kind=int4) :: i
         ! Exec code ....
         re = real(y); im = aimag(y)
         select case (op)
         case ("GT")
             do i = 1, length
                 gte(i,0) = x(i).re(0) > re
                 gte(i,1) = x(i).re(1) > re
                 gte(i,2) = x(i).re(2) > re
                 gte(i,3) = x(i).re(3) > re
                 gte(i,4) = x(i).im(0) > im
                 gte(i,5) = x(i).im(1) > im
                 gte(i,6) = x(i).im(2) > im
                 gte(i,7) = x(i).im(3) > im
             end do
         case ("GTE")
             do i = 1, length
                 gte(i,0) = x(i).re(0) >= re
                 gte(i,1) = x(i).re(1) >= re
                 gte(i,2) = x(i).re(2) >= re
                 gte(i,3) = x(i).re(3) >= re
                 gte(i,4) = x(i).im(0) >= im
                 gte(i,5) = x(i).im(1) >= im
                 gte(i,6) = x(i).im(2) >= im
                 gte(i,7) = x(i).im(3) >= im
             end do
         case default
              print*, "v4c_c4field1D_gte_c8: -- Invalid select case !!!"
              return
         end select
    end subroutine v4c_c4field1D_gte_c8
    
    subroutine v4c_c4field1D_lte_c8(x,y,lte,length,op)
         type(YMM4c8_t),     dimension(1:length),      intent(in)  :: x
         complex(kind=dp),                            intent(in)   :: y
         logical(kind=int4), dimension(1:length,0:7), intent(out)  :: lte
         integer(kind=int4),                          intent(in)   :: length
         character(len=*),                            intent(in)   :: op
         ! Locals
         real(kind=dp) :: re,im
         integer(kind=int4) :: i
         ! Exec code ....
         re = real(y); im = aimag(y)
         select case (op)
         case ("LT")
             do i = 1, length
                 lte(i,0) = x(i).re(0) < re
                 lte(i,1) = x(i).re(1) < re
                 lte(i,2) = x(i).re(2) < re
                 lte(i,3) = x(i).re(3) < re
                 lte(i,4) = x(i).im(0) < im
                 lte(i,5) = x(i).im(1) < im
                 lte(i,6) = x(i).im(2) < im
                 lte(i,7) = x(i).im(3) < im
             end do
         case ("LTE")
             do i = 1, length
                 lte(i,0) = x(i).re(0) <= re
                 lte(i,1) = x(i).re(1) <= re
                 lte(i,2) = x(i).re(2) <= re
                 lte(i,3) = x(i).re(3) <= re
                 lte(i,4) = x(i).im(0) <= im
                 lte(i,5) = x(i).im(1) <= im
                 lte(i,6) = x(i).im(2) <= im
                 lte(i,7) = x(i).im(3) <= im
             end do
         case default
              print*, "v4c_c4field1D_lte_c8: -- Invalid select case !!!"
              return
         end select
    end subroutine v4c_c4field1D_lte_c8
     
     pure function v4c_ymm4c8_normalize_prod(x,y) result(z)
          type(YMM4c8_t),       intent(in) :: x
          type(YMM4c8_t),       intent(in) :: y
          ! Locals
          type(YMM4c8_t) :: z,tmp
          type(YMM4r8_t) :: mag
          ! Exec code ....
          tmp.re = (x.re * y.re) - (x.im * y.im)
          tmp.im = (x.im * y.re) + (x.re * y.im)
          mag.v = SQRT(tmp.re*tmp.re + tmp.im*tmp.im)
          z.re = tmp.re / mag.v
          z.im = tmp.im / mag.v
     end function v4c_ymm4c8_normalize_prod
     
     pure function v4c_ymm4c8_conjugate_prod(x,y) result(z)
          use mod_vectypes, only : YMM4r8_t
          type(YMM4c8_t),         intent(in) :: x
          type(YMM4c8_t),         intent(in) :: y
          ! Locals
          type(YMM4c8_t) :: z
          ! Exec code ...
          z.re = (x.re * y.re) + (x.im * y.im)
          z.im = (x.im * y.re) - (x.re * y.im)
     end function v4c_ymm4c8_conjugate_prod
     
     pure function v4c_ymm4c8_normalize_conjugate_prod(x,y) result(z)
          use mod_vectypes, only : YMM4r8_t
          type(YMM4c8_t),         intent(in) :: x
          type(YMM4c8_t),         intent(in) :: y
          ! Locals
          type(YMM4c8_t) :: z,tmp
          type(YMM4r8_t) :: mag
          ! Exec code ....
          tmp.re = (x.re * y.re) - (x.im * y.im)
          tmp.im = (x.im * y.re) + (x.re * y.im)
          mag.v =  SQRT(tmp.re*tmp.re + tmp.im*tmp.im)
          z.re = tmp.re / mag.v
          z.im = tmp.im / mag.v
     end function v4c_ymm4c8_normalize_conjugate_prod
     
     pure function v4c_ymm4c8_normalize(x,y) result(z)
          use mod_vectypes, only : YMM4r8_t
          type(YMM4c8_t),          intent(in) :: x
          type(YMM4c8_t),          intent(in) :: y
          ! Locals
          type(YMM4c8_t) :: z
          type(YMM4r8_t) :: mag
          ! Exec code ....
          mag.v = SQRT(x.re*y.re + x.im*y.im)
          z.re = x.re / mag.v
          z.im = x.im / mag.v
     end function v4c_ymm4c8_normalize
     
     pure function v4c_ymm4c8_magnitude(x) result(z)
          use mod_vectypes, only : YMM4r8_t
          type(YMM4c8_t),    intent(in) :: x
          ! Locals
          type(YMM4r8_t) :: z
          ! Exec code ...
          z.v = SQRT(x.re*x.re + x.im*x.im)
     end function v4c_ymm4c8_magnitude
     
      ! 8-wide complex vector addition
     
      pure function v8c_zmm8c8_add_zmm8c8(x,y) result(z)
          type(ZMM8c8_t),       intent(in) :: x
          type(ZMM8c8_t),       intent(in) :: y
          ! Locals
          type(ZMM8c8_t) :: z
          ! Exec code ....
          z.re = x.re + y.re
          z.im = x.im + y.im
      end function  v8c_zmm8c8_add_zmm8c8
      
      pure function v8c_zmm8c8_add_c8(x,y) result(z)
          type(ZMM8c8_t),       intent(in) :: x
          complex(kind=dp),     intent(in) :: y
          ! Locals
          type(ZMM8c8_t) :: z
          ! Exec code ....
          z.re = x.re + real(y)
          z.im = x.im + aimag(y)
      end function v8c_zmm8c8_add_c8
      
      pure function v8c_zmm8c8_add_r8(x,y) result(z)
           type(ZMM8c8_t),       intent(in) :: x
           real(kind=dp),        intent(in) :: y
           ! Locals
           type(ZMM8c8_t) :: z
           ! Exec code ...
           z.re = x.re + y
           z.im = x.im + y
      end function v8c_zmm8c8_add_r8
      
      pure function v8c_zmm8c8_sub_zmm8c8(x,y) result(z)
           type(ZMM8c8_t),        intent(in) :: x
           type(ZMM8c8_t),        intent(in) :: y
           ! Locals
           type(ZMM8c8_t) :: z
           ! Exec code
           z.re = x.re - y.re
           z.im = x.im - y.im
      end function v8c_zmm8c8_sub_zmm8c8
      
      pure function v8c_zmm8c8_sub_c8(x,y) result(z)
           type(ZMM8c8_t),         intent(in) :: x
           complex(kind=dp),       intent(in) :: y
           ! Locals
           type(ZMM8c8_t) :: z
           !Exec code....
           z.re = x.re - real(y)
           z.im = x.im - aimag(y)
      end function v8c_zmm8c8_sub_c8
      
      pure function v8c_zmm8c8_sub_r8(x,y) result(z)
           type(ZMM8c8_t),         intent(in) :: x
           real(kind=dp),          intent(in) :: y
           ! Locals
           type(ZMM8c8_t) :: z
           ! Exec code ...
           z.re = x.re - y
           z.im = x.im - y
      end function v8c_zmm8c8_sub_r8
      
      pure function v8c_zmm8c8_mul_zmm8c8(x,y) result(z)
            type(ZMM8c8_t),         intent(in) :: x
            type(ZMM8c8_t),         intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z
            ! Exec code ....
             z.re = (x.re * y.re) - (x.im * y.im)
             z.im = (x.im * y.re) + (x.re * y.im)
      end function v8c_zmm8c8_mul_zmm8c8
      
      pure function v8c_zmm8c8_mul_c8(x,y) result(z)
            type(ZMM8c8_t),         intent(in) :: x
            complex(kind=dp),       intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z
            ! Exec code ...
            z.re = x.re * real(y)
            z.im = x.im * aimag(y)
      end function v8c_zmm8c8_mul_c8
      
      pure function v8c_zmm8c8_r8(x,y) result(z)
            type(ZMM8c8_t),         intent(in) :: x
            real(kind=dp),          intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z
            ! Exec code ....
            z.re = x.re * y
            z.im = x.im * y
      end function v8c_zmm8c8_r8
      
      pure function v8c_zmm8c8_div_zmm8c8(x,y) result(z)
            type(ZMM8c8_t),         intent(in) :: x
            type(ZMM8c8_t),         intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z,tmp,den
            ! Exec code ....
            tmp.re = (x.re * y.im) + (x.im * y.im)
            tmp.im = (x.im * y.im) - (x.re * y.im)
            den.re = (y.re * y.re) + (y.im * y.im)
            z.re = tmp.re / den.re
            z.im = tmp.im / den.re
      end function v8c_zmm8c8_div_zmm8c8
      
      pure function v8c_zmm8c8_div_c8(x,y) result(z)
            type(ZMM8c8_t),        intent(in) :: x
            complex(kind=dp),      intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z
            ! Exec code ...
            z.re = x.re / real(y)
            z.im = x.im / aimag(y)
      end function v8c_zmm8c8_div_c8
      
      pure function v8c_zmm8c8_div_r8(x,y) result(z)
            type(ZMM8c8_t),         intent(in) :: x
            real(kind=dp),          intent(in) :: y
            ! Locals
            type(ZMM8c8_t) :: z
            ! Exec code ...
            z.re = x.re / y
            z.im = x.im / y
      end function v8c_zmm8c8_div_r8
      
      pure function v8c_zmm8c8_eq_zmm8c8(x,y) result(eq)
           type(ZMM8c8_t),          intent(in) :: x
           type(ZMM8c8_t),          intent(in) :: y
           ! Locals
           logical(kind=int4), dimension(0:1) :: eq
           ! Exec code ...
           eq(0) = x.re(0) == y.re(0) .and. &
                   x.re(1) == y.re(1) .and. &
                   x.re(2) == y.re(2) .and. &
                   x.re(3) == y.re(3) .and. &
                   x.re(4) == y.re(4) .and. &
                   x.re(5) == y.re(5) .and. &
                   x.re(6) == y.re(6) .and. &
                   x.re(7) == y.re(7)
           eq(1) = x.im(0) == y.im(0) .and. &
                   x.im(1) == y.im(1) .and. &
                   x.im(2) == y.im(2) .and. &
                   x.im(3) == y.im(3) .and. &
                   x.im(4) == y.im(4) .and. &
                   x.im(5) == y.im(5) .and. &
                   x.im(6) == y.im(6) .and. &
                   x.im(7) == y.im(7)
      end function v8c_zmm8c8_eq_zmm8c8
      
      pure function v8c_zmm8c8_eq_c8(x,y) result(eq)
           type(ZMM8c8_t),          intent(in) :: x
           complex(kind=dp),        intent(in) :: y
           ! Locals
           logical(kind=int4), dimension(0:1) :: eq
           ! Exec code ....
           eq(0) = x.re(0) == real(y)  .and. &
                   x.re(1) == real(y)  .and. &
                   x.re(2) == real(y)  .and. &
                   x.re(3) == real(y)  .and. &
                   x.re(4) == real(y)  .and. &
                   x.re(5) == real(y)  .and. &
                   x.re(6) == real(y)  .and. &
                   x.re(7) == real(y) 
           eq(1) = x.im(0) == aimag(y) .and. &
                   x.im(1) == aimag(y) .and. &
                   x.im(2) == aimag(y) .and. &
                   x.im(3) == aimag(y) .and. &
                   x.im(4) == aimag(y) .and. &
                   x.im(5) == aimag(y) .and. &
                   x.im(6) == aimag(y) .and. &
                   x.im(7) == aimag(y)
      end function v8c_zmm8c8_eq_c8
      
      pure function v8c_zmm8c8_eq_r8(x,y) result(eq)
            type(ZMM8c8_t),         intent(in) :: x
            real(kind=dp),          intent(in) :: y
            ! Locals
            logical(kind=int4) :: eq
            ! Exec code ....
            eq = x.re(0) == y .and. &
                 x.re(1) == y .and. &
                 x.re(2) == y .and. &
                 x.re(3) == y .and. &
                 x.re(4) == y .and. &
                 x.re(5) == y .and. &
                 x.re(6) == y .and. &
                 x.re(6) == y
      end function v8c_zmm8c8_eq_r8
      
      pure function v8c_zmm8c8_neq_zmm8c8(x,y) result(neq)
          type(ZMM8c8_t),          intent(in) :: x
          type(ZMM8c8_t),          intent(in) :: y
           ! Locals
           logical(kind=int4), dimension(0:1) :: neq
           ! Exec code ...
           neq(0) = x.re(0)   /= y.re(0) .and. &
                    x.re(1)   /= y.re(1) .and. &
                    x.re(2)   /= y.re(2) .and. &
                    x.re(3)   /= y.re(3) .and. &
                    x.re(4)   /= y.re(4) .and. &
                    x.re(5)   /= y.re(5) .and. &
                    x.re(6)   /= y.re(6) .and. &
                    x.re(7)   /= y.re(7)
           neq(1) = x.im(0)   /= y.im(0) .and. &
                    x.im(1)   /= y.im(1) .and. &
                    x.im(2)   /= y.im(2) .and. &
                    x.im(3)   /= y.im(3) .and. &
                    x.im(4)   /= y.im(4) .and. &
                    x.im(5)   /= y.im(5) .and. &
                    x.im(6)   /= y.im(6) .and. &
                    x.im(7)   /= y.im(7)
      end function v8c_zmm8c8_neq_zmm8c8
      
      pure function v8c_zmm8c8_neq_c8(x,y) result(neq)
           type(ZMM8c8_t),          intent(in) :: x
           complex(kind=dp),        intent(in) :: y
           ! Locals
           logical(kind=int4), dimension(0:1) :: neq
           ! Exec code ....
           neq(0) = x.re(0) /= real(y)  .and. &
                   x.re(1)  /= real(y)  .and. &
                   x.re(2)  /= real(y)  .and. &
                   x.re(3)  /= real(y)  .and. &
                   x.re(4)  /= real(y)  .and. &
                   x.re(5)  /= real(y)  .and. &
                   x.re(6)  /= real(y)  .and. &
                   x.re(7)  /= real(y) 
           neq(1) = x.im(0) /= aimag(y) .and. &
                   x.im(1)  /= aimag(y) .and. &
                   x.im(2)  /= aimag(y) .and. &
                   x.im(3)  /= aimag(y) .and. &
                   x.im(4)  /= aimag(y) .and. &
                   x.im(5)  /= aimag(y) .and. &
                   x.im(6)  /= aimag(y) .and. &
                   x.im(7)  /= aimag(y)
      end function v8c_zmm8c8_neq_c8
      
      pure function v8c_zmm8c8_neq_r8(x,y) result(neq)
           type(ZMM8c8_t),         intent(in) :: x
           real(kind=dp),          intent(in) :: y
            ! Locals
            logical(kind=int4) :: neq
            ! Exec code ....
            neq = x.re(0) /= y .and. &
                 x.re(1)  /= y .and. &
                 x.re(2)  /= y .and. &
                 x.re(3)  /= y .and. &
                 x.re(4)  /= y .and. &
                 x.re(5)  /= y .and. &
                 x.re(6)  /= y .and. &
                 x.re(6)  /= y
      end function v8c_zmm8c8_neq_r8

end module mod_vcmplexop