
module mod_vcmplx16

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_vcmplx16'
 !          
 !          Purpose:
 !                     SIMD-like operations on derived types
 !                     which represent decomposed (re,im) parts
 !                     complex numbers.
 !                     Based on real (kind=16) components.
 !                     For use in planned vectorization of NEC4-1, NEC-2 MoM models 
 !
 !          History:
 !                        
 !                        Date: 24-10-2018
 !                        Time: 10:59 GMT+2
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
     use mod_kinds, only : int4, ep
     use mod_vectypes, only : YMM2c16_t, ZMM4c16_t
     implicit none
     
     public   
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_VCMPLX16_MAJOR = 1_int4
     
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_VCMPLX16_MINOR = 0_int4
    
    ! Micor version
    integer(kind=int4), parameter, public :: MOD_VCMPLX16_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_VCMPLX16_FULLVER = 1000_int4*MOD_VCMPLX16_MAJOR+100_int4*MOD_VCMPLX16_MINOR+ &
                                                                    10_int4*MOD_VCMPLX16_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VCMPLX16_CREATE_DATE = "24-10-2018 10:59 +00200 (WED 24 OCT 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_VCMPLX16_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_VCMPLX16_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOC_VCMPLX16_SYNOPSIS = "Packed AoS complex(16) arithemtic operations."
    
    contains
    
    

!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_add_ymm2c16 
    pure elemental function ymm2c16_add_ymm2c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_add_ymm2c16 
          type(YMM2c16_t),  intent(in) :: x
          type(YMM2c16_t),  intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ...
          z.re = x.re + y.re
          z.im = x.im + y.im
    end function ymm2c16_add_ymm2c16

!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_add_c16
    pure elemental function ymm2c16_add_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: ymm2c16_add_c16
         type(YMM2c16_t),   intent(in) :: x
         complex(kind=16),  intent(in) :: y
         ! Locals
!DIR$    ATTRIBUTES ALIGN : 32 :: z
         type(YMM2c16_t) :: z
         ! Exec code ...
         z.re = x.re + real(y)
         z.im = x.im + aimag(y)
    end function ymm2c16_add_c16

!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_add_r8
    pure elemental function ymm2c16_add_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: ymm2c16_add_r8
          
          type(YMM2c16_t),    intent(in) :: x
          real(kind=dp),      intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ...
          z.re = x.re + y ! implicit conversion to real(16)
          z.im = x.im + y ! implicit conversion to real(16)
    end function ymm2c16_add_r8

!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_sub_ymm2c16
    pure elemental function ymm2c16_sub_ymm2c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: ymm2c16_sub_ymm2c16
          type(YMM2c16_t),     intent(in) :: x
          type(YMM2c16_t),     intent(in) :: y
          ! Locals
!DIR$ ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ....
          z.re = x.re - y.re
          z.im = x.im - y.im
    end function ymm2c16_sub_ymm2c16

!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_sub_c16
    pure elemental function ymm2c16_sub_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: ymm2c16_sub_c16
          type(YMM2c16_t),      intent(in) :: x
          type(YMM2c16_t),      intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM216c_t) :: z
          ! Exe code ....
          z.re = x.re - real(y)
          z.im = x.im - aimag(y)
    end function ymm2c16_sub_c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_sub_r8
    pure elemental function ymm2c16_sub_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_sub_r8 
          type(YMM2c16_t),      intent(in) :: x
          real(kind=dp),        intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ....
          z.re = x.re - y
          z.im = x.im - y
    end function  ymm2c16_sub_r8
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_mul_ymm2c16
    pure elemental function ymm2c16_mul_ymm2c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_mul_ymm2c16
          type(YMM2c16_t),       intent(in) :: x
          type(YMM2c16_t),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ....
          z.re = x.re * y.re - x.im * y.im
          z.im = x.im * y.re + x.re * y.im
    end function ymm2c16_mul_ymm2c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_mul_c16
    pure elemental function ymm2c16_mul_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_mul_c16  
          type(YMM2c16_t),        intent(in) :: x
          complex(kind=16),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ...
          z.re = x.re * real(y) - x.im * aimag(y)
          z.im = x.im * real(y) + x.re * aimag(y)
    end function ymm2c16_mul_c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_mul_r8
    pure elemental function ymm2c16_mul_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_mul_r8
          type(YMM2c16_t),         intent(in) :: x
          real(kind=dp),           intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
          ! Exec code ...
          z.re = x.re * y
          z.im = x.im * y
    end function ymm2c16_mul_r8
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_div_ymm2c16
    pure elemental function ymm2c16_div_ymm2c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_div_ymm2c16
          use mod_vectypes, only :  YMM2r16_t
          type(YMM2c16_t),         intent(in) :: x
          type(YMM2c16_t),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z,tmp
          type(YMM2c16_t) :: z,tmp
!DIR$     ATTRIBUTES ALIGN : 32 :: den
          type(YMM2r16_t) :: den
          ! Exec code ...
          den.v = 0.0E+00_ep
          tmp.re = x.re * y.re + x.im * y.im
          tmp.im = x.im * y.re - x.re * y.im
          den.v  = y.re * y.re + y.im * y.im
          z.re = tmp.re / den.re
          z.im = tmp.im / den.re
    end function ymm2c16_div_ymm2c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_div_c16
    pure elemental function ymm2c16_div_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_div_c16  
          type(YMM2c16_t),          intent(in) :: x
          complex(kind=16),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z,tmp
          type(YMM2c16_t) :: z,tmp
!DIR$     ATTRIBUTES ALIGN : 32 :: den
          type(YMM2r16_t) :: den
          ! Exec code ...
          den.v = 0.0E+00_ep
          tmp.re = x.re * real(y) + x.im * aimag(y)
          tmp.im = x.im * real(y) - x.re * aimag(y)
          den.v  = real(y) * real(y) + aimag(y) * aimag(y)
          z.re = tmp.re / den.v
          z.im = tmp.im / den.v
    end function ymm2c16_div_c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: ymm2c16_div_r8
    pure elemental function ymm2c16_div_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_div_r8
          use mod_vectypes, only : YMM2r16_t
          type(YMM2c16_t),           intent(in) :: x
          real(kind=dp),             intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 32 :: z
          type(YMM2c16_t) :: z
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          type(YMM2r16_t) :: tmp
          ! Exec code
          tmp.v = y
          z.re = x.re / tmp.v
          z.im = x.im / tmp.v
    end function ymm2c16_div_r8
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_add_zmm4c16
     pure elemental function zmm4c16_add_zmm4c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: zmm4c16_add_zmm4c16
          type(ZMM4c16_t),      intent(in) :: x
          type(ZMM4c16_t),      intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ...
          z.re = x.re + y.re
          z.im = x.im + y.im
     end function zmm4c16_add_zmm4c16
     
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_add_c16
     pure elemental function zmm4c16_add_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: zmm4c16_add_c16    
          type(ZMM4c16_t),      intent(in) :: x
          complex(kind=16),     intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ...
          z.re = x.re + real(y)
          z.im = x.im + aimag(y)
     end function zmm4c16_add_c16
     
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_add_r8
     pure elemental function zmm4c16_add_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: zmm4c16_add_r8
        type(ZMM4c16_t),         intent(in) :: x
        real(kind=dp),           intent(in) :: y
        ! Locals
!DIR$   ATTRIBUTES ALIGN : 64 :: z
        type(ZMM4c16_t) :: z
        ! Exec code ...
        z.re = x.re / y
        z.im = x.im / y
     end function zmm4c16_add_r8
     
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_sub_zmm4c16
     pure elemental function zmm4c16_sub_zmm4c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: zmm4c16_sub_zmm4c16
          type(ZMM4c16_t),         intent(in) :: x
          type(ZMM4c16_t),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ....
          z.re = x.re - y.re
          z.im = x.im - y.im
     end function zmm4c16_sub_zmm4c16
     
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_sub_c16
     pure elemental function zmm4c16_sub_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR :: zmm4c16_sub_c16
          type(ZMM4c16_t),          intent(in) :: x
          complex(kind=16),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ...
          z.re = x.re - real(y)
          z.im = x.im - aimag(y)
     end function zmm4c16_sub_c16
     
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_sub_r8
    pure elemental function zmm4c16_sub_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_sub_r8 
          type(ZMM4c16_t),      intent(in) :: x
          real(kind=dp),        intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ....
          z.re = x.re - y
          z.im = x.im - y
    end function  zmm4c16_sub_r8
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_mul_zmm4c16
    pure elemental function zmm4c16_mul_zmm4c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_mul_zmm4c16
          type(ZMM4c16_t),       intent(in) :: x
          type(ZMM4c16_t),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ....
          z.re = x.re * y.re - x.im * y.im
          z.im = x.im * y.re + x.re * y.im
    end function zmm4c16_mul_zmm4c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_mul_c16
    pure elemental function zmm4c16_mul_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_mul_c16  
          type(ZMM4c16_t),        intent(in) :: x
          complex(kind=16),       intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ...
          z.re = x.re * real(y) - x.im * aimag(y)
          z.im = x.im * real(y) + x.re * aimag(y)
    end function zmm4c16_mul_c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_mul_r8
    pure elemental function zmm4c16_mul_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_mul_r8
          type(ZMM4c16_t),         intent(in) :: x
          real(kind=dp),           intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
          ! Exec code ...
          z.re = x.re * y
          z.im = x.im * y
    end function zmm4c16_mul_r8
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_div_zmm4c16
    pure elemental function zmm4c16_div_zmm4c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  ymm2c16_div_ymm2c16
          use mod_vectypes, only :  ZMM4r16_t
          type(ZMM4c16_t),         intent(in) :: x
          type(ZMM4c16_t),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z,tmp
          type(ZMM4c16_t) :: z,tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: den
          type(ZMM4r16_t) :: den
          ! Exec code ...
          den.v = 0.0E+00_ep
          tmp.re = x.re * y.re + x.im * y.im
          tmp.im = x.im * y.re - x.re * y.im
          den.v  = y.re * y.re + y.im * y.im
          z.re = tmp.re / den.re
          z.im = tmp.im / den.re
    end function zmm4c16_div_zmm4c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_div_c16
    pure elemental function zmm4c16_div_c16(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_div_c16  
          type(ZMM4c16_t),          intent(in) :: x
          complex(kind=16),         intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z,tmp
          type(ZMM4c16_t) :: z,tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: den
          type(ZMM4r16_t) :: den
          ! Exec code ...
          den.v = 0.0E+00_ep
          tmp.re = x.re * real(y) + x.im * aimag(y)
          tmp.im = x.im * real(y) - x.re * aimag(y)
          den.v  = real(y) * real(y) + aimag(y) * aimag(y)
          z.re = tmp.re / den.v
          z.im = tmp.im / den.v
    end function zmm4c16_div_c16
    
!DIR$ ATTRIBUTES FORCEINLINE :: zmm4c16_div_r8
    pure elemental function zmm4c16_div_r8(x,y) result(z)
!DIR$ ATTRIBUTES VECTOR ::  zmm4c16_div_r8
          use mod_vectypes, only : ZMM4r16_t
          type(ZMM4c16_t),           intent(in) :: x
          real(kind=dp),             intent(in) :: y
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: z
          type(ZMM4c16_t) :: z
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          type(ZMM4r16_t) :: tmp
          ! Exec code
          tmp.v = y
          z.re = x.re / tmp.v
          z.im = x.im / tmp.v
    end function zmm4c16_div_r8     

end module mod_vcmplx16