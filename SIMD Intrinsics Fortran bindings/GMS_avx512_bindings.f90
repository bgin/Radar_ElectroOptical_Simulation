

module mod_avx512_bindings


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_avx512_bindings'
 !          
 !          Purpose:
 !                    Fortran bindings to Intel AVX512 Intrinsics.
 !                   
 !                     
 !          History:
 !                        Date: 06-11-2019
 !                        Time: 10:21 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
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
     use mod_kinds, only : i4
     use, intrinsic :: ISO_C_BINDING
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4),  parameter :: MOD_AVX512_BINDINGS_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: MOD_AVX512_BINDINGS_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: MOD_AVX512_BINDINGS_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: MOD_AVX512_BINDINGS_FULLVER =  &
         1000*MOD_AVX512_BINDINGS_MAJOR+100*MOD_AVX512_BINDINGS_MINOR+10*MOD_AVX512_BINDINGS_MICRO
    ! Module creation date
    character(*),        parameter :: MOD_AVX512_BINDINGS_CREATE_DATE = "06-11-2019 10:21 +00200 (WED 06 NOV 2019 GMT+2)"
    ! Module build date
    character(*),        parameter :: MOD_AVX512_BINDINGS_BUILD_DATE  = "00-00-0000 00:00"
    ! Module author info
    character(*),        parameter :: MOD_AVX512_BINDINGS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Module short description
    character(*),        parameter :: MOD_AVX512_BINDINGS_SYNOPSIS = "Fortran bindings to Intel AVX512 Intrinsics"

    ! Interoperable with C-side v8f64 and v16f32 structs
    type, bind(c), public :: v8f64
       
       real(c_double), dimension(0:7) :: zmm
    end type v8f64

    type, bind(c), public :: v16f32
       
       real(c_float), dimension(0:15) :: zmm
    end type v16f32

    interface
       function v8f64_add_pd(a,b) &
            bind(c,name='v8f64_add_pd')
            import :: v8f64
            type(v8f64),   intent(in) :: a
            type(v8f64),   intent(in) :: b
            type(v8f64) :: v8f64_add_pd
       end function v8f64_add_pd
    end interface

    interface
       function v16f32_add_ps(a,b) &
            bind(c,name='v16f32_add_ps')
            import :: v16f32
            type(v16f32),   intent(in) :: a
            type(v16f32),   intent(in) :: b
            type(v16f32) :: v16f32_add_ps
       end function v16f32_add_ps
    end interface

    interface 
       function v8f64_sub_pd(a,b) &
            bind(c,name='v8f64_sub_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_sub_pd
       end function v8f64_sub_pd
    end interface

     interface 
       function v16f32_sub_ps(a,b) &
            bind(c,name='v16f32_sub_ps')
            import :: v16f32
            type(v16f32),  intent(in) :: a
            type(v16f32),  intent(in) :: b
            type(v16f32) :: v16f32_sub_ps
       end function v16f32_sub_ps
    end interface

    interface
       function v8f64_mul_pd(a,b) &
            bind(c,name='v8f64_mul_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_mul_pd
       end function v8f64_mul_pd
    end interface

     interface
       function v16f32_mul_ps(a,b) &
            bind(c,name='v16f32_mul_ps')
            import :: v16f32
            type(v16f32),  intent(in) :: a
            type(v16f32),  intent(in) :: b
            type(v16f32) :: v16f32_mul_ps
       end function v16f32_mul_ps
    end interface

    interface
       function v8f64_div_pd(a,b) &
            bind(c,name='v8f64_div_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_div_pd
        end function v8f64_div_pd
    end interface

    interface
       function v16f32_div_ps(a,b) &
            bind(c,name='v16f32_div_ps')
            import :: v16f32
            type(v16f32),  intent(in) :: a
            type(v16f32),  intent(in) :: b
            type(v16f32) :: v16f32_div_ps
        end function v16f32_div_ps
    end interface 

    interface
       function v8f64_mask_ceil_pd(src,k,a) &
            bind(c,name='v8f64_mask_ceil_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: src
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64) :: v8f64_mask_ceil_pd
       end function v8f64_mask_ceil_pd
    end interface
  
    interface
       function v16f32_mask_ceil_ps(src,k,a) &
            bind(c,name='v16f32_mask_ceil_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: src
            integer(c_short), intent(in), value :: k
            type(v16f32),      intent(in)        :: a
            type(v16f32) :: v16f32_mask_ceil_ps
       end function v16f32_mask_ceil_ps
    end interface
   

    interface
       function v8f64_ceil_pd(a) &
            bind(c,name='v8f64_ceil_pd')
            import :: v8f64
            type(v8f64),   intent(in) :: a
            type(v8f64) :: v8f64_ceil_pd
       end function v8f64_ceil_pd
    end interface

    interface
       function v16f32_ceil_ps(a) &
            bind(c,name='v16f32_ceil_ps')
            import :: v16f32
            type(v16f32),   intent(in) :: a
            type(v16f32) :: v16f32_ceil_ps
       end function v16f32_ceil_ps
    end interface

    interface
       function v8f64_abs_pd(a) &
            bind(c,name='v8f64_abs_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64) :: v8f64_abs_pd
       end function v8f64_abs_pd
    end interface

    interface
       function v16f32_abs_ps(a) &
            bind(c,name='v16f32_abs_ps')
            import :: v16f32
            type(v16f32),  intent(in) :: a
            type(v16f32) :: v16f32_abs_ps
       end function v16f32_abs_ps
    end interface

    interface
       function v8f64_mask_abs_pd(a,k,b) &
            bind(c,name='v8f64_mask_abs_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: b
            type(v8f64) :: v8f64_mask_abs_pd
       end function v8f64_mask_abs_pd
    end interface

    interface
       function v16f32_mask_abs_ps(a,k,b) &
            bind(c,name='v16f32_mask_abs_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            integer(c_short), intent(in), value :: k
            type(v16f32),      intent(in)       :: b
            type(v16f32) :: v16f32_mask_abs_ps
       end function v16f32_mask_abs_ps
    end interface

    interface
       function v8f64_mask_add_pd(src,k,a,b) &
            bind(c,name='v8f64_mask_add_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: src
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            type(v8f64) :: v8f64_mask_add_pd
       end function v8f64_mask_add_pd
    end interface

    interface
       function v16f32_mask_add_ps(src,k,a,b) &
            bind(c,name='v16f32_mask_add_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: src
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            type(v16f32) :: v16f32_mask_add_ps
       end function v16f32_mask_add_ps
    end interface

    interface
       function v8f64_maskz_add_pd(k,a,b) &
            bind(c,name='v8f64_maskz_add_pd')
            import :: v8f64
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            type(v8f64) :: v8f64_maskz_add_pd
        end function v8f64_maskz_add_pd
    end interface

    interface
       function v16f32_maskz_add_ps(k,a,b) &
            bind(c,name='v16f32_maskz_add_ps')
            import :: v16f32
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            type(v16f32) :: v16f32_maskz_add_ps
        end function v16f32_maskz_add_ps
    end interface 

    interface
       function v8f64_add_round_pd(a,b,rounding) &
            bind(c,name='v8f64_add_round_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            integer(c_int),  intent(in), value :: rounding
            type(v8f64) :: v8f64_add_round_pd
       end function v8f64_add_round_pd
    end interface

    interface
       function v16f32_add_round_ps(a,b,rounding) &
            bind(c,name='v16f32_add_round_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            integer(c_int),   intent(in), value :: rounding
            type(v16f32) :: v16f32_add_round_ps
       end function v16f32_add_round_ps
    end interface

    interface
       function v8f64_mask_add_round_pd(src,k,a,b,rounding) &
            bind(c,name='v8f64_mask_add_round_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: src
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            integer(c_int),  intent(in), value :: rounding
            type(v8f64) :: v8f64_mask_add_round_pd
       end function v8f64_mask_add_round_pd
    end interface

    interface
       function v16f32_mask_add_round_ps(src,k,a,b,rounding) &
            bind(c,name='v16f32_mask_add_round_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: src
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            integer(c_int),  intent(in), value :: rounding
            type(v16f32) :: v16f32_mask_add_round_ps
       end function v16f32_mask_add_round_ps
    end interface

    interface
       function v8f64_maskz_add_round_pd(k,a,b,rounding) &
            bind(c,name='v8f64_maskz_add_round_pd')
            import :: v8f64
            integer(c_char),  intent(in), value :: k
            type(v8f64),      intent(in)        :: a
            type(v8f64),      intent(in)        :: b
            integer(c_int),   intent(in), value :: rounding
            type(v8f64) :: v8f64_maskz_add_round_pd
       end function v8f64_maskz_add_round_pd
    end interface

    interface
       function v16f32_maskz_add_round_ps(k,a,b,rounding) &
            bind(c,name='v16f32_maskz_add_round_ps')
            import :: v16f32
            integer(c_short),  intent(in), value :: k
            type(v16f32),      intent(in)        :: a
            type(v16f32),      intent(in)        :: b
            integer(c_int),    intent(in), value :: rounding
            type(v16f32) :: v16f32_maskz_add_round_ps
       end function v16f32_maskz_add_round_ps
    end interface


    interface
       function v8f64_mask_div_pd(src,k,a,b) &
            bind(c,name='v8f64_mask_div_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: src
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            type(v8f64) :: v8f64_mask_div_pd
       end function v8f64_mask_div_pd
    end interface

    interface
       function v16f32_mask_div_ps(src,k,a,b) &
            bind(c,name='v16f32_mask_div_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: src
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            type(v16f32) :: v16f32_mask_div_ps
       end function v16f32_mask_div_ps
    end interface

    interface
       function v8f64_maskz_div_pd(k,a,b) &
            bind(c,name='v8f64_maskz_div_pd')
            import :: v8f64
            integer(c_char),  intent(in), value :: k
            type(v8f64),      intent(in)        :: a
            type(v8f64),      intent(in)        :: b
            type(v8f64) :: v8f64_maskz_div_pd
       end function v8f64_maskz_div_pd
    end interface

    interface
       function v16f32_maskz_div_ps(k,a,b) &
            bind(c,name='v16f32_maskz_div_ps')
            import :: v16f32
            integer(c_short),   intent(in), value :: k
            type(v16f32),       intent(in)        :: a
            type(v16f32),       intent(in)        :: b
            type(v16f32) :: v16f32_maskz_div_ps
       end function v16f32_maskz_div_ps
    end interface

    interface
       function v8f64_div_round_pd(a,b,rounding)  &
            bind(c,name='v8f64_div_round_pd')
            import :: v8f64
            type(v8f64),    intent(in)        :: a
            type(v8f64),    intent(in)        :: b
            integer(c_int), intent(in), value :: rounding
            type(v8f64) :: v8f64_div_round_pd
       end function v8f64_div_round_pd
    end interface

    interface
       function v16f32_div_round_ps(a,b,rounding)  &
            bind(c,name='v16f32_div_round_ps')
            import :: v16f32
            type(v16f32),    intent(in)        :: a
            type(v16f32),    intent(in)        :: b
            integer(c_int),  intent(in), value :: rounding
            type(v16f32) :: v16f32_div_round_ps
       end function v16f32_div_round_ps
    end interface

    interface
       function v8f64_mask_div_round_pd(src,k,a,b,rounding) &
            bind(c,name='v8f64_mask_div_round_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: src
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            integer(c_int),  intent(in), value :: rounding
            type(v8f64) :: v8f64_mask_div_round_pd
       end function v8f64_mask_div_round_pd
    end interface

    interface
       function v16f32_mask_div_round_ps(src,k,a,b,rounding) &
            bind(c,name='v16f32_mask_div_round_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: src
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            integer(c_int),   intent(in), value :: rounding
            type(v16f32) :: v16f32_mask_div_round_ps
       end function v16f32_mask_div_round_ps
    end interface

    interface
       function v8f64_maskz_div_round_pd(k,a,b,rounding) &
            bind(c,name='v8f64_maskz_div_round_pd')
            import :: v8f64
            integer(c_char),  intent(in), value :: k
            type(v8f64),      intent(in)        :: a
            type(v8f64),      intent(in)        :: b
            integer(c_int),   intent(in), value :: rounding
            type(v8f64) :: v8f64_maskz_div_round_pd
       end function v8f64_maskz_div_round_pd
    end interface

    interface
       function v16f32_maskz_div_round_ps(k,a,b,rounding) &
            bind(c,name='v16f32_maskz_div_round_ps')
            import :: v16f32
            integer(c_short),  intent(in), value :: k
            type(v16f32),      intent(in)        :: a
            type(v16f32),      intent(in)        :: b
            integer(c_int),   intent(in), value :: rounding
            type(v16f32) :: v16f32_maskz_div_round_ps
       end function v16f32_maskz_div_round_ps
    end interface


    interface
       function v8f64_fmadd_pd(a,b,c) &
            bind(c,name='v8f64_fmadd_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64),  intent(in) :: c
            type(v8f64) :: v8f64_fmadd_pd
       end function v8f64_fmadd_pd
    end interface

    interface
       function v16f32_fmadd_ps(a,b,c) &
            bind(c,name='v16f32_fmadd_ps')
            import :: v16f32
            type(v16f32),  intent(in) :: a
            type(v16f32),  intent(in) :: b
            type(v16f32),  intent(in) :: c
            type(v16f32) :: v16f32_fmadd_ps
       end function v16f32_fmadd_ps
    end interface

    interface
       function v8f64_mask_fmadd_pd(a,k,b,c) &
            bind(c,name='v8f64_mask_fmadd_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: b
            type(v8f64),     intent(in)        :: c
            type(v8f64) :: v8f64_mask_fmadd_pd
       end function v8f64_mask_fmadd_pd
    end interface

    interface
       function v16f32_mask_fmadd_ps(a,k,b,c) &
            bind(c,name='v16f32_mask_fmadd_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: b
            type(v16f32),     intent(in)        :: c
            type(v16f32) :: v16f32_mask_fmadd_ps
       end function v16f32_mask_fmadd_ps
    end interface


    interface
       function v8f64_mask3_fmadd_pd(a,b,c,k) &
            bind(c,name='v8f64_mask3_fmadd_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            type(v8f64),     intent(in)        :: c
            integer(c_char), intent(in), value :: k
            type(v8f64) :: v8f64_mask3_fmadd_pd
       end function v8f64_mask3_fmadd_pd
    end interface

    interface
       function v16f32_mask3_fmadd_ps(a,b,c,k) &
            bind(c,name='v16f32_mask3_fmadd_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            type(v16f32),     intent(in)        :: c
            integer(c_short), intent(in), value :: k
            type(v16f32) :: v16f32_mask3_fmadd_ps
       end function v16f32_mask3_fmadd_ps
    end interface

    interface
       function v8f64_maskz_fmadd_pd(k,a,b,c) &
            bind(c,name='v8f64_maskz_fmadd_pd')
            import :: v8f64
            integer(c_char),  intent(in), value :: k
            type(v8f64),      intent(in)        :: a
            type(v8f64),      intent(in)        :: b
            type(v8f64),      intent(in)        :: c
            type(v8f64) :: v8f64_maskz_fmadd_pd
       end function v8f64_maskz_fmadd_pd
    end interface

    interface
       function v16f32_maskz_fmadd_ps(k,a,b,c) &
            bind(c,name='v16f32_maskz_fmadd_ps')
            import :: v16f32
            integer(c_short),  intent(in), value :: k
            type(v16f32),      intent(in)        :: a
            type(v16f32),      intent(in)        :: b
            type(v16f32),      intent(in)        :: c
            type(v16f32) :: v16f32_maskz_fmadd_ps
       end function v16f32_maskz_fmadd_ps
    end interface

    interface
       function v8f64_fmadd_round_pd(a,b,c,rounding) &
            bind(c,name='v8f64_fmadd_round_pd')
            import :: v8f64
            type(v8f64),    intent(in)        :: a
            type(v8f64),    intent(in)        :: b
            type(v8f64),    intent(in)        :: c
            integer(c_int), intent(in), value :: rounding
            type(v8f64) :: v8f64_fmadd_round_pd
       end function v8f64_fmadd_round_pd
    end interface

    interface
       function v16f32_fmadd_round_ps(a,b,c,rounding) &
            bind(c,name='v16f32_fmadd_round_ps')
            import :: v16f32
            type(v16f32),    intent(in)        :: a
            type(v16f32),    intent(in)        :: b
            type(v16f32),    intent(in)        :: c
            integer(c_int),  intent(in), value :: rounding
            type(v16f32) :: v16f32_fmadd_round_ps
       end function v16f32_fmadd_round_ps
    end interface


    interface
       function v8f64_mask_fmadd_round_pd(a,k,b,c,rounding) &
            bind(c,name='v8f64_mask_fmadd_round_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            integer(c_char), intent(in), value :: k
            type(v8f64),     intent(in)        :: b
            type(v8f64),     intent(in)        :: c
            integer(c_int),  intent(in), value :: rounding
            type(v8f64) :: v8f64_mask_fmadd_round_pd
       end function v8f64_mask_fmadd_round_pd
    end interface

    interface
       function v16f32_mask_fmadd_round_ps(a,k,b,c,rounding) &
            bind(c,name='v16f32_mask_fmadd_round_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            integer(c_short), intent(in), value :: k
            type(v16f32),     intent(in)        :: b
            type(v16f32),     intent(in)        :: c
            integer(c_int),   intent(in), value :: rounding
            type(v16f32) :: v16f32_mask_fmadd_round_ps
       end function v16f32_mask_fmadd_round_ps
    end interface

    interface
       function v8f64_mask3_fmadd_round_pd(a,b,c,k,rounding) &
            bind(c,name='v8f64_mask3_fmadd_round_pd')
            import :: v8f64
            type(v8f64),     intent(in)        :: a
            type(v8f64),     intent(in)        :: b
            type(v8f64),     intent(in)        :: c
            integer(c_char), intent(in), value :: k
            integer(c_int),  intent(in), value :: rounding
            type(v8f64) :: v8f64_mask3_fmadd_round_pd
       end function v8f64_mask3_fmadd_round_pd
    end interface

    interface
       function v16f32_mask3_fmadd_round_ps(a,b,c,k,rounding) &
            bind(c,name='v16f32_mask3_fmadd_round_ps')
            import :: v16f32
            type(v16f32),     intent(in)        :: a
            type(v16f32),     intent(in)        :: b
            type(v16f32),     intent(in)        :: c
            integer(c_short), intent(in), value :: k
            integer(c_int),   intent(in), value :: rounding
            type(v16f32) :: v16f32_mask3_fmadd_round_ps
       end function v16f32_mask3_fmadd_round_ps
    end interface

    interface
       function v8f64_maskz_fmadd_round_pd(k,a,b,c,rounding) &
            bind(c,name='v8f64_maskz_fmadd_round_pd')
            import :: v8f64
            integer(c_char),  intent(in), value :: k
            type(v8f64),      intent(in)        :: a
            type(v8f64),      intent(in)        :: b
            type(v8f64),      intent(in)        :: c
            integer(c_int),   intent(in), value :: rounding
            type(v8f64) :: v8f64_maskz_fmadd_round_pd
        end function v8f64_maskz_fmadd_round_pd
     end interface

     interface
       function v16f32_maskz_fmadd_round_ps(k,a,b,c,rounding) &
            bind(c,name='v16f32_maskz_fmadd_round_ps')
            import :: v16f32
            integer(c_short),  intent(in), value :: k
            type(v16f32),      intent(in)        :: a
            type(v16f32),      intent(in)        :: b
            type(v16f32),      intent(in)        :: c
            integer(c_int),    intent(in), value :: rounding
            type(v16f32) :: v16f32_maskz_fmadd_round_ps
        end function v16f32_maskz_fmadd_round_ps
     end interface


     interface
        function v8f64_fmaddsub_pd(a,b,c) &
             bind(c,name='v8f64_fmaddsub_pd')
             import :: v8f64
             type(v8f64),  intent(in) :: a
             type(v8f64),  intent(in) :: b
             type(v8f64),  intent(in) :: c
             type(v8f64) :: v8f64_fmaddsub_pd
        end function v8f64_fmaddsub_pd
     end interface

     interface
        function v16f32_fmaddsub_ps(a,b,c) &
             bind(c,name='v16f32_fmaddsub_ps')
             import :: v16f32
             type(v16f32),  intent(in) :: a
             type(v16f32),  intent(in) :: b
             type(v16f32),  intent(in) :: c
             type(v16f32) :: v16f32_fmaddsub_ps
        end function v16f32_fmaddsub_ps
     end interface

     interface
        function v8f64_mask_fmaddsub_pd(a,k,b,c) &
             bind(c,name='v8f64_mask_fmaddsub_pd')
             import :: v8f64
             type(v8f64),     intent(in)        :: a
             integer(c_char), intent(in), value :: k
             type(v8f64),     intent(in)        :: b
             type(v8f64),     intent(in)        :: c
             type(v8f64) :: v8f64_mask_fmaddsub_pd
        end function v8f64_mask_fmaddsub_pd
     end interface

     interface
        function v16f32_mask_fmaddsub_ps(a,k,b,c) &
             bind(c,name='v8f64_mask_fmaddsub_ps')
             import :: v16f32
             type(v16f32),     intent(in)        :: a
             integer(c_short), intent(in), value :: k
             type(v16f32),     intent(in)        :: b
             type(v16f32),     intent(in)        :: c
             type(v16f32) :: v16f32_mask_fmaddsub_ps
        end function v16f32_mask_fmaddsub_ps
     end interface

     interface
        function v8f64_mask3_fmaddsub_pd(a,b,c,k) &
             bind(c,name='v8f64_mask3_fmaddsub_pd')
             import :: v8f64
             type(v8f64),     intent(in)        :: a
             type(v8f64),     intent(in)        :: b
             type(v8f64),     intent(in)        :: c
             integer(c_char), intent(in), value :: k
             type(v8f64) :: v8f64_mask3_fmaddsub_pd
         end function v8f64_mask3_fmaddsub_pd
      end interface

      interface
        function v16f32_mask3_fmaddsub_ps(a,b,c,k) &
             bind(c,name='v16f32_mask3_fmaddsub_ps')
             import :: v16f32
             type(v16f32),     intent(in)        :: a
             type(v16f32),     intent(in)        :: b
             type(v16f32),     intent(in)        :: c
             integer(c_short), intent(in), value :: k
             type(v16f32) :: v16f32_mask3_fmaddsub_ps
         end function v16f32_mask3_fmaddsub_ps
      end interface


      interface
         function v8f64_maskz_fmaddsub_pd(k,a,b,c) &
              bind(c,name='v8f64_maskz_fmaddsub_pd')
              import :: v8f64
              integer(c_char), intent(in), value :: k
              type(v8f64),     intent(in)        :: a
              type(v8f64),     intent(in)        :: b
              type(v8f64),     intent(in)        :: c
              type(v8f64) :: v8f64_maskz_fmaddsub_pd
         end function v8f64_maskz_fmaddsub_pd
      end interface

      interface
         function v16f32_maskz_fmaddsub_ps(k,a,b,c) &
              bind(c,name='v16f32_maskz_fmaddsub_ps')
              import :: v16f32
              integer(c_short), intent(in), value :: k
              type(v16f32),     intent(in)        :: a
              type(v16f32),     intent(in)        :: b
              type(v16f32),     intent(in)        :: c
              type(v16f32) :: v16f32_maskz_fmaddsub_ps
         end function v16f32_maskz_fmaddsub_ps
      end interface


      interface
         function v8f64_fmaddsub_round_pd(a,b,c,rounding) &
              bind(c,name='v8f64_fmaddsub_round_pd')
              import :: v8f64
              type(v8f64),    intent(in)        :: a
              type(v8f64),    intent(in)        :: b
              type(v8f64),    intent(in)        :: c
              integer(c_int), intent(in), value :: rounding
              type(v8f64) :: v8f64_fmaddsub_round_pd
         end function v8f64_fmaddsub_round_pd
      end interface

      interface
         function v16f32_fmaddsub_round_ps(a,b,c,rounding) &
              bind(c,name='v16f32_fmaddsub_round_ps')
              import :: v16f32
              type(v16f32),    intent(in)        :: a
              type(v16f32),    intent(in)        :: b
              type(v16f32),    intent(in)        :: c
              integer(c_int),  intent(in), value :: rounding
              type(v16f32) :: v16f32_fmaddsub_round_ps
         end function v16f32_fmaddsub_round_ps
      end interface


      interface
         function v8f64_mask_fmaddsub_round_pd(a,k,b,c,rounding) &
              bind(c,name='v8f64_mask_fmaddsub_pd')
              import :: v8f64
              type(v8f64),     intent(in)           :: a
              integer(c_char), intent(in), value    :: k
              type(v8f64),     intent(in)           :: b
              type(v8f64),     intent(in)           :: c
              integer(c_int),  intent(in), value    :: rounding
              type(v8f64) :: v8f64_mask_fmaddsub_round_pd
         end function v8f64_mask_fmaddsub_round_pd
      end interface

      interface
         function v16f32_mask_fmaddsub_round_ps(a,k,b,c,rounding) &
              bind(c,name='v16f32_mask_fmaddsub_ps')
              import :: v16f32
              type(v16f32),     intent(in)           :: a
              integer(c_short), intent(in), value    :: k
              type(v16f32),     intent(in)           :: b
              type(v16f32),     intent(in)           :: c
              integer(c_int),   intent(in), value    :: rounding
              type(v16f32) :: v16f32_mask_fmaddsub_round_ps
         end function v16f32_mask_fmaddsub_round_ps
      end interface

      interface
         function v8f64_mask3_fmaddsub_round_pd(a,b,c,k,rounding) &
              bind(c,name='v8f64_mask3_fmaddsub_round_pd')
              import :: v8f64
              type(v8f64),     intent(in)  :: a
              type(v8f64),     intent(in)  :: b
              type(v8f64),     intent(in)  :: c
              integer(c_char), intent(in), value :: k
              integer(c_int),  intent(in), value :: rounding
              type(v8f64) :: v8f64_mask3_fmaddsub_round_pd
         end function v8f64_mask3_fmaddsub_round_pd
      end interface

      interface
         function v16f32_mask3_fmaddsub_round_ps(a,b,c,k,rounding) &
              bind(c,name='v16f32_mask3_fmaddsub_round_ps')
              import :: v16f32
              type(v16f32),     intent(in)  :: a
              type(v16f32),     intent(in)  :: b
              type(v16f32),     intent(in)  :: c
              integer(c_short), intent(in), value :: k
              integer(c_int),   intent(in), value :: rounding
              type(v16f32) :: v16f32_mask3_fmaddsub_round_ps
         end function v16f32_mask3_fmaddsub_round_ps
      end interface

      interface
         function v8f64_maskz_fmaddsub_round_pd(k,a,b,c,rounding) &
              bind(c,name='v8f64_maskz_fmaddsub_round_pd')
              import :: v8f64
              integer(c_int),   intent(in), value :: k
              type(v8f64),      intent(in)        :: a
              type(v8f64),      intent(in)        :: b
              type(v8f64),      intent(in)        :: c
              integer(c_char),  intent(in), value :: rounding
              type(v8f64) :: v8f64_maskz_fmaddsub_round_pd
          end function v8f64_maskz_fmaddsub_round_pd
       end interface

       interface
         function v16f32_maskz_fmaddsub_round_ps(k,a,b,c,rounding) &
              bind(c,name='v16f32_maskz_fmaddsub_round_ps')
              import :: v16f32
              integer(c_int),    intent(in), value :: k
              type(v16f32),      intent(in)        :: a
              type(v16f32),      intent(in)        :: b
              type(v16f32),      intent(in)        :: c
              integer(c_short),  intent(in), value :: rounding
              type(v16f32) :: v16f32_maskz_fmaddsub_round_ps
          end function v16f32_maskz_fmaddsub_round_ps
       end interface


       interface
          function v8f64_fmsub_pd(a,b,c) &
               bind(c,name='v8f64_fmsub_pd')
               import :: v8f64
               type(v8f64),    intent(in) :: a
               type(v8f64),    intent(in) :: b
               type(v8f64),    intent(in) :: c
               type(v8f64) :: v8f64_fmsub_pd
          end function v8f64_fmsub_pd
       end interface

       interface
          function v16f32_fmsub_ps(a,b,c) &
               bind(c,name='v16f32_fmsub_ps')
               import :: v16f32
               type(v16f32),    intent(in) :: a
               type(v16f32),    intent(in) :: b
               type(v16f32),    intent(in) :: c
               type(v16f32) :: v16f32_fmsub_ps
          end function v16f32_fmsub_ps
       end interface


       interface
          function v8f64_mask_fmsub_pd(a,k,b,c) &
               bind(c,name='v8f64_mask_fmsub_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               integer(c_char), intent(in), value :: k
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               type(v8f64) :: v8f64_mask_fmsub_pd
           end function v8f64_mask_fmsub_pd
       end interface

       interface
          function v16f32_mask_fmsub_ps(a,k,b,c) &
               bind(c,name='v16f32_mask_fmsub_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               integer(c_short), intent(in), value :: k
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               type(v16f32) :: v16f32_mask_fmsub_ps
           end function v16f32_mask_fmsub_ps
       end interface 

       interface
          function v8f64_mask3_fmsub_pd(a,b,c,k) &
               bind(c,name='v8f64_mask3_fmsub_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_char), intent(in), value :: k
               type(v8f64) :: v8f64_mask3_fmsub_pd
          end function v8f64_mask3_fmsub_pd
       end interface

       interface
          function v16f32_mask3_fmsub_ps(a,b,c,k) &
               bind(c,name='v16f32_mask3_fmsub_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_short), intent(in), value :: k
               type(v16f32) :: v16f32_mask3_fmsub_ps
          end function v16f32_mask3_fmsub_ps
       end interface

       interface
          function v8f64_maskz_fmsub_pd(a,b,c,k) &
               bind(c,name='v8f64_maskz_fmsub_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_char), intent(in), value :: k
               type(v8f64) :: v8f64_maskz_fmsub_pd
          end function v8f64_maskz_fmsub_pd
       end interface

       interface
          function v16f32_maskz_fmsub_ps(a,b,c,k) &
               bind(c,name='v16f32_maskz_fmsub_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_short), intent(in), value :: k
               type(v16f32) :: v16f32_maskz_fmsub_ps
          end function v16f32_maskz_fmsub_ps
       end interface

       interface
          function v8f64_fmsubadd_pd(a,b,c) &
               bind(c,name='v8f64_fmsubadd_pd')
               import :: v8f64
               type(v8f64),     intent(in) :: a
               type(v8f64),     intent(in) :: b
               type(v8f64),     intent(in) :: c
               type(v8f64) :: v8f64_fmsubadd_pd
           end function v8f64_fmsubadd_pd
       end interface

       interface
          function v16f32_fmsubadd_ps(a,b,c) &
               bind(c,name='v16f32_fmsubadd_ps')
               import :: v16f32
               type(v16f32),     intent(in) :: a
               type(v16f32),     intent(in) :: b
               type(v16f32),     intent(in) :: c
               type(v16f32) :: v16f32_fmsubadd_ps
           end function v16f32_fmsubadd_ps
       end interface
 

       interface
          function v8f64_mask_fmsubadd_pd(a,k,b,c) &
               bind(c,name='v8f64_mask_fmsubadd_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               integer(c_char), intent(in), value :: k
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               type(v8f64) :: v8f64_mask_fmsubadd_pd
          end function v8f64_mask_fmsubadd_pd
       end interface

       interface
          function v16f32_mask_fmsubadd_ps(a,k,b,c) &
               bind(c,name='v16f32_mask_fmsubadd_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               integer(c_short), intent(in), value :: k
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               type(v16f32) :: v16f32_mask_fmsubadd_ps
          end function v16f32_mask_fmsubadd_ps
       end interface

       interface
          function v8f64_mask3_fmsubadd_pd(a,b,c,k) &
               bind(c,name='v8f64_mask3_fmsubadd_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_char), intent(in), value :: k
               type(v8f64) :: v8f64_mask3_fmsubadd_pd
          end function v8f64_mask3_fmsubadd_pd
       end interface

       interface
          function v16f32_mask3_fmsubadd_ps(a,b,c,k) &
               bind(c,name='v16f32_mask3_fmsubadd_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_short), intent(in), value :: k
               type(v16f32) :: v16f32_mask3_fmsubadd_ps
          end function v16f32_mask3_fmsubadd_ps
       end interface

       interface
          function v8f64_maskz_fmsubadd_pd(k,a,b,c) &
               bind(c,name='v8f64_maskz_fmsubadd_pd')
               import :: v8f64
               integer(c_char),  intent(in), value :: k
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               type(v8f64) :: v8f64_maskz_fmsubadd_pd
           end function v8f64_maskz_fmsubadd_pd
       end interface

       interface
          function v16f32_maskz_fmsubadd_ps(k,a,b,c) &
               bind(c,name='v16f32_maskz_fmsubadd_ps')
               import :: v16f32
               integer(c_short),  intent(in), value :: k
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               type(v16f32) :: v16f32_maskz_fmsubadd_ps
           end function v16f32_maskz_fmsubadd_ps
       end interface 

       interface
          function v8f64_fmsubadd_round_pd(a,b,c,rounding) &
               bind(c,name='v8f64_fmsubadd_round_pd')
               import :: v8f64
               type(v8f64),     intent(in)  :: a
               type(v8f64),     intent(in)  :: b
               type(v8f64),     intent(in)  :: c
               integer(c_int),  intent(in)  :: rounding
               type(v8f64) :: v8f64_fmsubadd_round_pd
          end function v8f64_fmsubadd_round_pd
       end interface

       interface
          function v16f32_fmsubadd_round_ps(a,b,c,rounding) &
               bind(c,name='v16f32_fmsubadd_round_ps')
               import :: v16f32
               type(v16f32),     intent(in)  :: a
               type(v16f32),     intent(in)  :: b
               type(v16f32),     intent(in)  :: c
               integer(c_int),  intent(in)  :: rounding
               type(v16f32) :: v16f32_fmsubadd_round_ps
          end function v16f32_fmsubadd_round_ps
       end interface

       interface
          function v8f64_mask_fmsubadd_round_pd(a,k,b,c,rounding) &
               bind(c,name='v8f64_mask_fmsubadd_round_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               integer(c_char), intent(in), value :: k
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_int),  intent(in), value :: rounding
               type(v8f64) :: v8f64_mask_fmsubadd_round_pd
          end function v8f64_mask_fmsubadd_round_pd
       end interface

         interface
          function v16f32_mask_fmsubadd_round_ps(a,k,b,c,rounding) &
               bind(c,name='v16f32_mask_fmsubadd_round_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               integer(c_short), intent(in), value :: k
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_int),   intent(in), value :: rounding
               type(v16f32) :: v16f32_mask_fmsubadd_round_ps
          end function v16f32_mask_fmsubadd_round_ps
       end interface


       interface
          function v8f64_mask3_fmsubadd_round_pd(a,b,c,k,rounding) &
               bind(c,name='v8f64_mask3_fmsubadd_round_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_char), intent(in), value :: k
               integer(c_int),  intent(in), value :: rounding
               type(v8f64) :: v8f64_mask3_fmsubadd_round_pd
          end function v8f64_mask3_fmsubadd_round_pd
       end interface

       interface
          function v16f32_mask3_fmsubadd_round_ps(a,b,c,k,rounding) &
               bind(c,name='v16f32_mask3_fmsubadd_round_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_short), intent(in), value :: k
               integer(c_int),   intent(in), value :: rounding
               type(v16f32) :: v16f32_mask3_fmsubadd_round_ps
          end function v16f32_mask3_fmsubadd_round_ps
       end interface


       interface
          function v8f64_maskz_fmsubadd_round_pd(k,a,b,c,rounding) &
               bind(c,name='v8f64_maskz_fmsubadd_round_pd')
               import :: v8f64
               integer(c_char),  intent(in), value :: k
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               integer(c_int),   intent(in), value :: rounding
               type(v8f64) :: v8f64_maskz_fmsubadd_round_pd
          end function v8f64_maskz_fmsubadd_round_pd
       end interface

       interface
          function v16f32_maskz_fmsubadd_round_ps(k,a,b,c,rounding) &
               bind(c,name='v16f32_maskz_fmsubadd_round_ps')
               import :: v16f32
               integer(c_short),  intent(in), value :: k
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               integer(c_int),    intent(in), value :: rounding
               type(v16f32) :: v16f32_maskz_fmsubadd_round_ps
          end function v16f32_maskz_fmsubadd_round_ps
       end interface

       interface
          function v8f64_fnmadd_pd(a,b,c) &
               bind(c,name='v8f64_fnmadd_pd')
               import :: v8f64
               type(v8f64),  intent(in) :: a
               type(v8f64),  intent(in) :: b
               type(v8f64),  intent(in) :: c
               type(v8f64) :: v8f64_fnmadd_pd
          end function v8f64_fnmadd_pd
       end interface

       interface
          function v16f32_fnmadd_ps(a,b,c) &
               bind(c,name='v16f32_fnmadd_ps')
               import :: v16f32
               type(v16f32),  intent(in) :: a
               type(v16f32),  intent(in) :: b
               type(v16f32),  intent(in) :: c
               type(v16f32) :: v16f32_fnmadd_ps
          end function v16f32_fnmadd_ps
       end interface


       interface
          function v8f64_mask_fnmadd_pd(a,k,b,c) &
               bind(c,name='v8f64_mask_fnmadd_pd')
               import :: v8f64
               type(v8f64),      intent(in)        :: a
               integer(c_char),  intent(in), value :: k
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               type(v8f64) :: v8f64_mask_fnmadd_pd
          end function v8f64_mask_fnmadd_pd
       end interface

        interface
          function v16f32_mask_fnmadd_ps(a,k,b,c) &
               bind(c,name='v16f32_mask_fnmadd_ps')
               import :: v16f32
               type(v16f32),      intent(in)        :: a
               integer(c_short),  intent(in), value :: k
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               type(v16f32) :: v16f32_mask_fnmadd_ps
          end function v16f32_mask_fnmadd_ps
       end interface

       interface
          function v8f64_mask3_fnmadd_pd(a,b,c,k) &
               bind(c,name='v8f64_mask3_fnmadd_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_char), intent(in), value :: k
               type(v8f64) :: v8f64_mask3_fnmadd_pd
           end function v8f64_mask3_fnmadd_pd
        end interface

          interface
          function v16f32_mask3_fnmadd_ps(a,b,c,k) &
               bind(c,name='v16f32_mask3_fnmadd_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_short), intent(in), value :: k
               type(v16f32) :: v16f32_mask3_fnmadd_ps
           end function v16f32_mask3_fnmadd_ps
        end interface

       interface
          function v8f64_maskz_fnmadd_pd(k,a,b,c) &
               bind(c,name='v8f64_maskz_fnmadd_pd')
               import :: v8f64
               integer(c_char),  intent(in), value :: k
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               type(v8f64) :: v8f64_maskz_fnmadd_pd
          end function v8f64_maskz_fnmadd_pd
       end interface

       interface
          function v16f32_maskz_fnmadd_ps(k,a,b,c) &
               bind(c,name='v16f32_maskz_fnmadd_ps')
               import :: v16f32
               integer(c_short),  intent(in), value :: k
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               type(v16f32) :: v16f32_maskz_fnmadd_ps
          end function v16f32_maskz_fnmadd_ps
       end interface

       interface
          function v8f64_fnmadd_round_pd(a,b,c,rounding) &
               bind(c,name='v8f64_fnmadd_round_pd')
               import :: v8f64
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               integer(c_int),   intent(in), value :: rounding
               type(v8f64) :: v8f64_fnmadd_round_pd
          end function v8f64_fnmadd_round_pd
       end interface

       interface
          function v16f32_fnmadd_round_ps(a,b,c,rounding) &
               bind(c,name='v16f32_fnmadd_round_ps')
               import :: v16f32
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               integer(c_int),    intent(in), value :: rounding
               type(v16f32) :: v16f32_fnmadd_round_ps
          end function v16f32_fnmadd_round_ps
       end interface

       interface
          function v8f64_mask_fnmadd_round_pd(a,k,b,c,rounding) &
               bind(c,name='v8f64_mask_fnmad_round_pd')
               import :: v8f64
               type(v8f64),     intent(in)        :: a
               integer(c_char), intent(in), value :: k
               type(v8f64),     intent(in)        :: b
               type(v8f64),     intent(in)        :: c
               integer(c_int),  intent(in), value :: rounding
               type(v8f64) :: v8f64_mask_fnmadd_round_pd
          end function v8f64_mask_fnmadd_round_pd
       end interface

       interface
          function v16f32_mask_fnmadd_round_ps(a,k,b,c,rounding) &
               bind(c,name='v16f32_mask_fnmad_round_ps')
               import :: v16f32
               type(v16f32),     intent(in)        :: a
               integer(c_short), intent(in), value :: k
               type(v16f32),     intent(in)        :: b
               type(v16f32),     intent(in)        :: c
               integer(c_int),   intent(in), value :: rounding
               type(v16f32) :: v16f32_mask_fnmadd_round_ps
          end function v16f32_mask_fnmadd_round_ps
       end interface

       interface
          function v8f64_mask3_fnmadd_round_pd(a,b,c,k,rounding) &
               bind(c,name='v8f64_mask3_fnmadd_round_pd')
               import :: v8f64
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               integer(c_char),  intent(in), value :: k
               integer(c_int),   intent(in), value :: rounding
               type(v8f64) :: v8f64_mask3_fnmadd_round_pd
          end function v8f64_mask3_fnmadd_round_pd
       end interface

         interface
          function v16f32_mask3_fnmadd_round_ps(a,b,c,k,rounding) &
               bind(c,name='v16f32_mask3_fnmadd_round_ps')
               import :: v16f32
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               integer(c_short),  intent(in), value :: k
               integer(c_int),    intent(in), value :: rounding
               type(v16f32) :: v16f32_mask3_fnmadd_round_ps
          end function v16f32_mask3_fnmadd_round_ps
       end interface


       interface
          function v8f64_maskz_fnmadd_round_pd(k,a,b,c,rounding) &
               bind(c,name='v8f64_maskz_fnmadd_round_pd')
               import :: v8f64
               integer(c_char),  intent(in), value :: k
               type(v8f64),      intent(in)        :: a
               type(v8f64),      intent(in)        :: b
               type(v8f64),      intent(in)        :: c
               integer(c_int),   intent(in), value :: rounding
               type(v8f64) :: v8f64_maskz_fnmadd_round_pd
           end function v8f64_maskz_fnmadd_round_pd
        end interface

        interface
          function v16f32_maskz_fnmadd_round_ps(k,a,b,c,rounding) &
               bind(c,name='v16f32_maskz_fnmadd_round_ps')
               import :: v16f32
               integer(c_short),  intent(in), value :: k
               type(v16f32),      intent(in)        :: a
               type(v16f32),      intent(in)        :: b
               type(v16f32),      intent(in)        :: c
               integer(c_int),    intent(in), value :: rounding
               type(v16f32) :: v16f32_maskz_fnmadd_round_ps
           end function v16f32_maskz_fnmadd_round_ps
        end interface

        interface
           function v8f64_fnmsub_pd(a,b,c) &
                bind(c,name='v8f64_fnmsub_pd')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                type(v8f64),      intent(in)        :: c
                type(v8f64) :: v8f64_fnmsub_pd
           end function v8f64_fnmsub_pd
        end interface

        interface
           function v16f32_fnmsub_ps(a,b,c) &
                bind(c,name='v16f32_fnmsub_ps')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                type(v16f32),      intent(in)        :: c
                type(v16f32) :: v16f32_fnmsub_ps
           end function v16f32_fnmsub_ps
        end interface

        interface
           function v8f64_mask_fnmsub_pd(a,k,b,c) &
                bind(c,name='v8f64_mask_fnmsub_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: a
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: b
                type(v8f64),     intent(in)        :: c
                type(v8f64) :: v8f64_mask_fnmsub_pd
            end function v8f64_mask_fnmsub_pd
        end interface

        interface
           function v16f32_mask_fnmsub_ps(a,k,b,c) &
                bind(c,name='v16f32_mask_fnmsub_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: a
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: b
                type(v16f32),     intent(in)        :: c
                type(v16f32) :: v16f32_mask_fnmsub_ps
            end function v16f32_mask_fnmsub_ps
        end interface 

        interface
           function v8f64_mask3_fnmsub_pd(a,b,c,k) &
                bind(c,name='v8f64_mask3_fnmsub_pd')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                type(v8f64),      intent(in)        :: c
                integer(c_char),  intent(in), value :: k
                type(v8f64) :: v8f64_mask3_fnmsub_pd
           end function v8f64_mask3_fnmsub_pd
        end interface

        interface
           function v16f32_mask3_fnmsub_ps(a,b,c,k) &
                bind(c,name='v16f32_mask3_fnmsub_ps')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                type(v16f32),      intent(in)        :: c
                integer(c_short),  intent(in), value :: k
                type(v16f32) :: v16f32_mask3_fnmsub_ps
           end function v16f32_mask3_fnmsub_ps
        end interface

        interface
           function v8f64_maskz_fnmsub_pd(k,a,b,c) &
                bind(c,name='v8f64_maskz_fnmsub_pd')
                import :: v8f64
                integer(c_char),  intent(in), value :: k
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                type(v8f64),      intent(in)        :: c
                type(v8f64) :: v8f64_maskz_fnmsub_pd
           end function v8f64_maskz_fnmsub_pd
        end interface

        interface
           function v16f32_maskz_fnmsub_ps(k,a,b,c) &
                bind(c,name='v16f32_maskz_fnmsub_ps')
                import :: v16f32
                integer(c_short),  intent(in), value :: k
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                type(v16f32),      intent(in)        :: c
                type(v16f32) :: v16f32_maskz_fnmsub_ps
           end function v16f32_maskz_fnmsub_ps
        end interface

        interface
           function v8f64_mask_mul_pd(src,k,a,b) &
                bind(c,name='v8f64_mask_mul_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_mask_mul_pd
           end function v8f64_mask_mul_pd
        end interface

        interface
           function v16f32_mask_mul_ps(src,k,a,b) &
                bind(c,name='v16f32_mask_mul_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_mask_mul_ps
           end function v16f32_mask_mul_ps
        end interface

        interface
           function v8f64_maskz_mul_pd(k,a,b) &
                bind(c,name='v8f64_mask_mul_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_maskz_mul_pd
           end function v8f64_maskz_mul_pd
        end interface

        interface
           function v16f32_maskz_mul_ps(k,a,b) &
                bind(c,name='v16f32_mask_mul_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_maskz_mul_ps
           end function v16f32_maskz_mul_ps
        end interface

        interface
           function v8f64_mask_mul_round_pd(src,k,a,b,rounding) &
                bind(c,name='v8f64_mask_mul_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_mul_round_pd
           end function v8f64_mask_mul_round_pd
        end interface

        interface
           function v16f32_mask_mul_round_ps(src,k,a,b,rounding) &
                bind(c,name='v16f32_mask_mul_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_mul_round_ps
           end function v16f32_mask_mul_round_ps
        end interface

        interface
           function v8f64_maskz_mul_round_pd(k,a,b,rounding) &
                bind(c,name='v8f64_maskz_mul_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_mul_round_pd
           end function v8f64_maskz_mul_round_pd
        end interface

        interface
           function v16f32_maskz_mul_round_ps(k,a,b,rounding) &
                bind(c,name='v16f32_maskz_mul_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_mul_round_ps
           end function v16f32_maskz_mul_round_ps
        end interface

        interface
           function v8f64_mask_reduce_add_pd(k,a) &
                bind(c,name='v8f64_mask_reduce_add_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                real(c_double) :: v8f64_mask_reduce_add_pd
           end function v8f64_mask_reduce_add_pd
        end interface

        interface
           function v16f32_mask_reduce_add_ps(k,a) &
                bind(c,name='v16f32_mask_reduce_add_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                real(c_float) :: v16f32_mask_reduce_add_ps
           end function v16f32_mask_reduce_add_ps
        end interface


        interface
           function v8f64_reduce_add_pd(a) &
                bind(c,name='v8f64_reduce_add_pd')
                import :: v8f64
                type(v8f64),  intent(in) :: a
                real(c_double) :: v8f64_reduce_add_pd
           end function v8f64_reduce_add_pd
        end interface

        interface
           function v16f32_reduce_add_ps(a) &
                bind(c,name='v16f32_reduce_add_ps')
                import :: v16f32
                type(v16f32),  intent(in) :: a
                real(c_float) :: v16f32_reduce_add_ps
           end function v16f32_reduce_add_ps
        end interface

        interface
           function v8f64_mask_reduce_mul_pd(k,a) &
                bind(c,name='v8f64_mask_reduce_mul_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                real(c_double) :: v8f64_mask_reduce_mul_pd
           end function v8f64_mask_reduce_mul_pd
        end interface

        interface
           function v16f32_mask_reduce_mul_ps(k,a) &
                bind(c,name='v16f32_mask_reduce_mul_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                real(c_float) :: v16f32_mask_reduce_mul_ps
           end function v16f32_mask_reduce_mul_ps
        end interface

        interface
           function v8f64_reduce_mul_pd(a) &
                bind(c,name='v8f64_reduce_mul_pd')
                import :: v8f64
                type(v8f64),  intent(in) :: a
                real(c_double) :: v8f64_reduce_mul_pd
           end function v8f64_reduce_mul_pd
        end interface

        interface
           function v16f32_reduce_mul_ps(a) &
                bind(c,name='v16f32_reduce_mul_ps')
                import :: v16f32
                type(v16f32),  intent(in) :: a
                real(c_float) :: v16f32_reduce_mul_ps
           end function v16f32_reduce_mul_ps
        end interface

        interface
           function v8f64_mask_sub_pd(src,k,a,b) &
                bind(c,name='v8f64_mask_sub_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_mask_sub_pd
           end function v8f64_mask_sub_pd
        end interface

        interface
           function v16f32_mask_sub_ps(src,k,a,b) &
                bind(c,name='v16f32_mask_sub_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_mask_sub_ps
           end function v16f32_mask_sub_ps
        end interface


        interface
           function v8f64_maskz_sub_pd(k,a,b) &
                bind(c,name='v8f64_maskz_sub_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_maskz_sub_pd
            end function v8f64_maskz_sub_pd
         end interface

        interface
           function v16f32_maskz_sub_ps(k,a,b) &
                bind(c,name='v16f32_maskz_sub_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_maskz_sub_ps
            end function v16f32_maskz_sub_ps
         end interface 

        interface
           function v8f64_cmp_pd_mask(a,b,imm8) &
                bind(c,name='v8f64_cmp_pd_mask')
                import :: v8f64
                type(v8f64),    intent(in)        :: a
                type(v8f64),    intent(in)        :: b
                integer(c_int), intent(in), value :: imm8
                integer(c_char) :: v8f64_cmp_pd_mask
           end function v8f64_cmp_pd_mask
        end interface

        interface
           function v16f32_cmp_ps_mask(a,b,imm8) &
                bind(c,name='v16f32_cmp_ps_mask')
                import :: v16f32
                type(v16f32),    intent(in)        :: a
                type(v16f32),    intent(in)        :: b
                integer(c_int), intent(in), value :: imm8
                integer(c_short) :: v16f32_cmp_ps_mask
           end function v16f32_cmp_ps_mask
        end interface

        interface
           function v8f64_mask_cmp_pd_mask(k1,a,b,imm8) &
                bind(c,name='v8f64_mask_cmp_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_char) :: v8f64_mask_cmp_pd_mask
           end function v8f64_mask_cmp_pd_mask
        end interface

        interface
           function v16f32_mask_cmp_ps_mask(k1,a,b,imm8) &
                bind(c,name='v16f32_mask_cmp_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_short) :: v16f32_mask_cmp_ps_mask
           end function v16f32_mask_cmp_ps_mask
        end interface


        interface
           function v8f64_cmp_round_pd_mask(a,b,imm8,sae) &
                bind(c,name='v8f64_cmp_round_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_int),   intent(in), value :: sae
                integer(c_char) :: v8f64_cmp_round_pd_mask
           end function v8f64_cmp_round_pd_mask
        end interface

        interface
           function v16f32_cmp_round_ps_mask(a,b,imm8,sae) &
                bind(c,name='v16f32_cmp_round_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_int),   intent(in), value :: sae
                integer(c_short) :: v16f32_cmp_round_ps_mask
           end function v16f32_cmp_round_ps_mask
        end interface

        interface
           function v8f64_mask_cmp_round_pd_mask(k1,a,b,imm8,sae) &
                bind(c,name='v8f64_mask_cmp_round_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_int),   intent(in), value :: sae
                integer(c_char) :: v8f64_mask_cmp_round_pd_mask
           end function v8f64_mask_cmp_round_pd_mask
        end interface

        interface
           function v16f32_mask_cmp_round_ps_mask(k1,a,b,imm8,sae) &
                bind(c,name='v16f32_mask_cmp_round_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_int),   intent(in), value :: imm8
                integer(c_int),   intent(in), value :: sae
                integer(c_short) :: v16f32_mask_cmp_round_ps_mask
           end function v16f32_mask_cmp_round_ps_mask
        end interface

        interface
           function v8f64_cmpeq_pd_mask(a,b) &
                bind(c,name='v8f64_cmpeq_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpeq_pd_mask
           end function v8f64_cmpeq_pd_mask
        end interface

        interface
           function v16f32_cmpeq_ps_mask(a,b) &
                bind(c,name='v16f32_cmpeq_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpeq_ps_mask
           end function v16f32_cmpeq_ps_mask
        end interface

        interface
           function v8f64_mask_cmpeq_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpeq_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpeq_pd_mask
            end function v8f64_mask_cmpeq_pd_mask
        end interface

        interface
           function v16f32_mask_cmpeq_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpeq_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpeq_ps_mask
            end function v16f32_mask_cmpeq_ps_mask
        end interface

        interface
           function v8f64_mask_cmpunord_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpunord_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpunord_pd_mask
           end function v8f64_mask_cmpunord_pd_mask
        end interface

        interface
           function v16f32_mask_cmpunord_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpunord_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpunord_ps_mask
           end function v16f32_mask_cmpunord_ps_mask
        end interface

        interface
           function v8f64_cmpunord_pd_mask(a,b) &
                bind(c,name='v8f64_cmpunord_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpunord_pd_mask
           end function v8f64_cpmunord_pd_mask
        end interface

        interface
           function v16f32_cmpunord_ps_mask(a,b) &
                bind(c,name='v16f32_cmpunord_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpunord_ps_mask
           end function v16f32_cpmunord_ps_mask
        end interface


        interface
           function v8f64_cmpord_pd_mask(a,b) &
                bind(c,name='v8f64_cmpord_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpord_pd_mask
           end function v8f64_cmpord_pd_mask
        end interface

        interface
           function v16f32_cmpord_ps_mask(a,b) &
                bind(c,name='v16f32_cmpord_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpord_ps_mask
           end function v16f32_cmpord_ps_mask
        end interface


        interface
           function v8f64_mask_cmpord_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpord_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpord_pd_mask
           end function v8f64_mask_cmpord_pd_mask
        end interface

        interface
           function v16f32_mask_cmpord_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpord_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpord_ps_mask
           end function v16f32_mask_cmpord_ps_mask
        end interface


        interface
           function v8f64_cmpnlt_pd_mask(a,b) &
                bind(c,name='v8f64_cmpnlt_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpnlt_pd_mask
           end function v8f64_cmpnlt_pd_mask
        end interface

        interface
           function v16f32_cmpnlt_ps_mask(a,b) &
                bind(c,name='v16f32_cmpnlt_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpnlt_ps_mask
           end function v16f32_cmpnlt_ps_mask
        end interface

        interface
           function v8f64_mask_cmpnlt_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpnlt_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpnlt_pd_mask
           end function v8f64_mask_cmpnlt_pd_mask
        end interface

        interface
           function v16f32_mask_cmpnlt_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpnlt_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpnlt_ps_mask
           end function v16f32_mask_cmpnlt_ps_mask
        end interface

        interface
           function v8f64_cmpnle_pd_mask(a,b) &
                bind(c,name='v8f64_mask_cmpnle_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpnle_pd_mask
           end function v8f64_cmpnle_pd_mask
        end interface

        interface
           function v16f32_cmpnle_ps_mask(a,b) &
                bind(c,name='v16f32_mask_cmpnle_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpnle_ps_mask
           end function v16f32_cmpnle_ps_mask
        end interface

        interface
           function v8f64_mask_cmpnle_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpnle_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpnle_pd_mask
           end function v8f64_mask_cmpnle_pd_mask
        end interface

       interface
           function v16f32_mask_cmpnle_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpnle_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpnle_ps_mask
           end function v16f32_mask_cmpnle_ps_mask
        end interface

        interface
           function v8f64_cmpneq_pd_mask(a,b) &
                bind(c,name='v8f64_cmpneq_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmpneq_pd_mask
           end function v8f64_cmpneq_pd_mask
        end interface

        interface
           function v16f32_cmpneq_ps_mask(a,b) &
                bind(c,name='v16f32_cmpneq_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmpneq_ps_mask
           end function v16f32_cmpneq_ps_mask
        end interface

        interface
           function v8f64_mask_cmpneq_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmpneq_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmpneq_pd_mask
           end function v8f64_mask_cmpneq_pd_mask
        end interface

        interface
           function v16f32_mask_cmpneq_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmpneq_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmpneq_ps_mask
           end function v16f32_mask_cmpneq_ps_mask
        end interface

        interface
           function v8f64_cmplt_pd_mask(a,b) &
                bind(c,name='v8f64_cmplt_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmplt_pd_mask
           end function v8f64_cmplt_pd_mask
        end interface

        interface
           function v16f32_cmplt_ps_mask(a,b) &
                bind(c,name='v16f32_cmplt_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmplt_ps_mask
           end function v16f32_cmplt_ps_mask
        end interface


        interface
           function v8f64_mask_cmplt_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmplt_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmplt_pd_mask
           end function v8f64_mask_cmplt_pd_mask
        end interface

        interface
           function v16f32_mask_cmplt_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmplt_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmplt_ps_mask
           end function v16f32_mask_cmplt_ps_mask
        end interface


        interface
           function v8f64_cmple_pd_mask(a,b) &
                bind(c,name='v8f64_cmple_pd_mask')
                import :: v8f64
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_cmple_pd_mask
           end function v8f64_cmple_pd_mask
        end interface

        interface
           function v16f32_cmple_ps_mask(a,b) &
                bind(c,name='v16f32_cmple_ps_mask')
                import :: v16f32
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_cmple_ps_mask
           end function v16f32_cmple_ps_mask
        end interface

        interface
           function v8f64_mask_cmple_pd_mask(k1,a,b) &
                bind(c,name='v8f64_mask_cmple_pd_mask')
                import :: v8f64
                integer(c_char),  intent(in), value :: k1
                type(v8f64),      intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_char) :: v8f64_mask_cmple_pd_mask
           end function v8f64_mask_cmple_pd_mask
        end interface

        interface
           function v16f32_mask_cmple_ps_mask(k1,a,b) &
                bind(c,name='v16f32_mask_cmple_ps_mask')
                import :: v16f32
                integer(c_short),  intent(in), value :: k1
                type(v16f32),      intent(in)        :: a
                type(v16f32),      intent(in)        :: b
                integer(c_short) :: v16f32_mask_cmple_ps_mask
           end function v16f32_mask_cmple_ps_mask
        end interface

        interface
           function v8f64_mask_rcp14_pd(src,k,a) &
                bind(c,name='v8f64_mask_rcp14_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_mask_rcp14_pd
           end function v8f64_mask_rcp14_pd
        end interface

        interface
           function v16f32_mask_rcp14_ps(src,k,a) &
                bind(c,name='v16f32_mask_rcp14_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32) :: v16f32_mask_rcp14_ps
           end function v16f32_mask_rcp14_ps
        end interface


        interface
           function v8f64_maskz_rcp14_pd(k,a) &
                bind(c,name='v8f64_maskz_rcp14_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_maskz_rcp14_pd
           end function v8f64_maskz_rcp14_pd
        end interface

        interface
           function v16f32_maskz_rcp14_ps(k,a) &
                bind(c,name='v16f32_maskz_rcp14_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32) :: v16f32_maskz_rcp14_ps
           end function v16f32_maskz_rcp14_ps
        end interface

        interface
           function v8f64_rcp14_pd(a) &
                bind(c,name='v8f64_rcp14_pd')
                import :: v8f64
                type(v8f64),  intent(in) :: a
                type(v8f64) :: v8f64_rcp14_pd
           end function v8f64_rcp14_pd
        end interface

        interface
           function v16f32_rcp14_ps(a) &
                bind(c,name='v16f32_rcp14_ps')
                import :: v16f32
                type(v16f32),  intent(in) :: a
                type(v16f32) :: v16f32_rcp14_ps
           end function v16f32_rcp14_ps
        end interface

        interface
           function v8f64_mask_rsqrt14_pd(src,k,a) &
                bind(c,name='v8f64_mask_rsqrt14_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_mask_rsqrt14_pd
           end function v8f64_mask_rsqrt14_pd
        end interface

        interface
           function v16f32_mask_rsqrt14_ps(src,k,a) &
                bind(c,name='v16f32_mask_rsqrt14_ps')
                import :: v16f32
                type(v16f32),      intent(in)        :: src
                integer(c_short),  intent(in), value :: k
                type(v16f32),      intent(in)        :: a
                type(v16f32) :: v16f32_mask_rsqrt14_ps
           end function v16f32_mask_rsqrt14_ps
        end interface


        interface
           function v8f64_maskz_rsqrt14_pd(k,a) &
                bind(c,name='v8f64_maskz_rsqrt14_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_maskz_rsqrt14_pd
           end function v8f64_maskz_rsqrt14_pd
        end interface

        interface
           function v16f32_maskz_rsqrt14_ps(k,a) &
                bind(c,name='v16f32_maskz_rsqrt14_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32) :: v16f32_maskz_rsqrt14_ps
           end function v16f32_maskz_rsqrt14_ps
        end interface

        interface
           function v8f64_rsqrt14_pd(a) &
                bind(c,name='v8f64_rsqrt14_pd')
                import :: v8f64
                type(v8f64),   intent(in) :: a
                type(v8f64) :: v8f64_rsqrt14_pd
           end function v8f64_rsqrt14_pd
        end interface

        interface
           function v16f32_rsqrt14_ps(a) &
                bind(c,name='v16f32_rsqrt14_ps')
                import :: v16f32
                type(v16f32),   intent(in) :: a
                type(v16f32) :: v16f32_rsqrt14_ps
           end function v16f32_rsqrt14_ps
        end interface

        interface
           function v8f64_mask_sqrt_pd(src,k,a) &
                bind(c,name='v8f64_mask_sqrt_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_mask_sqrt_pd
           end function v8f64_mask_sqrt_pd
        end interface

        interface
           function v16f32_mask_sqrt_ps(src,k,a) &
                bind(c,name='v16f32_mask_sqrt_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32) :: v16f32_mask_sqrt_ps
           end function v16f32_mask_sqrt_ps
        end interface

        interface
           function v8f64_maskz_sqrt_pd(k,a) &
                bind(c,name='v8f64_maskz_sqrt_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64) :: v8f64_maskz_sqrt_pd
           end function v8f64_maskz_sqrt_pd
        end interface

        interface
           function v16f32_maskz_sqrt_ps(k,a) &
                bind(c,name='v16f32_maskz_sqrt_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32) :: v16f32_maskz_sqrt_ps
           end function v16f32_maskz_sqrt_ps
        end interface
         
        interface
           function v8f64_mask_sqrt_round_pd(src,k,a,rounding) &
                bind(c,name='v8f64_mask_sqrt_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_sqrt_round_pd
           end function v8f64_mask_sqrt_round_pd
        end interface

        interface
           function v16f32_mask_sqrt_round_ps(src,k,a,rounding) &
                bind(c,name='v16f32_mask_sqrt_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_sqrt_round_ps
           end function v16f32_mask_sqrt_round_ps
        end interface

        interface
           function v8f64_maskz_sqrt_round_pd(k,a,rounding) &
                bind(c,name='v8f64_maskz_sqrt_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_sqrt_round_pd
           end function v8f64_maskz_sqrt_round_pd
        end interface

        interface
           function v16f32_maskz_sqrt_round_ps(k,a,rounding) &
                bind(c,name='v16f32_maskz_sqrt_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_sqrt_round_ps
           end function v16f32_maskz_sqrt_round_ps
        end interface

        interface
           function v8f64_mask_expandloadu_pd(src,k,mem_addr) &
                bind(c,name='v8f64_mask_expandloadu_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_mask_expandloadu_pd
           end function v8f64_mask_expandloadu_pd
        end interface

        interface
           function v16f32_mask_expandloadu_ps(src,k,mem_addr) &
                bind(c,name='v16f32_mask_expandloadu_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(c_ptr),      intent(in), value :: mem_addr
                type(v16f32) :: v16f32_mask_expandloadu_ps
           end function v16f32_mask_expandloadu_ps
        end interface

        interface
           function v8f64_maskz_expandloadu_pd(k,mem_addr) &
                bind(c,name='v8f64_maskz_expandloadu_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_maskz_expandloadu_pd
           end function v8f64_maskz_expandloadu_pd
        end interface

        interface
           function v16f32_maskz_expandloadu_ps(k,mem_addr) &
                bind(c,name='v16f32_maskz_expandloadu_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(c_ptr),      intent(in), value :: mem_addr
                type(v16f32) :: v16f32_maskz_expandloadu_ps
           end function v16f32_maskz_expandloadu_ps
        end interface

        interface
           function v8f64_mask_load_pd(src,k,mem_addr) &
                bind(c,name='v8f64_mask_load_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_mask_load_pd
           end function v8f64_mask_load_pd
        end interface

        interface
           function v16f32_mask_load_ps(src,k,mem_addr) &
                bind(c,name='v16f32_mask_load_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(c_ptr),      intent(in), value :: mem_addr
                type(v16f32) :: v16f32_mask_load_ps
           end function v16f32_mask_load_ps
        end interface


        interface
           function v8f64_maskz_load_pd(k,mem_addr) &
                bind(c,name='v8f64_maskz_load_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_maskz_load_pd
           end function v8f64_maskz_load_pd
        end interface

        interface
           function v16f32_maskz_load_ps(k,mem_addr) &
                bind(c,name='v16f32_maskz_load_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v16f32) :: v16f32_maskz_load_ps
           end function v16f32_maskz_load_ps
        end interface

        interface
           function v8f64_mask_loadu_pd(src,k,mem_addr) &
                bind(c,name='v8f64_mask_loadu_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_mask_loadu_pd
           end function v8f64_mask_loadu_pd
        end interface

        interface
           function v16f32_mask_loadu_ps(src,k,mem_addr) &
                bind(c,name='v16f32_mask_loadu_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(c_ptr),      intent(in), value :: mem_addr
                type(v16f32) :: v16f32_mask_loadu_ps
           end function v16f32_mask_loadu_ps
        end interface

        interface
           function v8f64_maskz_loadu_pd(k,mem_addr) &
                bind(c,name='v8f64_maskz_loadu_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(c_ptr),     intent(in), value :: mem_addr
                type(v8f64) :: v8f64_maskz_loadu_pd
           end function v8f64_maskz_loadu_pd
        end interface

        interface
           function v16f32_maskz_loadu_ps(k,mem_addr) &
                bind(c,name='v16f32_maskz_loadu_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(c_ptr),      intent(in), value :: mem_addr
                type(v16f32) :: v16f32_maskz_loadu_ps
           end function v16f32_maskz_loadu_ps
        end interface


        interface
           function v8f64_getexp_pd(a) &
                bind(c,name='v8f64_getexp_pd')
                import :: v8f64
                type(v8f64),  intent(in) :: a
                type(v8f64) :: v8f64_getexp_pd
           end function v8f64_getexp_pd
        end interface

        interface
           function v16f32_getexp_ps(a) &
                bind(c,name='v16f32_getexp_ps')
                import :: v16f32
                type(v16f32),  intent(in) :: a
                type(v16f32) :: v16f32_getexp_ps
           end function v16f32_getexp_ps
        end interface


        interface
           function v8f64_mask_getexp_pd(src,k,a) &
                bind(c,name='v8f64_mask_getexp_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in), value :: a
                type(v8f64) :: v8f64_mask_getexp_pd
           end function v8f64_mask_getexp_pd
        end interface

        interface
           function v16f32_mask_getexp_ps(src,k,a) &
                bind(c,name='v16f32_mask_getexp_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in), value :: a
                type(v16f32) :: v16f32_mask_getexp_ps
           end function v16f32_mask_getexp_ps
        end interface

        interface
           function v8f64_maskz_getexp_pd(k,a) &
                bind(c,name='v8f64_maskz_getexp_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in), value :: a
           end function v8f64_maskz_getexp_pd
        end interface

        interface
           function v16f32_maskz_getexp_ps(k,a) &
                bind(c,name='v16f32_maskz_getexp_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in), value :: a
           end function v16f32_maskz_getexp_ps
        end interface

        interface
           function v8f64_getexp_round_pd(a,rounding) &
                bind(c,name='v8f64_getexp_round_pd')
                import :: v8f64
                type(v8f64),    intent(in)        :: a
                integer(c_int), intent(in), value :: rounding
                type(v8f64) :: v8f64_getexp_round_pd
           end function v8f64_getexp_round_pd
        end interface

        interface
           function v16f32_getexp_round_ps(a,rounding) &
                bind(c,name='v16f32_getexp_round_ps')
                import :: v16f32
                type(v16f32),    intent(in)        :: a
                integer(c_int), intent(in), value :: rounding
                type(v16f32) :: v16f32_getexp_round_ps
           end function v16f32_getexp_round_ps
        end interface


        interface
           function v8f64_mask_getexp_round_pd(src,k,a,rounding) &
                bind(c,name='v8f64_mask_getexp_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_getexp_round_pd
            end function v8f64_mask_getexp_round_pd
         end interface

         interface
           function v16f32_mask_getexp_round_ps(src,k,a,rounding) &
                bind(c,name='v16f32_mask_getexp_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),   intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_getexp_round_ps
            end function v16f32_mask_getexp_round_ps
         end interface


        interface
           function v8f64_maskz_getexp_round_pd(k,a,rounding) &
                bind(c,name='v8f64_maskz_getexp_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_getexp_round_pd
           end function v8f64_maskz_getexp_round_pd
        end interface

        interface
           function v16f32_maskz_getexp_round_ps(k,a,rounding) &
                bind(c,name='v16f32_maskz_getexp_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_getexp_round_ps
           end function v16f32_maskz_getexp_round_ps
        end interface


        interface
           function v8f64_getmant_pd(a,interv,sc) &
                bind(c,name='v8f64_getmant_pd')
                import :: v8f64
                type(v8f64),    intent(in)        :: a
                integer(c_int), intent(in), value :: interv
                integer(c_int), intent(in), value :: sc
                type(v8f64) :: v8f64_getmant_pd
           end function v8f64_getmant_pd
        end interface

        interface
           function v16f32_getmant_ps(a,interv,sc) &
                bind(c,name='v16f32_getmant_ps')
                import :: v16f32
                type(v16f32),    intent(in)        :: a
                integer(c_int), intent(in), value :: interv
                integer(c_int), intent(in), value :: sc
                type(v16f32) :: v16f32_getmant_ps
           end function v16f32_getmant_ps
        end interface

        interface
           function v8f64_mask_getmant_pd(src,k,a,interv,sc) &
                bind(c,name='v8f64_mask_getmant_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int), intent(in),  value :: interv
                integer(c_int), intent(in),  value :: sc
                type(v8f64) :: v8f64_mask_getmant_pd
           end function v8f64_mask_getmant_pd
        end interface

        interface
           function v16f32_mask_getmant_ps(src,k,a,interv,sc) &
                bind(c,name='v16f32_mask_getmant_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),   intent(in),  value :: interv
                integer(c_int),   intent(in),  value :: sc
                type(v16f32) :: v16f32_mask_getmant_ps
           end function v16f32_mask_getmant_ps
        end interface

        interface
           function v8f64_maskz_getmant_pd(k,a,interv,sc) &
                bind(c,name='v8f64_maskz_getmant_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int), intent(in),  value :: interv
                integer(c_int), intent(in),  value :: sc
                type(v8f64) :: v8f64_maskz_getmant_pd
           end function v8f64_maskz_getmant_pd
        end interface

        interface
           function v16f32_maskz_getmant_ps(k,a,interv,sc) &
                bind(c,name='v16f32_maskz_getmant_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),   intent(in),  value :: interv
                integer(c_int),   intent(in),  value :: sc
                type(v16f32) :: v16f32_maskz_getmant_ps
           end function v16f32_maskz_getmant_ps
        end interface

        interface
           function v8f64_getmant_round_pd(a,interv,sc,rounding) &
                bind(c,name='v8f64_getmant_round_pd')
                import :: v8f64
                type(v8f64),    intent(in)        :: a
                integer(c_int), intent(in), value :: interv
                integer(c_int), intent(in), value :: sc
                integer(c_int), intent(in), value :: rounding
                type(v8f64) :: v8f64_getmant_round_pd
           end function v8f64_getmant_round_pd
        end interface

        interface
           function v16f32_getmant_round_ps(a,interv,sc,rounding) &
                bind(c,name='v16f32_getmant_round_ps')
                import :: v16f32
                type(v16f32),    intent(in)        :: a
                integer(c_int), intent(in), value :: interv
                integer(c_int), intent(in), value :: sc
                integer(c_int), intent(in), value :: rounding
                type(v16f32) :: v16f32_getmant_round_ps
           end function v16f32_getmant_round_ps
        end interface

        interface
           function v8f64_mask_getmant_round_pd(src,k,a,interv,sc,rounding) &
                bind(c,name='v8f64_mask_getmant_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: interv
                integer(c_int),  intent(in), value :: sc
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_getmant_round_pd
            end function v8f64_mask_getmant_round_pd
         end interface

         interface
           function v16f32_mask_getmant_round_ps(src,k,a,interv,sc,rounding) &
                bind(c,name='v16f32_mask_getmant_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),   intent(in), value :: interv
                integer(c_int),   intent(in), value :: sc
                integer(c_int),   intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_getmant_round_ps
            end function v16f32_mask_getmant_round_ps
        end interface

        interface
           function v8f64_maskz_getmant_round_pd(k,a,interv,sc,rounding) &
                bind(c,name='v8f64_maskz_getmant_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: interv
                integer(c_int),  intent(in), value :: sc
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_getmant_round_pd
           end function v8f64_maskz_getmant_round_pd
        end interface

        interface
           function v16f32_maskz_getmant_round_ps(k,a,interv,sc,rounding) &
                bind(c,name='v16f32_maskz_getmant_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: interv
                integer(c_int),  intent(in), value :: sc
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_getmant_round_ps
           end function v16f32_maskz_getmant_round_ps
        end interface


        interface
           function v8f64_mask_roundscale_pd(src,k,a,imm8) &
                bind(c,name='v8f64_mask_roundscale_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                type(v8f64) :: v8f64_mask_roundscale_pd
           end function v8f64_mask_roundscale_pd
        end interface

        interface
           function v16f32_mask_roundscale_ps(src,k,a,imm8) &
                bind(c,name='v16f32_mask_roundscale_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                type(v16f32) :: v16f32_mask_roundscale_ps
           end function v16f32_mask_roundscale_ps
        end interface

        

        interface
           function v8f64_maskz_roundscale_pd(k,a,imm8) &
                bind(c,name='v8f64_maskz_roundscale_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                type(v8f64) :: v8f64_maskz_roundscale_pd
           end function v8f64_maskz_roundscale_pd
        end interface

        interface
           function v16f32_maskz_roundscale_ps(k,a,imm8) &
                bind(c,name='v16f32_maskz_roundscale_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                type(v16f32) :: v16f32_maskz_roundscale_ps
           end function v16f32_maskz_roundscale_ps
        end interface

        interface
           function v8f64_roundscale_pd(a,imm) &
                bind(c,name='v8f64_roundscale_pd')
                import :: v8f64
                type(v8f64),    intent(in)        :: a
                integer(c_int), intent(in), value :: imm8
                type(v8f64) :: v8f64_roundscale_pd
           end function v8f64_roundscale_pd
        end interface

        interface
           function v16f32_roundscale_ps(a,imm) &
                bind(c,name='v16f32_roundscale_ps')
                import :: v16f32
                type(v16f32),    intent(in)        :: a
                integer(c_int), intent(in), value :: imm8
                type(v16f32) :: v16f32_roundscale_ps
           end function v16f32_roundscale_ps
        end interface

        

        interface
           function v8f64_mask_roundscale_round_pd(src,k,a,imm8,rounding) &
                bind(c,name='v8f64_mask_roundscale_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_roundscale_round_pd
           end function v8f64_mask_roundscale_round_pd
        end interface

        interface
           function v16f32_mask_roundscale_round_ps(src,k,a,imm8,rounding) &
                bind(c,name='v16f32_mask_roundscale_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_roundscale_round_ps
           end function v16f32_mask_roundscale_round_ps
        end interface

        interface
           function v8f64_maskz_roundscale_round_pd(k,a,imm8,rounding) &
                bind(c,name='v8f64_maskz_roundscale_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_roundscale_round_pd
            end function v8f64_maskz_roundscale_round_pd
        end interface

        interface
           function v16f32_maskz_roundscale_round_ps(k,a,imm8,rounding) &
                bind(c,name='v16f32_maskz_roundscale_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_roundscale_round_ps
            end function v16f32_maskz_roundscale_round_ps
        end interface 

        interface
           function v8f64_roundscale_round_pd(a,imm,rounding) &
                bind(c,name='v8f64_roundscale_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_roundscale_round_pd
           end function v8f64_roundscale_round_pd
        end interface

        interface
           function v16f32_roundscale_round_ps(a,imm,rounding) &
                bind(c,name='v16f32_roundscale_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: a
                integer(c_int),  intent(in), value :: imm8
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_roundscale_round_ps
           end function v16f32_roundscale_round_ps
        end interface

        interface
           function v8f64_mask_scalef_pd(src,k,a,b) &
                bind(c,name='v8f64_mask_scalef_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_mask_scalef_pd
           end function v8f64_mask_scalef_pd
        end interface

        interface
           function v16f32_mask_scalef_ps(src,k,a,b) &
                bind(c,name='v16f32_mask_scalef_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_mask_scalef_ps
           end function v16f32_mask_scalef_ps
        end interface

        interface
           function v8f64_maskz_scalef_pd(k,a,b) &
                bind(c,name='v8f64_maskz_scalef_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_maskz_scalef_pd
           end function v8f64_maskz_scalef_pd
        end interface

        interface
           function v16f32_maskz_scalef_ps(k,a,b) &
                bind(c,name='v16f32_maskz_scalef_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_maskz_scalef_ps
           end function v16f32_maskz_scalef_ps
        end interface

        interface
           function v8f64_scalef_pd(a,b) &
                bind(c,name='v8f64_scalef_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                type(v8f64) :: v8f64_scalef_pd
           end function v8f64_scalef_pd
        end interface

        interface
           function v16f32_scalef_ps(a,b) &
                bind(c,name='v16f32_scalef_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                type(v16f32) :: v16f32_scalef_ps
           end function v16f32_scalef_ps
        end interface

        interface
           function v8f64_mask_scalef_round_pd(src,k,a,b,rounding) &
                bind(c,name='v8f64_mask_scalef_round_pd')
                import :: v8f64
                type(v8f64),     intent(in)        :: src
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_mask_scalef_round_pd
           end function v8f64_mask_scalef_round_pd
        end interface

        interface
           function v16f32_mask_scalef_round_ps(src,k,a,b,rounding) &
                bind(c,name='v16f32_mask_scalef_round_ps')
                import :: v16f32
                type(v16f32),     intent(in)        :: src
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_mask_scalef_round_ps
           end function v16f32_mask_scalef_round_ps
        end interface


        interface
           function v8f64_maskz_scalef_round_pd(k,a,b,rounding) &
                bind(c,name='v8f64_maskz_scalef_round_pd')
                import :: v8f64
                integer(c_char), intent(in), value :: k
                type(v8f64),     intent(in)        :: a
                type(v8f64),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v8f64) :: v8f64_maskz_scalef_round_pd
            end function v8f64_maskz_scalef_round_pd
         end interface

         interface
           function v16f32_maskz_scalef_round_ps(k,a,b,rounding) &
                bind(c,name='v16f32_maskz_scalef_round_ps')
                import :: v16f32
                integer(c_short), intent(in), value :: k
                type(v16f32),     intent(in)        :: a
                type(v16f32),     intent(in)        :: b
                integer(c_int),  intent(in), value :: rounding
                type(v16f32) :: v16f32_maskz_scalef_round_ps
            end function v16f32_maskz_scalef_round_ps
         end interface

         interface
            function v8f64_scalef_round_pd(a,b,rounding) &
                 bind(c,name='v8f64_scalef_round_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                type(v8f64),      intent(in)        :: b
                integer(c_int),   intent(in), value :: rounding
                type(v8f64) :: v8f64_scalef_round_pd
            end function v8f64_scalef_round_pd
         end interface

         interface
            function v16f32_scalef_round_ps(a,b,rounding) &
                 bind(c,name='v16f32_scalef_round_ps')
                 import :: v16f32
                 type(v16f32),      intent(in)        :: a
                 type(v16f32),      intent(in)        :: b
                 integer(c_int),    intent(in), value :: rounding
                 type(v16f32) :: v16f32_scalef_round_ps
            end function v16f32_scalef_round_ps
         end interface

         interface
            function v8f64_mask_mov_pd(src,k,a) &
                 bind(c,name='v8f64_mask_mov_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_mask_mov_pd
            end function v8f64_mask_mov_pd
         end interface

         interface
            function v16f32_mask_mov_ps(src,k,a) &
                 bind(c,name='v16f32_mask_mov_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_mask_mov_ps
            end function v16f32_mask_mov_ps
         end interface

         interface
            function v8f64_maskz_mov_pd(k,a) &
                 bind(c,name='v8f64_maskz_mov_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_maskz_mov_pd
            end function v8f64_maskz_mov_pd
         end interface

         interface
            function v16f32_maskz_mov_ps(k,a) &
                 bind(c,name='v16f32_maskz_mov_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_maskz_mov_ps
            end function v16f32_maskz_mov_ps
         end interface


         interface
            function v8f64_mask_movedup_pd(src,k,a) &
                 bind(c,name='v8f64_mask_movedup_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_mask_movedup_pd
            end function v8f64_mask_movedup_pd
         end interface

         interface
            function v16f32_mask_movedup_ps(src,k,a) &
                 bind(c,name='v16f32_mask_movedup_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_mask_movedup_ps
            end function v16f32_mask_movedup_ps
         end interface

         interface
            function v8f64_maskz_movedup_pd(k,a) &
                 bind(c,name='v8f64_maskz_movedup_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_maskz_movedup_pd
            end function v8f64_maskz_movedup_pd
         end interface

         interface
            function v16f32_maskz_movedup_ps(k,a) &
                 bind(c,name='v16f32_maskz_movedup_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_maskz_movedup_ps
            end function v16f32_maskz_movedup_ps
         end interface

         interface
            function v8f64_movedup_pd(a) &
                 bind(c,name='v8f64_movedup_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_movedup_pd
            end function v8f64_movedup_pd
         end interface

         interface
            function v16f32_movedup_ps(a) &
                 bind(c,name='v16f32_movedup_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_movedup_ps
            end function v16f32_movedup_ps
         end interface

         interface
            function v8f64_set4_pd(d,c,b,a) &
                 bind(c,name='v8f64_set4_pd')
                 import :: v8f64
                 real(c_double), intent(in), value :: d
                 real(c_double), intent(in), value :: c
                 real(c_double), intent(in), value :: b
                 real(c_double), intent(in), value :: a
                 type(v8f64) :: v8f64_set4_pd
            end function v8f64_set4_pd
         end interface

         

         interface
            function v8f64_mask_max_pd(src,k,a,b) &
                 bind(c,name='v8f64_mask_max_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_max_pd
             end function v8f64_mask_max_pd
          end interface

          interface
            function v16f32_mask_max_ps(src,k,a,b) &
                 bind(c,name='v16f32_mask_max_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_max_ps
             end function v16f32_mask_max_ps
         end interface

         interface
            function v8f64_maskz_max_pd(k,a,b) &
                 bind(c,name='v8f64_maskz_max_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_maskz_max_pd
            end function v8f64_maskz_max_pd
         end interface

         interface
            function v16f32_maskz_max_ps(k,a,b) &
                 bind(c,name='v16f32_maskz_max_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_maskz_max_ps
            end function v16f32_maskz_max_ps
         end interface

         interface
            function v8f64_max_pd(a,b) &
                 bind(c,name='v8f64_max_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_max_pd
            end function v8f64_max_pd
         end interface

         interface
            function v16f32_max_ps(a,b) &
                 bind(c,name='v16f32_max_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_max_ps
            end function v16f32_max_ps
         end interface

         interface
            function v8f64_mask_max_round_pd(src,k,a,b,sae) &
                 bind(c,name='v8f64_mask_max_round_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_mask_max_round_pd
            end function v8f64_mask_max_round_pd
         end interface

         interface
            function v16f32_mask_max_round_ps(src,k,a,b,sae) &
                 bind(c,name='v16f32_mask_max_round_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v16f32) :: v16f32_mask_max_round_ps
            end function v16f32_mask_max_round_ps
         end interface


         interface
            function v8f64_maskz_max_round_pd(k,a,b,sae) &
                 bind(c,name='v8f64_maskz_max_round_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_maskz_max_round_pd
            end function v8f64_maskz_max_round_pd
         end interface

         interface
            function v16f32_maskz_max_round_ps(k,a,b,sae) &
                 bind(c,name='v16f32_maskz_max_round_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),   intent(in), value :: sae
                 type(v16f32) :: v16f32_maskz_max_round_ps
            end function v16f32_maskz_max_round_ps
         end interface

         interface
            function v8f64_max_round_pd(a,b,sae) &
                 bind(c,name='v8f64_max_round_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_max_round_pd
            end function v8f64_max_round_pd
         end interface

         interface
            function v16f32_max_round_ps(a,b,sae) &
                 bind(c,name='v16f32_max_round_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v16f32) :: v16f32_max_round_ps
            end function v16f32_max_round_ps
         end interface


         interface
            function v8f64_mask_min_pd(src,k,a,b) &
                 bind(c,name='v8f64_mask_min_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_min_pd
             end function v8f64_mask_min_pd
         end interface

         interface
            function v16f32_mask_min_ps(src,k,a,b) &
                 bind(c,name='v16f32_mask_min_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_min_ps
             end function v16f32_mask_min_ps
         end interface 

         interface
            function v8f64_maskz_min_pd(k,a,b) &
                 bind(c,name='v8f64_maskz_min_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_maskz_min_pd
            end function v8f64_maskz_min_pd
         end interface

         interface
            function v16f32_maskz_min_ps(k,a,b) &
                 bind(c,name='v16f32_maskz_min_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_maskz_min_ps
            end function v16f32_maskz_min_ps
         end interface


         interface
            function v8f64_min_pd(a,b) &
                 bind(c,name='v8f64_min_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_min_pd
            end function v8f64_min_pd
         end interface

         interface
            function v16f32_min_ps(a,b) &
                 bind(c,name='v16f32_min_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_min_ps
            end function v16f32_min_ps
         end interface

         interface
            function v8f64_mask_min_round_pd(src,k,a,b,sae) &
                 bind(c,name='v8f64_mask_min_round_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_mask_min_round_pd
            end function v8f64_mask_min_round_pd
         end interface

         interface
            function v16f32_mask_min_round_ps(src,k,a,b,sae) &
                 bind(c,name='v16f32_mask_min_round_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v16f32) :: v16f32_mask_min_round_ps
            end function v16f32_mask_min_round_ps
         end interface


         interface
            function v8f64_maskz_min_round_pd(k,a,b,sae) &
                 bind(c,name='v8f64_maskz_min_round_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_maskz_min_round_pd
            end function v8f64_maskz_min_round_pd
         end interface

         interface
            function v16f32_maskz_min_round_ps(k,a,b,sae) &
                 bind(c,name='v16f32_maskz_min_round_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v16f32) :: v16f32_maskz_min_round_ps
            end function v16f32_maskz_min_round_ps
         end interface

         interface
            function v8f64_min_round_pd(a,b,sae) &
                 bind(c,name='v8f64_min_round_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v8f64) :: v8f64_min_round_pd
            end function v8f64_min_round_pd
         end interface

         interface
            function v16f32_min_round_ps(a,b,sae) &
                 bind(c,name='v16f32_min_round_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: sae
                 type(v16f32) :: v16f32_min_round_ps
            end function v16f32_min_round_ps
         end interface


         interface
            function v8f64_mask_reduce_max_pd(k,a) &
                 bind(c,name='v8f64_mask_reduce_max_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 real(c_double) :: v8f64_mask_reduce_max_pd
            end function v8f64_mask_reduce_max_pd
         end interface

         interface
            function v16f32_mask_reduce_max_ps(k,a) &
                 bind(c,name='v16f32_mask_reduce_max_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 real(c_float) :: v16f32_mask_reduce_max_ps
            end function v16f32_mask_reduce_max_ps
         end interface


         interface
            function v8f64_reduce_max_pd(a) &
                 bind(c,name='v8f64_reduce_max_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 real(c_double) :: v8f64_reduce_max_pd
            end function v8f64_reduce_max_pd
         end interface

         interface
            function v16f32_reduce_max_ps(a) &
                 bind(c,name='v16f32_reduce_max_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 real(c_float) :: v16f32_reduce_max_ps
            end function v16f32_reduce_max_ps
         end interface


         interface
            function v8f64_mask_reduce_min_pd(k,a) &
                 bind(c,name='v8f64_mask_reduce_min_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 real(c_double) :: v8f64_mask_reduce_min_pd
            end function v8f64_mask_reduce_min_pd
         end interface

         interface
            function v16f32_mask_reduce_min_ps(k,a) &
                 bind(c,name='v16f32_mask_reduce_min_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 real(c_float) :: v16f32_mask_reduce_min_ps
            end function v16f32_mask_reduce_min_ps
         end interface


         interface
            function v8f64_reduce_min_pd(a) &
                 bind(c,name='v8f64_reduce_min_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 real(c_double) :: v8f64_reduce_min_pd
            end function v8f64_reduce_min_pd
         end interface

         interface
            function v16f32_reduce_min_ps(a) &
                 bind(c,name='v16f32_reduce_min_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 real(c_float) :: v16f32_reduce_min_ps
            end function v16f32_reduce_min_ps
         end interface

         
         interface
            function v8f64_mask_unpacklo_pd(src,k,a,b) &
                 bind(c,name='v8f64_mask_unpacklo_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_unpacklo_pd
            end function v8f64_mask_unpacklo_pd
         end interface

         interface
            function v16f32_mask_unpacklo_ps(src,k,a,b) &
                 bind(c,name='v16f32_mask_unpacklo_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_unpacklo_ps
            end function v16f32_mask_unpacklo_ps
         end interface

         interface
            function v8f64_maskz_unpacklo_pd(k,a,b) &
                 bind(c,name='v8f64_maskz_unpacklo_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_unpacklo_pd
            end function v8f64_maskz_unpacklo_pd
         end interface

         interface
            function v16f32_maskz_unpacklo_ps(k,a,b) &
                 bind(c,name='v16f32_maskz_unpacklo_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_unpacklo_ps
            end function v16f32_maskz_unpacklo_ps
         end interface

         interface
            function v8f64_unpacklo_pd(a,b) &
                 bind(c,name='v8f64_unpacklo_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_unpacklo_pd
            end function v8f64_unpacklo_pd
         end interface

         interface
            function v16f32_unpacklo_ps(a,b) &
                 bind(c,name='v16f32_unpacklo_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_unpacklo_ps
            end function v16f32_unpacklo_ps
         end interface


         interface
            function v8f64_mask_unpackhi_pd(src,k,a,b) &
                 bind(c,name='v8f64_mask_unpackhi_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_unpackhi_pd
            end function v8f64_mask_unpackhi_pd
         end interface

         interface
            function v16f32_mask_unpackhi_ps(src,k,a,b) &
                 bind(c,name='v16f32_mask_unpackhi_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_unpackhi_ps
            end function v16f32_mask_unpackhi_ps
         end interface

         interface
            function v8f64_maskz_unpackhi_pd(k,a,b) &
                 bind(c,name='v8f64_maskz_unpackhi_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_maskz_unpackhi_pd
            end function v8f64_maskz_unpackhi_pd
         end interface

         interface
            function v16f32_maskz_unpackhi_ps(k,a,b) &
                 bind(c,name='v16f32_maskz_unpackhi_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_maskz_unpackhi_ps
            end function v16f32_maskz_unpackhi_ps
         end interface

         interface
            function v8f64_unpackhi_pd(a,b) &
                 bind(c,name='v8f64_unpackhi_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_unpackhi_pd
             end function v8f64_unpackhi_pd
          end interface

          interface
            function v16f32_unpackhi_ps(a,b) &
                 bind(c,name='v16f32_unpackhi_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_unpackhi_ps
             end function v16f32_unpackhi_ps
          end interface


         interface
            function v8f64_mask_shuffle_pd(src,k,a,b,imm8) &
                 bind(c,name='v8f64_mask_shuffle_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_mask_shuffle_pd
            end function v8f64_mask_shuffle_pd
         end interface

         interface
            function v16f32_mask_shuffle_ps(src,k,a,b,imm8) &
                 bind(c,name='v16f32_mask_shuffle_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),   intent(in), value :: imm8
                 type(v16f32) :: v16f32_mask_shuffle_ps
            end function v16f32_mask_shuffle_ps
         end interface

         interface
            function v8f64_maskz_shuffle_pd(k,a,b,imm8) &
                 bind(c,name='v8f64_maskz_shuffle_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_maskz_shuffle_pd
            end function v8f64_maskz_shuffle_pd
         end interface

         interface
            function v16f32_maskz_shuffle_ps(k,a,b,imm8) &
                 bind(c,name='v16f32_maskz_shuffle_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_maskz_shuffle_ps
            end function v16f32_maskz_shuffle_ps
         end interface


         interface
            function v8f64_shuffle_pd(a,b,imm8) &
                 bind(c,name='v8f64_shuffle_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_shuffle_pd
            end function v8f64_shuffle_pd
         end interface

         interface
            function v16f32_shuffle_ps(a,b,imm8) &
                 bind(c,name='v16f32_shuffle_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_shuffle_ps
            end function v16f32_shuffle_ps
         end interface

         interface
            function v8f64_mask_shuffle_f64x2(src,k,a,b,imm8) &
                 bind(c,name='v8f64_mask_shuffle_f64x2')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_mask_shuffle_f64x2
             end function v8f64_mask_shuffle_f64x2
          end interface

       

         interface
            function v8f64_maskz_shuffle_f64x2(k,a,b,imm8) &
                 bind(c,name='v8f64_maskz_shuffle_f64x2')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_maskz_shuffle_f64x2
             end function v8f64_maskz_shuffle_f64x2
         end interface

         interface
            function v8f64_shuffle_f64x2(a,b,imm8) &
                 bind(c,name='v8f64_shuffle_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_shuffle_f64x2
             end function v8f64_shuffle_f64x2
         end interface

         interface
            function v8f64_mask_permutex_pd(src,k,a,imm8) &
                 bind(c,name='v8f64_mask_permutex_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_mask_permutex_pd
            end function v8f64_mask_permutex_pd
         end interface

         interface
            function v16f32_mask_permutex_ps(src,k,a,imm8) &
                 bind(c,name='v16f32_mask_permutex_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_mask_permutex_ps
            end function v16f32_mask_permutex_ps
         end interface

         interface
            function v8f64_maskz_permutex_pd(k,a,imm8) &
                 bind(c,name='v8f64_maskz_permutex_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_maskz_permutex_pd
            end function v8f64_maskz_permutex_pd
         end interface

         interface
            function v16f32_maskz_permutex_ps(k,a,imm8) &
                 bind(c,name='v16f32_maskz_permutex_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_maskz_permutex_ps
            end function v16f32_maskz_permutex_ps
         end interface

         interface
            function v8f64_permutex_pd(a,imm8) &
                 bind(c,name='v8f64_permutex_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_permutex_pd
            end function v8f64_permutex_pd
         end interface

         interface
            function v16f32_permutex_ps(a,imm8) &
                 bind(c,name='v16f32_permutex_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_permutex_ps
            end function v16f32_permutex_ps
         end interface

         interface
            function v8f64_mask_blend_pd(k,a,b) &
                 bind(c,name='v8f64_mask_blend_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64),     intent(in)        :: b
                 type(v8f64) :: v8f64_mask_blend_pd
            end function v8f64_mask_blend_pd
         end interface

         interface
            function v16f32_mask_blend_ps(k,a,b) &
                 bind(c,name='v16f32_mask_blend_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32),     intent(in)        :: b
                 type(v16f32) :: v16f32_mask_blend_ps
            end function v16f32_mask_blend_ps
         end interface

         interface
            function v8f64_mask_compress_pd(src,k,a) &
                 bind(c,name='v8f64_mask_compress_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_mask_compress_pd
            end function v8f64_mask_compress_pd
         end interface

         interface
            function v16f32_mask_compress_ps(src,k,a) &
                 bind(c,name='v16f32_mask_compress_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_mask_compress_ps
            end function v16f32_mask_compress_ps
         end interface

         interface
            function v8f64_maskz_compress_pd(k,a) &
                 bind(c,name='v8f64_maskz_compress_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_maskz_compress_pd
            end function v8f64_maskz_compress_pd
         end interface

         interface
            function v16f32_maskz_compress_ps(k,a) &
                 bind(c,name='v16f32_maskz_compress_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_maskz_compress_ps
            end function v16f32_maskz_compress_ps
         end interface


         interface
            subroutine  v8f64_mask_compressstoreu_pd(base_addr,k,a) &
                 bind(c,name='v8f64_mask_compressstoreu_pd')
                 import :: v8f64
                 type(c_ptr),     intent(out), value :: base_addr
                 integer(c_char), intent(in),  value :: k
                 type(v8f64),     intent(in)         :: a
            end subroutine v8f64_mask_compressstoreu_pd
         end interface

         interface
            subroutine  v16f32_mask_compressstoreu_ps(base_addr,k,a) &
                 bind(c,name='v16f32_mask_compressstoreu_ps')
                 import :: v16f32
                 type(c_ptr),     intent(out), value :: base_addr
                 integer(c_short), intent(in),  value :: k
                 type(v16f32),     intent(in)         :: a
            end subroutine v16f32_mask_compressstoreu_ps
         end interface

         interface
            function v8f64_mask_expand_pd(src,k,a) &
                 bind(c,name='v8f64_mask_expand_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_mask_expand_pd
            end function v8f64_mask_expand_pd
         end interface

         interface
            function v16f32_mask_expand_ps(src,k,a) &
                 bind(c,name='v16f32_mask_expand_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_mask_expand_ps
            end function v16f32_mask_expand_ps
         end interface

         interface
            function v8f64_maskz_expand_pd(k,a) &
                 bind(c,name='v8f64_maskz_expand_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 type(v8f64) :: v8f64_maskz_expand_pd
            end function v8f64_maskz_expand_pd
         end interface

         interface
            function v16f32_maskz_expand_ps(k,a) &
                 bind(c,name='v16f32_maskz_expand_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 type(v16f32) :: v16f32_maskz_expand_ps
            end function v16f32_maskz_expand_ps
         end interface

       

         interface
            function v8f64_mask_permute_pd(src,k,a,imm8) &
                 bind(c,name='v8f64_mask_permute_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: src
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_mask_permute_pd
             end function v8f64_mask_permute_pd
          end interface

        
         interface
            function v16f32_mask_permute_ps(src,k,a,imm8) &
                 bind(c,name='v16f32_mask_permute_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: src
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),   intent(in), value :: imm8
                 type(v16f32) :: v16f32_mask_permute_ps
             end function v16f32_mask_permute_ps
         end interface

         interface
            function v8f64_maskz_permute_pd(k,a,imm8) &
                 bind(c,name='v8f64_maskz_permute_pd')
                 import :: v8f64
                 integer(c_char), intent(in), value :: k
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_maskz_permute_pd
             end function v8f64_maskz_permute_pd
         end interface

         
        interface
            function v16f32_maskz_permute_ps(k,a,imm8) &
                 bind(c,name='v16f32_maskz_permute_ps')
                 import :: v16f32
                 integer(c_short), intent(in), value :: k
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_maskz_permute_ps
             end function v16f32_maskz_permute_ps
         end interface 

         interface
            function v8f64_permute_pd(a,imm8) &
                 bind(c,name='v8f64_permute_pd')
                 import :: v8f64
                 type(v8f64),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v8f64) :: v8f64_permute_pd
             end function v8f64_permute_pd
         end interface

        interface
            function v16f32_permute_ps(a,imm8) &
                 bind(c,name='v16f32_permute_ps')
                 import :: v16f32
                 type(v16f32),     intent(in)        :: a
                 integer(c_int),  intent(in), value :: imm8
                 type(v16f32) :: v16f32_permute_ps
             end function v16f32_permute_ps
         end interface  

          

          
    
end module mod_avx512_bindings
