

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
     use mod_kinds, only : int4
     use, intrinsic :: ISO_C_BINDING
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4),  parameter :: MOD_AVX512_BINDINGS_MAJOR = 1
    ! Minor version
    integer(kind=int4),  parameter :: MOD_AVX512_BINDINGS_MINOR = 0
    ! Micro version
    integer(kind=int4),  parameter :: MOD_AVX512_BINDINGS_MICRO = 0
    ! Full version
    integer(kind=int4),  parameter :: MOD_AVX512_BINDINGS_FULLVER =  &
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
       public
       real(c_double), dimension(0:7) :: zmm
    end type v8f64

    type, bind(c), public :: v16f32
       public
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
       function v8f64_sub_pd(a,b) &
            bind(c,name='v8f64_sub_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_sub_pd
       end function v8f64_sub_pd
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
       function v8f64_div_pd(a,b) &
            bind(c,name='v8f64_div_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_div_pd
        end function v8f64_div_pd
    end interface

    interface
       function v8f64_addsub_pd(a,b) &
            bind(c,name='v8f64_addsub_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_addsub_pd
       end function v8f64_addsub_pd
    end interface

    interface
       function v8f64_and_pd(a,b) &
            bind(c,name='v8f64_and_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_and_pd
       end function v8f64_and_pd
    end interface

    interface
       function v8f64_andnot_pd(a,b) &
            bind(c,name='v8f64_andnot_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64),  intent(in) :: b
            type(v8f64) :: v8f64_andnot_pd
       end function v8f64_andnot_pd
    end interface

    interface
       function v8f64_blendv_pd(a,b,c) &
            bind(c,name='v8f64_blendv_pd')
            import :: v8f64
            type(v8f64),   intent(in) :: a
            type(v8f64),   intent(in) :: b
            type(v8f64),   intent(in) :: c
            type(v8f64) :: v8f64_blendv_pd
       end function v8f64_blendv_pd
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
       function v8f64_abs_pd(a) &
            bind(c,name='v8f64_abs_pd')
            import :: v8f64
            type(v8f64),  intent(in) :: a
            type(v8f64) :: v8f64_abs_pd
       end function v8f64_abs_pd
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
       function v8f64_maskz_fmadd_round_pd(k,a,b,c,rounding) &
            bind(c,name='v8f64_maskz_fmadd_round')
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
    
end module mod_avx512_bindings
