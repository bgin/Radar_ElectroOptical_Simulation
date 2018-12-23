
    
module mod_avx_bindings


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_avx_bindings'
 !          
 !          Purpose:
 !                    Fortran bindings to Intel AVX Intrinsics.
 !                   
 !                     
 !          History:
 !                        Date: 12-22-2018
 !                        Time: 16:41 GMT+2
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
    use mod_kinds, only : int4, sp, dp
    use, intrinsic :: ISO_C_BINDING
    implicit none
    
    public :: vec4f64_add_vec4f64,    &
              vec4f64_sub_vec4f64,    &
              vec4f64_mul_vec4f64,    &
              vec4f64_div_vec4f64,    &
              vec4f64_addsub_vec4f64, &
              vec4f64_and_vec4f64,    &
              vec4f64_andnot_vec4f64, &
              vec4f64_blend_vec4f64,  &
              vec4f64_blendv_vec4f64, &
              vec4f64_cmp_vec4f64,    &
              
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_AVX_BINDINGS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_AVX_BINDINGS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_AVX_BINDINGS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_AVX_BINDINGS_FULLVER = 1000_int4*MOD_AVX_BINDINGS_MAJOR + &
                                                                        100_int4*MOD_AVX_BINDINGS_MINOR  + &
                                                                        10_int4*MOD_AVX_BINDINGS_MICRO
    
    ! Module  creation date
    character(*),       parameter, public :: MOD_AVX_BINDINGS_CREATE_DATE =  "22-12-2018 16:42 +00200 (SAT 22 DEC 2018 GMT+2)"
    
    ! Module  build date
    character(*),       parameter, public :: MOD_AVX_BINDINGS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module author info
    character(*),       parameter, public :: MOD_AVX_BINDINGS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_AVX_BINDINGS_SYNOPSIS = "Fortran bindings to Intel AVX Intrinsics."
    
    ! Interoperable with C-side v4f64
    type, bind(c), public :: v4f64
          real(c_double), dimension(0:3) :: v
    end type  v4f64
    
    interface
        function vec4f64_add_vec4f64(a,b)  &
                         bind(c,name='vec4f64_add_vec4f64')
                import :: v4f64
                type(v4f64), intent(in) :: a
                type(v4f64), intent(in) :: b
                type(v4f64) :: vec4f64_add_vec4f64
        end function vec4f64_add_vec4f64
    end interface
    
    interface
        function vec4f64_sub_vec4f64(a,b) &
                         bind(c,name='vec4f64_sub_vec4f64')
                import :: v4f64
                type(v4f64), intent(in) :: a
                type(v4f64), intent(in) :: b
                type(v4f64) :: vec4f64_sub_vec4f64
        end function vec4f64_sub_vec4f64
    end interface
    
    interface
        function vec4f64_mul_vec4f64(a,b) &
                         bind(c,name='vec4f64_mul_vec4f64')
                 import :: v4f64
                 type(v4f64), intent(in) :: a
                 type(v4f64), intent(in) :: b
                 type(v4f64) :: vec4f64_mul_vec4f64
        end function vec4f64_mul_vec4f64
    end interface
    
    interface
        function vec4f64_div_vec4f64(a,b) &
                         bind(c,name='vec4f64_div_vec4f64')
                 import :: v4f64
                 type(v4f64), intent(in) :: a
                 type(v4f64), intent(in) :: b
                 type(v4f64) :: vec4f64_div_vec4f64
        end function vec4f64_div_vec4f64
    end interface
    
    interface
         function vec4f64_addsub_vec4f64(a,b) &
                          bind(c,name='vec4f64_addsub_vec4f64')
                  import :: v4f64
                  type(v4f64), intent(in) :: a
                  type(v4f64), intent(in) :: b
                  type(v4f64) :: vec4f64_addsub_vec4f64
         end function vec4f64_addsub_vec4f64
    end interface
    
    interface
        function vec4f64_and_vec4f64(a,b) &
                         bind(c,name='vec4f64_and_vec4f64')
                  import :: v4f64
                  type(v4f64), intent(in) :: a
                  type(v4f64), intent(in) :: b
                  type(v4f64) :: vec4f64_and_vec4f64
        end function vec4f64_and_vec4f64
    end interface
    
    interface
        function vec4f64_andnot_vec4f64(a,b)  &
                         bind(c,name='vec4f64_andnot_vec4f64')
                  import :: v4f64
                  type(v4f64), intent(in) :: a
                  type(v4f64), intent(in) :: b
                  type(v4f64) :: vec4f64_andnot_vec4f64
        end function vec4f64_andnot_vec4f64
    end interface
    
    interface
        function vec4f64_blend_vec4f64(a,b,imm) &
                         bind(c,name='vec4f64_blend_vec4f64')
                  use, intrinsic :: ISO_C_BINDING
                  import :: v4f64
                  type(v4f64),    intent(in) :: a
                  type(v4f64),    intent(in) :: b
                  integer(c_int), intent(in), value :: imm
                  type(v4f64) :: vec4f64_blend_vec4f64
        end function vec4f64_blend_vec4f64
    end interface
    
    interface
        function vec4f64_blendv_vec4f64(a,b,c) &
                         bind(c,name='vec4f64_blendv_vec4f64' )
                 import :: v4f64
                 type(v4f64), intent(in) :: a
                 type(v4f64), intent(in) :: b
                 type(v4f64), intent(in) :: c
                 type(v4f64) :: vec4f64_blendv_vec4f64
        end function vec4f64_blendv_vec4f64
    end interface
    
    interface
        function vec4f64_ceil_vec4f64(a) &
                         bind(c,name='vec4f64_ceil_vec4f64')
                 import :: v4f64
                 type(v4f64), intent(in) :: a
                 type(v4f64) :: vec4f64_ceil_vec4f64
        end function vec4f64_ceil_vec4f64
    end interface
    
    interface
        function vec4f64_cmp_vec4f64(a,b,imm) &
                          bind(c,name='vec4f64_cmp_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 integer(c_int), intent(in), value :: imm
                 type(v4f64) :: vec4f64_cmp_vec4f64
        end function vec4f64_cmp_vec4f64
    end interface
    
    interface
        function vec4f64_floor_vec4f64(a)  &
                         bind(c,name='vec4f64_floor_vec4f64')
                  import :: v4f64
                  type(v4f64), intent(in) :: a
                  type(v4f64) :: vec4f64_floor_vec4f64
        end function vec4f64_floor_vec4f64
    end interface
    
    interface
        function vec4f64_fmadd_vec4f64(a,b,c)  &
                         bind(c,name='vec4f64_fmadd_vec4f64')
                    import :: v4f64
                    type(v4f64), intent(in) :: a
                    type(v4f64), intent(in) :: b
                    type(v4f64), intent(in) :: c
                    type(v4f64) :: vec4f64_fmadd_vec4f64
        end function vec4f64_fmadd_vec4f64
    end interface
    
    interface
        function vec4f64_fmaddsub_vec4f64(a,b,c) &
                         bind(c,name='vec4f64_fmaddsub_vec4f64')
                     import :: v4f64
                     type(v4f64), intent(in) :: a
                     type(v4f64), intent(in) :: b
                     type(v4f64), intent(in) :: c
                     type(v4f64) :: vec4f64_fmaddsub_vec4f64
        end function vec4f64_fmaddsub_vec4f64
    end interface
    
    interface 
        function vec4f64_fmsub_vec4f64(a,b,c)  &
                         bind(c,name='vec4f64_fmsub_vec4f64')
                       import :: v4f64
                       type(v4f64), intent(in) :: a
                       type(v4f64), intent(in) :: b
                       type(v4f64), intent(in) :: c
                       type(v4f64) :: vec4f64_fmsub_vec4f64
        end function vec4f64_fmsub_vec4f64
    end interface
    
    interface
        function vec4f64_fmsubadd_vec4f64(a,b,c) &
                          bind(c,name='vec4f64_fmsubadd_vec4f64')
                        import :: v4f64
                        type(v4f64), intent(in) :: a
                        type(v4f64), intent(in) :: b
                        type(v4f64), intent(in) :: c
                        type(v4f64) :: vec4f64_fmsubadd_vec4f64
        end function vec4f64_fmsubadd_vec4f64
    end interface
    
    interface
        function vec4f64_fnmadd_vec4f64(a,b,c) &
                          bind(c,name='vec4f64_fnmadd_vec4f64')
                        import :: v4f64
                        type(v4f64), intent(in) :: a
                        type(v4f64), intent(in) :: b
                        type(v4f64), intent(in) :: c
                        type(v4f64) :: vec4f64_fnmadd_vec4f64
        end function vec4f_fnmadd_vec4f64
    end interface
    
    interface
        function vec4f64_fnmsub_vec4f64(a,b,c) &
                          bind(c,name='vec4f64_fnmsub_vec4f64')
                        import :: v4f64
                        type(v4f64), intent(in) :: a
                        type(v4f64), intent(in) :: b
                        type(v4f64), intent(in) :: c
                        type(v4f64) :: vec4f64_fnmsub_vec4f64
        end function vec4f64_fnmsub_vec4f64
    end interface
    
    interface
        function vec4f64_hadd_vec4f64(a,b) &
                          bind(c,name='vec4f64_hadd_vec4f64')
                        import :: v4f64
                        type(v4f64), intent(in) :: a
                        type(v4f64), intent(in) :: b
                        type(v4f64) :: vec4f64_hadd_vec4f64
        end function vec4f64_hadd_vec4f64
    end interface
    
    interface
        function vec4f64_hsub_vec4f64(a,b) &
                          bind(c,name='vec4f64_hsub_vec4f64')
                        import :: v4f64
                        type(v4f64), intent(in) :: a
                        type(v4f64), intent(in) :: b
                        type(v4f64) :: vec4f64_hsub_vec4f64
        end function vec4f64_hsub_vec4f64
    end interface
    
    interface
        function vec4f64_max_vec4f64(a,b)  &
                           bind(c,name='vec4f64_max_vec4f64')
                         import :: v4f64
                         type(v4f64), intent(in) :: a
                         type(v4f64), intent(in) :: b
                         type(v4f64) :: vec4f64_max_vec4f64
        end function vec4f64_max_vec4f64
    end interface
    
    interface
        function vec4f64_min_vec4f64(a,b)  &
                           bind(c,name='vec4f64_min_vec4f64')
                          import :: v4f64
                          type(v4f64), intent(in) :: a
                          type(v4f64), intent(in) :: b
                          type(v4f64) :: vec4f64_min_vec4f64
        end function vec4f64_min_vec4f64
    end interface
    
    interface
        function vec4f64_movedup_vec4f64(a) &
                            bind(c,name='vec4f64_movedup_vec4f64')
                           import :: v4f64
                           type(v4f64), intent(in) :: a
                           type(v4f64) :: vec4f64_movedup_vec4f64
        end function vec4f64_movedup_vec4f64
    end interface
    
    interface 
        function vec4f64_movemask_vec4f64(a) &
                             bind(c,name='vec4f64_movemask_vec4f64')
                           use, intrinsic :: ISO_C_BINDING
                           import :: v4f64
                           type(v4f64), intent(in) :: a
                           integer(c_int) :: vec4f64_movemask_vec4f64
        end function vec4f64_movemask_vec4f64
    end interface
    
    interface
         function vec4f64_or_vec4f64(a,b)  &
                            bind(c,name='vec4f64_or_vec4f64')
                          import :: v4f64
                          type(v4f64), intent(in) :: a
                          type(v4f64), intent(in) :: b
                          type(v4f64) :: vec4f64_or_vec4f64
         end function vec4f64_or_vec4f64
    end interface
    
    interface
         function vec4f64_permute2f128_vec4f64(a,b,imm)  &
                            bind(c,name='vec4f64_permute2f128_vec4f64')
                          use, intrinsic :: ISO_C_BINDING
                          import :: v4f64
                          type(v4f64), intent(in) :: a
                          type(v4f64), intent(in) :: b
                          integer(c_int), intent(in), value :: imm
                          type(v4f64) :: vec4f64_permute2f128_vec4f64
         end function vec4f64_permute2f128_vec4f64
    end interface
    

end module mod_avx_bindings