
    
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
    use mod_kinds, only : int4
    use, intrinsic :: ISO_C_BINDING
    implicit none
    public
   
              
    
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
        function vec4f64_cmp_eqoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_eqoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_eqoq_vec4f64
               
        end function vec4f64_cmp_eqoq_vec4f64
    end interface
    
    interface
         function vec4f64_cmp_ltoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_ltoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_ltoq_vec4f64
               
        end function vec4f64_cmp_ltoq_vec4f64
    end interface
    
    interface
         function vec4f64_cmp_leoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_leoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_leoq_vec4f64
               
        end function vec4f64_cmp_leoq_vec4f64
    end interface
    
    interface
         function vec4f64_cmp_neqoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_neqoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_neqoq_vec4f64
               
        end function vec4f64_cmp_neqoq_vec4f64 
    end interface
    
    interface
          function vec4f64_cmp_gtoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_gtoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_gtoq_vec4f64
               
        end function vec4f64_cmp_gtoq_vec4f64
    end interface
    
    interface
            function vec4f64_cmp_geoq_vec4f64(a,b) &
                          bind(c,name='vec4f64_cmp_geoq_vec4f64')
                 use, intrinsic :: ISO_C_BINDING
                 import :: v4f64
                 type(v4f64),    intent(in) :: a
                 type(v4f64),    intent(in) :: b
                 type(v4f64) :: vec4f64_cmp_geoq_vec4f64
               
        end function vec4f64_cmp_geoq_vec4f64
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
        end function vec4f64_fnmadd_vec4f64
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
          subroutine vec4f64_add_pd(c,b,a)  &
                              bind(c,name='vec4f64_add_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_add_pd
    end interface
    
    interface
          subroutine vec4f64_sub_pd(c,b,a) &
                              bind(c,name='vec4f64_sub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_sub_pd
    end interface
    
    interface
          subroutine vec4f64_mul_pd(c,b,a) &
                              bind(c,name='vec4f64_mul_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_mul_pd
    end interface
    
    interface
           subroutine vec4f64_div_pd(c,b,a) &
                               bind(c,name='vec4f64_div_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
           end subroutine vec4f64_div_pd
    end interface
    
    interface
          subroutine vec4f64_addsub_pd(c,b,a) &
                                bind(c,name='vec4f64_addsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_addsub_pd
    end interface
    
    interface
          subroutine vec4f64_and_pd(c,b,a) &
                              bind(c,name='vec4f64_and_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_and_pd
    end interface
    
    interface
          subroutine vec4f64_andnot_pd(c,b,a) &
                               bind(c,name='vec4f64_andnot_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_andnot_pd
    end interface
    
   
    
    interface
          subroutine vec4f64_blendv_pd(c,b,a,pred) &
                                bind(c,name='vec4f64_blendv_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                          real(c_double), dimension(4), intent(in)  :: pred
          end subroutine vec4f64_blendv_pd
    end interface
    
    interface
          subroutine vec4f64_ceil_pd(c,a) &
                                bind(c,name='vec4f64_ceil_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_ceil_pd
    end interface
    
    interface
          subroutine vec4f64_cmpeq_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmpeq_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmpeq_pd
    end interface 
    
    interface
          subroutine vec4f64_cmpneq_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmpneq_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmpneq_pd
    end interface
    
    interface
           subroutine vec4f64_cmplt_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmplt_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmplt_pd
    end interface
    
    interface
          subroutine vec4f64_cmple_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmple_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmple_pd
    end interface
    
    interface
           subroutine vec4f64_cmpgt_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmpgt_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmpgt_pd
    end interface 
    
    interface
          subroutine vec4f64_cmpge_pd(c,b,a) &
                                 bind(c,name='vec4f64_cmpge_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
                         
          end subroutine vec4f64_cmpge_pd
    end interface
    
    interface
          subroutine vec4f64_floor_pd(c,a) &
                                  bind(c,name='vec4f64_floor_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_floor_pd
    end interface
    
    interface
          subroutine vec4f64_fmadd_pd(d,c,b,a) &
                                bind(c,name='vec4f64_fmadd_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: d
                          real(c_double), dimension(4), intent(in)  :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_fmadd_pd
    end interface
    
    interface
          subroutine vec4f64_fmaddsub_pd(d,c,b,a) &
                                 bind(c,name='vec4f64_fmaddsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: d
                          real(c_double), dimension(4), intent(in)  :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_fmaddsub_pd
    end interface
    
    interface
          subroutine vec4f64_fmsub_pd(d,c,b,a) &
                                bind(c,name='vec4f64_fmsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: d
                          real(c_double), dimension(4), intent(in)  :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_fmsub_pd
    end interface
    
    interface
          subroutine vec4f64_fnmadd_pd(d,c,b,a) &
                                bind(c,name='vec4f64_fnmadd_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: d
                          real(c_double), dimension(4), intent(in)  :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_fnmadd_pd
    end interface
    
    interface
          subroutine vec4f64_fnmsub_pd(d,c,b,a) &
                                 bind(c,name='vec4f64_fnmsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: d
                          real(c_double), dimension(4), intent(in)  :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_fnmsub_pd
    end interface
    
    interface
          subroutine vec4f64_hadd_pd(c,b,a)  &
                               bind(c,name='vec4f64_hadd_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_hadd_pd
    end interface
    
    interface
          subroutine vec4f64_hsub_pd(c,b,a) &
                               bind(c,name='vec4f64_hsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_hsub_pd
    end interface
    
    interface
          subroutine vec4f64_max_pd(c,b,a) &
                               bind(c,name='vec4f64_hsub_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_max_pd
    end interface
    
    interface
          subroutine vec4f64_min_pd(c,b,a) &
                               bind(c,name='vec4f64_min_pd')
                          use, intrinsic :: ISO_C_BINDING
                          real(c_double), dimension(4), intent(out) :: c
                          real(c_double), dimension(4), intent(in)  :: b
                          real(c_double), dimension(4), intent(in)  :: a 
          end subroutine vec4f64_min_pd
    end interface
    
    interface
          subroutine vec4f64_movedup_pd(b,a) &
                               bind(c,name='vec4f64_movedup_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(4), intent(out) :: b
                            real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_movedup_pd
    end interface
    
    interface
          subroutine vec4f64_movemask_pd(a,imm) &
                                bind(c,name='vec4f64_movemask_pd')
                             use, intrinsic :: ISO_C_BINDING
                             real(c_double), dimension(4), intent(in)   :: a
                             integer(c_int),               intent(out)  :: imm
          end subroutine vec4f64_movemask_pd
    end interface
    
    interface
          subroutine vec4f64_or_pd(c,b,a)   &
                                bind(c,name='vec4f64_or_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: c
                              real(c_double), dimension(4), intent(in)  :: b
                              real(c_double), dimension(4), intent(in)  :: a  
          end subroutine vec4f64_or_pd
    end interface
    
    interface
           subroutine vec4f64_permute2f128_pd(c,b,a,imm)   &
                                 bind(c,name='vec4f64_permute2f128_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: c
                              real(c_double), dimension(4), intent(in)  :: b
                              real(c_double), dimension(4), intent(in)  :: a 
                              integer(c_int),               intent(in), value :: imm
           end subroutine vec4f64_permute2f128_pd
    end interface
    
    interface
          subroutine vec4f64_cvtpd_ps(b,a) &
                                 bind(c,name='vec4f64_cvtpd_ps')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_float),   dimension(4), intent(out) :: b
                              real(c_double),  dimension(4), intent(in)  :: a
          end subroutine vec4f64_cvtpd_ps
    end interface
    
    interface
          subroutine vec4f64_cvtps_pd(b,a) &
                                 bind(c,name='vec4f64_cvtps_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_float),  dimension(4), intent(in)  :: a
          end subroutine vec4f64_cvtps_pd
    end interface
    
    interface
          subroutine vec4f64_sqrt_pd(b,a)  &
                                bind(c,name='vec4f64_sqrt_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(4), intent(out) :: b
                            real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_sqrt_pd
    end interface
    
    interface
          subroutine vec4f64_undefined_pd(a) &
                                bind(c,name='vec4f64_undefined_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(4), intent(out) :: a
          end subroutine vec4f64_undefined_pd
    end interface
    
    interface
          subroutine vec_zeroall()  &
                              bind(c,name='vec_zeroall')
          end subroutine vec_zeroall
    end interface 
    
    interface
          subroutine vec_zeroupper()  &
                                bind(c,name='vec_zeroupper')
          end subroutine vec_zeroupper
    end interface 
    
    interface
          subroutine vec4f64_broadcast_pd(b,a) &
                                bind(c,name='vecf64_broadcast_pd')
                           use, intrinsic :: ISO_C_BINDING
                           real(c_double),  dimension(4), intent(out) :: b
                           real(c_double),  dimension(2), intent(in)  :: a
          end subroutine vec4f64_broadcast_pd
    end interface
    
    interface
          subroutine vec4f64_broadcast_sd(b,a) &
                               bind(c,name='vec4f64_broadcast_sd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(4), intent(out) :: b
                            real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_broadcast_sd
    end interface
    
    interface
          subroutine vec4f64_testc_pd(c,b,a) &
                                bind(c,name='vec4f64_testc_pd')
                             use, intrinsic :: ISO_C_BINDING
                             integer(c_int),                intent(out) :: c
                             real(c_double),  dimension(4), intent(in)  :: b
                             real(c_double),  dimension(4), intent(in)  :: a
          end subroutine vec4f64_testc_pd
    end interface
    
    interface
          subroutine vecf64_testnzc_pd(c,b,a) &
                                 bind(c,name='vecf64_testnzc_pd')
                            use, intrinsic :: ISO_C_BINDING
                            integer(c_int),               intent(out) :: c
                            real(c_double), dimension(4), intent(in)  :: b
                            real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_testnzc_pd
    end interface
    
    interface
          subroutine vec4f64_testz_pd(c,b,a) &
                               bind(c,name='vecf64_testz_pd')
                           use, intrinsic :: ISO_C_BINDING
                           integer(c_int),               intent(out) :: c
                           real(c_double), dimension(4), intent(in)  :: b
                           real(c_double), dimension(4), intent(in)  :: a
          end subroutine vec4f64_testz_pd
    end interface
    
    interface
          subroutine vecf64_round_nearest_pd(b,a) &
                                bind(c,name='vecf64_round_nearest_pd')
                             use, intrinsic :: ISO_C_BINDING
                             real(c_double), dimension(4), intent(out) :: b
                             real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_round_nearest_pd
    end interface
    
    interface
          subroutine vecf64_round_down_pd(b,a) &
                                  bind(c,name='vecf64_round_down_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_round_down_pd
    end interface
    
    interface
          subroutine vecf64_round_up_pd(b,a) &
                                  bind(c,name='vecf64_round_up_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_round_up_pd
    end interface
    
    interface
          subroutine vecf64_round_truncate_pd(b,a) &
                                   bind(c,name='vecf64_round_truncate_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_round_truncate_pd
    end interface
    
    interface
          subroutine vecf64_stream_pd(b,a)  &
                                   bind(c,name='vecf64_stream_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_double), dimension(4), intent(in)  :: a
          end subroutine  vecf64_stream_pd
    end interface
    
    interface
          subroutine vecf64_unpacklo_pd(c,b,a) &
                                    bind(c,name='vecf64_unpacklo_pd')
                                use, intrinsic :: ISO_C_BINDING
                                real(c_double), dimension(4), intent(out) :: c
                                real(c_double), dimension(4), intent(in)  :: b
                                real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_unpacklo_pd
    end interface
    
    interface
          subroutine vecf64_unpackhi_pd(c,b,a) &
                                    bind(c,name='vecf64_unpackhi_pd')
                                use, intrinsic :: ISO_C_BINDING
                                real(c_double), dimension(4), intent(out) :: c
                                real(c_double), dimension(4), intent(in)  :: b
                                real(c_double), dimension(4), intent(in)  :: a
          end subroutine vecf64_unpackhi_pd
    end interface
    
    interface
          subroutine vec2f64_acos_pd(b,a) &
                                    bind(c,name='vec2f64_acos_pd')
                                use, intrinsic :: ISO_C_BINDING
                                real(c_double), dimension(2), intent(out) :: b
                                real(c_double), dimension(2), intent(in)  :: a
          end subroutine vec2f64_acos_pd
    end interface
    
    interface
         subroutine vec4f64_acos_pd(b,a)  &
                                  bind(c,name='vec4f64_acos_pd')
                              use, intrinsic :: ISO_C_BINDING
                              real(c_double), dimension(4), intent(out) :: b
                              real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_acos_pd
    end interface
    
    interface
         subroutine vec2f64_acosh_pd(b,a) &
                                 bind(c,name='vec2f64_acosh_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_acosh_pd
    end interface 
    
    interface
         subroutine vec4f64_acosh_pd(b,a) &
                                  bind(c,name='vec4f64_acosh_pd')
                             use, intrinsic :: ISO_C_BINDING
                             real(c_double), dimension(4), intent(out) :: b
                             real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_acosh_pd
    end interface
    
    interface
         subroutine vec2f64_asin_pd(b,a)  &
                                  bind(c,name='vec2f64_asin_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_asin_pd
    end interface
    
    interface
         subroutine vec4f64_asin_pd(b,a) &
                                 bind(c,name='vec4f64_asin_pd')
                             use, intrinsic :: ISO_C_BINDING
                             real(c_double), dimension(4), intent(out) :: b
                             real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_asin_pd
    end interface
    
    interface
         subroutine vec2f64_asinh_pd(b,a) &
                                  bind(c,name='vec2f64_asinh_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_asinh_pd
    end interface
    
    interface
         subroutine vec4f64_asinh_pd(b,a) &
                                 bind(c,name='vec4f64_asinh_pd')
                             use, intrinsic :: ISO_C_BINDING
                             real(c_double), dimension(4), intent(out) :: b
                             real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_asinh_pd
    end interface
    
    interface
         subroutine vec2f64_atan2_pd(c,b,a)  &
                                bind(c,name='vec2f64_atan2_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: c
                            real(c_double), dimension(2), intent(in)  :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_atan2_pd
    end interface
    
    interface
         subroutine vec4f64_atan2_pd(c,b,a) &
                                bind(c,name='vec4f64_atan2_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(4), intent(out) :: c
                            real(c_double), dimension(4), intent(in)  :: b
                            real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_atan2_pd
    end interface
    
    interface
         subroutine vec2f64_atan_pd(b,a)  &
                               bind(c,name='vec2f64_atan_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_atan_pd
    end interface
    
    interface
         subroutine vec4f64_atan_pd(b,a)  &
                              bind(c,name='vec4f64_atan_pd')
                           use, intrinsic :: ISO_C_BINDING
                           real(c_double), dimension(4), intent(out) :: b
                           real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_atan_pd
    end interface
    
    interface
         subroutine vec2f64_atanh_pd(b,a) &
                               bind(c,name='vec2f64_atan_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_atanh_pd
    end interface
    
    interface
         subroutine vec4f64_atanh_pd(b,a) &
                               bind(c,name='vec4f64_atanh_pd')
                           use, intrinsic :: ISO_C_BINDING
                           real(c_double), dimension(4), intent(out) :: b
                           real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_atanh_pd
    end interface
    
    interface
         subroutine vec2f64_cbrt_pd(b,a) &
                                bind(c,name='vec2f64_cbrt_pd')
                            use, intrinsic :: ISO_C_BINDING
                            real(c_double), dimension(2), intent(out) :: b
                            real(c_double), dimension(2), intent(in)  :: a
         end subroutine vec2f64_cbrt_pd
    end interface
    
    interface
         subroutine vec4f64_cbrt_pd(b,a) &
                               bind(c,name='vec4f64_cbrt_pd')
                           use, intrinsic :: ISO_C_BINDING
                           real(c_double), dimension(4), intent(out) :: b
                           real(c_double), dimension(4), intent(in)  :: a
         end subroutine vec4f64_cbrt_pd
    end interface
    
    
    
    

end module mod_avx_bindings