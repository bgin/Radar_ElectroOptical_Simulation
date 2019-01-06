
module mod_sse_matrix

    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_sse_matrix'
 !          
 !          Purpose:
 !                    Fortran bindings to small SSE  matrix library.
 !                   
 !                     
 !          History:
 !                        Date: 30-12-2018
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
    integer(kind=int4), parameter, public :: MOD_SSE_MATRIX_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_SSE_MATRIX_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_SSE_MATRIX_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_SSE_MATRIX_FULLVER = 1000_int4*MOD_SSE_MATRIX_MAJOR + &
                                                                      100_int4*MOD_SSE_MATRIX_MINOR  + &
                                                                      10_int4*MOD_SSE_MATRIX_MICRO
    
    character(*),       parameter, public :: MOD_SSE_MATRIX_CREATE_DATE = "30-12-2018 16:42 +00200 (SUN 30 DEC 2018 GMT+2)"
    
    character(*),       parameter, public :: MOD_SSE_MATRIX_BUILD_DATE = "00-00-0000 00:00"
    
    character(*),       parameter, public :: MOD_SSE_MATRIX_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    character(*),       parameter, public :: MOD_SSE_MATRIX_SYNOPSIS = " Fortran bindings to small SSE  matrix library."
    
    ! Interoperable derived types.
    
    type, bind(c), public :: M4x4f32
          real(c_float), dimension(0:3) :: row0
          real(c_float), dimension(0:3) :: row1
          real(c_float), dimension(0:3) :: row2
          real(c_float), dimension(0:3) :: row3
    end type M4x4f32
    
    type, bind(c), public :: V1x4f32
          real(c_float), dimension(0:3) :: v
    end type V1x4f32
    
    type, bind(c), public :: V1x3f32
           real(c_float), dimension(0:3) :: v
    end type V1x3f32
    
    ! Interfaces
    
    interface
        subroutine M4x4f32_setzero(m)  &
                           bind(c,name='M4x4f32_setzero')
                   import :: M4x4f32
                   type(M4x4f32), intent(inout) :: m
        end subroutine M4x4f32_setzero
    end interface
    
    interface
        subroutine M4x4f32_setzero_stream(m) &
                            bind(c,name='M4x4f32_setzero_stream')
                   import :: M4x4f32
                   type(M4x4f32), intent(inout) :: m
        end subroutine M4x4f32_setzero_stream
    end interface
    
    interface
        subroutine M4x4f32_set1(m,s) &
                            bind(c,name='M4x4f32_set1')
              use, intrinsic :: ISO_C_BINDING
              import :: M4x4f32
              type(M4x4f32), intent(inout)     :: m
              real(c_float), intent(in), value :: s
        end subroutine M4x4f32_set1
    end interface
    
    interface
        subroutine M4x4f32_set1_stream(m,s) &
                            bind(c,name='M4x4f32_set1_stream')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)     :: m
                real(c_float), intent(in), value :: s
        end subroutine M4x4f32_set1_stream
    end interface
    
    interface
         subroutine M4x4f32_set_scalars(m,             &
                                        s0,s1,s2,s3,   &
                                        s4,s5,s6,s7,   &
                                        s8,s9,s10,s11, &
                                        s12,s13,s14,s15 )   &  
                                bind(c,name='M4x4f32_set_scalars')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)     :: m
                real(c_float), intent(in), value :: s0,s1,s2,s3,  &
                                                    s4,s5,s6,s7,   &
                                                    s8,s9,s10,s11, &
                                                    s12,s13,s14,s15
         end subroutine M4x4f32_set_scalars
    end interface
    
    interface
         subroutine M4x4f32_set_scalars_stream(m,       &
                                               s0,s1,s2,s3,   &
                                               s4,s5,s6,s7,   &
                                               s8,s9,s10,s11, &
                                               s12,s13,s14,s15 )   &
                                 bind(c,name='M4x4f32_set_scalars_stream')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)     :: m
                real(c_float), intent(in), value :: s0,s1,s2,s3,  &
                                                    s4,s5,s6,s7,   &
                                                    s8,s9,s10,s11, &
                                                    s12,s13,s14,s15
         end subroutine M4x4f32_set_scalars_stream
    end interface
    
    interface
         subroutine M4x4f32_set_arrays(m,r0,r1,r2,r3) &
                                 bind(c,name='M4x4f32_set_arrays')
                 use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)              :: m
                real(c_float), dimension(0:3), intent(in) :: r0
                real(c_float), dimension(0:3), intent(in) :: r1
                real(c_float), dimension(0:3), intent(in) :: r2
                real(c_float), dimension(0:3), intent(in) :: r3
         end subroutine M4x4f32_set_arrays
    end interface
    
    interface
        subroutine M4x4f32_set_arrays_stream(m,r0,r1,r2,r3) &
                                 bind(c,name='M4x4f32_set_arrays_stream')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)              :: m
                real(c_float), dimension(0:3), intent(in) :: r0
                real(c_float), dimension(0:3), intent(in) :: r1
                real(c_float), dimension(0:3), intent(in) :: r2
                real(c_float), dimension(0:3), intent(in) :: r3
        end subroutine M4x4f32_set_arrays_stream
    end interface
    
    interface
        subroutine M4x4f32_set_array(m,r)  &
                                bind(c,name='M4x4f32_set_array')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32),                  intent(inout)  :: m
                real(c_float), dimension(0:15), intent(in)     :: r
        end subroutine M4x4f32_set_array
    end interface
    
    interface
         subroutine M4x4f32_set_array_stream(m,r)  &
                                 bind(c,name='M4x4f32_set_array_stream')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32),                  intent(inout)  :: m
                real(c_float), dimension(0:15), intent(in)     :: r
         end subroutine M4x4f32_set_array_stream
    end interface
    
    interface
        subroutine M4x4f32_set_M4x4f32(m0,m1)  &
                                  bind(c,name='M4x4f32_set_M4x4f32')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: m0
                type(M4x4f32), intent(in)    :: m1
        end subroutine M4x4f32_set_M4x4f32
    end interface
    
    interface
        subroutine M4x4f32_set_M4x4f32_stream(m0,m1) &
                                  bind(c,name='M4x4f32_set_M4x4f32_stream')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: m0
                type(M4x4f32), intent(in)    :: m1
        end subroutine M4x4f32_set_M4x4f32_stream
    end interface
    
    interface
         subroutine M4x4f32_identity_matrix(m) &
                                  bind(c,name='M4x4f32_identity_matrix')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32),  intent(inout) :: m
         end subroutine M4x4f32_identity_matrix
    end interface
    
    interface
         subroutine M4x4f32_add_M4x4f32(mc,mb,ma) &
                                  bind(c,name='M4x4f32_add_M4x4f32')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mc
                type(M4x4f32), intent(in)    :: mb
                type(M4x4f32), intent(in)    :: ma
         end subroutine M4x4f32_add_M4x4f32
    end interface
    
    interface
         subroutine M4x4f32_add_M4x4f32_inplace(mb,ma) &
                                   bind(c,name='M4x4f32_add_M4x4f32_inplace')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mb
                type(M4x4f32), intent(in)    :: ma
         end subroutine M4x4f32_add_M4x4f32_inplace
    end interface
    
    interface
         subroutine M4x4f32_sub_M4x4f32(mc,mb,ma) &
                                    bind(c,name='M4x4f32_sub_M4x4f32')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mc
                type(M4x4f32), intent(in)    :: mb
                type(M4x4f32), intent(in)    :: ma
         end subroutine M4x4f32_sub_M4x4f32
    end interface
    
    interface
         subroutine M4x4f32_sub_M4x4f32_inplace(mb,ma) &
                                    bind(c,name='M4x4f32_sub_M4x4f32_inplace')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mb
                type(M4x4f32), intent(in)    :: ma
         end subroutine M4x4f32_sub_M4x4f32_inplace
    end interface
    
    interface
         subroutine M4x4f32_mul_M4x4f32(mc,mb,ma) &
                                    bind(c,name='M4x4f32_mul_M4x4f32')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mc
                type(M4x4f32), intent(in)    :: mb
                type(M4x4f32), intent(in)    :: ma
         end subroutine M4x4f32_mul_M4x4f32
    end interface
    
    interface
          subroutine M4x4f32_mul_M4x4f32_inplace(mb,ma) &
                                     bind(c,name='M4x4f32_mul_M4x4f32_inplace')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout) :: mb
                type(M4x4f32), intent(in)    :: ma
          end subroutine M4x4f32_mul_M4x4f32_inplace
    end interface
    
    interface
         subroutine M4x4f32_mul_scalar(mb,ma,s) &
                                     bind(c,name='M4x4f32_mul_scalar')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)     :: mb
                type(M4x4f32), intent(in)        :: ma
                real(c_float), intent(in), value :: s
         end subroutine M4x4f32_mul_scalar
    end interface
    
    interface
        subroutine M4x4f32_mul_scalar_inplace(ma,s) &
                                    bind(c,name='M4x4f32_mul_scalar_inplace')
                use, intrinsic :: ISO_C_BINDING
                import :: M4x4f32
                type(M4x4f32), intent(inout)     :: ma
                real(c_float), intent(in), value :: s
        end subroutine M4x4f32_mul_scalar_inplace
    end interface
    
    interface
        subroutine M4x4f32_negate(mb,ma) &
                                   bind(c,name='M4x4f32_negate')
                   import :: M4x4f32
                   type(M4x4f32), intent(inout) :: mb
                   type(M4x4f32), intent(in)    :: ma
        end subroutine M4x4f32_negate
    end interface
    
    interface
        subroutine M4x4f32_transpose(m) &
                                  bind(c,name='M4x4f32_transpose')
                   import :: M4x4f32
                   type(M4x4f32), intent(inout) :: m
        end subroutine M4x4f32_transpose
    end interface
    
    interface
        subroutine M4x4f32_rotateX(m,d) &
                                 bind(c,name='M4x4f32_rotateX')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: d
        end subroutine M4x4f32_rotateX
    end interface
    
    interface
        subroutine M4x4f32_rotateY(m,d) &
                                 bind(c,name='M4x4f32_rotateY')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: d
        end subroutine M4x4f32_rotateY
    end interface
    
    interface
        subroutine M4x4f32_rotateZ(m,d) &
                                  bind(c,name='M4x4f32_rotateZ')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: d
        end subroutine M4x4f32_rotateZ
    end interface
    
    interface
        subroutine M4x4f32_translate_xyz(m,x,y,z) &
                                    bind(c,name='M4x4f32_translate_xyz')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: x,y,z
        end subroutine M4x4f32_translate_xyz
    end interface 
    
    interface
        subroutine M4x4f32_scale_xyz(m,x,y,z) &
                                   bind(c,name='M4x4f32_scale_xyz')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: x,y,z
        end subroutine M4x4f32_scale_xyz
    end interface
    
    interface
        subroutine M4x4f32_scale_s(m,s) &
                                 bind(c,name='M4x4f32_scale_s')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   type(M4x4f32), intent(inout)     :: m
                   real(c_float), intent(in), value :: s
        end subroutine M4x4f32_scale_s
    end interface
    
    interface
        subroutine M4x4f32_minimum(minv,m) &
                                 bind(c,name='M4x4f32_minimum')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   real(c_float), intent(inout) :: minv
                   type(M4x4f32), intent(in)    :: m
        end subroutine M4x4f32_minimum
    end interface
    
    interface
        subroutine M4x4f32_maximum(maxv,m) &
                                 bind(c,name='M4x4f32_maximum')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   real(c_float), intent(inout) :: maxv
                   type(M4x4f32), intent(in)    :: m
        end subroutine M4x4f32_maximum
    end interface
    
    interface
        subroutine M4x4f32_det(det,m) &
                                 bind(c,name='M4x4f32_det')
                   use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   real(c_float), intent(inout) :: det
                   type(M4x4f32), intent(in)    :: m
        end subroutine M4x4f32_det
    end interface
    
    interface
         subroutine M4x4f32_inverse(inv,m) &
                                bind(c,name='M4x4f32_inverse')
                    use, intrinsic :: ISO_C_BINDING
                   import :: M4x4f32
                   real(c_float), intent(inout) :: inv
                   type(M4x4f32), intent(in)    :: m
         end subroutine M4x4f32_inverse
    end interface
    
    interface
         subroutine V1x4f32_setzero(va) &
                               bind(c,name='V1x4f32_setzero')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: va
         end subroutine V1x4f32_setzero
    end interface
    
    interface
         subroutine V1x4f32_setzero_stream(va) &
                               bind(c,name='V1x4f32_setzero_stream')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: va
         end subroutine V1x4f32_setzero_stream
    end interface
    
    interface
         subroutine V1x4f32_set1(va,s) &
                                bind(c,name='V1x4f32_set1')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32), intent(inout)     :: va
                    real(c_float), intent(in), value :: s
         end subroutine V1x4f32_set1
    end interface
    
    interface
         subroutine V1x4f32_set1_stream(va,s) &
                                bind(c,name='V1x4f32_set1_stream')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32), intent(inout)     :: va
                    real(c_float), intent(in), value :: s
         end subroutine V1x4f32_set1_stream
    end interface
    
    interface
         subroutine V1x4f32_set_scalars(va,s0,s1,s2,s3) &
                                 bind(c,name='V1x4f32_set_scalars')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32), intent(inout)     :: va
                    real(c_float), intent(in), value :: s0,s1,s2,s3
         end subroutine V1x4f32_set_scalars
    end interface
    
    interface
          subroutine V1x4f32_set_scalars_stream(va,s0,s1,s2,s3) &
                                 bind(c,name='V1x4f32_set_scalars_stream')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32), intent(inout)     :: va
                    real(c_float), intent(in), value :: s0,s1,s2,s3
         end subroutine V1x4f32_set_scalars_stream
    end interface
    
    interface
         subroutine V1x4f32_set_array(va,a) &
                                bind(c,name='V1x4f32_set_array')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32),                 intent(inout)     :: va
                    real(c_float), dimension(0:3), intent(in)        :: a
         end subroutine V1x4f32_set_array
    end interface
    
    interface
          subroutine V1x4f32_set_array_stream(va,a) &
                                bind(c,name='V1x4f32_set_array_stream')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32),                 intent(inout)     :: va
                    real(c_float), dimension(0:3), intent(in)        :: a
         end subroutine V1x4f32_set_array_stream
    end interface
    
    interface
         subroutine V1x4f32_set_V1x4f32(vb,va) &
                                bind(c,name='V1x4f32_set_V1x4f32')
                  
                    import :: V1x4f32
                    type(V1x4f32),                 intent(inout)     :: vb
                    type(V1x4f32),                 intent(in)        :: va
         end subroutine V1x4f32_set_V1x4f32
    end interface
    
    interface
         subroutine V1x4f32_set_V1x4f32_stream(vb,va) &
                                 bind(c,name='V1x4f32_set_V1x4f32_stream')
                    
                    import :: V1x4f32
                    type(V1x4f32),                 intent(inout)     :: vb
                    type(V1x4f32),                 intent(in)        :: va
         end subroutine V1x4f32_set_V1x4f32_stream
    end interface
    
    interface
         subroutine V1x4f32_set_M4x4f32(vb,ma,imm) &
                                bind(c,name='V1x4f32_set_M4x4f32')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    import :: M4x4f32
                    type(V1x4f32),  intent(inout)     :: vb
                    type(M4x4f32),  intent(in)        :: ma
                    integer(c_int), intent(in), value :: imm
         end subroutine V1x4f32_set_M4x4f32
    end interface
    
    interface
          subroutine V1x4f32_set_M4x4f32_stream(vb,ma,imm) &
                                bind(c,name='V1x4f32_set_M4x4f32_stream')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    import :: M4x4f32
                    type(V1x4f32),  intent(inout)     :: vb
                    type(M4x4f32),  intent(in)        :: ma
                    integer(c_int), intent(in), value :: imm
         end subroutine V1x4f32_set_M4x4f32_stream
    end interface
    
    interface
         subroutine V1x4f32_mul_M4x4f32(vc,vb,ma) &
                                bind(c,name='V1x4f32_mul_M4x4f32')
                    import :: V1x4f32
                    import :: M4x4f32
                    type(V1x4f32), intent(inout) :: vc
                    type(V1x4f32), intent(in)    :: vb
                    type(M4x4f32), intent(in)    :: ma
         end subroutine V1x4f32_mul_M4x4f32
    end interface
    
    interface
         subroutine V1x4f32_mul_M4x4f32_inplace(vb,ma) &
                                bind(c,name='V1x4f32_mul_M4x4f32_inplace')
                    import :: V1x4f32
                    import :: M4x4f32
                    type(V1x4f32), intent(inout) :: vb
                    type(M4x4f32), intent(in)    :: ma
         end subroutine V1x4f32_mul_M4x4f32_inplace
    end interface
    
    interface
         subroutine V1x4f32_product(vc,vb,va) &
                               bind(c,name='V1x4f32_product')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vc
                    type(V1x4f32), intent(in)    :: vb
                    type(V1x4f32), intent(in)    :: va
         end subroutine V1x4f32_product
    end interface
    
    interface
        subroutine V1x4f32_product_inplace(vb,va) &
                               bind(c,name='V1x4f32_product_inplace')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vb
                    type(V1x4f32), intent(in)    :: va
        end subroutine V1x4f32_product_inplace
    end interface
    
    interface
        subroutine V1x4f32_mul_scalar(vb,va,s) &
                                bind(c,name='V1x4f32_mul_scalar')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    type(V1x4f32), intent(inout)     :: vb
                    type(V1x4f32), intent(in)        :: va
                    real(c_float), intent(in), value :: s
        end subroutine V1x4f32_mul_scalar
    end interface
    
    interface
        subroutine V1x4f32_add_V1x4f32(vc,vb,va) &
                               bind(c,name='V1x4f32_add_V1x4f32')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vc
                    type(V1x4f32), intent(in)    :: vb
                    type(V1x4f32), intent(in)    :: va
        end subroutine V1x4f32_add_V1x4f32
    end interface
    
    interface
        subroutine V1x4f32_add_V1x4f32_inplace(vb,va) &
                               bind(c,name='V1x4f32_add_V1x4f32_inplace')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vb
                    type(V1x4f32), intent(in)    :: va
        end subroutine V1x4f32_add_V1x4f32_inplace
    end interface
    
     interface
        subroutine V1x4f32_sub_V1x4f32(vc,vb,va) &
                               bind(c,name='V1x4f32_sub_V1x4f32')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vc
                    type(V1x4f32), intent(in)    :: vb
                    type(V1x4f32), intent(in)    :: va
        end subroutine V1x4f32_sub_V1x4f32
    end interface
    
    interface
        subroutine V1x4f32_sub_V1x4f32_inplace(vb,va) &
                               bind(c,name='V1x4f32_sub_V1x4f32_inplace')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vb
                    type(V1x4f32), intent(in)    :: va
        end subroutine V1x4f32_sub_V1x4f32_inplace
    end interface
    
    interface
         subroutine V1x4f32_dot(d,vb,va) &
                            bind(c,name='V1x4f32_dot')
                    use, intrinsic :: ISO_C_BINDING
                    import :: V1x4f32
                    real(c_float), intent(inout) :: d
                    type(V1x4f32), intent(in)    :: vb,va
         end subroutine V1x4f32_dot
    end interface
    
    interface
         subroutine V1x4f32_cross(vc,vb,va) &
                            bind(c,name='V1x4f32_cross')
                    import :: V1x4f32
                    type(V1x4f32), intent(inout) :: vc
                    type(V1x4f32), intent(in)    :: vb
                    type(V1x4f32), intent(in)    :: va
         end subroutine V1x4f32_cross
    end interface
    
end module mod_sse_matrix