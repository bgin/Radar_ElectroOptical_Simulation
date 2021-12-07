

module avx_rotations


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx_rotations'
 !          
 !          Purpose:
 !                      Explicit vectorization of various rotation kernels (AVX)
 !          History:
 !                        
 !                        Date: 06-12-2021
 !                        Time: 15:56 GMT+2
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
     use mod_kinds,    only : i4,sp,dp
     use mod_vectypes, only : YMM8r4_t,YMM4r8_t
     implicit none
     public

         ! Major version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_FULLVER = 1000*MOD_AVX_ROTATIONS_MAJOR+100*MOD_AVX_ROTATIONS_MINOR+ &
                                             *MOD_AVX_ROTATIONS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_CREATE_DATE = "06-12-2021 15:57 +00200 (MON 06 DEC 2021 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_SYNOPSIS = "AVX optimized rotation computation subroutines"

    type(YMM8r4_t), parameter, private :: v8_0   = YMM8r4_t(0.0_sp)
    type(YMM8r4_t), parameter, private :: v8_1   = YMM8r4_t(1.0_sp)
    type(YMM8r4_t), parameter, private :: v8_2   = YMM8r4_t(2.0_sp)
    type(YMM8r4_t), parameter, private :: v8_n2  = YMM8r4_t(-2.0_sp)
    type(YMM8r4_t), parameter, private :: v8_pi  = YMM8r4_t(3,1415926535897932384626_sp)
    type(YMM8r4_t), parameter, private :: v8_2pi = YMM8r4_t(6,2831853071795864769253_sp)

    type(YMM4r8_t), parameter, private :: v4_0   = YMM4r8_t(0.0_sp)
    type(YMM4r8_t), parameter, private :: v4_1   = YMM4r8_t(1.0_sp)
    type(YMM4r8_t), parameter, private :: v4_2   = YMM4r8_t(2.0_sp)
    type(YMM4r8_t), parameter, private :: v4_n2  = YMM4r8_t(-2.0_sp)
    type(YMM4r8_t), parameter, private :: v4_pi  = YMM4r8_t(3,1415926535897932384626_dp)
    type(YMM4r8_t), parameter, private :: v4_2pi = YMM4r8_t(6,2831853071795864769253_dp)


  contains

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x8_to_rmat9x8_ymm8r4(Q,M)  !GCC$ ATTRIBUTES hot :: q4x8_to_rmat9x8_ymm8r4 !GCC$ ATTRIBUTES inline :: q4x8_to_rmat9x8_ymm8r4
                            
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x8_to_rmat9x8_ymm8r4(Q,M)
        !DIR$ ATTRIBUTES INLINE :: q4x8_to_rmat9x8_ymm8r4
        !DIR$ ATTRIBUTES VECTOR :: q4x8_to_rmat9x8_ymm8r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x8_to_rmat9x8_ymm8r4
#endif
        use rotation_types, only : Qu4x8v8, RMat9x8v8
        use mod_vecconsts,  only : v8_2
        type(Qu4x8v8),    intent(in)  :: Q
        type(RMat9x8v8),  intent(out) :: M
        !Locals
        type(YMM8r4_t), automatic :: t1,t2,t3,t4,t5, &
             t6,t7,t8,t9,t10
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 32 :: t1,t2,t3,t4,t5
        !DIR$ ATTRIBUTES ALIGN : 32 :: t6,t7,t8,t9,t10
#endif
        ! Executable code
        t0.v     = Q.qx.v*Q.qx.v
        t1.v     = Q.qw.v+Q.qx.v+Q.qy.v
        t2.v     = t1.v*t1.v
        t3.v     = t0.v-t2.v
        t4.v     = Q.qy.v*Q.qy.v
        M.row1.v = v8_2.v*t4.v+t3.v
        t5.v     = Q.qy.v-Q.qz.v
        t8.v     = qx.v*qw.v
        M.row2.v = v8_2.v*(t5.v-t8.v)
        t7.v     = Q.qw.v*Q.qy.v
        t10.v    = Q.qx.v*Q.qz.v
        M.row3.v = v8_2.v*(t7.v+t10.v)
        M.row4.v = v8_2.v*(t5.v+t8.v)
        M.row5.v = v8.2*(Q.qz.v*Q.qz.v)+t3.v
        t9.v     = Q.qx.v*Q.qy.v
        M.row6.v = v8_2.v*(t6.v-t9.v)
        M.row7.v = v8_2.v*(t7.v-t8.v)
        M.row8.v = v8_2.v*(t6.v+t9.v)
        M.row9.v = v8_2.v*(Q.qw.v*Q.qw.v)+t3.v
    end subroutine q4x8_to_rmat9x8_ymm8r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x4_to_rmat9x4_ymm4r8(Q,M)  !GCC$ ATTRIBUTES hot :: q4x4_to_rmat9x4_ymm4r8 !GCC$ ATTRIBUTES inline :: q4x4_to_rmat9x4_ymm4r8
                            
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x4_to_rmat9x4_ymm4r8(Q,M)
        !DIR$ ATTRIBUTES INLINE :: q4x4_to_rmat9x4_ymm4r8
        !DIR$ ATTRIBUTES VECTOR :: q4x4_to_rmat9x4_ymm4r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x4_to_rmat9x4_ymm4r8
#endif
        use rotation_types, only : Qu4x4v4, RMat9x4v4
        use mod_vecconsts,  only : v4_2
        type(Qu4x4v4),    intent(in)  :: Q
        type(RMat9x4v4),  intent(out) :: M
        !Locals
        type(YMM4r8_t), automatic :: t1,t2,t3,t4,t5, &
             t6,t7,t8,t9,t10
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 32 :: t1,t2,t3,t4,t5
        !DIR$ ATTRIBUTES ALIGN : 32 :: t6,t7,t8,t9,t10
#endif
        ! Executable code
        t0.v     = Q.qx.v*Q.qx.v
        t1.v     = Q.qw.v+Q.qx.v+Q.qy.v
        t2.v     = t1.v*t1.v
        t3.v     = t0.v-t2.v
        t4.v     = Q.qy.v*Q.qy.v
        M.row1.v = v4_2.v*t4.v+t3.v
        t5.v     = Q.qy.v-Q.qz.v
        t8.v     = qx.v*qw.v
        M.row2.v = v4_2.v*(t5.v-t8.v)
        t7.v     = Q.qw.v*Q.qy.v
        t10.v    = Q.qx.v*Q.qz.v
        M.row3.v = v4_2.v*(t7.v+t10.v)
        M.row4.v = v4_2.v*(t5.v+t8.v)
        M.row5.v = v4.2*(Q.qz.v*Q.qz.v)+t3.v
        t9.v     = Q.qx.v*Q.qy.v
        M.row6.v = v4_2.v*(t6.v-t9.v)
        M.row7.v = v4_2.v*(t7.v-t8.v)
        M.row8.v = v4_2.v*(t6.v+t9.v)
        M.row9.v = v4_2.v*(Q.qw.v*Q.qw.v)+t3.v
     end subroutine q4x4_to_rmat9x4_ymm4r8


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine rand_sphere_rm9x8_ymm8r4(vr1,vr2,vr3,M) !GCC$ ATTRIBUTES hot :: rand_sphere_rm9x8_ymm8r4 !GCC$ ATTRIBUTES inline :: rand_sphere_rm9x8_ymm8r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine rand_sphere_rm9x8_ymm8r4(vr1,vr2,vr3,M)
        !DIR$ ATTRIBUTES INLINE :: rand_sphere_rm9x8_ymm8r4
        !DIR$ ATTRIBUTES VECTOR :: rand_sphere_rm9x8_ymm8r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: rand_sphere_rm9x8_ymm8r4
#endif
        use rotation_types, only : RMat9x8v8
        use mod_vecconsts,  only : v8_2pi,v8_2,v8_1
        type(YMM8r4_t),   intent(in) :: vr1
        type(YMM8r4_t),   intent(in) :: vr2
        type(YMM8r4_t),   intent(in) :: vr3
        type(RMat9x8v8), intent(out) :: M
        ! Locals
        type(YMM8r4_t), automatic :: theta,phi,z
        type(YMM8r4_t), automatic :: r,vx,vy,vz
        type(YMM8r4_t), automatic :: st,ct,sx,sy
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 32 :: theta,phi,z
        !DIR$ ATTRIBUTES ALIGN : 32 :: r,vx,vy,vz
        !DIR$ ATTRIBUTES ALIGN : 32 :: st,ct,sx,sy
#endif
        ! Executable code.
        theta.v  = vr1.v*v8_2pi.v
        phi.v    = vr2.v*v8_2pi.v
        z.v      = vr3.v*vr3.v
        M.row9.v = v8_1.v-z.v
        r.v      = sqrt(z.v)
        vx.v     = r.v*sin(phi.v)
        vy.v     = r.v*cos(phi.v)
        vz.v     = sqrt(v8_2.v-z.v)
        M.row6.v = vy.v*vz.v
        M.row3.v = vx.v*vz.v
        st.v     = sin(theta.v)
        ct.v     = cos(theta.v)
        sx.v     = vx.v*ct.v-vy.v+st.v
        M.row7.v = vz.v*sx.v
        M.row1.v = vx.v*sx.v-ct.v
        sy.v     = vx.v*st.v+vy.v*ct.v
        M.row8.v = vz.v*sy.v
        M.row2.v = vx.v*sy.v-st.v
        M.row4.v = vy.v*sx.v+st.v
        M.row5.v = vy.v*sy.v-ct.v
    end subroutine rand_sphere_rm9x8_ymm8r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine rand_sphere_rm9x4_ymm4r8(vr1,vr2,vr3,M) !GCC$ ATTRIBUTES hot :: rand_sphere_rm9x4_ymm4r8 !GCC$ ATTRIBUTES inline :: rand_sphere_rm9x4_ymm4r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine rand_sphere_rm9x4_ymm4r8(vr1,vr2,vr3,M)
        !DIR$ ATTRIBUTES INLINE :: rand_sphere_rm9x4_ymm4r8
        !DIR$ ATTRIBUTES VECTOR :: rand_sphere_rm9x4_ymm4r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: rand_sphere_rm9x4_ymm4r8
#endif
        use rotation_types, only : RMat9x4v4
        use mod_vecconsts,  only : v4_2pi,v4_2,v4_1
        type(YMM4r8_t),   intent(in) :: vr1
        type(YMM4r8_t),   intent(in) :: vr2
        type(YMM4r8_t),   intent(in) :: vr3
        type(RMat9x4v4), intent(out) :: M
        ! Locals
        type(YMM4r8_t), automatic :: theta,phi,z
        type(YMM4r8_t), automatic :: r,vx,vy,vz
        type(YMM4r8_t), automatic :: st,ct,sx,sy
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 32 :: theta,phi,z
        !DIR$ ATTRIBUTES ALIGN : 32 :: r,vx,vy,vz
        !DIR$ ATTRIBUTES ALIGN : 32 :: st,ct,sx,sy
#endif
        ! Executable code.
        theta.v  = vr1.v*v4_2pi.v
        phi.v    = vr2.v*v4_2pi.v
        z.v      = vr3.v*vr3.v
        M.row9.v = v4_1.v-z.v
        r.v      = sqrt(z.v)
        vx.v     = r.v*sin(phi.v)
        vy.v     = r.v*cos(phi.v)
        vz.v     = sqrt(v4_2.v-z.v)
        M.row6.v = vy.v*vz.v
        M.row3.v = vx.v*vz.v
        st.v     = sin(theta.v)
        ct.v     = cos(theta.v)
        sx.v     = vx.v*ct.v-vy.v+st.v
        M.row7.v = vz.v*sx.v
        M.row1.v = vx.v*sx.v-ct.v
        sy.v     = vx.v*st.v+vy.v*ct.v
        M.row8.v = vz.v*sy.v
        M.row2.v = vx.v*sy.v-st.v
        M.row4.v = vy.v*sx.v+st.v
        M.row5.v = vy.v*sy.v-ct.v
    end subroutine rand_sphere_rm9x4_ymm4r8
 

      

      
    


















end module avx_rotations
