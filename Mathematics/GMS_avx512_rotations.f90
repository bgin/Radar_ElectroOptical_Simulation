

module avx512_rotations


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx512_rotations'
 !          
 !          Purpose:
 !                      Explicit vectorization of various rotation kernels (AVX512)
 !          History:
 !                        
 !                        Date: 28-11-2021
 !                        Time: 17:56 GMT+2
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
     use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
    

     implicit none
     
     public

     private :: norm2_zmm16r4,norm2_zmm8r8, &
                clip_zmm16r4, clip_zmm8r8


       ! Major version
    integer(kind=i4), parameter, public :: MOD_AVX512_ROTATIONS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_AVX512_ROTATIONS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_AVX512_ROTATIONS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_AVX512_ROTATIONS_FULLVER = 1000*MOD_AVX512_ROTATIONS_MAJOR+100*MOD_AVX512_ROTATIONS_MINOR+ &
                                             *MOD_AVX512_ROTATIONS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_AVX512_ROTATIONS_CREATE_DATE = "21-11-2021 13:27 +00200 ( 21 NOV 2021 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_AVX512_ROTATIONS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_AVX512_ROTATIONS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_AVX512_ROTATIONS_SYNOPSIS = "AVX512 optimized rotation computation subroutines"


    contains

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function norm2_zmm16r4(y,z,w) result(vn2)  !GCC$ ATTRIBUTES hot :: norm2_zmm16r4 !GCC$ ATTRIBUTES inline :: norm2_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function norm2_zmm16r4(y,z,w) result(vn2)
        !DIR$ ATTRIBUTES INLINE :: norm2_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: norm2_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm2_zmm16r4
#endif
          type(ZMM16r4_t),        intent(in) :: y
          type(ZMM16r4_t),        intent(in) :: z
          type(ZMM16r4_t),        intent(in) :: w
          type(ZMM16r4_t) :: vn2
          !Locals
          type(ZMM16r4_t), automatic :: t0,t1,t2,v
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2,v
#endif
          t0.v = y.v*y.v
          t1.v = z.v*z.v
          t2.v = w.v*w.v
          v.v  = t0.v+t1.v+t2.v
          vn2.v = sqrt(v.v)
      end function norm2_zmm16r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function norm2_zmm8r8(y,z,w) result(vn2)  !GCC$ ATTRIBUTES hot :: norm2_zmm8r8 !GCC$ ATTRIBUTES inline :: norm2_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function norm2_zmm8r8(y,z,w) result(vn2)
        !DIR$ ATTRIBUTES INLINE :: norm2_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: norm2_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: norm2_zmm8r8
#endif
          type(ZMM8r8_t),        intent(in) :: y
          type(ZMM8r8_t),        intent(in) :: z
          type(ZMM8r8_t),        intent(in) :: w
          type(ZMM8r8_t) :: vn2
          !Locals
          type(ZMM8r8_t), automatic :: t0,t1,t2,v
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2,v
#endif
          t0.v = y.v*y.v
          t1.v = z.v*z.v
          t2.v = w.v*w.v
          v.v  = t0.v+t1.v+t2.v
          vn2.v = sqrt(v.v)
      end function norm2_zmm8r8


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))        
      pure function clip_zmm16r4(x,lo,hi) result(res)  !GCC$ ATTRIBUTES hot :: clip_zmm16r4 !GCC$ ATTRIBUTES inline :: norm2_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)        
      pure function clip_zmm16r4(x,lo,hi) result(res)
            !DIR$ ATTRIBUTES INLINE :: clip_zmm16r4
            !DIR$ ATTRIBUTES VECTOR :: clip_zmm16r4
            !DIR$ OPTIMIZE : 3
            !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: clip_zmm16r4
#endif
          type(ZMM8r8_t),        intent(in) :: x
          type(ZMM8r8_t),        intent(in) :: lo
          type(ZMM8r8_t),        intent(in) :: hi
          type(ZMM8r8_t) :: res
          res.v = max(lo.v,min(x.v,hi.v))
       end function clip_zmm16r4

        
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))        
      pure function clip_zmm8r8(x,lo,hi) result(res)  !GCC$ ATTRIBUTES hot :: clip_zmm8r8 !GCC$ ATTRIBUTES inline :: norm2_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)        
      pure function clip_zmm8r8(x,lo,hi) result(res)
            !DIR$ ATTRIBUTES INLINE :: clip_zmm8r8
            !DIR$ ATTRIBUTES VECTOR :: clip_zmm8r8
            !DIR$ OPTIMIZE : 3
            !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: clip_zmm8r8
#endif
          type(ZMM8r8_t),        intent(in) :: x
          type(ZMM8r8_t),        intent(in) :: lo
          type(ZMM8r8_t),        intent(in) :: hi
          type(ZMM8r8_t) :: res
          res.v = max(lo.v,min(x.v,hi.v))
      end function clip_zmm8r8        
        
        

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x16_to_rmat9x16_zmm16r4(Q,M)  !GCC$ ATTRIBUTES hot :: q4x16_to_rmat9x16_zmm16r4 !GCC$ ATTRIBUTES inline :: q4x16_to_rmat9x16_zmm16r4
                            
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x16_to_rmat9x16_zmm16r4(Q,M)
        !DIR$ ATTRIBUTES INLINE :: q4x16_to_rmat9x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: q4x16_to_rmat9x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x16_to_rmat9x16_zmm16r4
#endif
        use rotation_types, only : Q4x16v16, RotM9x16v16
        use mod_vecconsts,  only : v16_2
        type(Q4x16v16),    intent(in)  :: Q
        type(RotM9x16v16), intent(out) :: M
        !Locals
        type(ZMM16r4_t), automatic :: t1,t2,t3,t4,t5, &
             t6,t7,t8,t9,t10
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: t1,t2,t3,t4,t5
        !DIR$ ATTRIBUTES ALIGN : 64 :: t6,t7,t8,t9,t10
#endif
        ! Executable code
        t0.v     = Q.qx.v*Q.qx.v
        t1.v     = Q.qw.v+Q.qx.v+Q.qy.v
        t2.v     = t1.v*t1.v
        t3.v     = t0.v-t2.v
        t4.v     = Q.qy.v*Q.qy.v
        M.row1.v = v16_2.v*t4.v+t3.v
        t5.v     = Q.qy.v-Q.qz.v
        t8.v     = qx.v*qw.v
        M.row2.v = v16_2.v*(t5.v-t8.v)
        t7.v     = Q.qw.v*Q.qy.v
        t10.v    = Q.qx.v*Q.qz.v
        M.row3.v = v16_2.v*(t7.v+t10.v)
        M.row4.v = v16_2.v*(t5.v+t8.v)
        M.row5.v = v16.2*(Q.qz.v*Q.qz.v)+t3.v
        t9.v     = Q.qx.v*Q.qy.v
        M.row6.v = v16_2.v*(t6.v-t9.v)
        M.row7.v = v16_2.v*(t7.v-t8.v)
        M.row8.v = v16_2.v*(t6.v+t9.v)
        M.row9.v = v16_2.v*(Q.qw.v*Q.qw.v)+t3.v
    end subroutine q4x16_to_rmat9x16_zmm16r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x8_to_rmat9x8_zmm8r8(Q,M)  !GCC$ ATTRIBUTES hot :: q4x8_to_rmat9x8_zmm8r8 !GCC$ ATTRIBUTES inline :: q4x8_to_rmat9x8_zmm8r8
                            
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x8_to_rmat9x8_zmm8r8(Q,M)
        !DIR$ ATTRIBUTES INLINE :: q4x8_to_rmat9x8_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: q4x8_to_rmat9x8_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x8_to_rmat9x8_zmm8r8
#endif
        use rotation_types, only : Q4x8v8, RotM9x8v8
        use mod_vecconsts,  only : v8_2
        type(Q4x8v8),    intent(in)  :: Q
        type(RotM9x8v8), intent(out) :: M
        !Locals
        type(ZMM8r8_t), automatic :: t1,t2,t3,t4,t5, &
             t6,t7,t8,t9,t10
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: t1,t2,t3,t4,t5
        !DIR$ ATTRIBUTES ALIGN : 64 :: t6,t7,t8,t9,t10
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
    end subroutine q4x8_to_rmat9x8_zmm8r8


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine rand_sphere_rm9x16_zmm16r4(vr1,vr2,vr3,M) !GCC$ ATTRIBUTES hot :: rand_sphere_rm9x16_zmm16r4 !GCC$ ATTRIBUTES inline :: rand_sphere_rm9x16_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine rand_sphere_rm9x16_zmm16r4(vr1,vr2,vr3,M)
        !DIR$ ATTRIBUTES INLINE :: rand_sphere_rm9x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: rand_sphere_rm9x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: rand_sphere_rm9x16_zmm16r4
#endif
        use rotation_types, only : RotM9x16v16
        use mod_vecconsts,  only : v16_2pi,v16_2,v16_1
        type(ZMM16r4_t),   intent(in) :: vr1
        type(ZMM16r4_t),   intent(in) :: vr2
        type(ZMM16r4_t),   intent(in) :: vr3
        type(RotM9x16v16), intent(out) :: M
        ! Locals
        type(ZMM16r4_t), automatic :: theta,phi,z
        type(ZMM16r4_t), automatic :: r,vx,vy,vz
        type(ZMM16r4_t), automatic :: st,ct,sx,sy
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: theta,phi,z
        !DIR$ ATTRIBUTES ALIGN : 64 :: r,vx,vy,vz
        !DIR$ ATTRIBUTES ALIGN : 64 :: st,ct,sx,sy
#endif
        ! Executable code.
        theta.v  = vr1.v*v16_2pi.v
        phi.v    = vr2.v*v16_2pi.v
        z.v      = vr3.v*vr3.v
        M.row9.v = v16_1.v-z.v
        r.v      = sqrt(z.v)
        vx.v     = r.v*sin(phi.v)
        vy.v     = r.v*cos(phi.v)
        vz.v     = sqrt(v16_2.v-z.v)
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
    end subroutine rand_sphere_rm9x16_zmm16r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine rand_sphere_rm9x8_zmm8r8(vr1,vr2,vr3,M) !GCC$ ATTRIBUTES hot :: rand_sphere_rm9x8_zmm8r8 !GCC$ ATTRIBUTES inline :: rand_sphere_rm9x8_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine rand_sphere_rm9x8_zmm8r8(vr1,vr2,vr3,M)
        !DIR$ ATTRIBUTES INLINE :: rand_sphere_rm9x8_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: rand_sphere_rm9x8_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: rand_sphere_rm9x8_zmm8r8
#endif
        use rotation_types, only : RotM9x8v8
        use mod_vecconsts,  only : v8_2pi,v8_2,v8_1
        type(ZMM8r8_t),   intent(in) :: vr1
        type(ZMM8r8_t),   intent(in) :: vr2
        type(ZMM8r8_t),   intent(in) :: vr3
        type(RotM9x8v8), intent(out) :: M
        ! Locals
        type(ZMM8r8_t), automatic :: theta,phi,z
        type(ZMM8r8_t), automatic :: r,vx,vy,vz
        type(ZMM8r8_t), automatic :: st,ct,sx,sy
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: theta,phi,z
        !DIR$ ATTRIBUTES ALIGN : 64 :: r,vx,vy,vz
        !DIR$ ATTRIBUTES ALIGN : 64 :: st,ct,sx,sy
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
    end subroutine rand_sphere_rm9x8_zmm8r8


    !  
    !                           /*  This algorithm generates a gaussian deviate for each coordinate, so
    !                            *  the total effect is to generate a symmetric 4-D gaussian distribution,
    !                            *  by separability. Projecting onto the surface of the hypersphere gives
    !                            *  a uniform distribution.
    !                               Based on  Ken Shoemake, September 1991 implementation.
    !                               Manually vectorized.
    !                           */
    ! 
    
    
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))    
    subroutine urand_q4x16_zmm16r4(vrx,vry,vrz,vrw,Q) !GCC$ ATTRIBUTES hot :: urand_q4x16_zmm16r4 !GCC$ ATTRIBUTES inline :: urand_q4x16_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine urand_q4x16_zmm16r4(vrx,vry,vrz,vrw,Q)
        !DIR$ ATTRIBUTES INLINE :: urand_q4x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: urand_q4x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: urand_q4x16_zmm16r4
#endif
       use rotation_types, only : Q4x16v16
       use mod_vecconsts,  only : v16_1,v16_n2
       type(ZMM16r4_t),  intent(in)  :: vrx
       type(ZMM16r4_t),  intent(in)  :: vry
       type(ZMM16r4_t),  intent(in)  :: vrz
       type(ZMM16r4_t),  intent(in)  :: vrw
       type(Q4x16v16),   intent(out) :: Q
       ! Locals
       type(ZMM16r4_t), automatic :: s1,num1,s2,num2,t0
       type(ZMM16r4_t), automatic :: r,invr,root1,root2
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES ALIGN : 64 :: s1,num1,s2,num2,t0
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,invr,root1,root2
#endif
       ! Executable code
       s1.v     = vrx.v*vrx.v+vry.v*vry.v
       num1.v   = v16_n2.v*log(s1.v)
       s2.v     = vrz.v*vrz.v+vrw.v*vrw.v
       num2.v   = v16_n2.v*log(s2.v)
       r.v      = num1.v+num2.v
       invr     = v16_1.v/r.v
       t0.v     = num1.v/s1.v
       root1.v  = sqrt(invr.v*t0.v)
       Q.qx.v   = vrx.v*root1.v
       Q.qy.v   = vry.v*root1.v
       t0.v     = num2.v/s2.v
       root2.v  = sqrt(invr.v*t0.v)
       Q.qz.v   = vrz.v*root2.v
       Q.qw.v   = vrw.v*root2.v
    end subroutine urand_q4x16_zmm16r4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))    
    subroutine urand_q4x8_zmm8r8(vrx,vry,vrz,vrw,Q) !GCC$ ATTRIBUTES hot :: urand_q4x8_zmm8r8 !GCC$ ATTRIBUTES inline :: urand_q4x8_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine urand_q4x8_zmm8r8(vrx,vry,vrz,vrw,Q)
        !DIR$ ATTRIBUTES INLINE :: urand_q4x8_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: urand_q4x8_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: urand_q4x8_zmm8r8
#endif
       use rotation_types, only : Q4x8v8
       use mod_vecconsts,  only : v8_1,v8_n2
       type(ZMM8r8_t),  intent(in)  :: vrx
       type(ZMM8r8_t),  intent(in)  :: vry
       type(ZMM8r8_t),  intent(in)  :: vrz
       type(ZMM8r8_t),  intent(in)  :: vrw
       type(Q4x8v8),    intent(out) :: Q
       ! Locals
       type(ZMM8r8_t), automatic :: s1,num1,s2,num2,t0
       type(ZMM8r8_t), automatic :: r,invr,root1,root2
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES ALIGN : 64 :: s1,num1,s2,num2,t0
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,invr,root1,root2
#endif
       ! Executable code
       s1.v     = vrx.v*vrx.v+vry.v*vry.v
       num1.v   = v16_n2.v*log(s1.v)
       s2.v     = vrz.v*vrz.v+vrw.v*vrw.v
       num2.v   = v16_n2.v*log(s2.v)
       r.v      = num1.v+num2.v
       invr     = v16_1.v/r.v
       t0.v     = num1.v/s1.v
       root1.v  = sqrt(invr.v*t0.v)
       Q.qx.v   = vrx.v*root1.v
       Q.qy.v   = vry.v*root1.v
       t0.v.v   = num2.v/s2.v
       root2.v  = sqrt(invr.v*t0.v)
       Q.qz.v   = vrz.v*root2.v
       Q.qw.v   = vrw.v*root2.v
    end subroutine urand_q4x8_zmm8r8

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x16_to_ea3x16_zmm16r4(Q,EA)  !GCC$ ATTRIBUTES hot :: q4x16_to_ea3x16_zmm16r4 !GCC$ ATTRIBUTES inline :: q4x16_to_ea3x16_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x16_to_ea3x16_zmm16r4(Q,EA)
        !DIR$ ATTRIBUTES INLINE :: q4x16_to_ea3x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: q4x16_to_ea3x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x16_to_ea3x16_zmm16r4
#endif
        use rotation_types, only : Q4x16v16,EA3x16v16
        use mod_vectypes,   only : Mask16_t
        use mod_vecconsts,  only : v16_n1,v16_2,v16_2pi,v16_pi
        type(Q4x16v16),  intent(in)  :: Q
        type(EA3x16v16), intent(out) :: EA
        ! Locals
        type(ZMM16r4_t), automatic :: qxw,qyz,chi
        type(ZMM16r4_t), automatic :: t0,c0,t1,c1,c2
        type(ZMM16r4_t), automatic :: tmp0,tmp1,tmp2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: qxw,qyz,chi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,c0,t1,c1,c2
        !DIR$ ATTRIBUTES ALIGN : 64 :: tmp0,tmp1,tmp2
#endif
        type(Mask16_t), automatic :: k1,k2
        qxw.v = Q.qx.v*Q.qx.v+Q.qw.v*Q.qw.v
        qyz.v = Q.qy.v*Q.qy.v+Q.qz.v*Q.qz.v
        chi.v = sqrt(qxw.v*qyz.v)
        k1.m  = qyz.v==v16_0.v
        k2.m  = qxw.v==v16_0.v
        if(all(k1.m)) then
           t0.v = v16_n2.v*Q.qx.v*Q.qw.v
           t1.v = Q.qx.v*Q.qx.v-Q.qw.v*Q.qw.v
           EA.alpha.v = atan2(t0.v,t1.v)
           EA.beta.v  = v16_0.v
           EA.gamma.v = v16_0.v
        else if(all(k2.m)) then
           t0.v = v16_2.v*Q.qy.v*Q.qz.v
           t1.v = Q.qy.v*Q.qy.v-Q.qz.v*Q.qz.v
           EA.alpha.v = atan2(t0.v,t1.v)
           EA.beta.v  = v16_pi.v
           EA.gamma.v = v16_0.v
        else
           t0.v = Q.qy.v*Q.qw.v
           c0.v = Q.qx.v*Q.qz.v+t0.v
           t1.v = Q.qz.v*Q.qw.v
           c1.v = Q.qx.v*Q.qy.v-t1.v
           tmp1.v = chi.v*c0.v*v16_n1.v
           tmp2.v = chi.v*c1.v*v16_n1.v
           EA.alpha.v  = atan2(tmp1.v,tmp2.v)
           EA.beta.v  = atan2(v16_2.v*chi,v,qxw.v*qyz.v)
           c2.v = Q.qx.v*Q.qy.v+t1.v
           tmp1.v = chi.v*c0.v*v16_1.v
           tmp2.v = chi.v*c2.v*v16_n1.v
           EA.gamma.v  = atan2(tmp1.v,tmp2.v)
        endif
        where(EA.alpha.v<v16_0.v) EA.alpha.v = mod(EA.alpha.v+v16_2pi.v,v16_2pi.v)
        where(EA.beta.v<v16_0.v)  EA.beta.v  = mod(EA.beta.v+v16_2pi.v,v16_pi)
        where(EA.gamma.v<v16_0.v) EA.gamma.v = mod(EA.gamma.v+v16_2pi.v,v16_2pi.v)
    end subroutine q4x16_to_ea3x16_zmm16r4



#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x8_to_ea3x8_zmm8r8(Q,EA)  !GCC$ ATTRIBUTES hot :: q4x8_to_ea3x8_zmm8r8 !GCC$ ATTRIBUTES inline :: q4x8_to_ea3x8_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x8_to_ea3x8_zmm8r8(Q,EA)
        !DIR$ ATTRIBUTES INLINE :: q4x8_to_ea3x8_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: q4x8_to_ea3x8_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x8_to_ea3x8_zmm8r8
#endif
        use rotation_types, only : Q4x8v8,EA3x8v8
        use mod_vectypes,   only : Mask8_t
        use mod_vecconsts,  only : v8_n1,v8_2,v8_2pi,v8_pi
        type(Q4x8v8),  intent(in)  :: Q
        type(EA3x8v8), intent(out) :: EA
        ! Locals
        type(ZMM8r8_t), automatic :: qxw,qyz,chi
        type(ZMM8r8_t), automatic :: t0,c0,t1,c1,c2
        type(ZMM8r8_t), automatic :: tmp0,tmp1,tmp2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: qxw,qyz,chi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,c0,t1,c1,c2
        !DIR$ ATTRIBUTES ALIGN : 64 :: tmp0,tmp1,tmp2
#endif
        type(Mask8_t), automatic :: k1,k2
        qxw.v = Q.qx.v*Q.qx.v+Q.qw.v*Q.qw.v
        qyz.v = Q.qy.v*Q.qy.v+Q.qz.v*Q.qz.v
        chi.v = sqrt(qxw.v*qyz.v)
        k1.m  = qyz.v==v8_0.v
        k2.m  = qxw.v==v8_0.v
        if(all(k1.m)) then
           t0.v = v8_n2.v*Q.qx.v*Q.qw.v
           t1.v = Q.qx.v*Q.qx.v-Q.qw.v*Q.qw.v
           EA.alpha.v = atan2(t0.v,t1.v)
           EA.beta.v  = v8_0.v
           EA.gamma.v = v8_0.v
        else if(all(k2.m)) then
           t0.v = v8_2.v*Q.qy.v*Q.qz.v
           t1.v = Q.qy.v*Q.qy.v-Q.qz.v*Q.qz.v
           EA.alpha.v = atan2(t0.v,t1.v)
           EA.beta.v  = v8_pi.v
           EA.gamma.v = v8_0.v
        else
           t0.v = Q.qy.v*Q.qw.v
           c0.v = Q.qx.v*Q.qz.v+t0.v
           t1.v = Q.qz.v*Q.qw.v
           c1.v = Q.qx.v*Q.qy.v-t1.v
           tmp1.v = chi.v*c0.v*v8_n1.v
           tmp2.v = chi.v*c1.v*v8_n1.v
           EA.alpha.v  = atan2(tmp1.v,tmp2.v)
           EA.beta.v  = atan2(v8_2.v*chi,v,qxw.v*qyz.v)
           c2.v = Q.qx.v*Q.qy.v+t1.v
           tmp1.v = chi.v*c0.v*v8_1.v
           tmp2.v = chi.v*c2.v*v8_n1.v
           EA.gamma.v  = atan2(tmp1.v,tmp2.v)
        endif
        where(EA.alpha.v<v8_0.v) EA.alpha.v = mod(EA.alpha.v+v8_2pi.v,v8_2pi.v)
        where(EA.beta.v<v8_0.v)  EA.beta.v  = mod(EA.beta.v+v8_2pi.v,v8_pi)
        where(EA.gamma.v<v8_0.v) EA.gamma.v = mod(EA.gamma.v+v8_2pi.v,v8_2pi.v)
     end subroutine q4x8_to_ea3x8_zmm8r8


     !  /*
     !       Convert unit quaternion to axis angle pair
     !  */

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine q4x16_to_ax4x16_zmm16r4(Q,AA) !GCC$ ATTRIBUTES hot :: q4x16_to_ax4x16_zmm16r16 !GCC$ ATTRIBUTES inline :: q4x16_to_ax4x16_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine q4x16_to_ax4x16_zmm16r4(Q,AA)
        !DIR$ ATTRIBUTES INLINE :: q4x16_to_ax4x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: q4x16_to_ax4x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x16_to_ax4x16_zmm16r4
#endif
       use rotation_types, only : Q4x16v16, AX4x16v16
       use mod_vectypes,   only : Mask16_t
       use mod_vecconsts,  only : v16_0, v16_1, v16_pi, v16_2, v16_n1
       type(Q4x16v16),    intent(in)   :: Q
       type(AX4x16v16),   intent(out)  :: AA
       ! Locals
       type(ZMM16r4_t), automatic :: t0,t1,t2,v0
       type(ZMM16r4_t), automatic :: omega,s
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES ALIGNED : 64 :: t0,t1,t2,v0
       !DIR$ ATTRIBUTES ALIGNED : 64 :: omega,s
#endif
       type(Mask16_t), automatic :: k1,k2
       t0.v = Q.qy.v*Q.qy.v
       t1.v = Q.qz.v*Q.qz.v
       t2.v = Q.qw.v*Q.qw.v
       v0.v = t0.v+t1.v+t2.v
       k1.m = v0.v==v16_0.v
       if(all(k1.m)) then
          AA.ax_1.v=v16_0.v
          AA.ax_2.v=v16_0.v
          AA.ax_3.v=v16_0.v
          AA.ax_4.v=v16_0.v
          return
       end if
       k2.m = Q.qx.v/=v16_0.v
       if(all(k2.m)) then
           s.v = sign(v16_1,Q.qx.v)/norm2_zmm16r4(Q.qy.v, &
                                                 Q.qz.v, &
                                                 Q.qw.v)
           AA.ax_1.v = Q.qy.v*s.v
           AA.ax_2.v = Q.qz.v*s.v
           AA.ax_3.v = Q.qw.v*s.v
           omega.v   = v16_2.v*acos(clip_zmm16r4(Q.qx.v,   &
                                                v16_n1.v, &
                                                v16_1.v)
           AA.ax_4.v = omega.v
           return
        else
           AA.ax_1.v = Q.qy.v
           AA.ax_2.v = Q.qz.v
           AA.ax_3.v = Q.qw.v
           AA.ax_4.v = v16_pi.v
           return
       end if
     end subroutine q4x16_to_ax4x16_zmm16r4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine q4x8_to_ax4x8_zmm8r8(Q,AA) !GCC$ ATTRIBUTES hot :: q4x8_to_ax4x8_zmm8r8 !GCC$ ATTRIBUTES inline :: q4x8_to_ax4x8_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine q4x8_to_ax4x8_zmm8r8(Q,AA)
        !DIR$ ATTRIBUTES INLINE :: q4x8_to_ax4x8_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: q4x8_to_ax4x8_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x8_to_ax4x8_zmm8r8
#endif
       use rotation_types, only : Q4x8v8, AX4x8v8
       use mod_vectypes,   only : Mask8_t
       use mod_vecconsts,  only : v8_0, v8_1, v8_pi, v8_2, v8_n1
       type(Q4x8v8),    intent(in)   :: Q
       type(AX4x8v8),   intent(out)  :: AA
       ! Locals
       type(ZMM8r8_t), automatic :: t0,t1,t2,v0
       type(ZMM8r8_t), automatic :: omega,s
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !DIR$ ATTRIBUTES ALIGNED : 64 :: t0,t1,t2,v0
       !DIR$ ATTRIBUTES ALIGNED : 64 :: omega,s
#endif
       type(Mask8_t), automatic :: k1,k2
       t0.v = Q.qy.v*Q.qy.v
       t1.v = Q.qz.v*Q.qz.v
       t2.v = Q.qw.v*Q.qw.v
       v0.v = t0.v+t1.v+t2.v
       k1.m = v0.v==v16_0.v
       if(all(k1.m)) then
          AA.ax_1.v=v8_0.v
          AA.ax_2.v=v8_0.v
          AA.ax_3.v=v8_0.v
          AA.ax_4.v=v8_0.v
          return
       end if
       k2.m = Q.qx.v/=v8_0.v
       if(all(k2.m)) then
           s.v = sign(v8_1,Q.qx.v)/norm2_zmm8r8(Q.qy.v, &
                                                 Q.qz.v, &
                                                 Q.qw.v)
           AA.ax_1.v = Q.qy.v*s.v
           AA.ax_2.v = Q.qz.v*s.v
           AA.ax_3.v = Q.qw.v*s.v
           omega.v   = v8_2.v*acos(clip_zmm8r8(Q.qx.v,   &
                                                v8_n1.v, &
                                                v8_1.v)
           AA.ax_4.v = omega.v
           return
        else
           AA.ax_1.v = Q.qy.v
           AA.ax_2.v = Q.qz.v
           AA.ax_3.v = Q.qw.v
           AA.ax_4.v = v8_pi.v
           return
       end if
     end subroutine q4x8_to_ax4x8_zmm8r8
     
    

     


     
    









end module avx512_rotations



  
