

module mueller_calculus_avx512


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mueller_calculus_avx512'
 !          
 !          Purpose:
  !                       Mueller calculus based mainly on
  !                       // Based on: R.A. Chipman, "Polarimetry" chapter in Handbook of Optics Volume 2
 !                        // (McGraw-Hill, New York, 1995).
 !                     
 !
 !          History:
 !                        
 !                        Date: 12-01-2022
 !                        Time: 15:11 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                      Bernard Gingold
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
     use mod_kinds,         only : i4, sp, dp
     use mod_vectypes,      only : ZMM16r4_t, ZMM8r8_t
     use mod_avx512_cvec16, only : ZMM16c4
     use mod_avx512_cvec8,  only : ZMM8c8
     use mueller_types_avx512
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_FULLVER = 1000*MUELLER_CALCULUS_AVX512_MAJOR+100*MUELLER_CALCULUS_AVX512_MINOR+ &
                                             10*MUELLER_CALCULUS_AVX512_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_CREATE_DATE = "12-01-2022 15:16 +00200 (WED 12 JAN 2022 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_BUILD_DATE = __DATE__ ":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_SYNOPSIS = " Mueller calculus implementation"

  contains

    !=============================================================
    !
    ! /*
    !    Jones-Vector components -- packed single-precision
    !    16-tuple vectors of complex numbers [deinterleaved]
    !  */
    !==============================================================

    pure function JVec2x16c16_set_1() result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_set_1
#endif
       type(JVec2x16c16) :: cvec
      ! EXEC CODE ....
       cvec.p = ZMM16c4()
       cvec.s = ZMM16c4()
    end function JVec2x16c16_set_1
    

    pure function JVec2x16c16_set_2(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_set_2
#endif
        complex(kind=sp),    intent(in) :: c1
        complex(kind=sp),    intent(in) :: c2
        type(JVec2x16c16) :: cvec
        !Exec code ....
        cvec.p = complex1x_init(c1)
        cvec.s = complex1x_init(c2)
    end function JVec2x16c16_set_2

   
    pure function JVec2x16c16_set_3(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_3
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_3
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_set_3
#endif
        complex(kind=sp), dimension(0:15), intent(in) :: c1
        complex(kind=sp), dimension(0:15), intent(in) :: c2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED c1:64
        !DIR$ ASSUME_ALIGNED c2:64
#endif
        type(JVec2x16c16) :: cvec
        ! Exec code ....
        cvec.p = complex2x16_init(c1)
        cvec.s = complex2x16_init(c2)
    end function JVec2x16c16_set_3


    pure function JVec2x16c16_set_4(v1,v2,v3,v4) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_4
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_set_4
#endif
        type(ZMM16r4_t),    intent(in) :: v1
        type(ZMM16r4_t),    intent(in) :: v2
        type(ZMM16r4_t),    intent(in) :: v3
        type(ZMM16r4_t),    intent(in) :: v4
        type(JVec2x16c16) :: cvec
        ! Exec code ....
        cvec.p = zmm16r42x_init(v1,v2)
        cvec.s = zmm16r42x_init(v3,v4)
    end function JVec2x16c16_set_4


    pure function JVec2x16c16_set_5(v1,v2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_5
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_5
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_set_5
#endif
        type(ZMM16r4_t),    intent(in) :: v1
        type(ZMM16r4_t),    intent(in) :: v2
        type(JVec2x16c16) :: cvec
        ! Exec code ...
        cvec.p = zmm16r41x_init(v1)
        cvec.s = zmm16r41x_init(v2)
    end function JVec2x16c16_set_5
    

    pure function JVec2x16c16_copy(other) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_copy
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_copy
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_copy
#endif
       type(JVec2x16c16),   intent(in) :: other
       type(JVec2x16c16) :: this
       ! Exec code ....
       this.p = copy_init(other.p)
       this.s = copy_init(other.s)
    end function JVec2x16c16_copy


    pure function JVec2x16c16_mul_JVec2x16c16(jv1,jv2) result(cv)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_mul_JVec2x16c16
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_mul_JVec2x16c16
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_mul_JVec2x16c16
#endif
         type(JVec2x16c16),   intent(in) :: jv1
         type(JVec2x16c16),   intent(in) :: jv2
         type(ZMM16c4) :: cv
         ! Exec code ....
         cv = jv1.p*jv2.p+jv1.s*jv2.s
    end function JVec2x16c16_mul_JVec2x16c16
    


    pure function JVec2x16c16_mul_ZMM16c4(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_mul_ZMM16c4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_mul_ZMM16c4
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_mul_ZMM16c4
#endif
        type(JVec2x16c16),   intent(in) :: jv
        type(ZMM16c4),       intent(in) :: c
        type(JVec2x16c16) :: this
        ! Exec code ...
        this.p = jv.p*c
        this.s = jv.s*c
     end function JVec2x16c16_mul_ZMM16c4

      
     subroutine JVec2x16c16_mul_ZMM16c4_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_mul_ZMM16c4_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_mul_ZMM16c4_v2
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_mul_ZMM16c4_v2
#endif
         type(JVec2x16c16),   intent(inout) :: jv
         type(ZMM16c4),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p*c
         jv.s = jv.s*c
     end subroutine JVec2x16c16_mul_ZMM16c4_v2


     pure function JVec2x16c16_div_ZMM16c4(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_div_ZMM16c4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_div_ZMM16c4
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_div_ZMM16c4
#endif
         type(JVec2x16c16),     intent(in) :: jv
         type(ZMM16c4),         intent(in) :: c
         type(JVec2x16c16) :: this
         ! Exec code ....
         this.p = jv.p/c
         this.s = jv.s/c
      end function JVec2x16c16_div_ZMM16c4


      subroutine JVec2x16c16_div_ZMM16c4_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_div_ZMM16c4_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_div_ZMM16c4_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_div_ZMM16c4_v2
#endif
         type(JVec2x16c16),   intent(inout) :: jv
         type(ZMM16c4),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p/c
         jv.s = jv.s/c
     end subroutine JVec2x16c16_div_ZMM16c4_v2 
     
       
     pure function JVec2x16c16_conjugate(jv) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_conjugate
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_conjugate
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_conjugate
#endif
          type(JVec2x16c16),   intent(in) :: jv
          type(JVec2x16c16) :: this
          ! Exec code ....
          this.p = conjugate(jv.p)
          this.s = conjugate(jv.s)
      end function JVec2x16c16_conjugate


      pure function JVec2x16c16_add_JVec2x16c16(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_add_JVec2x16c16
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_add_JVec2x16c16
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_add_JVec2x16c16
#endif
          type(JVec2x16c16),    intent(in) :: jv1
          type(JVec2x16c16),    intent(in) :: jv2
          type(JVec2x16c16) :: this
          ! Exec code ....
          this.p = jv1.p+jv2.p
          this.s = jv1.s+jv2.s
      end function JVec2x16c16_add_JVec2x16c16


      subroutine JVec2x16c16_add_Jvec2x16c16_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_add_JVec2x16c16_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_add_JVec2x16c16_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_add_JVec2x16c16_v2
#endif
          type(JVec2x16c16),    intent(inout) :: jv
          type(JVec2x16c16),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p+jv1.p
          jv.s = jv.s+jv1.s
      end subroutine JVec2x16c16_add_Jvec2x16c16_v2
      
      
      pure function JVec2x16c16_sub_JVec2x16c16(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_sub_JVec2x16c16
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_sub_JVec2x16c16
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_sub_JVec2x16c16
#endif
          type(JVec2x16c16),    intent(in) :: jv1
          type(JVec2x16c16),    intent(in) :: jv2
          type(JVec2x16c16) :: this
          ! Exec code ....
          this.p = jv1.p-jv2.p
          this.s = jv1.s-jv2.s
      end function JVec2x16c16_sub_JVec2x16c16


      subroutine JVec2x16c16_sub_Jvec2x16c16_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_sub_JVec2x16c16_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_sub_JVec2x16c16_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_sub_JVec2x16c16_v2
#endif
          type(JVec2x16c16),    intent(inout) :: jv
          type(JVec2x16c16),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p-jv1.p
          jv.s = jv.s-jv1.s
      end subroutine JVec2x16c16_sub_Jvec2x16c16_v2


      pure function JVec2x16c16_norm(jv) result(norm)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_norm
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_norm
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_norm
#endif
          type(JVec2x16c16),   intent(in) :: jv
          type(ZMM16r4_t) :: norm
          ! Locals
          type(ZMM16r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1 = cnorm(jv.p)
          v2 = cnorm(jv.s)
          cn.v = v1.v+v2.v
       end function JVec2x16c16_norm


       pure function JVec2x16c16_psi(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_psi
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_psi
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_psi
#endif
           type(JVec2x16c16),   intent(in) :: jv
           type(ZMM16r4_t) :: v
           ! LOcals
    type(ZMM16r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = cabs_c16(jv.p)
          v2.v = cabs_c16(jv.s)
          v.v  = atan(v1.v/v2.v)
       end function JVec2x16c16_psi
       
      
      


     
    
    


      
    
    







end module mueller_calculus_avx512
