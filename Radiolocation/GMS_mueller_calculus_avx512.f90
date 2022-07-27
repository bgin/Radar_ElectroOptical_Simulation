

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

    type(ZMM16c4), parameter, private :: j     = ZMM16c4(0.0_sp,-1.0_sp)
    type(ZMM16c4), parameter, private :: czero = ZMM16c4()

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

        
       pure function JVec2x16c16_delta(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_delta
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_delta
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_delta
#endif
          type(JVec2x16c16), intent(in) :: jv
          type(ZMM16r4_t) :: v
          ! Locals
          type(ZMM16r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = carg_c16(jv.p)
          v2.v = carg_c16(jv.s)
          v.v  = v1.v-v2.v
       end function JVec2x16c16_delta
       
       !==============================================
       !  Double precision 8-tuple complex vector.
       !==============================================
        
       
      pure function JVec2x8c8_set_1() result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_set_1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c18_set_1
#endif
       type(JVec2x8c8) :: cvec
      ! EXEC CODE ....
       cvec.p = ZMM8c8()
       cvec.s = ZMM8c8()
    end function JVec2x8c8_set_1
    

    pure function JVec2x8c8_set_2(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_set_2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_set_2
#endif
        complex(kind=dp),    intent(in) :: c1
        complex(kind=dp),    intent(in) :: c2
        type(JVec2x8c8) :: cvec
        !Exec code ....
        cvec.p = complex1x_init(c1)
        cvec.s = complex1x_init(c2)
    end function JVec2x8c8_set_2

   
    pure function JVec2x8c8_set_3(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_set_3
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_set_3
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_set_3
#endif
        complex(kind=dp), dimension(0:7), intent(in) :: c1
        complex(kind=dp), dimension(0:7), intent(in) :: c2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED c1:64
        !DIR$ ASSUME_ALIGNED c2:64
#endif
        type(JVec2x8c8) :: cvec
        ! Exec code ....
        cvec.p = complex2x8_init(c1)
        cvec.s = complex2x8_init(c2)
    end function JVec2x8c8_set_3


    pure function JVec2x8c8_set_4(v1,v2,v3,v4) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_set_4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_set_4
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_set_4
#endif
        type(ZMM8r8_t),    intent(in) :: v1
        type(ZMM8r8_t),    intent(in) :: v2
        type(ZMM8r8_t),    intent(in) :: v3
        type(ZMM8r8_t),    intent(in) :: v4
        type(JVec2x8c8) :: cvec
        ! Exec code ....
        cvec.p = zmm8r82x_init(v1,v2)
        cvec.s = zmm8r82x_init(v3,v4)
    end function JVec2x8c8_set_4


    pure function JVec2x8c8_set_5(v1,v2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_set_5
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_set_5
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_set_5
#endif
        type(ZMM8r8_t),    intent(in) :: v1
        type(ZMM8r8_t),    intent(in) :: v2
        type(JVec2x8c8) :: cvec
        ! Exec code ...
        cvec.p = zmm8r81x_init(v1)
        cvec.s = zmm8r81x_init(v2)
    end function JVec2x8c8_set_5
    

    pure function JVec2x8c8_copy(other) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_copy
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_copy
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_copy
#endif
       type(JVec2x8c8),   intent(in) :: other
       type(JVec2x8c8) :: this
       ! Exec code ....
       this.p = copy_init(other.p)
       this.s = copy_init(other.s)
    end function JVec2x8c8_copy


    pure function JVec2x8c8_mul_JVec2x8c8(jv1,jv2) result(cv)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_mul_JVec2x8c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_mul_JVec2x8c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_mul_JVec2x8c8
#endif
         type(JVec2x8c8),   intent(in) :: jv1
         type(JVec2x8c8),   intent(in) :: jv2
         type(ZMM8c8) :: cv
         ! Exec code ....
         cv = jv1.p*jv2.p+jv1.s*jv2.s
    end function JVec2x8c8_mul_JVec2x8c8
    


    pure function JVec2x8c8_mul_ZMM8c8(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_mul_ZMM8c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_mul_ZMM8c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_mul_ZMM8c8
#endif
        type(JVec2x8c8),   intent(in) :: jv
        type(ZMM8c8),       intent(in) :: c
        type(JVec2x8c8) :: this
        ! Exec code ...
        this.p = jv.p*c
        this.s = jv.s*c
     end function JVec2x8c8_mul_ZMM8c8

      
     subroutine JVec2x8c8_mul_ZMM8c8_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_mul_ZMM8c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_mul_ZMM8c8_v2
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_mul_ZMM8c8_v2
#endif
         type(JVec2x8c8),   intent(inout) :: jv
         type(ZMM8c8),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p*c
         jv.s = jv.s*c
     end subroutine JVec2x8c8_mul_ZMM8c8_v2


     pure function JVec2x8c8_div_ZMM8c8(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_div_ZMM8c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_div_ZMM8c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_div_ZMM8c8
#endif
         type(JVec2x8c8),     intent(in) :: jv
         type(ZMM8c8),         intent(in) :: c
         type(JVec2x8c8) :: this
         ! Exec code ....
         this.p = jv.p/c
         this.s = jv.s/c
      end function JVec2x8c8_div_ZMM8c8


      subroutine JVec2x8c8_div_ZMM8c8_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_div_ZMM8c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_div_ZMM8c8_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_div_ZMM8c8_v2
#endif
         type(JVec2x8c8),   intent(inout) :: jv
         type(ZMM8c8),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p/c
         jv.s = jv.s/c
     end subroutine JVec2x8c8_div_ZMM8c8_v2 
     
       
     pure function JVec2x8c8_conjugate(jv) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_conjugate
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_conjugate
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_conjugate
#endif
          type(JVec2x8c8),   intent(in) :: jv
          type(JVec2x8c8) :: this
          ! Exec code ....
          this.p = conjugate(jv.p)
          this.s = conjugate(jv.s)
      end function JVec2x8c8_conjugate


      pure function JVec2x8c8_add_JVec2x8c8(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_add_JVec2x8c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_add_JVec2x8c8
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_add_JVec2x8c8
#endif
          type(JVec2x8c8),    intent(in) :: jv1
          type(JVec2x8c8),    intent(in) :: jv2
          type(JVec2x8c8) :: this
          ! Exec code ....
          this.p = jv1.p+jv2.p
          this.s = jv1.s+jv2.s
      end function JVec2x8c8_add_JVec2x8c8


      subroutine JVec2x8c8_add_Jvec2x8c8_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_add_JVec2x8c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_add_JVec2x8c8_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_add_JVec2x8c8_v2
#endif
          type(JVec2x8c8),    intent(inout) :: jv
          type(JVec2x8c8),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p+jv1.p
          jv.s = jv.s+jv1.s
      end subroutine JVec2x8c8_add_Jvec2x8c8_v2
      
      
      pure function JVec2x8c8_sub_JVec2x8c8(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_sub_JVec2x8c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_sub_JVec2x8c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_sub_JVec2x8c8
#endif
          type(JVec2x8c8),    intent(in) :: jv1
          type(JVec2x8c8),    intent(in) :: jv2
          type(JVec2x8c8) :: this
          ! Exec code ....
          this.p = jv1.p-jv2.p
          this.s = jv1.s-jv2.s
      end function JVec2x8c8_sub_JVec2x8c8


      subroutine JVec2x8c8_sub_Jvec2x8c8_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_sub_JVec2x8c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_sub_JVec2x8c8_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_sub_JVec2x8c8_v2
#endif
          type(JVec2x8c8),    intent(inout) :: jv
          type(JVec2x8c8),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p-jv1.p
          jv.s = jv.s-jv1.s
      end subroutine JVec2x8c8_sub_Jvec2x8c8_v2


      pure function JVec2x8c8_norm(jv) result(norm)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_norm
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_norm
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_norm
#endif
          type(JVec2x8c8),   intent(in) :: jv
          type(ZMM8r8_t) :: norm
          ! Locals
          type(ZMM8r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1 = cnorm(jv.p)
          v2 = cnorm(jv.s)
          cn.v = v1.v+v2.v
       end function JVec2x8c8_norm


       pure function JVec2x8c8_psi(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_psi
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_psi
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_psi
#endif
           type(JVec2x8c8),   intent(in) :: jv
           type(ZMM8r8_t) :: v
           ! LOcals
           type(ZMM8r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = cabs_zmm8c8(jv.p)
          v2.v = cabs_zmm8c8(jv.s)
          v.v  = atan(v1.v/v2.v)
       end function JVec2x8c8_psi

        
       pure function JVec2x8c8_delta(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x8c8_delta
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x8c8_delta
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c8_delta
#endif
          type(JVec2x8c8), intent(in) :: jv
          type(ZMM8r8_t) :: v
          ! Locals
          type(ZMM8r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = carg_zmm8c8(jv.p)
          v2.v = carg_zmm8c8(jv.s)
          v.v  = v1.v-v2.v
       end function JVec2x8c8_delta
      
        
#if 0

/*
                         Jones Matrix implementation based on SIMD 
                         16-tuple complex vector [deinterleaved]
                         single-precision.
                         @Reference
                           
                                  typedef struct  __ATTR_ALIGN__(64) JMat4x16c16 {
                                      ZMM16c4 pp; j0
				      ZMM16c4 ss; j1
				      ZMM16c4 ps; j2
				      ZMM16c4 sp; j3
		                 }JMat4x16c16;
                                 typedef struct __ATTR_ALIGN__(64)  JMat4x8c8 {
                                       ZMM8c8 pp;
				       ZMM8c8 ss;
				       ZMM8c8 ps;
				       ZMM8c8 sp;
		                }JMat4x8c8;
                  */

#endif       
       
       !==================================================!
       ! Notification!!
       ! Subroutine version shall be used in case of 
       ! failed vectorization of argument passing and
       ! result returning through ZMM registers.
       !==================================================!

       pure function JMat4x16c16_set_1() result(mat)

      !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_1
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_1

             type(JMat4x16c16) :: mat
             !dir$ attributes align : 64 :: mat
             !Executable code ....
             mat.j0 = default_init() ! 'pp' component
             mat.j1 = default_init() ! 'ss' component
             mat.j2 = default_init() ! 'ps' component
             mat.j3 = default_init() ! 'sp' component
        end function JMat4x16c16_set_1

       ! Subroutine version shall be used in case of 
       ! failed vectorization of argument passing and
       ! result returning through ZMM registers.

       
       subroutine JMat4x16c16_set1(mat) 

      !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set1
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set1

             type(JMat4x16c16),  intent(out) :: mat
            
             !Executable code ....
             mat.j0 = default_init() ! 'pp' component
             mat.j1 = default_init() ! 'ss' component
             mat.j2 = default_init() ! 'ps' component
             mat.j3 = default_init() ! 'sp' component
        end subroutine JMat4x16c16_set1

        pure function JMat4x16c16_set_2(pp,ss,ps,sp) result(mat)
      !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_2
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_2
             complex(kind=dp), intent(in) :: pp
             complex(kind=sp), intent(in) :: ss
             complex(kind=sp), intent(in) :: ps
             complex(kind=sp), intent(in) :: sp
             type(JMat4x16c16) :: mat
             !dir$ attributes align : 64 :: mat
             ! Executable code ....
             mat.j0 = complex1_init(pp)
             mat.j1 = complex1_init(ss)
             mat.j2 = complex1_init(ps)
             mat.j3 = complex1_init(sp)
        end function JMat4x16c16_set_2


        subroutine JMat4x16c16_set2(pp,ss,ps,sp,mat) 
      !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set2
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set2
             complex(kind=dp),  intent(in) :: pp
             complex(kind=sp),  intent(in) :: ss
             complex(kind=sp),  intent(in) :: ps
             complex(kind=sp),  intent(in) :: sp
             type(JMat4x16c16),intent(out) :: mat
             
             ! Executable code ....
             mat.j0 = complex1_init(pp)
             mat.j1 = complex1_init(ss)
             mat.j2 = complex1_init(ps)
             mat.j3 = complex1_init(sp)
        end subroutine JMat4x16c16_set2


        pure function JMat4x16c16_set_3(re1,im1,re2,im2,  &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_3
            real(kind=sp), dimension(0:15), intent(in) :: re1
            real(kind=sp), dimension(0:15), intent(in) :: im1
            real(kind=sp), dimension(0:15), intent(in) :: re2
            real(kind=sp), dimension(0:15), intent(in) :: im2
            real(kind=sp), dimension(0:15), intent(in) :: re3
            real(kind=sp), dimension(0:15), intent(in) :: im3
            real(kind=sp), dimension(0:15), intent(in) :: re4
            real(kind=sp), dimension(0:15), intent(in) :: im4
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            !Executable code ....
            mat.j0 = array_init(re1,im1)
            mat.j1 = array_init(re2,im2)
            mat.j2 = array_init(re3,im3)
            mat.j3 = array_init(re4,im4)
        end function JMat4x16c16_set_3


        subroutine JMat4x16c16_set3(re1,im1,re2,im2,  &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set3
            real(kind=sp), dimension(0:15), intent(in) :: re1
            real(kind=sp), dimension(0:15), intent(in) :: im1
            real(kind=sp), dimension(0:15), intent(in) :: re2
            real(kind=sp), dimension(0:15), intent(in) :: im2
            real(kind=sp), dimension(0:15), intent(in) :: re3
            real(kind=sp), dimension(0:15), intent(in) :: im3
            real(kind=sp), dimension(0:15), intent(in) :: re4
            real(kind=sp), dimension(0:15), intent(in) :: im4
            type(JMat4x16c16),              intent(out) :: mat
        
            !Executable code ....
            mat.j0 = array_init(re1,im1)
            mat.j1 = array_init(re2,im2)
            mat.j2 = array_init(re3,im3)
            mat.j3 = array_init(re4,im4)
        end subroutine JMat4x16c16_set_3


        pure function JMat4x16c16_set_4(re1,im1,re2,im2,   &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_4
            real(kind=sp),      intent(in) :: re1
            real(kind=sp),      intent(in) :: im1
            real(kind=sp),      intent(in) :: re2
            real(kind=sp),      intent(in) :: im2
            real(kind=sp),      intent(in) :: re3
            real(kind=sp),      intent(in) :: im3
            real(kind=sp),      intent(in) :: re4
            real(kind=sp),      intent(in) :: im4
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            complex(kind=sp), automatic :: c1,c2,c3,c4
            ! Executable code ....
            c1 = cmplx(re1,im1)
            mat.j0 = complex1_init(c1)
            c2 = cmplx(re2,im2)
            mat.j1 = complex1_init(c2)
            c3 = cmplx(re3,im3)
            mat.j2 = complex1_init(c3)
            c4 = cmplx(re4,im4)
            mat.j3 = complex1_init(c4) 
        end function JMat4x16c16_set_4


        subroutine JMat4x16c16_set4(re1,im1,re2,im2,   &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_4
            real(kind=sp),      intent(in) :: re1
            real(kind=sp),      intent(in) :: im1
            real(kind=sp),      intent(in) :: re2
            real(kind=sp),      intent(in) :: im2
            real(kind=sp),      intent(in) :: re3
            real(kind=sp),      intent(in) :: im3
            real(kind=sp),      intent(in) :: re4
            real(kind=sp),      intent(in) :: im4
            type(JMat4x16c16),  intent(out) :: mat
            
            complex(kind=sp), automatic :: c1,c2,c3,c4
            ! Executable code ....
            c1 = cmplx(re1,im1)
            mat.j0 = complex1_init(c1)
            c2 = cmplx(re2,im2)
            mat.j1 = complex1_init(c2)
            c3 = cmplx(re3,im3)
            mat.j2 = complex1_init(c3)
            c4 = cmplx(re4,im4)
            mat.j3 = complex1_init(c4) 
        end subroutine JMat4x16c16_set4


        pure function JMat4x16c16_set_5(re1,im1,re2,im2,   &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set_5
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set_5
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set_5
            type(ZMM16r4_t),  intent(in) :: re1
            type(ZMM16r4_t),  intent(in) :: im1
            type(ZMM16r4_t),  intent(in) :: re2
            type(ZMM16r4_t),  intent(in) :: im2
            type(ZMM16r4_t),  intent(in) :: re3
            type(ZMM16r4_t),  intent(in) :: im3
            type(ZMM16r4_t),  intent(in) :: re4
            type(ZMM16r4_t),  intent(in) :: im4
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code .....
            mat.j0 = zmm16r42x_init(re1,im1)
            mat.j1 = zmm16r42x_init(re2,im2)
            mat.j2 = zmm16r42x_init(re3,im3)
            mat.j3 = zmm16r42x_init(re4,im4)
        end function JMat4x16c16_set_5


        subroutine JMat4x16c16_set5(re1,im1,re2,im2,   &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_set5
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_set5
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_set5
            type(ZMM16r4_t),  intent(in) :: re1
            type(ZMM16r4_t),  intent(in) :: im1
            type(ZMM16r4_t),  intent(in) :: re2
            type(ZMM16r4_t),  intent(in) :: im2
            type(ZMM16r4_t),  intent(in) :: re3
            type(ZMM16r4_t),  intent(in) :: im3
            type(ZMM16r4_t),  intent(in) :: re4
            type(ZMM16r4_t),  intent(in) :: im4
            type(JMat4x16c16),intent(out) :: mat
          
            ! Executable code .....
            mat.j0 = zmm16r42x_init(re1,im1)
            mat.j1 = zmm16r42x_init(re2,im2)
            mat.j2 = zmm16r42x_init(re3,im3)
            mat.j3 = zmm16r42x_init(re4,im4)
        end subroutine JMat4x16c16_set5

        
        pure function JMat4x16c16_copy(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_copy
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_copy
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_copy
            type(JMat4x16c16),  intent(in) :: x
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code ....
            mat.j0 = copy_init(x.j0)
            mat.j1 = copy_init(x.j1)
            mat.j2 = copy_init(x.j2)
            mat.j3 = copy_init(x.j3)
        end function


        subroutine JMat4x16c16_copy_v2(x,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_copy_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_copy_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_copy_v2
            type(JMat4x16c16),  intent(in) :: x
            type(JMat4x16c16),  intent9out) :: mat
           
            ! Executable code ....
            mat.j0 = copy_init(x.j0)
            mat.j1 = copy_init(x.j1)
            mat.j2 = copy_init(x.j2)
            mat.j3 = copy_init(x.j3)
        end subroutine JMat4x16c16_copy_v2


        ! Jones-Matrix multiplication
        !     mat.j0 = default_init() ! 'pp' component
        !     mat.j1 = default_init() ! 'ss' component
        !     mat.j2 = default_init() ! 'ps' component
        !     mat.j3 = default_init() ! 'sp' component
        ! Multiply two Jones Matrices, i.e. of type JMat4x16c16

        pure function JMat4x16c16_matmul_v1(m1,m2) result(mat)
          
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_matmul_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_matmul_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_matmul_v1
            type(JMat4x16c16),  intent(in) :: m1
            type(JMat4x16c16),  intent(in) :: m2
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code ....
            mat.j0 = m1.j3*m2.j2+m1.j0*m2.j0
            mat.j1 = m1.j1*m2.j1+m1.j2*m2.j1
            mat.j2 = m1.j1*m2.j2+m1.j2*m2.j0
            mat.j3 = m1.j3*m2.j1+m1.j0*m2.j3
        end function JMat4x16c16_matmul_v1


        subroutine JMat4x16c16_matmul_v2(m1,m2,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_matmul_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_matmul_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_matmul_v2
            type(JMat4x16c16),  intent(in)  :: m1
            type(JMat4x16c16),  intent(in)  :: m2
            type(JMat4x16c16),  intent(out) :: mat
            ! Executable code ...
            mat.j0 = m1.j3*m2.j2+m1.j0*m2.j0
            mat.j1 = m1.j1*m2.j1+m1.j2*m2.j1
            mat.j2 = m1.j1*m2.j2+m1.j2*m2.j0
            mat.j3 = m1.j3*m2.j1+m1.j0*m2.j3
        end subroutine JMat4x16c16_matmul_v2


        ! Jones Matrix multiplied by contant i.e. complex 16-tuple vector
        function JMat4x16c16_vecmul_v1(m,v) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_vecmul_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_vecmul_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_vecmul_v1
            type(JMat4x16c16),  intent(in) :: m
            type(ZMM16c4),      intent(in) :: v
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code .....
            mat.j0 = m.j0*x
            mat.j1 = m.j1*x
            mat.j2 = m.j2*x
            mat.j3 = m.j3*x
        end function JMat4x16c16_vecmul_v1        


         ! Jones Matrix multiplied by contant i.e. complex 16-tuple vector
        subroutine JMat4x16c16_vecmul_v2(m,v,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_vecmul_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_vecmul_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_vecmul_v2
            type(JMat4x16c16),  intent(in)  :: m
            type(ZMM16c4),      intent(in)  :: v
            type(JMat4x16c16),  intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code .....
            mat.j0 = m.j0*x
            mat.j1 = m.j1*x
            mat.j2 = m.j2*x
            mat.j3 = m.j3*x
        end subroutine JMat4x16c16_vecmul_v2   


        !Multiplication by Jones Vector     
        pure function JVec2x16c16_mul_JMat4x16c16(v,m) result(jvec)
            
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_mul_JMat4x16c16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_mul_JMat4x16c16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_mul_JMat4x16c16
            type(JVec2x16c16),  intent(in) :: v
            type(JMat4x16c16),  intent(in) :: m
            type(JVec2x16c16) :: jvec
            !dir$ attributes align : 64 :: mat
            ! Executable code ....
            jvec.p = m.j1*v.p+m.j3*v.s
            jvec.s = m.j3*v.p+m.j0*v.s
        end function JVec2x16c16_mul_JMat4x16c16


        subroutine JVec2x16c16_mul_JMat4x16c16_v2(v,m,jvec) 
            
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_mul_JMat4x16c16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_mul_JMat4x16c16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x16c16_mul_JMat4x16c16_v2
            type(JVec2x16c16),  intent(in) :: v
            type(JMat4x16c16),  intent(in) :: m
            type(JVec2x16c16),  intent(out) :: jvec
            
            ! Executable code ....
            jvec.p = m.j1*v.p+m.j3*v.s
            jvec.s = m.j3*v.p+m.j0*v.s
        end subroutine JVec2x16c16_mul_JMat4x16c16_v2
     
    
        pure function JMat4x16c16_vecdiv_v1(m,x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_vecdiv_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_vecdiv_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_vecdiv_v1
            type(JMat4x16c16),  intent(in) :: m
            type(ZMM16c4),      intent(in) :: x
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code ....
            mat.j0 = m.j0/x
            mat.j1 = m.j1/x
            mat.j2 = m.j2/x
            mat.j3 = m.j3/x
        end function JMat4x16c16_vecdiv_v1


        subroutine JMat4x16c16_vecdiv_v2(m,x,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_vecdiv_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_vecdiv_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_vecdiv_v2
            type(JMat4x16c16),  intent(in) :: m
            type(ZMM16c4),      intent(in) :: x
            type(JMat4x16c16),  intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            ! Executable code ....
            mat.j0 = m.j0/x
            mat.j1 = m.j1/x
            mat.j2 = m.j2/x
            mat.j3 = m.j3/x
        end subroutine JMat4x16c16_vecdiv_v2


        pure function JMat4x16c16_add_JMat4xc16c16(x,y) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_add_JMat4xc16c16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_add_JMat4xc16c16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_add_JMat4xc16c16
            type(JMat4x16c16),   intent(in) :: x
            type(JMat4x16c16),   intent(in) :: y
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            mat.j0 = x.j0+y.j0
            mat.j1 = x.j1+y.j1
            mat.j2 = x.j2+y.j2
            mat.j3 = x.j3+y.j3
        end function JMat4x16c16_add_JMat4x16c16


        subroutine JMat4x16c16_add_JMat4xc16c16_v2(x,y,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_add_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_add_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_add_JMat4xc16c16_v2
            type(JMat4x16c16),   intent(in) :: x
            type(JMat4x16c16),   intent(in) :: y
            type(JMat4x16c16),   intent(out) :: mat
            
            mat.j0 = x.j0+y.j0
            mat.j1 = x.j1+y.j1
            mat.j2 = x.j2+y.j2
            mat.j3 = x.j3+y.j3
        end subroutine JMat4x16c16_add_JMat4x16c16_v2


        pure function JMat4x16c16_sub_JMat4xc16c16(x,y) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_sub_JMat4xc16c16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_sub_JMat4xc16c16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_sub_JMat4xc16c16
            type(JMat4x16c16),   intent(in) :: x
            type(JMat4x16c16),   intent(in) :: y
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            mat.j0 = x.j0-y.j0
            mat.j1 = x.j1-y.j1
            mat.j2 = x.j2-y.j2
            mat.j3 = x.j3-y.j3
        end function JMat4x16c16_sub_JMat4x16c16


        subroutine JMat4x16c16_sub_JMat4xc16c16_v2(x,y,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_sub_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_sub_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_sub_JMat4xc16c16_v2
            type(JMat4x16c16),   intent(in) :: x
            type(JMat4x16c16),   intent(in) :: y
            type(JMat4x16c16),   intent(out) :: mat
            
            mat.j0 = x.j0-y.j0
            mat.j1 = x.j1-y.j1
            mat.j2 = x.j2-y.j2
            mat.j3 = x.j3-y.j3
        end subroutine JMat4x16c16_sub_JMat4x16c16_v2


        pure function JMat4x16c16_rotator_v1(theta) result(mat)
 
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_rotator_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_rotator_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_rotator_v1
            type(ZMM16r4_t),  intent(in) :: theta
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t) :: vc,vs,nvs
            !dir$ attributes align : 64 :: vc,vs,nvs
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t()
            ! Exec code .... 
            vc.v   = cos(theta.v)
            mat.j0 = array_init(vc.v,zero.v)
            mat.j1 = mat.j0
            vs.v   = sin(theta.v)
            mat.j2 = array_init(vs.v,zero.v)
            nvs.v  = -vs.v
            mat.j3 = array_init(nvs.v,zero.v) 
        end function JMat4x16c16_rotator_v2


        subroutine JMat4x16c16_rotator_v2(theta,mat)
 
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_rotator_v12
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_rotator_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_rotator_v2
            type(ZMM16r4_t),    intent(in)  :: theta
            type(JMat4x16c16),  intent(out) :: mat
          
            type(ZMM16r4_t) :: vc,vs,nvs
            !dir$ attributes align : 64 :: vc,vs,nvs
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t()
            ! Exec code .... 
            vc.v   = cos(theta.v)
            mat.j0 = array_init(vc.v,zero.v)
            mat.j1 = mat.j0
            vs.v   = sin(theta.v)
            mat.j2 = array_init(vs.v,zero.v)
            nvs.v  = -vs.v
            mat.j3 = array_init(nvs.v,zero.v) 
        end subroutine JMat4x16c16_rotator_v2


        pure function JMat4x16c16_lin_retarder(phi,ang) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_lin_retarder
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_lin_retarder
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_lin_retarder
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16r4_t),   intent(in) :: ang
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: n1   = ZMM16r4_t(-1.0_sp)
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t(0.0_sp)
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16c4),   automatic :: phasor
            type(ZMM16r4_t), automatic :: h_phi
            !dir$ attributes align : 64 :: phasor,h_phi
            ! Exec code ....
            phasor = cexp_c16(j*h_phi)
            mat.j0 = phasor
            h_phi  = phi.v*half.v 
            mat.j1 = one/phasor
            mat.j2 = czero
            mat.j3 = czero
        end function JMat4x16c16_lin_retarder


        subroutine JMat4x16c16_lin_retarder_v2(phi,ang,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_lin_retarder_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_lin_retarder_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_lin_retarder_v2
            type(ZMM16r4_t),    intent(in)  :: phi
            type(ZMM16r4_t),    intent(in)  :: ang
            type(JMat4x16c16),  intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: n1   = ZMM16r4_t(-1.0_sp)
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t(0.0_sp)
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16c4),   automatic :: phasor
            type(ZMM16r4_t), automatic :: h_phi
            !dir$ attributes align : 64 :: phasor,h_phi
            ! Exec code ....
            phasor = cexp_c16(j*h_phi)
            mat.j0 = phasor
            h_phi  = phi.v*half.v 
            mat.j1 = one/phasor
            mat.j2 = czero
            mat.j3 = czero
        end subroutine JMat4x16c16_lin_retarder_v2
 

        pure function JMat4x16c16_circ_retarder(phi) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_circ_retarder
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_circ_retarder
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_circ_retarder
            type(ZMM16r4_t),  intent(in) :: phi
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: half  = ZMM16r4_t(0.5_sp)
            type(ZMM16c4),   automatic :: nre
            type(ZMM16r4_t), automatic :: h_phi,ch_phi,sh_phi
            !dir$ attributes align : 64 :: nre
            !dir$ attributes align : 64 :: h_phi
            !dir$ attributes align : 64 :: ch_phi
            !dir$ attributes align : 64 :: sh_phi
            ! Exec code ....
            ch_phi = cos(phi.v)
            mat.j0 = zmm16r41x_init(ch_phi)
            mat.j1 = mat.j0
            sh_phi = sin(phi.v)
            mat.j2 = zmm16r41x_init(sh_phi)
            mat.j3 = -mat.j2.re
        end function JMat4x16c16_circ_retarder


        subroutine JMat4x16c16_circ_retarder_v2(phi,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_circ_retarder_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_circ_retarder_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_circ_retarder_v2
            type(ZMM16r4_t),   intent(in) :: phi
            type(JMat4x16c16), intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: half  = ZMM16r4_t(0.5_sp)
            type(ZMM16c4),   automatic :: nre
            type(ZMM16r4_t), automatic :: h_phi,ch_phi,sh_phi
            !dir$ attributes align : 64 :: nre
            !dir$ attributes align : 64 :: h_phi
            !dir$ attributes align : 64 :: ch_phi
            !dir$ attributes align : 64 :: sh_phi
            ! Exec code ....
            ch_phi = cos(phi.v)
            mat.j0 = zmm16r41x_init(ch_phi)
            mat.j1 = mat.j0
            sh_phi = sin(phi.v)
            mat.j2 = zmm16r41x_init(sh_phi)
            mat.j3 = -mat.j2.re
        end subroutine JMat4x16c16_circ_retarder_v2


        pure function JMat4x16c16_circ_polar(atten) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_circ_polar
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_circ_polar
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_circ_polar
            type(ZMM16r4_t),   intent(in) :: atten
            type(JMat4x16c16)  :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t(0.0_sp)
            type(ZMM16r4_t), automatic :: t0,e,t1,t2
            !dir$ attributes align : 64 :: t0,e,t1,t2
            ! Exec code ....
            t0.v   = (atten.v-one.v)/(atten.v+one.v)
            e.v    = sqrt(t0.v)
            t2.v   = half.v*(one.v+e.v)
            mat.j0 = zmm16r41x_init(t2)
            mat.j1 = mat.j0 
            t1.v   = half.v*(one.v-e.v)
            mat.j2 = array_init(zero.v,t1.v)
            mat.j3 = array_init(zero.v,zero.v-t1.v)
        end function JMat4x16c16_circ_polar


        subroutine JMat4x16c16_circ_polar_v2(atten,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_circ_polar_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_circ_polar_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_circ_polar_v2
            type(ZMM16r4_t),   intent(in) :: atten
            type(JMat4x16c16), intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16r4_t), parameter :: one  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t), parameter :: half = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t(0.0_sp)
            type(ZMM16r4_t), automatic :: t0,e,t1,t2
            !dir$ attributes align : 64 :: t0,e,t1,t2
            ! Exec code ....
            t0.v   = (atten.v-one.v)/(atten.v+one.v)
            e.v    = sqrt(t0.v)
            t2.v   = half.v*(one.v+e.v)
            mat.j0 = zmm16r41x_init(t2)
            mat.j1 = mat.j0 
            t1.v   = half.v*(one.v-e.v)
            mat.j2 = array_init(zero.v,t1.v)
            mat.j3 = array_init(zero.v,zero.v-t1.v)
        end subroutine JMat4x16c16_circ_polar_v2


        pure function JMat4x16c16_eigenvalue(a,b,ca,cb) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_eigenvalue
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_eigenvalue
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_eigenvalue
            type(JVec2x16c16),  intent(in) :: a
            type(JVec2x16c16),  intent(in) :: b
            type(ZMM16c4),      intent(in) :: ca
            type(ZMM16c4),      intent(in) :: cb
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16c4), automatic :: det,t0,t1
            !dir$ attributes align : 64 :: det,t0,t1
            ! Exec code .....
            t0     = a.p*b.s
            t1     = a.s*b.p
            det    = t0-t1
            mat.j0 = t0*cb-t1*ca
            mat.j0 = mat.j0/det
            mat.j1 = t0.ca-t1*cb
            mat.j1 = mat.j1/det
            mat.j2 = a.p*b.p*(cb-ca)
            mat.j2 = mat.j2/det
            mat.j3 = a.s*b.S*(ca-cb)
            mat.j3 = mat.j3/set
        end function JMat4x16c16_eigenvalue


        subroutine JMat4x16c16_eigenvalue_v2(a,b,ca,cb,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_eigenvalue_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_eigenvalue_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_eigenvalue_v2
            type(JVec2x16c16),  intent(in) :: a
            type(JVec2x16c16),  intent(in) :: b
            type(ZMM16c4),      intent(in) :: ca
            type(ZMM16c4),      intent(in) :: cb
            type(JMat4x16c16),  intent(out) :: mat
            !dir$ attributes align : 64 :: mat
            type(ZMM16c4), automatic :: det,t0,t1
            !dir$ attributes align : 64 :: det,t0,t1
            ! Exec code .....
            t0     = a.p*b.s
            t1     = a.s*b.p
            det    = t0-t1
            mat.j0 = t0*cb-t1*ca
            mat.j0 = mat.j0/det
            mat.j1 = t0.ca-t1*cb
            mat.j1 = mat.j1/det
            mat.j2 = a.p*b.p*(cb-ca)
            mat.j2 = mat.j2/det
            mat.j3 = a.s*b.S*(ca-cb)
            mat.j3 = mat.j3/set
        subroutine JMat4x16c16_eigenvalue_v2


        pure function JMat4x16c16_transpose(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_transpose
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_transpose
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_transpose
            type(JMat4x16c16),  intent(in) :: x
            type(JMat4x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            mat.j0 = x.j0
            mat.j1 = x.j1
            mat.j2 = mat.j3
            mat.j3 = mat.j2
        end function JMat4x16c16_transpose


        subroutine JMat4x16c16_transpose_v2(x,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_transpose_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_transpose_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_transpose_v2
            type(JMat4x16c16),  intent(in) :: x
            type(JMat4x16c16),  intent(out) :: mat
            mat.j0 = x.j0
            mat.j1 = x.j1
            mat.j2 = mat.j3
            mat.j3 = mat.j2
        end subroutine JMat4x16c16_transpose


        pure function JMat4x16c16_conj(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_conj
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_conj
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_conj
            type(JMat4x16c16),  intent(in) :: x
            type(JMat47x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            mat.j0 = conjugate(x.j0)
            mat.j1 = conjugate(x.j1)
            mat.j2 = conjugate(x.j2)
            mat.j3 = conjugate(x.j3)
        end function JMat4x16c16_conj


        subroutine JMat4x16c16_conj_v2(x,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JMat4x16c16_conj_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JMat4x16c16_conj_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JMat4x16c16_conj_v2
            type(JMat4x16c16),  intent(in) :: x
            type(JMat47x16c16) :: mat
            !dir$ attributes align : 64 :: mat
            mat.j0 = conjugate(x.j0)
            mat.j1 = conjugate(x.j1)
            mat.j2 = conjugate(x.j2)
            mat.j3 = conjugate(x.j3)
        end subroutine JMat4x16c16_conj_v2

        pure function SVec4x16v16_set_1() result(svec)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_1
            type(SVec4x16v16) :: svec
            type(ZMM16r4_t), parameter :: vz = ZMM16r4_t(0.0_sp)
            svec.s0 = vz
            svec.s1 = vz
            svec.s2 = vz
            svec.s3 = vz
        end function SVec4x16v16_set_1


        subroutine SVec4x16v16_set1(svec) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set1_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set1_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set1_v2
            type(SVec4x16v16), intent(out) :: svec
            type(ZMM16r4_t), parameter :: vz = ZMM16r4_t(0.0_sp)
            svec.s0 = vz
            svec.s1 = vz
            svec.s2 = vz
            svec.s3 = vz
        end subroutine SVec4x16v16_set1_v2

       
        pure function SVec4x16v16_set_2(x0,x1,x2,x3) result(svec)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_2
            real(kind=sp),  intent(in) :: x0
            real(kind=sp),  intent(in) :: x1
            real(kind=sp),  intent(in) :: x2
            real(kind=dp),  intent(in) :: x3
            type(SVec4x16v16) :: svec
            svec.s0 = ZMM16r4_t(x0)
            svec.s1 = ZMM16r4_t(x1)
            svec.s2 = ZMM16r4_t(x2)
            svec.s3 = ZMM16r4_t(x3)
        end function SVec4x16v16_set_2


        subroutine SVec4x16v16_set2_v2(x0,x1,x2,x3,svec) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set2_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set2_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set2_v2
            real(kind=sp),     intent(in) :: x0
            real(kind=sp),     intent(in) :: x1
            real(kind=sp),     intent(in) :: x2
            real(kind=dp),     intent(in) :: x3
            type(SVec4x16v16), intent(out) :: svec
            svec.s0 = ZMM16r4_t(x0)
            svec.s1 = ZMM16r4_t(x1)
            svec.s2 = ZMM16r4_t(x2)
            svec.s3 = ZMM16r4_t(x3)
        end subroutine SVec4x16v16_set2_v2


        pure function SVec4x16v16_set_3(x0,x1,x2,x3) result(svec)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_3
            real(kind=sp), dimension(0:15),  intent(in) :: x0
            real(kind=sp), dimension(0:15),  intent(in) :: x1
            real(kind=sp), dimension(0:15),  intent(in) :: x2
            real(kind=dp), dimension(0:15),  intent(in) :: x3
            type(SVec4x16v16) :: svec
            svec.s0 = x0
            svec.s1 = x1
            svec.s2 = x2
            svec.s3 = x3
        end function SVec4x16v16_set_3


        subroutine SVec4x16v16_set2_v3(x0,x1,x2,x3,svec) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set2_v3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set2_v3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set2_v3
            real(kind=sp), dimension(0:15),  intent(in) :: x0
            real(kind=sp), dimension(0:15),  intent(in) :: x1
            real(kind=sp), dimension(0:15),  intent(in) :: x2
            real(kind=dp), dimension(0:15),  intent(in) :: x3
            type(SVec4x16v16), intent(out) :: svec
            svec.s0 = x0
            svec.s1 = x1
            svec.s2 = x2
            svec.s3 = x3
        end subroutine SVec4x16v16_set2_v3
        
 
        pure function SVec4x16v16_set_4(x0,x1,x2,x3) result(svec)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_4
            type(ZMM16r4_t), intent(in) :: x0
            type(ZMM16r4_t), intent(in) :: x1
            type(ZMM16r4_t), intent(in) :: x2
            type(ZMM16r4_t), intent(in) :: x3
            type(SVec4x16v16) :: svec
            svec.s0 = x0
            svec.s1 = x1
            svec.s2 = x2
            svec.s3 = x3
        end function SVec4x16v16_set_4


        subroutine SVec4x16v16_set_4_v2(x0,x1,x2,x3,svec) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_4_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_4_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_4_v2
            type(ZMM16r4_t), intent(in) :: x0
            type(ZMM16r4_t), intent(in) :: x1
            type(ZMM16r4_t), intent(in) :: x2
            type(ZMM16r4_t), intent(in) :: x3
            type(SVec4x16v16), intent(out) :: svec
            svec.s0 = x0
            svec.s1 = x1
            svec.s2 = x2
            svec.s3 = x3
        end subroutine SVec4x16v16_set_4_v2


        pure function SVec4x16v16_set_5(x) result(svec)


             !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_5
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_5
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_5
            type(SVec4x16v16),  intent(in) :: x
            type(SVec4x16v16) :: svec
            !dir$ attributes align : 64 :: svec
            ! Exec code ....
            svec.s0 = x.s0
            svec.s1 = x.s1
            svec.s2 = x.s2
            svec.s3 = x.s3
        end function SVec4x16v16_set_5
          

        subroutine SVec4x16v16_set_5_v2(x,svec)


             !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_set_5_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_set_5_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_set_5_v2
            type(SVec4x16v16),  intent(in)  :: x
            type(SVec4x16v16),  intent(out) :: svec
            !dir$ attributes align : 64 :: svec
            ! Exec code ....
            svec.s0 = x.s0
            svec.s1 = x.s1
            svec.s2 = x.s2
            svec.s3 = x.s3
         end subroutine  SVec4x16v16_set_5_v2


         pure function SVec4x16v16_normal_Q(jv) result(v16)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_Q
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_Q
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_Q
            type(SVec4x16v16),  intent(in) :: jv
            type(ZMM16r4_t)  :: v16
            !dir$ attributes align : 64 :: v16
            v16.v = jv.s1.v/jv.s0.v
         end function SVec4x16v16_normal_Q


         subroutine SVec4x16v16_normal_Q_v2(jv,v16) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_Q_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_Q_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_Q_v2
            type(SVec4x16v16),  intent(in)   :: jv
            type(ZMM16r4_t),    intent(out)  :: v16
            v16.v = jv.s1.v/jv.s0.v
         end subroutine SVec4x16v16_normal_Q_v2


         pure function SVec4x16v16_normal_U(jv) result(v16)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_U
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_U
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_U
            type(SVec4x16v16),  intent(in) :: jv
            type(ZMM16r4_t)  :: v16
            !dir$ attributes align : 64 :: v16
            v16.v = jv.s2.v/jv.s0.v
         end function SVec4x16v16_normal_U


         subroutine SVec4x16v16_normal_U_v2(jv,v16) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_U_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_U_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_U_v2
            type(SVec4x16v16),  intent(in)   :: jv
            type(ZMM16r4_t),    intent(out)  :: v16
            v16.v = jv.s2.v/jv.s0.v
         end subroutine SVec4x16v16_normal_U_v2

          
         pure function SVec4x16v16_normal_V(jv) result(v16)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_V
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_V
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_V
            type(SVec4x16v16),  intent(in) :: jv
            type(ZMM16r4_t)  :: v16
            !dir$ attributes align : 64 :: v16
            v16.v = jv.s3.v/jv.s0.v
         end function SVec4x16v16_normal_V


         subroutine SVec4x16v16_normal_V_v2(jv,v16) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_normal_V_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_normal_V_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_normal_V_v2
            type(SVec4x16v16),  intent(in)   :: jv
            type(ZMM16r4_t),    intent(out)  :: v16
            v16.v = jv.s3.v/jv.s0.v
         end subroutine SVec4x16v16_normal_V_v2

          
         pure function SVec4x16v16_add_SVec4x16v16(sv1,sv2) result(sv)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_add_SVec4x16v16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_add_SVec4x16v16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_add_SVec4x16v16
            type(SVec4x16v16),  intent(in) :: sv1
            type(SVec4x16v16),  intent(in) :: sv2
            type(SVec4x16v16) :: sv
            sv.s0.v = sv1.s0.v+sv2.s0.v
            sv.s1.v = sv1.s1.v+sv2.s1.v
            sv.s2.v = sv1.s2.v+sv2.s2.v
            sv.s3.v = sv1.s3.v+sv2.s3.v
         end function SVec4x16v16_add_SVec4x16v16


         subroutine SVec4x16v16_add_SVec4x16v16_v2(sv1,sv2,sv) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_add_SVec4x16v16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_add_SVec4x16v16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_add_SVec4x16v16_v2
            type(SVec4x16v16),  intent(in)  :: sv1
            type(SVec4x16v16),  intent(in)  :: sv2
            type(SVec4x16v16),  intent(out) :: sv
            sv.s0.v = sv1.s0.v+sv2.s0.v
            sv.s1.v = sv1.s1.v+sv2.s1.v
            sv.s2.v = sv1.s2.v+sv2.s2.v
            sv.s3.v = sv1.s3.v+sv2.s3.v
         subroutine SVec4x16v16_add_SVec4x16v16_v2

              
         subroutine SVec4x16v16_add_SVec4x16v16_v3(sv1,sv2) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_add_SVec4x16v16_v3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_add_SVec4x16v16_v3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_add_SVec4x16v16_v3
            type(SVec4x16v16),  intent(inout)  :: sv1
            type(SVec4x16v16),  intent(in)     :: sv2
            
            sv1.s0.v = sv1.s0.v+sv2.s0.v
            sv1.s1.v = sv1.s1.v+sv2.s1.v
            sv1.s2.v = sv1.s2.v+sv2.s2.v
            sv1.s3.v = sv1.s3.v+sv2.s3.v
         subroutine SVec4x16v16_add_SVec4x16v16_v3


          
         pure function SVec4x16v16_sub_SVec4x16v16(sv1,sv2) result(sv)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_sub_SVec4x16v16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_sub_SVec4x16v16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_sub_SVec4x16v16
            type(SVec4x16v16),  intent(in) :: sv1
            type(SVec4x16v16),  intent(in) :: sv2
            type(SVec4x16v16) :: sv
            sv.s0.v = sv1.s0.v-sv2.s0.v
            sv.s1.v = sv1.s1.v-sv2.s1.v
            sv.s2.v = sv1.s2.v-sv2.s2.v
            sv.s3.v = sv1.s3.v-sv2.s3.v
         end function SVec4x16v16_sub_SVec4x16v16


         subroutine SVec4x16v16_sub_SVec4x16v16_v2(sv1,sv2,sv) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_sub_SVec4x16v16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_sub_SVec4x16v16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_sub_SVec4x16v16_v2
            type(SVec4x16v16),  intent(in)  :: sv1
            type(SVec4x16v16),  intent(in)  :: sv2
            type(SVec4x16v16),  intent(out) :: sv
            sv.s0.v = sv1.s0.v-sv2.s0.v
            sv.s1.v = sv1.s1.v-sv2.s1.v
            sv.s2.v = sv1.s2.v-sv2.s2.v
            sv.s3.v = sv1.s3.v-sv2.s3.v
         subroutine SVec4x16v16_sub_SVec4x16v16_v2

              
         subroutine SVec4x16v16_sub_SVec4x16v16_v3(sv1,sv2) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_sub_SVec4x16v16_v3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_sub_SVec4x16v16_v3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_sub_SVec4x16v16_v3
            type(SVec4x16v16),  intent(inout)  :: sv1
            type(SVec4x16v16),  intent(in)     :: sv2
            
            sv1.s0.v = sv1.s0.v-sv2.s0.v
            sv1.s1.v = sv1.s1.v-sv2.s1.v
            sv1.s2.v = sv1.s2.v-sv2.s2.v
            sv1.s3.v = sv1.s3.v-sv2.s3.v
         subroutine SVec4x16v16_sub_SVec4x16v16_v3


         pure function SVec4x16v16_negate(sv1) result(sv)

             !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_negate
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_negate
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_negate
            type(SVec4x16v16),  intent(in) :: sv1
            type(SVec4x16v16) :: sv
            type(ZMM16r4_t), parameter :: zero = ZMM16r4_t(0.0_sp)
            sv.s0 = zero.v-sv1.s0.v
            sv.s1 = zero.v-sv1.s1.v
            sv.s2 = zero.v-sv1.s2.v
            sv.s3 = zero.v-sv1.s3.v
         end function SVec4x16v16_negate


         pure function SVec4x16v16_dot(sv1,sv2) result(dotp)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_dot
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_dot
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_dot
            type(SVec4x16v16),  intent(in) :: sv1
            type(SVec4x16v16),  intent(in) :: sv2
            type(ZMM16r4_t) :: dotp
            !dir$ attributes align : 64 :: dotp
            type(ZMM16r4_t), automatic :: t0,t1
            !dir$ attributes align : 64 :: t0,t1
            t0 = sv1.s0.v*sv2.s0.v+sv1.s1.v*sv2.s1.v
            t1 = sv1.s2.v*sv2.s2.v+sv1.s3.v*sv2.s3.v
            dotp.v = t1.v+t0.v
         end function SVec4x16v16_dot


         subroutine SVec4x16v16_dot_v2(sv1,sv2,dotp)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_dot_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_dot_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_dot_v2
            type(SVec4x16v16),  intent(in) :: sv1
            type(SVec4x16v16),  intent(in) :: sv2
            type(ZMM16r4_t),    intent(out) :: dotp
            !dir$ attributes align : 64 :: dotp
            type(ZMM16r4_t), automatic :: t0,t1
            !dir$ attributes align : 64 :: t0,t1
            t0 = sv1.s0.v*sv2.s0.v+sv1.s1.v*sv2.s1.v
            t1 = sv1.s2.v*sv2.s2.v+sv1.s3.v*sv2.s3.v
            dotp.v = t1.v+t0.v
         subroutine SVec4x16v16_dot_v2


         pure function SVec4x16v16_mul_ZMM16r4(sv,v16) result(svv)
             
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_mul_ZMM16r4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_mul_ZMM16r4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_mul_ZMM16r4
            type(SVec4x16v16),   intent(in) :: sv
            type(ZMM16r4_t),     intent(in) :: v16
            type(SVec4x16v16) :: svv
            svv.s0 = sv.s0.v*v16.v
            svv.s1 = sv.s1.v*v16.v
            svv.s2 = sv.s2.v*v16.v
            svv.s3 = sv.s3.v*v16.v
         end function SVec4x16v16_mul_ZMM16r4


         subroutine SVec4x16v16_mul_ZMM16r4_v2(sv,v16,svv) 
             
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: SVec4x16v16_mul_ZMM16r4_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SVec4x16v16_mul_ZMM16r4_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: SVec4x16v16_mul_ZMM16r4_v2
            type(SVec4x16v16),   intent(in) :: sv
            type(ZMM16r4_t),     intent(in) :: v16
            type(SVec4x16v16),   intent(out) :: svv
            svv.s0 = sv.s0.v*v16.v
            svv.s1 = sv.s1.v*v16.v
            svv.s2 = sv.s2.v*v16.v
            svv.s3 = sv.s3.v*v16.v
         subroutine SVec4x16v16_mul_ZMM16r4_v2

         
          
         
 

          

        

        


        
     
     


    
    







end module mueller_calculus_avx512
