

module mueller_calculus_sse


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mueller_calculus_sse'
 !          
 !          Purpose:
  !                       Mueller calculus based mainly on
  !                       // Based on: R.A. Chipman, "Polarimetry" chapter in Handbook of Optics Volume 2
 !                        // (McGraw-Hill, New York, 1995).
 !                     
 !
 !          History:
 !                        
 !                        Date: 31-08-2024
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
     use mod_vectypes,      only : XMM4r4_t, XMM2r8_t
     use sse_cvec4,         only : XMM4c4_t
     use sse_cvec2,         only : XMM2c8_t
     use mueller_types_sse
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_SSE_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_SSE_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_SSE_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_SSE_FULLVER = 1000*MUELLER_CALCULUS_SSE_MAJOR+100*MUELLER_CALCULUS_SSE_MINOR+ &
                                             10*MUELLER_CALCULUS_SSE_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MUELLER_TYPES_SSE_CREATE_DATE = "31-08-2024 11:51AM +00200 (SAT 31 AUG 2024 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MUELLER_TYPES_SSE_BUILD_DATE = __DATE__ ":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MUELLER_TYPES_SSE_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MUELLER_TYPES_SSE_SYNOPSIS = " Mueller calculus implementation"

    type(XMM4c4_t), parameter, private :: j     = XMM4c4_t(0.0_sp,-1.0_sp)
    type(XMM4c4_t), parameter, private :: czero = XMM4c4_t()

  contains

    !=============================================================
    !
    ! /*
    !    Jones-Vector components -- packed single-precision
    !    4-tuple vectors of complex numbers [deinterleaved]
    !  */
    !==============================================================

    pure function JVec2x4c4_set_1() result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_set_1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_set_1
#endif
       type(JVec2x4c4) :: cvec
      ! EXEC CODE ....
       cvec.p = XMM4c4_t()
       cvec.s = XMM4c4_t()
    end function JVec2x4c4_set_1
    

    pure function JVec2x4c4_set_2(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_set_2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_set_2
#endif
        complex(kind=sp),    intent(in) :: c1
        complex(kind=sp),    intent(in) :: c2
        type(JVec2x4c4) :: cvec
        !Exec code ....
        cvec.p = complex1_init(c1)
        cvec.s = complex1_init(c2)
    end function JVec2x4c4_set_2

   
    pure function JVec2x4c4_set_3(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_set_3
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_set_3
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_set_3
#endif
        complex(kind=sp), dimension(0:3), intent(in) :: c1
        complex(kind=sp), dimension(0:3), intent(in) :: c2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED c1:32
        !DIR$ ASSUME_ALIGNED c2:32
#endif
        type(JVec2x4c4) :: cvec
        ! Exec code ....
        cvec.p = complex2x4_init(c1)
        cvec.s = complex2x4_init(c2)
    end function JVec2x4c4_set_3


    pure function JVec2x4c4_set_4(v1,v2,v3,v4) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_set_4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_set_4
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_set_4
#endif
        type(XMM4r4_t),    intent(in) :: v1
        type(XMM4r4_t),    intent(in) :: v2
        type(XMM4r4_t),    intent(in) :: v3
        type(XMM4r4_t),    intent(in) :: v4
        type(JVec2x4c4) :: cvec
        ! Exec code ....
        cvec.p = xmm4r42x_init(v1,v2)
        cvec.s = xmm4r42x_init(v3,v4)
    end function JVec2x4c4_set_4


    pure function JVec2x4c4_set_5(v1,v2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_set_5
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_set_5
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_set_5
#endif
        type(XMM4r4_t),    intent(in) :: v1
        type(XMM4r4_t),    intent(in) :: v2
        type(JVec2x4c4) :: cvec
        ! Exec code ...
        cvec.p = xmm4r41x_init(v1)
        cvec.s = xmm4r41x_init(v2)
    end function JVec2x4c4_set_5
    

    pure function JVec2x4c4_copy(other) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_copy
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_copy
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_copy
#endif
       type(JVec2x4c4),   intent(in) :: other
       type(JVec2x4c4) :: this
       ! Exec code ....
       this.p = copy_init(other.p)
       this.s = copy_init(other.s)
    end function JVec2x4c4_copy


    pure function JVec2x4c4_mul_JVec2x4c4(jv1,jv2) result(cv)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_mul_JVec2x4c4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_mul_JVec2x4c4
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_mul_JVec2x4c4
#endif
         type(JVec2x4c4),   intent(in) :: jv1
         type(JVec2x4c4),   intent(in) :: jv2
         type(XMM4c4_t) :: cv
         ! Exec code ....
         cv = jv1.p*jv2.p+jv1.s*jv2.s
    end function JVec2x4c4_mul_JVec2x4c4
    


    pure function JVec2x4c4_mul_XMM4c4_t(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_mul_XMM4c4_t
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_mul_XMM4c4_t
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_mul_XMM4c4_t
#endif
        type(JVec2x4c4),   intent(in) :: jv
        type(XMM4c4_t),       intent(in) :: c
        type(JVec2x4c4) :: this
        ! Exec code ...
        this.p = jv.p*c
        this.s = jv.s*c
     end function JVec2x4c4_mul_XMM4c4_t

      
     subroutine JVec2x4c4_mul_XMM4c4_t_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_mul_XMM4c4_t_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_mul_XMM4c4_t_v2
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_mul_XMM4c4_t_v2
#endif
         type(JVec2x4c4),   intent(inout) :: jv
         type(XMM4c4_t),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p*c
         jv.s = jv.s*c
     end subroutine JVec2x4c4_mul_XMM4c4_t_v2


     pure function JVec2x4c4_div_XMM4c4_t(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_div_XMM4c4_t
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_div_XMM4c4_t
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_div_XMM4c4_t
#endif
         type(JVec2x4c4),     intent(in) :: jv
         type(XMM4c4_t),         intent(in) :: c
         type(JVec2x4c4) :: this
         ! Exec code ....
         this.p = jv.p/c
         this.s = jv.s/c
      end function JVec2x4c4_div_XMM4c4_t


      subroutine JVec2x4c4_div_XMM4c4_t_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_div_XMM4c4_t_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_div_XMM4c4_t_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_div_XMM4c4_t_v2
#endif
         type(JVec2x4c4),   intent(inout) :: jv
         type(XMM4c4_t),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p/c
         jv.s = jv.s/c
     end subroutine JVec2x4c4_div_XMM4c4_t_v2 
     
       
     pure function JVec2x4c4_conjugate(jv) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_conjugate
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_conjugate
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_conjugate
#endif
          type(JVec2x4c4),   intent(in) :: jv
          type(JVec2x4c4) :: this
          ! Exec code ....
          this.p = conjugate(jv.p)
          this.s = conjugate(jv.s)
      end function JVec2x4c4_conjugate


      pure function JVec2x4c4_add_JVec2x4c4(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_add_JVec2x4c4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_add_JVec2x4c4
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_add_JVec2x4c4
#endif
          type(JVec2x4c4),    intent(in) :: jv1
          type(JVec2x4c4),    intent(in) :: jv2
          type(JVec2x4c4) :: this
          ! Exec code ....
          this.p = jv1.p+jv2.p
          this.s = jv1.s+jv2.s
      end function JVec2x4c4_add_JVec2x4c4


      subroutine JVec2x4c4_add_Jvec2x8c14_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_add_JVec2x4c4_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_add_JVec2x4c4_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_add_JVec2x4c4_v2
#endif
          type(JVec2x4c4),    intent(inout) :: jv
          type(JVec2x4c4),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p+jv1.p
          jv.s = jv.s+jv1.s
      end subroutine JVec2x4c4_add_Jvec2x8c4_v2
      
      
      pure function JVec2x4c4_sub_JVec2x4c4(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_sub_JVec2x4c4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_sub_JVec2x4c4
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_sub_JVec2x4c4
#endif
          type(JVec2x4c4),    intent(in) :: jv1
          type(JVec2x4c4),    intent(in) :: jv2
          type(JVec2x4c4) :: this
          ! Exec code ....
          this.p = jv1.p-jv2.p
          this.s = jv1.s-jv2.s
      end function JVec2x4c4_sub_JVec2x4c4


      subroutine JVec2x4c4_sub_Jvec2x8c4_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_sub_JVec2x4c4_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_sub_JVec2x4c4_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_sub_JVec2x4c4_v2
#endif
          type(JVec2x4c4),    intent(inout) :: jv
          type(JVec2x4c4),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p-jv1.p
          jv.s = jv.s-jv1.s
      end subroutine JVec2x4c4_sub_Jvec2x8c4_v2


      pure function JVec2x4c4_norm(jv) result(norm)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_norm
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_norm
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_norm
#endif
          type(JVec2x4c4),   intent(in) :: jv
          type(XMM4r4_t) :: norm
          ! Locals
          type(XMM4r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1 = cnorm_xmm4c4(jv.p)
          v2 = cnorm_xmm4c4(jv.s)
          cn.v = v1.v+v2.v
       end function JVec2x4c4_norm


       pure function JVec2x4c4_psi(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_psi
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_psi
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_psi
#endif
           type(JVec2x4c4),   intent(in) :: jv
           type(XMM4r4_t) :: v
           ! LOcals
           type(XMM4r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = cabs_xmm4c4(jv.p)
          v2.v = cabs_xmm4c4(jv.s)
          v.v  = atan(v1.v/v2.v)
       end function JVec2x4c4_psi

        
       pure function JVec2x4c4_delta(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_delta
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_delta
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_delta
#endif
          type(JVec2x4c4), intent(in) :: jv
          type(XMM4r4_t) :: v
          ! Locals
          type(XMM4r4_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = carg_xmm4c4(jv.p)
          v2.v = carg_xmm4c4(jv.s)
          v.v  = v1.v-v2.v
       end function JVec2x4c4_delta
       
       !==============================================
       !  Double precision 2-tuple complex vector.
       !==============================================
        
       
      pure function JVec2x2c8_set_1() result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_set_1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x8c18_set_1
#endif
       type(JVec2x2c8) :: cvec
      ! EXEC CODE ....
       cvec.p = XMM2c8_t()
       cvec.s = XMM2c8_t()
    end function JVec2x2c8_set_1
    

    pure function JVec2x2c8_set_2(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_set_2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_set_2
#endif
        complex(kind=dp),    intent(in) :: c1
        complex(kind=dp),    intent(in) :: c2
        type(JVec2x2c8) :: cvec
        !Exec code ....
        cvec.p = complex1_init(c1)
        cvec.s = complex1_init(c2)
    end function JVec2x2c8_set_2

   
    pure function JVec2x2c8_set_3(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_set_3
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_set_3
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_set_3
#endif
        complex(kind=dp), dimension(0:3), intent(in) :: c1
        complex(kind=dp), dimension(0:3), intent(in) :: c2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED c1:32
        !DIR$ ASSUME_ALIGNED c2:32
#endif
        type(JVec2x2c8) :: cvec
        ! Exec code ....
        cvec.p = complex2x4_init(c1)
        cvec.s = complex2x4_init(c2)
    end function JVec2x2c8_set_3


    pure function JVec2x2c8_set_4(v1,v2,v3,v4) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_set_4
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_set_4
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_set_4
#endif
        type(XMM2r8_t),    intent(in) :: v1
        type(XMM2r8_t),    intent(in) :: v2
        type(XMM2r8_t),    intent(in) :: v3
        type(XMM2r8_t),    intent(in) :: v4
        type(JVec2x2c8) :: cvec
        ! Exec code ....
        cvec.p = xmm2r82x_init(v1,v2)
        cvec.s = xmm2r82x_init(v3,v4)
    end function JVec2x2c8_set_4


    pure function JVec2x2c8_set_5(v1,v2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_set_5
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_set_5
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_set_5
#endif
        type(XMM2r8_t),    intent(in) :: v1
        type(XMM2r8_t),    intent(in) :: v2
        type(JVec2x2c8) :: cvec
        ! Exec code ...
        cvec.p = xmm2r81x_init(v1)
        cvec.s = xmm2r81x_init(v2)
    end function JVec2x2c8_set_5
    

    pure function JVec2x2c8_copy(other) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_copy
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_copy
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_copy
#endif
       type(JVec2x2c8),   intent(in) :: other
       type(JVec2x2c8) :: this
       ! Exec code ....
       this.p = copy_init(other.p)
       this.s = copy_init(other.s)
    end function JVec2x2c8_copy


    pure function JVec2x2c8_mul_JVec2x2c8(jv1,jv2) result(cv)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_mul_JVec2x2c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_mul_JVec2x2c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_mul_JVec2x2c8
#endif
         type(JVec2x2c8),   intent(in) :: jv1
         type(JVec2x2c8),   intent(in) :: jv2
         type(XMM2c8_t) :: cv
         ! Exec code ....
         cv = jv1.p*jv2.p+jv1.s*jv2.s
    end function JVec2x2c8_mul_JVec2x2c8
    


    pure function JVec2x2c8_mul_XMM2c8_t(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_mul_XMM2c8_t
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_mul_XMM2c8_t
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_mul_XMM2c8_t
#endif
        type(JVec2x2c8),   intent(in) :: jv
        type(XMM2c8_t),       intent(in) :: c
        type(JVec2x2c8) :: this
        ! Exec code ...
        this.p = jv.p*c
        this.s = jv.s*c
     end function JVec2x2c8_mul_XMM2c8_t

      
     subroutine JVec2x2c8_mul_XMM2c8_t_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_mul_XMM2c8_t_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_mul_XMM2c8_t_v2
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_mul_XMM2c8_t_v2
#endif
         type(JVec2x2c8),   intent(inout) :: jv
         type(XMM2c8_t),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p*c
         jv.s = jv.s*c
     end subroutine JVec2x2c8_mul_XMM2c8_t_v2


     pure function JVec2x2c8_div_XMM2c8_t(jv,c) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_div_XMM2c8_t
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_div_XMM2c8_t
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_div_XMM2c8_t
#endif
         type(JVec2x2c8),     intent(in) :: jv
         type(XMM2c8_t),         intent(in) :: c
         type(JVec2x2c8) :: this
         ! Exec code ....
         this.p = jv.p/c
         this.s = jv.s/c
      end function JVec2x2c8_div_XMM2c8_t


      subroutine JVec2x2c8_div_XMM2c8_t_v2(jv,c)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_div_XMM2c8_t_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_div_XMM2c8_t_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_div_XMM2c8_t_v2
#endif
         type(JVec2x2c8),   intent(inout) :: jv
         type(XMM2c8_t),       intent(in)    :: c
         ! Exec code ....
         jv.p = jv.p/c
         jv.s = jv.s/c
     end subroutine JVec2x2c8_div_XMM2c8_t_v2 
     
       
     pure function JVec2x2c8_conjugate(jv) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_conjugate
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_conjugate
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_conjugate
#endif
          type(JVec2x2c8),   intent(in) :: jv
          type(JVec2x2c8) :: this
          ! Exec code ....
          this.p = conjugate(jv.p)
          this.s = conjugate(jv.s)
      end function JVec2x2c8_conjugate


      pure function JVec2x2c8_add_JVec2x2c8(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_add_JVec2x2c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_add_JVec2x2c8
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_add_JVec2x2c8
#endif
          type(JVec2x2c8),    intent(in) :: jv1
          type(JVec2x2c8),    intent(in) :: jv2
          type(JVec2x2c8) :: this
          ! Exec code ....
          this.p = jv1.p+jv2.p
          this.s = jv1.s+jv2.s
      end function JVec2x2c8_add_JVec2x2c8


      subroutine JVec2x2c8_add_Jvec2x8c8_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_add_JVec2x2c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_add_JVec2x2c8_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_add_JVec2x2c8_v2
#endif
          type(JVec2x2c8),    intent(inout) :: jv
          type(JVec2x2c8),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p+jv1.p
          jv.s = jv.s+jv1.s
      end subroutine JVec2x2c8_add_Jvec2x8c8_v2
      
      
      pure function JVec2x2c8_sub_JVec2x2c8(jv1,jv2) result(this)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_sub_JVec2x2c8
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_sub_JVec2x2c8
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_sub_JVec2x2c8
#endif
          type(JVec2x2c8),    intent(in) :: jv1
          type(JVec2x2c8),    intent(in) :: jv2
          type(JVec2x2c8) :: this
          ! Exec code ....
          this.p = jv1.p-jv2.p
          this.s = jv1.s-jv2.s
      end function JVec2x2c8_sub_JVec2x2c8


      subroutine JVec2x2c8_sub_Jvec2x4c8_v2(jv,jv1)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_sub_JVec2x2c8_v2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_sub_JVec2x2c8_v2
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_sub_JVec2x2c8_v2
#endif
          type(JVec2x2c8),    intent(inout) :: jv
          type(JVec2x2c8),    intent(in)    :: jv1
          ! Exec code ....
          jv.p = jv.p-jv1.p
          jv.s = jv.s-jv1.s
      end subroutine JVec2x2c8_sub_Jvec2x4c8_v2


      pure function JVec2x2c8_norm(jv) result(norm)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_norm
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_norm
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_norm
#endif
          type(JVec2x2c8),   intent(in) :: jv
          type(XMM2r8_t) :: norm
          ! Locals
          type(XMM2r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1 = cnorm_xmm2c8(jv.p)
          v2 = cnorm_xmm2c8(jv.s)
          cn.v = v1.v+v2.v
       end function JVec2x2c8_norm


       pure function JVec2x2c8_psi(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_psi
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_psi
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_psi
#endif
           type(JVec2x2c8),   intent(in) :: jv
           type(XMM2r8_t) :: v
           ! LOcals
           type(XMM2r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = cabs_xmm2c8(jv.p)
          v2.v = cabs_xmm2c8(jv.s)
          v.v  = atan(v1.v/v2.v)
       end function JVec2x2c8_psi

        
       pure function JVec2x2c8_delta(jv) result(v)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x2c8_delta
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x2c8_delta
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x2c8_delta
#endif
          type(JVec2x2c8), intent(in) :: jv
          type(XMM2r8_t) :: v
          ! Locals
          type(XMM2r8_t), automatic :: v1,v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
          !DIR$ ATTRIBUTES ALIGN : 64 :: v1,v2
#endif
          ! Exec code ....
          v1.v = carg_xmm2c8(jv.p)
          v2.v = carg_xmm2c8(jv.s)
          v.v  = v1.v-v2.v
       end function JVec2x2c8_delta
      
        
#if 0

/*
                         Jones Matrix implementation based on SIMD 
                         16-tuple complex vector [deinterleaved]
                         single-precision.
                         @Reference
                           
                                  typedef struct  __ATTR_ALIGN__(64)  JMat4x4c4 {
                                      XMM4c4_t pp; j0
				      XMM4c4_t ss; j1
				      XMM4c4_t ps; j2
				      XMM4c4_t sp; j3
		                 } JMat4x4c4;
                                 typedef struct __ATTR_ALIGN__(64)  JMat4x8c8 {
                                       XMM2c8_t pp;
				       XMM2c8_t ss;
				       XMM2c8_t ps;
				       XMM2c8_t sp;
		                }JMat4x8c8;
                  */

#endif       
       
       !==================================================!
       ! Notification!!
       ! Subroutine version shall be used in case of 
       ! failed vectorization of argument passing and
       ! result returning through XMM registers.
       !==================================================!

       pure function  JMat4x4c4_set_1() result(mat)

      !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_1
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_1

             type( JMat4x4c4) :: mat
             !dir$ attributes align : 16 :: mat
             !Executable code ....
             mat.j0 = default_init() ! 'pp' component
             mat.j1 = default_init() ! 'ss' component
             mat.j2 = default_init() ! 'ps' component
             mat.j3 = default_init() ! 'sp' component
        end function  JMat4x4c4_set_1

       ! Subroutine version shall be used in case of 
       ! failed vectorization of argument passing and
       ! result returning through ZMM registers.

       
       subroutine  JMat4x4c4_set1(mat) 

      !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set1
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set1

             type( JMat4x4c4),  intent(out) :: mat
            
             !Executable code ....
             mat.j0 = default_init() ! 'pp' component
             mat.j1 = default_init() ! 'ss' component
             mat.j2 = default_init() ! 'ps' component
             mat.j3 = default_init() ! 'sp' component
        end subroutine  JMat4x4c4_set1

        pure function  JMat4x4c4_set_2(pp,ss,ps,sp) result(mat)
      !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_2
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_2
             complex(kind=sp), intent(in) :: pp
             complex(kind=sp), intent(in) :: ss
             complex(kind=sp), intent(in) :: ps
             complex(kind=sp), intent(in) :: sp
             type( JMat4x4c4) :: mat
             !dir$ attributes align : 16 :: mat
             ! Executable code ....
             mat.j0 = complex1_init(pp)
             mat.j1 = complex1_init(ss)
             mat.j2 = complex1_init(ps)
             mat.j3 = complex1_init(sp)
        end function  JMat4x4c4_set_2


        subroutine  JMat4x4c4_set2(pp,ss,ps,sp,mat) 
      !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set2
      !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set2
             complex(kind=sp),  intent(in) :: pp
             complex(kind=sp),  intent(in) :: ss
             complex(kind=sp),  intent(in) :: ps
             complex(kind=sp),  intent(in) :: sp
             type( JMat4x4c4),intent(out) :: mat
             
             ! Executable code ....
             mat.j0 = complex1_init(pp)
             mat.j1 = complex1_init(ss)
             mat.j2 = complex1_init(ps)
             mat.j3 = complex1_init(sp)
        end subroutine  JMat4x4c4_set2


        pure function  JMat4x4c4_set_3(re1,im1,re2,im2,  &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_3
            real(kind=sp), dimension(0:3), intent(in) :: re1
            real(kind=sp), dimension(0:3), intent(in) :: im1
            real(kind=sp), dimension(0:3), intent(in) :: re2
            real(kind=sp), dimension(0:3), intent(in) :: im2
            real(kind=sp), dimension(0:3), intent(in) :: re3
            real(kind=sp), dimension(0:3), intent(in) :: im3
            real(kind=sp), dimension(0:3), intent(in) :: re4
            real(kind=sp), dimension(0:3), intent(in) :: im4
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            !Executable code ....
            mat.j0 = array_init(re1,im1)
            mat.j1 = array_init(re2,im2)
            mat.j2 = array_init(re3,im3)
            mat.j3 = array_init(re4,im4)
        end function  JMat4x4c4_set_3


        subroutine  JMat4x4c4_set3(re1,im1,re2,im2,  &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set3
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set3
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set3
            real(kind=sp), dimension(0:3), intent(in) :: re1
            real(kind=sp), dimension(0:3), intent(in) :: im1
            real(kind=sp), dimension(0:3), intent(in) :: re2
            real(kind=sp), dimension(0:3), intent(in) :: im2
            real(kind=sp), dimension(0:3), intent(in) :: re3
            real(kind=sp), dimension(0:3), intent(in) :: im3
            real(kind=sp), dimension(0:3), intent(in) :: re4
            real(kind=sp), dimension(0:3), intent(in) :: im4
            type( JMat4x4c4),              intent(out) :: mat
        
            !Executable code ....
            mat.j0 = array_init(re1,im1)
            mat.j1 = array_init(re2,im2)
            mat.j2 = array_init(re3,im3)
            mat.j3 = array_init(re4,im4)
        end subroutine  JMat4x4c4_set_3


        pure function  JMat4x4c4_set_4(re1,im1,re2,im2,   &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_4
            real(kind=sp),      intent(in) :: re1
            real(kind=sp),      intent(in) :: im1
            real(kind=sp),      intent(in) :: re2
            real(kind=sp),      intent(in) :: im2
            real(kind=sp),      intent(in) :: re3
            real(kind=sp),      intent(in) :: im3
            real(kind=sp),      intent(in) :: re4
            real(kind=sp),      intent(in) :: im4
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
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
        end function  JMat4x4c4_set_4


        subroutine  JMat4x4c4_set4(re1,im1,re2,im2,   &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_4
            real(kind=sp),      intent(in) :: re1
            real(kind=sp),      intent(in) :: im1
            real(kind=sp),      intent(in) :: re2
            real(kind=sp),      intent(in) :: im2
            real(kind=sp),      intent(in) :: re3
            real(kind=sp),      intent(in) :: im3
            real(kind=sp),      intent(in) :: re4
            real(kind=sp),      intent(in) :: im4
            type( JMat4x4c4),  intent(out) :: mat
            
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
        end subroutine  JMat4x4c4_set4


        pure function  JMat4x4c4_set_5(re1,im1,re2,im2,   &
                                        re3,im3,re4,im4)  result(mat)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set_5
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set_5
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set_5
            type(XMM4r4_t),  intent(in) :: re1
            type(XMM4r4_t),  intent(in) :: im1
            type(XMM4r4_t),  intent(in) :: re2
            type(XMM4r4_t),  intent(in) :: im2
            type(XMM4r4_t),  intent(in) :: re3
            type(XMM4r4_t),  intent(in) :: im3
            type(XMM4r4_t),  intent(in) :: re4
            type(XMM4r4_t),  intent(in) :: im4
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code .....
            mat.j0 =  xmm4r42x_init_init(re1,im1)
            mat.j1 =  xmm4r42x_init_init(re2,im2)
            mat.j2 =  xmm4r42x_init_init(re3,im3)
            mat.j3 =  xmm4r42x_init_init(re4,im4)
        end function  JMat4x4c4_set_5


        subroutine  JMat4x4c4_set5(re1,im1,re2,im2,   &
                                    re3,im3,re4,im4,mat)  
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_set5
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_set5
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_set5
            type(XMM4r4_t),  intent(in) :: re1
            type(XMM4r4_t),  intent(in) :: im1
            type(XMM4r4_t),  intent(in) :: re2
            type(XMM4r4_t),  intent(in) :: im2
            type(XMM4r4_t),  intent(in) :: re3
            type(XMM4r4_t),  intent(in) :: im3
            type(XMM4r4_t),  intent(in) :: re4
            type(XMM4r4_t),  intent(in) :: im4
            type( JMat4x4c4),intent(out) :: mat
          
            ! Executable code .....
            mat.j0 =  xmm4r42x_init_init(re1,im1)
            mat.j1 =  xmm4r42x_init(re2,im2)
            mat.j2 =  xmm4r42x_init(re3,im3)
            mat.j3 =  xmm4r42x_init(re4,im4)
        end subroutine  JMat4x4c4_set5

        
        pure function  JMat4x4c4_copy(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_copy
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_copy
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_copy
            type( JMat4x4c4),  intent(in) :: x
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code ....
            mat.j0 = copy_init(x.j0)
            mat.j1 = copy_init(x.j1)
            mat.j2 = copy_init(x.j2)
            mat.j3 = copy_init(x.j3)
        end function


        subroutine  JMat4x4c4_copy_v2(x,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_copy_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_copy_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_copy_v2
            type( JMat4x4c4),  intent(in) :: x
            type( JMat4x4c4),  intent9out) :: mat
           
            ! Executable code ....
            mat.j0 = copy_init(x.j0)
            mat.j1 = copy_init(x.j1)
            mat.j2 = copy_init(x.j2)
            mat.j3 = copy_init(x.j3)
        end subroutine  JMat4x4c4_copy_v2


        ! Jones-Matrix multiplication
        !     mat.j0 = default_init() ! 'pp' component
        !     mat.j1 = default_init() ! 'ss' component
        !     mat.j2 = default_init() ! 'ps' component
        !     mat.j3 = default_init() ! 'sp' component
        ! Multiply two Jones Matrices, i.e. of type  JMat4x4c4

        pure function  JMat4x4c4_matmul_v1(m1,m2) result(mat)
          
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_matmul_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_matmul_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_matmul_v1
            type( JMat4x4c4),  intent(in) :: m1
            type( JMat4x4c4),  intent(in) :: m2
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code ....
            mat.j0 = m1.j3*m2.j2+m1.j0*m2.j0
            mat.j1 = m1.j1*m2.j1+m1.j2*m2.j1
            mat.j2 = m1.j1*m2.j2+m1.j2*m2.j0
            mat.j3 = m1.j3*m2.j1+m1.j0*m2.j3
        end function  JMat4x4c4_matmul_v1


        subroutine  JMat4x4c4_matmul_v2(m1,m2,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_matmul_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_matmul_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_matmul_v2
            type( JMat4x4c4),  intent(in)  :: m1
            type( JMat4x4c4),  intent(in)  :: m2
            type( JMat4x4c4),  intent(out) :: mat
            ! Executable code ...
            mat.j0 = m1.j3*m2.j2+m1.j0*m2.j0
            mat.j1 = m1.j1*m2.j1+m1.j2*m2.j1
            mat.j2 = m1.j1*m2.j2+m1.j2*m2.j0
            mat.j3 = m1.j3*m2.j1+m1.j0*m2.j3
        end subroutine  JMat4x4c4_matmul_v2


        ! Jones Matrix multiplied by contant i.e. complex 16-tuple vector
        function  JMat4x4c4_vecmul_v1(m,v) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_vecmul_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_vecmul_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_vecmul_v1
            type( JMat4x4c4),  intent(in) :: m
            type(XMM4c4_t),      intent(in) :: v
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code .....
            mat.j0 = m.j0*x
            mat.j1 = m.j1*x
            mat.j2 = m.j2*x
            mat.j3 = m.j3*x
        end function  JMat4x4c4_vecmul_v1        


         ! Jones Matrix multiplied by contant i.e. complex 16-tuple vector
        subroutine  JMat4x4c4_vecmul_v2(m,v,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_vecmul_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_vecmul_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_vecmul_v2
            type( JMat4x4c4),  intent(in)  :: m
            type(XMM4c4_t),      intent(in)  :: v
            type( JMat4x4c4),  intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code .....
            mat.j0 = m.j0*x
            mat.j1 = m.j1*x
            mat.j2 = m.j2*x
            mat.j3 = m.j3*x
        end subroutine  JMat4x4c4_vecmul_v2   


        !Multiplication by Jones Vector     
        pure function JVec2x4c4_mul_ JMat4x4c4(v,m) result(jvec)
            
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_mul_ JMat4x4c4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_mul_ JMat4x4c4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_mul_ JMat4x4c4
            type(JVec2x4c4),  intent(in) :: v
            type( JMat4x4c4),  intent(in) :: m
            type(JVec2x4c4) :: jvec
            !dir$ attributes align : 16 :: mat
            ! Executable code ....
            jvec.p = m.j1*v.p+m.j3*v.s
            jvec.s = m.j3*v.p+m.j0*v.s
        end function JVec2x4c4_mul_ JMat4x4c4


        subroutine JVec2x4c4_mul_ JMat4x4c4_v2(v,m,jvec) 
            
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: JVec2x4c4_mul_ JMat4x4c4_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x4c4_mul_ JMat4x4c4_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: JVec2x4c4_mul_ JMat4x4c4_v2
            type(JVec2x4c4),  intent(in) :: v
            type( JMat4x4c4),  intent(in) :: m
            type(JVec2x4c4),  intent(out) :: jvec
            
            ! Executable code ....
            jvec.p = m.j1*v.p+m.j3*v.s
            jvec.s = m.j3*v.p+m.j0*v.s
        end subroutine JVec2x4c4_mul_ JMat4x4c4_v2
     
    
        pure function  JMat4x4c4_vecdiv_v1(m,x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_vecdiv_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_vecdiv_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_vecdiv_v1
            type( JMat4x4c4),  intent(in) :: m
            type(XMM4c4_t),      intent(in) :: x
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code ....
            mat.j0 = m.j0/x
            mat.j1 = m.j1/x
            mat.j2 = m.j2/x
            mat.j3 = m.j3/x
        end function  JMat4x4c4_vecdiv_v1


        subroutine  JMat4x4c4_vecdiv_v2(m,x,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_vecdiv_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_vecdiv_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_vecdiv_v2
            type( JMat4x4c4),  intent(in) :: m
            type(XMM4c4_t),      intent(in) :: x
            type( JMat4x4c4),  intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            ! Executable code ....
            mat.j0 = m.j0/x
            mat.j1 = m.j1/x
            mat.j2 = m.j2/x
            mat.j3 = m.j3/x
        end subroutine  JMat4x4c4_vecdiv_v2


        pure function  JMat4x4c4_add_JMat4x8c14(x,y) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_add_JMat4x4c4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_add_JMat4x4c4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_add_JMat4x4c4
            type( JMat4x4c4),   intent(in) :: x
            type( JMat4x4c4),   intent(in) :: y
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            mat.j0 = x.j0+y.j0
            mat.j1 = x.j1+y.j1
            mat.j2 = x.j2+y.j2
            mat.j3 = x.j3+y.j3
        end function  JMat4x4c4_add_ JMat4x4c4


        subroutine  JMat4x4c4_add_JMat4x4c4_v2(x,y,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_add_JMat4x4c4_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_add_JMat4x4c4_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_add_JMat4x4c4_v2
            type( JMat4x4c4),   intent(in) :: x
            type( JMat4x4c4),   intent(in) :: y
            type( JMat4x4c4),   intent(out) :: mat
            
            mat.j0 = x.j0+y.j0
            mat.j1 = x.j1+y.j1
            mat.j2 = x.j2+y.j2
            mat.j3 = x.j3+y.j3
        end subroutine  JMat4x4c4_add_ JMat4x4c4_v2


        pure function  JMat4x4c4_sub_JMat4x4c4(x,y) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_sub_JMat4x4c4
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_sub_JMat4x4c4
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_sub_JMat4x4c4
            type( JMat4x4c4),   intent(in) :: x
            type( JMat4x4c4),   intent(in) :: y
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            mat.j0 = x.j0-y.j0
            mat.j1 = x.j1-y.j1
            mat.j2 = x.j2-y.j2
            mat.j3 = x.j3-y.j3
        end function  JMat4x4c4_sub_ JMat4x4c4


        subroutine  JMat4x4c4_sub_JMat4x4c4_v2(x,y,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_sub_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_sub_JMat4xc16c16_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_sub_JMat4xc16c16_v2
            type( JMat4x4c4),   intent(in) :: x
            type( JMat4x4c4),   intent(in) :: y
            type( JMat4x4c4),   intent(out) :: mat
            
            mat.j0 = x.j0-y.j0
            mat.j1 = x.j1-y.j1
            mat.j2 = x.j2-y.j2
            mat.j3 = x.j3-y.j3
        end subroutine  JMat4x4c4_sub_ JMat4x4c4_v2


        pure function  JMat4x4c4_rotator_v1(theta) result(mat)
 
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_rotator_v1
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_rotator_v1
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_rotator_v1
            type(XMM4r4_t),  intent(in) :: theta
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t) :: vc,vs,nvs
            !dir$ attributes align : 16 :: vc,vs,nvs
            type(XMM4r4_t), parameter :: zero = XMM4r4_t()
            ! Exec code .... 
            vc.v   = cos(theta.v)
            mat.j0 = array_init(vc.v,zero.v)
            mat.j1 = mat.j0
            vs.v   = sin(theta.v)
            mat.j2 = array_init(vs.v,zero.v)
            nvs.v  = -vs.v
            mat.j3 = array_init(nvs.v,zero.v) 
        end function  JMat4x4c4_rotator_v2


        subroutine  JMat4x4c4_rotator_v2(theta,mat)
 
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_rotator_v12
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_rotator_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_rotator_v2
            type(XMM4r4_t),    intent(in)  :: theta
            type( JMat4x4c4),  intent(out) :: mat
          
            type(XMM4r4_t) :: vc,vs,nvs
            !dir$ attributes align : 16 :: vc,vs,nvs
            type(XMM4r4_t), parameter :: zero = XMM4r4_t()
            ! Exec code .... 
            vc.v   = cos(theta.v)
            mat.j0 = array_init(vc.v,zero.v)
            mat.j1 = mat.j0
            vs.v   = sin(theta.v)
            mat.j2 = array_init(vs.v,zero.v)
            nvs.v  = -vs.v
            mat.j3 = array_init(nvs.v,zero.v) 
        end subroutine  JMat4x4c4_rotator_v2


        pure function  JMat4x4c4_lin_retarder(phi,ang) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_lin_retarder
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_lin_retarder
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_lin_retarder
            type(XMM4r4_t),   intent(in) :: phi
            type(XMM4r4_t),   intent(in) :: ang
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: n1   = XMM4r4_t(-1.0_sp)
            type(XMM4r4_t), parameter :: zero = XMM4r4_t(0.0_sp)
            type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t), parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4c4_t),   automatic :: phasor
            type(XMM4r4_t), automatic :: h_phi
            !dir$ attributes align : 16 :: phasor,h_phi
            ! Exec code ....
            phasor = cexp_xmm2c4(j*h_phi)
            mat.j0 = phasor
            h_phi  = phi.v*half.v 
            mat.j1 = one/phasor
            mat.j2 = czero
            mat.j3 = czero
        end function  JMat4x4c4_lin_retarder


        subroutine  JMat4x4c4_lin_retarder_v2(phi,ang,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_lin_retarder_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_lin_retarder_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_lin_retarder_v2
            type(XMM4r4_t),    intent(in)  :: phi
            type(XMM4r4_t),    intent(in)  :: ang
            type( JMat4x4c4),  intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: n1   = XMM4r4_t(-1.0_sp)
            type(XMM4r4_t), parameter :: zero = XMM4r4_t(0.0_sp)
            type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t), parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4c4_t),   automatic :: phasor
            type(XMM4r4_t), automatic :: h_phi
            !dir$ attributes align : 16 :: phasor,h_phi
            ! Exec code ....
            phasor = cexp_xmm2c4(j*h_phi)
            mat.j0 = phasor
            h_phi  = phi.v*half.v 
            mat.j1 = one/phasor
            mat.j2 = czero
            mat.j3 = czero
        end subroutine  JMat4x4c4_lin_retarder_v2
 

        pure function  JMat4x4c4_circ_retarder(phi) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_circ_retarder
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_circ_retarder
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_circ_retarder
            type(XMM4r4_t),  intent(in) :: phi
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: half  = XMM4r4_t(0.5_sp)
            type(XMM4c4_t),   automatic :: nre
            type(XMM4r4_t), automatic :: h_phi,ch_phi,sh_phi
            !dir$ attributes align : 16 :: nre
            !dir$ attributes align : 16 :: h_phi
            !dir$ attributes align : 16 :: ch_phi
            !dir$ attributes align : 16 :: sh_phi
            ! Exec code ....
            ch_phi = cos(phi.v)
            mat.j0 = xmm4r41x_init(ch_phi)
            mat.j1 = mat.j0
            sh_phi = sin(phi.v)
            mat.j2 = xmm4r41x_init(sh_phi)
            mat.j3 = -mat.j2.re
        end function  JMat4x4c4_circ_retarder


        subroutine  JMat4x4c4_circ_retarder_v2(phi,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_circ_retarder_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_circ_retarder_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_circ_retarder_v2
            type(XMM4r4_t),   intent(in) :: phi
            type( JMat4x4c4), intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: half  = XMM4r4_t(0.5_sp)
            type(XMM4c4_t),   automatic :: nre
            type(XMM4r4_t), automatic :: h_phi,ch_phi,sh_phi
            !dir$ attributes align : 16 :: nre
            !dir$ attributes align : 16 :: h_phi
            !dir$ attributes align : 16 :: ch_phi
            !dir$ attributes align : 16 :: sh_phi
            ! Exec code ....
            ch_phi = cos(phi.v)
            mat.j0 = xmm4r41x_init(ch_phi)
            mat.j1 = mat.j0
            sh_phi = sin(phi.v)
            mat.j2 = xmm4r41x_init(sh_phi)
            mat.j3 = -mat.j2.re
        end subroutine  JMat4x4c4_circ_retarder_v2


        pure function  JMat4x4c4_circ_polar(atten) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_circ_polar
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_circ_polar
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_circ_polar
            type(XMM4r4_t),   intent(in) :: atten
            type( JMat4x4c4)  :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t), parameter :: zero = XMM4r4_t(0.0_sp)
            type(XMM4r4_t), automatic :: t0,e,t1,t2
            !dir$ attributes align : 16 :: t0,e,t1,t2
            ! Exec code ....
            t0.v   = (atten.v-one.v)/(atten.v+one.v)
            e.v    = sqrt(t0.v)
            t2.v   = half.v*(one.v+e.v)
            mat.j0 = xmm4r41x_init(t2)
            mat.j1 = mat.j0 
            t1.v   = half.v*(one.v-e.v)
            mat.j2 = array_init(zero.v,t1.v)
            mat.j3 = array_init(zero.v,zero.v-t1.v)
        end function  JMat4x4c4_circ_polar


        subroutine  JMat4x4c4_circ_polar_v2(atten,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_circ_polar_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_circ_polar_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_circ_polar_v2
            type(XMM4r4_t),   intent(in) :: atten
            type( JMat4x4c4), intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4r4_t), parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t), parameter :: zero = XMM4r4_t(0.0_sp)
            type(XMM4r4_t), automatic :: t0,e,t1,t2
            !dir$ attributes align : 16 :: t0,e,t1,t2
            ! Exec code ....
            t0.v   = (atten.v-one.v)/(atten.v+one.v)
            e.v    = sqrt(t0.v)
            t2.v   = half.v*(one.v+e.v)
            mat.j0 = xmm4r41x_init(t2)
            mat.j1 = mat.j0 
            t1.v   = half.v*(one.v-e.v)
            mat.j2 = array_init(zero.v,t1.v)
            mat.j3 = array_init(zero.v,zero.v-t1.v)
        end subroutine  JMat4x4c4_circ_polar_v2


        pure function  JMat4x4c4_eigenvalue(a,b,ca,cb) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_eigenvalue
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_eigenvalue
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_eigenvalue
            type(JVec2x4c4),  intent(in) :: a
            type(JVec2x4c4),  intent(in) :: b
            type(XMM4c4_t),      intent(in) :: ca
            type(XMM4c4_t),      intent(in) :: cb
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4c4_t), automatic :: det,t0,t1
            !dir$ attributes align : 16 :: det,t0,t1
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
        end function  JMat4x4c4_eigenvalue


        subroutine  JMat4x4c4_eigenvalue_v2(a,b,ca,cb,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_eigenvalue_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_eigenvalue_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_eigenvalue_v2
            type(JVec2x4c4),  intent(in) :: a
            type(JVec2x4c4),  intent(in) :: b
            type(XMM4c4_t),      intent(in) :: ca
            type(XMM4c4_t),      intent(in) :: cb
            type( JMat4x4c4),  intent(out) :: mat
            !dir$ attributes align : 16 :: mat
            type(XMM4c4_t), automatic :: det,t0,t1
            !dir$ attributes align : 16 :: det,t0,t1
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
        subroutine  JMat4x4c4_eigenvalue_v2


        pure function  JMat4x4c4_transpose(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_transpose
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_transpose
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_transpose
            type( JMat4x4c4),  intent(in) :: x
            type( JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            mat.j0 = x.j0
            mat.j1 = x.j1
            mat.j2 = mat.j3
            mat.j3 = mat.j2
        end function  JMat4x4c4_transpose


        subroutine  JMat4x4c4_transpose_v2(x,mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_transpose_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_transpose_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_transpose_v2
            type( JMat4x4c4),  intent(in) :: x
            type( JMat4x4c4),  intent(out) :: mat
            mat.j0 = x.j0
            mat.j1 = x.j1
            mat.j2 = mat.j3
            mat.j3 = mat.j2
        end subroutine  JMat4x4c4_transpose


        pure function  JMat4x4c4_conj(x) result(mat)

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_conj
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_conj
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_conj
            type( JMat4x4c4),  intent(in) :: x
            type(JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            mat.j0 = conjugate(x.j0)
            mat.j1 = conjugate(x.j1)
            mat.j2 = conjugate(x.j2)
            mat.j3 = conjugate(x.j3)
        end function  JMat4x4c4_conj


        subroutine  JMat4x4c4_conj_v2(x,mat) 

            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE ::  JMat4x4c4_conj_v2
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  JMat4x4c4_conj_v2
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) ::  JMat4x4c4_conj_v2
            type( JMat4x4c4),  intent(in) :: x
            type(JMat4x4c4) :: mat
            !dir$ attributes align : 16 :: mat
            mat.j0 = conjugate(x.j0)
            mat.j1 = conjugate(x.j1)
            mat.j2 = conjugate(x.j2)
            mat.j3 = conjugate(x.j3)
        end subroutine  JMat4x4c4_conj_v2

     
        

        


        
     
     


    
    







end module mueller_calculus_sse
