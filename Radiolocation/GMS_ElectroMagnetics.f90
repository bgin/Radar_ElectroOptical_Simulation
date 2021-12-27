

module  ElectroMagnetics



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'ElectroMagnetics'
 !          
 !          Purpose:
  !                     This module contains various implementations of
 !                      Computational ElectroMagnetics related routines.
 !          History:
 !                        
 !                        Date: 18-12-2021
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
     use mod_avx512c16f32
     use mod_avx512c8f64
     use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: ELECTROMAGNETICS_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: ELECTROMAGNETICS_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: ELECTROMAGNETICS_MICRO = 0
     ! Full version
     integer(kind=i4), parameter :: ELECTROMAGNETICS_FULLVER = &
          1000*ELECTROMAGNETICS_MAJOR+100*ELECTROMAGNETICS_MINOR+10*ELECTROMAGNETICS_MICRO
     ! Module creation date
     character(*),       parameter :: ELECTROMAGNETICSCREATION_DATE = "18-12-2021 15:57 +00200 (SAT 18 DEC 2021 GMT+2)"
     ! Module build date
     character(*),       parameter :: ELECTROMAGNETICS_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: ELECTROMAGNETICS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: ELECTROMAGNETICS_SYNOPSIS      = " Computational ElectroMagnetics related routines"

     type(ZMM16c4),   parameter, private :: jc4    = ZMM16c4((0.0_sp,1.0_sp))
     type(ZMM8c8),    parameter, private :: jc8    = ZMM8c8((0.0_dp,1.0_dp))
     type(ZMM16r4_t), parameter, private :: mu0r16 = ZMM16r4_t(0,0000012566370614359173_sp)
     type(ZMM8r4_t),  parameter, private :: mu0r8  = ZMM8r4_t(0,0000012566370614359173_dp)
     
   contains


     ! Helper functions

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function sdotv_zmm16r4(v1,v2) result(res) !GCC$ ATTRIBUTES aligned(32) :: sdotv_zmm16r4 !GCC$ ATTRIBUTES inline :: sdotv_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: sdotv_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function sdotv_zmm16r4(v1,v2) result(res)
        !DIR$ ATTRIBUTES INLINE :: sdotv_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: sdotv_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: sdotv_zmm16r4
#endif
        
         type(ZMM16r4_t),  dimension(3), intent(in) :: v1
         type(ZMM16r4_t),  dimension(3), intent(in) :: v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
#endif
         type(ZMM16r4_t) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: res
#endif
         !Locals
         type(ZMM16r4_t), automatic :: t0,t1,t2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
#endif
         t0.v  = v1(1).v*v2(1).v !x0...x15
         t1.v  = v1(2).v*v2(2).v !y0...y15
         t2.v  = v1(3).v*v2(3).v !z0...z15
         res.v = t0.v+t1.v+t2.v
     end function sdotv_zmm16r4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function cdotv_zmm16c4(v1,v2) result(res)  !GCC$ ATTRIBUTES aligned(32) :: cdotv_zmm16c4 !GCC$ ATTRIBUTES inline :: cdotv_zmm16c4 !GCC$ ATTRIBUTES vectorcall :: cdotv_zmm16c4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function cdotv_zmm16c4(v1,v2) result(res)
        !DIR$ ATTRIBUTES INLINE :: cdotv_zmm16c4
        !DIR$ ATTRIBUTES VECTOR :: cdotv_zmm16c4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdotv_zmm16c4
#endif
         type(ZMM16c4),   intent(in) :: v1
         type(ZMM16c4),   intent(in) :: v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
#endif         
         complex(kind=sp) :: res
         type(ZMM16c4), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         t0 = v1*v2
         res = complex(sum(t0.re),sum(t0.im))
      end function cdotv_zmm16c4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function zdotv_zmm8c8(v1,v2) result(res)  !GCC$ ATTRIBUTES aligned(32) :: zdotv_zmm8c8 !GCC$ ATTRIBUTES inline :: zdotv_zmm8c8 !GCC$ ATTRIBUTES vectorcall :: zdotv_zmm8c8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function zdotv_zmm8c8(v1,v2) result(res)
        !DIR$ ATTRIBUTES INLINE :: zdotv_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: zdotv_zmm8c8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: zdotv_zmm8c8
#endif
         type(ZMM8c8),   intent(in) :: v1
         type(ZMM8c8),   intent(in) :: v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
#endif         
         complex(kind=dp) :: res
         type(ZMM8c8), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         t0 = v1*v2
         res = complex(sum(t0.re),sum(t0.im))
     end function zdotv_zmm8c8


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine  scrossc_zmm16c4(vc1,vc2,res)  !GCC$ ATTRIBUTES aligned(32) :: scrossc_zmm16c4 !GCC$ ATTRIBUTES inline :: scrossc_zmm16c4 !GCC$ ATTRIBUTES vectorcall :: scrossc_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine scrossc_zmm16c4(vc1,vc2,res) 
        !DIR$ ATTRIBUTES INLINE :: scrossc_zmm16c4
        !DIR$ ATTRIBUTES VECTOR :: scrossc_zmm16c4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: scrossc_zmm16c4
#endif
         type(ZMM16c4), dimension(3), intent(in)  :: vc1
         type(ZMM16c4), dimension(3), intent(in)  :: vc2
         type(ZMM16c4), dimension(3), intent(out) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED vc1 : 64
         !DIR$ ASSUME_ALIGNED vc2 : 64
         !DIR$ ASSUME_ALIGNED res : 64
#endif         
         !Exec code ...
         res(0) = vc1(2)*vc2(3)-vc1(3)*vc2(2)
         res(1) = vc1(3)*vc2(1)-vc1(1)*vc2(3)
         res(3) = vc1(1)*vc2(2)-vc1(2)*vc2(1)
     end subroutine  scrossc_zmm16c4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine  dcrossc_zmm8c8(vc1,vc2,res)  !GCC$ ATTRIBUTES aligned(32) :: dcrossc_zmm8c8 !GCC$ ATTRIBUTES inline :: dcrossc_zmm8c8 !GCC$ ATTRIBUTES vectorcall :: dcrossc_zmm8c8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine dcrossc_zmm8c8(vc1,vc2,res) 
        !DIR$ ATTRIBUTES INLINE :: dcrossc_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: dcrossc_zmm8c8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dcrossc_zmm8c8
#endif
         type(ZMM8c8), dimension(3), intent(in)  :: vc1
         type(ZMM8c8), dimension(3), intent(in)  :: vc2
         type(ZMM8c8), dimension(3), intent(out) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED vc1 : 64
         !DIR$ ASSUME_ALIGNED vc2 : 64
         !DIR$ ASSUME_ALIGNED res : 64
#endif         
         !Exec code ...
         res(0) = vc1(2)*vc2(3)-vc1(3)*vc2(2)
         res(1) = vc1(3)*vc2(1)-vc1(1)*vc2(3)
         res(3) = vc1(1)*vc2(2)-vc1(2)*vc2(1)
     end subroutine  dcrossc_zmm8c8       
       
       
       
       


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     pure function ddotv_zmm8r8(v1,v2) result(res) !GCC$ ATTRIBUTES aligned(32) :: ddotv_zmm8r8 !GCC$ ATTRIBUTES inline :: ddotv_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: ddotv_zmm8r8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     pure function ddotv_zmm8r8(v1,v2) result(res)
        !DIR$ ATTRIBUTES INLINE :: ddotv_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: ddotv_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ddotv_zmm8r8
#endif
         
         type(ZMM8r8_t),  dimension(3), intent(in) :: v1
         type(ZMM8r8_t),  dimension(3), intent(in) :: v2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
#endif
         type(ZMM8r8_t) :: res
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: res
#endif
         ! Locals
         type(ZMM8r8_t), automatic :: t0,t1,t2
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
#endif
         t0.v  = v1(1).v*v2(1).v !x0...x7
         t1.v  = v1(2).v*v2(2).v !y0...y7
         t2.v  = v1(3).v*v2(3).v !z0...z7
         res.v = t0.v+t1.v+t2.v
      end function ddotv_zmm8r8

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      subroutine scrossv_zmm16r4(v1,v2,vc) !GCC$ ATTRIBUTES aligned(32) :: scrossv_zmm16r4 !GCC$ ATTRIBUTES inline :: scrossv_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: scrossv_zmm16r4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine scrossv_zmm16r4(v1,v2,vc)
        !DIR$ ATTRIBUTES INLINE :: scrossv_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: scrossv_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: scrossv_zmm16r4
#endif
        type(ZMM16r4_t),  dimension(3), intent(in)  :: v1
        type(ZMM16r4_t),  dimension(3), intent(in)  :: v2
        type(ZMM16r4_t),  dimension(3), intent(out) :: vc
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
         !DIR$ ASSUME_ALIGNED vc : 64
#endif        
        ! Exec code ....
        vc(0).v = v1(2).v*v2(3).v-v1(3).v*v2(2).v
        vc(1).v = v1(3).v*v2(1).v-v1(1).v*v2(3).v
        vc(2).v = v1(1).v*v2(2).v-v1(2).v*v2(1).v
      end subroutine scrossv_zmm16r4

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      subroutine dcrossv_zmm8r8(v1,v2,vc) !GCC$ ATTRIBUTES aligned(32) :: dcrossv_zmm8r8 !GCC$ ATTRIBUTES inline :: dcrossv_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: dcrossv_zmm8r8 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
      subroutine dcrossv_zmm8r8(v1,v2,vc)
        !DIR$ ATTRIBUTES INLINE :: dcrossv_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: dcrossv_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dcrossv_zmm8r8
#endif
        type(ZMM8r8_t),  dimension(3), intent(in)  :: v1
        type(ZMM8r8_t),  dimension(3), intent(in)  :: v2
        type(ZMM8r8_t),  dimension(3), intent(out) :: vc
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED v1 : 64
         !DIR$ ASSUME_ALIGNED v2 : 64
         !DIR$ ASSUME_ALIGNED vc : 64
#endif           
        ! Exec code ....
        vc(0).v = v1(2).v*v2(3).v-v1(3).v*v2(2).v
        vc(1).v = v1(3).v*v2(1).v-v1(1).v*v2(3).v
        vc(2).v = v1(1).v*v2(2).v-v1(2).v*v2(1).v
    end subroutine dcrossv_zmm8r8

      ! Direction Vector spherical [theta,phi] (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))   
    subroutine dir_vector_zmm16r4(theta,phi,dv) !GCC$ ATTRIBUTES aligned(32) :: dir_vector_zmm16r !GCC$ ATTRIBUTES inline :: dir_vector_zmm16r !GCC$ ATTRIBUTES vectorcall :: dir_vector_zmm16r
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine dir_vector_zmm16r4(theta,phi,dv)
        !DIR$ ATTRIBUTES INLINE :: dir_vector_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: dir_vector_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vector_zmm16r4
#endif
         type(ZMM16r4_t),               intent(in)  :: theta
         type(ZMM16r4_t),               intent(in)  :: phi
         type(ZMM16r4_t), dimension(3), intent(out) :: dv
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED dv : 64
#endif
         ! Locals
         type(ZMM16r4_t), automatic :: sth
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: sth
#endif         
         ! Exec code ...
         sth.v = sin(theta.v)
         dv(1).v = sth.v*cos(phi.v)
         dv(2).v = sth.v*sin(phi.v)
         dv(3).v = cos(theta.v)
     end subroutine dir_vector_zmm16r4


      ! Direction Vector spherical [theta,phi] (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))   
    subroutine dir_vector_zmm8r8(theta,phi,dv) !GCC$ ATTRIBUTES aligned(32) :: dir_vector_zmm8r8 !GCC$ ATTRIBUTES inline :: dir_vector_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: dir_vector_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine dir_vector_zmm8r8(theta,phi,dv)
        !DIR$ ATTRIBUTES INLINE :: dir_vector_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: dir_vector_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: dir_vector_zmm8r8
#endif
         type(ZMM8r8_t),               intent(in)  :: theta
         type(ZMM8r8_t),               intent(in)  :: phi
         type(ZMM8r8_t), dimension(3), intent(out) :: dv
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED dv : 64
#endif
         ! Locals
         
         type(ZMM8r8_t), automatic :: sth
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !DIR$ ATTRIBUTES ALIGN : 64 :: sth
#endif
         ! Exec code ...
         sth.v = sin(theta.v)
         dv(1).v = sth.v*cos(phi.v)
         dv(2).v = sth.v*sin(phi.v)
         dv(3).v = cos(theta.v)
    end subroutine dir_vector_zmm8r8     
      
    ! Polarization Vector of plane-wave propagating into direction computed by
    ! dir_vector_xmmxrx (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine pol_vector_zmm16r4(theta,phi,psi,pv) !GCC$ ATTRIBUTES aligned(32) :: pol_vector_zmm16r4 !GCC$ ATTRIBUTES inline :: pol_vector_zmm16r4 !GCC$ ATTRIBUTES vectorcall :: pol_vector_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine pol_vector_zmm16r4(theta,phi,psi,pv)
        !DIR$ ATTRIBUTES INLINE :: pol_vector_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: pol_vector_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vector_zmm16r4
#endif
        type(ZMM16r4_t),                  intent(in)  :: theta
        type(ZMM16r4_t),                  intent(in)  :: phi
        type(ZMM16r4_t),                  intent(in)  :: psi
        type(ZMM16r4_t),    dimension(3), intent(out) :: pv
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED pv : 64
#endif
        ! Locals
        type(ZMM16r4_t), automatic :: cpsi
        type(ZMM16r4_t), automatic :: cphi
        type(ZMM16r4_t), automatic :: spsi
        type(ZMM16r4_t), automatic :: sphi
        type(ZMM16r4_t), automatic :: t0
       
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: cpsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: cphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: spsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: sphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
      
        
#endif
        ! Exec code ...
        cpsi.v  = cos(psi.v)
        cphi.v  = cos(phi.v)
        spsi.v  = sin(psi.v)
        sphi.v  = sin(phi.v)
        t0.v    = spsi.v*cos(theta.v)
        pv(1).v = cpsi.v*sphi.v-t0.v*cphi.v
        pv(2).v = -cpsi.v*cphi-t0.v*sphi.v
        pv(3).v = spsi.v*sin(theta.v)
    end subroutine pol_vector_zmm16r4
    

  ! Polarization Vector of plane-wave propagating into direction computed by
    ! dir_vector_xmmxrx (SIMD data-types)
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine pol_vector_zmm8r8(theta,phi,psi,pv) !GCC$ ATTRIBUTES aligned(32) :: pol_vector_zmm8r8 !GCC$ ATTRIBUTES inline :: pol_vector_zmm8r8 !GCC$ ATTRIBUTES vectorcall :: pol_vector_zmm8r8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine pol_vector_zmm8r8(theta,phi,psi,pv)
        !DIR$ ATTRIBUTES INLINE :: pol_vector_zmm8r8
        !DIR$ ATTRIBUTES VECTOR :: pol_vector_zmm8r8
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: pol_vector_zmm8r8
#endif
        type(ZMM8r8_t),                  intent(in)  :: theta
        type(ZMM8r8_t),                  intent(in)  :: phi
        type(ZMM8r8_t),                  intent(in)  :: psi
        type(ZMM8r8_t),    dimension(3), intent(out) :: pv
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ASSUME_ALIGNED pv : 64
#endif
        ! Locals
        type(ZMM8r8_t), automatic :: cpsi
        type(ZMM8r8_t), automatic :: cphi
        type(ZMM8r8_t), automatic :: spsi
        type(ZMM8r8_t), automatic :: sphi
        type(ZMM8r8_t), automatic :: t0
       
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: cpsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: cphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: spsi
        !DIR$ ATTRIBUTES ALIGN : 64 :: sphi
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
       
        
#endif
        ! Exec code ...
        cpsi.v  = cos(psi.v)
        cphi.v  = cos(phi.v)
        spsi.v  = sin(psi.v)
        sphi.v  = sin(phi.v)
        t0.v    = spsi.v*cos(theta.v)
        pv(1).v = cpsi.v*sphi.v-t0.v*cphi.v
        pv(2).v = -cpsi.v*cphi-t0.v*sphi.v
        pv(3).v = spsi.v*sin(theta.v)
    end subroutine pol_vector_zmm8r8
    
          
        
       
          
       

       ! Vectorized Electric-field at 16 points 'R'
     ! vpol -- vector of vertical polarization at point 'R'
     ! vdir -- direction vector
     ! vr   -- vector radius r
     ! Exyz -- resulting electrical field (3D) at sixteen points 'R', i.e. R(xyz), x0-x15,y0-y15,z0-z15
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine EF3Dvp_zmm16c4(vpol,vdir,vr,k,Exyz) !GCC$ ATTRIBUTES aligned(32) :: EF3Dvp_zmm16c4 !GCC$ ATTRIBUTES inline :: EF3Dvp_zmm16c4 
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine EF3Dvp_zmm16c4(vpol,vdir,vr,k,Exyz)
         !DIR$ ATTRIBUTES INLINE :: EF3Dvp_zmm16c4
         !DIR$ ATTRIBUTES VECTOR :: EF3Dvp_zmm16c4
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: EF3Dvp_zmm16c4
#endif
        type(ZMM16r4_t), dimension(3),  intent(in)  :: vpol
        type(ZMM16r4_t), dimension(3),  intent(in)  :: vdir
        type(ZMM16r4_t), dimension(3),  intent(in)  :: vr
        type(ZMM16c4),                  intent(in)  :: k
        type(ZMM16c4),   dimension(3),  intent(out) :: Exyz
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED vpol : 64
        !DIR$ ASSUME_ALIGNED vdir : 64
        !DIR$ ASSUME_ALIGNED vr   : 64
        !DIR$ ASSUME_ALIGNED Exyz : 64
#endif
        type(ZMM16c4),   automatic :: carg
        type(ZMM16r4_t), automatic :: dp
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: dp
        !DIR$ ATTRIBUTES ALIGN : 64 :: carg
#endif
        ! Exec code ...
        dp.v    = sdotv_zmm16r4(vdir,vr)
        carg    = cexp_c16(jc4*k*dp)
        Exyz(1) = vpol(1).v*carg
        Exyz(2) = vpol(2).v*carg
        Exyz(3) = vpol(3).v*carg
     end subroutine EF3Dvp_zmm16c4


    ! Vectorized Electric-field at 8 points 'R'
     ! vpol -- vector of vertical polarization at point 'R'
     ! vdir -- direction vector
     ! vr   -- vector radius r
     ! Exyz -- resulting electrical field (3D) at sixteen points 'R', i.e. R(xyz), x0-x7,y0-y7,z0-z7
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine EF3Dvp_zmm8c8(vpol,vdir,vr,k,Exyz) !GCC$ ATTRIBUTES aligned(32) :: EF3Dvp_zmm8c8 !GCC$ ATTRIBUTES inline :: EF3Dvp_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine EF3Dvp_zmm8c8(vpol,vdir,vr,k,Exyz)
         !DIR$ ATTRIBUTES INLINE :: EF3Dvp_zmm8c8
         !DIR$ ATTRIBUTES VECTOR :: EF3Dvp_zmm8c8
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: EF3Dvp_zmm8c8
#endif
        type(ZMM8r8_t), dimension(3),  intent(in)  :: vpol
        type(ZMM8r8_t), dimension(3),  intent(in)  :: vdir
        type(ZMM8r8_t), dimension(3),  intent(in)  :: vr
        type(ZMM8c8),                  intent(in)  :: k
        type(ZMM8c8),   dimension(3),  intent(out) :: Exyz
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED vpol : 64
        !DIR$ ASSUME_ALIGNED vdir : 64
        !DIR$ ASSUME_ALIGNED vr   : 64
        !DIR$ ASSUME_ALIGNED Exyz : 64
#endif
        type(ZMM8c8),   automatic :: carg
        type(ZMM8r8_t), automatic :: dp
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: dp
        !DIR$ ATTRIBUTES ALIGN : 64 :: carg
#endif
        ! Exec code ...
        dp.v    = ddotv_zmm8r8(vdir,vr)
        carg    = cexp_c8(jc8*k*dp)
        Exyz(1) = vpol(1).v*carg
        Exyz(2) = vpol(2).v*carg
        Exyz(3) = vpol(3).v*carg
     end subroutine EF3Dvp_zmm8c8


      
#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine MF3Dvp_zmm16c4(vpol,vdir,k,omega,vr,Bxyz) !GCC$ ATTRIBUTES aligned(32) :: MF3Dvp_zmm16c4 !GCC$ ATTRIBUTES inline :: MF3Dvp_zmm16c4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine MF3Dvp_zmm16c4(vpol,vdir,k,omega,vr,Bxyz)
         !DIR$ ATTRIBUTES INLINE :: MF3Dvp_zmm16c4
         !DIR$ ATTRIBUTES VECTOR :: MF3Dvp_zmm16c4
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: MF3Dvp_zmm16c4
#endif
         type(ZMM16r4_t),  dimension(3),  intent(in)  :: vpol
         type(ZMM16r4_t),  dimension(3),  intent(in)  :: vdir
         type(ZMM16c4),                   intent(in)  :: k
         type(ZMM16r4_t),                 intent(in)  :: omega
         type(ZMM16r4_t),  dimension(3),  intent(in)  :: vr
         type(ZMM16c4),    dimension(3),  intent(out) :: Bxyz
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED vpol : 64
        !DIR$ ASSUME_ALIGNED vdir : 64
        !DIR$ ASSUME_ALIGNED vr   : 64
        !DIR$ ASSUME_ALIGNED Bxyz : 64
#endif
         type(ZMM16c4), automatic, dimension(3) :: cdir
         type(ZMM16c4), automatic, dimension(3) :: Exyz
         type(ZMM16c4), automatic, dimension(3) :: cp
         type(ZMM16c4), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdir
         !DIR$ ATTRIBUTES ALIGN : 64 :: Exyz
         !DIR$ ATTRIBUTES ALIGN : 64 :: cp
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         call EF3Dvp_zmm16c4(vpol,vdir,vr,k,Exyz)
         cdir(1) = zmm16r41x_init(vdir(1))
         cdir(2) = zmm16r41x_init(vdir(2))
         cdir(3) = zmm16r41x_init(vdir(3))
         t0 = k/(omega.v*mu0r16.v)
         call scrossc_zmm16c4(cdir,Exyz,cp)
         Bxyz(1) = t0*cp(1)
         Bxyz(2) = t0*cp(2)
         Bxyz(3) = t0*cp(3)
       end subroutine MF3Dvp_zmm16c4


#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     subroutine MF3Dvp_zmm8c8(vpol,vdir,k,omega,vr,Bxyz) !GCC$ ATTRIBUTES aligned(32) :: MF3Dvp_zmm8c8 !GCC$ ATTRIBUTES inline :: MF3Dvp_zmm8c8
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     subroutine MF3Dvp_zmm8c8(vpol,vdir,k,omega,vr,Bxyz)
         !DIR$ ATTRIBUTES INLINE :: MF3Dvp_zmm8c8
         !DIR$ ATTRIBUTES VECTOR :: MF3Dvp_zmm8c8
         !DIR$ OPTIMIZE : 3
         !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: MF3Dvp_zmm8c8
#endif
         type(ZMM8r8_t),  dimension(3),  intent(in)  :: vpol
         type(ZMM8r8_t),  dimension(3),  intent(in)  :: vdir
         type(ZMM8c8),                   intent(in)  :: k
         type(ZMM8r8_t),                 intent(in)  :: omega
         type(ZMM8r8_t),  dimension(3),  intent(in)  :: vr
         type(ZMM8c8),    dimension(3),  intent(out) :: Bxyz
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED vpol : 64
        !DIR$ ASSUME_ALIGNED vdir : 64
        !DIR$ ASSUME_ALIGNED vr   : 64
        !DIR$ ASSUME_ALIGNED Bxyz : 64
#endif
         type(ZMM8c8), automatic, dimension(3) :: cdir
         type(ZMM8c8), automatic, dimension(3) :: Exyz
         type(ZMM8c8), automatic, dimension(3) :: cp
         type(ZMM8c8), automatic :: t0
#if defined(__INTEL_COMPILER) || defined(__ICC)
         !DIR$ ATTRIBUTES ALIGN : 64 :: cdir
         !DIR$ ATTRIBUTES ALIGN : 64 :: Exyz
         !DIR$ ATTRIBUTES ALIGN : 64 :: cp
         !DIR$ ATTRIBUTES ALIGN : 64 :: t0
#endif
         call EF3Dvp_zmm8c8(vpol,vdir,vr,k,Exyz)
         cdir(1) = zmm8r81x_init(vdir(1))
         cdir(2) = zmm8r81x_init(vdir(2))
         cdir(3) = zmm8841x_init(vdir(3))
         t0 = k/(omega.v*mu0r16.v)
         call dcrossc_zmm8c8(cdir,Exyz,cp)
         Bxyz(1) = t0*cp(1)
         Bxyz(2) = t0*cp(2)
         Bxyz(3) = t0*cp(3)
      end subroutine MF3Dvp_zmm8c8


      
     

      
     



















end module ElectroMagnetics
