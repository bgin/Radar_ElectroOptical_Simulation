

module mod_chebyshev_hydrometeors_common_AVX512


 !!==============================================================!!
 !!  Explicitly vectorized(AVX512) subroutines -- common for the !!
 !!  Chebyshev HydroMeteors  shape components computations           !! 
 !!==============================================================!!
     
     use mod_kinds, only : int4,sp
     use mod_vectypes, only : ZMM16r4_t, Mask16_t
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59
     integer(kind=int4), parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MAJOR = 1
     integer(kind=int4), parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MINOR = 0
     integer(kind=int4), parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MICRO = 0
     integer(kind=int4), parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_FULLVER =    &
                                      1000*MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MAJOR + &
                                      100*MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MINOR  + &
                                      10*MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_MICRO
     character(*),       parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_CREATE_DATE = "21-01-2020 10:12 AM +00200 (TUE 21 JAN 2020 GMT+2)"
     character(*),       parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_BUILD_DATE  = __DATE__ " " __TIME__
     character(*),       parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),       parameter :: MOD_CHEBYSHEV_HYDROMETEORS_COMMON_AVX512_DESCRIPT    = "Subroutines which are common for Chebyshev HydroMeteors shape computation (AVX512)."
     ! Constants
     real(kind=sp),      parameter, private :: Deg90Rad   = 1.5708_sp
     type(ZMM16r4_t),    parameter, private :: vD90Rad    = ZMM16r4_t(Deg90Rad)
     type(ZMM16r4_t),    parameter, private :: vScaleToCm = ZMM16r4_t(3000.0_sp)
     ! For loop-unrolling (4x)
     real(kind=sp), dimension(0:15), parameter, private :: VINC0 = [1.0_sp,2.0_sp,3.0_sp,4.0_sp, &
                                                                    5.0_sp,6.0_sp,7.0_sp,8.0_sp, &
                                                                    9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                                    13.0_sp,14.0_sp,15.0_sp,16.0_sp]
     real(kind=sp), dimension(0:15), parameter, private :: VINC1 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                                    21.0_sp,22.0_sp,23.0_sp,24.0_sp, &
                                                                    25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                                    29.0_sp,30.0_sp,31.0_sp,32.0_sp]
     real(kind=sp), dimension(0:15), parameter, private :: VINC2 = [33.0_sp,34.0_sp,35.0_sp,36.0_sp, &
                                                                    37.0_sp,38.0_sp,39.0_sp,40.0_sp, &
                                                                    41.0_sp,42.0_sp,43.0_sp,44.0_sp, &
                                                                    45.0_sp,46.0_sp,47.0_sp,48.0_sp]
     real(kind=sp), dimension(0:15), parameter, private :: VINC3 = [49.0_sp,50.0_sp,51.0_sp,52.0_sp, &
                                                                    53.0_sp,54.0_sp,55.0_sp,56.0_sp, &
                                                                    57.0_sp,58.0_sp,59.0_sp,60.0_sp, &
                                                                    61.0_sp,62.0_sp,63.0_sp,64.0_sp]

   contains

    !===================================================================
    !                   Computational procedures 
    !  These procedures are not type bound in order to facilitate potential
    !  OpenMP parallelization (to be done later).
    !  Only Chebyshev Particles of type T2 and T4 should be used.
    !===================================================================

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeShape_ZMM16r4(pshape,radii,cn,cdef,nshpts,np) !GCC$ ATTRIBUTES hot :: ComputeShape_ZMM16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeShape_ZMM16r4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine ComputeShape_ZMM16r4(pshape,radii,cn,cdef,nshpts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeShape_ZMM16r4  
#endif
           use mod_vecconsts,  only : zmm16r4_one,zmm16r4_zero,zmm16r4_twopi
           type(ZMM16r4_t),   contiguous, dimension(:,:),  intent(inout) :: pshape  !   type(ZMM16r4_t), dimension(nshpts,np)
           real(kind=sp),     contiguous, dimension(:),    intent(inout) :: radii   !   real(kind=sp),  dimension(np)
           real(kind=sp),     contiguous, dimension(:),    intent(inout) :: cn      !   real(kind=sp),  dimension(np)
           real(kind=sp),     contiguous, dimension(:),    intent(inout) :: cdef    !   real(kind=sp),  dimension(np)
           integer(kind=int4),                             intent(in)    :: nshpts
           integer(kind=int4),                             intent(in)    :: np
           ! Locals
           ! Error checking removed
           type :: Vtheta4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vtheta0
              type(ZMM16r4_t) :: vtheta1
              type(ZMM16r4_t) :: vtheta2
              type(ZMM16r4_t) :: vtheta3
           end type Vtheta4CacheLines_t
           type :: Vthinc4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vthinc0
              type(ZMM16r4_t) :: vthinc1
              type(ZMM16r4_t) :: vthinc2
              type(ZMM16r4_t) :: vthinc3
           end type Vthinc4CacheLines_t
           type :: Termx4CacheLines_t
              sequence
              type(ZMM16r4_t) :: term0
              type(ZMM16r4_t) :: term1
              type(ZMM16r4_t) :: term2
              type(ZMM16r4_t) :: term3
           end type Termx4CacheLines_t
#if defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta4CL
           type(Vtheta4CacheLines_t) :: Vtheta4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vtheta4CacheLines_t) :: Vtheta4CL !GCC$ ATTRIBUTES aligned(64) :: Vtheta4CL
#endif
#if defined __INTEL_COMPILER        
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc4CL
         
           type(Vthinc4CacheLines_t) :: Vthinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vthinc4CacheLines_t) :: Vthinc4CL !GCC$ ATTRIBUTES aligned(64) :: Vthinc4CL
#endif
#if defined __INTEL_COMPILER           
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term4CL

           type(Termx4CacheLines_t) :: Term4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Termx4CacheLines_t) :: Term4CL    !GCC$ ATTRIBUTES aligned(64) :: Term4CL
#endif
#if defined __INTEL_COMPILER           
           !DIR$ ATTRIBUTES ALIGN : 64 :: cn_rand

           type(ZMM16r4_t), automatic :: cn_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cn_rand   !GCC$ ATTRIBUTES aligned(64) :: cn_rand
#endif
#if defined __INTEL_COMPILER
           ! DIR$ ATTRIBUTES ALIGN : 64 :: sphr_rand

           type(ZMM16r4_t), automatic :: sphr_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: sphr_rand  !GCC$ ATTRIBUTES aligned(64) :: sphr_rand
#endif
#if defined __INTEL_COMPILER           
           !DIR$ ATTRIBUTES ALIGN : 64  :: cdef_rand

           type(ZMM16r4_t), automatic :: cdef_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cdef_rand  !GCC$ ATTRIBUTES aligned(64) :: cdef_rand
#endif
#if defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS

           type(ZMM16r4_t), automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vNPTS      !GCC$ ATTRIBUTES aligned(64) :: vNPTS
#endif
#if defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vC

           type(ZMM16r4_t), automatic :: vC
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vC         !GCC$ ATTRIBUTES aligned(64) :: vC
#endif
#if defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 ::  tmp

           type(ZMM16r4_t), automatic :: tmp
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: tmp        !GCC$ ATTRIBUTES aligned(64) :: tmp
#endif
           real(kind=sp),  automatic :: cbn
           real(kind=sp),  automatic :: sphr
           real(kind=sp),  automatic :: cdef
           integer(kind=int4), automatic :: j,i
             ! Exec code ....
           cn_rand             = zmm16r4_zero
           sphr_rand           = zmm16r4_zero
           cdef_rand           = zmm16r4_zero
           Vtheta4CL.vtheta0   = zmm16r4_zero
           Vtheta4CL.vtheta1   = zmm16r4_zero
           Vtheta4CL.vtheta2   = zmm16r4_zero
           Vtheta4CL.vtheta3   = zmm16r4_zero
           Vthinc4CL.vthinc0   = zmm16r4_zero
           Vthinc4CL.vthinc1   = zmm16r4_zero
           Vthinc4CL.vthinc2   = zmm16r4_zero
           Vthinc4CL.vthinc3   = zmm16r4_zero
           vNPTS               = zmm16r4_zero
           tmp                 = zmm16r4_zero
           vC                  = zmm16r4_zero
           Term4CL.term0       = zmm16r4_zero
           Term4CL.term1       = zmm16r4_zero
           Term4CL.term2       = zmm16r4_zero
           Term4CL.term3       = zmm16r4_zero
           cbn       = 0.0_sp
           sphr      = 0.0_sp
           cdef      = 0.0_sp
           vNPTS.v   = real(nshpts,kind=sp)
           ! First touch
           pshape = zmm16r4_zero
           radii = 0.0_sp
           do j=1,  np
#if defined __ICC || defined  __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED cn:64,radii:64,cdef:64
#endif
              call RANDOM_SEED()
              call RANDOM_NUMBER(cn)
              if(0.0_sp == cbn) cbn = 0.1_sp
              cbn = cbn*10.0_sp
              cn(j) = cbn ! caching values for the different proprties computations
              cn_rand.v = cbn
              call RANDOM_NUMBER(sphr)
              sphr = sphr*3.0_sp
             ! asphr(j) = sphr
              sphr_rand.v = sphr
              radii(j) = sphr ! caching not used here directly.
              call RANDOM_NUMBER(cdef)
              cdef_rand.v = cdef
              cdef(j) = cdef
              vC.v = zmm16r4_twopi.v*sphr_rand.v
              tmp.v = vC.v/vNPTS.v
              Vthinc4CL.vthinc0.v = tmp.v
              Vthinc4CL.vthinc0.v = Vthinc4CL.vthinc0.v*VINC0.v
              Vthinc4CL.vthinc1.v = tmp.v
              Vthinc4CL.vthinc1.v = Vthinc4CL.vthinc1.v*VINC1.v
              Vthinc4CL.vthinc2.v = tmp.v
              Vthinc4CL.vthinc2.v = Vthinc4CL.vthinc2.v*VINC2.v
              Vthinc4CL.vthinc3.v = tmp.v
              Vthinc4CL.vthinc3.v = Vthinc4CL.vthinc3.v*VINC3.v
              Vtheta4CL.vtheta0   = zmm16r4_zero
              Term4CL.term0       = zmm16r4_zero
              Vtheta4CL.vtheta1   = zmm16r4_zero
              Term4CL.term1       = zmm16r4_zero
              Vtheta4CL.vtheta2   = zmm16r4_zero
              Term4CL.term2       = zmm16r4_zero
              Vtheta4CL.vtheta3   = zmm16r4_zero
              Term4CL.term3       = zmm16r4_zero
#if defined __ICC ||  defined __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
              !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif
              do i=1, nshpts-3, 4
#if defined __ICC || defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED pshape:64
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
                 !GCC$ builtin (sin) attributes simd
                 !GCC$ builtin (cos) attributes simd
#endif
                 Vtheta4CL.vtheta0.v   = Vtheta4CL.vtheta0.v+Vthinc4CL.vthinc0.v
                 Term4CL.term0.v       = zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta4CL.vtheta0.v)
                 pshape(i+0,j).v = sphr_rand.v*Term4CL.term0.v
                 Vtheta4CL.vtheta1.v   = Vtheta4CL.vtheta1.v+Vthinc4CL.vthinc1.v
                 Term4CL.term1.v       = zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta4CL.vtheta1.v)
                 pshape(i+1,j).v = sphr_rand.v*Term4CL.term1.v
                 Vtheta4CL.vtheta2.v   = Vtheta4CL.vtheta2.v+Vthinc4CL.vthinc2.v
                 Term4CL.term2.v       = zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta4CL.vtheta2.v)
                 pshape(i+2,j).v = sphr_rand.v*Term4CL.term2.v
                 Vtheta4CL.vtheta3.v   = Vtheta4CL.vtheta3.v+Vthinc4CL.vthinc3.v
                 Term4CL.term3.v       = zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta4CL.vtheta3.v)
                 pshape(i+3,j).v = sphr_rand.v*Term4CL.term3.v
              end do
           end do
     end subroutine ComputeShape_ZMM16r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeXparam_ZMM16r4(paramx,radii,cn,cdef,nxpts,np) !GCC$ ATTRIBUTES hot :: ComputeXparam_ZMM16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeXparam_ZMM16r4
#elif defined __INTEL_COMPILER
     subroutine ComputeXparam_ZMM16r4(paramx,radii,cn,cdef,nxpts,np)

       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeXparam_ZMM16r4
#endif         
           use mod_vecconsts, only : zmm16r4_twopi,zmm16r4_one,zmm16r4_zero
           type(ZMM16r4_t),  contiguous, dimension(:,:),      intent(inout) :: paramx ! dimension(nxpts,np)
           real(kind=sp),    contiguous, dimension(:),        intent(in)    :: radii  ! dimension(np), 
           real(kind=sp),    contiguous, dimension(:),        intent(inout) :: cn     ! dimension(np),
           real(kind=sp),    contiguous, dimension(:),        intent(inout) :: cdef   ! dimension(np),
           integer(kind=int4),                                intent(in)    :: nxpts
           integer(kind=int4),                                intent(in)    :: np
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vtheta0
              type(ZMM16r4_t) :: vtheta1
              type(ZMM16r4_t) :: vtheta2
              type(ZMM16r4_t) :: vtheta3
           end type Vtheta4CacheLines_t
           type :: Vphi4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vphi0
              type(ZMM16r4_t) :: vphi1
              type(ZMM16r4_t) :: vphi2
              type(ZMM16r4_t) :: vphi3
           end type Vphi4CacheLines_t
           type :: Vthinc4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vthinc0
              type(ZMM16r4_t) :: vthinc1
              type(ZMM16r4_t) :: vthinc2
              type(ZMM16r4_t) :: vthinc3
           end type Vthinc4CacheLines_t
           type :: Vphinc4CacheLines
              sequence
              type(ZMM16r4_t) :: vphinc0
              type(ZMM16r4_t) :: vphinc1
              type(ZMM16r4_t) :: vphinc2
              type(ZMM16r4_t) :: vphinc3
           end type Vphinc4CacheLines_t
           type :: Termx4CacheLines_t
              sequence
              type(ZMM16r4_t) :: term0
              type(ZMM16r4_t) :: term1
              type(ZMM16r4_t) :: term2
              type(ZMM16r4_t) :: term3
           end type Termx4CacheLines_t
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta4CL

           type(Vtheta4CacheLines_t) :: Vtheta4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vtheta4CacheLines_t) :: Vtheta4CL    !GCC$ ATTRIBUTES aligned(64) :: Vtheta4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphi4CL

           type(Vphi4CacheLines_t)  :: Vphi4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vphi4CacheLines_t)  :: Vphi4CL       !GCC$ ATTRIBUTES aligned(64) :: Vphi4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc4CL

           type(Vthinc4CacheLines_t) :: Vthinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vthinc4CacheLines_t) :: Vthinc4CL    !GCC$ ATTRIBUTES aligned(64) :: Vthinc4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphinc4CL

           type(Vphinc4CacheLines_t) :: Vphinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vphinc4CacheLines_t) :: Vphinc4CL    !GCC$ ATTRIBUTES aligned(64) :: Vphinc4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term4CL

           type(Termx4CacheLines_t) :: Term4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Termx4CacheLines_t) :: Term4CL       !GCC$ ATTRIBUTES aligned(64) :: Term4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: cn_rand

           type(ZMM16r4_t), automatic :: cn_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cn_rand      !GCC$ ATTRIBUTES aligned(64) :: cn_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           ! DIR$ ATTRIBUTES ALIGN : 64 :: sphr_rand
           type(ZMM16r4_t), automatic :: sphr_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: sphr_rand    !GCC$ ATTRIBUTES aligned(64) :: sphr_rand
#endif
#if defined __ICC || defined  __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64  :: cdef_rand
           type(ZMM16r4_t), automatic :: cdef_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cdef_rand    !GCC$ ATTRIBUTES aligned(64) :: cdef_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
           type(ZMM16r4_t), automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vNPTS        !GCC$ ATTRIBUTES aligned(64) :: vNPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vC
           type(ZMM16r4_t), automatic :: vC
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vC           !GCC$ ATTRIBUTES aligned(64) :: vC
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: tmp1,tmp2
           type(ZMM16r4_t), automatic :: tmp1,tmp2
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: tmp1         !GCC$ ATTRIBUTES aligned(64) :: tmp1
           type(ZMM16r4_t), automatic :: tmp2         !GCC$ ATTRIBUTES aligned(64) :: tmp2
#endif
           integer(kind=int4), automatic :: j,i
           !Exec code ....
           cn_rand             = zmm16r4_zero
           sphr_rand           = zmm16r4_zero
           cdef_rand           = zmm16r4_zero
           Vtheta4CL.vtheta0   = zmm16r4_zero
           Vtheta4CL.vtheta1   = zmm16r4_zero
           Vtheta4CL.vtheta2   = zmm16r4_zero
           Vtheta4CL.vtheta3   = zmm16r4_zero
           Vphi4CL.vphi0       = zmm16r4_zero
           Vphi4CL.vphi1       = zmm16r4_zero
           Vphi4CL.vphi2       = zmm16r4_zero
           Vphi4CL.vphi3       = zmm16r4_zero
           Vthinc4CL.vthinc0   = zmm16r4_zero
           Vthinc4CL.vthinc1   = zmm16r4_zero
           Vthinc4CL.vthinc2   = zmm16r4_zero
           Vthinc4CL.vthinc3   = zmm16r4_zero
           Vphinc4CL.vphinc0   = zmm16r4_zero
           Vphinc4CL.vphinc1   = zmm16r4_zero
           Vphinc4CL.vphinc2   = zmm16r4_zero
           Vphinc4CL.vphinc3   = zmm16r4_zero
           vNPTS               = zmm16r4_zero
           tmp1                = zmm16r4_zero
           tmp2                = zmm16r4_zero
           vC                  = zmm16r4_zero
           Term4CL.term0       = zmm16r4_zero
           Term4CL.term1       = zmm16r4_zero
           Term4CL.term2       = zmm16r4_zero
           Term4CL.term3       = zmm16r4_zero
           vNPTS.v   = real(nxpts,kind=sp)
           ! First touch
           paramx = zmm16r4_zero
           do j=1, np
#if defined __ICC || defined  __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED cn:64,radii:64,cdef:64
#endif
              cn_rand.v   = cn(j)
              sphr_rand.v = radii(j)
              cdef_rand.v = cdef(j)
              vC.v        = zmm16r4_twopi.v*sphr_rand.v
              tmp1.v      = vC.v/vNPTS.v
              tmp2.v      = tmp1.v
              Vthinc4CL.vthinc0.v   = tmp1.v
              Vthinc4CL.vthinc0.v   = Vthinc4CL.vthinc0.v*VINC0.v
              Vphinc4CL.vphinc0.v   = tmp2.v
              Vphinc4CL.vphinc0.v   = Vphinc4CL.vphinc0.v*VINC0.v
              Vthinc4CL.vthinc1.v   = tmp1.v
              Vthinc4CL.vthinc1.v   = Vthinc4CL.vthinc1.v*VINC1.v
              Vphinc4CL.vphinc1.v   = tmp2.v
              Vphinc4CL.vphinc1.v   = Vphinc4CL.vphinc1.v*VINC1.v
              Vthinc4CL.vthinc2.v   = tmp1.v
              Vthinc4CL.vthinc2.v   = Vthinc4CL.vthinc2.v*VINC2.v
              Vphinc4CL.vphinc2.v   = tmp2.v
              Vphinc4CL.vphinc2.v   = Vphinc4CL.vphinc2.v*VINC2.v
              Vthinc4CL.vthinc3.v   = tmp1.v
              Vthinc4CL.vthinc3.v   = Vthinc4CL.vthinc3.v*VINC3.v
              Vphinc4CL.vphinc3.v   = tmp2.v
              Vphinc4CL.vphinc3.v   = Vphinc4CL.vphinc3.v*VINC3.v
              Vtheta4CL.vtheta0     = zmm16r4_zero
              Vphi4CL.vphi0         = zmm16r4_zero
              Term4CL.term0         = zmm16r4_zero
              Vtheta4CL.vtheta1     = zmm16r4_zero
              Vphi4CL.vphi1         = zmm16r4_zero
              Term4CL.term1         = zmm16r4_zero
              Vtheta4CL.vtheta2     = zmm16r4_zero
              Vphi4CL.vphi2         = zmm16r4_zero
              Term4CL.term2         = zmm16r4_zero
              Vtheta4CL.vtheta3     = zmm16r4_zero
              Vphi4CL.vphi3         = zmm16r4_zero
              Term4CL.term3         = zmm16r4_zero
#if defined __ICC || defined  __INTEL_COMPILER
              !DIR$ VECTOR ALWAYS
              !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
              !GCC$ VECTOR
#endif
              do i=1, nxpts-3, 4
#if defined __ICC || defined __INTEL_COMPILER
                 !DIR$ ASSUME_ALIGNED paramx:64
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
                 !GCC$ builtin (sin) attributes simd
                 !GCC$ builtin (cos) attributes simd
#endif
                 Vtheta4CL.vtheta0.v = Vtheta4CL.vtheta0.v+Vthinc4CL.vthinc0.v
                 Vphi4CL.vphi0.v   = Vphi4CL.vphi0.v+Vphinc4CL.vphinc0.v
                 Term4CL.term0.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta0.v))
                 Term4CL.term0.v   = Term4CL.term0.v*sin(Vtheta4CL.vtheta0.v)*cos(Vphi4CL.vphi0.v)
                 paramx(i+0,j).v   = Term4CL.term0.v
                 Vtheta4CL.vtheta1.v = Vtheta4CL.vtheta1.v+Vthinc4CL.vthinc1.v
                 Vphi4CL.vphi1.v   = Vphi4CL.vphi1.v+Vphinc4CL.vphinc1.v
                 Term4CL.term1.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta1.v))
                 Term4CL.term1.v   = Term4CL.term1.v*sin(Vtheta4CL.vtheta1.v)*cos(Vphi4CL.vphi1.v)
                 paramx(i+1,j).v   = Term4CL.term1.v
                 Vtheta4CL.vtheta2.v = Vtheta4CL.vtheta2.v+Vthinc4CL.vthinc2.v
                 Vphi4CL.vphi2.v   = Vphi4CL.vphi2.v+Vphinc4CL.vphinc2.v
                 Term4CL.term2.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta2.v))
                 Term4CL.term2.v   = Term4CL.term2.v*sin(Vtheta4CL.vtheta2.v)*cos(Vphi4CL.vphi2.v)
                 paramx(i+2,j).v   = Term4CL.term2.v
                 Vtheta4CL.vtheta3.v = Vtheta4CL.vtheta3.v+Vthinc4CL.vthinc3.v
                 Vphi4CL.vphi3.v   = Vphi4CL.vphi3.v+Vphinc4CL.vphinc3.v
                 Term4CL.term3.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta3.v))
                 Term4CL.term3.v   = Term4CL.term3.v*sin(Vtheta4CL.vtheta3.v)*cos(Vphi4CL.vphi3.v)
                 paramx(i+3,j).v   = Term4CL.term3.v
              end do
           end do
     end subroutine ComputeXparam_ZMM16r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeYparam_ZMM16r4(paramy,radii,cn,cdef,nypts,np)  !GCC$ ATTRIBUTES hot :: ComputeYparam_ZMM16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeYparam_ZMM16r4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine ComputeYparam_ZMM16r4(paramy,radii,cn,cdef,nypts,np)
     !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ComputeYparam_ZMM16r4
#endif
          use mod_vecconsts, only : zmm16r4_twopi,zmm16r4_one,zmm16r4_zero
           type(ZMM16r4_t),  contiguous, dimension(:,:),        intent(inout) :: paramy !nypts,np
           real(kind=sp),    contiguous, dimension(:),          intent(in)    :: radii !np
           real(kind=sp),    contiguous, dimension(:),          intent(inout) :: cn    !np
           real(kind=sp),    contiguous, dimension(:),          intent(inout) :: cdef  !np
           integer(kind=int4),                                  intent(in)    :: nypts
           integer(kind=int4),                                  intent(in)    :: np
             ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vtheta0
              type(ZMM16r4_t) :: vtheta1
              type(ZMM16r4_t) :: vtheta2
              type(ZMM16r4_t) :: vtheta3
           end type Vtheta4CacheLines_t
           type :: Vphi4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vphi0
              type(ZMM16r4_t) :: vphi1
              type(ZMM16r4_t) :: vphi2
              type(ZMM16r4_t) :: vphi3
           end type Vphi4CacheLines_t
           type :: Vthinc4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vthinc0
              type(ZMM16r4_t) :: vthinc1
              type(ZMM16r4_t) :: vthinc2
              type(ZMM16r4_t) :: vthinc3
           end type Vthinc4CacheLines_t
           type :: Vphinc4CacheLines
              sequence
              type(ZMM16r4_t) :: vphinc0
              type(ZMM16r4_t) :: vphinc1
              type(ZMM16r4_t) :: vphinc2
              type(ZMM16r4_t) :: vphinc3
           end type Vphinc4CacheLines_t
           type :: Termx4CacheLines_t
              sequence
              type(ZMM16r4_t) :: term0
              type(ZMM16r4_t) :: term1
              type(ZMM16r4_t) :: term2
              type(ZMM16r4_t) :: term3
           end type Termx4CacheLines_t
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta4CacheLines_t) :: Vtheta4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vtheta4CacheLines_t) :: Vtheta4CL    !GCC$ ATTRIBUTES aligned(64) :: Vtheta4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphi4CL
           type(Vphi4CacheLines_t)  :: Vphi4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vphi4CacheLines_t)  :: Vphi4CL       !GCC$ ATTRIBUTES aligned(64) :: Vphi4CL
#endif
#if defined __ICC || defined __INTEL__COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc4CL
           type(Vthinc4CacheLines_t) :: Vthinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vthinc4CacheLines_t) :: Vthinc4CL    !GCC$ ATTRIBUTES aligned(64) :: Vthinc4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphinc4CL
           type(Vphinc4CacheLines_t) :: Vphinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Vphinc4CacheLines_t) :: Vphinc4CL    !GCC$ ATTRIBUTES aligned(64) :: Vphinc4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term4CL
           type(Termx4CacheLines_t) :: Term4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(Termx4CacheLines_t) :: Term4CL       !GCC$ ATTRIBUTES aligned(64) :: Term4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: cn_rand
           type(ZMM16r4_t), automatic :: cn_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cn_rand      !GCC$ ATTRIBUTES aligned(64) :: cn_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           ! DIR$ ATTRIBUTES ALIGN : 64 :: sphr_rand
           type(ZMM16r4_t), automatic :: sphr_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: sphr_rand    !GCC$ ATTRIBUTES aligned(64) :: sphr_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64  :: cdef_rand
           type(ZMM16r4_t), automatic :: cdef_rand
#elif defined __GFORTRAN__ &&!defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: cdef_rand    !GCC$ ATTRIBUTES aligned(64) :: cdef_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
           type(ZMM16r4_t), automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vNPTS        !GCC$ ATTRIBUTES aligned(64) :: vNPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 :: vC
           type(ZMM16r4_t), automatic :: vC
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: vC           !GCC$ ATTRIBUTES aligned(64) :: vC
#endif
#if defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES ALIGN : 64 tmp1,tmp2
           type(ZMM16r4_t), automatic :: tmp1,tmp2
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
           type(ZMM16r4_t), automatic :: tmp1         !GCC$ ATTRIBUTES aligned(64) :: tmp1
           type(ZMM16r4_t), automatic :: tmp2         !GCC$ ATTRIBUTES aligned(64) :: tmp2
#endif
           integer(kind=int4), automatic :: j,i
           ! Exec code .....
           cn_rand             = zmm16r4_zero
           sphr_rand           = zmm16r4_zero
           cdef_rand           = zmm16r4_zero
           Vtheta4CL.vtheta0   = zmm16r4_zero
           Vtheta4CL.vtheta1   = zmm16r4_zero
           Vtheta4CL.vtheta2   = zmm16r4_zero
           Vtheta4CL.vtheta3   = zmm16r4_zero
           Vphi4CL.vphi0       = zmm16r4_zero
           Vphi4CL.vphi1       = zmm16r4_zero
           Vphi4CL.vphi2       = zmm16r4_zero
           Vphi4CL.vphi3       = zmm16r4_zero
           Vthinc4CL.vthinc0   = zmm16r4_zero
           Vthinc4CL.vthinc1   = zmm16r4_zero
           Vthinc4CL.vthinc2   = zmm16r4_zero
           Vthinc4CL.vthinc3   = zmm16r4_zero
           Vphinc4CL.vphinc0   = zmm16r4_zero
           Vphinc4CL.vphinc1   = zmm16r4_zero
           Vphinc4CL.vphinc2   = zmm16r4_zero
           Vphinc4CL.vphinc3   = zmm16r4_zero
           vNPTS               = zmm16r4_zero
           tmp1                = zmm16r4_zero
           tmp2                = zmm16r4_zero
           vC                  = zmm16r4_zero
           Term4CL.term0       = zmm16r4_zero
           Term4CL.term1       = zmm16r4_zero
           Term4CL.term2       = zmm16r4_zero
           Term4CL.term3       = zmm16r4_zero
           vNPTS.v   = real(nypts,kind=sp)
           ! First touch
           paramy = zmm16r4_zero
           do j=1, np
#if defined __ICC || defined __INTEL_COMPILER
              !DIR$ ASSUME_ALIGNED cn:64
              !DIR$ ASSUME_ALIGNED radii:64
              !DIR$ ASSUME_ALIGNED cdef:64
#endif
                cn_rand.v   = cn(j)
                sphr_rand.v = radii(j)
                vC.v        = zmm16r4_twopi.v*sphr_rand.v
                cdef_rand.v = cdef(j)
                !
                tmp1.v      = vC.v/vNPTS.v
                tmp2.v      = tmp1.v
                Vthinc4CL.vthinc0.v   = tmp1.v
                Vthinc4CL.vthinc0.v   = Vthinc4CL.vthinc0.v*VINC0.v
                Vphinc4CL.vphinc0.v   = tmp2.v
                Vphinc4CL.vphinc0.v   = Vphinc4CL.vphinc0.v*VINC0.v
                Vthinc4CL.vthinc1.v   = tmp1.v
                Vthinc4CL.vthinc1.v   = Vthinc4CL.vthinc1.v*VINC1.v
                Vphinc4CL.vphinc1.v   = tmp2.v
                Vphinc4CL.vphinc1.v   = Vphinc4CL.vphinc1.v*VINC1.v
                Vthinc4CL.vthinc2.v   = tmp1.v
                Vthinc4CL.vthinc2.v   = Vthinc4CL.vthinc2.v*VINC2.v
                Vphinc4CL.vphinc2.v   = tmp2.v
                Vphinc4CL.vphinc2.v   = Vphinc4CL.vphinc2.v*VINC2.v
                Vthinc4CL.vthinc3.v   = tmp1.v
                Vthinc4CL.vthinc3.v   = Vthinc4CL.vthinc3.v*VINC3.v
                Vphinc4CL.vphinc3.v   = tmp2.v
                Vphinc4CL.vphinc3.v   = Vphinc4CL.vphinc3.v*VINC3.v
                Vtheta4CL.vtheta0     = zmm16r4_zero
                Vphi4CL.vphi0         = zmm16r4_zero
                Term4CL.term0         = zmm16r4_zero
                Vtheta4CL.vtheta1     = zmm16r4_zero
                Vphi4CL.vphi1         = zmm16r4_zero
                Term4CL.term1         = zmm16r4_zero
                Vtheta4CL.vtheta2     = zmm16r4_zero
                Vphi4CL.vphi2         = zmm16r4_zero
                Term4CL.term2         = zmm16r4_zero
                Vtheta4CL.vtheta3     = zmm16r4_zero
                Vphi4CL.vphi3         = zmm16r4_zero
                Term4CL.term3         = zmm16r4_zero
#if defined __ICC || defined __INTEL_COMPILER
                !DIR$ VECTOR ALWAYS
                !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                !GCC$ VECTOR
#endif
                do i=1, nypts-3, 4
#if defined __ICC || defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED paramy:64
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
                      !GCC$ builtin (sin) attributes simd
                      !GCC$ builtin (cos) attributes simd
#endif
                     Vtheta4CL.vtheta0.v = Vtheta4CL.vtheta0.v+Vthic4CL.vthinc0.v
                     Vphi4CL.vphi0.v   = Vphi4CL.vphi0.v+Vphinc4CL.vphinc0.v
                     Term4CL.term0.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta0.v))
                     Term4CL.term0.v   = Term4CL.term0.v*sin(Vtheta4CL.vtheta0.v)*sin(Vphi4CL.vphi0.v)
                     paramy(i+0,j).v = Term4CL.term0.v
                     Vtheta4CL.vtheta1.v = Vtheta4CL.vtheta1.v+Vthinc4CL.vthinc1.v
                     Vphi4CL.vphi1.v   = Vphi4CL.vphi1.v+Vphinc4CL.vphinc1.v
                     Term4CL.term1.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta1.v))
                     Term4CL.term1.v   = Term4CL.term1.v*sin(Vtheta4CL.vtheta1.v)*sin(Vphi4CL.vphi1.v)
                     paramy(i+1,j).v = Term4CL.term1.v
                     Vtheta4CL.vtheta2.v = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                     Vphi4CL.vphi2.v   = Vphi4CL.vphi2.v+Vphi4CL.vphinc2.v
                     Term4CL.term2.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta2.v))
                     Term4CL.term2.v   = Term4CL.term2.v*sin(Vtheta4CL.vtheta2.v)*sin(Vphi4CL.vphi2.v)
                     paramy(i+2,j).v = Term4CL.term2.v
                     Vtheta4CL.vtheta3.v = Vtheta4CL.vtheta3.v+Vtheta4CL.vthinc3.v
                     Vphi4CL.vphi3.v   = Vphi4CL.vphi3.v+Vphinc4CL.vphinc3.v
                     Term4CL.term3.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta3.v))
                     Term4CL.term3.v   = Term4CL.term3.v*sin(Vtheta4CL.vtheta3.v)*sin(Vtheta4CL.vphi3.v)
                     paramy(i+3,j).v = Term4CL.term3.v
                  end do
               end do

     end subroutine ComputeYparam_ZMM16r4

             
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
     subroutine ComputeZparam_ZMM16r4(paramz,radii,cn,cdef,nzpts,np)  !GCC$ ATTRIBUTES hot :: ComputeZparam_ZMM16r4 !GCC$ ATTRIBUTES aligned(32) :: ComputeZparam_ZMM16r4
#elif defined __ICC || defined __INTEL_COMPILER
     subroutine ComputeZparam_ZMM16r4(paramz,radii,cn,cdef,nzpts,np)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeZparam_ZMM16r4
#endif
           use mod_vecconsts, only : zmm16r4_twopi,zmm16r4_one,zmm16r4_zero
           type(ZMM16r4_t),  contiguous, dimension(:,:),           intent(inout) :: paramz !nzpts,np
           real(kind=sp),    contiguous, dimension(:),             intent(in)    :: radii  ! np
           real(kind=sp),    contiguous, dimension(:),             intent(inout) :: cn     ! np
           real(kind=sp),    contiguous, dimension(:),             intent(inout) :: cdef   ! np
           integer(kind=int4),                                     intent(in)    :: nzpts
           integer(kind=int4),                                     intent(in)    :: np
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vtheta0
              type(ZMM16r4_t) :: vtheta1
              type(ZMM16r4_t) :: vtheta2
              type(ZMM16r4_t) :: vtheta3
            end type Vtheta4CacheLines_t
            type :: Vthinc4CacheLines_t
              sequence
              type(ZMM16r4_t) :: vthinc0
              type(ZMM16r4_t) :: vthinc1
              type(ZMM16r4_t) :: vthinc2
              type(ZMM16r4_t) :: vthinc3
            end type Vthinc4CacheLines_t
            type :: Termx4CacheLines_t
              sequence
              type(ZMM16r4_t) :: term0
              type(ZMM16r4_t) :: term1
              type(ZMM16r4_t) :: term2
              type(ZMM16r4_t) :: term3
           end type Termx4CacheLines_t
#if defined __ICC || defined __INTEL_COMPILER
             !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta4CL
            type(Vtheta4CacheLines_t) :: Vtheta4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(Vtheta4CacheLines_t) :: Vtheta4CL     !GCC$ ATTRIBUTES aligned(64) :: Vtheta4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
             !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc4CL
            type(Vthinc4CacheLines_t) :: Vthinc4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(Vthinc4CacheLines_t) :: Vthinc4CL     !GCC$ ATTRIBUTES aligned(64) :: Vthinc4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
             !DIR$ ATTRIBUTES ALIGN : 64 :: Term4CL
            type(Termx4CacheLines_t) :: Term4CL
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(Termx4CacheLines_t) :: Term4CL        !GCC$ ATTRIBUTES aligned(64) :: Term4CL
#endif
#if defined __ICC || defined __INTEL_COMPILER
            !DIR$ ATTRIBUTES ALIGN : 64 :: cn_rand
            type(ZMM16r4_t), automatic :: cn_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: cn_rand       !GCC$ ATTRIBUTES aligned(64) :: cn_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
            ! DIR$ ATTRIBUTES ALIGN : 64 :: sphr_rand
            type(ZMM16r4_t), automatic :: sphr_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: sphr_rand     !GCC$ ATTRIBUTES aligned(64) :: sphr_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
            !DIR$ ATTRIBUTES ALIGN : 64  :: cdef_rand
            type(ZMM16r4_t), automatic :: cdef_rand
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: cdef_rand     !GCC$ ATTRIBUTES aligned(64) :: cdef_rand
#endif
#if defined __ICC || defined __INTEL_COMPILER
            !DIR$ ATTRIBUTES ALIGN : 64 :: vNPTS
            type(ZMM16r4_t), automatic :: vNPTS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: vNPTS        !GCC$ ATTRIBUTES aligned(64) :: vNPTS
#endif
#if defined __ICC || defined __INTEL_COMPILER
            !DIR$ ATTRIBUTES ALIGN : 64 :: vC
            type(ZMM16r4_t), automatic :: vC
#elif defined __GFORTRAN__  && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: vC           !GCC$ ATTRIBUTES aligned(64) :: vC
#endif
#if defined __ICC || defined  __INTEL_COMPILER
            !DIR$ ATTRIBUTES ALIGN : 32 tmp1
            type(ZMM16r4_t), automatic :: tmp1
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
            type(ZMM16r4_t), automatic :: tmp1         !GCC$ ATTRIBUTES aligned(64) :: tmp1
#endif
            integer(kind=int4), automatic :: j,i
            ! Exec code .....
            cn_rand             = zmm16r4_zero
            sphr_rand           = zmm16r4_zero
            cdef_rand           = zmm16r4_zero
            Vtheta4CL.vtheta0   = ymm8r4_zero
            Vtheta4CL.vtheta1   = zmm16r4_zero
            Vtheta4CL.vtheta2   = zmm16r4_zero
            Vtheta4CL.vtheta3   = zmm16r4_zero
            Vthinc4CL.vthinc0   = zmm16r4_zero
            Vthinc4CL.vthinc1   = zmm16r4_zero
            Vthinc4CL.vthinc2   = zmm16r4_zero
            Vthinc4CL.vthinc3   = zmm16r4_zero
            vNPTS               = zmm16r4_zero
            tmp1                = zmm16r4_zero
            !
            vC                  = zmm16r4_zero
            Term4CL.term0       = zmm16r4_zero
            Term4CL.term1       = zmm16r4_zero
            Term4CL.term2       = zmm16r4_zero
            Term4CL.term3       = zmm16r4_zero
            vNPTS.v   = real(nzpts,kind=sp)
            ! First touch
            paramz = zmm16r4_zero
            do j=1, np
#if defined __ICC || defined __INTEL_COMPILER
               !DIR$ ASSUME_ALIGNED cn:64
               !DIR$ ASSUME_ALIGNED radii:64
               !DIR$ ASSUME_ALIGNED cdef:64
#endif
                cn_rand.v   = cn(j)
                sphr_rand.v = radii(j)
                vC.v        = zmm16r4_twopi.v*sphr_rand.v
                cdef_rand.v = cdef(j)
                !
                tmp1.v      = vC.v/vNPTS.v
                Vthinc4CL.vthinc0.v   = tmp1.v
                Vthinc4CL.vthinc0.v   = Vthinc4CL.vthinc0.v*VINC0.v
                Vthinc4CL.vthinc1.v   = tmp1.v
                Vthinc4CL.vthinc1.v   = Vthinc4CL.vthinc1.v*VINC1.v
                Vthinc4CL.vthinc2.v   = tmp1.v
                Vthinc4CL.vthinc2.v   = Vthinc4CL.vthinc2.v*VINC2.v
                Vthinc4CL.vthinc3.v   = tmp1.v
                Vthinc4CL.vthinc3.v   = Vthinc4CL.vthinc3.v*VINC3.v
                Vtheta4CL.vtheta0     = zmm16r4_zero
                Term4CL.term0         = zmm16r4_zero
                Vtheta4CL.vtheta1     = zmm16r4_zero
                Term4CL.term1         = zmm16r4_zero
                Vtheta4CL.vtheta2     = zmm16r4_zero
                Term4CL.term2         = zmm16r4_zero
                Vtheta4CL.vtheta3     = zmm16r4_zero
                Term4CL.term3         = zmm16r4_zero
#if defined __ICC || defined __INTEL_COMPILER
                !DIR$ VECTOR ALWAYS
                !DIR$ CODE_ALIGN : 32
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
                !GCC$ VECTOR
#endif
                do i=1, nzpts-3, 4
#if defined __ICC || defined __INTEL_COMPILER
                   !DIR$ ASSUME_ALIGNED paramz:64
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
                   !GCC$ builtin (sin) attributes simd
                   !GCC$ builtin (cos) attributes simd
#endif
                     Vtheta4CL.vtheta0.v = Vtheta4CL.vtheta0.v+Vthinc4CL.vthinc0.v
                     Term4CL.term0.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta0.v))
                     Term4CL.term0.v   = term0.v*cos(Vtheta4CL.vtheta0.v)
                     paramz(i+0,j).v = Term4CL.term0.v
                     Vtheta4CL.vtheta1.v = Vtheta4CL.vtheta1.v+Vthinc4CL.vthinc1.v
                     Term4CL.term1.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta1.v))
                     Term4CL.term1.v   = term1.v*cos(Vtheta4CL.vtheta1.v)
                     paramz(i+1,j).v = Term4CL.term1.v
                     Vtheta4CL.vtheta2.v = Vtheta4CL.vtheta2.v+Vthinc4CL.vthinc2.v
                     Term4CL.term2.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta2.v))
                     Term4CL.term2.v   = term2.v*cos(Vtheta4CL.vtheta2.v)
                     paramz(i+2,j).v = Term4CL.term2.v
                     Vtheta4CL.vtheta3.v = Vtheta4CL.vtheta3.v+Vthinc4CL.vthinc3.v
                     Term4CL.term3.v   = sphr_rand.v*(zmm16r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta4CL.vtheta3.v))
                     Term4CL.term3.v   = term3.v*cos(Vtheta4CL.vtheta3.v)
                     paramz(i+3,j).v = Term4CL.term3.v
                 end do
              end do 
     end subroutine ComputeZparam_YMM8r4



     
end module mod_chebyshev_hydrometeors_common_AVX512
