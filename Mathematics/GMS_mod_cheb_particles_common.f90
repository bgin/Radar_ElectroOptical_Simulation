

module mod_cheb_particles_common

 !!=============================================================!!
 !!  Explicitly vectorized subroutines -- common for the        !!
 !!  Chebyshev particles shape components computations          !! 
 !!=============================================================!!
     
     use mod_kinds,    only : int4, sp
     use mod_vectypes, only : YMM8r4_t, Mask8_t
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59
     integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_COMMON_MAJOR = 1
     integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_COMMON_MINOR = 0
     integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_COMMON_MICRO = 0
     integer(kind=int4), parameter, public :: MOD_CHEB_PARTICLES_COMMON_FULLVER = 1000*MOD_CHEB_PARTICLES_COMMON_MAJOR + &
                                                                                  100*MOD_CHEB_PARTICLES_COMMON_MINOR  + &
                                                                                  10*MOD_CHEB_PARTICLES_COMMON_MICRO
     character(*),       parameter, public :: MOD_CHEB_PARTICLES_COMMON_CREATE_DATE = "18-08-2019 16:14 +00200 (SUN 18 AUG 2019 GMT+2)"
     character(*),       parameter, public :: MOD_CHEB_PARTICLES_COMMON_BUILD_DATE  = "00-00-0000 00:00"
     character(*),       parameter, public :: MOD_CHEB_PARTICLES_COMMON_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),       parameter, public :: MOD_CHEB_PARTICLES_COMMON_DESCRIPT    = "Common subroutines for Chebyshev particles shape computation."
     ! Constants
     real(kind=sp),    parameter, private  :: Deg90Rad = 1.5708_sp
     type(YMM8r4_t),   parameter, private  :: vD90Rad = YMM8r4_t(Deg90Rad)
     type(YMM8r4_t),   parameter, private  :: vScaleToCm = YMM8r4_t(3000.0_sp)
     ! For loop-unrolling
     real(kind=sp), dimension(0:7), parameter, private :: VINC0 = [1.0_sp,2.0_sp,3.0_sp, &
                                                         4.0_sp,5.0_sp,6.0_sp,7.0_sp,8.0_sp]
     real(kind=sp), dimension(0:7), parameter, private :: VINC2 = [9.0_sp,10.0_sp,11.0_sp,12.0_sp, &
                                                         13.0_sp,14.0_sp,15.0_sp,16.0_sp]
     real(kind=sp), dimension(0:7), parameter, private :: VINC3 = [17.0_sp,18.0_sp,19.0_sp,20.0_sp, &
                                                          21.0_sp,22.0_sp,23.0_sp,24.0_sp]
     real(kind=sp), dimension(0:7), parameter, private :: VINC4 = [25.0_sp,26.0_sp,27.0_sp,28.0_sp, &
                                                          29.0_sp,30.0_sp,31.0_sp]

     contains

    !===================================================================
    !                   Computational procedures 
    !  These procedures are not type bound in order to facilitate potential
    !  OpenMP parallelization (to be done later).
    !  Only Chebyshev Particles of type T2 and T4 should be used.
    !===================================================================

     subroutine ComputeShape_YMM8r4(pshape,radii,cn,cdef,nshpts,np)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeShape_YMM8r4
           use mod_vecconsts,     only : ymm8r4_one,ymm8r4_zero,ymm8r4_twopi
           type(YMM8r4_t), dimension(nshpts,np),     intent(inout) :: pshape
           real(kind=sp),  dimension(np),            intent(inout) :: radii
           real(kind=sp),  dimension(np),            intent(inout) :: cn
           real(kind=sp),  dimension(np),            intent(inout) :: cdef
           integer(kind=int4),                       intent(in)    :: nshpts
           integer(kind=int4),                       intent(in)    :: np
             ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
           end type Vtheta2CacheLines_t
           type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
           end type Vthinc2CacheLines_t
           type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
           end type Termx2CacheLines_t
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta2CacheLines_t) :: Vtheta2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
           type(Vthinc2CacheLines_t) :: Vthinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
           type(Termx2CacheLines_t) :: Term2CL
           !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
           type(YMM8r4_t), automatic :: cn_rand
           ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
           type(YMM8r4_t), automatic :: sphr_rand
           !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
           type(YMM8r4_t), automatic :: cdef_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic :: vC
           !DIR$ ATTRIBUTES ALIGN : 32 tmp
           type(YMM8r4_t), automatic :: tmp
           real(kind=sp),  automatic :: cn
           real(kind=sp),  automatic :: sphr
           real(kind=sp),  automatic :: cdef
           integer(kind=int4), automatic :: j,i
            ! Exec code ....
           cn_rand             = ymm8r4_zero
           sphr_rand           = ymm8r4_zero
           cdef_rand           = ymm8r4_zero
           Vtheta2CL.vtheta0   = ymm8r4_zero
           Vtheta2CL.vtheta1   = ymm8r4_zero
           Vtheta2CL.vtheta2   = ymm8r4_zero
           Vtheta2CL.vtheta3   = ymm8r4_zero
           Vthinc2CL.vthinc0   = ymm8r4_zero
           Vthinc2CL.vthinc1   = ymm8r4_zero
           Vthinc2CL.vthinc2   = ymm8r4_zero
           Vthinc2CL.vthinc3   = ymm8r4_zero
           vNPTS               = ymm8r4_zero
           tmp                 = ymm8r4_zero
           vC                  = ymm8r4_zero
           Term2CL.term0       = ymm8r4_zero
           Term2CL.term1       = ymm8r4_zero
           Term2CL.term2       = ymm8r4_zero
           Term2CL.term3       = ymm8r4_zero
           cn        = 0.0_sp
           sphr      = 0.0_sp
           cdef      = 0.0_sp
           vNPTS.v   = real(nshpts,kind=sp)
           ! First touch
           pshape = ymm8r4_zero
           radii = 0.0_sp
           do j=1,  np
              call RANDOM_SEED()
              call RANDOM_NUMBER(cn)
              if(0.0_sp == cn) cn = 0.1_sp
              cn = cn*10.0_sp
              cn(j) = cn ! caching values for the different proprties computations
              cn_rand.v = cn
              call RANDOM_NUMBER(sphr)
              sphr = sphr*3.0_sp
             ! asphr(j) = sphr
              sphr_rand.v = sphr
              radii(j) = sphr ! caching not used here directly.
              call RANDOM_NUMBER(cdef)
              cdef_rand.v = cdef
              cdef(j) = cdef
              vC.v = ymm8r4_twopi.v*sphr_rand.v
              tmp.v = vC.v/vNPTS.v
              Vthinc2CL.vthinc0.v = tmp.v
              Vthinc2CL.vthinc0.v = Vthinc2CL.vthinc0.v*VINC.v
              Vthinc2CL.vthinc1.v = tmp.v
              Vthinc2CL.vthinc1.v = Vthinc2CL.vthinc1.v*VINC2.v
              Vthinc2CL.vthinc2.v = tmp.v
              Vthinc2CL.vthinc2.v = Vthinc2CL.vthinc2.v*VINC3.v
              Vthinc2CL.vthinc3.v = tmp.v
              Vthinc2CL.vthinc3.v = Vthinc2CL.vthinc3.v*VINC4.v
              Vtheta2CL.vtheta0   = ymm8r4_zero
              Term2CL.term0       = ymm8r4_zero
              Vtheta2CL.vtheta1   = ymm8r4_zero
              Term2CL.term1       = ymm8r4_zero
              Vtheta2CL.vtheta2   = ymm8r4_zero
              Term2CL.term2       = ymm8r4_zero
              Vtheta2CL.vtheta3   = ymm8r4_zero
              Term2CL.term3       = ymm8r4_zero
              !DIR$ VECTOR ALWAYS
              do i=1, nshpts-3, 4
                 Vtheta2CL.vtheta0.v   = Vtheta2CL.vtheta0.v+Vthinc2CL.vthinc0.v
                 Term2CL.term0.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta0.v)
                 pshape(i+0,j).v = sphr_rand.v*Term2CL.term0.v
                 Vtheta2CL.vtheta1.v   = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                 Term2CL.term1.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta1.v)
                 pshape(i+1,j).v = sphr_rand.v*Term2CL.term1.v
                 Vtheta2CL.vtheta2.v   = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                 Term2CL.term2.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta2.v)
                 pshape(i+2,j).v = sphr_rand.v*Term2CL.term2.v
                 Vtheta2CL.vtheta3.v   = Vtheta2CL.vtheta3.v+Vthinc2CL.vthinc3.v
                 Term2CL.term3.v       = ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v+Vtheta2CL.vtheta3.v)
                 pshape(i+3,j).v = sphr_rand.v*Term2CL.term3.v
              end do
           end do
     end subroutine ComputeShape_YMM8r4

     subroutine ComputeXparam_YMM8r4(paramx,radii,cn,cdef,nxpts,np)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ComputeXparam_YMM8r4
           use mod_vecconsts, only : ymm8r4_twopi,ymm8r4_one,ymm8r4_zero
           type(YMM8r4_t),  dimension(nxpts,np),  intent(inout) :: paramx
           real(kind=sp),   dimension(np),        intent(in)    :: radii
           real(kind=sp),   dimension(np),        intent(inout) :: cd
           real(kind=sp),   dimension(np),        intent(inout) :: cdef
           integer(kind=int4),                    intent(in)    :: nxpts
           integer(kind=int4),                    intent(in)    :: np
           ! Locals
           ! Error checking moved to the outside world
           type :: Vtheta2CacheLines_t
              sequence
              type(YMM8r4_t) :: vtheta0
              type(YMM8r4_t) :: vtheta1
              type(YMM8r4_t) :: vtheta2
              type(YMM8r4_t) :: vtheta3
           end type Vtheta2CacheLines_t
           type :: Vphi2CacheLines_t
              sequence
              type(YMM8r4_t) :: vphi0
              type(YMM8r4_t) :: vphi1
              type(YMM8r4_t) :: vphi2
              type(YMM8r4_t) :: vphi3
           end type Vphi2CacheLines_t
           type :: Vthinc2CacheLines_t
              sequence
              type(YMM8r4_t) :: vthinc0
              type(YMM8r4_t) :: vthinc1
              type(YMM8r4_t) :: vthinc2
              type(YMM8r4_t) :: vthinc3
           end type Vthinc2CacheLines_t
           type :: Vphinc2CacheLines
              sequence
              type(YMM8r4_t) :: vphinc0
              type(YMM8r4_t) :: vphinc1
              type(YMM8r4_t) :: vphinc2
              type(YMM8r4_t) :: vphinc3
           end type Vphinc2CacheLines_t
           type :: Termx2CacheLines_t
              sequence
              type(YMM8r4_t) :: term0
              type(YMM8r4_t) :: term1
              type(YMM8r4_t) :: term2
              type(YMM8r4_t) :: term3
           end type Termx2CacheLines_t
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vtheta2CL
           type(Vtheta2CacheLines_t) :: Vtheta2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphi2CL
           type(Vphi2CacheLines_t)  :: Vphi2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vthinc2CL
           type(Vthinc2CacheLines_t) :: Vthinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Vphinc2CL
           type(Vphinc2CacheLines_t) :: Vphinc2CL
           !DIR$ ATTRIBUTES ALIGN : 64 :: Term2CL
           type(Termx2CacheLines_t) :: Term2CL
           !DIR$ ATTRIBUTES ALIGN : 32 :: cn_rand
           type(YMM8r4_t), automatic :: cn_rand
           ! DIR$ ATTRIBUTES ALIGN : 32 :: sphr_rand
           type(YMM8r4_t), automatic :: sphr_rand
           !DIR$ ATTRIBUTES ALIGN : 32  :: cdef_rand
           type(YMM8r4_t), automatic :: cdef_rand
           !DIR$ ATTRIBUTES ALIGN : 32 :: vNPTS
           type(YMM8r4_t), automatic :: vNPTS
           !DIR$ ATTRIBUTES ALIGN : 32 :: vC
           type(YMM8r4_t), automatic :: vC
           !DIR$ ATTRIBUTES ALIGN : 32 tmp1,tmp2
           type(YMM8r4_t), automatic :: tmp1,tmp2
           integer(kind=int4), automatic :: j,i
           !Exec code ....
           cn_rand             = ymm8r4_zero
           sphr_rand           = ymm8r4_zero
           cdef_rand           = ymm8r4_zero
           Vtheta2CL.vtheta0   = ymm8r4_zero
           Vtheta2CL.vtheta1   = ymm8r4_zero
           Vtheta2CL.vtheta2   = ymm8r4_zero
           Vtheta2CL.vtheta3   = ymm8r4_zero
           Vphi2CL.vphi0       = ymm8r4_zero
           Vphi2CL.vphi1       = ymm8r4_zero
           Vphi2CL.vphi2       = ymm8r4_zero
           Vphi2CL.vphi3       = ymm8r4_zero
           Vthinc2CL.vthinc0   = ymm8r4_zero
           Vthinc2CL.vthinc1   = ymm8r4_zero
           Vthinc2CL.vthinc2   = ymm8r4_zero
           Vthinc2CL.vthinc3   = ymm8r4_zero
           Vphinc2CL.vphinc0   = ymm8r4_zero
           Vphinc2CL.vphinc1   = ymm8r4_zero
           Vphinc2CL.vphinc2   = ymm8r4_zero
           Vphinc2CL.vphinc3   = ymm8r4_zero
           vNPTS               = ymm8r4_zero
           tmp1                = ymm8r4_zero
           tmp2                = ymm8r4_zero
           vC                  = ymm8r4_zero
           Term2CL.term0       = ymm8r4_zero
           Term2CL.term1       = ymm8r4_zero
           Term2CL.term2       = ymm8r4_zero
           Term2CL.term3       = ymm8r4_zero
           vNPTS.v   = real(nxpts,kind=sp)
           ! First touch
           paramx = ymm8r4_zero
           do j=1, np
              cn_rand.v   = cn(j)
              sphr_rand.v = radii(j)
              cdef_rand.v = cdef(j)
              vC.v        = ymm8r4_twopi.v*sphr_rand.v
              tmp1.v      = vC.v/vNPTS.v
              tmp2.v      = tmp1.v
              Vthinc2CL.vthinc0.v   = tmp1.v
              Vthinc2CL.vthinc0.v   = Vthinc2CL.vthinc0.v*VINC.v
              Vphinc2CL.vphinc0.v   = tmp2.v
              Vphinc2CL.vphinc0.v   = Vphinc2CL.vphinc0.v*VINC.v
              Vthinc2CL.vthinc1.v   = tmp1.v
              Vthinc2CL.vthinc1.v   = Vthinc2CL.vthinc1.v*VINC2.v
              Vphinc2CL.vphinc1.v   = tmp2.v
              Vphinc2CL.vphinc1.v   = Vphinc2CL.vphinc1.v*VINC2.v
              Vthinc2CL.vthinc2.v   = tmp1.v
              Vthinc2CL.vthinc2.v   = Vthinc2CL.vthinc2.v*VINC3.v
              Vphinc2CL.vphinc2.v   = tmp2.v
              Vphinc2CL.vphinc2.v   = Vphinc2CL.vphinc2.v*VINC3.v
              Vthinc2CL.vthinc3.v   = tmp1.v
              Vthinc2CL.vthinc3.v   = Vthinc2CL.vthinc3.v*VINC4.v
              Vphinc2CL.vphinc3.v   = tmp2.v
              Vphinc2CL.vphinc3.v   = Vphinc2CL.vphinc3.v*VINC4.v
              Vtheta2CL.vtheta0     = ymm8r4_zero
              Vphi2CL.vphi0         = ymm8r4_zero
              Term2CL.term0         = ymm8r4_zero
              Vtheta2CL.vtheta1     = ymm8r4_zero
              Vphi2CL.vphi1         = ymm8r4_zero
              Term2CL.term1         = ymm8r4_zero
              Vtheta2CL.vtheta2     = ymm8r4_zero
              Vphi2CL.vphi2         = ymm8r4_zero
              Term2CL.term2         = ymm8r4_zero
              Vtheta2CL.vtheta3     = ymm8r4_zero
              Vphi2CL.vphi3         = ymm8r4_zero
              Term2CL.term3         = ymm8r4_zero
              !DIR$ VECTOR ALWAYS
              do i=1, nxpts-3, 4
                 Vtheta2CL.vtheta0.v = Vtheta2CL.vtheta0.v+Vthinc2CL.vthinc0.v
                 Vphi2CL.vphi0.v   = Vphi2CL.vphi0.v+Vphinc2CL.vphinc0.v
                 Term2CL.term0.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta0.v))
                 Term2CL.term0.v   = Term2CL.term0.v*sin(Vtheta2CL.vtheta0.v)*cos(Vphi2CL.vphi0.v)
                 paramx(i+0,j).v   = Term2CL.term0.v
                 Vtheta2CL.vtheta1.v = Vtheta2CL.vtheta1.v+Vthinc2CL.vthinc1.v
                 Vphi2CL.vphi1.v   = Vphi2CL.vphi1.v+Vphinc2CL.vphinc1.v
                 Term2CL.term1.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta1.v))
                 Term2CL.term1.v   = Term2CL.term1.v*sin(Vtheta2CL.vtheta1.v)*cos(Vphi2CL.vphi1.v)
                 paramx(i+1,j).v   = Term2CL.term1.v
                 Vtheta2CL.vtheta2.v = Vtheta2CL.vtheta2.v+Vthinc2CL.vthinc2.v
                 Vphi2CL.vphi2.v   = Vphi2CL.vphi2.v+Vphinc2CL.vphinc2.v
                 Term2CL.term2.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta2.v))
                 Term2CL.term2.v   = Term2CL.term2.v*sin(Vtheta2CL.vtheta2.v)*cos(Vphi2CL.vphi2.v)
                 paramx(i+2,j).v   = Term2CL.term2.v
                 Vtheta2CL.vtheta3.v = Vtheta2CL.vtheta3.v+Vthinc2CL.vthinc3.v
                 Vphi2CL.vphi3.v   = Vphi2CL.vphi3.v+Vphinc2CL.vphinc3.v
                 Term2CL.term3.v   = sphr_rand.v*(ymm8r4_one.v+cdef_rand.v*cos(cn_rand.v*Vtheta2CL.vtheta3.v))
                 Term2CL.term3.v   = Term2CL.term3.v*sin(Vtheta2CL.vtheta3.v)*cos(Vphi2CL.vphi3.v)
                 paramx(i+3,j).v   = Term2CL.term3.v
              end do
           end do
     end subroutine ComputeXparam_YMM8r4
     
end module mod_cheb_particles_common
