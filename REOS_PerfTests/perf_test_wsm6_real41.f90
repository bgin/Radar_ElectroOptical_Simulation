

#if 0
 ICC and ifort commands
 icc -c -std=c99 GMS_fast_pmc_access.h GMS_fast_pmc_access.c

 ifort -o perf_test_wsm6d_real41 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 -mavx512f \
    -align array64byte -falign-loops=32 -fno-math-errno -fopenmp -fvec-with-mask -fvec-peel-loops \
    -qopt-assume-safe-padding -qopt-malloc-options=4 -qopt-multiple-gather-scatter-by-shuffles    \
    -qopt-zmm-usage=high -simd -unroll \
    -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_config.fpp GMS_kinds.f90 M_msg.f90 M_journal.f90 GMS_print_error.f90 GMS_pmc_samples.f90 \
     GMS_wsm6_kernel.f90 GMS_fast_pmc_access.f90 GMS_wsm6_driver.f90 GMS_fast_pmc_access.o perf_test_wsm6_real41.f90
    ASM:
    ifort -S perf_test_wsm6d_real41 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 -mavx512f \
    -align array64byte -falign-loops=32 -fno-math-errno -fopenmp -fvec-with-mask -fvec-peel-loops \
    -qopt-assume-safe-padding -qopt-malloc-options=4 -qopt-multiple-gather-scatter-by-shuffles    \
    -qopt-zmm-usage=high -simd -unroll \
    -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_config.fpp GMS_kinds.f90 M_msg.f90 M_journal.f90 GMS_print_error.f90 GMS_pmc_samples.f90 \
     GMS_wsm6_kernel.f90 GMS_fast_pmc_access.f90 GMS_wsm6_driver.f90 GMS_fast_pmc_access.o perf_test_wsm6_real41.f90
#endif


program main 
    use mod_wsm6_driver
    use pmc_samples, only : core_counters1D, &
                            init_core_counters1D
    use mod_kinds, only : i4, sp 

    real(kind=sp), allocatable, dimension(:,:,:)   :: t 
    real(kind=sp), allocatable, dimension(:,:,:,:) :: qci 
    real(kind=sp), allocatable, dimension(:,:,:,:) :: qrs 
    real(kind=sp), allocatable, dimension(:,:,:)   :: q 
    real(kind=sp), allocatable, dimension(:,:,:)   :: den 
    real(kind=sp), allocatable, dimension(:,:,:)   :: p 
    real(kind=sp), allocatable, dimension(:,:,:)   :: delz 
    real(kind=sp), allocatable, dimension(:,:)     :: rain
    real(kind=sp), allocatable, dimension(:,:)     :: rainncv 
    real(kind=sp), allocatable, dimension(:,:)     :: sr 
    real(kind=sp), allocatable, dimension(:,:)     :: snow 
    real(kind=sp), allocatable, dimension(:,:)     :: snowncv 
    real(kind=sp), allocatable, dimension(:,:)     :: graupel 
    real(kind=sp), allocatable, dimension(:,:)     :: graupelncv 
    !dir$ attributes align : 64 :: t 
    !dir$ attributes align : 64 :: qci 
    !dir$ attributes align : 64 :: qrs 
    !dir$ attributes align : 64 :: q 
    !dir$ attributes align : 64 :: den 
    !dir$ attributes align : 64 :: p 
    !dir$ attributes align : 64 :: delz 
    !dir$ attributes align : 64 :: rainz 
    !dir$ attributes align : 64 :: rainncv 
    !dir$ attributes align : 64 :: sr 
    !dir$ attributes align : 64 :: snow 
    !dir$ attributes align : 64 :: snowncv 
    !dir$ attributes align : 64 :: graupel 
    !dir$ attributes align : 64 :: graupelncv
    type(core_counters1D) :: core_counters 
    character(len=60), parameter  :: header = "[PERF-TEST #1: wsm62D, case: [real41] -- START]"
    character(len=60), parameter  :: footer = "[PERF-TEST #1: wsm62D, case: [real41] -- END]"
    integer(kind=i4)      :: ids,ide,jds,jde,kds,kde 
    integer(kind=i4)      :: ims,ime,jms,jme,kms,kme 
    integer(kind=i4)      :: its,ite,jts,jte,kts,kte 
    
    print*, header 
    call wsm6D_driver(ids,ide,jds,jde,kds,kde, &
                     ims,ime,jms,jme,kms,kme, &
                     its,ite,jts,jte,kts,kte, &
                     t,qci,qrs,q,den,p,delz,  &
                     rain,rainncv,sr,snow,    &
                     snowncv,graupel,graupelncv, &
                     core_counters)
                     

    print*, footer 

end program main 