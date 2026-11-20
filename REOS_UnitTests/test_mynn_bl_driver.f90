!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module mod_test_mynn_bl_driver


    use mod_kinds,                 only : i4, i8, sp 
    use iso_c_binding,             only : c_int, c_long_long 
    use mod_mynn_bl_driver 

    implicit none 
    integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
    integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_mynn_bl_driver -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -Wall -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_mynn_bl_driver.f90 test_mynn_bl_driver.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_mynn_bl_driver  -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_mynn_bl_driver.f90 test_mynn_bl_driver.f90

    For assembly only:
    ifort -S  test_avx_cvec4_add -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_mynn_bl_driver.f90 test_mynn_bl_driver.f90
#endif

contains 

function create_default_args() result(args)
    type(mynn_bl_driver_args_holder_t) :: args
    args%initflag           = 1_i4
    args%grav_settling      = 0_i4
    args%bl_mynn_tkebudget  = 1_i4
    args%bl_mynn_cloudpdf   = 2_i4
    args%bl_mixlength       = 0_i4
    args%bl_mynn_edmf       = 1_i4
    args%bl_mynn_tkeadvect  = 0_i4
    args%bl_mynn_edmf_mom   = 1_i4
    args%bl_mynn_edmf_tke   = 1_i4
    args%bl_mynn_edmf_part  = 0_i4
    args%bl_mynn_cloudmix   = 1_i4
    args%bl_mynn_mixqt      = 1_i4
    args%icloud_bl          = 1_i4
    args%FLAG_QI            = 1_i4
    args%FLAG_QNI           = 1_i4
    args%FLAG_QC            = 1_i4
    args%FLAG_QNC           = 1_i4

    ! Domain, Memory, and Tile indices
    args%IDS = 1_i4; args%IDE = 1000_i4
    args%JDS = 1_i4; args%JDE = 1000_i4
    args%KDS = 1_i4; args%KDE = 2000_i4

    args%IMS = 0_i4; args%IME = 1100_i4
    args%JMS = 0_i4; args%JME = 1100_i4
    args%KMS = 1_i4; args%KME = 2100_i4

    args%ITS = 1_i4; args%ITE = 1000_i4
    args%JTS = 1_i4; args%JTE = 1000_i4
    args%KTS = 1_i4; args%KTE = 2000_i4

    args%delt = 10.0_sp
    args%dx   = 3000.0_sp

end function create_default_args 

subroutine unit_test_mynn_bl_driver_create()
      use iso_c_binding, only : c_int 
      use IFPORT
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif

    interface 
        function rdtsc_wrap() bind(C,name="rdtsc_wrap")
            use iso_c_binding, only : c_long_long 
            integer(c_long_long) :: rdtsc_wrap 
        end function rdtsc_wrap 
    end interface
    character(len=128),             automatic     :: filename 
    integer(c_int),                 parameter     :: SIGTRAP = 5 
    character(len=64),              parameter     :: header = "[UNIT-TEST #1: create -- START]"
    character(len=64),              parameter     :: footer = "[UNIT-TEST #1: create -- END]"
    integer(kind=c_long_long),      automatic     :: start,end 
    integer(kind=c_long_long),      automatic     :: start_c,end_c 
    integer(kind=c_long_long),      automatic     :: tsc_elapsed 
    integer(kind=i4),               automatic     :: ier 
    type(mynn_bl_driver_args_holder_t), automatic :: args
    type(mynn_bl_driver_wrapper_t), automatic     :: mw 
    ier = -9999
    print*, header
    print*, "[UNIT-TEST]: [1] -- Initialize Derived Type: mynn_bl_driver_args_holder_t, STARTED."
    start = rdtsc_wrap()
    args  = create_default_args()
    end   = rdtsc_wrap()
    start_c     = start-RDTSC_LATENCY
    end_c       = end-RDTSC_LATENCY
    tsc_elapsed = end_c-start_c 
    if(tsc_elapsed<ZERO) then
        print*,"[INVALID-TSC]=", tsc_elapsed
    else 
        print*, "[WARNING]: Crude timing measurement!!"
        print*,"[TSC]=", tsc_elapsed 
    end if
    print*, "[UNIT-TEST]: [1] -- Initialize Derived Type: mynn_bl_driver_args_holder_t, ENDED."
    print*, "[UNIT-TEST]: [2] -- Initialize Derived Type: mynn_bl_driver_wrapper_t, STARTED."
    start = rdtsc_wrap()
    call construct(mw,args,ier)
    end   = rdtsc_wrap()
    start_c     = start-RDTSC_LATENCY
    end_c       = end-RDTSC_LATENCY
    tsc_elapsed = end_c-start_c 
    if(tsc_elapsed<ZERO) then
        print*,"[INVALID-TSC]=", tsc_elapsed
    else 
        print*, "[WARNING]: Crude timing measurement!!"
        print*,"[TSC]=", tsc_elapsed 
    end if
    if(ier .ne. 0) then 
       if(ier .eq. -9999) print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: execution did not process past prologue!!"
       if(ier .eq. -1)    print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: derived type already created!!"
       if(ier .eq. -2)    print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: i_dim_diff<=0_i4 (size mismatch)!!"
       if(ier .eq. -3)    print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: k_dim_diff<=0_i4 (size mismatch)!!"
       if(ier .eq. -4)    print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: j_dim_diff<=0_i4 (size mismatch)!!"
    else if(ier .eq. 0) then 
       print*, "[UNIT-TEST]: [2] mynn_bl_driver_wrapper_t: execution did reached end!!"
       print*, "ier=", ier 
    end if 
    print*, "[UNIT-TEST]: [2] -- Initialize Derived Type: mynn_bl_driver_wrapper_t, ENDED."
    print*, "[UNIT-TEST]: [3] -- Check Derived Type members: mynn_bl_driver_wrapper_t, STARTED."
    call allocation_status_array3D(mw.m_dz,"m_dz")
    call allocation_status_array3D(mw.m_u,"m_u")
    call allocation_status_array3D(mw.m_v,"m_v")
    call allocation_status_array3D(mw.m_w,"m_w")
    call allocation_status_array3D(mw.m_th,"m_th")
    call allocation_status_array3D(mw.m_qv,"m_qv")
    call allocation_status_array3D(mw.m_p,"m_p")
    call allocation_status_array3D(mw.m_exner,"m_exner")
    call allocation_status_array3D(mw.m_rho,"m_rho")
    call allocation_status_array3D(mw.m_T3D,"m_T3D")
    call allocation_status_array3D(mw.m_qc,"m_qc")
    call allocation_status_array3D(mw.m_qi,"m_qi")
    call allocation_status_array3D(mw.m_qni,"m_qni")
    call allocation_status_array3D(mw.m_qnc,"m_qnc") 
    call allocation_status_array2D(mw.m_xland,"m_xland")
    call allocation_status_array2D(mw.m_ust,"m_ust")
    call allocation_status_array2D(mw.m_ch,"m_ch")
    call allocation_status_array2D(mw.m_rmol,"m_rmol")
    call allocation_status_array2D(mw.m_ts,"m_ts")
    call allocation_status_array2D(mw.m_qsfc,"m_qsfc")
    call allocation_status_array2D(mw.m_qcg,"m_qcg")
    call allocation_status_array2D(mw.m_ps,"m_ps")
    call allocation_status_array2D(mw.m_hfx,"m_hfx")
    call allocation_status_array2D(mw.m_qfx,"m_qfx")
    call allocation_status_array2D(mw.m_wspd,"m_wspd")
    call allocation_status_array2D(mw.m_uoce,"m_uoce")
    call allocation_status_array2D(mw.m_voce,"m_voce")
    call allocation_status_array2D(mw.m_vdfg,"m_vdfg")
    call allocation_status_array2D(mw.m_znt,"m_znt")
    call allocation_status_array3D(mw.m_Qke,"m_Qke")
    call allocation_status_array3D(mw.m_Tsq,"m_Tsq")
    call allocation_status_array3D(mw.m_Qsq,"m_Qsq")
    call allocation_status_array3D(mw.m_Cov,"m_Cov")
    call allocation_status_array3D(mw.m_tke_pbl,"m_tke_pbl")
    call allocation_status_array3D(mw.m_qke_adv,"m_qke_adv")
    call allocation_status_array3D(mw.m_RUBLTEN,"m_RUBLTEN")
    call allocation_status_array3D(mw.m_RVBLTEN,"m_RVBLTEN")
    call allocation_status_array3D(mw.m_RTHBLTEN,"m_RTHBLTEN")
    call allocation_status_array3D(mw.m_RQVBLTEN,"m_RQVBLTEN")
    call allocation_status_array3D(mw.m_RQCBLTEN,"m_RQCBLTEN")
    call allocation_status_array3D(mw.m_RQIBLTEN,"m_RQIBLTEN")
    call allocation_status_array3D(mw.m_RQNIBLTEN,"m_RQNIBLTEN")
    call allocation_status_array3D(mw.m_exch_h,"m_exch_h")
    call allocation_status_array3D(mw.m_exch_m,"m_exch_m")
    call allocation_status_array3D(mw.m_edmf_a,"m_emdf_a")
    call allocation_status_array3D(mw.m_edmf_w,"m_edmf_w")
    call allocation_status_array3D(mw.m_edmf_qt,"m_edmf_qt")
    call allocation_status_array3D(mw.m_edmf_thl,"m_edmf_thl")
    call allocation_status_array3D(mw.m_edmf_ent,"m_emdf_ent")
    call allocation_status_array3D(mw.m_edmf_qc,"m_edmf_qc")
    call allocation_status_array2D(mw.m_Pblh,"m_Pblh")
    call allocation_status_array2D(mw.m_wstar,"m_wstar")
    call allocation_status_array2D(mw.m_KPBL,"m_KPBL")
    call allocation_status_array3D(mw.m_el_pbl,"m_el_pbl")
    call allocation_status_array3D(mw.m_qWT,"m_qWT")
    call allocation_status_array3D(mw.m_qSHEAR,"m_qSHEAR")
    call allocation_status_array3D(mw.m_qBUOY,"m_qBUOY")
    call allocation_status_array3D(mw.m_qDISS,"m_qDISS")
    call allocation_status_array3D(mw.m_dqke,"m_dqke")
    call allocation_status_array3D(mw.m_qc_bl,"m_qc_bl")
    call allocation_status_array3D(mw.m_cldfra_bl,"m_cldfra_bl")
    print*, "[UNIT-TEST]: [3] -- Check Derived Type members: mynn_bl_driver_wrapper_t, STARTED."
    print*, footer 
end subroutine unit_test_mynn_bl_driver_create


end module mod_test_mynn_bl_driver

program main 
    use mod_test_mynn_bl_driver
    call unit_test_mynn_bl_driver_create()
end program main 