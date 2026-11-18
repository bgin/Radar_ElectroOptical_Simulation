
#include "GMS_config.fpp"

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

module mod_mynn_bl_driver 


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_mynn_bl_driver
 !                         
 !          
 !          Purpose:
 !                       Module wrapping the WRF mynn_bl driver subroutine 
 !                        
 !          History:
 !                        Date: 16-11-2026
 !                        Time: 05:52AM GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !                 
 !          References:
 !         
 !                     WRF mynn_bl_driver subroutine
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i1,i4,i8,sp
    use omp_lib
    implicit none
    public

     ! Major version
    integer(kind=i4),   parameter :: MYNN_BL_DRIVER_MAJOR = 1
     ! Minor version
    integer(kind=i4),   parameter :: MYNN_BL_DRIVER_MINOR = 0
     ! Micro version
    integer(kind=i4),   parameter :: MYNN_BL_DRIVER_MICRO = 0
     ! Full version
    integer(kind=i4),   parameter :: MYNN_BL_DRIVER_FULLVER =   &
            1000*MYNN_BL_DRIVER_MAJOR+100*MYNN_BL_DRIVER_MINOR+10*MYNN_BL_DRIVER_MICRO
     ! Module creation date
    character(*),        parameter :: MYNN_BL_DRIVER_CREATE_DATE = "16-11-2026 05:52AM +00200 (MON 16 NOV 2026 GMT+2)"
     ! Module build date
    character(*),        parameter :: MYNN_BL_DRIVER_BUILD_DATE  = __DATE__ 

    character(*),        parameter :: MYNN_BL_DRIVER_BUILD_TIME  =  __TIME__
     ! Module author info
    character(*),        parameter :: MYNN_BL_DRIVER_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
    character(*),        parameter :: MYNN_BL_DRIVER_SYNOPSIS    = "Module wrapping the call to standalone mynn_bl_driver (of the WRF project)."

    character(*),        parameter :: MYNN_BL_DRIVER_REFERENCE   = "https://github.com/wrf-model/wrf"

#if !defined(MYNN_BL_DRIVER_EXEC_TRACE)
#define MYNN_BL_DRIVER_EXEC_TRACE 1
#endif

#if !defined(MYNN_BL_DRIVER_USE_EXPLICIT_DEALLOCATION)
#define MYNN_BL_DRIVER_USE_EXPLICIT_DEALLOCATION 1 
#endif 

#if !defined(MYNN_BL_DRIVER_WRF_CHEM)
#define MYNN_BL_DRIVER_WRF_CHEM 0
#endif 

#define MYNN_BL_DRIVER_TRACE_START \
print*, "[EXECUTION-TRACE: START]";\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;\
print*, "TSC-Start:", rdtsc_wrap()

#define MYNN_BL_DRIVER_TRACE_END \
print*, "TSC-End:", rdtsc_wrap();\
print*, "[EXECUTION-TRACE: END]" ;\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;

type, public :: mynn_bl_driver_args_holder_t 
    SEQUENCE
    integer(kind=i4) :: initflag
    integer(kind=i4) :: grav_settling
    integer(kind=i4) :: bl_mynn_tkebudget
    integer(kind=i4) :: bl_mynn_cloudpdf
    integer(kind=i4) :: bl_mixlength
    integer(kind=i4) :: bl_mynn_edmf
    integer(kind=i4) :: bl_mynn_tkeadvect
    integer(kind=i4) :: bl_mynn_edmf_mom
	integer(kind=i4) :: bl_mynn_edmf_tke
    integer(kind=i4) :: bl_mynn_edmf_part
	integer(kind=i4) :: bl_mynn_cloudmix
	integer(kind=i4) :: bl_mynn_mixqt
	integer(kind=i4) :: icloud_bl
	integer(kind=i4) :: FLAG_QI
	integer(kind=i4) :: FLAG_QNI
	integer(kind=i4) :: FLAG_QC
	integer(kind=i4) :: FLAG_QNC
	integer(kind=i4) :: IDS
	integer(kind=i4) :: IDE
	integer(kind=i4) :: JDS
	integer(kind=i4) :: JDE
	integer(kind=i4) :: KDS
	integer(kind=i4) :: KDE
	integer(kind=i4) :: IMS
	integer(kind=i4) :: IME
	integer(kind=i4) :: JMS
    integer(kind=i4) :: JME
	integer(kind=i4) :: KMS
	integer(kind=i4) :: KME
	integer(kind=i4) :: ITS
	integer(kind=i4) :: ITE
	integer(kind=i4) :: JTS
	integer(kind=i4) :: JTE
	integer(kind=i4) :: KTS
	integer(kind=i4) :: KTE 
    real(kind=sp)    :: delt 
    real(kind=sp)    :: dx
end type mynn_bl_driver_args_holder_t

type, public :: mynn_bl_driver_wrapper_t

    integer(kind=i4) :: m_initflag
    integer(kind=i4) :: m_grav_settling
    integer(kind=i4) :: m_bl_mynn_tkebudget
    integer(kind=i4) :: m_bl_mynn_cloudpdf
    integer(kind=i4) :: m_bl_mixlength
    integer(kind=i4) :: m_bl_mynn_edmf
    integer(kind=i4) :: m_bl_mynn_tkeadvect
    integer(kind=i4) :: m_bl_mynn_edmf_mom
	integer(kind=i4) :: m_bl_mynn_edmf_tke
    integer(kind=i4) :: m_bl_mynn_edmf_part
	integer(kind=i4) :: m_bl_mynn_cloudmix
	integer(kind=i4) :: m_bl_mynn_mixqt
	integer(kind=i4) :: m_icloud_bl
	integer(kind=i4) :: m_FLAG_QI
	integer(kind=i4) :: m_FLAG_QNI
	integer(kind=i4) :: m_FLAG_QC
	integer(kind=i4) :: m_FLAG_QNC
	integer(kind=i4) :: m_IDS
	integer(kind=i4) :: m_IDE
	integer(kind=i4) :: m_JDS
	integer(kind=i4) :: m_JDE
	integer(kind=i4) :: m_KDS
	integer(kind=i4) :: m_KDE
	integer(kind=i4) :: m_IMS
	integer(kind=i4) :: m_IME
	integer(kind=i4) :: m_JMS
    integer(kind=i4) :: m_JME
	integer(kind=i4) :: m_KMS
	integer(kind=i4) :: m_KME
	integer(kind=i4) :: m_ITS
	integer(kind=i4) :: m_ITE
	integer(kind=i4) :: m_JTS
	integer(kind=i4) :: m_JTE
	integer(kind=i4) :: m_KTS
	integer(kind=i4) :: m_KTE
    real(kind=sp)    :: m_delt 
    real(kind=sp)    :: m_dx
    logical(kind=i4) :: m_is_derived_type_built
    real(kind=sp), dimension(:,:,:), allocatable :: m_dz 
    real(kind=sp), dimension(:,:,:), allocatable :: m_u 
    real(kind=sp), dimension(:,:,:), allocatable :: m_v 
    real(kind=sp), dimension(:,:,:), allocatable :: m_w 
    real(kind=sp), dimension(:,:,:), allocatable :: m_th 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qv 
    real(kind=sp), dimension(:,:,:), allocatable :: m_p 
    real(kind=sp), dimension(:,:,:), allocatable :: m_exner 
    real(kind=sp), dimension(:,:,:), allocatable :: m_rho 
    real(kind=sp), dimension(:,:,:), allocatable :: m_T3D
    real(kind=sp), dimension(:,:,:), allocatable :: m_qc
    real(kind=sp), dimension(:,:,:), allocatable :: m_qi 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qni 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qnc 
    real(kind=sp), dimension(:,:),   allocatable :: m_xland 
    real(kind=sp), dimension(:,:),   allocatable :: m_ust 
    real(kind=sp), dimension(:,:),   allocatable :: m_ch 
    real(kind=sp), dimension(:,:),   allocatable :: m_rmol 
    real(kind=sp), dimension(:,:),   allocatable :: m_ts 
    real(kind=sp), dimension(:,:),   allocatable :: m_qsfc 
    real(kind=sp), dimension(:,:),   allocatable :: m_qcg 
    real(kind=sp), dimension(:,:),   allocatable :: m_ps 
    real(kind=sp), dimension(:,:),   allocatable :: m_hfx 
    real(kind=sp), dimension(:,:),   allocatable :: m_qfx 
    real(kind=sp), dimension(:,:),   allocatable :: m_wspd 
    real(kind=sp), dimension(:,:),   allocatable :: m_uoce 
    real(kind=sp), dimension(:,:),   allocatable :: m_voce 
    real(kind=sp), dimension(:,:),   allocatable :: m_vdfg 
    real(kind=sp), dimension(:,:),   allocatable :: m_znt 
    real(kind=sp), dimension(:,:,:), allocatable :: m_Qke 
    real(kind=sp), dimension(:,:,:), allocatable :: m_Tsq 
    real(kind=sp), dimension(:,:,:), allocatable :: m_Qsq 
    real(kind=sp), dimension(:,:,:), allocatable :: m_Cov 
    real(kind=sp), dimension(:,:,:), allocatable :: m_tke_pbl 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qke_adv 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RUBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RVBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RTHBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RQVBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RQCBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RQIBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_RQNIBLTEN 
    real(kind=sp), dimension(:,:,:), allocatable :: m_exch_h 
    real(kind=sp), dimension(:,:,:), allocatable :: m_exch_m 
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_a  
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_w 
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_qt 
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_thl 
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_ent 
    real(kind=sp), dimension(:,:,:), allocatable :: m_edmf_qc 
    real(kind=sp), dimension(:,:),   allocatable :: m_Pblh 
    real(kind=sp), dimension(:,:),   allocatable :: m_wstar 
    real(kind=sp), dimension(:,:),   allocatable :: m_delta 
    real(kind=sp), dimension(:,:),   allocatable :: m_KPBL 
    real(kind=sp), dimension(:,:,:), allocatable :: m_el_pbl 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qWT 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qSHEAR 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qBUOY 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qDISS 
    real(kind=sp), dimension(:,:,:), allocatable :: m_dqke 
    real(kind=sp), dimension(:,:,:), allocatable :: m_qc_bl 
    real(kind=sp), dimension(:,:,:), allocatable :: m_cldfra_bl 
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !dir$ attributes align : 64 :: m_dz 
    !dir$ attributes align : 64 :: m_u 
    !dir$ attributes align : 64 :: m_v 
    !dir$ attributes align : 64 :: m_w 
    !dir$ attributes align : 64 :: m_th 
    !dir$ attributes align : 64 :: m_qv 
    !dir$ attributes align : 64 :: m_p 
    !dir$ attributes align : 64 :: m_exner 
    !dir$ attributes align : 64 :: m_rho 
    !dir$ attributes align : 64 :: m_T3D 
    !dir$ attributes align : 64 :: m_qc 
    !dir$ attributes align : 64 :: m_qi 
    !dir$ attributes align : 64 :: m_qni
    !dir$ attributes align : 64 :: m_qnc 
    !dir$ attributes align : 64 :: m_xland
    !dir$ attributes align : 64 :: m_ust
    !dir$ attributes align : 64 :: m_ch
    !dir$ attributes align : 64 :: m_rmol
    !dir$ attributes align : 64 :: m_ts
    !dir$ attributes align : 64 :: m_qsfc
    !dir$ attributes align : 64 :: m_qcg
    !dir$ attributes align : 64 :: m_ps
    !dir$ attributes align : 64 :: m_hfx
    !dir$ attributes align : 64 :: m_qfx
    !dir$ attributes align : 64 :: m_wspd
    !dir$ attributes align : 64 :: m_uoce
    !dir$ attributes align : 64 :: m_voce
    !dir$ attributes align : 64 :: m_vdfg
    !dir$ attributes align : 64 :: m_znt
    !dir$ attributes align : 64 :: m_Qke
    !dir$ attributes align : 64 :: m_Tsq
    !dir$ attributes align : 64 :: m_Qsq
    !dir$ attributes align : 64 :: m_Cov
    !dir$ attributes align : 64 :: m_tke_pbl
    !dir$ attributes align : 64 :: m_qke_adv
    !dir$ attributes align : 64 :: m_RUBLTEN 
    !dir$ attributes align : 64 :: m_RVBLTEN
    !dir$ attributes align : 64 :: m_RTHBLTEN
    !dir$ attributes align : 64 :: m_RQVBLTEN 
    !dir$ attributes align : 64 :: m_RQCBLTEN 
    !dir$ attributes align : 64 :: m_RQIBLTEN
    !dir$ attributes align : 64 :: m_RQNIBLTEN 
    !dir$ attributes align : 64 :: m_exch_h
    !dir$ attributes align : 64 :: m_exch_m
    !dir$ attributes align : 64 :: m_edmf_a
    !dir$ attributes align : 64 :: m_edmf_w
    !dir$ attributes align : 64 :: m_edmf_qt
    !dir$ attributes align : 64 :: m_edmf_thl
    !dir$ attributes align : 64 :: m_edmf_ent
    !dir$ attributes align : 64 :: m_edmf_qc 
    !dir$ attributes align : 64 :: m_Pblh
    !dir$ attributes align : 64 :: m_wstar
    !dir$ attributes align : 64 :: m_delta
    !dir$ attributes align : 64 :: m_KPBL
    !dir$ attributes align : 64 :: m_el_pbl
    !dir$ attributes align : 64 :: m_qWT
    !dir$ attributes align : 64 :: m_qSHEAR
    !dir$ attributes align : 64 :: m_qBUOY
    !dir$ attributes align : 64 :: m_qDISS
    !dir$ attributes align : 64 :: m_dqke
    !dir$ attributes align : 64 :: m_qc_bl
    !dir$ attributes align : 64 :: m_cldfra_bl
#endif     
end type mynn_bl_driver_wrapper_t


contains 

subroutine construct(mw,args_holder,ier)
    implicit none   
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
            interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
            end interface
#endif           
                            
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "construct"
#endif 
    type(mynn_bl_driver_wrapper_t),     intent(out)   :: mw
    type(mynn_bl_driver_args_holder_t), intent(in)    :: args_holder
    integer(kind=i4),                   intent(inout) :: ier 
    ! locals
    integer(kind=i4), automatic :: i_dim_diff
    integer(kind=i4), automatic :: k_dim_diff 
    integer(kind=i4), automatic :: j_dim_diff
    if(ier .le. 0_i4) then 
       ier = -9999
    end if 
    if(mw.m_is_derived_type_built == .true.) then 
       ier = -1
       return 
    end if 
    i_dim_diff = args_holder.ime-args_holder.ims 
    k_dim_diff = args_holder.kme-args_holder.kms
    j_dim_diff = args_holder.jme-args_holder.jms 
    if(i_dim_diff<=0_i4) then 
       ier = -2
       return 
    end if 
    if(k_dim_diff<=0_i4) then 
       ier = -3 
       return 
    end if 
    if(j_dim_diff<=0_i4) then 
       ier = -4
       return 
    end if 
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_START
#endif 
    mw.m_initflag          = args_holder.initflag
    mw.m_grav_settling     = args_holder.grav_settling
    mw.m_bl_mynn_tkebudget = args_holder.bl_mynn_tkebudget
    mw.m_bl_mynn_cloudpdf  = args_holder.bl_mynn_cloudpdf
    mw.m_bl_mixlength      = args_holder.bl_mixlength
	mw.m_bl_mynn_edmf      = args_holder.bl_mynn_edmf
	mw.m_bl_mynn_tkeadvect = args_holder.bl_mynn_tkeadvect
	mw.m_bl_mynn_edmf_mom  = args_holder.bl_mynn_edmf_mom
	mw.m_bl_mynn_edmf_tke  = args_holder.bl_mynn_edmf_tke 
    mw.m_bl_mynn_edmf_part = args_holder.bl_mynn_edmf_part 
	mw.m_bl_mynn_cloudmix  = args_holder.bl_mynn_cloudmix
	mw.m_bl_mynn_mixqt     = args_holder.bl_mynn_mixqt
	mw.m_icloud_bl         = args_holder.icloud_bl
	mw.m_FLAG_QI           = args_holder.FLAG_QI
	mw.m_FLAG_QNI          = args_holder.FLAG_QNI
	mw.m_FLAG_QC           = args_holder.FLAG_QC
	mw.m_FLAG_QNC          = args_holder.FLAG_QNC
    mw.m_IDS               = args_holder.IDS 
	mw.m_IDE               = args_holder.IDE
	mw.m_JDS               = args_holder.JDS
	mw.m_JDE               = args_holder.JDE 
	mw.m_KDS               = args_holder.KDS 
	mw.m_KDE               = args_holder.KDE 
	mw.m_IMS               = args_holder.IMS 
	mw.m_IME               = args_holder.IME 
    mw.m_JMS               = args_holder.JMS
	mw.m_JME               = args_holder.JME
	mw.m_KMS               = args_holder.KMS
	mw.m_KME               = args_holder.KME
	mw.m_ITS               = args_holder.ITS
    mw.m_ITE               = args_holder.ITE
	mw.m_JTS               = args_holder.JTS
	mw.m_JTE               = args_holder.JTE
	mw.m_KTS               = args_holder.KTS
	mw.m_KTE               = args_holder.KTE 
    mw.m_delt              = args_holder.delt 
    mw.m_dx                = args_holder.dx 
    ! Allocation
    if(allocated(mw.m_dz)) deallocate(mw.m_dz); allocate(mw.m_dz(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_u)) deallocate(mw.m_u);   allocate(mw.m_u(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_v)) deallocate(mw.m_v);   allocate(mw.m_v(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_w)) deallocate(mw.m_w);   allocate(mw.m_w(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_th)) deallocate(mw.m_th); allocate(mw.m_th(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qv)) deallocate(mw.m_qv); allocate(mw.m_qv(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_p)) deallocate(mw.m_p);   allocate(mw.m_p(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_exner)) deallocate(mw.m_exner); allocate(mw.m_exner(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_rho)) deallocate(mw.m_rho); allocate(mw.m_rho(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_T3D)) deallocate(mw.m_T3D); allocate(mw.m_T3D(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qc)) deallocate(mw.m_qc); allocate(mw.m_qc(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qi)) deallocate(mw.m_qi); allocate(mw.m_qi(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qni)) deallocate(mw.m_qni); allocate(mw.m_qni(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qnc)) deallocate(mw.m_qnc); allocate(mw.m_qnc(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_xland)) deallocate(mw.m_xland); allocate(mw.m_xland(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_ust)) deallocate(mw.m_ust); allocate(mw.m_ust(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_ch)) deallocate(mw.m_ch); allocate(mw.m_ch(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_rmol)) deallocate(mw.m_rmol); allocate(mw.m_rmol(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_ts)) deallocate(mw.m_ts); allocate(mw.m_ts(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qsfc)) deallocate(mw.m_qsfc); allocate(mw.m_qsfc(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qcg)) deallocate(mw.m_qcg); allocate(mw.m_qcg(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_ps)) deallocate(mw.m_ps); allocate(mw.m_ps(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_hfx)) deallocate(mw.m_hfx); allocate(mw.m_hfx(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qfx)) deallocate(mw.m_qfx); allocate(mw.m_qfx(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_wspd)) deallocate(mw.m_wspd); allocate(mw.m_wspd(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_uoce)) deallocate(mw.m_uoce); allocate(mw.m_uoce(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_voce)) deallocate(mw.m_voce); allocate(mw.m_voce(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_vdfg)) deallocate(mw.m_vdfg); allocate(mw.m_vdfg(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_znt)) deallocate(mw.m_znt); allocate(mw.m_znt(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_Qke)) deallocate(mw.m_Qke); allocate(mw.m_Qke(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_Tsq)) deallocate(mw.m_Tsq); allocate(mw.m_Tsq(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_Qsq)) deallocate(mw.m_Qsq); allocate(mw.m_Qsq(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_Cov)) deallocate(mw.m_Cov); allocate(mw.m_Cov(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_tke_pbl)) deallocate(mw.m_tke_pbl); allocate(mw.m_tke_pbl(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qke_adv)) deallocate(mw.m_qke_adv); allocate(mw.m_qke_adv(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RUBLTEN)) deallocate(mw.m_RUBLTEN); allocate(mw.m_RUBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RVBLTEN)) deallocate(mw.m_RVBLTEN); allocate(mw.m_RVBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RTHBLTEN)) deallocate(mw.m_RTHBLTEN); allocate(mw.m_RTHBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RQVBLTEN)) deallocate(mw.m_RQVBLTEN); allocate(mw.m_RQVBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RQCBLTEN)) deallocate(mw.m_RQCBLTEN); allocate(mw.m_RQCBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RQIBLTEN)) deallocate(mw.m_RQIBLTEN); allocate(mw.m_RQIBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_RQNIBLTEN)) deallocate(mw.m_RQNIBLTEN); allocate(mw.m_RQNIBLTEN(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_exch_h)) deallocate(mw.m_exch_h); allocate(mw.m_exch_h(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_exch_m)) deallocate(mw.m_exch_m); allocate(mw.m_exch_m(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_a)) deallocate(mw.m_edmf_a); allocate(mw.m_edmf_a(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_w)) deallocate(mw.m_edmf_w); allocate(mw.m_edmf_w(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_qt)) deallocate(mw.m_edmf_qt); allocate(mw.m_edmf_qt(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_thl)) deallocate(mw.m_edmf_thl); allocate(mw.m_edmf_thl(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_ent)) deallocate(mw.m_edmf_ent); allocate(mw.m_edmf_ent(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_edmf_qc)) deallocate(mw.m_edmf_qc); allocate(mw.m_edmf_qc(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_Pblh)) deallocate(mw.m_Pblh); allocate(mw.m_Pblh(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_wstar)) deallocate(mw.m_wstar); allocate(mw.m_wstar(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_delta)) deallocate(mw.m_delta); allocate(mw.m_delta(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_KPBL)) deallocate(mw.m_KPBL); allocate(mw.m_KPBL(mw.m_IMS:mw.m_IME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_el_pbl)) deallocate(mw.m_el_pbl); allocate(mw.m_el_pbl(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qWT)) deallocate(mw.m_qWT); allocate(mw.m_qWT(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qSHEAR)) deallocate(mw.m_qSHEAR); allocate(mw.m_qSHEAR(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qBUOY)) deallocate(mw.m_qBUOY); allocate(mw.m_qBUOY(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qDISS)) deallocate(mw.m_qDISS); allocate(mw.m_qDISS(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_dqke)) deallocate(mw.m_dqke); allocate(mw.m_dqke(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_qc_bl)) deallocate(mw.m_qc_bl); allocate(mw.m_qc_bl(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    if(allocated(mw.m_cldfra_bl)) deallocate(mw.m_cldfra_bl); allocate(mw.m_cldfra_bl(mw.m_IMS:mw.m_IME,mw.m_KMS:mw.m_KME,mw.m_JMS:mw.m_JME))
    mw.m_is_derived_type_built = .true.
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_END
#endif 
    ier = 0
end subroutine construct 

subroutine destroy(mw,ier)
     implicit none   
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
            interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
            end interface
#endif           
                            
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "destroy"
#endif 
    type(mynn_bl_driver_wrapper_t),     intent(inout)   :: mw
    integer(kind=i4),                   intent(inout)   :: ier 
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_START
#endif
    ier = -9999
    if(mw.m_is_derived_type_built == .false.) then 
        ier = -1 
        return
    end if  
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_START
#endif
    if(allocated(mw.m_dz)) deallocate(mw.m_dz)
    if(allocated(mw.m_u)) deallocate(mw.m_u)
    if(allocated(mw.m_v)) deallocate(mw.m_v)
    if(allocated(mw.m_w)) deallocate(mw.m_w)
    if(allocated(mw.m_th)) deallocate(mw.m_th)
    if(allocated(mw.m_qv)) deallocate(mw.m_qv)
    if(allocated(mw.m_p)) deallocate(mw.m_p)
    if(allocated(mw.m_exner)) deallocate(mw.m_exner)
    if(allocated(mw.m_rho)) deallocate(mw.m_rho)
    if(allocated(mw.m_T3D)) deallocate(mw.m_T3D)
    if(allocated(mw.m_qc)) deallocate(mw.m_qc)
    if(allocated(mw.m_qi)) deallocate(mw.m_qi)
    if(allocated(mw.m_qni)) deallocate(mw.m_qni)
    if(allocated(mw.m_qnc)) deallocate(mw.m_qnc)
    if(allocated(mw.m_xland)) deallocate(mw.m_xland)
    if(allocated(mw.m_ust)) deallocate(mw.m_ust)
    if(allocated(mw.m_ch)) deallocate(mw.m_ch)
    if(allocated(mw.m_rmol)) deallocate(mw.m_rmol)
    if(allocated(mw.m_ts)) deallocate(mw.m_ts)
    if(allocated(mw.m_qsfc)) deallocate(mw.m_qsfc)
    if(allocated(mw.m_qcg)) deallocate(mw.m_qcg)
    if(allocated(mw.m_ps)) deallocate(mw.m_ps)
    if(allocated(mw.m_hfx)) deallocate(mw.m_hfx)
    if(allocated(mw.m_qfx)) deallocate(mw.m_qfx)
    if(allocated(mw.m_wspd)) deallocate(mw.m_wspd)
    if(allocated(mw.m_uoce)) deallocate(mw.m_uoce)
    if(allocated(mw.m_voce)) deallocate(mw.m_voce)
    if(allocated(mw.m_vdfg)) deallocate(mw.m_vdfg)
    if(allocated(mw.m_znt)) deallocate(mw.m_znt)
    if(allocated(mw.m_Qke)) deallocate(mw.m_Qke)
    if(allocated(mw.m_Tsq)) deallocate(mw.m_Tsq)
    if(allocated(mw.m_Qsq)) deallocate(mw.m_Qsq)
    if(allocated(mw.m_Cov)) deallocate(mw.m_Cov)
    if(allocated(mw.m_tke_pbl)) deallocate(mw.m_tke_pbl)
    if(allocated(mw.m_qke_adv)) deallocate(mw.m_qke_adv)
    if(allocated(mw.m_RUBLTEN)) deallocate(mw.m_RUBLTEN)
    if(allocated(mw.m_RVBLTEN)) deallocate(mw.m_RVBLTEN)
    if(allocated(mw.m_RTHBLTEN)) deallocate(mw.m_RTHBLTEN)
    if(allocated(mw.m_RQVBLTEN)) deallocate(mw.m_RQVBLTEN)
    if(allocated(mw.m_RQCBLTEN)) deallocate(mw.m_RQCBLTEN)
    if(allocated(mw.m_RQIBLTEN)) deallocate(mw.m_RQIBLTEN)
    if(allocated(mw.m_RQNIBLTEN)) deallocate(mw.m_RQNIBLTEN)
    if(allocated(mw.m_exch_h)) deallocate(mw.m_exch_h)
    if(allocated(mw.m_exch_m)) deallocate(mw.m_exch_m)
    if(allocated(mw.m_edmf_a)) deallocate(mw.m_edmf_a)
    if(allocated(mw.m_edmf_w)) deallocate(mw.m_edmf_w)
    if(allocated(mw.m_edmf_qt)) deallocate(mw.m_edmf_qt)
    if(allocated(mw.m_edmf_thl)) deallocate(mw.m_edmf_thl)
    if(allocated(mw.m_edmf_ent)) deallocate(mw.m_edmf_ent)
    if(allocated(mw.m_edmf_qc)) deallocate(mw.m_edmf_qc)
    if(allocated(mw.m_Pblh)) deallocate(mw.m_Pblh)
    if(allocated(mw.m_wstar)) deallocate(mw.m_wstar)
    if(allocated(mw.m_delta)) deallocate(mw.m_delta)
    if(allocated(mw.m_KPBL)) deallocate(mw.m_KPBL)
    if(allocated(mw.m_el_pbl)) deallocate(mw.m_el_pbl)
    if(allocated(mw.m_qWT)) deallocate(mw.m_qWT)
    if(allocated(mw.m_qSHEAR)) deallocate(mw.m_qSHEAR)
    if(allocated(mw.m_qBUOY)) deallocate(mw.m_qBUOY)
    if(allocated(mw.m_qDISS)) deallocate(mw.m_qDISS)
    if(allocated(mw.m_dqke)) deallocate(mw.m_dqke)
    if(allocated(mw.m_qc_bl)) deallocate(mw.m_qc_bl)
    if(allocated(mw.m_cldfra_bl)) deallocate(mw.m_cldfra_bl)
    mw.m_is_derived_type_built = .false.
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_END
#endif
    ier = 0
end subroutine destroy 

subroutine copy_create(this,other,ier)
    implicit none   
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
            interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
            end interface
#endif           
                            
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "copy_create"
#endif 
    type(mynn_bl_driver_wrapper_t), intent(inout) :: this 
    type(mynn_bl_driver_wrapper_t), intent(in)    :: other 
    integer(kind=i4),               intent(inout) :: ier 
    ! locals
    integer(kind=i4), automatic :: i,j,k 
    ! Exec code 
    if(this.m_is_derived_type_built .eq. .true.)  then 
       ier = -1
       return
    end if  
    if(other.m_is_derived_type_built .eq. .false.) then 
       ier = -2
       return
    end if  
    if(LOC(this) .eq. LOC(other)) then               
       ier = -3 
       return
    end if  
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_START
#endif
    this.m_initflag          = other.m_initflag
    this.m_grav_settling     = other.m_grav_settling
    this.m_bl_mynn_tkebudget = other.m_bl_mynn_tkebudget
    this.m_bl_mynn_cloudpdf  = other.m_bl_mynn_cloudpdf
    this.m_bl_mixlength      = other.m_bl_mixlength
	this.m_bl_mynn_edmf      = other.m_bl_mynn_edmf
	this.m_bl_mynn_tkeadvect = other.m_bl_mynn_tkeadvect
	this.m_bl_mynn_edmf_mom  = other.m_bl_mynn_edmf_mom
	this.m_bl_mynn_edmf_tke  = other.m_bl_mynn_edmf_tke 
    this.m_bl_mynn_edmf_part = other.m_bl_mynn_edmf_part 
	this.m_bl_mynn_cloudmix  = other.m_bl_mynn_cloudmix
	this.m_bl_mynn_mixqt     = other.m_bl_mynn_mixqt
	this.m_icloud_bl         = other.m_icloud_bl
	this.m_FLAG_QI           = other.m_FLAG_QI
	this.m_FLAG_QNI          = other.m_FLAG_QNI
	this.m_FLAG_QC           = other.m_FLAG_QC
	this.m_FLAG_QNC          = other.m_FLAG_QNC
    this.m_IDS               = other.m_IDS 
	this.m_IDE               = other.m_IDE
	this.m_JDS               = other.m_JDS
	this.m_JDE               = other.m_JDE 
	this.m_KDS               = other.m_KDS 
	this.m_KDE               = other.m_KDE 
	this.m_IMS               = other.m_IMS 
	this.m_IME               = other.m_IME 
    this.m_JMS               = other.m_JMS
	this.m_JME               = other.m_JME
	this.m_KMS               = other.m_KMS
	this.m_KME               = other.m_KME
	this.m_ITS               = other.m_ITS
    this.m_ITE               = other.m_ITE
	this.m_JTS               = other.m_JTS
	this.m_JTE               = other.m_JTE
	this.m_KTS               = other.m_KTS
	this.m_KTE               = other.m_KTE 
    this.m_delt              = other.m_delt 
    this.m_dx                = other.m_dx 
     ! Allocation
    if(allocated(this.m_dz)) deallocate(this.m_dz); allocate(this.m_dz(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_u)) deallocate(this.m_u);   allocate(this.m_u(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_v)) deallocate(this.m_v);   allocate(this.m_v(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_w)) deallocate(this.m_w);   allocate(this.m_w(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_th)) deallocate(this.m_th); allocate(this.m_th(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qv)) deallocate(this.m_qv); allocate(this.m_qv(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_p)) deallocate(this.m_p);   allocate(this.m_p(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_exner)) deallocate(this.m_exner); allocate(this.m_exner(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_rho)) deallocate(this.m_rho); allocate(this.m_rho(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_T3D)) deallocate(this.m_T3D); allocate(this.m_T3D(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qc)) deallocate(this.m_qc); allocate(this.m_qc(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qi)) deallocate(this.m_qi); allocate(this.m_qi(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qni)) deallocate(this.m_qni); allocate(this.m_qni(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qnc)) deallocate(this.m_qnc); allocate(this.m_qnc(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_xland)) deallocate(this.m_xland); allocate(this.m_xland(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_ust)) deallocate(this.m_ust); allocate(this.m_ust(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_ch)) deallocate(this.m_ch); allocate(this.m_ch(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_rmol)) deallocate(this.m_rmol); allocate(this.m_rmol(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_ts)) deallocate(this.m_ts); allocate(this.m_ts(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qsfc)) deallocate(this.m_qsfc); allocate(this.m_qsfc(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qcg)) deallocate(this.m_qcg); allocate(this.m_qcg(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_ps)) deallocate(this.m_ps); allocate(this.m_ps(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_hfx)) deallocate(this.m_hfx); allocate(this.m_hfx(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qfx)) deallocate(this.m_qfx); allocate(this.m_qfx(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_wspd)) deallocate(this.m_wspd); allocate(this.m_wspd(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_uoce)) deallocate(this.m_uoce); allocate(this.m_uoce(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_voce)) deallocate(this.m_voce); allocate(this.m_voce(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_vdfg)) deallocate(this.m_vdfg); allocate(this.m_vdfg(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_znt)) deallocate(this.m_znt); allocate(this.m_znt(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_Qke)) deallocate(this.m_Qke); allocate(this.m_Qke(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_Tsq)) deallocate(this.m_Tsq); allocate(this.m_Tsq(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_Qsq)) deallocate(this.m_Qsq); allocate(this.m_Qsq(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_Cov)) deallocate(this.m_Cov); allocate(this.m_Cov(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_tke_pbl)) deallocate(this.m_tke_pbl); allocate(this.m_tke_pbl(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qke_adv)) deallocate(this.m_qke_adv); allocate(this.m_qke_adv(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RUBLTEN)) deallocate(this.m_RUBLTEN); allocate(this.m_RUBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RVBLTEN)) deallocate(this.m_RVBLTEN); allocate(this.m_RVBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RTHBLTEN)) deallocate(this.m_RTHBLTEN); allocate(this.m_RTHBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RQVBLTEN)) deallocate(this.m_RQVBLTEN); allocate(this.m_RQVBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RQCBLTEN)) deallocate(this.m_RQCBLTEN); allocate(this.m_RQCBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RQIBLTEN)) deallocate(this.m_RQIBLTEN); allocate(this.m_RQIBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_RQNIBLTEN)) deallocate(this.m_RQNIBLTEN); allocate(this.m_RQNIBLTEN(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_exch_h)) deallocate(this.m_exch_h); allocate(this.m_exch_h(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_exch_m)) deallocate(this.m_exch_m); allocate(this.m_exch_m(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_a)) deallocate(this.m_edmf_a); allocate(this.m_edmf_a(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_w)) deallocate(this.m_edmf_w); allocate(this.m_edmf_w(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_qt)) deallocate(this.m_edmf_qt); allocate(this.m_edmf_qt(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_thl)) deallocate(this.m_edmf_thl); allocate(this.m_edmf_thl(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_ent)) deallocate(this.m_edmf_ent); allocate(this.m_edmf_ent(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_edmf_qc)) deallocate(this.m_edmf_qc); allocate(this.m_edmf_qc(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_Pblh)) deallocate(this.m_Pblh); allocate(this.m_Pblh(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_wstar)) deallocate(this.m_wstar); allocate(this.m_wstar(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_delta)) deallocate(this.m_delta); allocate(this.m_delta(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_KPBL)) deallocate(this.m_KPBL); allocate(this.m_KPBL(this.m_IMS:this.m_IME,this.m_JMS:this.m_JME))
    if(allocated(this.m_el_pbl)) deallocate(this.m_el_pbl); allocate(this.m_el_pbl(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qWT)) deallocate(this.m_qWT); allocate(this.m_qWT(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qSHEAR)) deallocate(this.m_qSHEAR); allocate(this.m_qSHEAR(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qBUOY)) deallocate(this.m_qBUOY); allocate(this.m_qBUOY(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qDISS)) deallocate(this.m_qDISS); allocate(this.m_qDISS(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_dqke)) deallocate(this.m_dqke); allocate(this.m_dqke(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_qc_bl)) deallocate(this.m_qc_bl); allocate(this.m_qc_bl(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    if(allocated(this.m_cldfra_bl)) deallocate(this.m_cldfra_bl); allocate(this.m_cldfra_bl(this.m_IMS:this.m_IME,this.m_KMS:this.m_KME,this.m_JMS:this.m_JME))
    ! Copy
    do j=this.m_JTS, this.m_JTE 
       do k=this.m_KTS, this.m_KTE 
          do i=this.m_ITS, this.m_ITE 
             this.m_dz(i,k,j)        = other.m_dz(i,k,j)
             this.m_u(i,k,j)         = other.m_u(i,k,j)
             this.m_v(i,k,j)         = other.m_v(i,k,j) 
             this.m_w(i,k,j)         = other.m_w(i,k,j)
             this.m_th(i,k,j)        = other.m_th(i,k,j)
             this.m_qv(i,k,j)        = other.m_qv(i,k,j)
             this.m_p(i,k,j)         = other.m_p(i,k,j)
             this.m_exner(i,k,j)     = other.m_exner(i,k,j)
             this.m_rho(i,k,j)       = other.m_rho(i,k,j)
             this.m_T3D(i,k,j)       = other.m_T3D(i,k,j)
             this.m_qc(i,k,j)        = other.m_qc(i,k,j)
             this.m_qi(i,k,j)        = other.m_qi(i,k,j)
             this.m_qni(i,k,j)       = other.m_qni(i,k,j)
             this.m_qnc(i,k,j)       = other.m_qnc(i,k,j)
             this.m_Qke(i,k,j)       = other.m_Qke(i,k,j)
             this.m_Tsq(i,k,j)       = other.m_Tsq(i,k,j)
             this.m_Qsq(i,k,j)       = other.m_Qsq(i,k,j)
             this.m_Cov(i,k,j)       = other.m_Cov(i,k,j)
             this.m_tke_pbl(i,k,j)   = other.m_tke_pbl(i,k,j)
             this.m_qke_adv(i,k,j)   = other.m_qke_adv(i,k,j)
             this.m_RUBLTEN(i,k,j)   = other.m_RUBLTEN(i,k,j)
             this.m_RVBLTEN(i,k,j)   = other.m_RVBLTEN(i,k,j)
             this.m_RTHBLTEN(i,k,j)  = other.m_RTHBLTEN(i,k,j)
             this.m_RQVBLTEN(i,k,j)  = other.m_RQVBLTEN(i,k,j)
             this.m_RQCBLTEN(i,k,j)  = other.m_RQCBLTEN(i,k,j)
             this.m_RQIBLTEN(i,k,j)  = other.m_RQIBLTEN(i,k,j)
             this.m_RQNIBLTEN(i,k,j) = other.m_RQNIBLTEN(i,k,j)
             this.m_exch_h(i,k,j)    = other.m_exch_h(i,k,j)
             this.m_exch_m(i,k,j)    = other.m_exch_m(i,k,j)
             this.m_edmf_a(i,k,j)    = other.m_edmf_a(i,k,j)
             this.m_edmf_w(i,k,j)    = other.m_edmf_w(i,k,j)
             this.m_edmf_qt(i,k,j)   = other.m_edmf_qt(i,k,j)
             this.m_edmf_thl(i,k,j)  = other.m_edmf_thl(i,k,j)
             this.m_edmf_ent(i,k,j)  = other.m_edmf_ent(i,k,j)
             this.m_edmf_qc(i,k,j)   = other.m_edmf_qc(i,k,j)
             this.m_el_pbl(i,k,j)    = other.m_el_pbl(i,k,j)
             this.m_qWT(i,k,j)       = other.m_qWT(i,k,j)
             this.m_qSHEAR(i,k,j)    = other.m_qSHEAR(i,k,j)
             this.m_qBUOY(i,k,j)     = other.m_qBUOY(i,k,j)
             this.m_qDISS(i,k,j)     = other.m_qDISS(i,k,j)
             this.m_dqke(i,k,j)      = other.m_dqke(i,k,j)
             this.m_qc_bl(i,k,j)     = other.m_qc_bl(i,k,j)
             this.m_cldfra_bl(i,k,j) = other.m_cldfra_bl(i,k,j)
         end do 
       end do 
    end do 
    do j=this.m_JTS, this.m_JTE 
       do i=this.m_ITS, this.m_ITE 
          this.m_xland(i,j)  = other.m_xland(i,j)
          this.m_ust(i,j)    = other.m_ust(i,j)
          this.m_ch(i,j)     = other.m_ch(i,j)
          this.m_rmol(i,j)   = other.m_rmol(i,j)
          this.m_ts(i,j)     = other.m_ts(i,j)
          this.m_qsfc(i,j)   = other.m_qsfc(i,j)
          this.m_qcg(i,j)    = other.m_qcg(i,j)
          this.m_ps(i,j)     = other.m_ps(i,j)
          this.m_hfx(i,j)    = other.m_hfx(i,j)
          this.m_qfx(i,j)    = other.m_qfx(i,j)
          this.m_wspd(i,j)   = other.m_wspd(i,j)
          this.m_uoce(i,j)   = other.m_uoce(i,j)
          this.m_voce(i,j)   = other.m_voce(i,j)
          this.m_vdfg(i,j)  = other.m_vdfg(i,j)
          this.m_znt(i,j)    = other.m_znt(i,j)
          this.m_Pblh(i,j)   = other.m_Pblh(i,j)
          this.m_wstar(i,j)  = other.m_wstar(i,j)
          this.m_delta(i,j)  = other.m_delta(i,j)
          this.m_KPBL(i,j)   = other.m_KPBL(i,j)
       end do 
    end do 
#if (MYNN_BL_DRIVER_EXEC_TRACE) == 1
     MYNN_BL_DRIVER_TRACE_END 
#endif
    this.m_is_derived_type_built = .true.
    ier = 0
end subroutine copy_create

subroutine allocation_status_array2D(array,name)
    implicit none
    real(kind=sp), allocatable, dimension(:,:), intent(in) :: array 
    character(len=*),                           intent(in) :: name 
                       ! Locals
    integer(kind=i8), automatic :: vaddr 
    integer(kind=i8), automatic :: size 
    integer(kind=i4), dimension(2), automatic :: lo_bound
    integer(kind=i4), dimension(2), automatic :: hi_bound 
    logical(kind=i1), automatic :: is_allocated 
    vaddr        = LOC(array)
    size         = SIZEOF(array)
    lo_bound     = LBOUND(array)
    hi_bound     = UBOUND(array)
    is_allocated = allocated(array)
    print*,"======================INFO======================"
    print*, "Array member name:", name 
    print*, "vaddr=",vaddr
    print*, "size=",size
    print*, "allocated=",is_allocated
    print*, "lo_bound=",lo_bound
    print*, "hi_bound=",hi_bound 
    print*,"======================INFO======================================"
end subroutine allocation_status_array2D

subroutine allocation_status_array3D(array,name)
    implicit none
    real(kind=sp), allocatable, dimension(:,:,:), intent(in) :: array 
    character(len=*),                           intent(in) :: name 
                       ! Locals
    integer(kind=i8), automatic :: vaddr 
    integer(kind=i8), automatic :: size 
    integer(kind=i4), dimension(3), automatic :: lo_bound
    integer(kind=i4), dimension(3), automatic :: hi_bound 
    logical(kind=i1), automatic :: is_allocated 
    vaddr        = LOC(array)
    size         = SIZEOF(array)
    lo_bound     = LBOUND(array)
    hi_bound     = UBOUND(array)
    is_allocated = allocated(array)
    print*,"======================INFO======================"
    print*, "Array member name:", name 
    print*, "vaddr=",vaddr
    print*, "size=",size
    print*, "allocated=",is_allocated
    print*, "lo_bound=",lo_bound
    print*, "hi_bound=",hi_bound 
    print*,"======================INFO======================================"
end subroutine allocation_status_array3D

end module mod_mynn_bl_driver

