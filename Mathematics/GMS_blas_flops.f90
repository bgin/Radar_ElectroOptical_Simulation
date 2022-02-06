

!====================================================================================!
!// Copyright (c) 2017-2020, University of Tennessee. All rights reserved.
!// SPDX-License-Identifier: BSD-3-Clause
!// This program is free software: you can redistribute it and/or modify it under
!// the terms of the BSD 3-Clause license. See the accompanying LICENSE file.
!====================================================================================!


module blas_flops



!================================================!
! BLAS FLOPS calculation routines.
! Ported from C++ header flops i.e. blas/flops.hh
!================================================!

     use mod_kinds, only : i4, dp
     implicit none
     public

     real(kind=dp),    parameter, private :: nano = 1.0e-9_dp
     integer(kind=i4), parameter, private :: mul_ops = 1
     integer(kind=i4), parameter, private :: add_ops = 1
     integer(kind=i4), parameter, private :: cmul_ops= 6 ! complex
     integer(kind=i4), parameter, private :: cadd_ops= 2 ! complex
     
     contains

     pure elemental function fmuls_asum(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_asum
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp) :: ops
       ops = 0.0_dp
     end function fmuls_asum

     pure elemental function fadds_asum(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_asum
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n-1.0_dp
     end function fadds_asum


     pure elemental function fmuls_axpy(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_axpy
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n
     end function fmuls_axpy


     pure elemental function fadds_axpy(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_axpy
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n
     end function fadds_axpy


     pure elemental function fmuls_iamax(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_iamax
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=0.0_dp
     end function fmuls_iamax

     !  n-1 compares, which are essentially adds (x > y is x - y > 0)
     pure elemental function fadds_iamax(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_iamax
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n-1.0_dp
     end function fadds_iamax


     pure elemental function fmuls_nrm2(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_nrm2
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n
     end function fmuls_nrm2


     pure elemental function fadds_nrm2(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_nrm2
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n-1.0_dp
     end function fadds_nrm2


     pure elemental function fadds_dot(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_dot
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n-1.0_dp
     end function fadds_dot


     pure elemental function fmuls_dot(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_dot
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n
     end function fmuls_dot


     pure elemental function fmuls_scal(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_scal
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=n
     end function fmuls_scal


     pure elemental function fadds_scal(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_scal
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=0.0_dp
     end function fadds_scal

     ! Level 2 BLAS
     !// most formulas assume alpha=1, beta=0 or 1; otherwise add lower-order terms.
     !// i.e., this is minimum flops and bandwidth that could be consumed.
     pure elemental function fmuls_gemv(m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_gemv
       !dir$ optimize : 3
#endif
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: ops
       ops=m*n
     end function fmuls_gemv


     pure elemental function fadds_gemv(m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_gemv
       !dir$ optimize : 3
#endif
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: ops
       ops=m*n
     end function fadds_gemv


     pure elemental function fmuls_trmv(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_trmv
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=0.5_dp*n*(n+1.0_dp)
     end function fmuls_trmv


     pure elemental function fadds_trmv(n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_trmv
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: n
       real(kind=dp)  :: ops
       ops=0.5_dp*n*(n-1.0_dp)
     end function fadds_trmv


     pure elemental function fmuls_ger(m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_ger
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: m,n
       real(kind=dp) :: ops
       ops=m*n
     end function fmuls_ger


     pure elemental function fadds_ger(m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_ger
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: m,n
       real(kind=dp) :: ops
       ops=m*n
     end function fadds_ger


     pure elemental function fmuls_gemm(m,n,k) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_gem
       !dir$ optimize : 3
#endif
       real(kind=dp),  intent(in) :: m,n,k
       real(kind=dp) :: ops
       ops=m*n*k
     end function fmuls_gemm


     pure elemental function fadds_gemm(m,n,k) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_gem
       !dir$ optimize : 3
#endif
       real(kind=dp),  intent(in) :: m,n,k
       real(kind=dp) :: ops
       ops=m*n*k
     end function fadds_gemm


     pure elemental function fmuls_gbmm(m,n,kl,ku) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_gbmm
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: m,n,kl,ku
       real(kind=dp)  :: ops
       real(kind) :: kl2,ku2,ltrm,rtrm
       kl2=kl*0.5_dp;ku2=ku*0.5_dp
       ltrm=m*kl+m*ku-kl*kl2
       rtrm=ku*ku2*m-kl2-ku2
       ops=(ltrm-rtrm)*n
     end function fmuls_gbmm


     pure elemental function fadds_gbmm(m,n,kl,ku) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_gbmm
       !dir$ optimize : 3
#endif
       real(kind=dp),   intent(in) :: m,n,kl,ku
       real(kind=dp)  :: ops
       ops=fmuls_gbmm(m,n,kl,ku)
     end function fadds_gbmm


     pure elemental function fmuls_hemm(side,m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_hemm
       !dir$ optimize : 3
#end
       logical(kind=i4),    intent(in) :: side !originally blas::Side::left
       real(kind=dp),       intent(in) :: m,n
       real(kind=dp) :: ops
       if(side) then
          ops=m*m*n
       else
          ops=m*n*n
       end if
     end function fmuls_hemm


     pure elemental function fadds_hemm(side,m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_hemm
       !dir$ optimize : 3
#end
       logical(kind=i4),    intent(in) :: side !originally blas::Side::left
       real(kind=dp),       intent(in) :: m,n
       real(kind=dp) :: ops
       ops=fmuls_hemm(side,m,n)
     end function fadds_hemm


     pure elemental function fmuls_herk(n,k) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_herk
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n,k
       real(kind=dp) :: ops
       ops=0.5_dp*k*n*(n+1)
     end function fmuls_herk


     pure elemental function fadds_herk(n,k) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_herk
       !dir$ optimize : 3
#end
         real(kind=dp),  intent(in) :: n,k
         real(kind=dp) :: ops
         ops=fmuls_herk(n,k)
     end function fadds_herk


     pure elemental function fmuls_her2k(n,k) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_her2k
       !dir$ optimize : 3
#end
         real(kind=dp),  intent(in) :: n,k
         real(kind=dp) :: ops
         ops=k*n*n
     end function fmuls_her2k


     pure elemental function fadds_her2k(n,k)  result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_her2k
       !dir$ optimize : 3
#end
         real(kind=dp),  intent(in) :: n,k
         real(kind=dp) :: ops
         ops=fmuls_her2k(n,k)
     end function fadds_her2k


     pure elemental function fmuls_trmm(side,m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fmuls_trmm
       !dir$ optimize : 3
#end       
       logical(kind=i4),    intent(in) :: side !originally blas::Side::left
       real(kind=dp),       intent(in) :: m,n
       real(kind=dp) :: ops
       if(side) then
          ops=0.5_dp*n*m*(m+1.0_dp)
       else
          ops=0.5_dp*m*n*(n+1.0_dp)
       end if
     end function fmuls_trmm


     pure elemental function fadds_trmm(side,m,n) result(ops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: fadds_trmm
       !dir$ optimize : 3
#end       
       logical(kind=i4),    intent(in) :: side !originally blas::Side::left
       real(kind=dp),       intent(in) :: m,n
       real(kind=dp) :: ops
       if(side) then
          ops=0.5_dp*n*m*(m-1.0_dp)
       else
          ops=0.5_dp*m*n*(n-1.0_dp)
       end if
     end function fadds_trmm

     ! read: x
     pure elemental function bytes_asum(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_asum
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(n*sizeof(n))
     end function bytes_asum

     ! read x, y; write y
     pure elemental function bytes_axpy(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_axpy
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(3.0_dp*n*sizeof(n))
     end function bytes_axpy

     !// read x; write y
     pure elemental function bytes_copy(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_copy
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(2.0_dp*n*sizeof(n))
     end function bytes_copy

     ! read x
     pure elemental function bytes_iamax(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_iamax
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(n*sizeof(n))
     end function bytes_iamax

     ! read x
       pure elemental function bytes_nrm2(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_nrm2
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(n*sizeof(n))
     end function bytes_nrm2

     ! read x, y
     pure elemental function bytes_dot(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_dot
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(2.0_dp*n*sizeof(n))
     end function bytes_dot

     ! read x, write y
      pure elemental function bytes_scal(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_scal
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(2.0_dp*n*sizeof(n))
     end function bytes_scal

     !  // read x, y; write x, y
     pure elemental function bytes_swap(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_swap
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(4.0_dp*n*sizeof(n))
     end function bytes_swap


     ! BLAS LEVEL 2
     !  read A, x; write y
     pure elemental function bytes_gemv(m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_gemv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp)  :: bytes
       bytes=nano*((m*n+m+n)*sizeof(m))
     end function bytes_gemv

     !  // read A triangle, x; write y
     pure elemental function bytes_hemv(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_hemv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*((0.5_dp*(n+1.0_dp)*n+2.0_dp*n)*sizeof(n))
     end function bytes_hemv

     pure elemental function bytes_symv(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_symv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=bytes_hemv(n)
     end function bytes_symv

     pure elemental function bytes_trmv(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_trmv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*((0.5_dp*(n+1.0_dp)*n+2.0_dp*n)*sizeof(n))
     end function bytes_trmv

     pure elemental function bytes_trsv(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_trsv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=bytes_trmv(n)
     end function bytes_trsv

     !  // read A, x, y; write A
     pure elemental function bytes_ger(m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_ger
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp)  :: bytes
       bytes=nano*((2.0_dp*m*n+m+n)*sizeof(m))
     end function bytes_ger

     !  // read A triangle, x; write A triangle
     pure elemental function bytes_her(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_her
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(((n+1.0_dp)*n+n)*sizeof(n))
     end function bytes_her

      pure elemental function bytes_syr(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_syr
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=bytes_her(n)
     end function bytes_syr

     ! read A triangle, x, y; write A triangle
     pure elemental function bytes_her2(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_her2
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=nano*(((n+1.0_dp)*n+n+n)*sizeof(n))
     end function bytes_her2

     pure elemental function bytes_syr2(n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_syr2
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp)  :: bytes
       bytes=bytes_her2(n)
     end function bytes_syr2

     ! BLAS Level 3
     ! // read A, B, C; write C
     pure elemental function bytes_gemm(m,n,k) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_gemm
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n,k
       real(kind=dp)  :: bytes
       bytes=nano*((m*k+k*n+2.0_dp*m*m)*sizeof(n))
     end function bytes_gemm

     pure elemental function bytes_hemm(side,m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_hemm
       !dir$ optimize : 3
#end
        logical(kind=i4),       intent(in) :: side
        real(kind=dp),          intent(in) :: m,n
        real(kind=dp) :: bytes
        real(kind=dp) :: tmp
        if(side) then
           tmp=0.5_dp*m*(m+1)
        else
           tmp=0.5_dp*n*(n+1)
        end if
        bytes=nano*(tmp+3.0_dp*m*n)*sizeof(n)
     end function bytes_hemm

     pure elemental function bytes_symm(side,m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_symm
       !dir$ optimize : 3
#end
         logical(kind=i4),       intent(in) :: side
         real(kind=dp),          intent(in) :: m,n
         real(kind=dp) :: bytes
         bytes=bytes_hemm(side,m,n)
     end function bytes_symm

       
     pure elemental function bytes_herk(n,k) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_herk
       !dir$ optimize : 3
#end
        real(kind=dp),  intent(in) :: n,k
        real(kind=dp) :: bytes
        real(kind=dp) :: tmp
        tmp=0.5_dp*n*(n+1.0_dp)
        bytes=nano*(n*k+2.0_dp*tmp)*sizeof(n)
     end function bytes_herk


     pure elemental function bytes_syrk(n,k) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_syrk
       !dir$ optimize : 3
#end
        real(kind=dp),  intent(in) :: n,k
        real(kind=dp) :: bytes
        bytes=bytes_herk(n,k)
     end function bytes_syrk


     pure elemental function bytes_herk2(n,k) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_herk2
       !dir$ optimize : 3
#end
        real(kind=dp),  intent(in) :: n,k
        real(kind=dp) :: bytes
        real(kind=dp) :: tmp
        tmp=0.5_dp*n*(n+1.0_dp)
        bytes=nano*((2.0_dp*n*k+2.0_dp*tmp)*sizeof(n)
     end function bytes_herk2

      
     pure elemental function bytes_syrk2(n,k) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_syrk2
       !dir$ optimize : 3
#end
        real(kind=dp),  intent(in) :: n,k
        real(kind=dp) :: bytes
        bytes=bytes_herk2(n,k)
     end function bytes_syrk2

     pure elemental function bytes_trmm(side,m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_trmm
       !dir$ optimize : 3
#end
       logical(kind=i4),  intent(in) :: side
       real(kind=dp),     intent(in) :: m,n
       real(kind=dp) :: bytes
       if(side) then
          bytes=nano*(0.5_dp*(m+1)*m+2.0_dp*m*n)*sizeof(n)
       else
          bytes=nano*(0.5_dp*(n+1)*m+2.0_dp*m*n)*sizeof(n)
       end if
     end function bytes_trmm

     pure elemental function bytes_trsm(side,m,n) result(bytes)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: bytes_trmm
       !dir$ optimize : 3
#end
       logical(kind=i4),  intent(in) :: side
       real(kind=dp),     intent(in) :: m,n
       real(kind=dp) :: bytes
       bytes=bytes_trmm(side,m,n)
     end function bytes_trsm
     
     
     ! Gflops calculation functions
     ! BLAS Level 1
     pure function flops_asum(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_asum
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_asum(n)+ &
                   add_ops*fadds_asum(n))
     end function flops_asum

     pure function flops_axpy(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_asum
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_axpy(n)+ &
                   add_ops*fadds_axpy(n))
     end function flops_axpy

     pure function flops_copy(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_copy
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
        flops=0.0_dp
     end function flops_copy

     pure function flops_iamax(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_iamax
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_iamax(n)+ &
                   add_ops*fadds_iamax(n))
     end function flops_iamax

     pure function flops_nrm2(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_nrm2
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_nrm2(n)+ &
                   add_ops*fadds_nrm2(n))
     end function flops_nrm2

     pure function flops_dot(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_dot
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_dot(n)+ &
                   add_ops*fadds_dot(n))
     end function flops_dot

     pure function flops_scal(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_scal
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_scal(n)+ &
                   add_ops*fadds_scal(n))
     end function flops_scal

     pure function flops_swap(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_swap
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
        flops=0.0_dp
     end function flops_swap

     ! BLAS Level 2
     
      pure function flops_gemv(m,n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_gemv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_gemv(m,n)+ &
                   add_ops*fadds_gemv(m,n))
     end function flops_gemv

     pure function flops_symv(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_symv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_gemv(n)
     end function flops_symv

     pure function flops_hemv(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_hemv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_symv(n)
     end function flops_hemv

     pure function flops_trmv(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_trmv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_trmv(n)+ &
                   add_ops*fadds_trmv(n))
     end function flops_trmv
     
     pure function flops_trsv(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_trsv
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_trmv(n)
     end function flops_trsv

     pure function flops_her(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_her
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_ger(n)
     end function flops_her

     pure function flops_syr(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_syr
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_her(n)
     end function flops_syr

     pure function flops_ger(m,n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_ger
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_ger(m,n)+ &
                   add_ops*fadds_ger(m,n))
     end function flops_ger

     pure function flops_her2(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_her2
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=2.0_dp*flops_her(n)
     end function flops_her2

     pure function flops_syr2(n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_syr
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       flops=flops_her2(n)
     end function flops_syr2

     ! BLAS Level 3
       pure function flops_gemm(m,n,k) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_gemm
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: m,n,k
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_gemm(m,n,k)+ &
                   add_ops*fadds_gemm(m,n,k))
     end function flops_gemm

     pure function flops_gbmm(m,n,k,kl,ku) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_gemm
       !dir$ optimize : 3
#end
       real(kind=dp),  intent(in) :: n
       real(kind=dp) :: flops
       if(m==k) then
          flops=nano*(mul_ops*fmuls_gbmm(m,n,k,kl,ku)+ &
                      add_ops*fadds_gemm(m,n,k,kl,ku))
       else
          flops=-1.0_dp
       end if
     end function flops_gbmm

     pure function flops_hemm(side,m,n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_hemm
       !dir$ optimize : 3
#end
       logical(kind=i4), intent(in) :: side
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_gemm(side,m,n)+ &
                   add_ops*fadds_gemm(side,m,n))
     end function flops_hemm

     pure function flops_hbmm(m,n,kd) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_hemm
       !dir$ optimize : 3
#end
       
       real(kind=dp),  intent(in) :: m,n,kd
       real(kind=dp) :: flops
       flops=nano*(mul_ops*fmuls_gbmm(m,n,m,kd,kd)+ &
                   add_ops*fadds_gbmm(m,n,m,kd,kd))
     end function flops_hbmm

     pure function flops_symm(side,m,n) result(flops)
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attributes inline :: flops_symm
       !dir$ optimize : 3
#end
       logical(kind=i4), intent(in) :: side
       real(kind=dp),  intent(in) :: m,n
       real(kind=dp) :: flops
       flops=flops_hemm(sid,m,n)
     end function flops_symm

     

     
     
       
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
       

       
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
 

















end module blas_flops
