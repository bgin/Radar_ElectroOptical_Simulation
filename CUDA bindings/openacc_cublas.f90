!!  Fortran CUDA Library interface -- CUBLAS Device for OpenACC
!!
!!  Copyright (C) 2018 Mentor, A Siemens Business
!!
!!  This software is provided 'as-is', without any express or implied
!!  warranty.  In no event will the authors be held liable for any damages
!!  arising from the use of this software.
!!
!!  Permission is granted to anyone to use this software for any purpose,
!!  including commercial applications, and to alter it and redistribute it
!!  freely, subject to the following restrictions:
!!
!!  1. The origin of this software must not be misrepresented; you must not
!!     claim that you wrote the original software. If you use this software
!!     in a product, an acknowledgment in the product documentation would be
!!     appreciated but is not required.
!!  2. Altered source versions must be plainly marked as such, and must not be
!!     misrepresented as being the original software.
!!  3. This notice may not be removed or altered from any source distribution.

module openacc_cublas
  use iso_c_binding

  type, bind(c) :: cuBlasHandle
     type(c_ptr) :: handle
  end type cuBlasHandle

  enum, bind(c)
     enumerator :: CUBLAS_STATUS_SUCCESS          = 0
     enumerator :: CUBLAS_STATUS_NOT_INITIALIZED  = 1
     enumerator :: CUBLAS_STATUS_ALLOC_FAILED     = 3
     enumerator :: CUBLAS_STATUS_INVALID_VALUE    = 7
     enumerator :: CUBLAS_STATUS_ARCH_MISMATCH    = 8
     enumerator :: CUBLAS_STATUS_MAPPING_ERROR    = 11
     enumerator :: CUBLAS_STATUS_EXECUTION_FAILED = 13
     enumerator :: CUBLAS_STATUS_INTERNAL_ERROR   = 14
     enumerator :: CUBLAS_STATUS_NOT_SUPPORTED    = 15
     enumerator :: CUBLAS_STATUS_LICENSE_ERROR    = 16
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_FILL_MODE_LOWER = 0
     enumerator :: CUBLAS_FILL_MODE_UPPER = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_DIAG_NON_UNIT = 0
     enumerator :: CUBLAS_DIAG_UNIT     = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_SIDE_LEFT  = 0
     enumerator :: CUBLAS_SIDE_RIGHT = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_OP_N = 0
     enumerator :: CUBLAS_OP_T = 1
     enumerator :: CUBLAS_OP_C = 2
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_POINTER_MODE_HOST   = 0
     enumerator :: CUBLAS_POINTER_MODE_DEVICE = 1
  end enum

  interface

     integer(c_int) function cublasCreate (handle) &
         bind (c, name="cublasCreate_v2")
       import
       type(cublasHandle) :: handle
       !$acc routine seq
     end function cublasCreate

     integer(c_int) function cublasDestroy (handle) &
         bind (c, name="cublasDestroy_v2")
       import
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDestroy

     integer(c_int) function cublasGetVersion (handle, version) &
         bind (c, name="cublasGetVersion_v2")
       import
       type(cublasHandle), value :: handle
       integer(c_int) :: version
       !$acc routine seq
     end function cublasGetVersion

     integer(c_int) function cublasSetStream (handle, streamId) &
         bind (c, name="cublasSetStream_v2")
       import
       type(c_ptr), value :: streamId
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasSetStream

     integer(c_int) function cublasGetStream (handle, streamId) &
         bind (c, name="cublasGetStream_v2")
       import
       type(cublasHandle), value :: handle
       type(c_ptr) :: streamId
       !$acc routine seq
     end function cublasGetStream

     integer(c_int) function cublasSrotm_v2 (handle, n, x, incx, y, incy, param) &
         bind (c, name="cublasSrotm_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y, param
       !$acc routine seq
     end function cublasSrotm_v2

     integer(c_int) function cublasSrotm (handle, n, x, incx, y, incy, param) &
         bind (c, name="cublasSrotm_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y, param
       !$acc routine seq
     end function cublasSrotm

     integer(c_int) function cublasDrotm_v2 (handle, n, x, incx, y, incy, param) &
         bind (c, name="cublasDrotm_v2")
       import
       real(c_double), dimension(*) :: x, y, param
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotm_v2

     integer(c_int) function cublasDrotm (handle, n, x, incx, y, incy, param) &
         bind (c, name="cublasDrotm_v2")
       import
       real(c_double), dimension(*) :: x, y, param
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotm

     integer(c_int) function cublasSrotmg_v2 (handle, d1, d2, x1, y1, param) &
         bind (c, name="cublasSrotmg_v2")
       import
       real(c_float) :: d1, d2, x1, y1
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: param
       !$acc routine seq
     end function cublasSrotmg_v2

     integer(c_int) function cublasSrotmg (handle, d1, d2, x1, y1, param) &
         bind (c, name="cublasSrotmg_v2")
       import
       real(c_float) :: d1, d2, x1, y1
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: param
       !$acc routine seq
     end function cublasSrotmg

     integer(c_int) function cublasDrotmg_v2 (handle, d1, d2, x1, y1, param) &
         bind (c, name="cublasDrotmg_v2")
       import
       real(c_double), dimension(*) :: param
       real(c_double) :: d1, d2, x1, y1
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotmg_v2

     integer(c_int) function cublasDrotmg (handle, d1, d2, x1, y1, param) &
         bind (c, name="cublasDrotmg_v2")
       import
       real(c_double), dimension(*) :: param
       real(c_double) :: d1, d2, x1, y1
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotmg

     integer(c_int) function cublasSsbmv_v2 (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSsbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsbmv_v2

     integer(c_int) function cublasSsbmv (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSsbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsbmv

     integer(c_int) function cublasDsbmv_v2 (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDsbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDsbmv_v2

     integer(c_int) function cublasDsbmv (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDsbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDsbmv

     integer(c_int) function cublasSspmv_v2 (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasSspmv_v2")
       import
       real(c_float) :: alpha, beta
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x, y
       !$acc routine seq
     end function cublasSspmv_v2

     integer(c_int) function cublasSspmv (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasSspmv_v2")
       import
       real(c_float) :: alpha, beta
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x, y
       !$acc routine seq
     end function cublasSspmv

     integer(c_int) function cublasDspmv_v2 (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasDspmv_v2")
       import
       real(c_double), dimension(*) :: AP, x, y
       integer(c_int), value :: uplo, n, incx, incy
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspmv_v2

     integer(c_int) function cublasDspmv (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasDspmv_v2")
       import
       real(c_double), dimension(*) :: AP, x, y
       integer(c_int), value :: uplo, n, incx, incy
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspmv

     integer(c_int) function cublasSspr_v2 (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasSspr_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: uplo, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, AP
       !$acc routine seq
     end function cublasSspr_v2

     integer(c_int) function cublasSspr (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasSspr_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: uplo, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, AP
       !$acc routine seq
     end function cublasSspr

     integer(c_int) function cublasDspr_v2 (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasDspr_v2")
       import
       real(c_double), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspr_v2

     integer(c_int) function cublasDspr (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasDspr_v2")
       import
       real(c_double), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspr

     integer(c_int) function cublasSspr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasSspr2_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y, AP
       !$acc routine seq
     end function cublasSspr2_v2

     integer(c_int) function cublasSspr2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasSspr2_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y, AP
       !$acc routine seq
     end function cublasSspr2

     integer(c_int) function cublasDspr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasDspr2_v2")
       import
       real(c_double), dimension(*) :: x, y, AP
       integer(c_int), value :: uplo, n, incx, incy
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspr2_v2

     integer(c_int) function cublasDspr2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasDspr2_v2")
       import
       real(c_double), dimension(*) :: x, y, AP
       integer(c_int), value :: uplo, n, incx, incy
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDspr2

     integer(c_int) function cublasIsamax_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIsamax_v2")
       import
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasIsamax_v2

     integer(c_int) function cublasIsamax (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIsamax_v2")
       import
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasIsamax

     integer(c_int) function cublasIdamax_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIdamax_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIdamax_v2

     integer(c_int) function cublasIdamax (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIdamax_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIdamax

     integer(c_int) function cublasIcamax_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIcamax_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIcamax_v2

     integer(c_int) function cublasIcamax (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIcamax_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIcamax

     integer(c_int) function cublasIzamax_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIzamax_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIzamax_v2

     integer(c_int) function cublasIzamax (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIzamax_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIzamax

     integer(c_int) function cublasIsamin_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIsamin_v2")
       import
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasIsamin_v2

     integer(c_int) function cublasIsamin (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIsamin_v2")
       import
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasIsamin

     integer(c_int) function cublasIdamin_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIdamin_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIdamin_v2

     integer(c_int) function cublasIdamin (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIdamin_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIdamin

     integer(c_int) function cublasIcamin_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIcamin_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIcamin_v2

     integer(c_int) function cublasIcamin (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIcamin_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIcamin

     integer(c_int) function cublasIzamin_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIzamin_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIzamin_v2

     integer(c_int) function cublasIzamin (handle, n, x, incx, ret_result) &
         bind (c, name="cublasIzamin_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       integer(c_int) :: ret_result
       !$acc routine seq
     end function cublasIzamin

     integer(c_int) function cublasSasum_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasSasum_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSasum_v2

     integer(c_int) function cublasSasum (handle, n, x, incx, ret_result) &
         bind (c, name="cublasSasum_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSasum

     integer(c_int) function cublasDasum_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDasum_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDasum_v2

     integer(c_int) function cublasDasum (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDasum_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDasum

     integer(c_int) function cublasScasum_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasScasum_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasScasum_v2

     integer(c_int) function cublasScasum (handle, n, x, incx, ret_result) &
         bind (c, name="cublasScasum_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasScasum

     integer(c_int) function cublasDzasum_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDzasum_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDzasum_v2

     integer(c_int) function cublasDzasum (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDzasum_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDzasum

     integer(c_int) function cublasSgbmv_v2 &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgbmv_v2

     integer(c_int) function cublasSgbmv &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgbmv

     integer(c_int) function cublasDgbmv_v2 &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDgbmv_v2

     integer(c_int) function cublasDgbmv &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDgbmv

     integer(c_int) function cublasCgbmv_v2 &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgbmv_v2

     integer(c_int) function cublasCgbmv &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgbmv

     integer(c_int) function cublasZgbmv_v2 &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgbmv_v2

     integer(c_int) function cublasZgbmv &
         (handle, trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZgbmv_v2")
       import
       integer(c_int), value :: trans, m, n, kl, ku, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgbmv

     integer(c_int) function cublasSgemv_v2 &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgemv_v2

     integer(c_int) function cublasSgemv &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgemv

     integer(c_int) function cublasDgemv_v2 &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDgemv_v2

     integer(c_int) function cublasDgemv &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDgemv

     integer(c_int) function cublasCgemv_v2 &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemv_v2

     integer(c_int) function cublasCgemv &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemv

     integer(c_int) function cublasZgemv_v2 &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgemv_v2

     integer(c_int) function cublasZgemv &
         (handle, trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZgemv_v2")
       import
       integer(c_int), value :: trans, m, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgemv

     integer(c_int) function cublasSaxpy_v2 (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasSaxpy_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSaxpy_v2

     integer(c_int) function cublasSaxpy (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasSaxpy_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSaxpy

     integer(c_int) function cublasDaxpy_v2 (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasDaxpy_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDaxpy_v2

     integer(c_int) function cublasDaxpy (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasDaxpy_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDaxpy

     integer(c_int) function cublasCaxpy_v2 (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasCaxpy_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCaxpy_v2

     integer(c_int) function cublasCaxpy (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasCaxpy_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCaxpy

     integer(c_int) function cublasZaxpy_v2 (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasZaxpy_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZaxpy_v2

     integer(c_int) function cublasZaxpy (handle, n, alpha, x, incx, y, incy) &
         bind (c, name="cublasZaxpy_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZaxpy

     integer(c_int) function cublasScopy_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasScopy_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasScopy_v2

     integer(c_int) function cublasScopy (handle, n, x, incx, y, incy) &
         bind (c, name="cublasScopy_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasScopy

     integer(c_int) function cublasDcopy_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasDcopy_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDcopy_v2

     integer(c_int) function cublasDcopy (handle, n, x, incx, y, incy) &
         bind (c, name="cublasDcopy_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDcopy

     integer(c_int) function cublasCcopy_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasCcopy_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCcopy_v2

     integer(c_int) function cublasCcopy (handle, n, x, incx, y, incy) &
         bind (c, name="cublasCcopy_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCcopy

     integer(c_int) function cublasZcopy_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasZcopy_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZcopy_v2

     integer(c_int) function cublasZcopy (handle, n, x, incx, y, incy) &
         bind (c, name="cublasZcopy_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZcopy

     integer(c_int) function cublasSdot_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasSdot_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSdot_v2

     integer(c_int) function cublasSdot (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasSdot_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSdot

     integer(c_int) function cublasDdot_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasDdot_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDdot_v2

     integer(c_int) function cublasDdot (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasDdot_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDdot

     integer(c_int) function cublasCdotc_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasCdotc_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasCdotc_v2

     integer(c_int) function cublasCdotc (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasCdotc_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasCdotc

     integer(c_int) function cublasZdotc_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasZdotc_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdotc_v2

     integer(c_int) function cublasZdotc (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasZdotc_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdotc

     integer(c_int) function cublasCdotu_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasCdotu_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasCdotu_v2

     integer(c_int) function cublasCdotu (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasCdotu_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: ret_result
       !$acc routine seq
     end function cublasCdotu

     integer(c_int) function cublasZdotu_v2 (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasZdotu_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdotu_v2

     integer(c_int) function cublasZdotu (handle, n, x, incx, y, incy, ret_result) &
         bind (c, name="cublasZdotu_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: ret_result
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdotu

     integer(c_int) function cublasSnrm2_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasSnrm2_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSnrm2_v2

     integer(c_int) function cublasSnrm2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasSnrm2_v2")
       import
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSnrm2

     integer(c_int) function cublasDnrm2_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDnrm2_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDnrm2_v2

     integer(c_int) function cublasDnrm2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDnrm2_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: ret_result
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDnrm2

     integer(c_int) function cublasScnrm2_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasScnrm2_v2")
       import
       complex(c_float), dimension(*) :: x
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasScnrm2_v2

     integer(c_int) function cublasScnrm2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasScnrm2_v2")
       import
       complex(c_float), dimension(*) :: x
       real(c_float) :: ret_result
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasScnrm2

     integer(c_int) function cublasDznrm2_v2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDznrm2_v2")
       import
       real(c_double) :: ret_result
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDznrm2_v2

     integer(c_int) function cublasDznrm2 (handle, n, x, incx, ret_result) &
         bind (c, name="cublasDznrm2_v2")
       import
       real(c_double) :: ret_result
       integer(c_int), value :: n, incx
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDznrm2

     integer(c_int) function cublasSrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasSrot_v2")
       import
       real(c_float) :: c, s
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSrot_v2

     integer(c_int) function cublasSrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasSrot_v2")
       import
       real(c_float) :: c, s
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSrot

     integer(c_int) function cublasDrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasDrot_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrot_v2

     integer(c_int) function cublasDrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasDrot_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       real(c_double) :: c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrot

     integer(c_int) function cublasCrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasCrot_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: c, s
       !$acc routine seq
     end function cublasCrot_v2

     integer(c_int) function cublasCrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasCrot_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: c, s
       !$acc routine seq
     end function cublasCrot

     integer(c_int) function cublasZrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasZrot_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: c, s
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZrot_v2

     integer(c_int) function cublasZrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasZrot_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: c, s
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZrot

     integer(c_int) function cublasCsrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasCsrot_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: c, s
       !$acc routine seq
     end function cublasCsrot_v2

     integer(c_int) function cublasCsrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasCsrot_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: c, s
       !$acc routine seq
     end function cublasCsrot

     integer(c_int) function cublasZdrot_v2 (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasZdrot_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: c, s
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdrot_v2

     integer(c_int) function cublasZdrot (handle, n, x, incx, y, incy, c, s) &
         bind (c, name="cublasZdrot_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double) :: c, s
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdrot

     integer(c_int) function cublasSrotg_v2 (handle, a, b, c, s) &
         bind (c, name="cublasSrotg_v2")
       import
       real(c_float) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasSrotg_v2

     integer(c_int) function cublasSrotg (handle, a, b, c, s) &
         bind (c, name="cublasSrotg_v2")
       import
       real(c_float) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasSrotg

     integer(c_int) function cublasDrotg_v2 (handle, a, b, c, s) &
         bind (c, name="cublasDrotg_v2")
       import
       real(c_double) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotg_v2

     integer(c_int) function cublasDrotg (handle, a, b, c, s) &
         bind (c, name="cublasDrotg_v2")
       import
       real(c_double) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDrotg

     integer(c_int) function cublasCrotg_v2 (handle, a, b, c, s) &
         bind (c, name="cublasCrotg_v2")
       import
       type(cublasHandle), value :: handle
       complex(c_float) :: a, b, c, s
       !$acc routine seq
     end function cublasCrotg_v2

     integer(c_int) function cublasCrotg (handle, a, b, c, s) &
         bind (c, name="cublasCrotg_v2")
       import
       type(cublasHandle), value :: handle
       complex(c_float) :: a, b, c, s
       !$acc routine seq
     end function cublasCrotg

     integer(c_int) function cublasZrotg_v2 (handle, a, b, c, s) &
         bind (c, name="cublasZrotg_v2")
       import
       complex(c_double) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZrotg_v2

     integer(c_int) function cublasZrotg (handle, a, b, c, s) &
         bind (c, name="cublasZrotg_v2")
       import
       complex(c_double) :: a, b, c, s
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZrotg

     integer(c_int) function cublasSscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasSscal_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSscal_v2

     integer(c_int) function cublasSscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasSscal_v2")
       import
       real(c_float) :: alpha
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSscal

     integer(c_int) function cublasDscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasDscal_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDscal_v2

     integer(c_int) function cublasDscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasDscal_v2")
       import
       real(c_double), dimension(*) :: x
       integer(c_int), value :: n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDscal

     integer(c_int) function cublasCscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasCscal_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCscal_v2

     integer(c_int) function cublasCscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasCscal_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCscal

     integer(c_int) function cublasZscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasZscal_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZscal_v2

     integer(c_int) function cublasZscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasZscal_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZscal

     integer(c_int) function cublasCsscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasCsscal_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsscal_v2

     integer(c_int) function cublasCsscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasCsscal_v2")
       import
       complex(c_float), dimension(*) :: x
       integer(c_int), value :: n, incx
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsscal

     integer(c_int) function cublasZdscal_v2 (handle, n, alpha, x, incx) &
         bind (c, name="cublasZdscal_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdscal_v2

     integer(c_int) function cublasZdscal (handle, n, alpha, x, incx) &
         bind (c, name="cublasZdscal_v2")
       import
       integer(c_int), value :: n, incx
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZdscal

     integer(c_int) function cublasSswap_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasSswap_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSswap_v2

     integer(c_int) function cublasSswap (handle, n, x, incx, y, incy) &
         bind (c, name="cublasSswap_v2")
       import
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSswap

     integer(c_int) function cublasDswap_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasDswap_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDswap_v2

     integer(c_int) function cublasDswap (handle, n, x, incx, y, incy) &
         bind (c, name="cublasDswap_v2")
       import
       real(c_double), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDswap

     integer(c_int) function cublasCswap_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasCswap_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCswap_v2

     integer(c_int) function cublasCswap (handle, n, x, incx, y, incy) &
         bind (c, name="cublasCswap_v2")
       import
       complex(c_float), dimension(*) :: x, y
       integer(c_int), value :: n, incx, incy
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCswap

     integer(c_int) function cublasZswap_v2 (handle, n, x, incx, y, incy) &
         bind (c, name="cublasZswap_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZswap_v2

     integer(c_int) function cublasZswap (handle, n, x, incx, y, incy) &
         bind (c, name="cublasZswap_v2")
       import
       integer(c_int), value :: n, incx, incy
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZswap

     integer(c_int) function cublasSger_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasSger_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSger_v2

     integer(c_int) function cublasSger (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasSger_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSger

     integer(c_int) function cublasDger_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasDger_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       real(c_double), dimension(*) :: x, y
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDger_v2

     integer(c_int) function cublasDger (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasDger_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       real(c_double), dimension(*) :: x, y
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDger

     integer(c_int) function cublasCgeru_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCgeru_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCgeru_v2

     integer(c_int) function cublasCgeru (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCgeru_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCgeru

     integer(c_int) function cublasZgeru_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZgeru_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgeru_v2

     integer(c_int) function cublasZgeru (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZgeru_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgeru

     integer(c_int) function cublasCgerc_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCgerc_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCgerc_v2

     integer(c_int) function cublasCgerc (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCgerc_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCgerc

     integer(c_int) function cublasZgerc_v2 (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZgerc_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgerc_v2

     integer(c_int) function cublasZgerc (handle, m, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZgerc_v2")
       import
       integer(c_int), value :: m, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZgerc

     integer(c_int) function cublasSsymv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsymv_v2

     integer(c_int) function cublasSsymv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasSsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       real(c_float) :: alpha, beta
       real(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsymv

     integer(c_int) function cublasDsymv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDsymv_v2

     integer(c_int) function cublasDsymv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasDsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x, y
       !$acc routine seq
     end function cublasDsymv

     integer(c_int) function cublasCsymv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsymv_v2

     integer(c_int) function cublasCsymv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasCsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsymv

     integer(c_int) function cublasZsymv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsymv_v2

     integer(c_int) function cublasZsymv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZsymv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsymv

     integer(c_int) function cublasSsyr_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasSsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSsyr_v2

     integer(c_int) function cublasSsyr (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasSsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x
       !$acc routine seq
     end function cublasSsyr

     integer(c_int) function cublasDsyr_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasDsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_double), dimension(*) :: x
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDsyr_v2

     integer(c_int) function cublasDsyr (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasDsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_double), dimension(*) :: x
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDsyr

     integer(c_int) function cublasCsyr_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasCsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsyr_v2

     integer(c_int) function cublasCsyr (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasCsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsyr

     integer(c_int) function cublasZsyr_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasZsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsyr_v2

     integer(c_int) function cublasZsyr (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasZsyr_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsyr

     integer(c_int) function cublasSsyr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasSsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSsyr2_v2

     integer(c_int) function cublasSsyr2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasSsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       real(c_float) :: alpha
       real(c_float), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: x, y
       !$acc routine seq
     end function cublasSsyr2

     integer(c_int) function cublasDsyr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasDsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       real(c_double), dimension(*) :: x, y
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDsyr2_v2

     integer(c_int) function cublasDsyr2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasDsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       real(c_double), dimension(*) :: x, y
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasDsyr2

     integer(c_int) function cublasCsyr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsyr2_v2

     integer(c_int) function cublasCsyr2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCsyr2

     integer(c_int) function cublasZsyr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsyr2_v2

     integer(c_int) function cublasZsyr2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZsyr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZsyr2

     integer(c_int) function cublasStbmv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasStbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStbmv_v2

     integer(c_int) function cublasStbmv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasStbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStbmv

     integer(c_int) function cublasDtbmv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasDtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtbmv_v2

     integer(c_int) function cublasDtbmv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasDtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtbmv

     integer(c_int) function cublasCtbmv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasCtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtbmv_v2

     integer(c_int) function cublasCtbmv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasCtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtbmv

     integer(c_int) function cublasZtbmv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasZtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtbmv_v2

     integer(c_int) function cublasZtbmv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasZtbmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtbmv

     integer(c_int) function cublasStbsv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasStbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStbsv_v2

     integer(c_int) function cublasStbsv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasStbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStbsv

     integer(c_int) function cublasDtbsv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasDtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtbsv_v2

     integer(c_int) function cublasDtbsv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasDtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtbsv

     integer(c_int) function cublasCtbsv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasCtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtbsv_v2

     integer(c_int) function cublasCtbsv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasCtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtbsv

     integer(c_int) function cublasZtbsv_v2 (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasZtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtbsv_v2

     integer(c_int) function cublasZtbsv (handle, uplo, trans, diag, n, k, A, lda, x, incx) &
         bind (c, name="cublasZtbsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, k, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtbsv

     integer(c_int) function cublasStpmv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasStpmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x
       !$acc routine seq
     end function cublasStpmv_v2

     integer(c_int) function cublasStpmv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasStpmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x
       !$acc routine seq
     end function cublasStpmv

     integer(c_int) function cublasDtpmv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasDtpmv_v2")
       import
       real(c_double), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDtpmv_v2

     integer(c_int) function cublasDtpmv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasDtpmv_v2")
       import
       real(c_double), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDtpmv

     integer(c_int) function cublasCtpmv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasCtpmv_v2")
       import
       complex(c_float), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCtpmv_v2

     integer(c_int) function cublasCtpmv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasCtpmv_v2")
       import
       complex(c_float), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCtpmv

     integer(c_int) function cublasZtpmv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasZtpmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       complex(c_double), dimension(*) :: AP, x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtpmv_v2

     integer(c_int) function cublasZtpmv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasZtpmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       complex(c_double), dimension(*) :: AP, x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtpmv

     integer(c_int) function cublasStpsv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasStpsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x
       !$acc routine seq
     end function cublasStpsv_v2

     integer(c_int) function cublasStpsv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasStpsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       real(c_float), dimension(*) :: AP, x
       !$acc routine seq
     end function cublasStpsv

     integer(c_int) function cublasDtpsv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasDtpsv_v2")
       import
       real(c_double), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDtpsv_v2

     integer(c_int) function cublasDtpsv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasDtpsv_v2")
       import
       real(c_double), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDtpsv

     integer(c_int) function cublasCtpsv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasCtpsv_v2")
       import
       complex(c_float), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCtpsv_v2

     integer(c_int) function cublasCtpsv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasCtpsv_v2")
       import
       complex(c_float), dimension(*) :: AP, x
       integer(c_int), value :: uplo, trans, diag, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasCtpsv

     integer(c_int) function cublasZtpsv_v2 (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasZtpsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       complex(c_double), dimension(*) :: AP, x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtpsv_v2

     integer(c_int) function cublasZtpsv (handle, uplo, trans, diag, n, AP, x, incx) &
         bind (c, name="cublasZtpsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, incx
       complex(c_double), dimension(*) :: AP, x
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtpsv

     integer(c_int) function cublasStrmv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasStrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrmv_v2

     integer(c_int) function cublasStrmv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasStrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrmv

     integer(c_int) function cublasDtrmv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasDtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtrmv_v2

     integer(c_int) function cublasDtrmv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasDtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtrmv

     integer(c_int) function cublasCtrmv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasCtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtrmv_v2

     integer(c_int) function cublasCtrmv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasCtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtrmv

     integer(c_int) function cublasZtrmv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasZtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrmv_v2

     integer(c_int) function cublasZtrmv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasZtrmv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrmv

     integer(c_int) function cublasStrsv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasStrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrsv_v2

     integer(c_int) function cublasStrsv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasStrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrsv

     integer(c_int) function cublasDtrsv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasDtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtrsv_v2

     integer(c_int) function cublasDtrsv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasDtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       real(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       real(c_double), dimension(*) :: x
       !$acc routine seq
     end function cublasDtrsv

     integer(c_int) function cublasCtrsv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasCtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtrsv_v2

     integer(c_int) function cublasCtrsv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasCtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCtrsv

     integer(c_int) function cublasZtrsv_v2 (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasZtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrsv_v2

     integer(c_int) function cublasZtrsv (handle, uplo, trans, diag, n, A, lda, x, incx) &
         bind (c, name="cublasZtrsv_v2")
       import
       integer(c_int), value :: uplo, trans, diag, n, lda, incx
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrsv

     integer(c_int) function cublasSgemm_v2 &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgemm_v2

     integer(c_int) function cublasSgemm &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSgemm

     integer(c_int) function cublasDgemm_v2 &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDgemm_v2

     integer(c_int) function cublasDgemm &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDgemm

     integer(c_int) function cublasCgemm_v2 &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemm_v2

     integer(c_int) function cublasCgemm &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemm

     integer(c_int) function cublasZgemm_v2 &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZgemm_v2

     integer(c_int) function cublasZgemm &
         (handle, transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZgemm_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZgemm

     integer(c_int) function cublasSsymm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsymm_v2

     integer(c_int) function cublasSsymm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsymm

     integer(c_int) function cublasDsymm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsymm_v2

     integer(c_int) function cublasDsymm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsymm

     integer(c_int) function cublasCsymm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsymm_v2

     integer(c_int) function cublasCsymm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsymm

     integer(c_int) function cublasZsymm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsymm_v2

     integer(c_int) function cublasZsymm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsymm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsymm

     integer(c_int) function cublasSsyrk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasSsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyrk_v2

     integer(c_int) function cublasSsyrk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasSsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyrk

     integer(c_int) function cublasDsyrk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasDsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_double), dimension(lda, *) :: A
       real(c_double), dimension(ldc, *) :: C
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDsyrk_v2

     integer(c_int) function cublasDsyrk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasDsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_double), dimension(lda, *) :: A
       real(c_double), dimension(ldc, *) :: C
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasDsyrk

     integer(c_int) function cublasCsyrk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasCsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyrk_v2

     integer(c_int) function cublasCsyrk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasCsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyrk

     integer(c_int) function cublasZsyrk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasZsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyrk_v2

     integer(c_int) function cublasZsyrk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasZsyrk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyrk

     integer(c_int) function cublasSsyr2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyr2k_v2

     integer(c_int) function cublasSsyr2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyr2k

     integer(c_int) function cublasDsyr2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsyr2k_v2

     integer(c_int) function cublasDsyr2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsyr2k

     integer(c_int) function cublasCsyr2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyr2k_v2

     integer(c_int) function cublasCsyr2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyr2k

     integer(c_int) function cublasZsyr2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyr2k_v2

     integer(c_int) function cublasZsyr2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsyr2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyr2k

     integer(c_int) function cublasSsyrkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyrkx_v2

     integer(c_int) function cublasSsyrkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasSsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha, beta
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasSsyrkx

     integer(c_int) function cublasDsyrkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsyrkx_v2

     integer(c_int) function cublasDsyrkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasDsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       !$acc routine seq
     end function cublasDsyrkx

     integer(c_int) function cublasCsyrkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyrkx_v2

     integer(c_int) function cublasCsyrkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCsyrkx

     integer(c_int) function cublasZsyrkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyrkx_v2

     integer(c_int) function cublasZsyrkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZsyrkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZsyrkx

     integer(c_int) function cublasStrmm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasStrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrmm_v2

     integer(c_int) function cublasStrmm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasStrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_float) :: alpha
       real(c_float), dimension(ldc, *) :: C
       real(c_float), dimension(ldb, *) :: B
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrmm

     integer(c_int) function cublasDtrmm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasDtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       !$acc routine seq
     end function cublasDtrmm_v2

     integer(c_int) function cublasDtrmm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasDtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       real(c_double), dimension(ldc, *) :: C
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       !$acc routine seq
     end function cublasDtrmm

     integer(c_int) function cublasCtrmm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasCtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrmm_v2

     integer(c_int) function cublasCtrmm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasCtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrmm

     integer(c_int) function cublasZtrmm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasZtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZtrmm_v2

     integer(c_int) function cublasZtrmm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb, C, ldc) &
         bind (c, name="cublasZtrmm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZtrmm

     integer(c_int) function cublasStrsm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasStrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       real(c_float) :: alpha
       real(c_float), dimension(ldb, *) :: B
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrsm_v2

     integer(c_int) function cublasStrsm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasStrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       real(c_float) :: alpha
       real(c_float), dimension(ldb, *) :: B
       type(cublasHandle), value :: handle
       real(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasStrsm

     integer(c_int) function cublasDtrsm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasDtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       !$acc routine seq
     end function cublasDtrsm_v2

     integer(c_int) function cublasDtrsm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasDtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       real(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       real(c_double), dimension(ldb, *) :: B
       !$acc routine seq
     end function cublasDtrsm

     integer(c_int) function cublasCtrsm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasCtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       complex(c_float), dimension(ldb, *) :: B
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrsm_v2

     integer(c_int) function cublasCtrsm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasCtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       complex(c_float), dimension(ldb, *) :: B
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrsm

     integer(c_int) function cublasZtrsm_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasZtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double) :: alpha
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrsm_v2

     integer(c_int) function cublasZtrsm &
         (handle, side, uplo, trans, diag, m, n, alpha, A, lda, B, ldb) &
         bind (c, name="cublasZtrsm_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double) :: alpha
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZtrsm

     integer(c_int) function cublasSgetrfBatched_v2 (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasSgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasSgetrfBatched_v2

     integer(c_int) function cublasSgetrfBatched (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasSgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasSgetrfBatched

     integer(c_int) function cublasDgetrfBatched_v2 (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasDgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasDgetrfBatched_v2

     integer(c_int) function cublasDgetrfBatched (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasDgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasDgetrfBatched

     integer(c_int) function cublasCgetrfBatched_v2 (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasCgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasCgetrfBatched_v2

     integer(c_int) function cublasCgetrfBatched (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasCgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasCgetrfBatched

     integer(c_int) function cublasZgetrfBatched_v2 (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasZgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasZgetrfBatched_v2

     integer(c_int) function cublasZgetrfBatched (handle, n, Aarray, lda, PivotArray, infoArray, batchSize) &
         bind (c, name="cublasZgetrfBatched_v2")
       import
       integer(c_int), value :: n, lda, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray
       !$acc routine seq
     end function cublasZgetrfBatched

     integer(c_int) function cublasSgetrsBatched_v2 &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasSgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasSgetrsBatched_v2

     integer(c_int) function cublasSgetrsBatched &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasSgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasSgetrsBatched

     integer(c_int) function cublasDgetrsBatched_v2 &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasDgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasDgetrsBatched_v2

     integer(c_int) function cublasDgetrsBatched &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasDgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasDgetrsBatched

     integer(c_int) function cublasCgetrsBatched_v2 &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasCgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasCgetrsBatched_v2

     integer(c_int) function cublasCgetrsBatched &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasCgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasCgetrsBatched

     integer(c_int) function cublasZgetrsBatched_v2 &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasZgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasZgetrsBatched_v2

     integer(c_int) function cublasZgetrsBatched &
         (handle, trans, n, nrhs, Aarray, lda, devIpiv, Barray, ldb, info, batchSize) &
         bind (c, name="cublasZgetrsBatched_v2")
       import
       integer(c_int), value :: trans, n, nrhs, lda, ldb, batchSize
       integer(c_int), dimension(*) :: devIpiv, info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasZgetrsBatched

     integer(c_int) function cublasSgetriBatched_v2 &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasSgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasSgetriBatched_v2

     integer(c_int) function cublasSgetriBatched &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasSgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasSgetriBatched

     integer(c_int) function cublasDgetriBatched_v2 &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasDgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasDgetriBatched_v2

     integer(c_int) function cublasDgetriBatched &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasDgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasDgetriBatched

     integer(c_int) function cublasCgetriBatched_v2 &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasCgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasCgetriBatched_v2

     integer(c_int) function cublasCgetriBatched &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasCgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasCgetriBatched

     integer(c_int) function cublasZgetriBatched_v2 &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasZgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasZgetriBatched_v2

     integer(c_int) function cublasZgetriBatched &
         (handle, n, Aarray, lda, PivotArray, Carray, ldc, infoArray, batchSize) &
         bind (c, name="cublasZgetriBatched_v2")
       import
       integer(c_int), value :: n, lda, ldc, batchSize
       integer(c_int), dimension(*) :: PivotArray, infoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasZgetriBatched

     integer(c_int) function cublasSgemmBatched_v2 &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasSgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       real(c_float) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasSgemmBatched_v2

     integer(c_int) function cublasSgemmBatched &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasSgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       real(c_float) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasSgemmBatched

     integer(c_int) function cublasDgemmBatched_v2 &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasDgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasDgemmBatched_v2

     integer(c_int) function cublasDgemmBatched &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasDgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasDgemmBatched

     integer(c_int) function cublasCgemmBatched_v2 &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasCgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemmBatched_v2

     integer(c_int) function cublasCgemmBatched &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasCgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasCgemmBatched

     integer(c_int) function cublasZgemmBatched_v2 &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasZgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       complex(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasZgemmBatched_v2

     integer(c_int) function cublasZgemmBatched &
         (handle, transa, transb, m, n, k, alpha, Aarray, lda, Barray, ldb, beta, Carray, ldc, batchCount) &
         bind (c, name="cublasZgemmBatched_v2")
       import
       integer(c_int), value :: transa, transb, m, n, k, lda, ldb, ldc, batchCount
       complex(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray, Carray
       !$acc routine seq
     end function cublasZgemmBatched

     integer(c_int) function cublasStrsmBatched_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasStrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       real(c_float) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasStrsmBatched_v2

     integer(c_int) function cublasStrsmBatched &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasStrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       real(c_float) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasStrsmBatched

     integer(c_int) function cublasDtrsmBatched_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasDtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasDtrsmBatched_v2

     integer(c_int) function cublasDtrsmBatched &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasDtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasDtrsmBatched

     integer(c_int) function cublasCtrsmBatched_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasCtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       type(c_ptr), dimension(*) :: Aarray, Barray
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrsmBatched_v2

     integer(c_int) function cublasCtrsmBatched &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasCtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       type(c_ptr), dimension(*) :: Aarray, Barray
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCtrsmBatched

     integer(c_int) function cublasZtrsmBatched_v2 &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasZtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       complex(c_double) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasZtrsmBatched_v2

     integer(c_int) function cublasZtrsmBatched &
         (handle, side, uplo, trans, diag, m, n, alpha, Aarray, lda, Barray, ldb, batchCount) &
         bind (c, name="cublasZtrsmBatched_v2")
       import
       integer(c_int), value :: side, uplo, trans, diag, m, n, lda, ldb, batchCount
       complex(c_double) :: alpha
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Barray
       !$acc routine seq
     end function cublasZtrsmBatched

     integer(c_int) function cublasSmatinvBatched_v2 (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasSmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasSmatinvBatched_v2

     integer(c_int) function cublasSmatinvBatched (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasSmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasSmatinvBatched

     integer(c_int) function cublasDmatinvBatched_v2 (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasDmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasDmatinvBatched_v2

     integer(c_int) function cublasDmatinvBatched (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasDmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasDmatinvBatched

     integer(c_int) function cublasCmatinvBatched_v2 (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasCmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasCmatinvBatched_v2

     integer(c_int) function cublasCmatinvBatched (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasCmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasCmatinvBatched

     integer(c_int) function cublasZmatinvBatched_v2 (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasZmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasZmatinvBatched_v2

     integer(c_int) function cublasZmatinvBatched (handle, n, Aarray, lda, Ainv, lda_inv, info, batchSize) &
         bind (c, name="cublasZmatinvBatched_v2")
       import
       integer(c_int), value :: n, lda, lda_inv, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Ainv
       !$acc routine seq
     end function cublasZmatinvBatched

     integer(c_int) function cublasSgeqrfBatched_v2 (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasSgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasSgeqrfBatched_v2

     integer(c_int) function cublasSgeqrfBatched (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasSgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasSgeqrfBatched

     integer(c_int) function cublasDgeqrfBatched_v2 (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasDgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasDgeqrfBatched_v2

     integer(c_int) function cublasDgeqrfBatched (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasDgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasDgeqrfBatched

     integer(c_int) function cublasCgeqrfBatched_v2 (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasCgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasCgeqrfBatched_v2

     integer(c_int) function cublasCgeqrfBatched (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasCgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasCgeqrfBatched

     integer(c_int) function cublasZgeqrfBatched_v2 (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasZgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasZgeqrfBatched_v2

     integer(c_int) function cublasZgeqrfBatched (handle, m, n, Aarray, lda, TauArray, info, batchSize) &
         bind (c, name="cublasZgeqrfBatched_v2")
       import
       integer(c_int), value :: m, n, lda, batchSize
       integer(c_int), dimension(*) :: info
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, TauArray
       !$acc routine seq
     end function cublasZgeqrfBatched

     integer(c_int) function cublasSgelsBatched_v2 &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasSgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasSgelsBatched_v2

     integer(c_int) function cublasSgelsBatched &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasSgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasSgelsBatched

     integer(c_int) function cublasDgelsBatched_v2 &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasDgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasDgelsBatched_v2

     integer(c_int) function cublasDgelsBatched &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasDgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasDgelsBatched

     integer(c_int) function cublasCgelsBatched_v2 &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasCgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasCgelsBatched_v2

     integer(c_int) function cublasCgelsBatched &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasCgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasCgelsBatched

     integer(c_int) function cublasZgelsBatched_v2 &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasZgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasZgelsBatched_v2

     integer(c_int) function cublasZgelsBatched &
         (handle, trans, m, n, nrhs, Aarray, lda, Carray, ldc, info, devInfoArray, batchSize) &
         bind (c, name="cublasZgelsBatched_v2")
       import
       integer(c_int), value :: trans, m, n, nrhs, lda, ldc, batchSize
       integer(c_int), dimension(*) :: info, devInfoArray
       type(cublasHandle), value :: handle
       type(c_ptr), dimension(*) :: Aarray, Carray
       !$acc routine seq
     end function cublasZgelsBatched

     integer(c_int) function cublasChbmv_v2 (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasChbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChbmv_v2

     integer(c_int) function cublasChbmv (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasChbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChbmv

     integer(c_int) function cublasZhbmv_v2 (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZhbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhbmv_v2

     integer(c_int) function cublasZhbmv (handle, uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZhbmv_v2")
       import
       integer(c_int), value :: uplo, n, k, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhbmv

     integer(c_int) function cublasChemv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasChemv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChemv_v2

     integer(c_int) function cublasChemv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasChemv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChemv

     integer(c_int) function cublasZhemv_v2 (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZhemv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhemv_v2

     integer(c_int) function cublasZhemv (handle, uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
         bind (c, name="cublasZhemv_v2")
       import
       integer(c_int), value :: uplo, n, lda, incx, incy
       complex(c_double), dimension(*) :: x, y
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(lda, *) :: A
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhemv

     integer(c_int) function cublasChpmv_v2 (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasChpmv_v2")
       import
       complex(c_float), dimension(*) :: AP, x, y
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChpmv_v2

     integer(c_int) function cublasChpmv (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasChpmv_v2")
       import
       complex(c_float), dimension(*) :: AP, x, y
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChpmv

     integer(c_int) function cublasZhpmv_v2 (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasZhpmv_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(*) :: AP, x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpmv_v2

     integer(c_int) function cublasZhpmv (handle, uplo, n, alpha, AP, x, incx, beta, y, incy) &
         bind (c, name="cublasZhpmv_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(*) :: AP, x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpmv

     integer(c_int) function cublasCher_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasCher_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_float) :: alpha
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCher_v2

     integer(c_int) function cublasCher (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasCher_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       real(c_float) :: alpha
       complex(c_float), dimension(*) :: x
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCher

     integer(c_int) function cublasZher_v2 (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasZher_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZher_v2

     integer(c_int) function cublasZher (handle, uplo, n, alpha, x, incx, A, lda) &
         bind (c, name="cublasZher_v2")
       import
       integer(c_int), value :: uplo, n, incx, lda
       complex(c_double), dimension(*) :: x
       complex(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZher

     integer(c_int) function cublasCher2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCher2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCher2_v2

     integer(c_int) function cublasCher2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasCher2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_float), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCher2

     integer(c_int) function cublasZher2_v2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZher2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZher2_v2

     integer(c_int) function cublasZher2 (handle, uplo, n, alpha, x, incx, y, incy, A, lda) &
         bind (c, name="cublasZher2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy, lda
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZher2

     integer(c_int) function cublasChpr_v2 (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasChpr_v2")
       import
       real(c_float) :: alpha
       complex(c_float), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasChpr_v2

     integer(c_int) function cublasChpr (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasChpr_v2")
       import
       real(c_float) :: alpha
       complex(c_float), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasChpr

     integer(c_int) function cublasZhpr_v2 (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasZhpr_v2")
       import
       complex(c_double), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpr_v2

     integer(c_int) function cublasZhpr (handle, uplo, n, alpha, x, incx, AP) &
         bind (c, name="cublasZhpr_v2")
       import
       complex(c_double), dimension(*) :: x, AP
       integer(c_int), value :: uplo, n, incx
       real(c_double) :: alpha
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpr

     integer(c_int) function cublasChpr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasChpr2_v2")
       import
       complex(c_float), dimension(*) :: x, y, AP
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasChpr2_v2

     integer(c_int) function cublasChpr2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasChpr2_v2")
       import
       complex(c_float), dimension(*) :: x, y, AP
       integer(c_int), value :: uplo, n, incx, incy
       type(cublasHandle), value :: handle
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasChpr2

     integer(c_int) function cublasZhpr2_v2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasZhpr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y, AP
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpr2_v2

     integer(c_int) function cublasZhpr2 (handle, uplo, n, alpha, x, incx, y, incy, AP) &
         bind (c, name="cublasZhpr2_v2")
       import
       integer(c_int), value :: uplo, n, incx, incy
       complex(c_double) :: alpha
       complex(c_double), dimension(*) :: x, y, AP
       type(cublasHandle), value :: handle
       !$acc routine seq
     end function cublasZhpr2

     integer(c_int) function cublasChemm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasChemm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChemm_v2

     integer(c_int) function cublasChemm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasChemm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha, beta
       !$acc routine seq
     end function cublasChemm

     integer(c_int) function cublasZhemm_v2 &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZhemm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZhemm_v2

     integer(c_int) function cublasZhemm &
         (handle, side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZhemm_v2")
       import
       integer(c_int), value :: side, uplo, m, n, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha, beta
       complex(c_double), dimension(ldb, *) :: B
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZhemm

     integer(c_int) function cublasCherk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasCherk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_float) :: alpha, beta
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCherk_v2

     integer(c_int) function cublasCherk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasCherk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       real(c_float) :: alpha, beta
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       complex(c_float), dimension(lda, *) :: A
       !$acc routine seq
     end function cublasCherk

     integer(c_int) function cublasZherk_v2 (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasZherk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZherk_v2

     integer(c_int) function cublasZherk (handle, uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
         bind (c, name="cublasZherk_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldc
       complex(c_double), dimension(lda, *) :: A
       real(c_double) :: alpha, beta
       type(cublasHandle), value :: handle
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZherk

     integer(c_int) function cublasCher2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCher2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float) :: beta
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCher2k_v2

     integer(c_int) function cublasCher2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCher2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float) :: beta
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCher2k

     integer(c_int) function cublasZher2k_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZher2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       real(c_double) :: beta
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZher2k_v2

     integer(c_int) function cublasZher2k &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZher2k_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       real(c_double) :: beta
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZher2k

     integer(c_int) function cublasCherkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCherkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float) :: beta
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCherkx_v2

     integer(c_int) function cublasCherkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasCherkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       complex(c_float), dimension(ldc, *) :: C
       type(cublasHandle), value :: handle
       real(c_float) :: beta
       complex(c_float), dimension(ldb, *) :: B
       complex(c_float), dimension(lda, *) :: A
       complex(c_float) :: alpha
       !$acc routine seq
     end function cublasCherkx

     integer(c_int) function cublasZherkx_v2 &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZherkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       real(c_double) :: beta
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZherkx_v2

     integer(c_int) function cublasZherkx &
         (handle, uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
         bind (c, name="cublasZherkx_v2")
       import
       integer(c_int), value :: uplo, trans, n, k, lda, ldb, ldc
       type(cublasHandle), value :: handle
       complex(c_double), dimension(lda, *) :: A
       complex(c_double) :: alpha
       complex(c_double), dimension(ldb, *) :: B
       real(c_double) :: beta
       complex(c_double), dimension(ldc, *) :: C
       !$acc routine seq
     end function cublasZherkx

  end interface
end module
