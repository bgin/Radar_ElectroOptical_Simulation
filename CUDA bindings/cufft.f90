!!  Fortran CUDA Library interface -- CUFFT module
!!
!!  Copyright (C) 2017-2018 Mentor, A Siemens Business
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

module cufft
  use iso_c_binding

  integer, parameter :: CUFFT_FORWARD = -1
  integer, parameter :: CUFFT_INVERSE = 1

  ! CUFFT Status
  enum, bind(C)
    enumerator :: CUFFT_SUCCESS = 0
    enumerator :: CUFFT_INVALID_PLAN = 1
    enumerator :: CUFFT_ALLOC_FAILED = 2
    enumerator :: CUFFT_INVALID_TYPE = 3
    enumerator :: CUFFT_INVALID_VALUE = 4
    enumerator :: CUFFT_INTERNAL_ERROR = 5
    enumerator :: CUFFT_EXEC_FAILED = 6
    enumerator :: CUFFT_SETUP_FAILED = 7
    enumerator :: CUFFT_INVALID_SIZE = 8
    enumerator :: CUFFT_UNALIGNED_DATA = 9
  end enum

  ! CUFFT Transform Types
  enum, bind(C)
    enumerator :: CUFFT_R2C = int(z'2a') ! Real to Complex (interleaved)
    enumerator :: CUFFT_C2R = int(z'2c') ! Complex (interleaved) to Real
    enumerator :: CUFFT_C2C = int(z'29') ! Complex to Complex, interleaved
    enumerator :: CUFFT_D2Z = int(z'6a') ! Double to Double-Complex
    enumerator :: CUFFT_Z2D = int(z'6c') ! Double-Complex to Double
    enumerator :: CUFFT_Z2Z = int(z'69') ! Double-Complex to Double-Complex
  end enum

  ! CUFFT Data Layouts
  enum, bind(C)
    enumerator :: CUFFT_COMPATIBILITY_NATIVE = 0
    enumerator :: CUFFT_COMPATIBILITY_FFTW_PADDING = 1
    enumerator :: CUFFT_COMPATIBILITY_FFTW_ASYMMETRIC = 2
    enumerator :: CUFFT_COMPATIBILITY_FFTW_ALL = 3
  end enum

  integer, parameter :: CUFFT_COMPATIBILITY_DEFAULT = CUFFT_COMPATIBILITY_FFTW_PADDING

  interface
     integer(c_int) function cufftSetCompatibilityMode (plan, mode) &
         bind (c, name="cufftSetCompatibilityMode")
       import
       integer(c_int), value :: plan, mode
     end function cufftSetCompatibilityMode

     integer(c_int) function cufftSetStream (plan, stream) &
         bind (c, name="cufftSetStream")
       import
       integer(c_int), value :: plan
       type(c_ptr), value :: stream
     end function cufftSetStream

     integer(c_int) function cufftGetVersion (version) &
         bind (c, name="cufftGetVersion")
       import
       integer(c_int) :: version
     end function cufftGetVersion

     integer(c_int) function cufftSetAutoAllocation (plan, autoAllocate) &
         bind (c, name="cufftSetAutoAllocation")
       import
       integer(c_int), value :: plan, autoAllocate
     end function cufftSetAutoAllocation

     integer(c_int) function cufftSetWorkArea (plan, workArea) &
         bind (c, name="cufftSetWorkArea")
       import
       integer(c_int), value :: plan
       integer(c_signed_char), dimension(*) :: workArea
     end function cufftSetWorkArea

     integer(c_int) function cufftDestroy (plan) &
         bind (c, name="cufftDestroy")
       import
       integer(c_int), value :: plan
     end function cufftDestroy

     integer(c_int) function cufftPlan1d (plan, nx, type, batch) &
         bind (c, name="cufftPlan1d")
       import
       integer(c_int), value :: nx, type, batch
       integer(c_int) :: plan
     end function cufftPlan1d

     integer(c_int) function cufftPlan2d (plan, nx, ny, type) &
         bind (c, name="cufftPlan2d")
       import
       integer(c_int), value :: nx, ny, type
       integer(c_int) :: plan
     end function cufftPlan2d

     integer(c_int) function cufftPlan3d (plan, nx, ny, nz, type) &
         bind (c, name="cufftPlan3d")
       import
       integer(c_int), value :: nx, ny, nz, type
       integer(c_int) :: plan
     end function cufftPlan3d

     integer(c_int) function cufftPlanMany &
         (plan, rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch) &
         bind (c, name="cufftPlanMany")
       import
       integer(c_int), value :: rank, istride, idist, ostride, odist, type, batch
       integer(c_int), dimension(*) :: n, inembed, onembed
       integer(c_int) :: plan
     end function cufftPlanMany

     integer(c_int) function cufftCreate (plan) &
         bind (c, name="cufftCreate")
       import
       integer(c_int) :: plan
     end function cufftCreate

     integer(c_int) function cufftMakePlan1d (plan, nx, type, batch, workSize) &
         bind (c, name="cufftMakePlan1d")
       import
       integer(c_int), value :: plan, nx, type, batch
       integer(c_size_t) :: workSize
     end function cufftMakePlan1d

     integer(c_int) function cufftMakePlan2d (plan, nx, ny, type, workSize) &
         bind (c, name="cufftMakePlan2d")
       import
       integer(c_int), value :: plan, nx, ny, type
       integer(c_size_t) :: workSize
     end function cufftMakePlan2d

     integer(c_int) function cufftMakePlan3d (plan, nx, ny, nz, type, workSize) &
         bind (c, name="cufftMakePlan3d")
       import
       integer(c_int), value :: plan, nx, ny, nz, type
       integer(c_size_t) :: workSize
     end function cufftMakePlan3d

     integer(c_int) function cufftMakePlanMany &
         (plan, rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch, workSize) &
         bind (c, name="cufftMakePlanMany")
       import
       integer(c_int), value :: plan, rank, istride, idist, ostride, odist, type, batch
       integer(c_int), dimension(*) :: n, inembed, onembed
       integer(c_size_t) :: workSize
     end function cufftMakePlanMany

     integer(c_int) function cufftMakePlanMany64 &
         (plan, rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch, workSize) &
         bind (c, name="cufftMakePlanMany64")
       import
       integer(c_long_long), value :: istride, idist, ostride, odist, batch
       integer(c_long_long), dimension(*) :: n, inembed, onembed
       integer(c_int), value :: plan, rank, type
       integer(c_size_t) :: workSize
     end function cufftMakePlanMany64

     integer(c_int) function cufftEstimate1d (nx, type, batch, workSize) &
         bind (c, name="cufftEstimate1d")
       import
       integer(c_int), value :: nx, type, batch
       integer(c_size_t) :: workSize
     end function cufftEstimate1d

     integer(c_int) function cufftEstimate2d (nx, ny, type, workSize) &
         bind (c, name="cufftEstimate2d")
       import
       integer(c_int), value :: nx, ny, type
       integer(c_size_t) :: workSize
     end function cufftEstimate2d

     integer(c_int) function cufftEstimate3d (nx, ny, nz, type, workSize) &
         bind (c, name="cufftEstimate3d")
       import
       integer(c_int), value :: nx, ny, nz, type
       integer(c_size_t) :: workSize
     end function cufftEstimate3d

     integer(c_int) function cufftEstimateMany &
         (rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch, workSize) &
         bind (c, name="cufftEstimateMany")
       import
       integer(c_int), value :: rank, istride, idist, ostride, odist, type, batch
       integer(c_size_t) :: workSize
       integer(c_int) :: n, inembed, onembed
     end function cufftEstimateMany

     integer(c_int) function cufftGetSize1d (plan, nx, type, batch, workSize) &
         bind (c, name="cufftGetSize1d")
       import
       integer(c_int), value :: plan, nx, type, batch
       integer(c_size_t) :: workSize
     end function cufftGetSize1d

     integer(c_int) function cufftGetSize2d (plan, nx, ny, type, workSize) &
         bind (c, name="cufftGetSize2d")
       import
       integer(c_int), value :: plan, nx, ny, type
       integer(c_size_t) :: workSize
     end function cufftGetSize2d

     integer(c_int) function cufftGetSize3d (plan, nx, ny, nz, type, workSize) &
         bind (c, name="cufftGetSize3d")
       import
       integer(c_int), value :: plan, nx, ny, nz, type
       integer(c_size_t) :: workSize
     end function cufftGetSize3d

     integer(c_int) function cufftGetSizeMany &
         (plan, rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch, workSize) &
         bind (c, name="cufftGetSizeMany")
       import
       integer(c_int), value :: plan, rank, istride, idist, ostride, odist, type, batch
       integer(c_int), dimension(*) :: n, inembed, onembed
       integer(c_size_t) :: workSize
     end function cufftGetSizeMany

     integer(c_int) function cufftGetSizeMany64 &
         (plan, rank, n, inembed, istride, idist, onembed, ostride, odist, type, batch, workSize) &
         bind (c, name="cufftGetSizeMany64")
       import
       integer(c_long_long), value :: istride, idist, ostride, odist, batch
       integer(c_long_long), dimension(*) :: n, inembed, onembed
       integer(c_int), value :: plan, rank, type
       integer(c_size_t) :: workSize
     end function cufftGetSizeMany64

     integer(c_int) function cufftGetSize (plan, workSize) &
         bind (c, name="cufftGetSize")
       import
       integer(c_int), value :: plan
       integer(c_size_t) :: workSize
     end function cufftGetSize

     integer(c_int) function cufftExecC2C (plan, idata, odata, direction) &
         bind (c, name="cufftExecC2C")
       import
       complex(c_float), dimension(*) :: idata, odata
       integer(c_int), value :: plan, direction
     end function cufftExecC2C

     integer(c_int) function cufftExecR2C (plan, idata, odata) &
         bind (c, name="cufftExecR2C")
       import
       complex(c_float), dimension(*) :: odata
       integer(c_int), value :: plan
       real(c_float), dimension(*) :: idata
     end function cufftExecR2C

     integer(c_int) function cufftExecC2R (plan, idata, odata) &
         bind (c, name="cufftExecC2R")
       import
       complex(c_float), dimension(*) :: idata
       integer(c_int), value :: plan
       real(c_float), dimension(*) :: odata
     end function cufftExecC2R

     integer(c_int) function cufftExecZ2Z (plan, idata, odata, direction) &
         bind (c, name="cufftExecZ2Z")
       import
       integer(c_int), value :: plan, direction
       complex(c_double), dimension(*) :: idata, odata
     end function cufftExecZ2Z

     integer(c_int) function cufftExecD2Z (plan, idata, odata) &
         bind (c, name="cufftExecD2Z")
       import
       real(c_double), dimension(*) :: idata
       integer(c_int), value :: plan
       complex(c_double), dimension(*) :: odata
     end function cufftExecD2Z

     integer(c_int) function cufftExecZ2D (plan, idata, odata) &
         bind (c, name="cufftExecZ2D")
       import
       complex(c_float), dimension(*) :: idata
       integer(c_int), value :: plan
       real(c_float), dimension(*) :: odata
     end function cufftExecZ2D

  end interface
end module
