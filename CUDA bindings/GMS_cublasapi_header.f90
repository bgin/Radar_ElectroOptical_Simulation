
module mod_cublasapi_header

   

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cublasapi_header'
 !          
 !          Purpose:
 !                    
 !                        !=============================================
 !                        ! Fortran interfaces to cublas_api.h header
 !                        !=============================================
 !                     
 !          History:
 !                        Date: 01-26-2019
 !                        Time: 10:54 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use, intrinsic :: ISO_C_BINDING
    use mod_cuda_types
    
    implicit none
    
    !  /* CUBLAS status type returns */ 
    
    ! enum cublasStatus_t
    enum, bind(c)
       enumerator ::   CUBLAS_STATUS_SUCCESS          = 0
       enumerator ::   CUBLAS_STATUS_NOT_INITIALIZED  = 1
       enumerator ::   CUBLAS_STATUS_ALLOC_FAILED     = 3
       enumerator ::   CUBLAS_STATUS_INVALID_VALUE    = 7
       enumerator ::   CUBLAS_STATUS_ARCH_MISMATCH    = 8
       enumerator ::   CUBLAS_STATUS_MAPPING_ERROR    = 11
       enumerator ::   CUBLAS_STATUS_EXECUTION_FAILED = 13
       enumerator ::   CUBLAS_STATUS_INTERNAL_ERROR   = 14
    end enum
    
    ! enum cublasFillMode_t
    enum, bind(c)
       enumerator ::  CUBLAS_FILL_MODE_LOWER = 0
       enumerator ::  CUBLAS_FILL_MODE_UPPER = 1
    end enum
    
    ! enum cublasDiagType_t
    enum, bind(c)
       enumerator ::  CUBLAS_DIAG_NON_UNIT = 0
       enumerator ::  CUBLAS_DIAG_UNIT     = 1
    end enum
    
    ! enum  cublasSideMode_t
    enum, bind(c)
       enumerator ::  CUBLAS_SIDE_LEFT = 0 
       enumerator ::  CUBLAS_SIDE_RIGHT = 1 
    end enum
    
    ! enum cublasOperation_t
    enum, bind(c)
       enumerator ::  CUBLAS_OP_N = 0  
       enumerator ::  CUBLAS_OP_T = 1  
       enumerator ::  CUBLAS_OP_C = 2  
    end enum
    
    ! enum  cublasPointerMode_t
    enum, bind(c)
       enumerator ::  CUBLAS_POINTER_MODE_HOST   = 0  
       enumerator ::  CUBLAS_POINTER_MODE_DEVICE = 1   
    end enum
    
    ! enum cublasAtomicsMode_t
    enum, bind(c)
        enumerator :: CUBLAS_ATOMICS_NOT_ALLOWED   = 0  
        enumerator :: CUBLAS_ATOMICS_ALLOWED       = 1  
    end enum 
    
    interface
        function cublasCreate_v2(handle) result(cublas_status)  &
                    bind(c,name='cublasCreate_v2')
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext
                 type(cublasContext) ::  handle
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCreate_v2
    end interface
    
    interface
        function cublasDestroy_v2(handle) result(cublas_status) &
                        bind(c,name='cublasDestroy_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext  
                  type(cublasContext), value :: handle
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDestroy_v2
    end interface
    
    !/* 
        !* cublasStatus_t 
        !* cublasSetVector (int n, int elemSize, const void *x, int incx, 
        !*                  void *y, int incy) 
        !*
        !* copies n elements from a vector x in CPU memory space to a vector y 
        !* in GPU memory space. Elements in both vectors are assumed to have a 
        !* size of elemSize bytes. Storage spacing between consecutive elements
        !* is incx for the source vector x and incy for the destination vector
        !* y. In general, y points to an object, or part of an object, allocated
        !* via cublasAlloc(). Column major format for two-dimensional matrices
        !* is assumed throughout CUBLAS. Therefore, if the increment for a vector 
        !* is equal to 1, this access a column vector while using an increment 
        !* equal to the leading dimension of the respective matrix accesses a 
       ! * row vector.
        !*
        !* Return Values
        !* -------------
        !* CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if incx, incy, or elemSize <= 0
        !* CUBLAS_STATUS_MAPPING_ERROR    if an error occurred accessing GPU memory   
        !* CUBLAS_STATUS_SUCCESS          if the operation completed successfully
    !*/
    
    interface
        function cublasSetVector(n,elemSize,x,incx,devicePtr,incy) result(cublas_status) &
                        bind(c,name='cublasSetVector')
                     use,intrinsic :: ISO_C_BINDING
                     import :: CUBLAS_STATUS_SUCCESS
                     integer(c_int), value :: n
                     integer(c_int), value :: elemSize
                     type(c_ptr),    value :: x
                     integer(c_int), value :: incx
                     type(c_ptr),    value :: devicePtr
                     integer(c_int), value ::incy
                     integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSetVector
    end interface
    
    !/* 
        !* cublasStatus_t 
        !* cublasGetVector (int n, int elemSize, const void *x, int incx, 
        !*                  void *y, int incy)
        !* 
        !* copies n elements from a vector x in GPU memory space to a vector y 
        !* in CPU memory space. Elements in both vectors are assumed to have a 
        !* size of elemSize bytes. Storage spacing between consecutive elements
        !* is incx for the source vector x and incy for the destination vector
        !* y. In general, x points to an object, or part of an object, allocated
        !* via cublasAlloc(). Column major format for two-dimensional matrices
       ! * is assumed throughout CUBLAS. Therefore, if the increment for a vector 
       ! * is equal to 1, this access a column vector while using an increment 
       ! * equal to the leading dimension of the respective matrix accesses a 
       ! * row vector.
       ! *
        !* Return Values
       ! * -------------
       ! * CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if incx, incy, or elemSize <= 0
        !* CUBLAS_STATUS_MAPPING_ERROR    if an error occurred accessing GPU memory   
       ! * CUBLAS_STATUS_SUCCESS          if the operation completed successfully
    !*/
    
    interface
        function cublasGetVector(n,elemSize,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasGetVector')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: n
                  integer(c_int), value ::elemSize
                  type(c_ptr),    value :: x
                  integer(c_int), value :: incx
                  type(c_ptr),    value :: y
                  integer(c_int), value :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasGetVector
    end interface
    
    !/*
        !* cublasStatus_t 
        !* cublasSetMatrix (int rows, int cols, int elemSize, const void *A, 
        !*                  int lda, void *B, int ldb)
        !*
        !* copies a tile of rows x cols elements from a matrix A in CPU memory
        !* space to a matrix B in GPU memory space. Each element requires storage
        !* of elemSize bytes. Both matrices are assumed to be stored in column 
        !* major format, with the leading dimension (i.e. number of rows) of 
        !* source matrix A provided in lda, and the leading dimension of matrix B
        !* provided in ldb. In general, B points to an object, or part of an 
        !* object, that was allocated via cublasAlloc().
        !*
        !* Return Values 
        !* -------------
        !* CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library has not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if rows or cols < 0, or elemSize, lda, or 
        !*                                ldb <= 0
        !* CUBLAS_STATUS_MAPPING_ERROR    if error occurred accessing GPU memory
        !* CUBLAS_STATUS_SUCCESS          if the operation completed successfully
    !*/
    
    interface
        function cublasSetMatrix(rows,cols,elemSize,A,lda,B,ldb) result(cublas_status) &
                    bind(c,name='cublasSetMatrix')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: rows
                  integer(c_int), value :: cols
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: A
                  integer(c_int), value :: lda
                  type(c_ptr),    value :: B
                  integer(c_int), value :: ldb
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSetMatrix
    end interface
    
    !/*
        !* cublasStatus_t 
        !* cublasGetMatrix (int rows, int cols, int elemSize, const void *A, 
        !*                  int lda, void *B, int ldb)
        !*
        !* copies a tile of rows x cols elements from a matrix A in GPU memory
        !* space to a matrix B in CPU memory space. Each element requires storage
        !* of elemSize bytes. Both matrices are assumed to be stored in column 
        !* major format, with the leading dimension (i.e. number of rows) of 
        !* source matrix A provided in lda, and the leading dimension of matrix B
       ! * provided in ldb. In general, A points to an object, or part of an 
        ! * object, that was allocated via cublasAlloc().
        !*
        !* Return Values 
        !* -------------
        !* CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library has not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if rows, cols, eleSize, lda, or ldb <= 0
        !* CUBLAS_STATUS_MAPPING_ERROR    if error occurred accessing GPU memory
        !* CUBLAS_STATUS_SUCCESS          if the operation completed successfully
    !*/
    
    interface
        function cublasGetMatrix(rows,cols,elemSize,A,lda,B,ldb) result(cublas_status) &
                    bind(c,name='cublasGetMatrix')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: rows
                  integer(c_int), value :: cols
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: A
                  integer(c_int), value :: lda
                  type(c_ptr),    value :: B
                  integer(c_int), value :: ldb
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasGetMatrix
    end interface
    
    !/* 
        !* cublasStatus 
        !* cublasSetVectorAsync ( int n, int elemSize, const void *x, int incx, 
        !*                       void *y, int incy, cudaStream_t stream );
        !*
       ! * cublasSetVectorAsync has the same functionnality as cublasSetVector
        !* but the transfer is done asynchronously within the CUDA stream passed
        !* in parameter.
        !*
        !* Return Values
        !* -------------
        !* CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if incx, incy, or elemSize <= 0
       ! * CUBLAS_STATUS_MAPPING_ERROR    if an error occurred accessing GPU memory   
       ! * CUBLAS_STATUS_SUCCESS          if the operation completed successfully
   ! */
    
    interface
        function cublasSetVectorAsync(n,elemSize,hostPtr,incx,devPtr,incy,stream) result(cublas_status) &
                    bind(c,name='cublasSetVectorAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: n
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: hostPtr
                  integer(c_int), value :: incx
                  type(c_ptr),    value :: devPtr
                  integer(c_int), value :: incy
                  type(CUstream), value :: stream
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSetVectorAsync
    end interface
    
    !/* 
        !* cublasStatus 
        !* cublasGetVectorAsync( int n, int elemSize, const void *x, int incx, 
        !*                       void *y, int incy, cudaStream_t stream)
        !* 
        !* cublasGetVectorAsync has the same functionnality as cublasGetVector
        !* but the transfer is done asynchronously within the CUDA stream passed
        !* in parameter.
        !*
       ! * Return Values
       ! * -------------
       ! * CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library not been initialized
       ! * CUBLAS_STATUS_INVALID_VALUE    if incx, incy, or elemSize <= 0
       ! * CUBLAS_STATUS_MAPPING_ERROR    if an error occurred accessing GPU memory   
       ! * CUBLAS_STATUS_SUCCESS          if the operation completed successfully
    !*/
    
    interface
        function cublasGetVectorAsync(n,elemSize,devPtr,incx,hostPtr,incy,stream) result(cublas_status) &
                    bind(c,name='cublasGetVectorAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: n
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: devPtr
                  integer(c_int), value :: incx
                  type(c_ptr),    value :: hostPtr
                  integer(c_int), value :: incy
                  type(CUstream), value :: stream
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasGetVectorAsync
    end interface
    
    !/*
        !* cublasStatus_t 
        !*   cublasSetMatrixAsync (int rows, int cols, int elemSize, const void *A, 
        !*                       int lda, void *B, int ldb, cudaStream_t stream)
        !*
        !* cublasSetMatrixAsync has the same functionnality as cublasSetMatrix
       ! * but the transfer is done asynchronously within the CUDA stream passed
        !* in parameter.
        !*
        !* Return Values 
       ! * -------------
        ! * CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library has not been initialized
        !* CUBLAS_STATUS_INVALID_VALUE    if rows or cols < 0, or elemSize, lda, or 
        !*                                ldb <= 0
        !* CUBLAS_STATUS_MAPPING_ERROR    if error occurred accessing GPU memory
        !* CUBLAS_STATUS_SUCCESS          if the operation completed successfully
   ! */
    
    interface
        function cublasSetMatrixAsync(rows,cols,elemSize,A,lda,B,ldb,stream) result(cublas_status) &
                    bind(c,name='cublasSetMatrixAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: rows
                  integer(c_int), value :: cols
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: A
                  integer(c_int), value :: lda
                  type(c_ptr),    value :: B
                  integer(c_int), value :: ldb
                  type(CUstream), value :: stream
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSetMatrixAsync
    end interface
    
    !/*
        !* cublasStatus_t 
        !* cublasGetMatrixAsync (int rows, int cols, int elemSize, const void *A, 
        !*                       int lda, void *B, int ldb, cudaStream_t stream)
        !*
        !* cublasGetMatrixAsync has the same functionnality as cublasGetMatrix
        !* but the transfer is done asynchronously within the CUDA stream passed
        !* in parameter.
       ! *
       ! * Return Values 
       ! * -------------
       ! * CUBLAS_STATUS_NOT_INITIALIZED  if CUBLAS library has not been initialized
       ! * CUBLAS_STATUS_INVALID_VALUE    if rows, cols, eleSize, lda, or ldb <= 0
       ! * CUBLAS_STATUS_MAPPING_ERROR    if error occurred accessing GPU memory
       ! * CUBLAS_STATUS_SUCCESS          if the operation completed successfully
   ! */
    
    interface
        function cublasGetMatrixAsync(rows,cols,elemSize,A,lda,B,ldb,stream) result(cublas_status) &
                    bind(c,name='cublasGetMatrixAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUBLAS_STATUS_SUCCESS
                  integer(c_int), value :: rows
                  integer(c_int), value :: cols
                  integer(c_int), value :: elemSize
                  type(c_ptr),    value :: A
                  integer(c_int), value :: lda
                  type(c_ptr),    value :: B
                  integer(c_int), value :: ldb
                  type(CUstream), value :: stream
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasGetMatrixAsync
    end interface
    
    interface
        function cublasXerbla(srName,info) &
                    bind(c,name='cublasXerbla')
                  use, intrinsic :: ISO_C_BINDING
                  character(c_char)       :: srName
                  integer(c_int), value   :: info
        end function cublasXerbla
    end interface
    
    !    /* ---------------- CUBLAS BLAS1 functions ---------------- */
    
    interface
        function cublasSnrm2_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasSnrm2_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext), value  :: handle
                 integer(c_int),      value  :: n
                 real(c_float), dimension(*) :: x
                 integer(c_int),      value  :: incx
                 real(c_float)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSnrm2_v2
    end interface
    
    interface
        function cublasDnrm2_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasDnrm2_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext), value   :: handle
                 integer(c_int),      value   :: n
                 real(c_double), dimension(*) :: x
                 integer(c_int),      value   :: incx
                 real(c_double)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDnrm2_v2
    end interface
    
    interface
        function cublasScnrm2_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasScnrm2_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext), value     :: handle
                 integer(c_int),      value     :: n
                 complex(c_float), dimension(*) :: x
                 integer(c_int),      value     :: incx
                 real(c_float)                  :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasScnrm2_v2
    end interface
    
    interface
        function cublasDznrm2_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasDznrm2_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext  
                 type(cublasContext), value      :: handle
                 integer(c_int),      value      :: n
                 complex(c_double), dimension(*) :: x
                 integer(c_int),      value      :: incx
                 real(c_double)                  :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDznrm2_v2
    end interface
    
    interface
        function cublasSdot_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasSdot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext), value  :: handle
                 integer(c_int),      value  :: n
                 real(c_float), dimension(*) :: x
                 integer(c_int),      value  :: incx
                 real(c_float), dimension(*) :: y
                 integer(c_int),      value  :: incy
                 real(c_float)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSdot_v2
    end interface
    
    interface
        function cublasDdot_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasDdot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),  value  :: handle
                 integer(c_int),       value  :: n
                 real(c_double), dimension(*) :: x
                 integer(c_int),       value  :: incx
                 real(c_double), dimension(*) :: y
                 integer(c_int),       value  :: incy
                 real(c_double)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDdot_v2
    end interface
    
    interface
        function cublasCdotu_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasCdotu_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value   :: handle
                 integer(c_int),        value   :: n
                 complex(c_float), dimension(*) :: x
                 integer(c_int),        value   :: incx
                 complex(c_float), dimension(*) :: y
                 integer(c_int),        value   :: incy
                 complex(c_float)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCdotu_v2
    end interface
    
    interface
        function cublasCdotc_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasCdotc_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value   :: handle
                 integer(c_int),        value   :: n
                 complex(c_float), dimension(*) :: x
                 integer(c_int),        value   :: incx
                 complex(c_float), dimension(*) :: y
                 integer(c_int),        value   :: incy
                 complex(c_float)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCdotc_v2
    end interface
    
    interface
        function cublasZdotu_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasZdotu_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle
                 integer(c_int),        value    :: n
                 complex(c_double), dimension(*) :: x
                 integer(c_int),        value    :: incx
                 complex(c_double), dimension(*) :: y
                 integer(c_int),        value    :: incy
                 complex(c_double)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasZdotu_v2
    end interface
    
    interface
        function cublasZdotc_v2(handle,n,x,incx,y,incy,res) result(cublas_status) &
                    bind(c,name='cublasZdotc_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 complex(c_double), dimension(*) :: x
                 integer(c_int),        value    :: incx
                 complex(c_double), dimension(*) :: y
                 integer(c_int),        value    :: incy
                 complex(c_double)               :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasZdotc_v2
    end interface
    
    interface
        function cublasSscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasSscal_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_float)                   :: alpha
                 real(c_float), dimension(*)     :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSscal_v2
    end interface
    
    interface
        function cublasDscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasDscal_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_double)                  :: alpha
                 real(c_double), dimension(*)    :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDscal_v2
    end interface
    
    interface
        function cublasCscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasCscal_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 complex(c_float)                :: alpha
                 complex(c_float), dimension(*)  :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCscal_v2
    end interface
    
    interface
        function cublasCsscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasCsscal_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_float)                   :: alpha
                 complex(c_float), dimension(*)  :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCsscal_v2
    end interface
    
    interface
        function cublasZscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasZscal_v2')
                  use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 complex(c_double)               :: alpha
                 complex(c_double), dimension(*) :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasZscal_v2
    end interface
    
    interface
        function cublasZdscal_v2(handle,n,alpha,x,incx) result(cublas_status) &
                    bind(c,name='cublasZdscal_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_double)                  :: alpha
                 complex(c_double), dimension(*) :: x
                 integer(c_int),        value    :: incx
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasZdscal_v2
    end interface
    
    interface
        function cublasSaxpy_v2(handle,n,alpha,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasSaxpy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_float)                   :: alpha
                 real(c_float), dimension(*)     :: x
                 integer(c_int),        value    :: incx
                 real(c_float), dimension(*)     :: y
                 integer(c_int),        value    :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSaxpy_v2
    end interface
    
    interface
        function cublasDaxpy_v2(handle,n,alpha,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasDaxpy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 real(c_double)                  :: alpha
                 real(c_double), dimension(*)    :: x
                 integer(c_int),        value    :: incx
                 real(c_double), dimension(*)    :: y
                 integer(c_int),        value    :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDaxpy_v2
    end interface
    
    interface
        function cublasCaxpy_v2(handle,n,alpha,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasCaxpy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 complex(c_float)                :: alpha
                 complex(c_float), dimension(*)  :: x
                 integer(c_int),        value    :: incx
                 complex(c_float), dimension(*)  :: y
                 integer(c_int),        value    :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCaxpy_v2
    end interface
    
    interface
        function cublasZaxpy_v2(handle,n,alpha,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasZaxpy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle  
                 integer(c_int),        value    :: n
                 complex(c_double),     value    :: alpha
                 complex(c_double), dimension(*) :: x
                 integer(c_int),        value    :: incx
                 complex(c_double), dimension(*) :: y
                 integer(c_int),        value    :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasZaxpy_v2
    end interface
    
    interface
        function cublasScopy_v2(handle,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasScopy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value    :: handle 
                 real(c_float), dimension(*)     :: x
                 integer(c_int),        value    :: incx
                 real(c_float), dimension(*)     :: y
                 integer(c_int),        value    :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasScopy_v2
    end interface
    
    interface
        function cublasDcopy_v2(handle,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasDcopy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value     :: handle 
                 real(c_double), dimension(*)     :: x
                 integer(c_int),        value     :: incx
                 real(c_double), dimension(*)     :: y
                 integer(c_int),        value     :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDcopy_v2
    end interface
    
    interface
        function cublasCcopy_v2(handle,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasCcopy_v2')
                  use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value     :: handle 
                 complex(c_float),  dimension(*)  :: x
                 integer(c_int),        value     :: incx
                 complex(c_float),  dimension(*)  :: y
                 integer(c_int),        value     :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCcopy_v2
    end interface
    
    interface
        function cublasZcopy_v2(handle,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasZcopy_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value      :: handle 
                 complex(c_double),  dimension(*)  :: x
                 integer(c_int),        value      :: incx
                 complex(c_double),  dimension(*)  :: y
                 integer(c_int),        value      :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasZcopy_v2
    end interface
    
    interface
        function cublasSswap_v2(handle,n,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasSswap_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value      :: handle 
                 integer(c_int),        value      :: n
                 real(c_float), dimension(*)       :: x
                 integer(c_int),        value      :: incx
                 real(c_float), dimension(*)       :: y
                 integer(c_int),        value      :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasSswap_v2
    end interface
    
    interface
        function cublasDswap_v2(handle,n,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasDswap_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value       :: handle 
                 integer(c_int),        value       :: n
                 real(c_double), dimension(*)       :: x
                 integer(c_int),        value       :: incx
                 real(c_double), dimension(*)       :: y
                 integer(c_int),        value       :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasDswap_v2
    end interface
    
    interface
        function cublasCswap_v2(handle,n,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasCswap_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value       :: handle 
                 integer(c_int),        value       :: n
                 complex(c_float), dimension(*)     :: x
                 integer(c_int),        value       :: incx
                 complex(c_float), dimension(*)     :: y
                 integer(c_int),        value       :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasCswap_v2
    end interface
    
    interface
        function cublasZswap_v2(handle,n,x,incx,y,incy) result(cublas_status) &
                    bind(c,name='cublasZswap_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 complex(c_double), dimension(*)     :: x
                 integer(c_int),        value        :: incx
                 complex(c_double), dimension(*)     :: y
                 integer(c_int),        value        :: incy
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasZswap_v2
    end interface
    
    interface
        function cublasIasmax_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIsamax_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 real(c_float), dimension(*)         :: x
                 integer(c_int),        value        :: incx
                 integer(c_int)                      :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIsamax_v2
    end interface
    
    interface
        function cublasIdamax_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIdamax_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 real(c_double), dimension(*)        :: x
                 integer(c_int),        value        :: incx
                 integer(c_int)                      :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIdamax_v2
    end interface
    
    interface
        function cublasIcamax_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIcamax_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 complex(c_float), dimension(*)      :: x
                 integer(c_int),        value        :: incx
                 integer(c_int)                      :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIcamax_v2
    end interface
    
    interface
        function cublasIzamax_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIzamax_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 complex(c_double), dimension(*)     :: x
                 integer(c_int),        value        :: incx
                 integer(c_int)                      :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIzamax_v2
    end interface
    
    interface
        function cublasIsamin_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIsamin_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value        :: handle 
                 integer(c_int),        value        :: n
                 real(c_float), dimension(*)         :: x
                 integer(c_int),        value        :: incx
                 integer(c_int)                      :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIsamin_v2
    end interface
    
    interface
        function cublasIdamin_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIdamin_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 real(c_double), dimension(*)         :: x
                 integer(c_int),        value         :: incx
                 integer(c_int)                       :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIdamin_v2
    end interface
    
    interface
        function cublasIcamin_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIcamin_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 complex(c_float), dimension(*)       :: x
                 integer(c_int),        value         :: incx
                 integer(c_int)                       :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasIcamin_v2
    end interface
    
    interface
        function cublasIzamin_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasIzamin_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 complex(c_double), dimension(*)       :: x
                 integer(c_int),        value         :: incx
                 integer(c_int)                       :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasIzamin_v2
    end interface
    
    interface
        function cublasSasum_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasSasum_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 real(c_float), dimension(*)          :: x
                 integer(c_int),        value         :: incx
                 real(c_float)                        :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasSasum_v2
    end interface
    
    interface
        function cublasDasum_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasDasum_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 real(c_double), dimension(*)          :: x
                 integer(c_int),        value         :: incx
                 real(c_double)                        :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasDasum_v2
    end interface
    
    interface
        function cublasScasum_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasScasum_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 complex(c_float), dimension(*)          :: x
                 integer(c_int),        value         :: incx
                 real(c_float)                        :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status    
        end function cublasScasum_v2
    end interface
    
    interface
        function cublasDzasum_v2(handle,n,x,incx,res) result(cublas_status) &
                    bind(c,name='cublasDzasum_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 complex(c_double), dimension(*)          :: x
                 integer(c_int),        value         :: incx
                 real(c_double)                        :: res
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status     
        end function cublasDzasum_v2
    end interface
    
    interface
        function cublasSrot_v2(handle,n,x,incx,y,incy,c,s) result(cublas_status) &
                    bind(c,name='cublasSrot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value         :: handle 
                 integer(c_int),        value         :: n
                 real(c_float), dimension(*)          :: x
                 integer(c_int),        value         :: incx
                 real(c_float), dimension(*)          :: y
                 integer(c_int),        value         :: incy
                 real(c_float)                        :: c
                 real(c_float)                        :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status    
        end function cublasSrot_v2
    end interface
    
    interface
        function cublasDrot_v2(handle,n,x,incx,y,incxy,c,s) result(cublas_status) &
                   bind(c,name='cublasDrot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 integer(c_int),        value          :: n
                 real(c_double), dimension(*)          :: x
                 integer(c_int),        value          :: incx
                 real(c_double), dimension(*)          :: y
                 integer(c_int),        value          :: incy
                 real(c_double)                        :: c
                 real(c_double)                        :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status    
        end function cublasDrot_v2
    end interface
    
    interface
        function cublasCrot_v2(handle,n,x,incx,y,incy,c,s) result(cublas_status) &
                    bind(c,name='cublasCrot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 integer(c_int),        value          :: n
                 complex(c_float), dimension(*)        :: x
                 integer(c_int),        value          :: incx
                 complex(c_float), dimension(*)        :: y
                 integer(c_int),        value          :: incy
                 real(c_float)                         :: c
                 complex(c_float)                      :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasCrot_v2
    end interface
    
    interface
        function cublasCsrot_v2(handle,n,x,incx,y,incy,c,s) result(cublas_status) &
                    bind(c,name='cublasCsrot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 integer(c_int),        value          :: n 
                 complex(c_float), dimension(*)        :: x
                 integer(c_int),        value          :: incx
                 complex(c_float), dimension(*)        :: y
                 integer(c_int),        value          :: incy
                 real(c_float)                         :: c
                 real(c_float)                         :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasCsrot_v2
    end interface
    
    interface
        function cublasZrot_v2(handle,n,x,incx,y,incy,c,s) result(cublas_status) &
                    bind(c,name='cublasZrot_v2')
                  use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 integer(c_int),        value          :: n 
                 complex(c_double), dimension(*)       :: x
                 integer(c_int),        value          :: incx
                 complex(c_double), dimension(*)       :: y
                 integer(c_int),        value          :: incy
                 real(c_double)                        :: c
                 complex(c_double)                     :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasZrot_v2
    end interface
    
    interface
        function cublasZdrot_v2(handle,n,x,incx,y,incy,c,s) result(cublas_status) &
                    bind(c,name='cublasZdrot_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 integer(c_int),        value          :: n 
                 complex(c_double), dimension(*)       :: x
                 integer(c_int),        value          :: incx
                 complex(c_double), dimension(*)       :: y
                 integer(c_int),        value          :: incy
                 real(c_double)                        :: c
                 real(c_double)                        :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasZdrot_v2
    end interface
    
    interface
        function cublasSrotg_v2(handle,a,b,c,s) result(cublas_status) &
                    bind(c,name='cublasSrotg_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value          :: handle 
                 real(c_float)                         :: a
                 real(c_float)                         :: b
                 real(c_float)                         :: c
                 real(c_float)                         :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasSrotg_v2
    end interface
    
    interface
        function cublasDrotg_v2(handle,a,b,c,s) result(cublas_status) &
                    bind(c,name='cublasDrotg_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 real(c_double)                         :: a
                 real(c_double)                         :: b
                 real(c_double)                         :: c
                 real(c_double)                         :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasDrotg_v2
    end interface
    
    interface
        function cublasCrotg_v2(handle,a,b,c,s) result(cublas_status) &
                    bind(c,name='cublasCrotg_v2')
                  use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 complex(c_float)                       :: a
                 complex(c_float)                       :: b
                 real(c_float)                          :: c
                 complex(c_float)                       :: s
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasCrotg_v2
    end interface
    
    interface
        function cublasSrotm_v2(handle,n,x,incx,y,incy,param) result(cublas_status) &
                    bind(c,name='cublasSrotm_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 integer(c_int),        value           :: n
                 real(c_float), dimension(*)            :: x
                 integer(c_int),        value           :: incx
                 real(c_float), dimension(*)            :: y
                 integer(c_int),        value           :: incy
                 real(c_float), dimension(5)            :: param
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasSrotm_v2
    end interface
    
    interface
        function cublasDrotm_v2(handle,n,x,incx,y,incy,param) result(cublas_status) &
                    bind(c,name='cublasDrotm_v2')
                  use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 integer(c_int),        value           :: n
                 real(c_double), dimension(*)            :: x
                 integer(c_int),        value           :: incx
                 real(c_double), dimension(*)            :: y
                 integer(c_int),        value           :: incy
                 real(c_double), dimension(5)            :: param
                 integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasDrotm_v2
    end interface
    
    interface
        function cublasSrotmg_v2(handle,d1,d2,x1,y1,param) result(cublas_status) &
                    bind(c,name='cublasSrotmg_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 real(c_float)                          :: d1
                 real(c_float)                          :: d2
                 real(c_float)                          :: x1
                 real(c_float)                          :: y1
                 real(c_float), dimension(5)            :: param
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status 
        end function cublasSrotmg_v2
    end interface
    
    interface
        function cublasDrotmg_v2(handle,d1,d2,x1,y1,param) result(cublas_status) &
                    bind(c,name='cublasDrotmg_v2')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUBLAS_STATUS_SUCCESS
                 import :: cublasContext 
                 type(cublasContext),   value           :: handle 
                 real(c_double)                          :: d1
                 real(c_double)                          :: d2
                 real(c_double)                          :: x1
                 real(c_double)                          :: y1
                 real(c_double), dimension(5)            :: param
                 integer(kind(CUBLAS_STATUS_SUCCESS))    :: cublas_status  
        end function cublasDrotmg_v2
    end interface
    
    interface
        function cublasSgemv_v2(handle,trans,m,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                     bind(c,name='cublasSgemv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext), value           :: handle
                  integer(kind(CUBLAS_OP_N))           :: trans
                  integer(c_int),      value           :: m
                  integer(c_int),      value           :: n
                  real(c_float)                        :: alpha
                  real(c_float), dimension(*)          :: A
                  integer(c_int),      value           :: lda
                  real(c_float)  dimension(*)          :: x
                  integer(c_int),      value           :: incx
                  real(c_float)                        :: beta
                  real(c_float), dimension(*)          :: y
                  integer(c_int),      value           :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasSgemv_v2
    end interface
    
    interface
        function cublasDgemv_v2(handle,trans,m,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasDgemv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),  value       :: handle
                  integer(kind(CUBLAS_OP_N))        :: trans
                  integer(c_int),       value       :: m
                  integer(c_int),       value       :: n
                  real(c_double)                    :: alpha
                  real(c_double),   dimension(*)    :: A
                  integer(c_int),       value       :: lda
                  real(c_double),   dimension(*)    :: x
                  integer(c_int),       value       :: incx
                  real(c_double)                    :: beta
                  real(c_double),   dimension(*)    :: y
                  integer(c_int),       value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasDgemv_v2
    end interface
    
    interface
        function cublasCgemv_v2(handle,trans,m,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasCgemv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasCgemv_v2
    end interface
    
    interface
        function cublasZgemv_v2(handle,trans,m,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZgemv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_double)                     :: alpha
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)       :: x
                  integer(c_int),           value       :: incx
                  complex(c_double)                     :: beta
                  complex(c_double), dimension(*)       :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasZgemv_v2
    end interface
    
    interface
        function cublasSgbmv_v2(handle,trans,m,n,kl,ku,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasSgbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),       value     :: handle
                  integer(kind(CUBLAS_OP_N))           :: trans
                  integer(c_int),            value     :: m
                  integer(c_int),            value     :: n
                  integer(c_int),            value     :: kl
                  integer(c_int),            value     :: ku
                  real(c_float)                        :: alpha
                  real(c_float), dimension(*)          :: A
                  integer(c_int),            value     :: lda
                  real(c_float)  dimension(*)          :: x
                  integer(c_int),            value     :: incx
                  real(c_float)                        :: beta
                  real(c_float), dimension(*)          :: y
                  integer(c_int),            value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasSgbmv_v2
    end interface
    
    interface
        function cublasDgbmv_v2(handle,trans,m,n,kl,ku,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasDgbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),       value      :: handle
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),            value      :: m
                  integer(c_int),            value      :: n
                  integer(c_int),            value      :: kl
                  integer(c_int),            value      :: ku
                  real(c_double)                        :: alpha
                  real(c_double), dimension(*)          :: A
                  integer(c_int),            value      :: lda
                  real(c_double)  dimension(*)          :: x
                  integer(c_int),            value      :: incx
                  real(c_double)                        :: beta
                  real(c_double), dimension(*)          :: y
                  integer(c_int),            value      :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasDgbmv_v2
    end interface
    
    interface
        function cublasCgbmv_v2(handle,trans,m,n,kl,ku,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasCgbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),       value     :: handle
                  integer(kind(CUBLAS_OP_N))           :: trans
                  integer(c_int),            value     :: m
                  integer(c_int),            value     :: n
                  integer(c_int),            value     :: kl
                  integer(c_int),            value     :: ku
                  complex(c_float)                     :: alpha
                  complex(c_float), dimension(*)       :: A
                  integer(c_int),            value     :: lda
                  complex(c_float)  dimension(*)       :: x
                  integer(c_int),            value     :: incx
                  complex(c_float)                     :: beta
                  complex(c_float), dimension(*)       :: y
                  integer(c_int),            value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status
        end function cublasCgbmv_v2
    end interface
    
    interface
        function cublasZgbmv_v2(handle,trans,m,n,kl,ku,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZgbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),       value     :: handle
                  integer(kind(CUBLAS_OP_N))           :: trans
                  integer(c_int),            value     :: m
                  integer(c_int),            value     :: n
                  integer(c_int),            value     :: kl
                  integer(c_int),            value     :: ku
                  complex(c_double)                     :: alpha
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),            value     :: lda
                  complex(c_double)  dimension(*)       :: x
                  integer(c_int),            value     :: incx
                  complex(c_double)                     :: beta
                  complex(c_double), dimension(*)       :: y
                  integer(c_int),            value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasZgbmv_v2
    end interface
    
    interface
        function cublasStrmv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                     bind(c,name='cublasStrmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),             value     :: n
                  real(c_float), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasStrmv_v2
    end interface
    
    interface
        function cublasDtrmv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtrmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),             value     :: n
                  real(c_double), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasDtrmv_v2
    end interface
    
    interface
        function cublasCtrmv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtrmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),             value     :: n
                  complex(c_float), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasCtrmv_v2
    end interface
    
    interface
        function cublasZtrmv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtrmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),             value     :: n
                  complex(c_double), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  complex(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasZtrmv_v2
    end interface
    
    interface
        function cublasStbmv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasStbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),       value           :: n
                  integer(c_int),       value           :: k
                  real(c_float),    dimension(*)        :: A
                  integer(c_int),       value           :: lda
                  real(c_float),    dimension(*)        :: x
                  integer(c_int),       value           :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasStbmv_v2
    end interface
    
    interface
        function cublasDtbmv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),       value           :: n
                  integer(c_int),       value           :: k
                  real(c_double),    dimension(*)        :: A
                  integer(c_int),       value           :: lda
                  real(c_double),    dimension(*)        :: x
                  integer(c_int),       value           :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasDtbmv_v2
    end interface
    
    interface
        function cublasCtbmv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtbmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),       value           :: n
                  integer(c_int),       value           :: k
                  complex(c_float),    dimension(*)        :: A
                  integer(c_int),       value           :: lda
                  complex(c_float),    dimension(*)        :: x
                  integer(c_int),       value           :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status 
        end function cublasCtbmv_v2
    end interface
    
    interface
        function cublasZtbmv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtbmv_v2')
                   use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag
                  integer(c_int),       value           :: n
                  integer(c_int),       value           :: k
                  complex(c_double),    dimension(*)        :: A
                  integer(c_int),       value           :: lda
                  complex(c_double),    dimension(*)        :: x
                  integer(c_int),       value           :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasZtbmv_v2
    end interface
    
    interface
        function cublasStpmv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasStpmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_float), dimension(*)           :: AP
                  real(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status  
        end function cublasStpmv_v2
    end interface
    
    interface
        function cublasDtpmv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtpmv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_double), dimension(*)           :: AP
                  real(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS)) :: cublas_status   
        end function cublasDtpmv_v2
    end interface
    
    interface
        function cublasCtpmv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtpmv_v2')
                   use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_float), dimension(*)           :: AP
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasCtpmv_v2
    end interface
    
    interface
        function cublasZtpmv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtpmv_v2')
                   use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_double), dimension(*)           :: AP
                  complex(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZtpmv_v2
    end interface
    
    interface
        function cublasStrsv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasStrsv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_float), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasStrsv_v2
    end interface
    
    interface
        function cublasDtrsv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtrsv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_double), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDtrsv_v2
    end interface
    
    interface
        function cublasCtrsv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtrsv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),             value     :: lda
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCtrsv_v2
    end interface
    
    interface
        function cublasZtrsv_v2(handle,uplo,trans,diag,n,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtrsv_v2')
                   use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),             value     :: lda
                  complex(c_double), dimension(*)       :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZtrsv_v2
    end interface
    
    interface
        function cublasStpsv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasStpsv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  real(c_float),    dimension(*)        :: AP
                  real(c_float),    dimension(*)        :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasStpsv_v2
    end interface
    
    interface
        function cublasDtpsv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtpsv_v2')
                  use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  real(c_double),    dimension(*)       :: AP
                  real(c_double),    dimension(*)       :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDtpsv_v2
    end interface
    
    interface
        function cublasCtpsv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtpsv_v2')
                   use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  complex(c_float),    dimension(*)     :: AP
                  complex(c_float),    dimension(*)     :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCtpsv_v2
    end interface
    
    interface
        function cublasZtpsv_v2(handle,uplo,trans,diag,n,AP,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtpsv_v2')
                    use,intrinsic :: ISO_C_BINDING
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  complex(c_double),    dimension(*)    :: AP
                  complex(c_double),    dimension(*)    :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZtpsv_v2
    end interface
    
    interface
        function cublasStbsv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasStbsv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  integer(c_int),             value     :: k
                  real(c_float), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasStbsv_v2
    end interface
    
    interface
        function cublasDtbsv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasDtbsv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  integer(c_int),             value     :: k
                  real(c_double), dimension(*)          :: A
                  integer(c_int),             value     :: lda
                  real(c_double), dimension(*)          :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasDtbsv_v2
    end interface
    
    interface
        function cublasCtbsv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasCtbsv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  integer(c_int),             value     :: k
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),             value     :: lda
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCtbsv_v2
    end interface
    
    interface
        function cublasZtbsv_v2(handle,uplo,trans,diag,n,k,A,lda,x,incx) result(cublas_status) &
                    bind(c,name='cublasZtbsv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag  
                  integer(c_int),             value     :: n
                  integer(c_int),             value     :: k
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),             value     :: lda
                  complex(c_double), dimension(*)       :: x
                  integer(c_int),             value     :: incx
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZtbsv_v2
    end interface
    
    interface
        function cublasSsymv_v2(handle,uplo,trans,diag,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasSsymv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_float), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: y
                  integer(c_int),             value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasSsymv_v2
    end interface
    
    interface
        function cublasDsymv_v2(handle,uplo,trans,diag,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasDsymv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: A
                  integer(c_int),             value     :: lda
                  real(c_double), dimension(*)           :: x
                  integer(c_int),             value     :: incx
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: y
                  integer(c_int),             value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasDsymv_v2
    end interface
    
    interface
        function cublasCsymv_v2(handle,uplo,trans,diag,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasCsymv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),             value     :: lda
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),             value     :: incx
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),             value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasCsymv_v2
    end interface
    
    interface
        function cublasZsymv_v2(handle,uplo,trans,diag,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZsymv_v2')
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_DIAG_NON_UNIT
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),        value     :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(kind(CUBLAS_NON_UNIT))        :: diag 
                  integer(c_int),             value     :: n
                  complex(c_double)                     :: alpha
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),             value     :: lda
                  complex(c_double), dimension(*)       :: x
                  integer(c_int),             value     :: incx
                  complex(c_double)                     :: beta
                  complex(c_double), dimension(*)       :: y
                  integer(c_int),             value     :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZsymv_v2
    end interface
    
    interface
        function cublasChemv_v2(handle,uplo,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasChemv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasChemv_v2
    end interface
    
    interface
        function cublasZhemv_v2(handle,uplo,n,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZhemv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double)                      :: beta
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZhemv_v2
    end interface
    
    interface
        function cublasSsbmv_v2(handle,uplo,n,k,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasSsbmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasSsbmv_v2
    end interface
    
    interface
        function cublasDsbmv_v2(handle,uplo,n,k,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasDsbmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasDsbmv_v2
    end interface
    
    interface
        function cublasChbmv_v2(handle,uplo,n,k,alpha,A,lda,x,incx,beta,y,incy) result(cublas_status) &
                     bind(c,name='cublasChbmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_float)                         :: beta
                  complex(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasChbmv_v2
    end interface
    
    interface
        function cublasZhbmv_v2(handle,uplo,n,k,alpha,lda,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZhbmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                         :: alpha
                  complex(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_double)                         :: beta
                  complex(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasZhbmv_v2
    end interface
    
    interface
        function cublasSspmv_v2(handle,uplo,n,alpha,AP,x,incx,beta,y,incy)  result(cublas_status) &
                    bind(c,name='cublasSspmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: AP
                
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasSspmv_v2
    end interface
    
    interface
        function cublasDspmv_v2(handle,uplo,n,alpha,AP,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasDspmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: AP
                 
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDspmv_v2
    end interface
    
    interface
        function cublasChpmv_v2(handle,uplo,n,alpha,AP,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasChpmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: AP
                 
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status     
        end function cublasChpmv_v2
    end interface
    
    interface
        function cublasZhpmv_v2(handle,uplo,n,alpha,AP,x,incx,beta,y,incy) result(cublas_status) &
                    bind(c,name='cublasZhpmv_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo 
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: AP
                 
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double)                      :: beta
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZhpmv_v2
    end interface
    
    interface
        function cublasSger_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasSger_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasSger_v2
    end interface
    
    interface
        function cublasDger_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasDger_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasDger_v2
    end interface
    
    interface
        function cublasCgeru_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasCgeru_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCgeru_v2
    end interface
    
    interface
        function cublasCgerc_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasCgerc_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCgerc_v2
    end interface
    
    interface
        function cublasZgeru_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasZgeru_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasZgeru_v2
    end interface
    
    interface
        function cublasZgerc_v2(handle,m,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasZgerc_v2')
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(c_int),           value       :: m
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasZgerc_v2
    end interface
    
    interface
        function cublasSsyr_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasSsyr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasSsyr_v2
    end interface
    
    interface
        function cublasDsyr_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasDsyr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status     
        end function cublasDsyr_v2
    end interface
    
    interface
        function cublasCsyr_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasCsyr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status     
        end function cublasCsyr_v2
    end interface
    
    interface
        function cublasZsyr_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasZsyr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_double)                     :: alpha
                  complex(c_double), dimension(*)       :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasZsyr_v2
    end interface
    
    interface
        function cublasCher_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasCher_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCher_v2
    end interface
    
    interface
        function cublasZher_v2(handle,uplo,n,alpha,x,incx,A,lda) result(cublas_status) &
                    bind(c,name='cublasZher_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZher_v2
    end interface
    
    interface
        function cublasSspr_v2(handle,uplo,n,alpha,x,incx,AP) result(cublas_status) &
                    bind(c,name='cublasSspr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasSspr_v2
    end interface
    
    interface
        function cublasDspr_v2(handle,uplo,n,alpha,x,incx,AP) result(cublas_status) &
                    bind(c,name='cublasDspr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasDspr_v2
    end interface
    
    interface
        function cublasChpr_v2(handle,uplo,n,alpha,x,incx,AP) result(cublas_status) &
                    bind(c,name='cublasChpr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasChpr_v2
    end interface
    
    interface
        function cublasZhpr_v2(handle,uplo,n,alpha,x,incx,AP) result(cublas_status) &
                    bind(c,name='cublasZhpr_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  complex(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasZhpr_v2
    end interface
    
    interface
        function cublasSsyr2_v2(handle,uplo,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasSsyr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasSsyr2_v2
    end interface
    
    interface
        function cublasDsyr2_v2(handle,uplo,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasDsyr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDsyr2_v2
    end interface
    
    interface
        function cublasCsyr2_v2(handle,upload,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasCsyr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasCsyr2_v2
    end interface
    
    interface
        function cublasZsyr2_v2(handle,uplo,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasZsyr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZsyr2_v2
    end interface
    
    interface
        function cublasCher2_v2(handle,uplo,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasCher2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCher2_v2
    end interface
    
    interface
        function cublasZher2_v2(handle,uplo,n,alpha,x,incx,y,incy,A,lda) result(cublas_status) &
                    bind(c,name='cublasZher2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)        :: y
                  integer(c_int),           value       :: incy
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZher2_v2
    end interface
    
    interface
        function cublasSspr2_v2(handle,uplo,n,alpha,x,incx,y,incy,AP) result(cublas_status) &
                    bind(c,name='cublasSspr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_float), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasSspr2_v2
    end interface
    
    interface
        function cublasDspr2_v2(handle,uplo,n,alpha,x,incx,y,incy,AP) result(cublas_status) &
                    bind(c,name='cublasDspr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  real(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  real(c_double), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDspr2_v2
    end interface
    
    interface
        function cublasChpr2_v2(handle,uplo,n,alpha,x,incx,y,incy,AP) result(cublas_status) &
                    bind(c,name='cublasChpr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_float), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  complex(c_float), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasChpr2_v2
    end interface
    
    interface
        function cublasZhpr2_v2(handle,uplo,n,alpha,x,incx,y,incy,AP) result(cublas_status) &
                    bind(c,name='cublasZhpr2_v2')
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  import :: cublasContext
                  type(cublasContext),      value       :: handle 
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(c_int),           value       :: n
                  complex(c_double)                         :: alpha
                  complex(c_double), dimension(*)           :: x
                  integer(c_int),           value       :: incx
                  complex(c_double), dimension(*)           :: y
                  integer(c_int),           value       :: incy
                  complex(c_double), dimension(*)           :: AP
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZhpr2_v2
    end interface
    
    interface
        function cublasSgemm_v2(handle,transa,transb,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSgemm_v2')
                 import ::  CUBLAS_OP_N 
                 import ::  CUBLAS_STATUS_SUCCESS
                 import ::  cublasContext
                 type(cublasContext),      value       :: handle
                 integer(kind(CUBLAS_OP_N))            :: transa
                 integer(kind(CUBLAS_OP_N))            :: transb
                 integer(c_int),           value       :: m
                 integer(c_int),           value       :: n
                 integer(c_int),           value       :: k
                 real(c_float)                         :: alpha
                 real(c_float), dimension(*)           :: A
                 integer(c_int),           value       :: lda
                 real(c_float), dimension(*)           :: B
                 integer(c_int),           value       :: ldb
                 real(c_float), dimension(*)           :: C
                 integer(c_int),           value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasSgemm_v2
    end interface
    
    interface
        function cublasDgemm_v2(handle,transa,transb,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDgemm_v2')
                 import ::  CUBLAS_OP_N 
                 import ::  CUBLAS_STATUS_SUCCESS
                 import ::  cublasContext
                 type(cublasContext),      value       :: handle
                 integer(kind(CUBLAS_OP_N))            :: transa
                 integer(kind(CUBLAS_OP_N))            :: transb
                 integer(c_int),           value       :: m
                 integer(c_int),           value       :: n
                 integer(c_int),           value       :: k
                 real(c_double)                         :: alpha
                 real(c_double), dimension(*)           :: A
                 integer(c_int),           value       :: lda
                 real(c_double), dimension(*)           :: B
                 integer(c_int),           value       :: ldb
                 real(c_double), dimension(*)           :: C
                 integer(c_int),           value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasDgemm_v2
    end interface
    
    interface
        function cublasCgemm_v2(handle,transa,transb,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCgemm_v2')
                 import ::  CUBLAS_OP_N 
                 import ::  CUBLAS_STATUS_SUCCESS
                 import ::  cublasContext
                 type(cublasContext),      value       :: handle
                 integer(kind(CUBLAS_OP_N))            :: transa
                 integer(kind(CUBLAS_OP_N))            :: transb
                 integer(c_int),           value       :: m
                 integer(c_int),           value       :: n
                 integer(c_int),           value       :: k
                 complex(c_float)                      :: alpha
                 complex(c_float), dimension(*)        :: A
                 integer(c_int),           value       :: lda
                 complex(c_float), dimension(*)        :: B
                 integer(c_int),           value       :: ldb
                 complex(c_float), dimension(*)        :: C
                 integer(c_int),           value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasCgemm_v2
    end interface
    
    interface
        function cublasZgemm_v2(handle,transa,transb,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZgemm_v2')
                 import ::  CUBLAS_OP_N 
                 import ::  CUBLAS_STATUS_SUCCESS
                 import ::  cublasContext
                 type(cublasContext),      value       :: handle
                 integer(kind(CUBLAS_OP_N))            :: transa
                 integer(kind(CUBLAS_OP_N))            :: transb
                 integer(c_int),           value       :: m
                 integer(c_int),           value       :: n
                 integer(c_int),           value       :: k
                 complex(c_double)                     :: alpha
                 complex(c_double), dimension(*)       :: A
                 integer(c_int),           value       :: lda
                 complex(c_double), dimension(*)       :: B
                 integer(c_int),           value       :: ldb
                 complex(c_double), dimension(*)       :: C
                 integer(c_int),           value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasZgemm_v2
    end interface
    
    interface
        function cublasSsyrk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSsyrk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasSsyrk_v2
    end interface
    
    interface
        function cublasDsyrk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDsyrk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDsyrk_v2
    end interface
    
    interface
        function cublasCsyrk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCsyrk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasCsyrk_v2
    end interface
    
    interface
        function cublasZsyrk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZsyrk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_double)                      :: beta
                  complex(c_double), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZsyrk_v2
    end interface
    
    interface
        function cublasCherk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCherk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_float)                         :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  real(c_float)                         :: beta
                  complex(c_float), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasCherk_v2
    end interface
    
    interface
        function cublasZherk_v2(handle,uplo,trans,n,k,alpha,A,lda,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZherk_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_double)                         :: alpha
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  real(c_double)                         :: beta
                  complex(c_double), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasZherk_v2
    end interface
    
    interface
        function cublasSsyr2k_v2(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSsyr2k_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_float), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasSsyr2k_v2
    end interface
    
    interface
        function cublasDsyr2k_v2(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDsyr2k_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_double), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status 
        end function cublasDsyr2k_v2
    end interface
    
    interface
        function cublasCsyr2k_v2(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCsyr2k_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  complex(c_float)                         :: beta
                  complex(c_float), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCsyr2k_v2
    end interface
    
    interface
        function cublasZsyr2k_v2(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZsyr2k_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                     :: alpha
                  complex(c_double), dimension(*)       :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)       :: B
                  integer(c_int),           value       :: ldb
                  complex(c_double)                     :: beta
                  complex(c_double), dimension(*)       :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasZsyr2k_v2
    end interface
    
    interface
        function cublasCher2k_v2(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCher2k_v2')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)        :: B
                  integer(c_int),           value       :: ldb
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasCher2k_v2
    end interface
    
    interface
        function cublasZher2k_v2(handle,uplo,trans,n,k,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZher2k_v2')
                   import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)        :: B
                  integer(c_int),           value       :: ldb
                  complex(c_double)                      :: beta
                  complex(c_double), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasZher2k_v2
    end interface
    
    interface
        function cublasSsyrkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSsyrkx')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_float)                         :: alpha
                  real(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_float), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  real(c_float)                         :: beta
                  real(c_float), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasSsyrkx
    end interface
    
    interface
        function cublasDsyrkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDsyrkx')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  real(c_double)                         :: alpha
                  real(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  real(c_double), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  real(c_double)                         :: beta
                  real(c_double), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasDsyrkx
    end interface
    
    interface
        function cublasCsyrkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCsyrkx')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                         :: alpha
                  complex(c_float), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  complex(c_float)                         :: beta
                  complex(c_float), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status    
        end function cublasCsyrkx
    end interface
    
    interface
        function cublasZsyrkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZsyrkx')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                         :: alpha
                  complex(c_double), dimension(*)           :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)           :: B
                  integer(c_int),           value       :: ldb
                  complex(c_double)                         :: beta
                  complex(c_double), dimension(*)           :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status   
        end function cublasZsyrkx
    end interface
    
    interface
        function cublasCherkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCherkx')
                  import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_float)                      :: alpha
                  complex(c_float), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_float), dimension(*)        :: B
                  integer(c_int),           value       :: ldb
                  complex(c_float)                      :: beta
                  complex(c_float), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status  
        end function cublasCherkx
    end interface
    
    interface
        function cublasZherkx(handle,uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZherkx')
                    import :: cublasContext
                  import :: CUBLAS_OP_N
                  import :: CUBLAS_FILL_MODE_LOWER 
                  import :: CUBLAS_STATUS_SUCCESS 
                  type(cublasContext),      value       :: handle
                  integer(kind(CUBLAS_FILL_MODE_LOWER)) :: uplo
                  integer(kind(CUBLAS_OP_N))            :: trans
                  integer(c_int),           value       :: n
                  integer(c_int),           value       :: k
                  complex(c_double)                      :: alpha
                  complex(c_double), dimension(*)        :: A
                  integer(c_int),           value       :: lda
                  complex(c_double), dimension(*)        :: B
                  integer(c_int),           value       :: ldb
                  complex(c_double)                      :: beta
                  complex(c_double), dimension(*)        :: C
                  integer(c_int),           value       :: ldc
                  integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasZherkx
    end interface
    
    interface
        function cublasSsymm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSsymm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_float)                          :: alpha
                 real(c_float), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 real(c_float), dimension(*)            :: B
                 integer(c_int),            value       :: ldb
                 real(c_float)                          :: beta
                 real(c_float), dimension(*)            :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))  :: cublas_status
        end function cublasSsymm_v2
    end interface
    
    interface
        function cublasDsymm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDsymm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_double)                          :: alpha
                 real(c_double), dimension(*)            :: A
                 integer(c_int),            value        :: lda
                 real(c_double), dimension(*)            :: B
                 integer(c_int),            value        :: ldb
                 real(c_double)                          :: beta
                 real(c_double), dimension(*)            :: C
                 integer(c_int),            value        :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))    :: cublas_status 
        end function cublasDsymm_v2
    end interface
    
    interface
        function cublasCsymm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCsymm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_float)                       :: alpha
                 complex(c_float), dimension(*)         :: A
                 integer(c_int),            value       :: lda
                 complex(c_float), dimension(*)         :: B
                 integer(c_int),            value       :: ldb
                 complex(c_float)                       :: beta
                 complex(c_float), dimension(*)         :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status  
        end function cublasCsymm_v2
    end interface
    
    interface
        function cublasZsymm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZsymm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_double)                       :: alpha
                 complex(c_double), dimension(*)         :: A
                 integer(c_int),            value       :: lda
                 complex(c_double), dimension(*)         :: B
                 integer(c_int),            value       :: ldb
                 complex(c_double)                       :: beta
                 complex(c_double), dimension(*)         :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status 
        end function cublasZsymm_v2
    end interface
    
    interface
        function cublasChemm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasChemm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_float)                       :: alpha
                 complex(c_float), dimension(*)         :: A
                 integer(c_int),            value       :: lda
                 complex(c_float), dimension(*)         :: B
                 integer(c_int),            value       :: ldb
                 complex(c_float)                       :: beta
                 complex(c_float), dimension(*)         :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status  
        end function cublasChemm_v2
    end interface
    
    interface
        function cublasZhemm_v2(handle,side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZhemm_v2')
                  
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_double)                       :: alpha
                 complex(c_double), dimension(*)         :: A
                 integer(c_int),            value       :: lda
                 complex(c_double), dimension(*)         :: B
                 integer(c_int),            value       :: ldb
                 complex(c_double)                       :: beta
                 complex(c_double), dimension(*)         :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status  
        end function cublasZhemm_v2
    end interface
    
    interface
        function cublasSdgmm(handle,mode,m,n,A,lda,x,incx,C,ldc) result(cublas_status) &
                    bind(c,name='cublasSdgmm')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_float), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 real(c_float), dimension(*)            :: x
                 integer(c_int),            value       :: incx
                 real(c_float), dimension(*)            :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status  
        end function cublasSdgmm
    end interface
    
    interface
        function cublasDdgmm(handle,mode,m,n,A,lda,x,incx,C,ldc) result(cublas_status) &
                    bind(c,name='cublasDdgmm')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_double), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 real(c_double), dimension(*)            :: x
                 integer(c_int),            value       :: incx
                 real(c_double), dimension(*)            :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status   
        end function cublasDdgmm
    end interface
    
    interface
        function cublasCdgmm(handle,mode,m,n,A,lda,x,incx,C,ldc) result(cublas_status) &
                    bind(c,name='cublasCdgmm')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_float), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 complex(c_float), dimension(*)            :: x
                 integer(c_int),            value       :: incx
                 complex(c_float), dimension(*)            :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status  
        end function cublasCdgmm
    end interface
    
    interface
        function cublasZdgmm(handle,mode,m,n,A,lda,x,incx,C,ldc) result(cublas_status) &
                    bind(c,name='cublasZdgmm')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_double), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 complex(c_double), dimension(*)            :: x
                 integer(c_int),            value       :: incx
                 complex(c_double), dimension(*)            :: C
                 integer(c_int),            value       :: ldc
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status   
        end function cublasZdgmm
    end interface
    
    interface
        function cublasStrsm_v2(handle,side,uplo,trans,diag,m,n,alpha,A,lda,B,ldb) result(cublas_status) &
                    bind(c,name='cublasStrsm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_OP_N
                 import ::  CUBLAS_DIAG_NON_UNIT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(kind(CUBLAS_OP_N))             :: trans
                 integer(kind(CUBLAS_DIAG_NON_UNIT))    :: diag
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_float)                          :: alpha
                 real(c_float), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 real(c_float), dimension(*)            :: B
                 integer(c_int),            value       :: ldb
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status   
        end function cublasStrsm_v2
    end interface
    
    interface
        function cublasDtrsm_v2(handle,side,uplo,trans,diag,m,n,alpha,A,lda,B,ldb) result(cublas_status) &
                    bind(c,name='cublasDtrsm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_OP_N
                 import ::  CUBLAS_DIAG_NON_UNIT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(kind(CUBLAS_OP_N))             :: trans
                 integer(kind(CUBLAS_DIAG_NON_UNIT))    :: diag
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 real(c_double)                          :: alpha
                 real(c_double), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 real(c_double), dimension(*)            :: B
                 integer(c_int),            value       :: ldb
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status   
        end function cublasDtrsm_v2
    end interface
    
    interface
        function cublasCtrsm_v2(handle,side,uplo,trans,diag,m,n,alpha,A,lda,B,ldb) result(cublas_status) &
                  bind(c,name='cublasCtrsm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_OP_N
                 import ::  CUBLAS_DIAG_NON_UNIT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(kind(CUBLAS_OP_N))             :: trans
                 integer(kind(CUBLAS_DIAG_NON_UNIT))    :: diag
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_float)                          :: alpha
                 complex(c_float), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 complex(c_float), dimension(*)            :: B
                 integer(c_int),            value       :: ldb
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status    
        end function cublasCtrsm_v2
    end interface
    
    interface
        function cublasZtrsm_v2(handle,side,uplo,trans,diag,m,n,alpha,A,lda,B,ldb) result(cublas_status) &
                    bind(c,name='cublasZtrsm_v2')
                 import ::  cublasContext
                 import ::  CUBLAS_SIDE_LEFT
                 import ::  CUBLAS_FILL_MODE_LOWER 
                 import ::  CUBLAS_OP_N
                 import ::  CUBLAS_DIAG_NON_UNIT
                 import ::  CUBLAS_STATUS_SUCCESS
                 type(cublasContext),       value       :: handle
                 integer(kind(CUBLAS_SIDE_LEFT))        :: side
                 integer(kind(CUBLAS_FILL_MODE_LOWER))  :: uplo
                 integer(kind(CUBLAS_OP_N))             :: trans
                 integer(kind(CUBLAS_DIAG_NON_UNIT))    :: diag
                 integer(c_int),            value       :: m
                 integer(c_int),            value       :: n
                 complex(c_double)                          :: alpha
                 complex(c_double), dimension(*)            :: A
                 integer(c_int),            value       :: lda
                 complex(c_double), dimension(*)            :: B
                 integer(c_int),            value       :: ldb
                 integer(kind(CUBLAS_STATUS_SUCCESS))   :: cublas_status    
        end function cublasZtrsm_v2
    end interface

end module mod_cublasapi_header