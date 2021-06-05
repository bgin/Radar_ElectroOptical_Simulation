
#include "f2c-bridge.h"
#include "blas_enum.h"
#include "GMS_config"
void BLAS_dgemv_x(enum blas_order_type order, enum blas_trans_type trans,
		  int m, int n, double alpha, double * __restrict __ATTR_ALIGN__(64)a, int lda,
		  double * __restrict __ATTR_ALIGN__(64) x, int incx, double beta, double * __restrict __ATTR_ALIGN__(64) y,
		  int incy, enum blas_prec_type prec)  __ATTR_HOT__
                                                       __ATTR_ALIGN__(32);


extern void FC_FUNC_(blas_dgemv_x, BLAS_DGEMV_X)
 
  (int *trans, int *m, int *n, double *alpha, const double *a, int *lda,
   const double *x, int *incx, double *beta, double *y, int *incy,
   int *prec) {
  BLAS_dgemv_x(blas_colmajor, (enum blas_trans_type) *trans, *m, *n, *alpha,
	       a, *lda, x, *incx, *beta, y, *incy,
	       (enum blas_prec_type) *prec);
}
