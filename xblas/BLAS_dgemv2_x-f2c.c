
#include "f2c-bridge.h"
#include "blas_enum.h"
#include "GMS_config.h"

void BLAS_dgemv2_x(enum blas_order_type order, enum blas_trans_type trans,
		   int m, int n, double alpha, double * __restrict __ATTR_ALIGN__(64) a, int lda,
		   double * __restrict __ATTR_ALIGN__(64) head_x, double * __restrict __ATTR_ALIGN__(64) tail_x, int incx,
		   double beta, double * __restrict __ATTR_ALIGN__(64) y, int incy,
		   enum blas_prec_type prec)  __ATTR_HOT__
                                              __ATTR_ALIGN__(32);


extern void FC_FUNC_(blas_dgemv2_x, BLAS_DGEMV2_X)
 
  (int *trans, int *m, int *n, double *alpha, const double *a, int *lda,
   const double *head_x, const double *tail_x, int *incx, double *beta,
   double *y, int *incy, int *prec) {
  BLAS_dgemv2_x(blas_colmajor, (enum blas_trans_type) *trans, *m, *n, *alpha,
		a, *lda, head_x, tail_x, *incx, *beta, y, *incy,
		(enum blas_prec_type) *prec);
}
