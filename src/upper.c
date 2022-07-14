#include "vuw.h"

SEXP upper(SEXP x, SEXP y) {
  /* y is a matrix of row vectors */
  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, k, l, m, n, o, p, xcol, ycol, zcol;
  SEXP z;

  m = nrows(x), n = ncols(x);
  o = isMatrix(y) ? nrows(y) : ncols(y);
  p = isMatrix(y) ? ncols(y) : nrows(y);

  if (n != p)
    error(_("different length"));

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(z = allocMatrix(INTSXP, m * o, n));
    int *iz = INTEGER(z);
    int *ix = INTEGER(x);
    int *iy = INTEGER(y);
    memset(iz, 0, m * n * o * sizeof(int));
    for (i = 0; i < n; ++i) {
      xcol = i*m, ycol = i*o, zcol = i*m*o;
      for (j = 0, k = 0, l = 0; j < m * o; ++j, ++k) {
        if (k >= m) k -= m, ++l;
        if (k < iy[ycol + l]) iz[zcol + j] = ix[xcol + k];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocMatrix(REALSXP, m * o, n));
    double *iz = REAL(z);
    double *ix = REAL(x);
    double *iy = REAL(y);
    memset(iz, 0, m * n * o * sizeof(double));
    for (i = 0; i < n; ++i) {
      xcol = i*m, ycol = i*o, zcol = i*m*o;
      for (j = 0, k = 0, l = 0; j < m * o; ++j, ++k) {
        if (k >= m) k -= m, ++l;
        if (k < iy[ycol + l]) iz[zcol + j] = ix[xcol + k];
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}
