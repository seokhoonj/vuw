#include "vuw.h"

SEXP ratio_by_period(SEXP x, SEXP start, SEXP end, SEXP ratio) {
  if (!isMatrix(x))
    error("not a matrix");

  R_xlen_t i, j, m, n, s, e, r, col;
  SEXP ans;
  m = nrows(x), n = ncols(x);
  s = XLENGTH(start), e = XLENGTH(end), r = XLENGTH(ratio);

  if (n != s || n != e || n != r)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(ans = allocMatrix(REALSXP, m, n));
    int* ix = INTEGER(x);
    int* istart = INTEGER(coerceVector(start, INTSXP));
    int* iend = INTEGER(coerceVector(end, INTSXP));
    double* iratio = REAL(ratio);
    double* ians = REAL(ans);
    memset(ians, 0, m * n * sizeof(double));
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (ix[col + j] > istart[i] && ix[col + j] <= iend[i]) {
          ians[col + j] = iratio[i];
        } else {
          ians[col + j] = 1;
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(ans = allocMatrix(REALSXP, m, n));
    double* ix = REAL(x);
    int* istart = INTEGER(coerceVector(start, INTSXP));
    int* iend = INTEGER(coerceVector(end, INTSXP));
    double* iratio = REAL(ratio);
    double* ians = REAL(ans);
    memset(ians, 0, m * n * sizeof(double));
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (ix[col + j] > istart[i] && ix[col + j] <= iend[i]) {
          ians[col + j] = iratio[i];
        } else {
          ians[col + j] = 1;
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(1);
  return ans;
}

SEXP set_ratio_by_period(SEXP x, SEXP start, SEXP end, SEXP ratio) {
  if (!isMatrix(x))
    error("not a matrix");

  R_xlen_t i, j, m, n, s, e, r, col;
  m = nrows(x), n = ncols(x);
  s = XLENGTH(start), e = XLENGTH(end), r = XLENGTH(ratio);

  if (n != s || n != e || n != r)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    int* ix = INTEGER(x);
    int* istart = INTEGER(coerceVector(start, INTSXP));
    int* iend = INTEGER(coerceVector(end, INTSXP));
    double* iratio = REAL(ratio);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (ix[col + j] > istart[i] && ix[col + j] <= iend[i]) {
          ix[col + j] = iratio[i];
        } else {
          ix[col + j] = 1;
        }
      }
    }
  } break;
  case REALSXP:{
    double* ix = REAL(x);
    int* istart = INTEGER(coerceVector(start, INTSXP));
    int* iend = INTEGER(coerceVector(end, INTSXP));
    double* iratio = REAL(ratio);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (ix[col + j] > istart[i] && ix[col + j] <= iend[i]) {
          ix[col + j] = iratio[i];
        } else {
          ix[col + j] = 1;
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  return R_NilValue;
}
