#include "vuw.h"

SEXP setmul_mat(SEXP x, SEXP y) {
  if (!isMatrix(x) || !isMatrix(y))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, o, p, col;
  m = nrows(x), n = ncols(x), o = nrows(y), p = ncols(y);

  if (m != o || n != p)
    error(_("different length"));
  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[col + j];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[col + j];
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP setmul_row(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, p, col;
  m = nrows(x), n = ncols(x), p = XLENGTH(y);

  if (n != p)
    error(_("different length"));

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[i];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[i];
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP setmul_col(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, o, col;
  m = nrows(x), n = ncols(x), o = XLENGTH(y);

  if (m != o)
    error(_("different length"));

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[j];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[j];
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP setmul_num(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, col;
  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x);
    int* ix = INTEGER(x);
    int  iy = asInteger(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x);
    double* ix = REAL(x);
    double  iy = asReal(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy;
      }
    }
  } break;
  default:
    error(_("invalid length"));
  }
  UNPROTECT(1);
  return R_NilValue;
}
