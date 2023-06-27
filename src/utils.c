#include "vuw.h"

SEXP fillValue(SEXP x, double value) {
  R_xlen_t i, n;
  n = XLENGTH(x);
  switch(TYPEOF(x)) {
  case INTSXP:{
    int *ix = INTEGER(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(int));
    } else {
      for (i = 0; i < n; ++i) ix[i] = (int) value;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(double));
    } else {
      for (i = 0; i < n; ++i) ix[i] = value;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  return x;
}

SEXP printArray(SEXP x) {
  R_xlen_t i, len;
  len = XLENGTH(x);
  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = LOGICAL(x);
    for (i = 0; i < len; ++i) printf("%d ", ix[i]);
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    for (i = 0; i < len; ++i) printf("%d ", ix[i]);
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    for (i = 0; i < len; ++i) printf("%f ", ix[i]);
  } break;
  case CPLXSXP:{
    Rcomplex *ix = COMPLEX(x);
    for (i = 0; i < len; ++i) printf("%f+%f ", ix[i].r, ix[i].i);
  } break;
  case STRSXP:{
    SEXP *ix = STRING_PTR(x);
    for (i = 0; i < len; ++i) printf("%s ", CHAR(ix[i]));
  } break;
  default:
    error(_("invalid input"));
  }
  return R_NilValue;
}
