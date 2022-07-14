#include "vuw.h"

SEXP reverse(SEXP x) {
  R_xlen_t i, size;

  size = XLENGTH(x);
  PROTECT(x);

  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = LOGICAL(x);
    int tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    int tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    double tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case STRSXP:{
    SEXP *ix = STRING_PTR(x);
    SEXP tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return x;
}
