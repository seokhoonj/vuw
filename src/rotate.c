#include "vuw.h"

SEXP rotate(SEXP x, SEXP angle) {
  R_xlen_t i, j, n, m, p, degree;
  SEXP z;

  n = nrows(x), m = ncols(x), p = XLENGTH(x);
  degree = asInteger(angle);

  if (degree % 360 == 0)
    return x;

  switch(TYPEOF(x)){
    case LGLSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(LGLSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = n-1; i < p; ++i, j += n) {
          iz[i] = ix[j];
          if (j > p-n) j -= (p+1); // add j +=n right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(LGLSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(LGLSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-n; i < p; ++i, j -= n) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case INTSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(INTSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = n-1; i < p; ++i, j += n) {
          iz[i] = ix[j];
          if (j > p-n) j -= (p+1); // add j +=n right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(INTSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(INTSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-n; i < p; ++i, j -= n) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case REALSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(REALSXP, m, n));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = n-1; i < p; ++i, j += n) {
          iz[i] = ix[j];
          if (j > p-n) j -= (p+1); // add j +=n right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(REALSXP, n, m));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
        setAttrib(z, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(REALSXP, m, n));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = p-n; i < p; ++i, j -= n) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case STRSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(STRSXP, m, n));
        for (i = 0, j = n-1; i < p; ++i, j += n) {
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
          if (j > p-n) j -= (p+1); // add j +=n right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(STRSXP, n, m));
        for (i = 0, j = p-1; i < p; ++i, --j) {
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(STRSXP, m, n));
        for (i = 0, j = p-n; i < p; ++i, j -= n) {
          if (j < 0) j += (p+1);
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    default:{
      error("invalid input");
    }
  }
  UNPROTECT(1);
  return z;
}
