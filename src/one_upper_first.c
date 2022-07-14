#include "vuw.h"

SEXP one_upper_first(SEXP x, SEXP id) {
  R_xlen_t i, j, m, n, p, col, flag;
  SEXP ans;

  m = nrows(x), n = ncols(x), p = XLENGTH(id);

  if (m != p)
    error(_("different length"));

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(ans = allocMatrix(TYPEOF(x), m, n));
    int* ians = INTEGER(ans);
    int* ix = INTEGER(x);
    memset(ians, 0, m * n * sizeof(int));
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ians[col] = 1;
      for (i = 1; i < m; ++i) {
        int pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ians[col + i] = 1;
        } else {
          ians[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(ans = allocMatrix(TYPEOF(x), m, n));
    double* ians = REAL(ans);
    double* ix = REAL(x);
    memset(ians, 0, m * n * sizeof(double));
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ians[col] = 1;
      for (i = 1; i < m; ++i) {
        double pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ians[col + i] = 1;
        } else {
          ians[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return ans;
}

SEXP set_one_upper_first(SEXP x, SEXP id) {
  R_xlen_t i, j, m, n, p, col, flag;

  m = nrows(x), n = ncols(x), p = XLENGTH(id);

  if (m != p)
    Rf_error("different length");

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x);
    int* ix = INTEGER(x);
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ix[col] = 1;
      for (i = 1; i < m; ++i) {
        int pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ix[col + i] = 1;
        } else {
          ix[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x);
    double* ix = REAL(x);
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ix[col] = 1;
      for (i = 1; i < m; ++i) {
        double pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ix[col + i] = 1;
        } else {
          ix[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return R_NilValue;
}
