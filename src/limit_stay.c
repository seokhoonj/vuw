#include "vuw.h"

SEXP expand_date(SEXP from, SEXP to) {
  R_xlen_t n, s, i, j, k;
  SEXP z, stay;
  n = XLENGTH(from);
  s = 0, i = 0, j = 0, k = 0;
  switch(TYPEOF(from)) {
  case INTSXP:{
    PROTECT(stay = allocVector(INTSXP, n));
    int *istay = INTEGER(stay);
    int *ifrom = INTEGER(from);
    int *ito   = INTEGER(to);
    for (i = 0; i < n; ++i) {
      istay[i] = ito[i] - ifrom[i] + 1;
      s += istay[i];
    }
    PROTECT(z = allocVector(INTSXP, s));
    int *iz = INTEGER(z);
    for (i = 0; i < s; j++) {
      for (k = 0; k < istay[j]; k++, i++) {
        iz[i] = ifrom[j] + k;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(stay = allocVector(REALSXP, n));
    double *istay = REAL(stay);
    double *ifrom = REAL(from);
    double *ito   = REAL(to);
    for (i = 0; i < n; ++i) {
      istay[i] = ito[i] - ifrom[i] + 1;
      s += istay[i];
    }
    PROTECT(z = allocVector(REALSXP, s));
    double *iz = REAL(z);
    for (i = 0; i < s; j++) {
      for (k = 0; k < istay[j]; k++, i++) {
        iz[i] = ifrom[j] + k;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return z;
}

SEXP limit_stay(SEXP x, SEXP limit, SEXP waiting) {
  // limit: 90, waiting: 90
  // ex) x = c(rep(1, 5), rep(0, 5), rep(1, 8), rep(0, 5), rep(1, 5))
  // ex) count_limit(x, 3, 6)
  // x = c(1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1)
  // c(1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0)
  // waiting periods == elimination periods == qualifying periods
  // printf('%d\n', isInteger(x));
  R_xlen_t n = XLENGTH(x);
  SEXP z;
  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, n));
    int *xs = INTEGER(x), *xe = xs + n;
    int *zs = INTEGER(z);
    int *xi = xs, *zi = zs;
    int lim = asInteger(limit), wait = asInteger(waiting);
    int sum = 0, len = 0;
    while (xi < xe) {
      if (sum < lim) {
        sum += *xi;
        *zi = *xi;
      } else if (len < wait) {
        *zi = 0;
        len += 1;
      } else {
        sum = 0, len = 0;
        sum += *xi;
        *zi = *xi;
      }
      ++zi, ++xi;
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x), *xe = xs + n;
    double *zs = REAL(z);
    double *xi = xs, *zi = zs;
    double lim = asReal(limit), wait = asReal(waiting);
    double sum = 0, len = 0;
    while (xi < xe) {
      if (sum < lim) {
        sum += *xi;
        *zi = *xi;
      } else if (len < wait) {
        *zi = 0;
        len += 1;
      } else {
        sum = 0, len = 0;
        sum += *xi;
        *zi = *xi;
      }
      ++zi, ++xi;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP limit_stay_in_the_interval(SEXP x, SEXP p, SEXP limit, SEXP waiting) {
  // limit = 90; waiting = 90
  // ex) x = rep(1, 100)
  // ex) p = c(30, 40, 30) # split
  // ex) count_limit_and_waiting(x, p, 3, 6)
  // waiting periods == elimination periods == qualifying periods
  // printf("%d\n", isInteger(x));
  R_xlen_t n = XLENGTH(x);
  SEXP z;
  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, n));
    int *ps = INTEGER(p), *pe = ps + XLENGTH(p);
    int *xs = INTEGER(x);
    int *zs = INTEGER(z);
    int *pi = ps;
    int *xi = xs, *zi = zs;
    int lim = asInteger(limit), wait = asInteger(waiting);
    int sum, len;
    while (pi < pe) {
      sum = 0, len = 0;
      for (double i = 0; i < *pi; ++i) {
        if (sum < lim) {
          sum += *xi;
          *zi = *xi;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        } else if (len < wait) {
          *zi = 0;
          len += 1;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        } else {
          sum = 0, len = 0;
          sum += *xi;
          *zi = *xi;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        }
        ++xi, ++zi;
      }
      ++pi;
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, n));
    double *ps = REAL(p), *pe = ps + XLENGTH(p);
    double *xs = REAL(x);
    double *zs = REAL(z);
    double *pi = ps;
    double *xi = xs, *zi = zs;
    double lim = asReal(limit), wait = asReal(waiting);
    double sum, len;

    while (pi < pe) {
      sum = 0, len = 0;
      for (double i = 0; i < *pi; ++i) {
        if (sum < lim) {
          sum += *xi;
          *zi = *xi;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        } else if (len < wait) {
          *zi = 0;
          len += 1;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        } else {
          sum = 0, len = 0;
          sum += *xi;
          *zi = *xi;
          // printf("%f, %f, %f, %f, %f\n", sum, len, *xi, *zi, *pi);
        }
        ++xi, ++zi;
      }
      ++pi;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}
