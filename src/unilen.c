#include "vuw.h"

// refer to 'kit'
SEXP unilen(SEXP x) {
  if (isFactor(x)) {
    const int len = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *count = (bool*)Calloc(len+1, bool);
    const int *px = INTEGER(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      if (!count[px[i]]) {
        j++;
        if (j == len)
          break;
        count[px[i]] = true;
      }
    }
    Free(count);
    return ScalarInteger(j);
  }
  if (isLogical(x)) {
    bool *count = (bool*)Calloc(3, bool);
    const int *px = LOGICAL(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
      if (!count[cs]) {
        j++;
        if (j == 3)
          break;
        count[cs] = true;
      }
    }
    Free(count);
    return ScalarInteger(j);
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  int K;
  size_t M;
  if (tx == INTSXP || tx == REALSXP || tx == STRSXP) {
    const size_t n2 = 2U * (size_t) n;
    M = 256;
    K = 8;
    while (M < n2) {
      M *= 2;
      K++;
    }
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported", type2char(tx));
  }
  R_xlen_t count = 0;
  int* h = (int*)Calloc(M, int);
  switch(tx) {
  case INTSXP:{
    const int* px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1] == px[i]) {
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      ibl:;
    }
  } break;
  case REALSXP:{
    const double* px = REAL(x);
    size_t id = 0;
    union uno tpv;
    for (int i = 0; i < n; i++) {
      tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN : px[i]);
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        if (REQUAL(px[h[id]-1], px[i])) {
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      rbl:;
    }
  } break;
  case STRSXP:{
    const SEXP *px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff), K);
      while (h[id]) {
        if (px[h[id] - 1]==px[i]) {
          goto sbl;
        }
        id++; id %=M;
      }
      h[id] = (int) i + 1;
      count++;
      sbl:;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  Free(h);
  return ScalarInteger(count);
}

