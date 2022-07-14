#include "vuw.h"

SEXP index_overlapping_date_range(SEXP id, SEXP from, SEXP to, SEXP interval) {
  R_xlen_t m, n, i, j;
  SEXP loc, sub, v, z;
  m = XLENGTH(VECTOR_ELT(id, 0)), n = XLENGTH(id);

  int *ifr = INTEGER(from);
  int *ito = INTEGER(to);
  int vinterval = asInteger(interval);

  PROTECT(loc = allocVector(INTSXP, m));
  PROTECT(sub = allocVector(INTSXP, m));
  fillValue(loc, 1);
  fillValue(sub, 0);
  int *iloc = INTEGER(loc);
  int *isub = INTEGER(sub);

  int p = 1, mx = 0; // index, maximum `to`
  bool c1, c2; // condition 1, condition 2
  for (i = 1; i < m; ++i) {
    j = 0, c1 = true;
    while (j < n) {
      v = VECTOR_ELT(id, j);
      switch(TYPEOF(v)){
      case LGLSXP:{
        int *iv = LOGICAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case INTSXP:{
        int *iv = INTEGER(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case REALSXP:{
        double *iv = REAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case STRSXP:{
        SEXP *iv = STRING_PTR(v);
        c1 = (!strcmp(CHAR(iv[i-1]), CHAR(iv[i])));
      } break;
      default:
        error(_("invalid input"));
      }
      if (c1 == false) break;
      j++;
    }
    mx = (ito[i-1] > mx) ? ito[i-1] : mx;
    c2 = ifr[i] <= (mx + 1 + vinterval);
    if (c1 && c2) {
      iloc[i] = p;
      if (ifr[i] > mx) isub[i] = ifr[i] - mx - 1;
    } else {
      iloc[i] = ++p;
      mx = ito[i];
    }
  }
  const char *names[] = {"loc", "sub", ""};
  PROTECT(z = mkNamed(VECSXP, names));
  SET_VECTOR_ELT(z, 0, loc);
  SET_VECTOR_ELT(z, 1, sub);
  UNPROTECT(3);
  return z;
}
