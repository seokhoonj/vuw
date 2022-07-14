#include "vuw.h"

SEXP count_stay(SEXP id, SEXP from, SEXP to) {
  SEXP pos, stay;
  PROTECT(pos = sort_group_by(id));
  int nrow = XLENGTH(pos);
  int *pp = INTEGER(pos);
  int *pf = INTEGER(from);
  int *pt = INTEGER(to);

  PROTECT(stay = allocVector(INTSXP, nrow-1));
  int *ps = INTEGER(stay);
  int p = 0;
  int *days = (int*)calloc(100000, sizeof(int));
  int *day = (int*)calloc(10000, sizeof(int));
  for (int k = 0; k < nrow-1; ++k) {
    int q = 0;
    for (int j = pp[k]; j < pp[k+1]; ++j) {
      int m = pt[j] - pf[j] + 1;
      day[0] = pf[j];
      days[q++] = day[0];
      for (int i = 1; i < m; ++i) {
        day[i] = day[i-1] + 1;
        days[q++] = day[i];
      }
    }
    ps[p++] = length(iunique(days, q));
  }
  free(day); free(days);
  UNPROTECT(2);
  return stay;
}
