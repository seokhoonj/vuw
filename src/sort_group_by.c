#include "vuw.h"

int compare(const void *a, const void *b) {
  return *(int*)a - *(int*)b;
}

SEXP iunique(int arr[], size_t n) {
  const int n2 = 2U * (int) n;
  int M = 256;
  size_t K = 8;
  while (M < n2) {
    M *= 2;
    K++;
  }
  size_t count = 0;
  int *h = (int*)Calloc(M, int);
  int *p = (int*)Calloc(n, int);
  for (int i = 0; i < n; ++i) {
    int id = (arr[i] == INT_MIN) ? 0 : HASH(arr[i], K);
    while (h[id]) {
      if (arr[h[id]-1]==arr[i]) {
        goto iblt;
      }
      id++, id %= M;
    }
    h[id] = (int) i + 1;
    p[i]++;
    count++;
    iblt:;
  }
  size_t ct = 0;
  int *z = (int*)Calloc(count, int);
  for (int i = 0; ct < count; ++i) {
    if (p[i]) {
      z[ct++] = arr[i];
    }
  }
  SEXP ans;
  PROTECT(ans = allocVector(INTSXP, ct));
  int *pans = INTEGER(ans);
  for (int i = 0; i < ct; ++i) {
    pans[i] = z[i];
  }
  free(h); free(p); free(z);
  UNPROTECT(1);
  return ans;
}

SEXP sort_group_by(SEXP id) {
  R_xlen_t i, j, m, n, p = 0;
  SEXP pos, v;
  switch(TYPEOF(id)) {
  case VECSXP:{
    m = XLENGTH(VECTOR_ELT(id, 0)), n = XLENGTH(id);
    int *ipos = (int*)Calloc(m*n, int);
    ipos[p++] = 0;
    for (i = 0; i < n; ++i) {
      v = VECTOR_ELT(id, i);
      switch(TYPEOF(v)){
      case LGLSXP:{
        int *iv = LOGICAL(v);
        for (j = 0; j < m-1; ++j) {
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
        }
      } break;
      case INTSXP:{
        int *iv = INTEGER(v);
        for (j = 0; j < m-1; ++j) {
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
        }
      } break;
      case REALSXP:{
        double *iv = REAL(v);
        for (j = 0; j < m-1; ++j) {
          if (!REQUAL(iv[j], iv[j+1])) ipos[p++] = j+1;
        }
      } break;
      case STRSXP:{
        SEXP *iv = STRING_PTR(v);
        for (j = 0; j < m-1; ++j) {
          if (strcmp(CHAR(iv[j]), CHAR(iv[j+1]))) ipos[p++] = j+1;
        }
      } break;
      default:
        error(_("invalid input"));
      }
    }
    ipos[p++] = m;
    int *jpos = (int*)realloc(ipos, sizeof(int)*p);
    qsort(jpos, p, sizeof(int), compare);
    pos = iunique(jpos, p);
  } break;
  default:
    error(_("invalid input"));
  }
  return pos;
}
