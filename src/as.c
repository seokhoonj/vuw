#include "vuw.h"

// same as 'as.' but not break the shape
SEXP as_logical(SEXP x) {
  return Rf_coerceVector(x, LGLSXP);
}

SEXP as_integer(SEXP x) {
  return Rf_coerceVector(x, INTSXP);
}

SEXP as_double(SEXP x) {
  return Rf_coerceVector(x, REALSXP);
}

SEXP as_character(SEXP x) {
  return Rf_coerceVector(x, STRSXP);
}
