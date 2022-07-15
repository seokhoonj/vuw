
#ifndef VUW_VUW_H
#define VUW_VUW_H

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#define USE_RINTERNALS 1

#ifdef WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#endif

/* for a message translation */
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("vuw", String)
#else
#define _(String) (String)
#endif

union uno { double d; unsigned int u[2]; };

/* functions */
#define UTYPEOF(x) ((unsigned)TYPEOF(x))
#define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x)) // to avoid overhead of looped STRING_ELT ans VECTOR_ELT
#define IS_BOOL(x) (LENGTH(x)==1 && TYPEOF(x)==LGLSXP && LOGICAL(x)[0]!=NA_LOGICAL)
#define N_ISNAN(x, y) (!ISNAN(x) && !ISNAN(y))
#define B_IsNA(x, y) (R_IsNA(x) && R_IsNA(y)) // both
#define B_IsNaN(x, y) (R_IsNaN(x) && R_IsNaN(y)) // both
#define REQUAL(x, y) (N_ISNAN(x, y) ? (x == y) : (B_IsNA(x, y) || B_IsNaN(x, y)))
#define HASH(key, K) (3141592653U * (unsigned int)(key) >> (32 - (K)))


/* Error messages */
#define R_ERR_MSG_NA	_("NaNs produced")

/* C */
#ifdef __cplusplus
extern "C" {
#endif

SEXP printArray(SEXP x);
SEXP fillValue(SEXP x, int value);
SEXP dupVecR(SEXP x, SEXP uniq, SEXP fromLast);
SEXP iunique(int arr[], size_t n);

SEXP as_logical(SEXP x);
SEXP as_integer(SEXP x);
SEXP as_double(SEXP x);
SEXP as_character(SEXP x);
SEXP reverse(SEXP x);
SEXP traverse(SEXP x, SEXP y);
SEXP rotate(SEXP x, SEXP angle);
SEXP repcol(SEXP x, SEXP each);
SEXP upper(SEXP x, SEXP y);
SEXP row_min(SEXP x);
SEXP row_max(SEXP x);
SEXP row_sum(SEXP x);
SEXP row_min_by_rn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval);
SEXP row_max_by_rn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval);
SEXP row_sum_by_rn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm);
SEXP row_min_by_cn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval);
SEXP row_max_by_cn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval);
SEXP row_sum_by_cn(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm);
SEXP only_first(SEXP x, SEXP id, SEXP ot);
SEXP set_only_first(SEXP x, SEXP id, SEXP ot);
SEXP one_upper_first(SEXP x, SEXP id);
SEXP set_one_upper_first(SEXP x, SEXP id);
SEXP ratio_by_period(SEXP x, SEXP start, SEXP end, SEXP ratio);
SEXP set_ratio_by_period(SEXP x, SEXP start, SEXP end, SEXP ratio);
SEXP setmul_mat(SEXP x, SEXP y);
SEXP setmul_row(SEXP x, SEXP y);
SEXP setmul_col(SEXP x, SEXP y);
SEXP setmul_num(SEXP x, SEXP y);
SEXP unilen(SEXP x);
SEXP lookup(SEXP g, SEXP uniqueg);
SEXP sort_group_by(SEXP x);
SEXP count_stay(SEXP id, SEXP from, SEXP to);
SEXP modify_stay(SEXP x, SEXP limit, SEXP waiting);
SEXP modify_stay_in_the_interval(SEXP x, SEXP p, SEXP limit, SEXP waiting);
SEXP replace_vec_in_mat(SEXP mat, SEXP col, SEXP vec);
SEXP replace_val_in_mat(SEXP mat, SEXP val, SEXP refmat, SEXP refval);
SEXP index_date_overlap(SEXP id, SEXP from, SEXP to, SEXP interval);
SEXP expand_dates(SEXP from, SEXP to);

#endif // VUW_VUW_H

#ifdef __cplusplus
}
#endif

// no	SEXPTYPE   Description
//  0	NILSXP     NULL
//  1	SYMSXP     symbols
//  2	LISTSXP    pairlists
//  3	CLOSXP     closures
//  4	ENVSXP     environments
//  5	PROMSXP	   promises
//  6	LANGSXP	   language objects
//  7	SPECIALSXP special functions
//  8	BUILTINSXP builtin functions
//  9	CHARSXP    internal character strings
// 10	LGLSXP     logical vectors
// 13	INTSXP     integer vectors
// 14	REALSXP    numeric vectors
// 15	CPLXSXP    complex vectors
// 16	STRSXP     character vectors
// 17	DOTSXP     dot-dot-dot object
// 18	ANYSXP     make “any” args work
// 19	VECSXP     list (generic vector)
// 20	EXPRSXP    expression vector
// 21	BCODESXP   byte code
// 22	EXTPTRSXP  external pointer
// 23	WEAKREFSXP weak reference
// 24	RAWSXP     raw vector
// 25	S4SXP      S4 classes not of simple type

// 64 bit
// int               :  4 byte
// unsigned int      :  4 byte
// long int          :  8 byte
// unsigned long int :  8 byte
// long long int     :  8 byte
// float             :  4 byte
// double            :  8 byte
// long double       : 16 byte
// (void *)          :  8 byte

// INT_MAX = 2147483647
// UINT_MAX = 4294967295d
// LONG_MAX = 9223372036854775807
// ULONG_MAX = 18446744073709551615d
