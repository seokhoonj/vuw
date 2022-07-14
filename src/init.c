#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "vuw.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n} // # is a stringify operator

static const R_CallMethodDef callEntries[] = {
  CALLDEF(as_logical, 1),
  CALLDEF(as_integer, 1),
  CALLDEF(as_double, 1),
  CALLDEF(as_character, 1),
  CALLDEF(reverse,  1),
  CALLDEF(traverse, 2),
  CALLDEF(rotate,   2),
  CALLDEF(repcol,   2),
  CALLDEF(upper,    2),
  CALLDEF(row_min,  1),
  CALLDEF(row_max,  1),
  CALLDEF(row_sum,  1),
  CALLDEF(row_min_by_rn, 5),
  CALLDEF(row_max_by_rn, 5),
  CALLDEF(row_sum_by_rn, 4),
  CALLDEF(row_min_by_cn, 5),
  CALLDEF(row_max_by_cn, 5),
  CALLDEF(row_sum_by_cn, 4),
  CALLDEF(only_first, 3),
  CALLDEF(set_only_first, 3),
  CALLDEF(one_upper_first, 2),
  CALLDEF(set_one_upper_first, 2),
  CALLDEF(ratio_by_period, 4),
  CALLDEF(set_ratio_by_period, 4),
  CALLDEF(unilen, 1),
  CALLDEF(lookup, 2),
  CALLDEF(sort_group_by, 1),
  CALLDEF(count_stay, 3),
  CALLDEF(modify_stay, 3),
  CALLDEF(modify_stay_in_the_interval, 4),
  CALLDEF(setmul_mat, 2),
  CALLDEF(setmul_row, 2),
  CALLDEF(setmul_col, 2),
  CALLDEF(setmul_num, 2),
  CALLDEF(replace_vec_in_mat, 3),
  CALLDEF(replace_val_in_mat, 4),
  CALLDEF(index_overlapping_date_range, 4),
  CALLDEF(expand_dates, 2),
  {NULL, NULL, 0}
};

void attribute_visible R_init_raum(DllInfo *info) {
  R_registerRoutines(info, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
