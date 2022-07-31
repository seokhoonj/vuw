
# information functions ---------------------------------------------------

get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (inherits(x, "character")) x <- x[x != ""]
  if (inherits(x, "Date"))      x <- as.character(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

get_class <- function(x) {
  assert_class(x, "data.frame")
  column <- names(x)
  class <- sapply(x, class)
  type <- sapply(x, typeof)
  data.table(column, class, type)
}

get_info <- function(x) {
  assert_class(x, "data.frame")
  column <- names(x)
  class <- sapply(x, class)
  type <- sapply(x, typeof)
  nrows <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, get_mode)
  data.table(column, class, type, n, missing, distinct,
             prop = 1-missing/nrows, mode)
}

prop_table <- function(x, digits = 2) {
  v <- unilen(x)
  if (v >= 1000)
    stop("Distinct values >= 100")
  round(prop.table(table(x, useNA = "ifany")) * 100, digits = digits)
}

group_binary <- function(df, cols) {
  assert_class(df, "data.table")
  if (!missing(cols)) {
    cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  } else {
    cols <- names(df)[which(sapply(df, function(x) any(is.na(x))))]
  }
  nrows <- nrow(df)
  z <- data.table(sapply(df, function(x) as.factor(ifelse(is.na(x), 0, 1))))
  z[, .(n = .N, prop = .N / nrows), cols]
}

group_binary_ <- function(df, cols) {
  assert_class(df, "data.table")
  if (missing(cols))
    cols <- names(df)[which(sapply(df, function(x) any(is.na(x))))]
  nrows <- nrow(df)
  z <- data.table(sapply(df, function(x) as.factor(ifelse(is.na(x), 0, 1))))
  z[, .(n = .N, prop = .N / nrows), cols]
}

group_stats <- function(df, group_var, value_var, fun.aggregate = sum) {
  assert_class(df, "data.table")
  group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  value_var <- match_cols(df, vapply(substitute(value_var), deparse, "character"))
  df[, lapply(.SD, fun.aggregate), keyby = group_var, .SDcols = value_var]
}

group_stats_ <- function(df, group_var, value_var, fun.aggregate = sum) {
  assert_class(df, "data.table")
  df[, lapply(.SD, fun.aggregate), keyby = group_var, .SDcols = value_var]
}

get_prop <- function(df, group_var, uniq_var, sum_var, multiple = 1) {                                                                                group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  assert_class(df, "data.table")
  if (!missing(uniq_var)) {
    uniq_var <- match_cols(df, vapply(substitute(uniq_var), deparse, "character"))
    if (!missing(sum_var)) {
      sum_var <- match_cols(df, vapply(substitute(sum_var), deparse, "character"))
      z <- df[, .(n = .N,
                  uniq_n = uniqueN(get(uniq_var)),
                  sum = sum(get(sum_var)),
                  sum_per_uniq_n = sum(get(sum_var)) / uniqueN(get(uniq_var))),
              by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "uniq_n_prop", value = z$uniq_n / uniqueN(df[[uniq_var]]) * multiple)
      set(z, j = "sum_prop"   , value = z$sum    / sum(z$sum)    * multiple)
    } else {
      z <- df[, .(n = .N,
                  uniq_n = uniqueN(get(uniq_var))),
              by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "uniq_n_prop", value = z$uniq_n / uniqueN(df[[uniq_var]]) * multiple)
    }
  } else {
    if (!missing(sum_var)) {
      sum_var <- match_cols(df, vapply(substitute(sum_var), deparse, "character"))
      z <- df[, .(n = .N, sum = sum(get(sum_var))), by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "sum_prop"   , value = z$sum    / sum(z$sum)    * multiple)
    } else {
      z <- df[, .(n = .N), by = group_var]
      set(z, j = "prop", value = z$n / sum(z$n) * multiple)
    }
  }
  setorderv(z, group_var)
  return(z)
}

get_prop_ <- function(df, group_var, uniq_var, sumivar, multiple = 1) {
  group_var <- match_cols(df, group_var)
  if (!missing(uniq_var)) {
    if (!missing(sum_var)) {
      z <- df[, .(n = .N,
                  uniq_n = uniqueN(get(uniq_var)),
                  sum = sum(get(sum_var)),
                  sum_per_uniq_n = sum(get(sum_var)) / uniqueN(get(uniq_var))),
              by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "uniq_n_prop", value = z$uniq_n / uniqueN(df[[uniq_var]]) * multiple)
      set(z, j = "sum_prop"   , value = z$sum    / sum(z$sum)    * multiple)
    } else {
      z <- df[, .(n = .N,
                  uniq_n = uniqueN(get(uniq_var))),
              by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "uniq_n_prop", value = z$uniq_n / uniqueN(df[[uniq_var]]) * multiple)
    }
  } else {
    if (!missing(sum_var)) {
      z <- df[, .(n = .N, sum = sum(get(sum_var))), by = group_var]
      set(z, j = "n_prop"     , value = z$n      / sum(z$n)      * multiple)
      set(z, j = "sum_prop"   , value = z$sum    / sum(z$sum)    * multiple)
    } else {
      z <- df[, .(n = .N), by = group_var]
      set(z, j = "prop", value = z$n / sum(z$n) * multiple)
    }
  }
  setorderv(z, group_var)
  return(z)
}
