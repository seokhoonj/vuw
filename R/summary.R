
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
  round(prop.table(table(x)) * 100, digits = digits)
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
