
# utils -------------------------------------------------------------------

get_options <- function(param) {
  opt_names <- names(options())
  sub_names <- opt_names[grepl(param, opt_names)]
  options()[sub_names]
}

load_rds <- function(file, refhook = NULL)
  alloc.col(readRDS(file, refhook))

assert_type <- function(obj, type) {
  obj_name <- deparse(substitute(obj))
  if (typeof(obj) != type) {
    stop(obj_name, " is not an object of type: '",
      paste0(type, collapse = ", "), "'",
      call. = FALSE)
  }
}

assert_class <- function(obj, class) {
  obj_name <- deparse(substitute(obj))
  if (!inherits(obj, class)) {
    stop(obj_name, " is not an object of class: '",
      paste0(class, collapse = ", "), "'",
      call. = FALSE)
  }
}

match_class <- function(df, dtype) {
  if (missing(dtype))
    stop("no 'dtype' argument")
  dtype <- dtypes[match(tolower(dtype),
    c("integer", "numeric", "character", "list", "date"),
    nomatch = 0L)]
  if (length(dtype) == 0)
    stop("invalid 'dtype' argument")
  names(df)[which(tolower(sapply(df, class)) %in% dtype)]
}

has_rows <- function(df) {
  df_name <- deparse(substitute(df))
  if (!nrow(df)) {
    stop("'", df_name, "' doesn't have row(s): ",
      call. = FALSE)
  }
}

has_cols <- function(df, cols) {
  df_name <- deparse(substitute(df))
  df_cols <- colnames(df)
  diff_cols <- setdiff(cols, df_cols)
  if (length(diff_cols) > 0) {
    stop("'", df_name, "' doesn't have column(s): ",
      paste0(diff_cols, collapse = ", "), ".",
      call. = FALSE)
  }
}

has_missing <- function(x) {
  column_name <- deparse(substitute(x))
  if (any(is.na(x))) {
    stop("'", column_name, "' has missing value(s): ",
      call. = FALSE)
  }
}

match_cols <- function(df, cols) {
  colnames(df)[match(cols, colnames(df), 0L)]
}

regmatch_cols <- function(df, pattern) {
  colnames(df)[grepl(pattern, names(df), perl = TRUE)]
}

diff_cols <- function(df, cols) {
  setdiff(colnames(df), cols)
}

icol <- function(df, cols) {
  has_cols(df, cols)
  unlist(lapply(unique(cols), function(x) which(colnames(df) == x)))
}

rm_cols <- function(df, cols) {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  df[, `:=`((cols), NULL)]
}

rm_dots <- function (df, cols) {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  df[, `:=`((cols), lapply(.SD, function(x) gsub("\\.", "", x))), .SDcols = cols]
}

equal <- function(x, y) {
  if (any(colnames(x) != colnames(y)))
    stop("different column names")
  sapply(colnames(x), function(col) all(x[[col]]==y[[col]]))
}

setcolafter <- function(df, cols, after = NA) {
  cols  <- match_cols(df, vapply(substitute(cols) , deparse, "character"))
  after <- match_cols(df, vapply(substitute(after), deparse, "character"))
  all_cols <- colnames(df)
  cols_pos <- sapply(cols, function(x) which(all_cols == x), USE.NAMES = FALSE)
  rest_pos <- which(!all_cols %in% cols)
  if (missing(after)) {
    neworder <- c(cols_pos, rest_pos)
  } else {
    after_pos <- which(all_cols == after)
    head_order <- rest_pos[rest_pos <= after_pos]
    tail_order <- rest_pos[rest_pos  > after_pos]
    new_order <- c(head_order, cols_pos, tail_order)
  }
  new_cols <- all_cols[new_order]
  setcolorder(df, new_cols)
}

setcolafter_ <- function(df, cols, after = NA) {
  all_cols <- colnames(df)
  cols_pos <- sapply(cols, function(x) which(all_cols == x), USE.NAMES = FALSE)
  rest_pos <- which(!all_cols %in% cols)
  if (missing(after)) {
    neworder <- c(cols_pos, rest_pos)
  } else {
    after_pos <- which(all_cols == after)
    head_order <- rest_pos[rest_pos <= after_pos]
    tail_order <- rest_pos[rest_pos  > after_pos]
    new_order <- c(head_order, cols_pos, tail_order)
  }
  new_cols <- all_cols[new_order]
  setcolorder(df, new_cols)
}

replace_num_na <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols]
}
replace_chr_na <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class == "character")]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = cols]
}
set_sum <- function(df, cols, value_name = "sum") {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  set(df, j = value_name, value = apply(df[, ..cols], 1, sum))
}
set_sum_ <- function(df, cols, value_name = "sum") {
  set(df, j = value_name, value = apply(df[, ..cols], 1, sum))
}

#' Merge data frames
#'
#' This function merges several data frames at once
#' @param ... data frames, or objects to be coerced to one.
#' @param by specifications of the columns used for merging.
#' @param all logical; all = L is shorthand for all.x = L and all.y = L, where L is either TRUE or FALSE.
#' @param all.y logical; analogous to all.x.
#' @param sort  logical. Should the result be sorted on the by columns?
join <- function(..., by, all = FALSE, all.x = all, all.y = all, sort = TRUE) {
  Reduce(function(...) merge(..., by = by, all = all, all.x = all.x, all.y = all.y, sort = sort), list(...))
}

minmax_scaler <- function(x) if (length(x) > 1) (x - min(x)) / (max(x) - min(x)) else 0

# vector ------------------------------------------------------------------

nolast <- function(x) x[-length(x)]

change_point <- function(x) which(x[-1L] != x[-length(x)]) + 1

change_interval <- function(x) diff(c(1, change_point(x), length(x)+1))

vseq <- function(from, to, by = 1L) {
  if (length(from) != length(to))
    stop("Two vectors have a different length.")
  lapply(seq_along(from), function(x) seq(from[x], to[x], by))
}

# date --------------------------------------------------------------------

num2date <- function(x) as.Date(x, origin = "1970-01-01")
add_year <- function(date, year) {
  date <- as.POSIXlt(date)
  date$year <- date$year + year
  as.Date(date)
}
add_mon <- function(date, mon) {
  date <- as.POSIXlt(date)
  date$mon <- date$mon + mon
  as.Date(date)
}
bmonth <- function(x) as.Date(format(x, format = "%Y-%m-01"))
emonth <- function(x) add_mon(x, 1L) - 1L
expand_date <- function(from, to) {
  if (inherits(from, "character"))
    from <- as.Date(from)
  if (inherits(to, "character"))
    to <- as.Date(to)
  num2date(.Call(vuw_expand_date, from, to)) # not a unique value
}

# time --------------------------------------------------------------------

sec_to_hms <- function(sec) {
  h <- sec %/% (60^2)
  r <- sec %%  (60^2)
  m <- r   %/% (60)
  s <- r   %%  (60)
  d <- s - trunc(s)
  d <- substr(sub("0.", "", as.character(d)), 1, 5)
  cat(sprintf("%02d:%02d:%02d.%s", h, m, trunc(s), d))
}
#' Simple running time of {vuw} package
#'
#' @param expr Valid \R expression to be timed
#' @examples
#' proc_time(df <- iris)
#' @export
proc_time <- function(expr) {
  stime <- as.numeric(Sys.time())
  eval(expr)
  etime <- as.numeric(Sys.time())
  sec_to_hms(etime - stime)
}

# comma utils -------------------------------------------------------------

# quote_comma(x, y, z)
# paste_comma(c("x", "y", "z"))

quote_comma <- function(...)
  cat(paste0("\"", paste(vapply(substitute(list(...)), deparse, "character")[-1L], collapse = '", "'), "\""))

paste_comma <- function(x)
  cat(paste0("\"", paste(x, collapse = '", "'), "\""))
