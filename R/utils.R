
# utils -------------------------------------------------------------------

add_folder <- function(folder = c("R", "info", "raw", "data", "output")) {
  width <- max(nchar(folder))
  for (i in seq_along(folder)) {
    if (!file.exists(folder[i])) {
      dir.create(folder[i])
      cat(sprintf("%s folder is created\n",
                  str_pad(folder[i], width = width, side = "right")))
    } else {
      cat(sprintf("%s folder is already exists\n",
                  str_pad(folder[i], width = width, side = "right")))
    }
  }
}

get_options <- function(param) {
  opt_names <- names(options())
  sub_names <- opt_names[grepl(param, opt_names)]
  options()[sub_names]
}

get_files <- function(pattern, folder = getwd())
  sort(dir(folder)[grepl(pattern, dir(folder))])

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

factor_cols <- function(df, cols) {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  df[, (cols) := lapply(.SD, factor), .SDcols = cols]
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

rm_cols_ <- function(df, cols) {
  cols <- match_cols(df, cols)
  df[, `:=`((cols), NULL)]
}

rm_dots <- function(df, cols) {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  df[, `:=`((cols), lapply(.SD, function(x) gsub("\\.", "", x))), .SDcols = cols]
}

rm_dots_ <- function(df, cols) {
  cols <- match_cols(df, cols)
  df[, `:=`((cols), lapply(.SD, function(x) gsub("\\.", "", x))), .SDcols = cols]
}

trim_ws <- function(x, ws = "[ \t\r\n]", perl = FALSE) {
  re <- sprintf("^%s+|%s+$", ws, ws)
  gsub(re, "", x, perl = perl)
}

rm_ws <- function(df, ws = "[ \t\r\n]", perl = FALSE) {
  col <- names(sapply(df, class)[sapply(df, class) == "character"])
  if (length(col) > 0)
    df[, (col) := lapply(.SD, trim_ws, ws = ws, perl = perl), .SDcols = col]
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

replace_na_with_zero <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols]
}
replace_na_with_empty <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class == "character")]
  df[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = cols]
}
replace_empty_with_na <- function(df) {
  class <- sapply(df, class)
  cols <- names(class)[which(class == "character")]
  df[, (cols) := lapply(.SD, function(x) ifelse(x == "", NA, x)), .SDcols = cols]
}

set_rowsum  <- function(df, cols, name = "sum") {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  set(df, j = name, value = apply(df[, ..cols], 1, sum))
}
set_rowsum_ <- function(df, cols, name = "sum") {
  set(df, j = name, value = apply(df[, ..cols], 1, sum))
}
set_cumsum  <- function(df, group_var, value_var, prefix = "c") {
  group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  value_var <- match_cols(df, vapply(substitute(value_var), deparse, "character"))
  cols <- sprintf("%s%s", prefix, value_var)
  df[, (cols) := lapply(.SD, cumsum), group_var, .SDcols = value_var]
}
set_cumsum_ <- function(df, group_var, value_var, prefix = "c") {
  cols <- sprintf("%s%s", prefix, value_var)
  df[, (cols) := lapply(.SD, cumsum), group_var, .SDcols = value_var]
}
set_prop  <- function(df, cols, sum_col) {
  cols <- match_cols(df, vapply(substitute(cols), deparse, "character"))
  sum_col <- match_cols(df, vapply(substitute(sum_col), deparse, "character"))
  name <- sprintf("%s_prop", cols)
  df[, (name) := lapply(.SD, function(x) x / get(sum_col)), .SDcols = cols]
}
set_prop_ <- function(df, cols, sum_col) {
  name <- sprintf("%s_prop", cols)
  df[, (name) := lapply(.SD, function(x) x / get(sum_col)), .SDcols = cols]
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

expand.grid.df <- function(...) Reduce(function(...) merge(..., by = NULL), list(...))

minmax_scaler <- function(x) if (length(x) > 1) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) else 0

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
bmonth <- function(x) as.Date(format(as.Date(x), format = "%Y-%m-01"))
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
  cat(sprintf("%02d:%02d:%02d.%s\n", h, m, trunc(s), d))
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

# word --------------------------------------------------------------------

break_word <- function(x, len = 15) {
  n <- ceiling(nchar(x) / len)
  v <- vector(mode = "character", length = length(n))
  for (j in 1:length(v)) {
    w <- vector(mode = "character", length = n[j])
    for (i in 1:length(w)) {
      w[i] <- substr(x[j], len*(i-1)+1, len*i)
    }
    v[j] <- paste0(w, collapse = "\n")
  }
  v
}

# comma utils -------------------------------------------------------------

# quote_comma(x, y, z)
# paste_comma(c("x", "y", "z"))

quote_comma <- function(..., newline = FALSE) {
  if (newline) {
    cat(paste0("\"", paste(vapply(substitute(list(...)), deparse, "character")[-1L], collapse = '",\n"'), "\""))
  } else {
    cat(paste0("\"", paste(vapply(substitute(list(...)), deparse, "character")[-1L], collapse = '", "'), "\""))
  }
}

paste_comma <- function(x, newline = FALSE) {
  if (newline) {
    cat(paste0("\"", paste(x, collapse = '",\n"'), "\""))
  } else {
    cat(paste0("\"", paste(x, collapse = '", "'), "\""))
  }
}

# setnames ----------------------------------------------------------------

setlowernames <- function(x) setnames(x, colnames(x), tolower(colnames(x)))
setuppernames <- function(x) setnames(x, colnames(x), toupper(colnames(x)))


