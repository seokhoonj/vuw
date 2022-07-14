
# data types --------------------------------------------------------------

as_logical   <- function(x) .Call(vuw_as_logical, x)
as_integer   <- function(x) .Call(vuw_as_integer, x)
as_double    <- function(x) .Call(vuw_as_double, x)
as_character <- function(x) .Call(vuw_as_character, x)

# vector to matrix --------------------------------------------------------

colvec <- function(x) array(x, dim = c(length(x), 1L), dimnames = list(names(x), NULL))
rowvec <- function(x) array(x, dim = c(1L, length(x)), dimnames = list(NULL, names(x)))

# matrix ------------------------------------------------------------------

ones    <- function(dim) array(1L, dim = dim)
zeros   <- function(dim) array(0L, dim = dim)
strings <- function(dim) array(NA_character_, dim = dim)
numbers <- function(dim) array(seq_len(prod(dim)), dim = dim)
randoms <- function(dim, x = c(0L, 1L), replace = TRUE, prob = NULL)
  array(sample(x, size = prod(dim), replace = replace, prob = prob), dim = dim)

# names -------------------------------------------------------------------

setdimnames  <- function(x, dimnames) setattr(x, "dimnames", dimnames)
setrownames  <- function(x, rownames) setattr(x, "dimnames", list(rownames, colnames(x)))
setcolnames  <- function(x, colnames) setattr(x, "dimnames", list(rownames(x), colnames))
setfakenames <- function(x, dim) {
  rn <- k_split(seq_len(nrow(x)), k = dim[1L], number = TRUE)
  cn <- k_split(seq_len(ncol(x)), k = dim[2L], number = TRUE)
  setdimnames(x, list(rn, cn))
}

# transform ---------------------------------------------------------------

reverse  <- function(x) invisible(.Call(vuw_reverse, x))
traverse <- function(x, y) .Call(vuw_traverse, x, y)
rotate   <- function(x, angle = 180) {
  z <- .Call(vuw_rotate, x, angle)
  if (angle %% 360 == 90) {
    dn <- dimnames(x)
    dn <- rev(dn)
    dn[[2L]] <- rev(dn[[2L]])
    setdimnames(z, dn)
  } else if (angle %% 360 == 180) {
    dn <- dimnames(x)
    dn <- lapply(dn, rev)
    setdimnames(z, dn)
  } else if (angle %% 360 == 270) {
    dn <- dimnames(x)
    dn[[2L]] <- rev(dn[[2L]])
    dn <- rev(dn)
    setdimnames(z, dn)
  }
  return(z)
}
repcol   <- function(x, each) .Call(vuw_repcol, x, each)
upper    <- function(x, y) .Call(vuw_upper, x, y)
uncumprod <- function(x) c(x[1L], exp(diff(log(x))))
only_first <- function(x, id, ot) .Call(vuw_only_first, x, id, ot)
set_only_first <- function(x, id, ot) invisible(.Call(vuw_set_only_first, x, id, ot))
one_upper_first <- function(x, id) {
  z <- .Call(vuw_one_upper_first, x, id)
  setdimnames(z, dimnames(x))
  return(z)
}
set_one_upper_first <- function(x, id) invisible(.Call(vuw_set_one_upper_first, x, id))


# replication functions ---------------------------------------------------

#' Data replication function
#'
#' This function is for replicating data rows
#' @title Data replication function
#' @param x matrix, data.frame, data.table.
#' @param ... times, each
#' @examples
#' reprow(iris, times = 3)
#' reprow(iris, each  = 3)
#' @export
reprow <- function(x, ...) UseMethod("reprow")

#' @rdname reprow
#' @export
reprow.matrix <- function(x, ...) do.call(cbind, lapply(seq_len(ncol(x)), function(s) rep(x[, s], ...)))
#' @rdname reprow
#' @export
reprow.data.frame <- function(x, ...) as.data.frame(lapply(x, rep, ...)) # times, each

#' @rdname reprow
#' @export
reprow.data.table <- function(x, ...) as.data.table(lapply(x, rep, ...)) # times, each

# row-based statistics ----------------------------------------------------

row_min <- function(x) .Call(vuw_row_min, x)
row_max <- function(x) .Call(vuw_row_max, x)
row_sum <- function(x) .Call(vuw_row_sum, x)

row_min_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  maxval <- max(x)
  .Call(vuw_row_min_by_rn, x, g, uniqueg, na.rm, maxval)
}
row_max_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  minval <- min(x)
  .Call(vuw_row_max_by_rn, x, g, uniqueg, na.rm, minval)
}
row_sum_by_rn <- function(x, na.rm = TRUE) {
  g <- rownames(x)
  uniqueg <- unique(g)
  .Call(vuw_row_sum_by_rn, x, g, uniqueg, na.rm)
}

row_min_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  maxval <- max(x)
  .Call(vuw_row_min_by_cn, x, g, uniqueg, na.rm, maxval)
}
row_max_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  minval <- min(x)
  .Call(vuw_row_max_by_cn, x, g, uniqueg, na.rm, minval)
}
row_sum_by_cn <- function(x, na.rm = TRUE) {
  g <- colnames(x)
  uniqueg <- unique(g)
  .Call(vuw_row_sum_by_cn, x, g, uniqueg, na.rm)
}

row_min_by_dn <- function(x, na.rm = TRUE) {
  row_min_by_rn(row_min_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}
row_max_by_dn <- function(x, na.rm = TRUE) {
  row_max_by_rn(row_max_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}
row_sum_by_dn <- function(x, na.rm = TRUE) {
  row_sum_by_rn(row_sum_by_cn(x, na.rm = na.rm), na.rm = na.rm)
}

# unique ------------------------------------------------------------------

unilen <- function(x) .Call(vuw_unilen, x)
sort_group_by <- function(x) .Call(vuw_sort_group_by, x)

# hash --------------------------------------------------------------------

lookup  <- function(g, uniqueg) .Call(vuw_lookup, g, uniqueg)
hashfun <- function(key, K) bitwShiftL(as.integer(3141592653 * key), (32 - K))

# replace -----------------------------------------------------------------

replace_vec_in_mat <- function(x, col, vec)
  invisible(.Call(vuw_replace_vec_in_mat, x, col, vec))
replace_val_in_mat <- function(mat, val, refmat, refval)
  invisible(.Call(vuw_replace_val_in_mat, mat, val, refmat, refval))

# multiplication ----------------------------------------------------------

setmul <- function(x, y, axis = c(1, 2)) {
  if (!is.matrix(x) | !is.numeric(x) | !is.numeric(y))
    stop("invalid input")
  xdim <- dim(x); ydim <- dim(y)
  if (is.null(ydim)) {
    ylen <- length(y)
    if (ylen > 1) {
      if (xdim[1L] != xdim[2L]) {
        if (xdim[1L] == ylen) {
          ydim <- c(ylen, 1)
          y <- colvec(y)
        } else if (xdim[2L] == ylen) {
          ydim <- c(1, ylen)
          y <- rowvec(y)
        }
      } else {
        if (axis[1L] == 1) {
          ydim <- c(ylen, 1)
          y <- colvec(y)
        } else {
          ydim <- c(1, ylen)
          y <- rowvec(y)
        }
      }
    } else {
      ydim <- c(0, 0)
    }
  }
  if (all(xdim == ydim))
    return(invisible(.Call(vuw_setmul_mat, x, y)))
  if (any(xdim == ydim) & axis[1L] == 1)
    return(invisible(.Call(vuw_setmul_row, x, rowvec(y))))
  if (any(xdim == ydim) & axis[1L] == 2)
    return(invisible(.Call(vuw_setmul_col, x, colvec(y))))
  if (!all(xdim == ydim))
    return(invisible(.Call(vuw_setmul_num, x, y)))
}

# period ------------------------------------------------------------------

ratio_by_period <- function(x, start, end, ratio)
  .Call(vuw_ratio_by_period, x, start, end, ratio)

set_ratio_by_period <- function(x, start, end, ratio)
  invisible(.Call(vuw_set_ratio_by_period, x, start, end, ratio))
