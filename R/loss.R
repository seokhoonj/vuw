
#' Pull covered kcd code
#'
#' This function pulled the covered kcd code from the kcd code vector
#' @param cvd_kcd covered kcd code. regular expression
#' @param kcd kcd code vector
#' @return the return type is a character vector
#' @examples
#' pull_cvd_kcd("A05|B03", kcd_book$code)
#' @export
pull_cvd_kcd <- function(cvd_kcd, kcd) {
  m <- length(kcd); n <- length(cvd_kcd)
  z <- strings(c(m, n))
  for (i in seq_along(cvd_kcd)) {
    replace_vec_in_mat(z, i, unlist(
      lapply(regmatches(kcd, gregexpr(paste0(cvd_kcd[i], "[.0-9]*"), kcd, perl = TRUE)),
             paste0, collapse = '|')))
  }
  return(z)
}

#' Check covered kcd code
#'
#' This function checks the covered kcd code from the kcd code vector
#' @param cvd_kcd covered kcd code vector
#' @param kcd kcd code vector
#' @return the return type is a binary matrix
#' @examples
#' head(check_cvd_kcd(c("A00", "A01", "A02"), kcd_book$kcd), 20)
#' @export
check_cvd_kcd <- function(cvd_kcd, kcd) {
  m <- length(kcd); n <- length(cvd_kcd)
  z <- zeros(c(m, n))
  for (i in seq_along(cvd_kcd)) {
    replace_vec_in_mat(z, i, grepl(cvd_kcd[i], kcd, perl = T))
  }
  return(z)
}

#' Check surgery level
#'
#' This function checks the covered surgery level from the surgery level vector
#' @param level covered surgery level
#' @param kcd surgery level vector
#' @return the return type is a binary matrix
#' @examples
#' head(check_sur_lvl(c(1, 2, 3, 4, 5), c(1, 1, 3, 2, 4, 5, 2, 3, 2, 5)), 20)
#' @export
check_cvd_level <- function(cvd_level, level) {
  m <- length(level); n <- length(cvd_level)
  z <- zeros(c(m, n))
  for (i in seq_along(cvd_level)) {
    replace_vec_in_mat(z, i, grepl(cvd_level[i], level, perl = T))
  }
  return(z)
}

#' Overlap matrix
#'
#' This function overlaps two matrices with same dimension by a binary vector.\cr
#' There are two claim methods, one for one-time payment and the other for continuous payments.\cr
#' Therefore, we have to combine the two matrices separately according to whether it is a one-time payment or continuous payments.
#' @param clm     a matrix. Claim matrix
#' @param clm_1st a matrix. Claim matrix only the first claim is 1 and the rest are 0 for each column and each row id
#' @param col     a binary vector. it is 1 if it is one-time payment, it is 0 if it is continuous payment
#' @examples
#' @export
overlap_matrix <- function(clm, clm_1st, col) {
  if (any(col == 1))
    col <- which(col == 1L)
  clm[, col] <- clm_1st[, col]
  return(clm)
}

#' Differences between start and end
#'
#' @param udate Date
#' @param sdate Date
#' @param ncol  integer. The number of columns you need to create
#' @param mon   integer. The months
#' @return Period matrix
#' @examples
#' @export
diff_period <- function(start, end, group = 1) {
  assert_class(start, "Date")
  assert_class(end, "Date")
  ys <- year(start)
  ye <- year(end)
  ms <- month(start)
  me <- month(end)
  z <- as.integer(ceiling(((ye-ys) * 12 + (me-ms) + 1) / group))
  return(z)
}

period_to_binary_loss <- function(period, one_time, expiration) { # period after period[clm == 0] <- 0
  # row names
  rn <- rownames(period)
  # sum
  set_one_upper_first(period, rn)
  clm_row_cnt <- row_sum_by_rn(period) # one_time is not applied
  # apply expiration condition
  clm_row_exp <- repcol(apply_expiration(clm_row_cnt, expiration),
                        each = change_interval(colnames(clm_row_cnt)))
  # maximum payment
  row_interval <- change_interval(rn)
  clm_row_cap  <- structure(repcol(colvec(row_interval), ncol(clm_row_cnt)), dimnames = dimnames(clm_row_cnt))
  # continuous vs one_time
  clm_row_cmb <- overlap_matrix(clm_row_cap, clm_row_exp, one_time)
  # binary matrix
  period_gap <- clm_row_cap - clm_row_cmb # differences
  clm_trv <- traverse(clm_row_cmb, period_gap)
  bin_trv <- rep(c(1L, 0L), length(clm_row_cmb)) # binary
  matrix(rep(bin_trv, times = clm_trv), ncol = ncol(clm_row_cmb),
         dimnames = list(rep(rownames(clm_row_cmb), times = row_interval), colnames(clm_row_cmb)))
}

leave_positive_loss <- function(loss_obj) { # leave rows the sum of rows are greater than 0
  loss_obj_name <- deparse(substitute(loss_obj))
  loss_obj_cols <- regmatch_cols(loss_obj, "^loss")
  text <- sprintf("%s[!(%s)]", loss_obj_name,
                  paste0(paste(loss_obj_cols, "==", 0), collapse = " & "))
  eval(parse(text = text))
}

loss_simulation <- function(claim_info, df, udate, mon = 60, group = 1, unit = 1, waiting = TRUE, lapse,
                            stay_var, level_var, type = c("data.table", "matrix")) {
  # check
  assert_class(claim_info, "data.table")
  assert_class(df, "data.table")
  has_rows(claim_info)
  has_rows(df)
  # order
  setorder(df, id, sdate, edate, kcd)
  # claim information variables
  rn                     <- claim_info$rn
  cvd_kcd                <- claim_info$cvd_kcd
  cvd_level              <- claim_info$cvd_level
  reduction_period_start <- claim_info$reduction_period_start
  reduction_period_end   <- claim_info$reduction_period_end
  reduction_period_ratio <- claim_info$reduction_period_ratio
  waiting_period_start   <- claim_info$waiting_period_start
  waiting_period_end     <- claim_info$waiting_period_end
  one_time               <- claim_info$one_time
  expiration             <- claim_info$expiration
  amount_mean            <- claim_info$amount_mean
  claim_times            <- claim_info$claim_times
  # claim information variables derived
  width <- max(nchar(rn))
  rd    <- sprintf(paste0("loss%0", width, "s"), rn) # for expiration table
  if (missing(level_var)) {
    label <- sprintf("%s_%s", rd, cvd_kcd)
  } else {
    label <- sprintf("%s_%s_%s", rd, cvd_kcd, cvd_level) # surgery level
  }
  # data variables
  id    <- df$id
  kcd   <- df$kcd
  sdate <- df$sdate
  # dimensions
  m <- nrow(claim_info)
  n <- nrow(df)
  # claim
  if (missing(level_var)) {
    cat("checking covered kcd...\n")
    clm <- check_cvd_kcd(cvd_kcd, kcd)
  } else {
    cat("checking covered kcd...\n")
    clm <- check_cvd_kcd(cvd_kcd, kcd)
    cat("checking covered level of surgery...\n")
    setmul(clm, check_cvd_level(cvd_level, df[[level_var]]))
  }
  # set only first claim
  cat("classifying one-time payments...\n")
  set_only_first(clm, id, one_time)
  # period
  period  <- diff_period(udate, sdate)
  clm_prd <- repcol(colvec(period), length(rn))
  setdimnames(clm_prd, list(id, rn)) # not label because of the expiration conditions
  # reduction ratio
  cat("applying reduction period ratio...\n")
  clm_red <- ratio_by_period(as_double(clm_prd),
                             reduction_period_start,
                             reduction_period_end,
                             reduction_period_ratio)
  # waiting period
  if (waiting) {
    cat("applying waiting period...\n")
    clm_wai <- ratio_by_period(as_double(clm_prd),
                               waiting_period_start,
                               waiting_period_end,
                               rep(0, length(rn)))
    setmul(clm_red, clm_wai)
    rm(clm_wai); gc()
  }
  # expiration condition matrix (expiration 1이 표시된 최종시점까지 1을 표시, 계속 지급하다가)
  replace_val_in_mat(clm_prd, 0L, clm, 0L)
  cat("applying expiration conditions...\n")
  clm_exp <- period_to_binary_loss(clm_prd, one_time, expiration) # to be modified
  clm_amt <- rowvec(amount_mean * claim_times)
  # add label (rd + cvd_kcd)
  setdimnames(clm    , list(id, label))   # claims
  setdimnames(clm_red, list(id, label))   # reduction
  setdimnames(clm_exp, list(id, label))   # expiration
  setcolnames(clm_amt, label) # amount * claim_times
  # remove duplicated (make reduction periods as one column)
  clm     <- row_min_by_cn(clm)
  clm_red <- row_min_by_cn(clm_red) # minimize reduction ratio in the same covered kcds
  clm_exp <- row_min_by_cn(clm_exp)
  clm_amt <- row_min_by_cn(clm_amt)
  # multiplication (clm * clm_red * clm_exp * unit)
  cat("calculating...\n")
  clm <- as_double(clm)
  setmul(clm, clm_red); #rm(clm_red); gc()
  setmul(clm, as_double(clm_exp)); #rm(clm_exp); gc()
  setmul(clm, as_double(clm_amt), axis = 1)
  setmul(clm, unit)
  # hospitalization
  if (!missing(stay_var)) {
    clm_hos <- df[[stay_var]] # if hospitalization
    setmul(clm, clm_hos, axis = 2)
  }
  # name label to rd
  setcolnames(clm    , pull_code(glue_code(rd), colnames(clm)))
  # setcolnames(clm_exp, pull_code(glue_code(rd), colnames(clm_exp)))
  clm <- row_max_by_cn(clm)
  if (match.arg(type) == "data.table") {
    clm <- data.table(id, period, clm)[period > 0 & period <= mon]
    set(clm, j = "period", value = ceiling(clm$period / group))
    cols <- c("id", "period")
    diff_cols <- diff_cols(clm, cols)
    clm <- clm[, lapply(.SD, sum), keyby = cols, .SDcols = diff_cols]
  }
  return(clm)
}
