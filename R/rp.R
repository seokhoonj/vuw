
create_base_risk <- function(no = 0, risk = "base", gender = c(1L, 2L), age = 0:120, grade = 0, rate = 1.) {
  base_risk <- data.table(expand.grid(
    no = no, risk = risk, gender = as.factor(gender), age = age, grade = grade, rate = rate,
    stringsAsFactors = FALSE))
  setorder(base_risk, no, risk, gender, age, grade)
  return(base_risk)
}

join_info <- function(risk_info, claim_info) {
  tot_info <- risk_info[claim_info, on = .(risk)]
  tot_info[risk_info, on = .(risk2 = risk, age = age, gender = gender),
           rate2 := i.rate]
  # remove the risk having several payment condition
  unique(tot_info[, .(
    age, gender, grade,
    rn, rider,
    risk, risk2,
    rate, rate2,
    amount_mean,
    rp_times, claim_times,
    reduction_period_start,
    reduction_period_end,
    reduction_period_ratio,
    waiting_period_start,
    waiting_period_end,
    proportion)
  ])
}

to_monthly_lapse <- function(x) 1-(1-x)^(1/12)
lapse2persi <- function(x) cumprod(1 - x)
persi2lapse <- function(x) 1 - (x / shift(x, fill = 1, type = "lag"))
lapse2dist  <- function(x) abs(diff(c(1, cumprod(1-x), 0)))
dist2lapse  <- function(x) c(x[1L], 1-exp(diff(log(1-cumsum(x)))))

random_pay_num <- function(df, lapse, mon, seed) {
  uid <- unique(df$id)
  prob <- lapse2dist(lapse[1:(mon-1)])
  if (!missing(seed)) set.seed(seed)
  structure(colvec(sample(
    seq_len(mon), size = length(uid), replace = T, prob = prob)),
    dimnames = list(uid, NULL))
}

create_rp_matrix <- function(risk_info, claim_info, igender, iage, igrade, mon, waiting = TRUE, unit = 1, type = c("rider", "risk")) {
  urn <- unique(claim_info$rn)
  urider <- unique(claim_info$rider)
  rider_list <- vector(mode = "list", length(urider))
  for (j in seq_along(urider)) {
    # variables
    iages <- iage + seq_len((mon-1)%/%12+1L) - 1L
    igrades <- unique(c(0, igrade))
    # join info
    rider_info <- claim_info[rider == urider[j]]
    total_info <- join_info(risk_info, rider_info)
    sub_info   <- total_info[(gender  ==  igender) &
                             (age    %in% iages  ) &
                             (grade  %in% igrades)]
    if (!nrow(sub_info))
      stop("The insured has no risk rate")
    risk_list <- vector(mode = "list", nrow(rider_info))
    for (i in seq_len(nrow(rider_info))) {
      sub_tbl <- sub_info[(risk == rider_info$risk[i] &
                           risk2 == rider_info$risk2[i] &
                           rp_times == rider_info$rp_times[i] &
                           reduction_period_start == rider_info$reduction_period_start[i] &
                           reduction_period_end   == rider_info$reduction_period_end[i] &
                           reduction_period_ratio == rider_info$reduction_period_ratio[i])]
      rp_mon <- with(sub_tbl, rate * rate2 * amount_mean * rp_times / 12)
      rp  <- reprow(rowvec(rp_mon), 12L)
      prd <- (numbers(dim(rp))-1) %% mon + 1
      rat <- ratio_by_period(prd,
                             sub_tbl$reduction_period_start,
                             sub_tbl$reduction_period_end,
                             sub_tbl$reduction_period_ratio) # to be modified
      setmul(rp, rat)
      if (waiting) {
        wai <- ratio_by_period(prd,
                               sub_tbl$waiting_period_start,
                               sub_tbl$waiting_period_end,
                               rep(0, nrow(sub_tbl)))
        setmul(rp, wai)
      }
      setdimnames(rp, list(seq_len(12L), sub_tbl$age))
      risk_list[[i]] <- structure(
        colvec(rp[seq_len(mon)]),
        dimnames = list(NULL, rider_info$risk[i])
      )
    }
    z <- row_sum_by_cn(do.call("cbind", risk_list))
    if (type[[1L]] == "rider")
      z <- structure(colvec(row_sum(z)), dimnames = list(NULL, urn[j]))
    rider_list[[j]] <- z
  }
  zs <- do.call("cbind", rider_list)
  setrownames(zs, seq_len(mon))
  return(zs)
}

apply_expiration <- function(x, expiration) {
  #       1    1    1    1
  # 1     7   28   60   60
  # 2    51   52   60   60
  # expiration <- c(0, 0, 0, 0) row_max_by_cn 계속
  # expiration <- c(1, 1, 1, 1) row_min_by_cn 하나라도 해당하면 종료
  # expiration <- c(2, 2, 2, 2) row_max_by_cn 모두 해당하면 종료
  # expiration <- c(0, 0, 1, 1) row_min_by_cn 1인 것 중 하나라도 해당하면 종료
  # expiration <- c(2, 2, 0, 0) row_max_by_cn 2인 것이 모두 해당하면 종료
  rn <- colnames(x)
  ur <- unique(rn)
  ex <- structure(rowvec(expiration), dimnames = list(NULL, colnames(x))) # 0, 1, 2
  em <- as_integer(row_max_by_cn(ex)) # maximum expiration

  z <- vector(mode = "list", length = length(ur))
  for (i in seq_along(ur)) {
    col_em <- icol(em, ur[i])
    if (em[, col_em] == 0L) {
      col_x0 <- icol(x, ur[i])
      x0 <- x[, col_x0, drop = F]
      mx <- row_max_by_cn(x0)
      z[[i]] <- mx
    } else if (em[, col_em] == 1L) {
      # select columns
      col_ex1 <- icol(ex, ur[i])
      col_x1 <- icol(x , ur[i])
      # subset columns
      ex1 <- ex[, col_ex1, drop = F]
      x1 <- x[, col_x1, drop = F]
      z[[i]] <- row_min_by_cn(x1[, which(ex1 == 1L), drop = F])
    } else if (em[, col_em] == 2L) {
      # select columns
      col_ex2 <- icol(ex, ur[i])
      col_x2  <- icol(x, ur[i])
      # subset columns
      ex2 <- ex[, col_ex2, drop = F]
      x2  <- x[, col_x2, drop = F]
      z[[i]] <- row_max_by_cn(x2[, which(ex2 == 2L), drop = F])
    }
  }
  return(do.call("cbind", z))
}

count_pay_num <- function(claim_info, df, udate, mon, waiting = TRUE) {
  # check claim information
  has_cols(claim_info, c("rn", "rider", "cvd_kcd", "one_time"))
  has_cols(df, c("id", "kcd", "sdate", "edate", "ldate"))
  # variables (claim_info)
  rn         <- claim_info$rn
  rider      <- claim_info$rider
  cvd_kcd    <- claim_info$cvd_kcd
  one_time   <- claim_info$one_time
  expiration <- claim_info$expiration
  waiting_period_start <- claim_info$waiting_period_start
  waiting_period_end   <- claim_info$waiting_period_end
  # order
  setorder(df, id, sdate, edate)
  # variables (clm)
  id    <- df$id
  kcd   <- df$kcd
  sdate <- df$sdate
  ldate <- df$ldate
  # check the last claim point
  clm <- check_cvd_kcd(cvd_kcd, kcd)
  set_only_first(clm, id, one_time)
  # check the month from the underwriting
  period <- diff_period(udate, sdate)
  latest <- diff_period(udate, ldate)
  loc <- which(is.na(period))
  period[loc] <- latest[loc] # no sdate (for the data with no claims)
  clm_prd <- as_integer(repcol(colvec(period), length(rn)))
  # check waiting period
  if (waiting) {
    clm_wai <- as_integer(
      ratio_by_period(
        as_double(clm_prd),
        waiting_period_start,
        waiting_period_end,
        rep(0, length(rn))
      )
    )
    setmul(clm, clm_wai)
    rm(clm_wai); gc()
  }
  # payment before the first claim (Don't be confused due to 'max')
  replace_val_in_mat(clm_prd, 0L, clm, 0L)
  setdimnames(clm_prd, list(id, rn))
  clm_prd_max <- row_max_by_rn(clm_prd) # 한 ID에 여러 개의 month 가 있으면 가장 큰 월도를 가져옴 (one_time이 아닌 column이므로 추후 통일 됨)
  # calculate claim period cap from ldate
  clm_lat <- as_integer(repcol(colvec(latest), length(rn)))
  setdimnames(clm_lat, list(id, rn))
  clm_prd_cap <- row_max_by_rn(clm_lat)
  # modify no of premium payment by method of benefit payment
  clm_prd_max[clm_prd_max == 0] <- clm_prd_cap[clm_prd_max == 0]
  m <- overlap_matrix(clm_prd_cap, clm_prd_max, one_time)
  z <- apply_expiration(m, expiration)
  as_double(z)
}

# 1 rn == 1 rider
rp_simulation <- function(risk_info, claim_info, df, udate, mon = 60, group = 1L, waiting = FALSE,
                          lapse, unit = 1L, seed = 123) {
  # check columns
  has_cols(df, c("id", "gender", "age", "grade", "kcd", "sdate", "edate", "ldate"))
  # order
  setorder(df, id, sdate, edate, kcd)
  # the insured having claim data
  insured <- unique(df[, .(id, gender, age, grade, mon = diff_period(udate, ldate))])
  demo <- insured[, .(count = .N), keyby = .(gender, age, grade, mon)]
  set(demo, j = "scale", value = minmax_scaler(demo$count))
  # count risk premium payment
  cat("Counting number of payments...\n")
  pay_count <- count_pay_num(claim_info, df, udate, mon, waiting = waiting)
  limit_count <- structure(repcol(insured$mon, each = ncol(pay_count)),
                           dimnames = dimnames(pay_count))
  pay_count <- cbind(insured, pmin(limit_count, pay_count))
  if (!missing(lapse)) {
    cat("Applying random lapse...\n")
    lapse_point <- random_pay_num(df, lapse, mon, seed = seed)
    lapse_point <- structure(repcol(lapse_point, each = ncol(pay_count)),
                             dimnames = dimnames(pay_count))
    pay_count <- pmin(lapse_point, pay_count)
  }
  rp_list <- vector(mode = "list", length = nrow(demo))
  # repeat
  cat("Start calculating...\n")
  for (i in seq_len(nrow(demo))) {
    # variables
    count   <- demo$count[i]
    scale   <- demo$scale[i]
    iage    <- demo$age[i]
    igender <- demo$gender[i]
    igrade  <- unique(c(0, demo$grade[i]))
    imon    <- demo$mon[i]
    # risk premium matrix
    rp  <- create_rp_matrix(risk_info, claim_info, igender, iage, igrade,
                            mon, waiting = FALSE, unit = unit) # Male: 1, Female: 2
    # subset pay_count
    ipay <- pay_count[age == iage & gender == igender & grade %in% igrade & mon == imon]
    # create variables
    pd <- rep((seq_len(mon)-1L) %/% group + 1L, nrow(ipay))
    id_mon <- rep(ipay$id, each = mon) # by month
    id_pd  <- rep(ipay$id, each = unilen(pd)) # by period (months grouped)
    pd_pd  <- rep(unique(pd), times = nrow(ipay))
    # subset ipay
    cols <- diff_cols(ipay, c("id", "gender", "age", "grade", "mon"))
    pay <- as.matrix(ipay[, ..cols])
    pay <- upper(rp, pay[,, drop = FALSE])
    pay <- structure(pay, dimnames = list(paste(id_mon, pd), colnames(rp)))
    pay <- row_sum_by_rn(pay)
    # sum by scenario
    rp_list[[i]] <- data.table(id = id_pd, period = pd_pd, pay)
    # print
    cat(sprintf("Group %3d (gender: %d, age: %2d, grade: %d, mon: %2d): %s %s\n",
                i, igender, iage, max(igrade), imon,
                stri_pad_left(comma(count), width = 9L), draw_line(scale*20)))
  }
  z <- do.call("rbind", rp_list)
  setnames(z, c("id", "period", paste0("rp", stri_pad_left(colnames(rp), width = max(nchar(claim_info$rn)), pad = "0"))))
  return(z)
}

apply_weight <- function(df, weight) {
  # columns
  loss_cols <- regmatch_cols(df, "^loss[0-9]+")
  rp_cols <- regmatch_cols(df, "^rp[0-9]+")
  pre_cols <- diff_cols(df, c(loss_cols, rp_cols))
  loss <- as.matrix(df[, ..loss_cols])
  rp <- as.matrix(df[, ..rp_cols])
  # weighted
  if (length(loss_cols) > 0)
    setmul(loss, weight, axis = 1)
  if (length(rp_cols) > 0)
    setmul(rp, weight, axis = 1)
  cbind(df[, ..pre_cols], data.table(loss), data.table(rp))
}

# set_lr <- function(df) {
#   loss_cols <- regmatch_cols(dm, "^loss")
#   loss_type <- gsub("^loss", "", loss_cols)
#   rp_cols   <- regmatch_cols(dm, "^rp")
#   rp_type   <- gsub("^rp", "", rp_cols)
#
#   lr_type   <- intersect(loss_type, rp_type)
#   lr_cols   <- sprintf("lr%s"  , lr_type)
#   rp_cols   <- sprintf("rp%s"  , lr_type)
#   loss_cols <- sprintf("loss%s", lr_type)
#
#   for (i in seq_along(lr_cols)) {
#     set(df, j = lr_cols[i], value = df[[loss_cols[i]]] / df[[rp_cols[i]]])
#   }
# }

set_lr <- function(df, prefix = "") {
  loss_cols <- regmatch_cols(df, sprintf("^%sloss", prefix))
  loss_type <- gsub(sprintf("^%sloss", prefix), "", loss_cols)
  rp_cols   <- regmatch_cols(df, sprintf("^%srp", prefix))
  rp_type   <- gsub(sprintf("^%srp", prefix), "", rp_cols)

  lr_type   <- intersect(loss_type, rp_type)
  lr_cols   <- sprintf("%slr%s"  , prefix, lr_type)
  rp_cols   <- sprintf("%srp%s"  , prefix, lr_type)
  loss_cols <- sprintf("%sloss%s", prefix, lr_type)

  for (i in seq_along(lr_cols)) {
    set(df, j = lr_cols[i], value = df[[loss_cols[i]]] / df[[rp_cols[i]]])
  }
}

mix_lr <- function(df, biz_mix, group_cols, join_cols = c("gender", "age_band")) {
  z <- df[biz_mix, on = join_cols]
  z[, tot_prop := sum(prop), group_cols]
  lr_cols <- regmatch_cols(z, "^lr_")
  wt_lr_cols <- sprintf("wt_%s", lr_cols)
  for (i in seq_along(lr_cols)) {
    set(z, j = wt_lr_cols[i], value = z[[lr_cols[i]]] * z$prop / z$tot_prop)
  }
  z[, lapply(.SD, sum), group_cols, .SDcols = wt_lr_cols]
}
