
get_period_inforce <- function(df, group_var, policy_var, policy_date, latest_date, group = 1L) {
  group_var   <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  policy_var  <- match_cols(df, deparse(substitute(policy_var)))
  policy_date <- match_cols(df, deparse(substitute(policy_date)))
  latest_date <- match_cols(df, deparse(substitute(latest_date)))
  p <- diff_period(df[[policy_date]], df[[latest_date]])
  df[, `:=`(period, p)]
  group_cols <-  c("period", group_var)
  inforce <- df[, .(n = uniqueN(get(policy_var))), keyby = group_cols][order(-period)]
  inforce[, `:=`(n, cumsum(n)), group_var]
  inforce <- inforce[order(period)]
  # inforce are added if there is no policy with only 1st period
  inforce_add <- inforce[, .(period = min(period)-1, n = max(n)), keyby = group_var][period > 0]
  if (nrow(inforce_add) > 0) {
    inforce_add <- reprow(inforce_add, inforce_add$period)
    inforce_add[, `:=`(period, rank(period, ties.method = "first")), keyby = group_var]
    inforce <- rbind(inforce_add, inforce)
    setorderv(inforce, group_cols)
  }
  if (group > 1) {
    inforce[, `:=`(period, (period-1) %/% group + 1)]
    return(inforce[, .(n = mean(n)), keyby = group_cols])
  }
  return(inforce[])
}

get_period_inforce_by_policy_ym <- function(df, group_var, policy_var, policy_ym, latest_ym, group = 1L) {
  group_var <- match_cols(df, vapply(substitute(group_var),
                                     deparse, "character"))
  policy_var <- match_cols(df, deparse(substitute(policy_var)))
  policy_ym <- match_cols(df, deparse(substitute(policy_ym)))
  latest_ym <- match_cols(df, deparse(substitute(latest_ym)))
  group_cols <- c(group_var, policy_ym, latest_ym)
  dm <- df[, .(n = uniqueN(policy)), keyby = group_cols]
  ym_list <- vseq(dm[[policy_ym]], dm[[latest_ym]], by = "month")
  times <- sapply(ym_list, length)
  z <- reprow(dm, times)
  z[, inforce_ym := num2date(unlist(ym_list))]
  p <- diff_period(z[[policy_ym]], z[["inforce_ym"]], group = group)
  z[, period := p]
  group_cols <- c(group_var, "inforce_ym", "period")
  z[, .(n = sum(n)), keyby = group_cols]
}
