
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

