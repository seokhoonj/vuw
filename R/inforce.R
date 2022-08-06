
get_period_inforce <- function(df, group_var, policy_var, policy_date, last_date, group = 1L) {
  group_var   <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  policy_var  <- match_cols(df, deparse(substitute(policy_var)))
  policy_date <- match_cols(df, deparse(substitute(policy_date)))
  last_date   <- match_cols(df, deparse(substitute(last_date)))
  p <- diff_period(df[[policy_date]], df[[last_date]])
  df[, `:=`(period, p)]
  group_cols <-  c("period", group_var)
  inforce <- df[, .(n = uniqueN(get(policy_var))), keyby = group_cols][order(-period)]
  inforce[, `:=`(n, cumsum(n)), group_var]
  inforce <- inforce[order(period)]
  if (group > 1) {
    inforce[, `:=`(period, (period - 1)%/%group + 1)]
    return(inforce[, .(n = mean(n)), group_cols])
  }
  return(inforce)
}
