
get_period_inforce <- function(df, policy_var, policy_date, last_date, group = 1L) {
  policy_var  <- deparse(substitute(policy_var))
  policy_date <- deparse(substitute(policy_date))
  last_date   <- deparse(substitute(last_date))
  p <- diff_period(df[[policy_date]], df[[last_date]])
  df[, period := p]
  inforce <- df[, .(n = uniqueN(get(policy_var))), keyby = .(period)][order(-period)]
  inforce[, n := cumsum(n)]
  inforce <- inforce[order(period)]
  if (group > 1) {
    inforce[, period := (period-1) %/% group + 1]
    return(inforce[, .(n = mean(n)), .(period)])
  }
  return(inforce)
}
