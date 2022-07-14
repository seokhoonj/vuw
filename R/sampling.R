
# sampling ----------------------------------------------------------------

#' Stratified Sampling
#'
#' Common stratified sampling technique
#' @param df data.table object
#' @param group_var group variable
#' @param size sampling size
#' @param method approximation method
#' @param replace replacement
#' @param zero non-sampling
#' @param verbose messages
#' @param seed random seed
#' @export
strat_sampling <- function(df, group_var, size, replace = TRUE, contain0 = FALSE,
                           verbose = TRUE, method = c("round", "floor", "ceiling"), seed) {
  assert_class(df, "data.table")
  group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  group <- df[, .(n = .N), keyby = group_var]
  set(group, j = "g", value = seq_len(nrow(group)))
  if (size > 0 & size < 1) {
    method <- match.arg(method)
    set(group, j = "s", value = do.call(method, list(x = group$n * size)))
  } else if (size >= 1) {
    method <- "none"
    set(group, j = "s", value = size)
  }
  if (!contain0)
    set(group, i = which(group$s == 0), j = "s", value = 1)
  set(group, j = "p", value = group$s / group$n)
  # sampling proportion
  if (verbose) {
    cat(draw_line(), "\n")
    cat(sprintf("Target prop: %.2f %% ( method = %s, replace = %s )\n", size * 100, method, replace))
    cat(sprintf("Population : %s unit\n", str_pad(comma(sum(group$n)), width = 14, pad = " ")))
    cat(sprintf("Sample     : %s unit\n", str_pad(comma(sum(group$s)), width = 14, pad = " ")))
    cat(sprintf("Actual prop: %.2f %%\n", sum(group$s) / sum(group$n) * 100))
    cat(draw_line(), "\n")
    print(cbind(group, prop = sprintf("%.2f %%", group$p * 100)))
    cat(draw_line(), "\n")
  }
  # add group variable to data
  if (nrow(group) > 1) {
    df[group, on = group_var, g := i.g]
  } else {
    set(df, j = "g", value = 1L)
  }
  # create objects
  n <- group$n # all
  s <- group$s # sample
  spl <- split(seq_len(nrow(df)), df$g)
  # set.seed
  if (!missing(seed)) set.seed(seed)
  # location vector v
  v <- sort(unlist(lapply(seq_along(spl), function(x) {
    if (n[x] > 1) {
      sample(spl[[x]], s[x], replace = replace)
    } else {
      sample(unname(spl[x]), s[x], replace = replace)
      # we make the spl[[x]] to unname(spl[x])
      # because single vector works 1:v in sample function
    }
  })))
  z <- df[v]
  setorder(z, g)
  setattr(z, "group", group)
  rm_cols(z, g)
  return(z[])
}

k_fold <- function(x, k) {
  n <- NROW(x)
  s <- round(n / k)
  q <- n %/% s
  r <- n %% s
  t <- c(rep(s, q), r)
  re <- rep(1:(q+1), times = t)
  sp <- sample(n, size = n)
  z <- split(sp, re)
  names(z) <- paste0("fold", str_pad(names(z), width = nchar(k), pad = "0"))
  return(z)
}

k_split <- function(x, k, number = FALSE) {
  u <- unique(x)
  n <- NROW(u)
  s <- round(n / k)
  q <- n %/% s
  r <- n %% s
  t <- c(rep(s, q), r)
  z <- rep(1:(q+1), times = t)
  if (!number) {
    z <- split(u, z)
    names(z) <- paste0("split", str_pad(names(z), width = nchar(k), pad = "0"))
  }
  return(z)
}
