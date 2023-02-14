
intersect_rn <- function(df, prefix = "") {
  loss_cols <- regmatch_cols(df, sprintf("^%sloss", prefix))
  loss_type <- gsub(sprintf("^%sloss", prefix), "", loss_cols)
  rp_cols   <- regmatch_cols(df, sprintf("^%srp", prefix))
  rp_type   <- gsub(sprintf("^%srp"  , prefix), "", rp_cols)
  intersect(loss_type, rp_type)
}

intersect_rn_cols <- function(df, prefix = "") {
  loss_cols <- regmatch_cols(df, sprintf("^%sloss", prefix))
  loss_type <- gsub(sprintf("^%sloss", prefix), "", loss_cols)
  rp_cols   <- regmatch_cols(df, sprintf("^%srp", prefix))
  rp_type   <- gsub(sprintf("^%srp"  , prefix), "", rp_cols)

  rn_type   <- intersect(loss_type, rp_type)
  rp_cols   <- sprintf("%srp%s"  , prefix, rn_type)
  loss_cols <- sprintf("%sloss%s", prefix, rn_type)

  list(loss_cols, rp_cols)
}

set_rn_col_order <- function(df) {
  loss_cols <- sort(regmatch_cols(df, "^loss[0-9]+"))
  rp_cols   <- sort(regmatch_cols(df, "^rp[0-9]+"))
  lr_cols   <- sort(regmatch_cols(df, "^lr[0-9]+"))
  pre_cols  <- diff_cols(df, c(loss_cols, rp_cols, lr_cols))
  setcolorder(df, c(pre_cols, loss_cols, rp_cols, lr_cols))
}

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

mix_lr <- function(df, biz_mix, group_cols = c("vuw", "period"), join_cols = c("gender", "age_band"), prefix = "") {
  has_cols(biz_mix, c("prop"))
  z <- copy(df)
  z[biz_mix, prop := i.prop, on = join_cols]
  z[, tot_prop := sum(prop), group_cols]
  lr_cols <- regmatch_cols(z, sprintf("^%slr", prefix))
  wlr_cols <- sprintf("w%s", lr_cols)
  for (i in seq_along(lr_cols)) {
    set(z, j = wlr_cols[i], value = z[[lr_cols[i]]] * z$prop / z$tot_prop)
  }
  z[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), group_cols, .SDcols = wlr_cols]
}