
get_demography <- function(df, group_var, cycle = c("year", "month"), cast = TRUE) {
  assert_class(df, "data.table")
  has_cols(df, c("id", "gender", "age_band", "sdate"))
  if (!length(match_cols(df, "year"))) set(df, j = "year", value = year(df$sdate))
  if (!length(match_cols(df, "month"))) set(df, j = "month", value = month(df$sdate))
  group_cols <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  demography_cols <- c(group_cols, cycle[[1L]])
  demography <- df[, .(n = uniqueN(id)), keyby = demography_cols]
  if (cast) {
    fml <- formula(paste(paste(group_cols, collapse = " + "), "~", cycle[[1L]]))
    demography <- dcast(demography, fml, value.var = "n", fill = 0)
  }
  return(demography)
}

get_incidence <- function(df, group_var, cvd_kcd, cycle = c("year", "month"), cast = TRUE) {
  assert_class(df, "data.table")
  has_cols(df, c("id", "gender", "age_band", "kcd", "sdate"))
  incidence <- check_cvd_kcd(cvd_kcd = cvd_kcd, kcd = df$kcd)
  set_only_first(incidence, df$id, rep(1L, length(cvd_kcd)))
  setcolnames(incidence, cvd_kcd)
  incidence <- data.table(df[, .(id, gender, age_band, kcd, sdate)], incidence)
  set(incidence, j = "year", value = year(df$sdate))
  set(incidence, j = "month", value = month(df$sdate))
  group_cols <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  incidence_cols <- c(group_cols, cycle[[1L]])
  cvd_kcd_cols <- match_cols(incidence, cvd_kcd)
  incidence <- incidence[, lapply(.SD, sum), .SDcols = cvd_kcd_cols, keyby = incidence_cols]
  if (cast) {
    fml <- formula(paste0(paste0(group_cols, collapse = " + "), " ~ ", cycle[[1L]]))
    incidence <- lapply(seq_along(cvd_kcd),
                        function(x) dcast(incidence, fml, value.var = cvd_kcd_cols[[x]], fill = 0))
  } else {
    incidence <- lapply(cvd_kcd, function(x) {
      cols <- c(incidence_cols, x)
      data <- incidence[, ..cols]
      setnames(data, c(incidence_cols, "count"))
    })
  }
  names(incidence) <- cvd_kcd
  return(incidence)
}
