
# subset id with kcd ------------------------------------------------------

subset_id_with_kcd <- function(df, id_var, kcd_var, from_var, to_var,
                               udate, start, end, ...) {
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  dots <- list(...)
  for (i in seq_along(dots)) {
    fdate <- add_mon(udate, start)
    tdate <- add_mon(udate, end  )
    key <- pull_code("^!", dots[[i]])
    diz <- remv_code("^!", dots[[i]])
    if (is.na(key)) {
      df <- df[ unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                          (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    } else {
      df <- df[!unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                          (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    }
  }
  return(df)
}

subset_id_with_kcd_ <- function(df, id_var, kcd_var, from_var, to_var,
                                udate, start, end, ...) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    fdate <- add_mon(udate, start)
    tdate <- add_mon(udate, end  )
    key <- pull_code("^!", dots[[i]])
    diz <- remv_code("^!", dots[[i]])
    if (is.na(key)) {
      df <- df[ unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                          (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    } else {
      df <- df[!unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                          (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    }
  }
  return(df)
}

subset_id_with_kcd_terms <- function(df, id_var, kcd_var, from_var, to_var,
                                     udate, ...) {
  # "": any diz, "!": no diz
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  kcd_terms <- list(...)
  for (i in seq_along(kcd_terms)) {
    fdate <- add_mon(udate, kcd_terms[[i]][[1L]][[1L]])
    tdate <- add_mon(udate, kcd_terms[[i]][[2L]][[1L]])
    key <- pull_code("^!" , kcd_terms[[i]][[3L]][[1L]])
    diz <- remv_code("^!" , kcd_terms[[i]][[3L]][[1L]])
    if (is.na(key)) {
      df <- df[unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                         (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    } else {
      df <- df[!unique(df[(df[[to_var]] >= fdate & df[[from_var]] < tdate) &
                          (grepl(diz, df[[kcd_var]], perl = TRUE)), ..id_var]), on = id_var]
    }
  }
  return(df)
}

# id with kcd for a certain period ----------------------------------------

id_with_kcd <- function(df, id_var, kcd_var, from_var, to_var, udate, start, end, ...) {
  if (start > end)
    stop("`start` has to be smaller than `end`")
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  kcd_codes <- list(...)
  n <- length(kcd_codes)
  id_list <- vector(mode = "list", length = n+1)
  id_list[[1L]] <- unique(df[, ..id_var])
  id_list[2L:length(id_list)] <- lapply(seq_along(kcd_codes), function(x) {
    d <- unique(subset_id_with_kcd_(df, id_var, kcd_var, from_var, to_var,
                                    udate, start, end, kcd_codes[[x]])[, ..id_var])
    col <- sprintf("%s_%s_%s", kcd_codes[[x]], start, end)
    set(d, j = col, value = 1L)
  })
  z <- Reduce(function(...) merge(..., by = id_var, all = TRUE), id_list)
  replace_na_with_zero(z)
  return(z[])
}

id_with_kcd_ <- function(df, id_var, kcd_var, from_var, to_var, udate, start, end, ...) {
  if (start > end)
    stop("`start` has to be smaller than `end`")
  kcd_codes <- list(...)
  n <- length(kcd_codes)
  id_list <- vector(mode = "list", length = n+1)
  id_list[[1L]] <- unique(df[, ..id_var])
  id_list[2L:length(id_list)] <- lapply(seq_along(kcd_codes), function(x) {
    d <- unique(subset_id_with_kcd_(df, id_var, kcd_var, from_var, to_var,
                                    udate, start, end, kcd_codes[[x]])[, ..id_var])
    col <- sprintf("%s_%s_%s", kcd_codes[[x]], start, end)
    set(d, j = col, value = 1L)
  })
  z <- Reduce(function(...) merge(..., by = id_var, all = TRUE), id_list)
  replace_na_with_zero(z)
  return(z[])
}

id_with_kcd_terms <- function(df, id_var, kcd_var, from_var, to_var, udate, ...) {
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  kcd_terms <- list(...)
  n <- length(kcd_terms)
  id_list <- vector(mode = "list", length = n+1)
  id_list[[1L]] <- unique(df[, ..id_var])
  id_list[2L:length(id_list)] <- lapply(seq_along(kcd_terms), function(x) {
    start    <- kcd_terms[[x]][[1L]]
    end      <- kcd_terms[[x]][[2L]]
    kcd_code <- kcd_terms[[x]][[3L]]
    d <- unique(subset_id_with_kcd_(df, id_var, kcd_var, from_var, to_var, udate,
                                    start, end, kcd_code)[, ..id_var])
    col <- sprintf("%s_%s_%s", kcd_code, start, end)
    set(d, j = col, value = 1L)
  })
  z <- Reduce(function(...) merge(..., by = id_var, all = TRUE), id_list)
  replace_na_with_zero(z)

  # column names
  if (!is.null(names(kcd_terms)))
    setnames(z, c(id_var, names(kcd_terms)))

  # summarize
  col <- diff_cols(z, id_var[[1]])
  zs  <- z[, .(n = .N), by = col]
  zs[, (col) := lapply(.SD, factor), .SDcols = col]
  setorderv(zs, col)
  attr(z, "raw") <- copy(zs)

  for (i in 1:(length(col)-1L)) {
    grp <- col[1:(length(col)-i)]
    zs[, nsum := sum(n), by = grp]
    zs[, ratio := n / nsum]
    zs[, label := sprintf("%.2f (%s)", ratio * 100, comma(n))]
    attr(z, paste0("summary.", i)) <- copy(zs)
  }

  return(z)
}

relative_risk_test <- function(data, group_var = c("gender", "age_band"), value_var = c("decl")) {
  dt <- data[data$excl == 0 & data$claim == 1,]
  formula <- formula(sprintf("%s ~ %s", paste(group_var, collapse = " + "), value_var))
  dt_n <- dcast(dt, formula, value.var = "n", fun.aggregate = sum)
  replace_na_with_zero(dt_n)
  setnames(dt_n, c("0", "1"), c("n01", "n11"))
  dt_nsum <- dcast(dt, formula, value.var = "nsum", fun.aggregate = sum)
  replace_na_with_zero(dt_nsum)
  setnames(dt_nsum, c("0", "1"), c("nsum0", "nsum1"))
  dt_nsum[dt_n, on = group_var, `:=`(n01 = i.n01, n11 = i.n11)]
  dt_nsum[, n00 := nsum0 - n01]
  dt_nsum[, n10 := nsum1 - n11]
  rm_cols(dt_nsum, .(nsum0, nsum1))
  setnames(dt_nsum, c("n11", "n01", "n10", "n00"), c("tp", "fn", "fp", "tn"))
  cols <- c(group_var, "tp", "fn", "fp", "tn")
  z <- dt_nsum[, ..cols]
  m <- array(t(as.matrix(z[, c("tp", "fn", "fp", "tn")])), dim = c(2L, 2L, nrow(z)))
  pvalue <- sapply(1:nrow(z), function(x) fisher.test(m[,, x])$p.value)
  z[, inc0 := fn / (fn + tn)]
  z[, inc1 := tp / (tp + fp)]
  z[, rr  := ifelse(inc1 > inc0, inc1 / inc0 - 1, 0)]
  z[, or  := (tp/fp) / (fn/tn) - 1]
  z$pvalue <- pvalue
  z$confident <- ifelse(pvalue < .025, 1, 0)
  return(z)
}

kcd_in_months <- function (df, id_var, kcd_var, from_var, to_var, udate, start, end,
                           kcd_code, col) {
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  dm <- subset_id_with_kcd_(df, id_var, kcd_var, udate, start, end, kcd_code)
  if (mon < 0) {
    if (missing(col))
      col <- sprintf("%s_PST_%s", kcd_code, abs(mon))
    z <- unique(dm[grepl(kcd_code, dm[[kcd_var]], perl = TRUE) &
                dm[[from_var]] < udate & dm[[to_var]] >= add_mon(udate, mon)][, .(id)])
  } else {
    if (missing(col))
      col <- sprintf("%s_FTR_%s", kcd_code, abs(mon))
    z <- unique(dm[grepl(kcd_code, dm[[kcd_var]], perl = TRUE) &
                dm[[from_var]] < add_mon(udate, mon) & dm[[to_var]] >= udate][, .(id)])
  }
  set(z, j = col, value = 1L)
  return(z)
}

kcd_in_years <- function (df, id_var, kcd_var, from_var, to_var, udate, start, end,
                          kcd_code, col) {
  id_var   <- match_cols(df, vapply(substitute(id_var)  , deparse, "character"))
  kcd_var  <- match_cols(df, vapply(substitute(kcd_var) , deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse, "character"))
  to_var   <- match_cols(df, vapply(substitute(to_var)  , deparse, "character"))
  dm <- subset_id_with_kcd_(df, id_var, kcd_var, udate, start, end, kcd_code)
  if (year < 0) {
    if (missing(col))
      col <- sprintf("%s_PST_%s", kcd_code, abs(year))
    z <- unique(dm[grepl(kcd_code, dm[[kcd_var]], perl = TRUE) &
                dm[[from_var]] < udate & dm[[to_var]] >= add_year(udate, year)][, .(id)])
  } else {
    if (missing(col))
      col <- sprintf("%s_FTR_%s", kcd_code, abs(year))
    z <- unique(dm[grepl(kcd_code, dm[[kcd_var]], perl = TRUE) &
                dm[[from_var]] < add_year(udate, year) & dm[[to_var]] >= udate][, .(id)])
  }
  set(z, j = col, value = 1L)
  return(z)
}

# kcd_dist <- function(df, id_var, kcd_var, from_var, to_var, group_var, udate, start, end, multiple = 100) {
#   if (start > end) stop("`start` has to be smaller than `end`")
#   id_var    <- match_cols(df, vapply(substitute(id_var), deparse, "character"))
#   kcd_var   <- match_cols(df, vapply(substitute(kcd_var), deparse, "character"))
#   from_var  <- match_cols(df, deparse(substitute(from_var)))
#   to_var    <- match_cols(df, deparse(substitute(to_var)))
#   group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
#
#   cols <- c(kcd_var, from_var, to_var)
#
#   fdate <- add_mon(udate, start * 12) # from
#   tdate <- add_mon(udate, end   * 12) # to
#
#   incl <- df[ (get(to_var) >= fdate & get(from_var) < tdate)] #     having past data
#   excl <- df[!(get(to_var) >= fdate & get(from_var) < tdate)] # not having past data
#   excl[, (cols) := lapply(.SD, function(x) `<-`(x, NA)), .SDcols = cols]
#   excl <- unique(excl)[!incl, on = id_var]
#   dz <- rbind(incl, excl)
#   z <- get_prop_(dz, group_var, id_var, multiple = multiple)
#   return(z)
# }
#
# kcd_dist_ <- function(df, id_var, kcd_var, from_var, to_var, group_var, udate, start, end, multiple = 100) {
#   if (start > end) stop("`start` has to be smaller than `end`")
#   cols <- c(kcd_var, from_var, to_var)
#
#   fdate <- add_mon(udate, start * 12) # from
#   tdate <- add_mon(udate, end   * 12) # to
#
#   incl <- df[ (get(to_var) >= fdate & get(from_var) < tdate)] #     having past data
#   excl <- df[!(get(to_var) >= fdate & get(from_var) < tdate)] # not having past data
#   excl[, (cols) := lapply(.SD, function(x) `<-`(x, NA)), .SDcols = cols]
#   excl <- unique(excl)[!incl, on = id_var]
#   dz <- rbind(incl, excl)
#   z <- get_prop_(dz, group_var, id_var, multiple = multiple)
#   return(z)
# }

stay_plot <- function(df, id_var, kcd_var, stay_var, kcd_code = "M51", digit, stay_cut = c(7, 15, 30), logscale = TRUE, scales = "fixed", ncol = NULL) {
  z <- copy(df)
  id_var <- match_cols(z, vapply(substitute(id_var), deparse, "character"))
  kcd_var <- match_cols(z, vapply(substitute(kcd_var), deparse, "character"))
  stay_var <- match_cols(z, vapply(substitute(stay_var), deparse, "character"))
  setnames(z, c(id_var, kcd_var, stay_var), c("id", "kcd", "stay"))
  z <- split_merge_var(z, kcd)[grepl(kcd_code, kcd)]
  if (!missing(digit))
    z[, `:=`(kcd, substr(kcd, 1, digit))]
  if (logscale) {
    label <- str_pad(stay_cut, width = max(nchar(stay_cut)))
    annotation <- vector(mode = "list", length = length(stay_cut))
    for (i in seq_along(stay_cut)) {
      annotation[[i]] <- annotate(geom = "text", x = log(stay_cut[i]), y = Inf, label = label[i], hjust = 0, vjust = 3)
    }
    g <- ggplot(z, aes(x = log(stay))) +
      geom_histogram() +
      geom_vline(xintercept = log(stay_cut), color = "red", lty = "dashed") +
      annotation +
      scale_x_continuous(labels = function(x) round(exp(x)), limits = range(log(z$stay), na.rm = TRUE)) +
      scale_y_continuous(labels = comma) +
      facet_wrap(~ kcd, scales = scales, ncol = ncol) +
      ylab("count") +
      labs(title = "Hospitalization distribution", subtitle = kcd_code)
  } else {
    label <- str_pad(stay_cut, width = max(nchar(stay_cut)))
    limit <- max(stay_cut)
    z[, stay := ifelse(stay > limit, sprintf("%s+", limit+1), stay)]
    z[, stay := factor(stay, levels = c(as.character(1:limit), sprintf("%s+", limit+1)))]
    annotation <- vector(mode = "list", length = length(stay_cut))
    for (i in seq_along(stay_cut)) {
      annotation[[i]] <- annotate(geom = "text", x = stay_cut[i], y = Inf, label = label[i], hjust = 0, vjust = 3)
    }
    g <- ggplot(z, aes(x = stay)) +
      geom_histogram(stat = "count") +
      geom_vline(xintercept = stay_cut, color = "red", lty = "dashed") +
      annotation +
      scale_y_continuous(labels = comma) +
      facet_wrap(~ kcd, scales = scales, ncol = ncol) +
      ylab("count") +
      labs(title = "Hospitalization distribution", subtitle = kcd_code)
  }
  return(g)
}
