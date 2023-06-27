
get_kcd <- function(x, lang = c("ko", "en"), type = c("kcd", "ko", "en")) {
  if (missing(x))
    stop("Please insert kcd code string or regular expressions.")
  if (any(grepl(toupper(x), kcd_book$kcd) |
          grepl(toupper(x), gsub("\\.", "", kcd_book$kcd, ignore.case = TRUE)))) {
    if (lang[[1L]] == "ko") {
      df <- kcd_book[grepl(x, kcd_book$kcd, ignore.case = TRUE) |
                       grepl(x, gsub("\\.", "", kcd_book$kcd, ignore.case = TRUE)),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$ko), collapse = "\n")
    }
    else {
      df <- kcd_book[grepl(x, kcd_book$kcd, ignore.case = TRUE) |
                       grepl(x, gsub("\\.", "", kcd_book$kcd, ignore.case = TRUE)),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
  else if (any(grepl(x, kcd_book$ko))) {
    if (lang[[1L]] == "ko") {
      df <- kcd_book[grepl(x, kcd_book$ko, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$ko), collapse = "\n")
    }
    else {
      df <- kcd_book[grepl(x, kcd_book$ko, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
  else if(any(grepl(x, kcd_book$en))) {
    if (lang[[1L]] == "ko") {
      df <- kcd_book[grepl(x, kcd_book$en, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$ko), collapse = "\n")
    }
    else {
      df <- kcd_book[grepl(x, kcd_book$en, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      result <- paste0(paste0(str_pad(df$kcd, width = nc, pad = " ", side = "right"),
                              " | ", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
}

get_claim <- function(claim_info, x) {
  if (missing(x)) {
    hprint(unique(claim_info[, .(rn, main_category, sub_category, rider_category,
                                 proportion = sprintf("%.2f", proportion), rider)]))
    x <- splt_code(readline("Please insert rider: "))
  }
  claim_info_sub <- claim_info[claim_info$rider %in% x]
  cols <- colnames(claim_info_sub)
  width <- max(nchar(cols))
  cols <- str_pad(cols, width = width, pad = " ", side = "right")
  cat(draw_line(), "\n")
  cat(paste0(paste0(x, collapse = "\n"), "\n"))
  cat(draw_line(), "\n")
  cat(paste0(traverse(paste(cols, "| "), paste0(claim_info_sub, "\n")), collapse = ""))
  cat(draw_line(), "\n")
  invisible(claim_info_sub)
}

get_risk <- function(risk_info, x) {
  if (missing(x)) {
    hprint(unique(risk_info[, .(risk)]))
    x <- splt_code(readline("Please insert risk: "))
  }
  risk_info_sub <- risk_info[risk_info$risk %in% x,]
  risk_info_sub[, scale := draw_line(minmax_scaler(rate) * 20)]
  cat(draw_line(), "\n")
  cat(paste0(paste0(x, collapse = "\n"), "\n"))
  view_cols <- match_cols(risk_info_sub, c("gender", "age", "grade", "rate", "scale", "risk"))
  hprint(risk_info_sub[, ..view_cols])
  invisible(risk_info_sub)
}

get_rider_info <- function(claim_info) {
  unique(claim_info[, .(rn, main_category, sub_category, rider_category, rider, proportion)])
}

risk_plot <- function(risk_info, x, nrow = NULL, ncol = NULL, scales = "free_y", age_unit = 10, logscale = FALSE, facet = TRUE, max_label = TRUE, family = "Comic Sans MS") {
  assert_class(risk_info$gender, "factor")
  if (missing(x)) {
    hprint(unique(risk_info[, .(risk)]))
    x <- splt_code(readline("Please insert risk (if you want all risks, 'all'): "))
  }
  if (any(x == "all")) {
    risk_info <- risk_info[risk_info$grade <= 1,]
  } else {
    risk_info <- risk_info[risk_info$risk %in% x,]
  }
  risk_info_s <- risk_info[, .(max_rate = max(rate)), .(risk, gender)]
  risk_info[risk_info_s, max_rate := i.max_rate, on = .(risk, gender, rate = max_rate)]
  risk_info_a <- risk_info[!is.na(max_rate), .(age = min(age)), .(risk, gender, max_rate)]
  setcolafter(risk_info_a, age, gender)
  rm_cols(risk_info, max_rate)
  risk_info[risk_info_a, on = .(risk, gender, age), max_rate := i.max_rate]
  risk_info[, label := ifelse(!is.na(max_rate), sprintf("%.4f (%d)", max_rate, age), max_rate)]
  risk_info_a[, label := sprintf("%d: %.4f (%d)", gender, max_rate, age), .(risk)]
  risk_info_b <- risk_info_a[, .(label = glue_code(label, "\n")), .(risk)]
  risk_info_b[, gender := factor(1, levels = c(1, 2))]
  risk_info_b[, age := -Inf]
  risk_info_b[, rate := Inf]
  if (logscale) {
    g <- ggplot(risk_info, aes(x = age, y = log(rate), group = gender, col = gender)) +
      geom_line() +
      list(if (max_label) {
        geom_label(data = risk_info_b, aes(label = label), family = family, colour = "black", alpha = .3, hjust = -.1, vjust = 1.2)
      }) +
      scale_gender_manual(risk_info$gender) +
      scale_x_continuous(n.breaks = floor(unilen(risk_info$age) / age_unit)) +
      scale_y_continuous(labels = function(x) sprintf("%.4f", exp(x))) +
      list(if (facet) {
        facet_wrap(~ risk, nrow = nrow, ncol = ncol, scales = scales)
      })
  } else {
    g <- ggplot(risk_info, aes(x = age, y = rate, group = gender, col = gender)) +
      geom_line() +
      list(if (max_label) {
        geom_label(data = risk_info_b, aes(label = label), family = family, colour = "black", alpha = .3, hjust = -.1, vjust = 1.2)
      }) +
      scale_gender_manual(risk_info$gender) +
      scale_x_continuous(n.breaks = floor(unilen(risk_info$age) / age_unit)) +
      scale_y_continuous(labels = function(x) sprintf("%.4f", x)) +
      list(if (facet) {
        facet_wrap(~ risk, nrow = nrow, ncol = ncol, scales = scales)
      })
  }
  return(g)
}

ratio_plot <- function(risk_info, risk1, risk2, nrow = NULL, ncol = NULL,
                       scales = "fixed", age_unit = 10, plot = TRUE, family = "Comic Sans MS") {
  x <- risk_info[risk == risk1]
  y <- risk_info[risk == risk2]
  z <- merge(x, y, by = c("gender", "age"), all.x = TRUE)
  setorder(z, gender, age)
  setnames(z, gsub("\\.", "_", names(z)))
  set(z, j = "rate_x_prop", value = z$rate_x / (z$rate_x + z$rate_y))
  set(z, j = "rate_y_prop", value = z$rate_y / (z$rate_x + z$rate_y))
  set(z, j = "ratio", value = z$rate_x / z$rate_y)

  m <- melt(
    z,
    id.vars       = c("age", "gender"),
    measure.vars  = c("rate_x", "rate_y", "ratio"),
    variable.name = c("risk"),
    value.name    = c("rate")
  )
  set(m, i = which(m$risk == "rate_x"), j = "risk", value = risk1)
  set(m, i = which(m$risk == "rate_y"), j = "risk", value = risk2)
  set(m, j = "label", value = paste(m$risk, "(", m$gender, ")"))

  # plot
  if (plot) {
    colours <- rev(scales::hue_pal()(2)) # show_col(hue_pal()(2))

    g1 <- ggplot(m[risk != "ratio"], aes(x = age, y = rate, ymin = 0, group = label, linetype = risk)) +
      geom_line() +
      scale_x_continuous(n.breaks = floor(unilen(m$age) / age_unit)) +
      scale_y_continuous(labels = function(x) if (max(m[risk != "ratio"]$rate) <= 1)
        sprintf("%.2f%%", x * 100) else sprintf("%.2f", x)) +
      scale_color_manual(values = colours) +
      facet_wrap(~ gender, scales = scales) +
      theme(legend.position = "top")

    g2 <- ggplot(m[risk == "ratio"], aes(x = age, y = rate, ymin = 0, group = gender, col = gender)) +
      geom_line() +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      scale_x_continuous(n.breaks = floor(unilen(m$age) / age_unit)) +
      scale_color_manual(values = colours) +
      ylab("ratio") +
      facet_wrap(~ risk) +
      theme(legend.position = "top", plot.title = element_text(size = 7, hjust = .5))

    g <- grid.arrange(g1, g2, widths = c(1, 1, 1),
                      layout_matrix = cbind(1, 1, 2),
                      top = grid::textGrob(bquote("Risk Ratio = " ~ frac(.(risk1), .(risk2)))))
    print(g)
  }
  return(z)
}
