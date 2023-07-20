
apply_rules <- function(rule_info, df) {
  dm <- merge(df, rule_info, by = c("kcd"), all.x = TRUE, allow.cartesian = TRUE)
  dm[!is.na(kcd) & age >= age_min & age <= age_max & hos >= hos_min & hos <= hos_max & sur >= sur_min & sur <= sur_max & elp >= elp_min,
     order_result := result]
  dm[!is.na(kcd) & is.na(order), `:=`(order = 0, order_result = "na")]
  dm[!is.na(kcd) & is.na(order_result), `:=`(order_result = "dec")]
  do <- dm[!is.na(order_result), .(order = min(order)), .(id, kcd)]
  dm[do, on = .(id, kcd, order), interim_result := order_result]
  dm[is.na(kcd), `:=`(order = 0, interim_result = "std")]
  z <- dm[!is.na(interim_result)]
  z[, final_result := sort_code(order_result), .(id)]
  z[final_result == "", final_result := "std"]
  rm_cols(z, .(result, order_result))
  setorder(z, id, final_result)
  z[grepl("dec", final_result, ignore.case = TRUE),
    decision := "dec"]
  z[is.na(decision) & grepl("na|uwer", final_result, ignore.case = TRUE),
    decision := "uwer"]
  z[is.na(decision) & grepl("excl", final_result, ignore.case = TRUE),
    decision := pull_code_all("excl[0-9]\\([0-9\\-\\|\\[\\]]+\\)", final_result,
                              collapse = ",", ignore.case = TRUE)]
  set(z, i = which(is.na(z$decision)), j = "decision", value = "std")
  set(z, j = "decision", value = toupper(z$decision))
  return(z[])
}

review_rules <- function(rule_info) {
  has_cols(rule_info, c("version", "kcd", "hos_max", "sur_max", "elp_min"))

  g_list <- vector(mode = "list", length = 4L)

  # 1st
  d_version <- rule_info[, .(n = uniqueN(kcd)), keyby = .(version)]
  ymax <- round(max(d_version$n) * 1.3)
  d_version[, label := comma(n)]

  g_list[[1L]] <- ggbar(d_version, x = version, y = n, ymax = ymax, fill = version,
                        label = label, angle = 90, hjust = -.1) +
    scale_y_continuous(labels = comma) +
    ylab("") +
    labs(title = "Number of KCD Rules") +
    theme_view(y.size = 0, panel.background = NULL)

  legend <- get_legend(g_list[[1L]])
  g_list[[1L]] <- g_list[[1L]] + theme_view(y.size = 0, legend.position = "none")

  # 2nd, 3rd, 4th
  factors <- c("hos_max", "sur_max", "elp_min")
  titles <- sprintf("Condition of %s", c("Hospitalization", "Surgery", "Elapsed Days"))

  for (i in seq_along(factors)) {
    col <- factors[i]
    cols <- c("version", col)
    tmp <- rule_info[, .(n = uniqueN(kcd)), keyby = cols]
    ymax <- round(max(tmp$n) * 1.3)
    levels <- sort(unique(tmp[[factors[i]]]))
    tmp[, (col) := factor(get(col), levels = levels), .SDcols = col]
    tmp[, label := comma(n)]
    g_list[[i+1L]] <- ggbar_(tmp, x = factors[i], y = "n", ymax = ymax, fill = "version",
                             label = "label", angle = 90, hjust = -.1) +
      scale_y_continuous(labels = comma) +
      ylab("") +
      labs(title = titles[i]) +
      theme_view(y.size = 0, legend.position = "none", panel.background = NULL)
  }

  top <- grid::textGrob(
    label = expression(italic(underline("Rules Review"))),
    gp = grid::gpar(fontfamily = "Comic Sans MS", fontsize = 16),
    x = 0, just = "left"
  )
  left <- grid::textGrob(
    label = "Number of KCD",
    gp = grid::gpar(fontfamily = "Comic Sans MS"),
    rot = 90
  )
  p <- arrangeGrob(top, arrangeGrob(
    arrangeGrob(
      ggplotGrob(g_list[[1L]]),
      ggplotGrob(g_list[[2L]]),
      ggplotGrob(g_list[[3L]]),
      ggplotGrob(g_list[[4L]]),
      ncol = 2), legend, ncol = 2, widths = c(8.5, 1.5)
  ), nrow = 2, heights = c(1, 9))

  grid.arrange(p,left = left)
  invisible(p)
}
