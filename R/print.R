
# print functions ---------------------------------------------------------

draw_line <- function(width, mark = "=") {
  if (missing(width))
    width <- options()$width
  sapply(width, function(x)
    paste0(paste0(rep(mark, times = ifelse(
      !is.na(x), min(x, options()$width), 0)), collapse = "")
    ))
}

reduce_rows <- function(x, n = 242L) {
  tn <- nrow(x)
  if (tn > 242L)
    return(rbind(head(x, n/2), tail(x, n/2)))
  return(x)
}

na2str <- function(x)
  if (is.character(x)) ifelse(is.na(x), "", x) else x

repaste <- function(x, sep = "|") {
  n <- length(x)
  if (n == 1L) {
    return(x[[1L]])
  } else {
    x[[n-1]] <- paste0(x[[n-1]], sep, x[[n]])
    x[[n]] <- NULL
    repaste(x, sep = sep)
  }
}

adjust_column_width <- function(x, hchar) {
  df <- reduce_rows(as.data.frame(x))
  cols <- names(df)
  nchar_cols <- nchar(cols)
  notc_cols_no <- which(sapply(df, class) != "character")
  if (length(notc_cols_no) > 0)
    df[, notc_cols_no] <- lapply(df[, notc_cols_no, drop = FALSE], as.character)
  width <- sapply(df, function(x) if (all(is.na(x))) 2L else max(nchar(x), na.rm = T))
  if (!missing(hchar))
    width <- pmax(width, min(hchar, max(nchar_cols)))
  df[] <- lapply(df, na2str)
  side <- sapply(df, function(x) if (is.character(x)) "right" else "left")
  df[] <- lapply(seq_along(df), function(x)
    str_pad(df[[x]], width = width[x], side = side[x]))
  abb_cols <- substr(names(width), 1L, width)
  new_cols <- str_pad(abb_cols, width = width, pad = " ", side = "both")
  names(df) <- new_cols
  attr(df, "columns") <- cols
  attr(df, "width") <- width
  attr(df, "side") <- side
  return(df)
}

hprint <- function(x, hchar = 4) {
  df <- adjust_column_width(x, hchar = hchar)
  txt <- repaste(df)
  cols <- colnames(df)
  cat(draw_line(), "\n")
  cat(paste0("|", paste0(cols, collapse = "|"), "\n"))
  cat(draw_line(), "\n")
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cat(draw_line(), "\n")
}

vprint <- function(x, hchar = 4, vchar = 16) {
  df <- adjust_column_width(x, hchar = hchar)
  txt <- repaste(df)
  cols <- toupper(attr(df, "columns"))
  width <- max(nchar(cols))
  dots <- str_pad(cols, width = width, pad = " ", side = "right")
  vcols <- lapply(seq(1, min(vchar+1, width), hchar),
                  function(x) paste0(
                    str_pad(substr(dots, x, x+hchar-1),
                            width = attr(df, "width"),
                            pad = " ",
                            side = "both"),
                    collapse = "|"))
  cat(draw_line(), "\n")
  cat(paste0(paste0("|", vcols), collapse = "\n"), "\n")
  cat(draw_line(), "\n")
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cat(draw_line(), "\n")
}

aprint <- function(x, hchar = 4, vchar = 16) {
  df <- adjust_column_width(x, hchar = hchar)
  txt <- repaste(df)
  cols <- toupper(attr(df, "columns"))
  width <- max(nchar(cols))
  dots <- str_pad(cols, width = width, pad = " ", side = "right")
  vcols <- lapply(seq(1, min(vchar+1, width), hchar),
                  function(x) paste0(
                    str_pad(substr(dots, x, x+hchar-1),
                            width = attr(df, "width"),
                            pad = " ",
                            side = "both"),
                    collapse = "|"))
  cat(draw_line(), "\n")
  cat(paste0(paste0("|", vcols), collapse = "\n"), "\n")
  cat(draw_line(), "\n")
  cat(paste0("|", paste0(names(df), collapse = "|"), "\n"))
  cat(draw_line(), "\n")
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cat(draw_line(), "\n")
}

ggbar <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL, color = NULL, fill = NULL,
                  label, family = "Malgun Gothic", size = 4, angle = 0, hjust = .5, vjust = .5) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  group <- deparse(substitute(group))
  color <- deparse(substitute(color))
  fill  <- deparse(substitute(fill))
  ggplot(data = data, aes_string(
    x = x, y = y, ymin = ymin, ymax = ymax, group = group, color = color, fill = fill)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + list(
      if (!missing(label)) {
        label <- deparse(substitute(label))
        geom_text(aes_string(label = label),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust, vjust = vjust)
      })
}

ggbar_ <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL, color = NULL, fill = NULL,
                   label, family = "Malgun Gothic", size = 4, angle = 0, hjust = .5, vjust = .5) {
  ggplot(data = data, aes_string(
    x = x, y = y, ymin = ymin, ymax = ymax, group = group, color = color, fill = fill)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + list(
      if (!missing(label)) {
        geom_text(aes_string(label = label),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust, vjust = vjust)
      })
}

ggline <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL, color = NULL, fill = NULL,
                   label, family = "Malgun Gothic", size = 4, angle = 0, hjust = .5, vjust = .5) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  group <- deparse(substitute(group))
  color <- deparse(substitute(color))
  fill <- deparse(substitute(fill))
  ggplot(data = data, aes_string(x = x, y = y, ymin = ymin, ymax = ymax, group = group, color = color, fill = fill)) +
    geom_line() + list(
      if (!missing(label)) {
        label <- deparse(substitute(label))
        geom_text(aes_string(label = label),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust, vjust = vjust)
      })
}

ggline_ <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL, color = NULL, fill = NULL,
                    label, family = "Malgun Gothic", size = 4, angle = 0, hjust = .5, vjust = .5) {
  ggplot(data = data, aes_string(x = x, y = y, ymin = ymin, ymax = ymax, group = group, color = color, fill = fill)) +
    geom_line() + list(
      if (!missing(label)) {
        geom_text(aes_string(label = label),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust, vjust = vjust)
      })
}

ggdensity <- function(data, x, facet, probs = .95, logscale = F, scales = "free_y") {
  x <- match_cols(data, vapply(substitute(x), deparse, "character"))
  if (logscale)
    data[[x]] <- log(data[[x]] + 1)
  if (missing(facet)) {
    dens <- data[, .(x = density(get(x))$x, y = density(get(x))$y)]

    cutoff_dens <- dens[, .(prob = probs, cutoff = quantile(x, probs = probs))]
    cutoff_dens[, prob := as.numeric(as.character(prob))]

    cutoff_data <- data[, .(prob = probs, cutoff = quantile(get(x), probs = probs))]
    cutoff_data[, prob := as.numeric(as.character(prob))]
  } else {
    facet <- match_cols(data, vapply(substitute(facet), deparse, "character"))
    dens <- data[, .(x = density(get(x))$x, y = density(get(x))$y), keyby = facet]

    cutoff_dens <- dens[, lapply(probs, function(p) quantile(x, probs = p)), keyby = facet]
    setnames(cutoff_dens, as.character(c(facet, probs)))
    cutoff_dens <- melt(cutoff_dens, id.vars = facet, variable.name = "prob", value.name = "cutoff")
    cutoff_dens[, prob := as.numeric(as.character(prob))]

    cutoff_data <- data[, lapply(probs, function(p) quantile(get(x), probs = p)), keyby = facet]
    setnames(cutoff_data, as.character(c(facet, probs)))
    cutoff_data <- melt(cutoff_data, id.vars = facet, variable.name = "prob", value.name = "cutoff")
    cutoff_data[, prob := as.numeric(as.character(prob))]
  }

  cutoff <- cutoff_data[prob == probs[length(probs)]]
  levels <- paste0(c(">", "<"), probs[length(probs)] * 1e2, "%")

  cutoff_data[, y := Inf]
  cutoff_data[, area := levels[1L]]

  if (missing(facet)) {
    dens[, cutoff := cutoff$cutoff]
  } else {
    dens[cutoff, cutoff := i.cutoff, on = facet]
  }
  dens[, `:=`(area, factor(ifelse(x >= cutoff, levels[1L], levels[2L]), levels = levels))]

  ggplot(data = dens, aes(x = x, ymin = 0, ymax = y, group = area, fill = area)) +
    geom_ribbon() + geom_line(aes(y = y)) +
    list(
      if (logscale) {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1)), group = area), hjust = -0.1, vjust = 2)
      } else {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff)), group = area), hjust = -0.1, vjust = 2)
      }) +
    geom_vline(data = cutoff_data, aes(xintercept = cutoff), color = "red", linetype = "dashed") +
    list(
      if (!missing(facet)) {
        facet_wrap(formula(paste("~", facet)), scales = scales)
      } else {}
    ) +
    ylab("density")
}

ggdensity_ <- function(data, x, facet, probs = .95, logscale = F, scales = "free_y") {
  if (logscale)
    data[[x]] <- log(data[[x]] + 1)
  if (missing(facet)) {
    dens <- data[, .(x = density(get(x))$x, y = density(get(x))$y)]

    cutoff_dens <- dens[, .(prob = probs, cutoff = quantile(x, probs = probs))]
    cutoff_dens[, prob := as.numeric(as.character(prob))]

    cutoff_data <- data[, .(prob = probs, cutoff = quantile(get(x), probs = probs))]
    cutoff_data[, prob := as.numeric(as.character(prob))]
  } else {
    dens <- data[, .(x = density(get(x))$x, y = density(get(x))$y), keyby = facet]

    cutoff_dens <- dens[, lapply(probs, function(p) quantile(x, probs = p)), keyby = facet]
    setnames(cutoff_dens, as.character(c(facet, probs)))
    cutoff_dens <- melt(cutoff_dens, id.vars = facet, variable.name = "prob", value.name = "cutoff")
    cutoff_dens[, prob := as.numeric(as.character(prob))]

    cutoff_data <- data[, lapply(probs, function(p) quantile(get(x), probs = p)), keyby = facet]
    setnames(cutoff_data, as.character(c(facet, probs)))
    cutoff_data <- melt(cutoff_data, id.vars = facet, variable.name = "prob", value.name = "cutoff")
    cutoff_data[, prob := as.numeric(as.character(prob))]
  }

  cutoff <- cutoff_data[prob == probs[length(probs)]]
  levels <- paste0(c(">", "<"), probs[length(probs)] * 1e2, "%")

  cutoff_data[, y := Inf]
  cutoff_data[, area := levels[1L]]

  if (missing(facet)) {
    dens[, cutoff := cutoff$cutoff]
  } else {
    dens[cutoff, cutoff := i.cutoff, on = facet]
  }
  dens[, `:=`(area, factor(ifelse(x >= cutoff, levels[1L], levels[2L]), levels = levels))]

  ggplot(data = dens, aes(x = x, ymin = 0, ymax = y, group = area, fill = area)) +
    geom_ribbon() + geom_line(aes(y = y)) +
    list(
      if (logscale) {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1)), group = area), hjust = -0.1, vjust = 2)
      } else {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff)), group = area), hjust = -0.1, vjust = 2)
      }) +
    geom_vline(data = cutoff_data, aes(xintercept = cutoff), color = "red", linetype = "dashed") +
    list(
      if (!missing(facet)) {
        facet_wrap(formula(paste("~", facet)), scales = scales)
      } else {}
    ) +
    ylab("density")
}

ggpie <- function(data, y, group, size = 4, unit = 100, round = 1) {
  y <- deparse(substitute(y))
  group <- deparse(substitute(group))
  data[[y]] <- round(data[[y]] * unit, round)
  ggplot(data, aes_string(x = 0, y = y, group = group, fill = group))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 0) +
    geom_text(aes_string(label = sprintf("%s", y)),
              position = position_stack(vjust = .5), size = size) +
    theme_void()
}

ggpie_ <- function(data, y, group, size = 4, unit = 100, round = 1) {
  data[[y]] <- round(data[[y]] * unit, round)
  ggplot(data, aes_string(x = 0, y = y, group = group, fill = group))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 0) +
    geom_text(aes_string(label = sprintf("%s", y)),
              position = position_stack(vjust = .5), size = size) +
    theme_void()
}

data2treemap <- function(df, group_var, value_var, fig = TRUE, add_names = FALSE, sep = " / ") {
  assert_class(df, "data.table")
  group_cols <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  value_cols <- match_cols(df, vapply(substitute(value_var), deparse, "character"))
  if (add_names)
    df[, group_cols] <- lapply(seq_along(group_cols), function(x)
      paste(rep(group_cols[x], nrow(df)), df[[group_cols[x]]]))
  prop0 <- data.table(parents = "", labels = "Total", df[, lapply(.SD, sum), .SDcols = value_cols])
  props <- lapply(seq_along(group_cols), function(x) {
    label_cols <- group_cols[1:x]
    prop <- df[, lapply(.SD, sum), by = label_cols, .SDcols = value_cols]
    if (x > 1) {
      parent_cols <- group_cols[1:(x-1)]
      parents <- repaste(prop[, ..parent_cols], sep = sep)
      labels <- repaste(prop[, ..label_cols], sep = sep)
    } else {
      parents <- "Total"
      labels <- repaste(prop[, ..label_cols], sep = sep)
    }
    data.table(parents = parents,
               labels = labels,
               prop[, ..value_cols])
  })
  props <- rbind(prop0, do.call("rbind", props))
  if (fig) {
    g <- plot_ly(
      type = "treemap",
      branchvalues = "total",
      labels  = props$labels,
      parents = props$parents,
      values  = props[[value_cols]],
      marker = list(colors = c("", rep(brewer.pal(12, "Set3"), ceiling(nrow(props)/12)))),
      # hoverinfo = "text",
      # text = ~paste("</br> Count: ", props[[value_codls[[1L]]]],
      #               "</br> Stay: ", props[[value_cols[[2L]]]]),
      textinfo = "label+value+percent entry+percent parent+percent root",
      hoverinfo = "percent entry+percent parent+percent root",
      domain = list(column = 0)
    )
    attr(props, "fig") <- g
    print(g)
  }
  return(props)
}
