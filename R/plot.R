##' Simple ggplot
##'
##' Draw a simple bar plot quickly and easily
##'
##' @param data a data.frame
##' @param x an expression variable
##' @param y an expression variable
##' @param ymin an expression variable
##' @param ymax an expression variable
##' @param ymin_err an expression variable
##' @param ymax_err an expression variable
##' @param group an expression variable
##' @param color an expression variable
##' @param fill an expression variable
##' @param label an expression variable
##' @param family a string
##' @param size a numeric
##' @param angle a numeric
##' @param hjust a numeric
##' @param vjust a numeric
##' @export
ggbar <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                  group = NULL, color = NULL, fill = NULL,
                  barcolor = "transparent", text, label,
                  family = "Comic Sans MS", size = 4, angle = 0,
                  hjust = .5, vjust = .5) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  group <- if (!is.null(substitute(group))) deparse(substitute(group))
  color <- if (!is.null(substitute(color))) deparse(substitute(color))
  fill  <- if (!is.null(substitute(fill)))  deparse(substitute(fill))
  text  <- if (!is.null(substitute(text)))  deparse(substitute(text))
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill, text = text),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single"),
             color = barcolor) +
    list(
      if (!(missing(ymin_err) & missing(ymax_err))) {
        ymin_err <- deparse(substitute(ymin_err))
        ymax_err <- deparse(substitute(ymax_err))
        args_err <- lapply(list(x = x, ymin = ymin_err, ymax = ymax_err),
                           function(x) if (!is.null(x) & !is.numeric(x)) sym(x)
                           else x)
        geom_errorbar(aes(!!!args_err),
                      position = position_dodge2(preserve = "single"),
                      alpha = .5)
      }) +
    list(
      if (!missing(label)) {
        label <- deparse(substitute(label))
        geom_text(aes(label = .data[[label]]),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust,
                  vjust = vjust)
      })
}

ggbar_ <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                   group = NULL, color = NULL, fill = NULL,
                   barcolor = "transparent", text, label,
                   family = "Comic Sans MS", size = 4, angle = 0, hjust = .5,
                   vjust = .5) {
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single"),
             color = barcolor) +
    list(
      if (!(missing(ymin_err) & missing(ymax_err))) {
        args_err <- lapply(list(x = x, ymin = ymin_err, ymax = ymax_err,
                                text = text),
                           function(x) if (!is.null(x) & !is.numeric(x)) sym(x)
                           else x)
        geom_errorbar(aes(!!!args_err),
                      position = position_dodge2(preserve = "single"),
                      alpha = .5)
      }) +
    list(
      if (!missing(label)) {
        geom_text(aes(label = .data[[label]]),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust,
                  vjust = vjust)
      })
}

ggmix <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL,
                  color = NULL, fill = NULL, barcolor = "transparent", text,
                  label, family = "Comic Sans MS", size = 4, angle = 0,
                  hjust = 0.5, vjust = 0.5, reverse = TRUE) {
  x     <- deparse(substitute(x))
  y     <- deparse(substitute(y))
  group <- if (!is.null(substitute(group))) deparse(substitute(group))
  color <- if (!is.null(substitute(color))) deparse(substitute(color))
  fill  <- if (!is.null(substitute(fill)))  deparse(substitute(fill))
  text  <- if (!is.null(substitute(text)))  deparse(substitute(text))
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill, text = text),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_bar(stat = "identity",
             position = position_fill(vjust = 0.5, reverse = reverse)) +
    list(if (!missing(label)) {
      label <- deparse(substitute(label))
      geom_text(aes(label = .data[[label]]),
                position = position_fill(vjust = 0.5, reverse = reverse),
                family = family, size = size, angle = angle, hjust = hjust,
                vjust = vjust, color = "#FAF9F6")
    })
}

ggmix_ <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL,
                   color = NULL, fill = NULL, barcolor = "transparent", text,
                   label, family = "Comic Sans MS", size = 4, angle = 0,
                   hjust = 0.5, vjust = 0.5, reverse = TRUE) {
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill, text = text),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_bar(stat = "identity",
             position = position_fill(vjust = 0.5, reverse = reverse)) +
    list(if (!missing(label)) {
      geom_text(aes(label = .data[[label]]),
                position = position_fill(vjust = 0.5, reverse = reverse),
                family = family, size = size, angle = angle, hjust = hjust,
                vjust = vjust, color = "#FAF9F6")
    })
}


ggline <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                   group = NULL, color = NULL, fill = NULL, text, label,
                   linetype = "solid", family = "Comic Sans MS", size = 4,
                   angle = 0, hjust = .5, vjust = .5) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  group <- if (!is.null(substitute(group))) deparse(substitute(group))
  color <- if (!is.null(substitute(color))) deparse(substitute(color))
  fill  <- if (!is.null(substitute(fill)))  deparse(substitute(fill))
  text  <- if (!is.null(substitute(text)))  deparse(substitute(text))
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill, text = text),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_line(linetype = linetype) +
    list(
      if (!(missing(ymin_err) & missing(ymax_err))) {
        ymin_err <- deparse(substitute(ymin_err))
        ymax_err <- deparse(substitute(ymax_err))
        args_err <- lapply(list(x = x, ymin = ymin_err, ymax = ymax_err),
                           function(x) if (!is.null(x) & !is.numeric(x)) sym(x)
                           else x)
        geom_errorbar(aes(!!!args_err),
                      position = position_dodge2(preserve = "single"),
                      alpha = .5)
      }) +
    list(
      if (!missing(label)) {
        label <- deparse(substitute(label))
        geom_text(aes(label = .data[[label]]),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust,
                  vjust = vjust)
      })
}

ggline_ <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                    group = NULL, color = NULL, fill = NULL, text, label,
                    linetype = "solid", family = "Comic Sans MS", size = 4,
                    angle = 0, hjust = .5, vjust = .5) {
  args <- lapply(list(x = x, y = y, ymin = ymin, ymax = ymax, group = group,
                      color = color, fill = fill, text = text),
                 function(x) if (!is.null(x) & !is.numeric(x)) sym(x) else x)
  ggplot(data = data, aes(!!!args)) +
    geom_line(linetype = linetype) +
    list(
      if (!(missing(ymin_err) & missing(ymax_err))) {
        args_err <- lapply(list(x = x, ymin = ymin_err, ymax = ymax_err),
                           function(x) if (!is.null(x) & !is.numeric(x)) sym(x)
                           else x)
        geom_errorbar(aes(!!!args_err),
                      position = position_dodge2(preserve = "single"),
                      alpha = .5)
      }) +
    list(
      if (!missing(label)) {
        geom_text(aes(label = .data[[label]]),
                  position = position_dodge2(width = .9, preserve = "single"),
                  family = family, size = size, angle = angle, hjust = hjust,
                  vjust = vjust)
      })
}

ggdensity <- function(data, x, facet, probs = .95, logscale = F, digits = 0,
                      scales = "free_y", family = "Comic Sans MS") {
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
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1, digits)), group = area), family = family, hjust = -0.1, vjust = 2)
      } else {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff, digits)), group = area), family = family, hjust = -0.1, vjust = 2)
      }) +
    geom_vline(data = cutoff_data, aes(xintercept = cutoff), color = "red", linetype = "dashed") +
    list(
      if (!missing(facet)) {
        facet_wrap(formula(paste("~", facet)), scales = scales)
      } else {}
    ) +
    ylab("density")
}

ggdensity_ <- function(data, x, facet, probs = .95, logscale = F, digits = 0, scales = "free_y", family = "Comic Sans MS") {
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
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1, digits)), group = area), family = family, hjust = -0.1, vjust = 2)
      } else {
        geom_text(data = cutoff_data,
                  aes(x = cutoff, y = y, label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff, digits)), group = area), family = family, hjust = -0.1, vjust = 2)
      }) +
    geom_vline(data = cutoff_data, aes(xintercept = cutoff), color = "red", linetype = "dashed") +
    list(
      if (!missing(facet)) {
        facet_wrap(formula(paste("~", facet)), scales = scales)
      } else {}
    ) +
    ylab("density")
}

ggpie <- function(data, y, group, family = "Comic Sans MS", size = 4, unit = 100, round = 1) {
  y <- deparse(substitute(y))
  group <- deparse(substitute(group))
  data[[y]] <- round(data[[y]] * unit, round)
  ggplot(data, aes(x = 0, y = .data[[y]], group = .data[[group]], fill = .data[[group]]))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 0) +
    geom_text(aes(label = sprintf("%s", data[[y]])),
              position = position_stack(vjust = .5), family = family, size = size) +
    theme_void()
}

ggpie_ <- function(data, y, group, family = "Comic Sans MS", size = 4, unit = 100, round = 1) {
  data[[y]] <- round(data[[y]] * unit, round)
  ggplot(data, aes_string(x = 0, y = .data[[y]], group = .data[[group]], fill = .data[[group]]))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 0) +
    geom_text(aes(label = sprintf("%s", .data[[y]])),
              position = position_stack(vjust = .5), family = family, size = size) +
    theme_void()
}

ggtable <- function(data, x, y, label, family = "Comic Sans MS",
                    size = 4, angle = 0, hjust = .5, vjust = .5) {
  x <- match_cols(data, deparse(substitute(x)))
  y <- match_cols(data, deparse(substitute(y)))
  label <- match_cols(data, deparse(substitute(label)))

  if (is.character(data[[x]]))
    data[[x]] <- as.factor(data[[x]])
  if (is.character(data[[y]]))
    data[[y]] <- as.factor(data[[y]])

  stopifnot(is.factor(data[[x]]), is.factor(data[[y]]))

  xlvl <- levels(data[[x]])
  ylvl <- levels(data[[y]])
  xlen <- length(xlvl)
  ylen <- length(ylvl)
  data[[x]] <- as.numeric(data[[x]])
  data[[y]] <- as.numeric(data[[y]])
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_text(aes(label = .data[[label]]), size = size, family = family,
              angle = angle, hjust = hjust, vjust = vjust) +
    geom_vline(xintercept = seq(1, 1+xlen) - .5, linetype = "dashed") +
    geom_hline(yintercept = seq(1, 1+ylen) - .5, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, xlen), label = xlvl, position = "top") +
    scale_y_reverse(breaks = seq(1, ylen), label = ylvl)
}

ggtable_ <- function(data, x, y, label, family = "Comic Sans MS",
                    size = 4, angle = 0, hjust = .5, vjust = .5) {
  if (is.character(data[[x]]))
    data[[x]] <- as.factor(data[[x]])
  if (is.character(data[[y]]))
    data[[y]] <- as.factor(data[[y]])

  stopifnot(is.factor(data[[x]]), is.factor(data[[y]]))

  xlvl <- levels(data[[x]])
  ylvl <- levels(data[[y]])
  xlen <- length(xlvl)
  ylen <- length(ylvl)
  data[[x]] <- as.numeric(data[[x]])
  data[[y]] <- as.numeric(data[[y]])
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_text(aes(label = .data[[label]]), size = size, family = family,
              angle = angle, hjust = hjust, vjust = vjust) +
    geom_vline(xintercept = seq(1, 1+xlen) - .5, linetype = "dashed") +
    geom_hline(yintercept = seq(1, 1+ylen) - .5, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, xlen), label = xlvl, position = "top") +
    scale_y_reverse(breaks = seq(1, ylen), label = ylvl)
}

get_legend <- function(plot) {
  gtable <- ggplot_gtable(ggplot_build(plot))
  guide <- which(sapply(gtable$grobs, function(x) x$name) == "guide-box")
  return(gtable$grobs[[guide]])
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
