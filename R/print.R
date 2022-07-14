
# print functions ---------------------------------------------------------

draw_line <- function(width, mark = "=") {
  if (missing(width))
    width <- options()$width
  sapply(width, function(x)
    paste0(paste0(rep(mark, times = min(x, options()$width)), collapse = "")))
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
