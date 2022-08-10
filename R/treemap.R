
data2treemap <- function(df, group_var, value_var, fig = TRUE, add_names = FALSE, sep = " / ") {
  assert_class(df, "data.table")
  group_cols <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  value_cols <- match_cols(df, vapply(substitute(value_var), deparse, "character"))
  if (add_names)
    df[, group_cols] <- lapply(seq_along(group_cols), function(x) paste(rep(columns[x], nrow(df)), df[[x]]))
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
  fig <- plot_ly(
    type = "treemap",
    branchvalues = "total",
    labels  = props$labels,
    parents = props$parents,
    values  = props[[value_cols[[1L]]]],
    marker = list(colors = c("", rep(brewer.pal(12L, "Set3"), ceiling(nrow(props)/12)))),
    textinfo = "label+value+percent entry+percent parent+percent root",
    hoverinfo = "percent entry+percent parent+percent root",
    domain = list(column = 0)
  )
  attr(props, "fig") <- fig
  return(props)
}
