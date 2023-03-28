
#' To the a1 style column from an r1c1 style column
#'
#' This function coverts an r1c1 style column to the a1 column
#' @param x a r1c1 style column
#' @export
#' @examples
#' to_a1_col(1)
to_a1_col <- function(x) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  if (x <= 26) {
    return(paste0(names(tbl[x]), collapse = ""))
  }

  quo_vec <- vector(mode = "integer")
  rem_vec <- vector(mode = "integer")
  i <- 1
  while (x > 26) {
    quo_vec[i] <- quo <- x %/% 26
    rem_vec[i] <- rem <- x %% 26
    x <- quo
    i <- i+1L
  }
  quo <- quo_vec[length(quo_vec)]
  z <- c(quo, rev(rem_vec))
  for (i in rev(2:length(z))) {
    if (z[i] == 0) {
      z[i] <- 26
      z[i-1] <- z[i-1] - 1L
    }
  }
  return(paste0(names(tbl[z]), collapse = ""))
}

#' To the r1c1 style column from an a1 style column
#'
#' This function coverts an a1 style column to the r1c1 column
#' @param x a a1 style column
#' @export
#' @examples
#' to_r1c1_col("A")
to_r1c1_col <- function(x) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  spl <- unlist(strsplit(pull_code("[A-Z]+", x), split = "", perl = TRUE))
  num <- tbl[spl]
  dig <- rev(seq_along(num)-1)
  return(sum(num * 26^dig))
}

#' Add rows and columns from a cell
#'
#' This function converts cell range to numeric rows and columns values
#' @param x a cell (a1 style)
#' @param r row
#' @param c col
#' @export
#' @examples
#' nu_cell(x = "A1", r = 1, c = 3)
mv_cell <- function(x, r, c) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  row <- as.integer(pull_code("[0-9]+", x))
  row <- row + r

  col <- pull_code("[A-Z]+", x)
  col <- to_r1c1_col(col)
  col <- col + c

  paste0(to_a1_col(col), row)
}

#' Numeric value of columns, rows
#'
#' This function converts cell range to numeric rows and columns values
#' @param x a cell (a1 style)
#' @param y a cell (a1 style)
#' @export
#' @examples
#' nu_cell(x = "A1", y = "C3")
nu_cell <- function(x, y) {
  r1 <- as.integer(pull_code("[0-9]+", x))
  r2 <- as.integer(pull_code("[0-9]+", y))
  c1 <- to_r1c1_col(pull_code("[A-Z]+", x))
  c2 <- to_r1c1_col(pull_code("[A-Z]+", y))
  z <- list(rows = r1:r2, cols = c1:c2)
  return(z)
}

#' Distance between two cells
#'
#' This function calculates distance between two cells
#' @param x a cell (a1 style)
#' @param y a cell (a1 style)
#' @export
#' @examples
#' di_cell(x = "A1", y = "C3")
di_cell <- function(x, y) {
  r1 <- as.integer( pull_code("[0-9]+", x))
  r2 <- as.integer( pull_code("[0-9]+", y))
  c1 <- to_r1c1_col(pull_code("[A-Z]+", x))
  c2 <- to_r1c1_col(pull_code("[A-Z]+", y))
  rd <- r2 - r1
  cd <- c2 - c1
  z <- list(rows = rd, cols = cd)
  return(z)
}

#' Get risk premium table in the excel file
#'
#' This function convert the risk premium table in the excel file to a new format
#' @param xlsxFile file name character
#' @param sheet character
#' @param risk_rng vector
#' @param rate_rng vector
#' @export
#' @examples
#' get_rp_tbl(xlsxFile, sheet, c("D2", "S2"), c("D3", "S104"))
get_rp_table <- function(xlsxFile, sheet, risk_range, rate_range, skipEmptyRows = TRUE) {
  rate_range <- nu_cell(rate_range[1L], rate_range[2L])
  rate_dat <- readWorkbook(xlsxFile = xlsxFile,
                           sheet    = sheet,
                           rows     = rate_range$rows,
                           cols     = rate_range$cols,
                           skipEmptyRows = skipEmptyRows,
                           skipEmptyCols = TRUE)
  setDT(rate_dat)
  setlowernames(rate_dat)
  rate_dat[, age := seq_len(nrow(rate_dat))-1L]
  rate_tbl <- melt(rate_dat, id.vars = "age", variable.name = "gender", value.name = "rate")
  rm_cols(rate_dat, age)

  risk_range <- nu_cell(risk_range[1L], risk_range[2L])
  risk_dat <- readWorkbook(xlsxFile = xlsxFile,
                           sheet    = sheet,
                           rows     = risk_range$rows,
                           cols     = risk_range$cols,
                           skipEmptyRows = skipEmptyRows,
                           skipEmptyCols = TRUE)
  risk_vec <- names(risk_dat)
  risk_vec <- gsub("\\.", " ", risk_vec)
  each <- length(rate_dat) / length(risk_vec)
  if (each == 2)
    risk_vec <- rep(risk_vec, each = each)
  risk_tbl <- data.table(risk = risk_vec)
  risk_tbl <- reprow(risk_tbl, each = nrow(rate_dat))

  if (nrow(risk_tbl) != nrow(rate_tbl))
    stop("number of risk != number of rate.")

  z <- cbind(risk_tbl, rate_tbl)
  return(z)
}

write_data <- function(wb, sheet, data, rc = c(1L, 1L), rowNames = TRUE,
                       fontName = "Malgun Gothic", borderColour = "#4F81BD",
                       widths = 8.43) {
  headerStyle1 <- createStyle(
    fontName = fontName,
    fontSize = 14,
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    fgFill = "#4F81BD",
    border = "TopRightBottom",
    borderColour = "#000000",
    borderStyle = c("thick", "thin", "double")
  )
  headerStyle2 <- createStyle(
    fontName = fontName,
    fontSize = 14,
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    fgFill = "#4F81BD",
    border = "TopBottom",
    borderColour = "#000000",
    borderStyle = c("thick", "double")
  )
  bodyStyle1  <- createStyle(
    fontName = fontName,
    border = "TopRightBottom",
    borderColour = borderColour
  )
  bodyStyle2 <- createStyle(
    fontName = fontName,
    border = "TopBottom",
    borderColour = borderColour
  )
  footerStyle1 <- createStyle(
    fontName = fontName,
    border = "TopRightBottom",
    borderColour = c(borderColour, borderColour, "#000000"),
    borderStyle = c("thin", "thin", "thick")
  )
  footerStyle2 <- createStyle(
    fontName = fontName,
    border = "TopBottom",
    borderColour = c(borderColour, "#000000"),
    borderStyle = c("thin", "thick")
  )

  writeData(wb = wb, sheet = sheet, x = data, xy = rev(rc), rowNames = rowNames)

  startCell <- rev(rc)
  endCell   <- startCell + dim(data)

  srow <- startCell[1L]
  scol <- startCell[2L]
  erow <- endCell[1L]
  ecol <- endCell[2L]

  if (!rowNames) ecol <- ecol - 1

  headerCols  <- scol:ecol
  headerRows1 <- srow
  headerCols1 <- scol:(ecol-1)
  headerRows2 <- srow
  headerCols2 <- ecol
  bodyRows1   <- (srow+1):(erow-1)
  bodyCols1   <- scol:(ecol-1)
  bodyRows2   <- (srow+1):(erow-1)
  bodyCols2   <- ecol
  footerRows1 <- erow
  footerCols1 <- scol:(ecol-1)
  footerRows2 <- erow
  footerCols2 <- ecol

  addStyle(wb, sheet = sheet, headerStyle1, rows = headerRows1, cols = headerCols1, gridExpand = TRUE)
  addStyle(wb, sheet = sheet, headerStyle2, rows = headerRows2, cols = headerCols2, gridExpand = TRUE)
  addStyle(wb, sheet = sheet, bodyStyle1  , rows = bodyRows1  , cols = bodyCols1  , gridExpand = TRUE)
  addStyle(wb, sheet = sheet, bodyStyle2  , rows = bodyRows2  , cols = bodyCols2  , gridExpand = TRUE)
  addStyle(wb, sheet = sheet, footerStyle1, rows = footerRows1, cols = footerCols1, gridExpand = TRUE)
  addStyle(wb, sheet = sheet, footerStyle2, rows = footerRows2, cols = footerCols2, gridExpand = TRUE)
  setColWidths(wb, sheet, cols = headerCols, widths = widths)
}

write_xlsx <- function(data, file, rc = c(1L, 1L), overwrite = FALSE) {
  wb <- createWorkbook()
  if (is.data.frame(data))
    data <- list(data)
  sheetName <- names(data)
  if (is.null(sheetName))
    sheetName <- sprintf("Sheet %s", seq_along(data))
  lapply(seq_along(data), function(x)
    addWorksheet(wb = wb, sheetName = sheetName[[x]], gridLines = FALSE))
  lapply(seq_along(data), function(x)
    write_data(wb, sheet = sheetName[[x]], data = data[[x]], xy = rev(rc), rowNames = FALSE))
  saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

draw_image <- function(wb, sheet, image, rc = c(1L, 1L), width = 12, height = 6) {
  print(image)
  insertPlot(wb, sheet = sheet, width = width, height = height, xy = rev(rc))
}

draw_xlsx <- function(image, file, rc = c(1L, 1L), width = 12, height = 6, overwrite = FALSE) {
  wb <- createWorkbook()
  if (class(image)[[1L]] == "gg")
    image <- list(image)
  sheetName <- names(image)
  if (is.null(sheetName))
    sheetName <- sprintf("Sheet %s", seq_along(image))
  lapply(seq_along(image), function(x) addWorksheet(wb = wb,
                                                    sheetName = sheetName[[x]], gridLines = FALSE))
  lapply(seq_along(image), function(x) {
    draw_image(wb, sheet = sheetName[[x]], image = image[[x]], rc = rc, width = width, height = height)
  })
  saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

save_xlsx <- function(..., file, width = 12, height = 6, overwrite = FALSE) {
  data_list <- list(...)
  wb <- createWorkbook()
  sheetName <- NULL
  for (i in seq_along(data_list)) {
    data <- data_list[[i]][[1L]]
    if (is.data.frame(data) | is.ggplot(data))
      data <- list(data)
    dataSheetName <- names(data)
    if (is.null(dataSheetName))
      dataSheetName <- sprintf("Sheet %s", seq_along(data))
    sheetName <- c(sheetName, dataSheetName)
  }
  sheetName <- sort(unique(sheetName))
  lapply(seq_along(sheetName), function(x) addWorksheet(wb = wb, sheetName = sheetName[[x]], gridLines = FALSE))
  for (i in seq_along(data_list)) {
    data <- data_list[[i]][[1L]]
    if (is.data.frame(data) | is.ggplot(data))
      data <- list(data)
    rc <- if (length(data_list[[i]]) == 2) data_list[[i]][[2L]] else c(1, 1)
    if (is.data.frame(data[[1L]]))
      lapply(seq_along(data), function(x) {
        write_data(wb, sheet = sheetName[[x]], data = data[[x]], rc = rc, rowNames = FALSE)
      })
    if (is.ggplot(data[[1L]]))
      lapply(seq_along(data), function(x) {
        draw_image(wb, sheet = sheetName[[x]], image = data[[x]], rc = rc, width = width, height = height)
      })
  }
  saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

