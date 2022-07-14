
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
    hprint(unique(claim_info[, .(rn, main_category, sub_category, rd_category, rider)]))
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
  hprint(risk_info_sub[, .(gender, age, grade, rate, scale, risk)])
  invisible(risk_info_sub)
}

risk_plot <- function(risk_info, x, nrow = NULL, ncol = NULL, scales = "free_y") {
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
  colors <- options()$vuw.gender.colors
  ggplot(risk_info, aes(x = age, y = rate, group = gender, col = gender)) +
    geom_line() + scale_color_manual(values = colors) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    facet_wrap(~ risk, nrow = nrow, ncol = ncol, scales = scales)
}
