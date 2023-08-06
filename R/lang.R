
is_english <- function(x) grepl("[a-zA-Z]", x, perl = TRUE)
is_korean <- function(x) {
  korean <- c(91, 12593, 45, 12622, 12623, 45, 12643, 44032, 45, 55203, 93)
  grepl(korean, x, perl = TRUE)
}
is_japanese <- function(x) {
  japanese <- intToUtf8(c(91, 19968, 45, 40879, 12353, 45, 12435, 12449, 45, 12531, 93))
  grepl(japanese, x, perl = TRUE)
}

set_translate <- function() {
  # use_python(__python_path__)
  source_python(
    paste(system.file(package = "vuw"), "python/jap2kor.py", sep = "/"),
    envir = globalenv()
  )
}

clean_zen <- function(x) {
  if (Encoding(x[1]) != "UTF-8")
    x <- iconv(x, from = "", to = "UTF-8")
  zenEisu <- paste0(intToUtf8(65295 + 1:10), intToUtf8(65312 + 1:26),
                    intToUtf8(65344 + 1:26))
  zenKigo <- c(65281, 65283, 65284, 65285, 65286, 65290, 65291,
               65292, 12540, 65294, 65295, 65306, 65307, 65308,
               65309, 65310, 65311, 65312, 65342, 65343, 65372,
               65374)
  x <- chartr(zenEisu,"0-9A-Za-z", x)
  x <- chartr(intToUtf8(zenKigo), '!#$%&*+,-./:;<=>?@^_|~', x)
  x <- gsub(intToUtf8(12288), "", x)
  return(x)
}

zen2han <- function(x) {
  if (Encoding(x[1]) != "UTF-8")
    x <- iconv(x, from = "", to = "UTF-8")
  x <- stri_trans_general(x, "Halfwidth-Fullwidth")
  s <- strsplit(x, split = "")
  v <- sapply(seq_along(s), function(x) {
    i <- unlist(stri_enc_toutf32(s[[x]]))
    intToUtf8(ifelse(i >= 65281 & i <= 65374, i-65248, i))
  })
  gsub(intToUtf8(12288), " ", v)
}

zen2han4dat <- function(df) {
  assert_class(df, "data.table")
  setnames(df, zen2han(names(df)))
  cols <- names(which(sapply(df, function(x) any(is_japanese(x)))))
  #df[, (cols) := lapply(.SD, clean_zen), .SDcols = cols]
  df[, (cols) := lapply(.SD, zen2han), .SDcols = cols]
  return(df[])
}
