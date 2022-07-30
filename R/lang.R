
is_korean <- function(x) grepl("[ㄱ-ㅎㅏ-ㅣ가-힣]", x, perl = TRUE)
is_english <- function(x) grepl("[a-zA-Z]", x, perl = TRUE)
is_japanese <- function(x) grepl("[一-龯ぁ-んァ-ン]", x, perl = TRUE)

set_translate <- function() {
  source_python(
    paste(system.file(package = "vuw"), "python/jap2kor.py", sep = "/"),
    envir = globalenv()
  )
}

zen2han <- function(x) {
  if (any(Encoding(x) != "UTF-8"))
    x <- iconv(x, from = "", to = "UTF-8")
  s <- strsplit(x, split = "")
  sapply(seq_along(s), function(x) {
    i <- unlist(stri_enc_toutf32(s[[x]]))
    intToUtf8(ifelse(i >= 65281 & i <= 65374, i-65248, i))
  })
}
