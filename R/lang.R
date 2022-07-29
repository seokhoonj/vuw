
jap2kor <- function(text, api_key_id, api_key) {
  reticulate::import("jap2kor")$jap2kor(text, api_key_id, api_key)
}

is_korean <- function(x) grepl("[ㄱ-ㅎㅏ-ㅣ가-힣]", x, perl = TRUE)
is_english <- function(x) grepl("[a-zA-Z]", x, perl = TRUE)
is_japanese <- function(x) grepl("[一-龯ぁ-んァ-ン]", x, perl = TRUE)

