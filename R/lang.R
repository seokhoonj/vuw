
# jap2kor <- function(text, api_key_id, api_key) {
#   import("jap2kor")$jap2kor(text, api_key_id, api_key)
# }
# jap2kor4dat <- function(text, api_key_id, api_key) {
#   import("jap2kor")$jap2kor4dat(text, api_key_id, api_key)
# }
# jap2kor4uni <- function(text, api_key_id, api_key) {
#   import("jap2kor")$jap2kor4uni(text, api_key_id, api_key)
# }

set_translate <- function() {
  source_python(
    paste(system.file(package = "vuw"), "python/jap2kor.py", sep = "/"),
    envir = globalenv()
  )
}
is_korean <- function(x) grepl("[ㄱ-ㅎㅏ-ㅣ가-힣]", x, perl = TRUE)
is_english <- function(x) grepl("[a-zA-Z]", x, perl = TRUE)
is_japanese <- function(x) grepl("[一-龯ぁ-んァ-ン]", x, perl = TRUE)
