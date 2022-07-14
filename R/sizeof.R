
# sizeof ------------------------------------------------------------------

#' Object size
#'
#' This function calculates the object size
#' @title Object size
#' @param x object
#' @param unit size unit; "B", "KB", "MB", "GB"
#' @examples
#' sizeof(iris, unit = "kb")
#' @export
sizeof <- function(x, unit) UseMethod("sizeof")

#' @rdname sizeof
#' @export
sizeof.default <- function(x, unit = "mb") {
  class <- paste0(class(x), collapse = ",")
  type  <- typeof(x)
  size  <- object.size(x)[1L]
  e     <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.table(class = class,
             type  = type,
             size  = round(size / (2^10)^e, 3),
             unit  = toupper(unit))
}

#' @rdname sizeof
#' @export
sizeof.data.frame <- function(x, unit = "mb") {
  col   <- names(x)
  class <- sapply(x, class)
  type  <- sapply(x, typeof)
  size  <- c(sapply(x, function(s) object.size(s)), object.size(x))
  e     <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.table(col   = c(col,   "total"),
             class = c(class, "total"),
             type  = c(type,  "total"),
             size  = round(size / (2^10)^e, 3),
             unit  = toupper(unit))
}

#' @rdname sizeof
#' @export
sizeof.environment <- function(x, unit = "mb") {
  env <- ls(envir = x)
  if (length(env) == 0L) stop("Object not found.")
  class <- sapply(env, function(s) paste0(class(get(s, envir = x)), collapse = ","))
  size  <- sapply(env, function(s) object.size(get(s, envir = x)))
  sizes <- c(size, sum(size))
  m   <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.table(obj   = c(names(size), "total"),
             class = c(class, "total"),
             size  = round(sizes / (2^10)^m, 3),
             unit  = toupper(unit))
}
