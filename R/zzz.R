
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Written by Seokhoon Joo. (Note, kcd terms arguments are monthly-basis)")
  op <- options()
  op.vuw <- list(
    vuw.eps = 1e-8,
    vuw.scipen = 14,
    vuw.guess_max = 21474836,
    vuw.two.colors1 = c("#80B1D3", "#FB8072"),
    vuw.two.colors2 = c("#00BFC4", "#F8766D"),
    vuw.two.colors3 = c("#377EB8", "#E41A1C"),
    vuw.three.colors1 = c("#4DAF4A", "#377EB8", "#E41A1C"),
    vuw.three.colors2 = c("#8DD3C7", "#80B1D3", "#FB8072")
  )
  toset <- !(names(op.vuw) %in% names(op))
  if (any(toset)) options(op.vuw[toset])

  if (Sys.info()[["sysname"]] == "Linux")
    Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")

  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("vuw", libpath)
}

