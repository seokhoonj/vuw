
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Written by Joo, Seokhoon.")
  options(scipen = 14)
  options(vuw.eps = 1e-8)
  options(vuw.triple.colors = c("#E41A1C", "#377EB8", "#4DAF4A"))
  options(vuw.gender.colors = c("#00BFC4", "#F8766D"))
  source_python(
    paste(system.file(package = "vuw"), "python/jap2kor.py", sep = "/"),
    envir = parent.frame()
  )
  if (Sys.info()[["sysname"]] == "Linux")
    Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")
}

.onUnload <- function(libpath) {
  library.dynam.unload("vuw", libpath)
}
