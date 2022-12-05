
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Written by Joo, Seokhoon.")
  options(vuw.eps = 1e-8)
  options(vuw.scipen = 14)
  options(vuw.guess_max = 21474836)
  options(vuw.two.colors1 = c("#377EB8", "#E41A1C"))
  options(vuw.two.colors2 = c("#00BFC4", "#F8766D"))
  options(vuw.two.colors3 = c("#80B1D3", "#FB8072"))
  options(vuw.three.colors1 = c("#4DAF4A", "#377EB8", "#E41A1C"))
  options(vuw.three.colors2 = c("#8DD3C7", "#80B1D3", "#FB8072"))
  if (Sys.info()[["sysname"]] == "Linux")
    Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")
}

.onUnload <- function(libpath) {
  library.dynam.unload("vuw", libpath)
}
