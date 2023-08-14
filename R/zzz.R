##' @export
jap2kor4uni <- function(data, api_key_id, api_key) {
  return(jap2kor$jap2kor4uni(data, api_key_id, api_key))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Written by Joo, Seokhoon. (Note, kcd terms arguments are monthly-basis)")
  options(scipen = 14)
  options(vuw.eps = 1e-8)
  options(vuw.scipen = 14)
  options(vuw.guess_max = 21474836)
  options(vuw.two.colors1 = c("#80B1D3", "#FB8072"))
  options(vuw.two.colors2 = c("#00BFC4", "#F8766D"))
  options(vuw.two.colors3 = c("#377EB8", "#E41A1C"))
  options(vuw.three.colors1 = c("#4DAF4A", "#377EB8", "#E41A1C"))
  options(vuw.three.colors2 = c("#8DD3C7", "#80B1D3", "#FB8072"))
  if (Sys.info()[["sysname"]] == "Linux")
    Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")
  # source_python(
  #   paste(system.file(package = "vuw"), "python/jap2kor.py", sep = "/"),
  #   envir = environment()
  # )
  # jap2kor <<- reticulate::py_run_file(system.file("python", "jap2kor.py", package = "vuw"))
  jap2kor <<- reticulate::import_from_path(
    "jap2kor", path = system.file("python", package = "vuw"), delay_load = TRUE)
#   user_permission <- utils::askYesNo("Install miniconda? downloads 50MB and takes time")
#   if (isTRUE(user_permission)) {
#     reticulate::install_miniconda()
#   } else {
#     message("You should run `reticulate::install_miniconda()` before using this package")
#   }
}
