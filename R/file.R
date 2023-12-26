
create_library_r <- function(file = "library.R", path) {
  if (!missing(path))
    file <- paste0(path, "/", file)
  file.create(file)
  libs <- c(
    "bslib",
    "data.table",
    "devtools",
    "easythemes",
    "ecos",
    "forecast",
    "grid",
    "gridExtra",
    "igraph",
    "kosis",
    "lossratio",
    "openxlsx",
    "plotly",
    "readxl",
    "rintrojs",
    "scales",
    "shiny",
    "shinyBS",
    "shinybusy",
    "shinycssloaders",
    "shinydashboard",
    "shinyFiles",
    "shinyjs",
    "shinymodules",
    "shinyWidgets",
    "stringr",
    "tibble",
    "visNetwork",
    "vuw",
    "writexl"
  )
  cat(
    "\nsuppressPackageStartupMessages({",
    paste0("\n\tlibrary(", libs, ")"), "\r})",
    file = file
  )
}
