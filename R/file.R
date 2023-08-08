
create_library_r <- function(file = "library.R", path) {
  if (!missing(path))
    file <- paste0(path, "/", file)
  file.create(file)
  libs <- c(
    "bslib",
    "data.table",
    "devtools",
    "ecos",
    "gridExtra",
    "kosis",
    "openxlsx",
    "plotly",
    "readxl",
    "rintrojs",
	  "scales",
    "shiny",
    "shinyBS",
    "shinycssloaders",
    "shinydashboard",
    "shinyjs",
    "shinyWidgets",
    "tibble",
    "vuw"
  )
  cat(
    "\nsuppressPackageStartupMessages({",
    paste0("\n\tlibrary(", libs, ")"), "\r})",
    file = file
  )
}
