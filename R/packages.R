
get_packages <- function()
  sapply(.libPaths(), function(x) dir(x))

install_packages <- function(packages) {
  if (missing(packages))
    packages <- c(
      "adabag", "arules", "arulesViz", "bayesplot", "benchmarkme", "bestglm"
    , "Boruta", "brms", "bslib", "calendR", "car", "caret", "caretEnsemble", "caTools"
    , "chemometrics", "circlize", "cluster", "compareGroups", "corrplot"
    , "data.table", "DBI", "devtools", "DiagrammeR", "dplyr", "DT", "dygraphs"
    , "e1071", "earth", "epiR", "extrafont", "faraway", "fastAdaboost", "feather"
    , "forcats", "forecast", "foreign", "formattable", "fortunes", "gamlss"
    , "gbm", "genridge", "ggalluvial", "gganimate", "ggbump", "ggdark", "ggExtra"
    , "ggfortify", "ggmap", "ggnetwork", "ggparliament", "ggplot2", "ggridges"
    , "ggstream", "ggthemes", "ggVennDiagram", "ggvoronoi", "ggwordcloud", "glmnet"
    , "gridExtra", "gvlma", "haven", "highcharter", "Hmisc", "htmlwidgets"
    , "imager", "installr", "investr", "ISLR", "jsonlite"
    , "kernlab", "kknn", "KMsurv", "leaflet", "leaps", "lightgbm", "lmtest"
    , "lubridate", "maps", "metricsgraphics", "mice", "microbenchmark"
    , "missForest", "mlbench", "mlr", "MPV", "multilevel", "multilinguer"
    , "NbClust", "networkD3", "neuralnet", "odbc", "openssl", "openxlsx"
    , "party", "partykit", "plotly", "pls", "pROC", "profvis", "proto", "pryr"
    , "pscl", "psych", "purrr", "qdap", "quanteda", "quantmod", "quantreg"
    , "randomForest", "rattle", "RColorBrewer", "Rcpp", "RcppArmadillo"
    , "RcppEigen", "RcppParallel", "RCurl", "readr", "readxl", "recommenderlab"
    , "reticulate", "Rfast", "rgl", "rglwidget", "ridge", "rintrojs", "rio", "rJava"
    , "rlang", "rmarkdown", "RMySQL", "ROCR", "RODBC", "roxygen2", "rpart"
    , "rpart.plot", "RSQLite", "rstudioapi", "sampling", "scales", "scatterplot3d"
    , "segmented", "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs"
    , "shinymanager", "shinyWidgets", "showtext", "slickR"
    , "snpsettest", "sparcl", "statmod", "stplanr", "stringi", "stringr"
    , "survival", "survminer", "testthat", "textclean", "tibble", "tidybayes"
    , "tidymodels", "tidyr", "tidytext", "tm", "topicmodels", "TraMineR"
    , "treemap", "truncreg", "tseries", "tweedie", "utf8", "vars", "vcd", "VGAM"
    , "VIM", "wordcloud", "xgboost", "xlsx", "XML", "xtable"
    )
  avbl_pkgss <- data.table(utils::available.packages())
  packages <- avbl_pkgss[Package %in% packages]$Package
  dependencies <- tools::package_dependencies(packages, db = avbl_pkgss, recursive = TRUE)
  pkgs_db <- data.table(cbind(dependencies), keep.rownames = "package")
  pkgs_list <- sort(unique(c(pkgs_db$package, unlist(pkgs_db$dependencies))))
  pkgs_add <- unlist(sapply(.libPaths(), function(x) dir(x), USE.NAMES = FALSE))
  pkgs_list <- pkgs_list[!pkgs_list %in% pkgs_add]

  while (length(pkgs_list) > 0) {
    cat(sprintf("\n%d package(s) left.\n", length(pkgs_list)))
    pkg <- pkgs_list[1L]
    if (Sys.info()["sysname"] == "Linux") {
      if (!requireNamespace(pkg, quietly = T)) install.packages(pkg)
    } else {
      if (!requireNamespace(pkg, quietly = T)) install.packages(pkg, type = "binary")
    }
    pkgs_add <- unlist(sapply(.libPaths(), function(x) dir(x), USE.NAMES = FALSE))
    pkgs_list <- pkgs_list[!pkgs_list %in% pkgs_add]
    if (length(pkgs_list) > 1 & pkgs_list[1L] == pkg)
      stop("Installing '", pkg, "' is failed.")
  }

}
