
get_packages <- function()
  sapply(.libPaths(), function(x) dir(x))

install_packages <- function(packages) {
  if (tolower(packages) == "all")
    packages <- c(
      "adabag", "arules", "arulesViz", "bayesplot", "benchmarkme", "bestglm"
    , "Boruta", "brms", "calendR", "car", "caret", "caretEnsemble", "caTools"
    , "chemometrics", "circlize", "cluster", "compareGroups", "corrplot"
    , "data.table", "DBI", "devtools", "DiagrammeR", "dplyr", "DT", "dygraphs"
    , "e1071", "earth", "epiR", "extrafont", "faraway", "fastAdaboost", "feather"
    , "forcats", "forecast", "foreign", "formattable", "fortunes", "gamlss"
    , "gbm", "genridge", "ggalluvial", "gganatogram", "gganimate", "ggbump"
    , "ggdark", "ggExtra", "ggfortify", "ggmap", "ggnetwork", "ggparliament"
    , "ggplot2", "ggradar", "ggridges", "ggsankey", "ggstream", "ggteck"
    , "ggthemes", "ggVennDiagram", "ggvoronoi", "ggwordcloud", "glmnet"
    , "gridExtra", "gvlma", "haven", "highcharter", "Hmisc", "htmlwidgets"
    , "imager", "InformationValue", "installr", "investr", "ISLR", "jsonlite"
    , "kernlab", "kknn", "KMsurv", "leaflet", "leaps", "lightgbm", "lmtest"
    , "lubridate", "maps", "metricsgraphics", "mice", "microbenchmark"
    , "missForest", "mlbench", "mlr", "MPV", "multilevel", "multilinguer"
    , "NbClust", "networkD3", "neuralnet", "odbc", "openssl", "openxlsx"
    , "party", "partykit", "plotly", "pls", "pROC", "profvis", "proto", "pryr"
    , "pscl", "psych", "purrr", "qdap", "quanteda", "quantmod", "quantreg"
    , "randomForest", "rattle", "RColorBrewer", "Rcpp", "RcppArmadillo"
    , "RcppEigen", "RcppParallel", "RCurl", "readr", "readxl", "recommenderlab"
    , "reticulate", "Rfast", "rgl", "rglwidget", "ridge", "rio", "rJava"
    , "rlang", "rmarkdown", "RMySQL", "ROCR", "RODBC", "roxygen2", "rpart"
    , "rpart.plot", "RSQLite", "rstudioapi", "sampling", "scales", "scatterplot3d"
    , "segmented", "shiny", "shinyjs", "shinymanager", "showtext", "slickR"
    , "snpsettest", "sparcl", "statmod", "stplanr", "stringi", "stringr"
    , "survival", "survminer", "testthat", "textclean", "tibble", "tidybayes"
    , "tidymodels", "tidyr", "tidytext", "tm", "topicmodels", "TraMineR"
    , "treemap", "truncreg", "tseries", "tweedie", "utf8", "vars", "vcd", "VGAM"
    , "VIM", "wordcloud", "xgboost", "xlsx", "XML", "xtable"
    )
  db_pkg <- utils::available.packages()
  dependencies <- tools::package_dependencies(packages, db = db_pkg, recursive = TRUE)
  pkg <- data.table(cbind(dependencies), keep.rownames = "package")
  pkg_lst <- sort(unique(c(pkg$package, unlist(pkg$dependencies))))
  pkg_add <- unlist(sapply(.libPaths(), function(x) dir(x), USE.NAMES = FALSE))
  pkg_lst <- pkg_lst[!pkg_lst %in% pkg_add]
  while (length(pkg_lst) > 0) {
    cat(sprintf("\n%d package(s) left.\n", length(pkg_lst)))
    if (Sys.info()["sysname"] == "Linux") {
      if (!requireNamespace(pkg_lst[1L], quietly = T)) install.packages(pkg_lst[1L])
    } else {
      if (!requireNamespace(pkg_lst[1L], quietly = T)) install.packages(pkg_lst[1L], type = "binary")
    }
    pkg_add <- unlist(sapply(.libPaths(), function(x) dir(x), USE.NAMES = FALSE))
    pkg_lst <- pkg_lst[!pkg_lst %in% pkg_add]
  }

  # suppressPackageStartupMessages({
  #   if (!require(vuw))      install_github("seokhoonj/vuw"   , upgrade = "never")
  #   if (!require(lineprof)) install_github("hadley/lineprof" , upgrade = "never")
  #   if (!require(ggbiplot)) install_github("vqv/ggbiplot"    , upgrade = "never")
  #   if (!require(twidlr))   install_github("drsimonj/twidlr" , upgrade = "never")
  #   if (!require(KoNLP))    install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))
  # })
}
