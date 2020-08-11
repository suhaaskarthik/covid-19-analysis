my_packages = c('rvest', 'tidyverse','ggplot2','dplyr','jsonlite','dplyr','lubridate','shiny','plotly','scales')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))