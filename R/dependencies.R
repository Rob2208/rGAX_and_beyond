pkgs <- c(
  "comets",
  "tidyverse",
  "xgboost",
  "caret",
  "coin",
  "ggpubr",
  "latex2exp",
  "Cairo",
  "injurytools",
  "survival"
)

install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_cran(pkgs, repos = "https://cloud.r-project.org")
