options(digits = 7, scipen = 999) 
RNGversion("4.4.1") 
set.seed(1234)

if (!exists("dir", mode = "character")) {
  dir <- "~/repo/ui_pubbias"
  output <- paste0(dir, "/release")
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(writexl)
  library(lubridate)
  library(kableExtra)
  library(readxl)
  library(pacman)
  library(magrittr)
  library(BMS)
  library(readr)
  library(xtable)
  library(stats)
  library(testthat)
  library(RColorBrewer)
  library(latex2exp)
  library(pacman)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(ggridges)
  library(lmtest)
  library(sandwich)
  # need to run remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
  library(rlang)
  library(reldist)
  library(ragg)

  filter <- dplyr::filter
  mutate <- dplyr::mutate
  group_by <- dplyr::group_by
  summarise <- dplyr::summarise
  rename <- dplyr::rename
  select <- dplyr::select
  ungroup <- dplyr::ungroup
  left_join <- dplyr::left_join
  inner_join <- dplyr::inner_join
  arrange <- dplyr::arrange
  map_chr <- purrr::map_chr

  setwd("~/repo/ui_pubbias")
  theme_file_path <- "~/repo/ui_pubbias/source/funcs/prelim.R"
  source(theme_file_path)

  source("~/repo/ui_pubbias/source/funcs/metastudies.R")


  get_data <- function(df_name, estimate_str,
                       estimate_column = "estimate", value_column = "value") {
    data <- df_name

    estimate_col <- sym(estimate_column)
    value_col <- sym(value_column)

    filtered_data <- data %>%
      filter(!!estimate_col == estimate_str) %>%
      pull(!!value_col)

    return(filtered_data)
  }
}
