rm(list = ls())

source("~/repo/ui_pubbias/source/funcs/setup.R")

code_path <- paste0(dir, "/source/")

start_time <- Sys.time()

source(paste0(code_path, "01_clean_data.R"))
source(paste0(code_path, "02_model_averaging.R")) #40 seconds
source(paste0(code_path, "03_andrews_kasy.R")) #50 seconds
source(paste0(code_path, "04_parametric_ak.R"))
source(paste0(code_path, "05_extra_figures.R"))
source(paste0(code_path, "06_lit_review.R"))
source(paste0(code_path, "07_make_df.R"))

# If seeking to reproduce `random_pip.csv` and `random_pip_hist.png`,
# un-comment the following line of code (40 min runtime):
# source(paste0(dir, "/stats/generate_random_pip.R"))

total_time <- Sys.time() - start_time
print(total_time)  # Time to run
gc()               # Memory used
