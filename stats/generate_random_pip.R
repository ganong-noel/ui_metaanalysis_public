# ------------------------------------------------------------------------------
# This script creates randon_pip.csv by running Bayesian Model Averaging (BMA) 
# with a random normal covariate included 50 times, and extracting the posterior 
# inclusion probability (PIP) value each time. 
# 
# This script is not part of the main analysis. It is used to support factual 
# information in the body of the text. 
# ------------------------------------------------------------------------------
  
start_time <- Sys.time()

# setup --------------
source("~/repo/ui_pubbias/source/funcs/setup.R")
source(paste0(dir, "/source/funcs/bms.R"))

# import data -------------
bma_input_raw <- read.csv(
  paste0(dir, "/input/clean/bma_input.csv"), encoding = "UTF-8") %>%
  select(-X)

# placeholder for pip ----
pip_results <- data.frame(iteration = 1:50, pip = NA)

RNGversion("4.4.1")
set.seed(1234) # ensure reproducibility

# generate 50 random 
for (i in 1:50) {
  # Generate a random covariate (drawn from standard normal dist.)
  random_covariate <- rnorm(nrow(bma_input_raw)) 
  
  # Add covariate to the dataset used for BMA
  bma_input <- bma_input_raw %>%
    mutate(random_covariate = random_covariate)
  
  # Run BMA
  bma_model <- bms(
    bma_input, burn = 1e5, iter = 3e5, g = "UIP", mprior = "uniform",
    nmodel = 50000, mcmc = "bd", user.int = FALSE,
    randomizeTimer = FALSE)
  
  # Extract PIP for the random covariate
  random_pip <- coef(bma_model)["random_covariate", "PIP"]
  
  # Save the result
  pip_results$pip[i] <- random_pip
  
}

print(pip_results)

write.csv(pip_results, 
          paste0("./stats/random_pip.csv"), row.names = FALSE, 
          fileEncoding = "UTF-8") 

# plot 
ggplot(pip_results, aes(x = pip)) +
  geom_histogram(binwidth = 0.02, fill = "skyblue", color = "black") +
  labs(title = "Histogram of PIP Values",
       x = "PIP",
       y = "Frequency") +
  fte_theme()

ggsave("./stats/random_pip_hist.png", width = 8, height = 4.5, 
       dpi = 300, device = ragg::agg_png)

total_time <- Sys.time() - start_time
print(total_time)  # Time to run
gc()               # Memory used


summary(pip_results$pip)
