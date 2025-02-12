source("~/repo/ui_pubbias/source/funcs/setup.R")

####data cleaning
csv <- paste0(dir, "/input/clean/clean_review_estimates_long.csv")
long_estimates <- read.csv(csv, encoding = "UTF-8") %>%
  dplyr::select(elasticity, se, authors, research_design, pbd_vs_rr,
         reform_id, paper_name_long)


#add duplicates for without hunt
wo_hunt <- long_estimates %>%
  dplyr::filter(!(elasticity < -2 & grepl("Hunt", authors)),
         pbd_vs_rr == "RR") %>%
  dplyr::mutate(pbd_vs_rr = "RRwoHunt")

long_estimates <- rbind(long_estimates, wo_hunt)


####gmm and output
gmm <- paste0(dir, "/source/funcs/PublicationbiasPackage.R")
source(gmm)

process_data <- function(data, data_type) {
  x <- as.matrix(data$elasticity)
  se <- as.matrix(data$se)
  cluster <- as.matrix(data$reform_id)
  names <- as.character(data$paper_name_long)

  symmetric <- 0
  cutoffs <- c(1.96)

  # construct variable names
  var_symmetric <- paste0("gmm_", data_type, "_symmetric_raw")
  var_one_cut   <- paste0("gmm_", data_type, "_one_cut_raw")

  PublicationbiasGMM(x, se, cluster, symmetric, cutoffs, names,
                     var_one_cut)
  if (data_type != "RRwoHunt") {
    symmetric <- 1
    PublicationbiasGMM(x, se, cluster, symmetric, cutoffs, names,
                       var_symmetric)
  }
}

long_estimates %>%
  split(.$pbd_vs_rr) %>%
  imap(~process_data(.x, .y))
