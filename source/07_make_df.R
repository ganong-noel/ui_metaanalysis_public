source("~/repo/ui_pubbias/source/funcs/setup.R")

csv <- paste0(dir, "/input/clean/clean_review_estimates_long.csv")
data <- read.csv(csv, encoding = "UTF-8")
pbd_data <- data %>% filter(pbd_vs_rr == "PBD")
rr_data <- data %>% filter(pbd_vs_rr == "RR")
bma_data <- bma_coef_weighted_raw
bma_input <- read.csv(paste0(dir, "/input/clean/bma_input.csv"))
oecd_rr <- read.csv(paste0(dir, "/input/raw/oecd_rr_data.csv"))
oecd_wedge <- read.csv(paste0(dir, "/input/raw/oecd_wedge_data.csv"))

europe_countries <- c("Austria", "Germany", "Norway", "Sweden", "Finland",
                      "France", "Netherlands", "Portugal", "Slovenia", "Spain",
                      "Switzerland", "Belgium", "Denmark", "Estonia", "Greece",
                      "Hungary", "Ireland", "Iceland", "Italy", "Latvia",
                      "Lithuania", "Luxembourg", "Poland")

#baily_chetty calculation function
bc_optimal <- function(elasticity, slope = 0) {
  e_g_correction <- 1/(2 * 0.95)
  return ((elasticity * e_g_correction -  0.435 * slope * e_g_correction - 0.222) /
            (-0.265 - slope * e_g_correction))
}

num_studies <- get_data(data_characteristics_raw, "num_studies")
num_estimates <- nrow(data)
num_rr_estimates <- data |> filter(PBD_indicator == 0) |> nrow()
num_pbd_estimates <- data |> filter(PBD_indicator == 1) |> nrow()
num_margin <- nrow(read.csv(paste0(dir, "/input/clean/bma_input.csv"))) + 1
pbd_beta <- get_data(summary_table_descriptives_PBD_raw, "t_beta")
rr_beta <- get_data(summary_table_descriptives_RR_raw, "t_beta")
ratio_significant_rr <- 1 / rr_beta
ratio_significant_pbd <- 1 / pbd_beta
pbd_mean <- get_data(summary_table_descriptives_PBD_raw,
                     "mean_elasticity")
rr_mean <- get_data(summary_table_descriptives_RR_raw,
                    "mean_elasticity")
pbd_bma_average <- get_data(bma_elasticities_raw, "PBD")
rr_bma_average <- get_data(bma_elasticities_raw, "RR")
pbd_theta <- get_data(summary_table_descriptives_PBD_raw,
                      "t_theta")
rr_theta <- get_data(summary_table_descriptives_RR_raw,
                     "t_theta")
correction_ak <- 1 - rr_theta / rr_mean

rrxrr_interaction_coef <- get_data(bma_coef_weighted_raw,
                              "Baseline RR (fraction) x RR estimate",
                              "X", "Posterior.Mean")
bc_bma_const <- bc_optimal(rr_bma_average, rrxrr_interaction_coef)
bc_avg_rr <- bc_optimal(rr_bma_average)
bc_ak <- bc_optimal(rr_theta)

top_tercile_av <- get_data(tercile_summary_raw, 3,
                           "se_tercile", "mean_elasticity")
mid_tercile_av <- get_data(tercile_summary_raw, 2,
                           "se_tercile", "mean_elasticity")
bottom_tercile_av <- get_data(tercile_summary_raw, 1,
                              "se_tercile", "mean_elasticity")
ratio_top_bottom_tercile <- top_tercile_av / bottom_tercile_av

num_journal_publications <- nrow(read.csv(paste0(dir, "/input/clean/PoPCites_cleaned.csv"), encoding = "UTF-8"))

percent_quasi <- get_data(data_characteristics_raw, "num_quasi") / num_studies
median_impact_factor <- quantile(data$impact_factor, 0.5)
quartile_impact_factor <- quantile(data$impact_factor, 0.25)

field_ratio <- get_data(data_characteristics_raw,
                        "field journal count") / num_studies
ratio_top5 <- get_data(data_characteristics_raw,
                                 "top 5 count") / num_studies
ratio_method <- get_data(data_characteristics_raw,
                                      "econometric method count") / num_studies

macro_studies <- get_data(data_characteristics_raw, "macro_treatment count")

min_base_rr <- min(data$mean_rr)
max_base_rr <- max(data$mean_rr)
iqr_base_rr <- IQR(data$mean_rr, na.rm = TRUE)

pbd_quasi_ratio <- 0.9 #1 - other column in table b.2
rr_quasi_ratio <- 0.45 #1 - other column in table b.2

hunt_elasticity <- data %>% filter(elasticity < -2 & grepl("Hunt", authors)) %>% pull(elasticity)
hunt_se <- data %>% filter(elasticity < -2 & grepl("Hunt", authors)) %>% pull(se)

bennmarker_excluded_elasticity <- data %>% filter(elasticity < 0 & grepl("Bennmarker", authors)) %>% pull(elasticity)
bennmarker_excluded_se <- data %>% filter(elasticity < 0 & grepl("Bennmarker", authors)) %>% pull(se)

ratio_pos <- nrow(data %>% filter(elasticity > 0)) / nrow(data)
ratio_pos_figure <- nrow(data %>% filter(elasticity > 0 & elasticity < 1.5))  / nrow(data %>% filter(abs(elasticity) < 1.5))

conf_int_high_beta <- rr_beta + 1.96 * get_data(summary_table_descriptives_RR_raw, "t_beta",
                                                         value_column = "se")

pbd_tau <- get_data(summary_table_descriptives_PBD_raw,
                    "t_sigma")
rr_tau <- get_data(summary_table_descriptives_RR_raw,
                   "t_sigma")
pbd_90conf_low <- pbd_theta - 1.645 * pbd_tau
pbd_90conf_high <- pbd_theta + 1.645 * pbd_tau
rr_90conf_low <- rr_theta - 1.645 * rr_tau
rr_90conf_high <- rr_theta + 1.645 * rr_tau

pbd_high_theta <- as.numeric(gmm_PBD_one_cut_raw %>% slice(1) %>% pull(Theta))
pbd_low_theta <- get_data(summary_table_descriptives_PBD_raw,
                             "norm_theta")
rr_high_theta <- get_data(summary_table_descriptives_RR_raw,
                          "symmetric_theta")
rr_low_theta <- as.numeric(gmm_RR_one_cut_raw %>% slice(1) %>% pull(Theta))

bma_posterior_dist_variance <- get_data(bma_elasticities_raw, "post dist for variance")
bma_num_signif_covar <- nrow(bma_data %>% filter (`Posterior.Inclusion.Probability` > 0.3)) - 1 #subtract Intercept

US_baseline_rr <- 0.435
#US rr from https://oui.doleta.gov/unemploy/repl_ratio/repl_ratio_rpt.asp
#calculate 2022 Jan - Dec, and use RR1 which excludes outliers >2
US_baseline_pbd <- 26

gruber_optimal <- 0.023 #from table 4 in Gruber (1997)
schmieder_average <- 0.594 #this spreadsheet: https://docs.google.com/spreadsheets/d/1rTt5-VRtu3-eQX7PyVWSThmWVuj6cP2en99shZqg-j4/edit?gid=0#gid=0
schmieder_us <- 0.38 # median among US studies from Schmieder and von Wachter (2016)

bma_intercept <- get_data(bma_coef_weighted_raw,
                          "(Intercept)",
                          "X", "Posterior.Mean")
all_affected_coef <- get_data(bma_coef_weighted_raw,
                             "Macro treatment",
                             "X", "Posterior.Mean")
macro_inclusion_prob <- get_data(bma_coef_weighted_raw,
                              "Macro treatment",
                              "X", "Posterior.Inclusion.Probability")
us_coef <- get_data(bma_coef_weighted_raw,
                   "United States dummy",
                   "X", "Posterior.Mean")
tax_wedge_coef <- get_data(bma_coef_weighted_raw,
                                "Labor tax wedge (pp)",
                                "X", "Posterior.Mean")
admin_data_coef <- get_data(bma_coef_weighted_raw,
                           "Administrative data",
                           "X", "Posterior.Mean")
rdd_coef <- get_data(bma_coef_weighted_raw,
                    "Regression Discontinuity Design",
                    "X", "Posterior.Mean")
pbdxrr_coef <- get_data(bma_coef_weighted_raw,
                             "Baseline PBD (weeks) x RR estimate",
                             "X", "Posterior.Mean")
pbdxpbd_coef <- get_data(bma_coef_weighted_raw,
                        "Baseline PBD (weeks) x PBD estimate",
                        "X", "Posterior.Mean")
se_coef <- get_data(bma_coef_weighted_raw,
                    "Standard Error",
                    "X", "Posterior.Mean")

average_se <- mean(bma_input$SE)
correction_bma <- 1 - rr_bma_average / (rr_bma_average + se_coef * average_se)

all_affected_sd <- get_data(bma_coef_weighted_raw,
                            "Macro treatment",
                            "X", "Posterior.SD")
lower_interval_macro <- pbd_bma_average - 1.96 * all_affected_sd
upper_interval_macro <- pbd_bma_average + 1.96 * all_affected_sd

#defer numbers for Table B.2. until redone
hazard_loglog_count <- get_data(data_characteristics_raw, "hazard log-log count")
hazard_av_range <- get_data(data_characteristics_raw, "hazard average range")
hazard_av_mean <- get_data(data_characteristics_raw, "hazard average mean")
roed_hazard_unadj <- get_data(data_characteristics_raw, "hazard Roed unadjusted")
roed_hazard_adj <- get_data(data_characteristics_raw, "hazard Roed adjusted")

florida_rr_elasticity <- get_data(bma_elasticities_raw, "Florida RR")
florida_pbd_elasticity <- get_data(bma_elasticities_raw, "Florida RBD")
france_rr_elasticity <- get_data(bma_elasticities_raw, "France RR")
france_pbd_elasticity <- get_data(bma_elasticities_raw, "France PBD")
florida_rr_val <- get_data(bma_elasticities_raw, "Florida RR Value")
florida_pbd_val <- get_data(bma_elasticities_raw, "Florida PBD Value")
france_rr_val <- get_data(bma_elasticities_raw, "France RR Value")
france_pbd_val <- get_data(bma_elasticities_raw, "France PBD Value")

europe_rr <- oecd_rr %>%
  filter(TIME == 2022, Country %in% europe_countries) %>%
  summarise(max(Value) / 100) %>%
  pull()

us_wedge <- oecd_wedge %>%
  rename("Country" = Reference.area, "wedge" = OBS_VALUE) %>%
  filter(TIME_PERIOD == 2022, Country == "United States") %>%
  pull("wedge")

employment_rate <- 0.95
tercile_excluded <- sum(abs(data$elasticity) > 1.5)

roed_westlie <- data %>% 
  filter(grepl("Roed", authors) & grepl("Westlie", authors)) %>% 
  pull(implied_elasticity_nonemp)
roed_zhang <- data %>% 
  filter(grepl("Roed", authors) & grepl("Zhang", authors) & 
           margin_outcome_identifier == "Men") %>% 
  pull(implied_elasticity_nonemp)

lhs_neg_intercept <- 0.222
lhs_neg_slope <- -0.265
gamma <- 2
schmieder__us_rr <- ((0.38 / 0.95 - lhs_neg_intercept * gamma ) /
        (lhs_neg_slope * gamma))

appendix_num_studies <- data %>%
  select(paper_id, macro_treatment, journal, sample_country,
         coef_type_nonemp, reg_outcome_nonemp, research_design, pbd_vs_rr) %>%
  distinct() |>
  summarise(count = n()) |>
  pull()



#footnotes denoted by y.x,
#where y is page number and x is the footnote number
#tablenotes denoted by .99
#appendix denoted by .88
df <-
  tribble(
    ~statistic, ~value, ~page,
    "x times more likely", ratio_significant_rr, 1, #1
    "rep rate optimal", bc_bma_const, 1, #2
    "number of studies", num_studies, 2,#3
    "top tercile vs bot tercile", ratio_top_bottom_tercile, 2, #4
    "x times more likely low", ratio_significant_rr, 2, #5
    "x time more likely high", ratio_significant_pbd, 2, #6
    "correction for ak", correction_ak, 2, #7
    "correction for bma", correction_bma, 2, #8
    "US RR elasticity", rr_bma_average, 2, #9
    "US PBD elasticity", pbd_bma_average, 2, #10
    "Florida RR elasticity", florida_rr_elasticity, 2, #11
    "Florida PBD elasticity", florida_pbd_elasticity, 3, #12
    "France RR elasticity", france_rr_elasticity, 3, #13
    "France PBD elasticity", france_pbd_elasticity, 3, #14
    "gruber rep rate", gruber_optimal, 3.1, #15
    "rep rate optimal", bc_bma_const, 3, #16
    "num macro studies", macro_studies, 3, #17
    "num journal publications", num_journal_publications, 4, #18
    "papers with info", num_studies, 4, #19
    "percent quasi", percent_quasi, 4, #20
    "25th percentile impact", quartile_impact_factor, 4, #21
    "median impact factor", median_impact_factor, 4, #22 #for these two, need to use impact factor sheet in main spreadsheet to get corresponding journal
    "field journal ratio", field_ratio, 5, #23
    "top five journal ratio", ratio_top5, 5, #24
    "econometric method journal ratio", ratio_method, 5, #25
    "number of estimates", num_estimates, 5, #26
    "mean pbd elasticity", pbd_mean, 5, #27
    "mean rr elasticity", rr_mean, 5, #28
    "baseline rr min", min_base_rr, 5, #29
    "baseline rr max", max_base_rr, 5, #30
    "baseline rr iqr", iqr_base_rr, 5, #31
    "percent quasi-methods PBD", pbd_quasi_ratio, 5, #32
    "percent quasi-methods RR", rr_quasi_ratio, 5, #33
    "bottom tercile mean", bottom_tercile_av, 6, #34
    "mid tercile mean", mid_tercile_av, 6, #35
    "top tercile mean", top_tercile_av, 6, #36
    "percent positive", ratio_pos, 8.8, #38
    "percent positive", ratio_pos, 8.8, #40
    "percent positive in figure", ratio_pos_figure, 6.8, #41
    "estimates excluded", tercile_excluded, 7, #42
    "beta in table 1 low", pbd_beta, 9, #43
    "beta in table 1 high", rr_beta, 9, #44
    "upper end conf int", conf_int_high_beta, 9, #47
    "latent mean PBD", pbd_theta, 9, #48
    "mean PBD", pbd_mean, 9, #49
    "latent mean RR", rr_theta, 9, #50
    "mean RR", rr_mean, 9, #51
    "PBD tau", pbd_tau, 9, #52
    "RR tau", rr_tau, 9, #53
    "90% conf int low PBD", pbd_90conf_low, 9, #54
    "90% conf int high PBD", pbd_90conf_high, 9, #55
    "90% conf int low RR", rr_90conf_low, 9, #56
    "90% conf int high RR", rr_90conf_high, 9, #57
    "number of RR estimates", num_rr_estimates, 10.99, #45
    "number of PBD estimates", num_pbd_estimates, 10.99, #46
    "RR lowest theta", rr_low_theta, 11, #60
    "RR highest theta", rr_high_theta, 11, #61
    "PBD lowest theta", pbd_low_theta, 11, #58
    "PBD highest theta", pbd_high_theta, 11, #59
    "Hunt elasticity", hunt_elasticity, 11.10, #62
    "Hunt se", hunt_se, 11.10, #63
    "Hunt elasticity", hunt_elasticity, 13, #64
    "Hunt se", hunt_se, 13, #65
    "model posterior dist for variance", bma_posterior_dist_variance, 15, #66
    "number of significant covariates", bma_num_signif_covar, 15, #67
    "posterior mean rrxrr", rrxrr_interaction_coef, 15, #68
    "US lower end RR", florida_rr_val, 15, #69
    "Europe upper end RR", europe_rr, 15, #70
    "Effect 50wk baseline on RR elasticity", 50 * pbdxrr_coef, 15, #71
    "Effect 50wk baseline on PBD elasticity", 50 * pbdxpbd_coef, 15, #72
    "Roed and Westlie PBD elasticity", roed_westlie, 15.14, #75
    "Roed and Zhang RR elasticity", roed_zhang, 15.14,
    "posterior mean drop Norway", NA, 15.14, #76
    "effect 50wk baseline on RR elasticity", NA, 15.44, #77
    "average replacement rate", US_baseline_rr, 16, #78
    "average weeks PBD", US_baseline_pbd, 16, #79
    "RR elasticity prediction", rr_bma_average, 16, #80
    "PBD elasticity prediction", pbd_bma_average, 16, #81
    "Florida RR", florida_rr_val, 16, #82
    "Florida PBD high", florida_pbd_val, 16, #83
    "Florida RR elasticity", florida_rr_elasticity, 16, #84
    "Florida PBD elasticity", florida_pbd_elasticity, 16, #85
    "France RR", france_rr_val, 16, #86
    "France RR elasticity", france_rr_elasticity, 16, #87
    "France PBD", france_pbd_val, 16, #88
    "France PBD elasticity", france_pbd_elasticity, 16, #89
    "intercept", bma_intercept, 16.15, #90
    "Aggregate Variation", all_affected_coef, 16.15, #91
    "US Dummy", us_coef, 16.15, #92
    "Tax Wedge", tax_wedge_coef, 16.15, #93
    "US tax wedge", us_wedge, 16.15, #94
    "admin data", admin_data_coef, 16.15, #95
    "rdd", rdd_coef, 16.15, #96
    "Baseline PBD", pbdxrr_coef, 16.15, #97
    "average weeks PBD", US_baseline_pbd, 16.15, #98
    "Baseline RR", rrxrr_interaction_coef, 16.15, #99
    "average replacement rate", US_baseline_rr, 16.15, #100
    "final number", rr_bma_average, 16.15, #101
    "US context PBD", US_baseline_pbd, 18, #102
    "employment rate", employment_rate, 18, #103
    "effect of 10 point increase RR", rrxrr_interaction_coef * 0.1, 18, #104
    "optimal rep rate", bc_bma_const, 18, #105
    "optimal rep rate at sample avg", bc_avg_rr, 20, #106
    "ak optimal rep rate", bc_ak, 20, #107
    "schmieder average", schmieder_average, 20.18, #108
    "schmieder US median", schmieder_us, 20.18,
    "optimal rep rate schmieder", schmieder__us_rr, 20.18, 
    "number studies", num_studies, 21, #109
    "num macro studies", macro_studies, 21, #110
    "change for macro", all_affected_coef, 22, #111
    "posterior inclusion prob", macro_inclusion_prob, 22, #112
    "lower end conf interval", lower_interval_macro, 22, #113
    "higher end conf interval", upper_interval_macro, 22, #114
    "total observations", num_margin, 5.88, #115
    "typical hazard range", hazard_av_range, 2.88, #117
    "typical hazard average", hazard_av_mean, 2.88, #118
    "Observation count", num_estimates, 5.88, #119
    "per paper-margin number", num_margin, 5.88, #120
    "hunt elasticity", hunt_elasticity, 6.88, #121
    "hunt se", hunt_se, 6.88, #122
    "total observations", appendix_num_studies, 12.88,
    "num papers", num_studies, 12.88,
    "num papers two estimates", appendix_num_studies - num_studies, 12.88,
    "total observations", appendix_num_studies, 13.88,
    "num papers", num_studies, 13.88,
    "num papers with two estimates", appendix_num_studies - num_studies, 13.88
  ) %>%
  mutate(value = round(value, 6))

#9 of the values are NA and unnesting removes these values
df <- df |> unnest(value)

write.csv(df, file = paste0(dir, "/stats/stats.csv"), fileEncoding = "UTF-8")
