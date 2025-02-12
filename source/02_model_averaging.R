source("~/repo/ui_pubbias/source/funcs/setup.R")
source(paste0(dir, "/source/funcs/bms.R"))


####data cleaning
df_long <-
  read.csv(paste0(dir, "/input/clean/clean_review_estimates_long.csv"), encoding = "UTF-8") %>%
  dplyr::filter(!(elasticity < -2 & grepl("Hunt", authors))) |>
  as_tibble()

test_that(
 "All study covariates are the same across collapsed designs",
 expect_equal(
   df_long |>
     dplyr::filter(estimates_per_study_margin > 1) |>
     distinct(paper_id, pbd_vs_rr, ue_measure, estimate_id) |>
     nrow(),
   df_long |>
     dplyr::filter(estimates_per_study_margin > 1) |>
     distinct(paper_id, pbd_vs_rr, ue_measure, estimate_id, PBD_indicator, macro_treatment,
              research_design, admin, hazard, year, us_country, country_tax_wedge,
              ue_deviation, impact_factor_z, mean_pbd, mean_rr) |>
     nrow()
 )
)

df_one_est_per_margin <-
  df_long |>
  dplyr::group_by(paper_id, pbd_vs_rr, ue_measure) |>
  dplyr::mutate(
    #inverse variance formula from https://en.wikipedia.org/wiki/Inverse-variance_weighting
    #sum over 1 / var_i
    e_denom = sum(1/se^2),
    #y_hat = sum over i y_i / var_i / sum over 1 / var_i
    elasticity = sum(elasticity/se^2)/e_denom,
    #var(y_hat) = sum over 1 / var_i
    se = sqrt(1/e_denom)
  ) |>
  dplyr::arrange(desc(sample_year)) |>
  dplyr::filter(row_number() == 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(X)

test_that(
  "when we collapse to one estimate per margin, 19 rows dropped",
  expect_equal(
    nrow(df_long) - 19,
    nrow(df_one_est_per_margin)
  )
)

# demean year
covariates <- df_one_est_per_margin %>%
  dplyr::mutate(
    years_until_2023 = 2023 - year,
    design_RKD = case_when(
      research_design == "RKD" ~ 1,
      TRUE ~ 0
    ),
    RR = case_when(
      pbd_vs_rr == "PBD" ~ 0,
      TRUE ~ 1
    ),
    DIDorRKD = design_DID + design_RKD,
    ue_measure_nonemp = as.numeric(ue_measure_nonemp)
  ) %>%
  dplyr::rename(
    "SE" = se,
    "PBD" = PBD_indicator,
    "MacroTreatment" = macro_treatment,
    "RDD" = design_RDD_RKD,
    "Admin" = admin,
    "NonemploymentAsOutcome" = ue_measure_nonemp,
    "HazardModel" = hazard,
    "YearsTo2023" = years_until_2023,
    "USA" = us_country,
    "TaxWedge" = country_tax_wedge,
    "RelativeUnemp" = ue_deviation,
    "ImpactFactorZ" = impact_factor_z,
    "BaselinePBD" = mean_pbd,
    "BaselineRR" = mean_rr
  ) %>%
  dplyr::mutate(
    PBDxRDD = PBD * RDD,
    PBDxDIDorRKD = PBD * DIDorRKD,
    DIDorRKDxSE = DIDorRKD * SE,
    RDDxSE = RDD * SE,
    PBDxBaselinePBD = PBD * BaselinePBD,
    RRxBaselinePBD = RR * BaselinePBD,
    PBDxBaselineRR = PBD * BaselineRR,
    RRxBaselineRR = RR * BaselineRR
  )

full_covariates <- c("SE", "DIDorRKD", "RDD",
                     "MacroTreatment", "PBD", "RRxBaselinePBD", "RRxBaselineRR",
                     "PBDxBaselinePBD", "PBDxBaselineRR", "Admin",
                     "NonemploymentAsOutcome", "HazardModel",
                     "YearsTo2023", "RelativeUnemp",
                     "USA", "TaxWedge", "ImpactFactorZ")

all_full <- covariates %>%
  dplyr::select("elasticity", all_of(full_covariates)) %>%
  drop_na()
write.csv(all_full,
          paste0(dir, "/input/clean/bma_input.csv"), fileEncoding = "UTF-8")


### bma analysis and latex output
bms_out <- bms(all_full, burn = 1e5, iter = 3e5, g = "UIP", mprior = "uniform",
           nmodel = 50000, mcmc = "bd", user.int = FALSE,
           randomizeTimer = FALSE)

for (use_condi in c("weighted")) {
  tex_path <- paste0(dir, "/release/BMA_", use_condi, "_coefficients.tex")

  sink(tex_path)


  desired_order <- c("(Intercept)", "Baseline RR (fraction) x RR estimate",
                     "Baseline RR (fraction) x PBD estimate",
                     "Baseline PBD (weeks) x RR estimate",
                     "Baseline PBD (weeks) x PBD estimate",
                     "PBD estimate (vs. RR estimate)",
                     "Macro treatment",
                     "Sample year (2023 = 0)",
                     "Relative unemployment (pp)",
                     "Labor tax wedge (pp)",
                     "United States dummy",
                     "Administrative data",
                     "Nonemployment as outcome",
                     "Hazard model",
                     "Difference-in-Difference or Regression Kink Design",
                     "Regression Discontinuity Design",
                     "Standard Error",
                     "SE x (DID or RKD)",
                     "SE x RDD",
                     "Impact Factor (z-score)")
  coef <- as.data.frame(estimates.bma(bms_out,exact=TRUE,
                                      condi.coef= (use_condi == "condi"),
                                      order.by.pip=FALSE,
                                      include.constant = TRUE)) %>%
    tibble::rownames_to_column(var = "Term") %>%
    dplyr::mutate(
      Term = c("Standard Error", "Difference-in-Difference or Regression Kink Design",
               "Regression Discontinuity Design", "Macro treatment",
               "PBD estimate (vs. RR estimate)",
               "Baseline PBD (weeks) x RR estimate", "Baseline RR (fraction) x RR estimate",
               "Baseline PBD (weeks) x PBD estimate",
               "Baseline RR (fraction) x PBD estimate", "Administrative data",
               "Nonemployment as outcome", "Hazard model",
               "Sample year (2023 = 0)", "Relative unemployment (pp)",
               "United States dummy", "Labor tax wedge (pp)",
               "Impact Factor (z-score)", "(Intercept)")
    ) %>%
    dplyr::filter(!Term == "remove") %>%
    dplyr::mutate(
      Term = factor(Term, levels = desired_order)
    ) %>%
    dplyr::arrange(Term) %>%
    dplyr::rename("Posterior Inclusion Probability" = PIP,
           "Posterior Mean" = `Post Mean`,
           "Posterior SD" = `Post SD`) %>%
    dplyr::select(Term, `Posterior Inclusion Probability`, `Posterior Mean`,
           `Posterior SD`) %>%
    tibble::column_to_rownames(var = "Term") %>%
    dplyr::mutate_if(is.numeric, round, digits = 3)

  bma_coef_weighted_raw <- coef %>%
    tibble::rownames_to_column(var = "X") %>%
    rename_with(~ gsub(" ", ".", .))

  coef <- coef %>%
    dplyr::select(-`Posterior SD`)
  kable_output <- kable(coef, format = "latex", booktabs = TRUE,
                        digits = 3, escape = FALSE) %>%
    kable_styling(full_width = FALSE) %>%
    pack_rows("Baseline benefits", 2, 5, bold = TRUE, indent = FALSE) %>%
    pack_rows("Policy variation", 6, 7, bold = TRUE, indent = FALSE) %>%
    pack_rows("Study context", 8, 11, bold = TRUE, indent = FALSE) %>%
    pack_rows("Data and estimation", 12, 16, bold = TRUE, indent = FALSE) %>%
    pack_rows("Standard errors", 17, 17, bold = TRUE, indent = FALSE) %>%
    pack_rows("Journal", 18, 18, bold = TRUE, indent = FALSE)

  # Remove \begin{table}, \centering, and \end{table} using gsub
  kable_output <- gsub("\\\\begin\\{table\\}|\\\\centering|\\\\end\\{table\\}", "", kable_output)

  print(kable_output)

  sink()
}



####model prediction for different contexts
av_se <- mean(df_one_est_per_margin$se, na.rm = TRUE)


#US rr from https://oui.doleta.gov/unemploy/repl_ratio/repl_ratio_rpt.asp
#calculate 2022 Jan - Dec, and use RR1 which excludes outliers >2

#US PBD assumed to be 26

us_wedge <- read.csv(paste0(dir, "/input/raw/oecd_wedge_data.csv"), encoding = "UTF-8") %>%
  dplyr::rename("Country" = Reference.area,
                "wedge" = OBS_VALUE) %>%
  dplyr::filter(TIME_PERIOD == 2022,
         Country == "United States") %>%
  pull("wedge")
us_vec <- c("base_rr" = 0.435, "base_pbd" = 26,
               "tax_wedge" = us_wedge)

europe_countries <- c("Austria", "Germany", "Norway", "Sweden", "Finland",
                      "France", "Netherlands", "Portugal", "Slovenia", "Spain",
                      "Switzerland", "Belgium", "Denmark", "Estonia", "Greece",
                      "Hungary", "Ireland", "Iceland", "Italy", "Latvia",
                      "Lithuania", "Luxembourg", "Poland")
europe_rr <- read.csv(paste0(dir, "/input/raw/oecd_rr_data.csv"), encoding = "UTF-8") %>%
  dplyr::filter(TIME == 2022,
         Country %in% europe_countries) %>%
  dplyr::select(Country, Value) %>%
  dplyr::rename("rr" = Value) %>%
  dplyr::mutate(rr = rr / 100)

europe_pbd <- read.csv(paste0(dir, "/input/raw/oecd_pbd_data.csv"), encoding = "UTF-8") %>%
  dplyr::select(pbd, Country)

europe_wedge <- read.csv(paste0(dir, "/input/raw/oecd_wedge_data.csv"), encoding = "UTF-8") %>%
  dplyr::rename("Country" = Reference.area,
         "wedge" = OBS_VALUE) %>%
  dplyr::filter(TIME_PERIOD == 2022,
         Country %in% europe_countries) %>%
  dplyr::select("Country", "wedge")

europe_pop <- read.csv(paste0(dir, "/input/raw/oecd_population_data.csv"), encoding = "UTF-8") %>%
  dplyr::filter(YEAR == 2021,
         SEX == "_T",
         AGE == "_T",
         Country %in% europe_countries) %>%
  dplyr::rename("population" = Value) %>%
  dplyr::select(Country, population)

europe_data <- europe_pop %>%
  dplyr::left_join(europe_rr, by = join_by(Country)) %>%
  dplyr::left_join(europe_pbd, by = join_by(Country)) %>%
  dplyr::left_join(europe_wedge, by = join_by(Country))

europe_vec <- c("base_rr" = reldist::wtd.quantile(europe_data$rr, q = 0.5,
                                         weight = europe_data$population,
                                         na.rm = TRUE),
                "base_pbd" = reldist::wtd.quantile(europe_data$pbd, q = 0.5,
                                          weight = europe_data$population,
                                          na.rm = TRUE),
                "tax_wedge" = reldist::wtd.quantile(europe_data$wedge, q = 0.5,
                                           weight = europe_data$population,
                                           na.rm = TRUE))


predict_calc <- function(pbd, naive, us) {
  #all parameters should be bool
  #pbd = T if you want pbd_vs_rr to indicate pbd
  #naive = T, if you want se included, i.e no adjustment
  #us = T if you want US values, not european
  if (us) {
    nation_vec <- us_vec
  } else {
    nation_vec <- europe_vec
  }
  context_vec <<- c(av_se * as.numeric(naive),
                   0,
                   1,
                   1,
                   1 * as.numeric(pbd),
                   nation_vec["base_pbd"] * as.numeric(!pbd),
                   nation_vec["base_rr"] * as.numeric(!pbd),
                   nation_vec["base_pbd"] * as.numeric(pbd),
                   nation_vec["base_rr"] * as.numeric (pbd),
                   1,
                   0,
                   0,
                   0,
                   0,
                   as.numeric(us),
                   nation_vec["tax_wedge"],
                   0)
  names(context_vec) <- c("SE",
                          "DID or RKD",
                          "RDD",
                          "Macro treatment",
                          "PBD estimate (vs. RR estimate)",
                          "Baseline PBD (weeks) x RR estimate",
                          "Baseline RR (fraction) x RR estimate",
                          "Baseline PBD x PBD estimate",
                          "Baseline RR x PBD estimate",
                          "Administrative data",
                          "Nonemployment as outcome",
                          "Hazard model",
                          "Sample year (2023 = 0)",
                          "Relative unemployment (pp)",
                          "United States dummy",
                          "Labor tax wedge (pp)",
                          "Impact Factor (z-score)")

  return(round(predict(bms_out, context_vec, se.fit = TRUE, exact = TRUE),
               2))
}


predict_calc_alt <- function(pbd, naive, us, rr_value, pbd_value) {
  #all parameters should be bool
  #pbd = T if you want pbd_vs_rr to indicate pbd
  #naive = T, if you want se included, i.e no adjustment
  #us = T if you want US values, not european
  if (us) {
    nation_vec <- us_vec
  } else {
    nation_vec <- europe_vec
  }
  context_vec <<- c(av_se * as.numeric(naive),
                    0,
                    1,
                    1,
                    1 * as.numeric(pbd),
                    pbd_value * as.numeric(!pbd),
                    rr_value * as.numeric(!pbd),
                    pbd_value * as.numeric(pbd),
                    rr_value * as.numeric (pbd),
                    1,
                    0,
                    0,
                    0,
                    0,
                    as.numeric(us),
                    nation_vec["tax_wedge"],
                    0)
  names(context_vec) <- c("SE",
                          "DID or RKD",
                          "RDD",
                          "Aggregate variation",
                          "PBD estimate (vs. RR estimate)",
                          "Baseline PBD (weeks) x RR estimate",
                          "Baseline RR (fraction) x RR estimate",
                          "Baseline PBD x PBD estimate",
                          "Baseline RR x PBD estimate",
                          "Administrative data",
                          "Nonemployment as outcome",
                          "Hazard model",
                          "Sample year (2023 = 0)",
                          "Relative unemployment (pp)",
                          "United States dummy",
                          "Labor tax wedge (pp)",
                          "Impact Factor (z-score)")

  return(round(predict(bms_out, context_vec, se.fit = TRUE, exact = TRUE),
               2))
}

#european policy data frame
europe_policy <-
  read.csv(paste0(dir, "/input/raw/oecd_rr_data.csv"), encoding = "UTF-8") %>%
  dplyr::filter(TIME == 2023) %>%
  dplyr::transmute(Country, rr_value = Value/100) |>
  #drops 11 countries with no PBD
  inner_join(read.csv(paste0(dir, "/input/raw/oecd_pbd_data.csv"), encoding = "UTF-8") %>%
               dplyr::transmute(pbd_value = pbd, Country),
             join_by("Country"))

results <-
  europe_policy |>
  mutate(
    predict_rr =
      pmap(
        list(rr_value, pbd_value),
        ~ predict_calc_alt(pbd = FALSE, naive = FALSE, us = FALSE, rr_value = .x, pbd_value = .y)
      ) |>
      unlist(),
    predict_pbd =
      pmap(
        list(rr_value, pbd_value),
        ~ predict_calc_alt(pbd = TRUE, naive = FALSE, us = FALSE, rr_value = .x, pbd_value = .y)
      ) |>
      unlist()
  )

#other cases included for reference
# Alaska
predict_calc_alt(pbd = FALSE, naive = FALSE, us = TRUE, rr_value = 0.33, pbd_value = 26)


bma_stats <- tribble(
  ~estimate, ~value,
  "RR w pubbias", predict_calc(pbd = FALSE, naive = TRUE, us = TRUE),
  "RR", predict_calc(pbd = FALSE, naive = FALSE, us = TRUE),
  "PBD w pubbias", predict_calc(pbd = TRUE, naive = TRUE, us = TRUE),
  "PBD", predict_calc(pbd = TRUE, naive = FALSE, us = TRUE),
  "EU RR w pubbias", predict_calc(pbd = FALSE, naive = TRUE, us = FALSE),
  "EU RR", predict_calc(pbd = FALSE, naive = FALSE, us = FALSE),
  "EU PBD w pubbias", predict_calc(pbd = TRUE, naive = TRUE, us = FALSE),
  "EU PBD", predict_calc(pbd = TRUE, naive = FALSE, us = FALSE),
  "post dist for variance", post.pr2(bms_out),
  "France RR Value", results[results$Country == "France", 'rr_value'],
  "France RR", results[results$Country == "France", 'predict_rr'],
  "France PBD Value", results[results$Country == "France", 'pbd_value'],
  "France PBD", results[results$Country == "France", 'predict_pbd'],
  "Florida RR Value", 0.33,
  "Florida RR", predict_calc_alt(pbd = FALSE, naive = FALSE, us = TRUE, rr_value = 0.33, pbd_value = 12),
  "Florida PBD Value", 12,
  "Florida RBD", predict_calc_alt(pbd = TRUE, naive = FALSE, us = TRUE, rr_value = 0.33, pbd_value = 12)
)

bma_elasticities_raw  <- bma_stats %>%
  tibble::rownames_to_column(var = "X") %>%
  rename_with(~ gsub(" ", ".", .))
