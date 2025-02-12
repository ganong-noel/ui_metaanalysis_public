source("~/repo/ui_pubbias/source/funcs/setup.R")

####data cleaning
csv <- paste0(dir, "/input/clean/clean_review_estimates_long.csv")
long_estimates <- read.csv(csv, encoding = "UTF-8") %>%
  dplyr::select(elasticity, se, authors, research_design, pbd_vs_rr, reform_id)

wo_hunt <- long_estimates %>%
  dplyr::filter(!(elasticity < -2 & grepl("Hunt", authors)),
         pbd_vs_rr == "RR") %>%
  dplyr::mutate(pbd_vs_rr = "RRwoHunt")

long_estimates <- rbind(long_estimates, wo_hunt)

####parametric andrews kasy output
process_data <- function(data, data_type) {
  data_rdd <- data %>%
    dplyr::filter(research_design == "RDD")

  x <- as.matrix(data$elasticity)
  se <- as.matrix(data$se)
  cluster <- as.matrix(data$reform_id)

  normal_onecutoff <- metastudies_estimation(
    x,
    se,
    cutoffs = c(1.96),
    symmetric = FALSE,
    model = "normal",
    eval.max = 10^5,
    iter.max = 10^5,
    abs.tol = 10^(-8),
    stepsize = 10^(-6),
    cluster
  )
  norm_val <- normal_onecutoff$psi_hat
  norm_se <- normal_onecutoff$SE

  t_onecutoff <- metastudies_estimation(
    x,
    se,
    cutoffs = c(1.96),
    symmetric = FALSE,
    model = "t",
    eval.max = 10^5,
    iter.max = 10^5,
    abs.tol = 10^(-8),
    stepsize = 10^(-6),
    cluster
  )
  t_val <- t_onecutoff$psi_hat
  t_se <- t_onecutoff$SE

  symmetric_onecutoff <- metastudies_estimation(
    x,
    se,
    cutoffs = c(1.96),
    symmetric = TRUE,
    model = "t",
    eval.max = 10^5,
    iter.max = 10^5,
    abs.tol = 10^(-8),
    stepsize = 10^(-6)
  )
  symmetric_val <- symmetric_onecutoff$psi_hat
  symmetric_se <- symmetric_onecutoff$SE

  t_twocutoff <<- metastudies_estimation(
    x,
    se,
    cutoffs = c(0, 1.96),
    symmetric = FALSE,
    model = "t",
    eval.max = 10^5,
    iter.max = 10^5,
    abs.tol = 10^(-8),
    stepsize = 10^(-6),
    cluster
  )
  two_val <<- t_twocutoff$psi_hat
  two_se <- t_twocutoff$SE

  # clustered naive standard error calculation
  model <- lm(elasticity ~ 1, data = data)
  results <- coeftest(model, vcov = vcovCL, type = "HC1", cluster = ~ reform_id)

  summary_table <- tribble(
    ~estimate, ~value, ~se,
    "mean_elasticity", results[, 1], results[, 2],
    "mean_elasticity_precision_weighted", sum(x/se^2) / sum(1/se^2), NA ,
    "mean_elasticity_RDD", ifelse(data_type == "PBD", mean(data_rdd$elasticity),
                                  NA),
    ifelse(data_type == "PBD", sd(data_rdd$elasticity)
           / sqrt(length(data_rdd$elasticity)), NA),
    "n_total", length(data$elasticity), NA,
    "n_cluster", length(unique(cluster)), NA,
    "norm_theta", norm_val[1], norm_se[1],
    "norm_sigma", norm_val[2], norm_se[2],
    "norm_beta", norm_val[3], norm_se[3],
    "t_theta", t_val[1], t_se[1],
    "t_sigma", t_val[2], t_se[2],
    "t_df", t_val[3], t_se[3],
    "t_beta", t_val[4], t_se[4],
    "symmetric_theta", symmetric_val[1], symmetric_se[1],
    "symmetric_sigma", symmetric_val[2], symmetric_se[2],
    "symmetric_beta", symmetric_val[4], symmetric_se[4],
    "twocut_theta", two_val[1], two_se[1],
    "twocut_sigma", two_val[2], two_se[2],
    "twocut_df", two_val[3], two_se[3],
    "twocut_beta_neg", two_val[4], two_se[4],
    "twocut_beta", two_val[5], two_se[5]
  )
  
  if (data_type == "PBD") {
    summary_table_descriptives_PBD_raw <<- summary_table %>%
      tibble::rownames_to_column(var = "X") %>%
      rename_with(~ gsub(" ", ".", .))
  } else if (data_type == "RR") {
    summary_table_descriptives_RR_raw <<- summary_table %>%
      tibble::rownames_to_column(var = "X") %>%
      rename_with(~ gsub(" ", ".", .))
  } else {
    summary_table_descriptives_RRwoHunt_raw <<- summary_table %>%
      tibble::rownames_to_column(var = "X") %>%
      rename_with(~ gsub(" ", ".", .))
  }

  return(symmetric_onecutoff)
}

long_estimates %>%
  split(.$pbd_vs_rr) %>%
  imap(~process_data(.x, .y))


####generate table

out_table <- data.frame("margin" = character(0),
                        `Difference from baseline` = character(0),
                        `$\\beta_p$` = numeric(0),
                        `$\\bar{\\theta}$` = numeric(0),
                        `$\\tau$` = numeric(0),
                        `$\\bar{\\epsilon}$` = numeric(0),
                        stringsAsFactors = FALSE,
                        check.names = FALSE)

addrow <- function(margin, diff, beta, theta, tau, epsilon) {
  out_table <<- rbind(out_table, data.frame("margin" = margin,
                                            `Difference from baseline` = diff,
                                            `$\\beta_p$` = beta,
                                            `$\\bar{\\theta}$` = theta,
                                            `$\\tau$` = tau,
                                            `$\\bar{\\epsilon}$` = epsilon,
                                            check.names = FALSE))
}

for (margin in c("RR", "PBD")) {
  margin <- "RR"
  dist <- c("t", "norm", "symmetric", "twocut")

  if (margin == "RR") {
    mods <- c("woHunt")
  } else {
    mods <- c()
  }

  if (margin == "RR") {
    gmm <- c("_one_cut", "_symmetric", "woHunt_one_cut")
  } else {
    gmm <- c("_one_cut", "_symmetric")
  }

  for (d in dist) {
    beta <- get_data(summary_table_descriptives_RR_raw, paste0(d, "_beta"))
    theta <- get_data(summary_table_descriptives_RR_raw, paste0(d, "_theta"))
    tau <- get_data(summary_table_descriptives_RR_raw, paste0(d, "_sigma"))
    epsilon <- get_data(summary_table_descriptives_RR_raw, "mean_elasticity")

    diff <- case_when(
      d == "norm" ~ "Normal Distribution",
      d == "symmetric" ~ "Symmetric p(t)",
      d == "twocut" ~ "Extra p(t) cutoff",
      TRUE ~ ""
    )

    addrow(margin, diff, beta, theta, tau, epsilon)
  }
  for (m in mods) {
    beta <- get_data(summary_table_descriptives_RRwoHunt_raw, "t_beta")
    theta <- get_data(summary_table_descriptives_RRwoHunt_raw, "t_theta")
    tau <- get_data(summary_table_descriptives_RRwoHunt_raw, "t_sigma")
    epsilon <- get_data(summary_table_descriptives_RRwoHunt_raw, "mean_elasticity")

    diff <- case_when(
      m == "quasi" ~ "Quasi-experiments only",
      TRUE ~ "Drop Hunt (1995)"
    )

    addrow(margin, diff, beta, theta, tau, epsilon)
  }
  for (g in gmm) {
    variable_name <- paste0("gmm_", margin, g, "_raw")
    df <- get(variable_name)
    beta <- df %>%
      slice(1) %>%
      pull(beta_.p.)
    theta <- df %>%
      slice(1) %>%
      pull(Theta)
    tau <- df %>%
      slice(1) %>%
      pull(Sigma)

    epsilon <- case_when(
      margin == "PBD" ~ get_data(summary_table_descriptives_PBD_raw, "mean_elasticity"),
      g == "woHunt_one_cut" ~ get_data(summary_table_descriptives_RRwoHunt_raw, "mean_elasticity"),
      TRUE ~ get_data(summary_table_descriptives_RR_raw, "mean_elasticity")
    )

    diff <- case_when(
      g == "_symmetric" ~ "Non-Parametric GMM; Symmetric p(t)",
      g == "woHunt_one_cut" ~ " Non-Parametric GMM; Drop Hunt (1995)",
      TRUE ~ "Non-Parametric GMM"
    )

    addrow(margin, diff, beta, theta, tau, epsilon)
  }
}

latex_table <- out_table %>%
  dplyr::mutate(across(.cols = c(`$\\bar{\\theta}$`, `$\\tau$`, `$\\beta_p$`, `$\\bar{\\epsilon}$`),
                .fns = ~as.numeric(.))) %>%
  dplyr::mutate(`$\\bar{\\theta} - 1.64\\tau$` = `$\\bar{\\theta}$` - 1.64 * `$\\tau$`,
         `$\\bar{\\theta} + 1.64\\tau$` = `$\\bar{\\theta}$` + 1.64 * `$\\tau$`) %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 2)))%>%
  dplyr::mutate(`$\\bar{\\theta} \\pm 1.64\\tau$` =
                  paste0("[", format(`$\\bar{\\theta} - 1.64\\tau$`, nsmall = 2), ", " ,
                         format(`$\\bar{\\theta} + 1.64\\tau$`, nsmall = 2), "]")) %>%
  dplyr::select(margin, `Difference from baseline`, `$\\bar{\\epsilon}$`,
                `$\\beta_p$`, `$\\bar{\\theta}$`, `$\\tau$`,
                `$\\bar{\\theta} \\pm 1.64\\tau$`)

names(latex_table)[1] <- "Margin"

sink(paste0(output, "/ak_robustness_table.tex"))

print(xtable(latex_table),
      include.rownames = FALSE,
      sanitize.text.function = function(x) {x},
      comment = FALSE)

sink()

####descriptive table
rr_estimates <- summary_table_descriptives_RR_raw %>%
  dplyr::rename("rr_value" = value,
                "rr_se" = se) %>%
  mutate(rr_value = case_when(is.na(rr_value) ~ NA,
                              estimate == "n_total" ~ sprintf("%.0f", rr_value),
                              TRUE ~ sprintf("%.2f", rr_value)),
         rr_se = case_when(is.na(rr_se) ~ NA,
                           TRUE ~ paste0("(", sprintf("%.2f", rr_se), ")"))) %>%
  dplyr::select(-X)
pbd_estimates <- summary_table_descriptives_PBD_raw %>%
  dplyr::rename("pbd_value" = value,
                "pbd_se" = se) %>%
  mutate(pbd_value = case_when(is.na(pbd_value) ~ NA,
                               estimate == "n_total" ~ sprintf("%.0f",
                                                               pbd_value),
                               TRUE ~ sprintf("%.2f", pbd_value)),
         pbd_se = case_when(is.na(pbd_se) ~ NA,
                            TRUE ~ paste0("(", sprintf("%.2f",
                                                       pbd_se), ")"))) %>%
  dplyr::select(-X)

table_data <- rr_estimates %>%
  dplyr::left_join(pbd_estimates, join_by(estimate)) %>%
  pivot_longer(cols = starts_with("pbd_") | starts_with("rr_"),
               names_to = "key",
               values_to = "value") %>%
  pivot_wider(names_from = estimate,
              values_from = value) %>%
  dplyr::select(key, mean_elasticity, t_beta, t_theta,
                t_sigma, t_df) %>%
  dplyr::mutate(key = c("Potential Benefit Duration",
                        "  ",
                        "Replacement Rate",
                        " ")) %>%
  slice(c(3, 4, 1, 2)) %>%
  tibble::column_to_rownames(var = "key")


latex_column_names <- c(
  "$\\epsilon$",
  "$\\beta_p$",
  "$\\theta$",
  "$\\tau$",
  "$df$"
)
colnames(table_data) <- latex_column_names

sink(paste0(output, "/descriptives_table.tex"))

# Create the xtable output
xtable_output <- print(xtable(table_data), sanitize.text.function = function(x) {x},
                       comment = FALSE, print.results = FALSE, include.colnames = FALSE)

# Remove \begin{table}[ht], \centering, and \end{table} using gsub
xtable_output <- gsub("\\\\begin\\{table\\}\\[ht\\]|\\\\centering|\\\\end\\{table\\}|\\\\begin\\{tabular\\}\\{rlllll\\}|& \\$\\\\epsilon\\$ & \\$\\\\beta_p\\$ & \\$\\\\theta\\$ & \\$\\\\tau\\$ & \\$df\\$ \\\\\\\\|\\\\hline", "", xtable_output)
xtable_output <- gsub("&  \\\\\\\\", "& \\\\\\\\\n\\\\hline", xtable_output)
xtable_output <- gsub("\\\\end\\{tabular\\}", "\\\\hline\n\\\\end{tabular}", xtable_output)

manual_text <- "
\\newcolumntype{H}{>{\\renewcommand{\\arraystretch}{0.75}\\arraybackslash}c}

\\begin{tabular}{rHHHHH}
  \\hline
  & \\begin{tabular}[c]{@{}H@{}}Average Published\\\\ Elasticity\\end{tabular}
  & \\begin{tabular}[c]{@{}H@{}}Publication Prob\\\\ if $t < 1.96$\\end{tabular}
  & \\multicolumn{3}{c}{\\makecell{Latent Dist. Parameters\\\\ $\\Theta^* \\sim \\bar{\\theta} + t(\\nu) \\cdot \\tau$}} \\\\
  \\cmidrule(lr){4-6}
  & $\\hat{\\epsilon}$ & $\\beta_p$ & $\\bar{\\theta}$ & $\\tau$ & $\\nu$ \\\\
  \\hline
"
cat(paste(manual_text, xtable_output, sep=""))

sink()
#FY end of table 1

