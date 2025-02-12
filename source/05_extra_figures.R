source("~/repo/ui_pubbias/source/funcs/setup.R")
setwd("~/repo/ui_pubbias")

####quasi experimental plot
data <- read.csv(paste0(dir,
                        "/input/clean/clean_review_estimates_long.csv"), encoding = "UTF-8")

#quasi experimental by year plot
quasi_data <- data %>%
  dplyr::mutate(ventile = ntile(year, 20)) %>%
  dplyr::group_by(ventile) %>%
  dplyr::summarise(
    prop_quasi = mean(research_design %in% c("RDD", "DID", "RKD")),
    year = max(year)
  )

quasi_plot <- ggplot(quasi_data, aes(x = year, y = prop_quasi)) +
  fte_theme() +
  geom_point() +
  scale_x_continuous(limits = c(1979, 2024), breaks = seq(1980, 2020, by = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Publication year", y = "")

ggsave(paste0(output, "/quasi_experimental_by_year.png"),
       plot = quasi_plot,
       height = 4.5, width = 8, dpi = 300, 
       device = ragg::agg_png)
#FY end of Figure A.1

####main descriptive plots (Figure 1)
# tercile funnel plot
df_se <- data %>%
  dplyr::mutate(se_tercile = ntile(se, 3)) %>%
  dplyr::filter(abs(elasticity) < 1.5)

tercile_colors <- c("turquoise", "blue", "purple")
tercile_funnel_plot <- ggplot(df_se,
                              aes(x = elasticity, y = se,
                                  color = as.factor(se_tercile))) +
  geom_point(aes(shape = pbd_vs_rr), size = 2) +
  fte_theme() +
  geom_segment(x = 0, y = 0, xend = 2, yend = 1 / 1.96 * 2, color = "#999999") +
  geom_segment(x = 0, y = 0, xend = -2, yend = 1 / 1.96 * 2, color = "#999999") +
  annotate("text", x = 1.1, y = 0.7, label = "t-stat = 1.96",
           size = 4, color = "#999999") +
  annotate("text", x = -1.1, y = 0.7, label = "t-stat = -1.96",
           size = 4, color = "#999999") +
  scale_x_continuous(limits = c(-1.75, 1.75), breaks = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = tercile_colors,
                     labels = c("Bottom 3rd", "Middle 3rd", "Top 3rd")) +
  labs(x = "Elasticity", subtitle = "Standard Error", y = "",
       color = "Standard Error Tercile", shape = "") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        panel.grid.major = element_line(color = "grey64", size = 0.15)) +
  scale_shape_manual(values = c("PBD" = 15, "RR" = 16),
                     labels = c("Potential Benefit \nDuration",
                                "Replacement \nRate")) +
  guides(color = guide_legend(title.position = "top", order = 1),
         shape = guide_legend(title.position = "top", order = 2))

ggsave(paste0(output, "/funnel_by_margin_se_group_all.png"),
       plot = tercile_funnel_plot,
       height = 4.5, width = 8, dpi = 300, 
       device = ragg::agg_png)
#FY end of figure 1(a)
#violin plot
violin_plot <- ggplot(df_se, aes(x = elasticity, y = as.factor(se_tercile),
                                 fill = as.factor(se_tercile))) +
  fte_theme() +
  geom_density_ridges(scale = 0.9, rel_min_height = 0.01) +
  labs(x = "Elasticity", subtitle = "Standard Error Tercile", y = "") +
  scale_fill_manual(values = tercile_colors) +
  scale_x_continuous(limits = c(-1.75, 1.75), breaks = seq(-1.5, 1.5, by = 0.5)) +
  theme(legend.position = "none") +
  scale_y_discrete(labels = c("Bottom", "Middle", "Top")) +
  theme(plot.title.position = "plot")

#hide message about picking bandwidth
suppressMessages(ggsave(paste0(output, "/density_ridge.png"),
                        plot = violin_plot, height = 4.5, 
                        width = 8, dpi = 300, device = ragg::agg_png))
#FY end of figure 1b
#summary plot
mean_total_tercile <- mean(df_se$elasticity)
tercile_summary <- df_se %>%
  dplyr::group_by(se_tercile) %>%
  dplyr::summarise(
    mean_elasticity = mean(elasticity),
    upper_elasticity = mean_elasticity + 1.96 * 1/length(se) * sqrt(sum(se^2)),
    lower_elasticity = mean_elasticity - 1.96 * 1/length(se) * sqrt(sum(se^2)),
    max_se = max(se),
    min_se = min(se),
    mean_se = mean(se),
    .groups = "keep"
  )

tercile_summary_raw <- tercile_summary %>%
  tibble::rownames_to_column(var = "X") %>%
  rename_with(~ gsub(" ", ".", .))

summary_plot <- ggplot(tercile_summary, aes(x = mean_elasticity, y = mean_se)) +
  fte_theme() +
  geom_point(aes(color = as.factor(se_tercile)), size = 3) +
  geom_errorbarh(aes(xmin = lower_elasticity, xmax = upper_elasticity,
                     color = as.factor(se_tercile)), linewidth = 0.5) +
  labs(x = "Mean elasticity", subtitle = "Mean Standard Error", y = "",
       color = "Standard error tercile") +
  geom_text(aes(label = sprintf("%.2f", mean_elasticity),
                color = as.factor(se_tercile)),
            vjust = -1, hjust = -0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(0.2, 0.8), breaks = seq(0.25, 0.75, by = 0.25)) +
  scale_y_continuous(limits = c(-0.02, 0.5)) +
  scale_color_manual(values = tercile_colors,
                     labels = c("Bottom 3rd", "Middle 3rd", "Top 3rd")) +
  theme(legend.position = "bottom") + labs(color = "Standard Error Tercile") +
  geom_vline(xintercept = mean_total_tercile, linetype = "dashed",
             color = "black") +
  annotate("text", x = mean_total_tercile - 0.2, y = 0.25, hjust = -0.45,
           label = "Null hypothesis under \n  no publication bias") +
  theme(plot.title.position = "plot",
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(title.position = "top"))

ggsave(paste0(output, "/average_elasticity_per_se.png"), plot = summary_plot,
       height = 4.5, width = 8, dpi = 300, device = ragg::agg_png)
#FY end of figure 1c


####baily chetty plot
#Fixed values
lhs_neg_intercept <- 0.222
lhs_neg_slope <- -0.265
gamma <- 2

#values from bayesian model averaging analysis
rhs_debiased_bma <- get_data(bma_elasticities_raw, "RR")

bma_coef <- bma_coef_weighted_raw %>%
  tibble::column_to_rownames(var = "X")

bma_slope <- get_data(bma_coef_weighted_raw,
                      "Baseline RR (fraction) x RR estimate",
                      "X", "Posterior.Mean")

simple_avg_bma <- read.csv(paste0(dir, "/input/clean/bma_input.csv"), encoding = "UTF-8") |>
  filter(PBD == 0) |>
  summarise(avg = mean(elasticity)) |>
  pull(avg)

# predict_calc defined in 02_model_averaging.R
elasticity_us_bma <- predict_calc(pbd = FALSE, naive = TRUE, us = TRUE)

#values from andrews kasy analysis

simple_avg_ak <- get_data(summary_table_descriptives_RR_raw, "mean_elasticity")
rhs_debiased_ak <- get_data(summary_table_descriptives_RR_raw, "t_theta")

#Divide by e = 0.95
e <- 0.95
vars <- mget(c("rhs_debiased_bma", "simple_avg_bma", "elasticity_us_bma",
               "bma_slope", "rhs_debiased_ak", "simple_avg_ak"))
vars <- lapply(vars, function(x) x / e)
list2env(vars, envir = .GlobalEnv)

# Create a data frame for plotting functions
replacement_rate <- seq(0, 1, by = 0.01)

#Andrews & Kasy 2019 (AK)
bc_ak <-
  tibble(
    replacement_rate = replacement_rate,
    lhs = gamma * (lhs_neg_intercept + lhs_neg_slope * replacement_rate),
    simpleavg_ak = simple_avg_ak,
    rhsdebiased_ak = rhs_debiased_ak,
  )

avg_intercept <- max((simple_avg_ak / gamma - lhs_neg_intercept) / lhs_neg_slope,0)
debiased_intercept <- (rhs_debiased_ak / gamma
                       - lhs_neg_intercept) / lhs_neg_slope

bc_ak_plot_data <-
  bc_ak |>
  pivot_longer(
    cols = c("lhs", "simpleavg_ak", starts_with("rhs")),
    names_to = c("estimate")
  ) |>
  dplyr::mutate(
    estimate = factor(
      estimate,
      levels = c("lhs", "simpleavg_ak", "rhsdebiased_ak"),
      labels =
        c("Welfare gain from consumption-smoothing (Gruber 1997)",
          "Elasticity (simple average)",
        "Elasticity (pub-bias corrected)")
      )
    )

# Add annotations here for each intersection point
ak_ann_text <- data.frame(
  label = c(sprintf("%.0f%%", avg_intercept * 100),
            sprintf("%.0f%%", debiased_intercept * 100)),
  x_val = c(avg_intercept,
             debiased_intercept),
  y_val = c(simple_avg_ak,
             rhs_debiased_ak))

bc_ak_plot <-
  ggplot(data = bc_ak_plot_data, aes(x = round(replacement_rate, 6))) +
  fte_theme() +
  geom_line(aes(y = round(value, 6), color = estimate, linetype = estimate)) +
  scale_linetype_manual(values = c( "longdash", "solid", "solid")) +
  scale_color_manual(values = c("black", tercile_colors[1], "#e41a1c")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Estimate", nrow = 4),
         linetype = guide_legend(title = "Estimate", nrow = 4)) +
  labs(
    x = paste0("Optimal replacement rate to equate welfare gain and ",
               "elasticity of duration"),
    subtitle = "Welfare gain or elasticity", y ="",
    colour = ""
  ) +
  #removes 17 rows on purpose because we don't want black line becoming negative
  scale_y_continuous(limits = c(0, simple_avg_ak * 1.2)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  theme(plot.title.position = "plot") +
  geom_text(
    data = ak_ann_text,
    mapping = aes(x = round(x_val, 6), y = round(y_val, 6), label = label),
    hjust = 0,
    vjust = -0.8
  ) +
  geom_point(
    data = ak_ann_text,
    mapping = aes(x = round(x_val, 6), y = round(y_val, 6))
  ) +
  theme(legend.text = element_text(size = 15))

#hides warning about removing rows
suppressWarnings(ggsave(paste0(output, "/bailychetty_ak.png"),
                        plot = bc_ak_plot, width = 8, height = 4.5, dpi = 300, 
                        device = ragg::agg_png))

#Bayesian model averaging (BMA) plot
bc_bma <-
  tibble(
    replacement_rate = replacement_rate,
    lhs = gamma * (lhs_neg_intercept + lhs_neg_slope * replacement_rate),
    rhsdebiased_bma = rhs_debiased_bma,
    simpleavg_bma = simple_avg_bma,
    elasticityus_bma = elasticity_us_bma,
    rhsdebiasedavg_bma = rhs_debiased_bma
  ) |>
  dplyr::mutate(across(c(rhsdebiased_bma),
                       ~ .x + bma_slope * (replacement_rate  - 0.435)))

bma_debiased_intercept <- ((rhs_debiased_bma
                            - lhs_neg_intercept * gamma - 0.435 * bma_slope) /
                             (lhs_neg_slope * gamma - bma_slope))
bma_debiased_avg_intercept <- ((rhs_debiased_bma - lhs_neg_intercept * gamma ) /
                                 (lhs_neg_slope * gamma))

#Add annotations here for each intersection point
bma_ann_text <- data.frame(
  label = c(sprintf("%.0f%%", bma_debiased_intercept * 100),
            sprintf("%.0f%%", bma_debiased_avg_intercept * 100),
            sprintf("%.0f%%", 0),
            sprintf("%.0f%%", 0)),
  x_val = c(bma_debiased_intercept,
            bma_debiased_avg_intercept,
            0,
            0),
  y_val = c(rhs_debiased_bma + (bma_debiased_intercept - 0.435) * bma_slope,
            rhs_debiased_bma,
            simple_avg_bma,
            elasticity_us_bma))

bc_bma_plot_data <-
  bc_bma |>
  pivot_longer(
    cols = c("lhs", "simpleavg_bma", "elasticityus_bma", starts_with("rhs")),
    names_to = c("estimate")
  ) |>
  dplyr::mutate(
    estimate = factor(
      estimate,
      levels = c("lhs", "simpleavg_bma", "elasticityus_bma", "rhsdebiasedavg_bma", "rhsdebiased_bma"),
      labels =
        c("Welfare gain from consumption-smoothing (Gruber 1997)",
          "Elasticity (simple average)",
          "Elasticity (US context)",
          "Elasticity (US context + pub-bias corrected)",
          "Elasticity (US context + pub-bias corrected + RR heterogeneity)")
      )
    )

#Animated plots
order <- c(levels( bc_bma_plot_data$estimate))
colors <- c('black', "#66c2a5", "#fc8d62","#8da0cb","#e78ac3")
lines <- c("solid", "twodash", "dashed", "longdash", "D3")
line_width <- 1.5

bc_bma_plot_base <-
  ggplot(data = bc_bma_plot_data, aes(x = replacement_rate)) +
  fte_theme() +
  scale_color_manual(values = colors, breaks = order) +
  scale_linetype_manual(values = lines, breaks = order) +
  theme(legend.position = "bottom",
        legend.key.height = unit(0.05, "in")) +
  guides(
    color = guide_legend(title = "Estimate", nrow = 5),
    linetype = guide_legend(title = "Estimate", nrow = 5)
  ) +
  labs(
    x = paste0("Optimal replacement rate to equate welfare gain and ",
               "elasticity of duration"),
    subtitle = "Welfare gain or elasticity", y ="",
    colour = ""
  ) +
  #removes 17 rows on purpose because we don't want black line becoming negative
  scale_y_continuous(limits = c(0, simple_avg_bma * 1.2)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  theme(plot.title.position = "plot") +
  theme(legend.text = element_text(size = 12))

bma_plot_1 <-
  bc_bma_plot_base +
  geom_line(
    data = . %>% filter(estimate == order[1]),
    aes(y = value, color = estimate, linetype = estimate),
    linewidth = line_width
    )

bma_plot_2 <-
  bma_plot_1 +
  geom_line(
    data = . %>% filter(estimate == order[2]),
    aes(y = value, color = estimate, linetype = estimate),
    linewidth = line_width
    ) +
  geom_text(
    data = (bma_ann_text)[3, ],
    mapping = aes(x = x_val, y = y_val, label = label),
    hjust = 0,
    vjust = -0.8
  ) +
  geom_point(
    data = (bma_ann_text)[3, ],
    mapping = aes(x = x_val, y = y_val)
    )

bma_plot_3 <-
  bma_plot_2 +
  geom_line(
    data = . %>% filter(estimate == order[3]),
    aes(y = value, color = estimate, linetype = estimate),
    linewidth = line_width
    ) +
  geom_text(
    data = (bma_ann_text)[4, ],
    mapping = aes(x = x_val, y = y_val, label = label),
    hjust = -0.1,
    vjust = 1.6
  ) +
  geom_point(
    data = (bma_ann_text)[4, ],
    mapping = aes(x = x_val, y = y_val)
  )

bma_plot_4 <-
  bma_plot_3 +
  geom_line(
    data = . %>% filter(estimate == order[4]),
    aes(y = value, color = estimate, linetype = estimate),
    linewidth = line_width
    ) +
  geom_text(
    data = (bma_ann_text)[2, ],
    mapping = aes(x = x_val, y = y_val, label = label),
    hjust = 0,
    vjust = -0.8
  ) +
  geom_point(
    data = (bma_ann_text)[2, ],
    mapping = aes(x = x_val, y = y_val)
  )

bma_plot_5 <-
  bma_plot_4 +
  geom_line(
    data = . %>% filter(estimate == order[5]),
    aes(y = value, color = estimate, linetype = estimate),
    linewidth = line_width
  )  +
  geom_text(
    data = (bma_ann_text)[1,],
    mapping = aes(x = x_val, y = y_val, label = label),
    hjust = 0.4,
    vjust = 1.6
  ) +
  geom_point(
    data = (bma_ann_text)[1, ],
    mapping = aes(x = x_val, y = y_val)
  )

#hides warning about removing rows
suppressWarnings(ggsave(paste0(output, "/bailychetty_bma.png"),
       plot = bma_plot_5, width = 8, height = 5.5, dpi = 300, 
       device = ragg::agg_png))

plots = list(bma_plot_1, bma_plot_2, bma_plot_3, bma_plot_4, bma_plot_5)
#hides warnings about removing rows
suppressWarnings(save_animation(plots, out_path = "./release/slides",
               out_name = "bma_plot", width = 8, height = 4.5))
#end of figure 2

####t statistic density plot
bwidth <- 1
kernel <- "rectangular"
density_colors = c("black", "grey48")

tval_data <- read.csv(paste0(dir,
                             "/input/clean/clean_review_estimates_long.csv"), encoding = "UTF-8") %>%
  dplyr::filter(tstat <= 10)

tval_densitymargin <- ggplot(tval_data, aes(x = tstat)) +
  geom_density(data = tval_data %>% dplyr::filter(pbd_vs_rr == "PBD"),
               aes(color = "Potential benefit duration"), bw = bwidth, kernel = kernel, fill = NA,
               key_glyph = draw_key_path) +
  geom_density(data = tval_data %>% dplyr::filter(pbd_vs_rr != "PBD"),
               aes(color = "Replacement rate"), bw = bwidth, kernel = kernel, fill = NA,
               key_glyph = draw_key_path) +
  scale_color_manual(values = c("Potential benefit duration" = density_colors[2],
                                "Replacement rate" = density_colors[1])) +
  geom_vline(xintercept = 1.96, linetype = "solid", color = "red",
             linewidth = 0.5) +
  labs(x = "t-statistic", y = "Density", color = "Research Design") +
  fte_theme() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        panel.grid.major = element_line(color = "grey64", size = 0.15))

ggsave(paste0(output, "/tvaldensity_margin.png"),
       plot = tval_densitymargin, width = 8, height = 4.5, dpi = 300, 
       device = ragg::agg_png)



####table C.2 (Distribution of Countries)
by_country <- read.csv(paste0(dir,
                             "/input/clean/clean_review_estimates_long.csv"), encoding = "UTF-8") %>%
  dplyr::group_by(paper_id, pbd_vs_rr) |>
  dplyr::filter(row_number() == 1) |>
  dplyr::ungroup() %>%
  dplyr::count(sample_country) |>
  arrange(desc(n))

####table B.2 (Distribution of Research Design)
by_margin <- read.csv(paste0(dir,
                             "/input/clean/clean_review_estimates_long.csv"), encoding = "UTF-8") %>%
  dplyr::group_by(paper_id, pbd_vs_rr) |>
  dplyr::filter(row_number() == 1) |>
  dplyr::ungroup() %>%
  dplyr::select(pbd_vs_rr, research_design) %>%
  dplyr::mutate(
    research_design = case_when(
      research_design %in% c("RKD", "DID", "RDD") ~ research_design,
      TRUE ~ "Other"
    )
  )

summary_table <- by_margin %>%
  dplyr::count(pbd_vs_rr, research_design) %>%
  spread(key = research_design, value = n, fill = 0) %>%
  rowwise() %>%
  dplyr::select(pbd_vs_rr, DID, RDD, RKD, Other) %>%
  dplyr::mutate(Total = sum(c_across(DID:Other))) %>%
  ungroup() %>%
  dplyr::mutate(across(DID:Total, ~ paste0(.x, " (", gsub("%", "\\\\%", percent(.x / Total, accuracy = 1)), ")")))

total_row <- summary_table %>%
  dplyr::summarize(across(DID:Other, ~sum(parse_number(.x)))) %>%
  dplyr::mutate(Total = sum(c_across(DID:Other))) %>%
  dplyr::mutate(across(DID:Total, ~ paste0(.x, " (", gsub("%", "\\\\%", percent(.x / Total, accuracy = 1)), ")"))) %>%
  dplyr::mutate(pbd_vs_rr = "Total")

final_table <- bind_rows(summary_table, total_row) %>%
  tibble::column_to_rownames(var = "pbd_vs_rr")

sink(paste0(output, "/research_design_table.tex"))

xtable_output <- print(xtable(final_table), sanitize.text.function = function(x) {x}, comment = FALSE, print.results = FALSE)

# Remove \begin{table}[ht], \centering, and \end{table} using gsub
xtable_output <- gsub("\\\\begin\\{table\\}\\[ht\\]|\\\\centering|\\\\end\\{table\\}", "", xtable_output)

cat(xtable_output)

sink()


####BMA elasticity to baseline RR plot
bma_elasticities <- read.csv(paste0(dir, "/input/clean/bma_input.csv"), encoding = "UTF-8") |>
  filter(PBD == 0) |>
  select(elasticity, RRxBaselineRR)

bma_baseline_plot <- ggplot(bma_elasticities) +
  geom_point(aes(y = elasticity, x = RRxBaselineRR)) +
  labs(
    x = "Baseline Replacement Rate",
    y = "",
    subtitle = "Replacement Rate Elasticity"
    ) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent)  +
  fte_theme()

ggsave(paste0(output, "/bma_elasticities_by_baseline.png"),
       plot = bma_baseline_plot, width = 8, height = 4.5, dpi = 300, 
       device = ragg::agg_png)

