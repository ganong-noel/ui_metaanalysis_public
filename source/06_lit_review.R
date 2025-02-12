source("~/repo/ui_pubbias/source/funcs/setup.R")

####micro_to_macro figure
mean <- get_data(bma_elasticities_raw, "PBD")

all_affected_coef <- get_data(bma_coef_weighted_raw, "Macro treatment", "X",
                              "Posterior.Mean")

all_affected_sd <- get_data(bma_coef_weighted_raw, "Macro treatment", "X",
                            "Posterior.SD")

df_mm_estimate <-
  tribble(
    ~source, ~micro, ~macro,
    "Johnston and Mas (2018)", 0.45, 0.45, #this is the UI elasticity
    "Karahan, Mitman and Moore (2022)", (1-0.55)*0.42, 0.42, #0.42 from page 13, 0.55 is in main table but is only called "preferred" in the appendix
    "Landais Michaillat Saez (2018, b)", 0.6, 0.1,
    "Landais Michaillat Saez (2018, a)", 0.4, 0.3,
    "Lalive et al (2015)", 30/(52*3), (30-6.91)/(52*3),
    "Frederiksson and Soderstrom (2020)", 1.5, 3.24,
    "Jessen et al (2023)", 0.28, (1 + 0.11)*0.28, #0.28 from paragraph 1 of page 24 and -0.11 from page 26 para 1
    "**Meta analysis of 57 studies**", mean - all_affected_coef, mean
  ) %>%
  dplyr::mutate(diff = macro - micro) %>%
  dplyr::arrange(diff) %>%
  dplyr::mutate(source = factor(source, levels = unique(source)))

df_mm_w_se <-
  tribble(
    ~source, ~micro, ~macro, ~se,
    "Frederiksson and Soderstrom (2020)", 1.5, 3.24, sqrt(0.96^2 + 0.08^2),
    "**Meta analysis of 57 studies**", mean - all_affected_coef, mean, all_affected_sd,
    "Lalive et al (2015)", 30/(52*3), (30-6.91)/(52*3), 2.1/(52*3),
    "Jessen et al (2023)", 0.28, (1 + 0.11)*0.28, 0.024
  ) %>%
  dplyr::mutate(
    macro_low = macro - 1.96 * se,
    macro_high = macro + 1.96 * se
  )



mtom_plot <- df_mm_estimate |>
  pivot_longer(cols = c("micro", "macro")) |>
  ggplot(aes(y = source)) +
  fte_theme() +
  geom_point(aes(x = value, color = name,  shape = name)) +
  scale_x_log10() +
  scale_shape_manual(values = c(5, 16)) +
  labs(x = "Elasticity (log scale)", color = "", y = "", shape = "") +
  geom_errorbarh(
    data = df_mm_w_se,
    mapping = aes(xmin = macro_low, xmax = macro_high, color = "macro"),
    height = 0.2
  ) +
  theme(axis.text.y = element_markdown(),
        panel.grid.major = element_line(color = "grey65", size = 0.15),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10))

ggsave(paste0(output, "/lit_review.png"), plot = mtom_plot,
       height = 4.5, width = 8, dpi = 300, device = ragg::agg_png)

