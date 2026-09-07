# Figure 10. A factorised scene-geometry extension.
#
# Panel A reports the high-minus-low contrast for each declared geometry factor.
# Each seed first averages the four paired settings of the other two factors;
# intervals and exact tests therefore use 30 seed-level contrasts. The 120
# seed-by-setting terms remain a sensitivity readout, and the 1,440 episode
# rows are never treated as independent replicates.
#
# Panel B opens the same receipt into a condition-by-force map.  The nominal /
# near / aligned cell is the historical scene generator; the remaining cells
# are deterministic transforms of the same seed-level draws.

factor_plot_data <- GEOMETRY_FACTORS

factor_plot <- ggplot(
  factor_plot_data,
  aes(force_N, effect, colour = factor_label, shape = factor_label,
      group = factor_label)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = effect_lo, ymax = effect_hi),
                position = position_dodge(width = 0.010), width = 0.010,
                linewidth = 0.35) +
  geom_line(position = position_dodge(width = 0.010), linewidth = 0.45) +
  geom_point(position = position_dodge(width = 0.010), size = 1.75,
             fill = "white", stroke = 0.45) +
  scale_colour_manual(values = c(
    "Target spread" = "#2166AC",
    "Target range" = "#1B7837",
    "Crossing orientation" = "#B2182B"
  ), name = NULL) +
  scale_shape_manual(values = c(21, 22, 24), name = NULL) +
  scale_x_continuous(
    name = "True horizontal force (N)",
    breaks = sort(unique(factor_plot_data$force_N))
  ) +
  scale_y_continuous(
    name = "High minus low paired gain",
    limits = c(-0.22, 0.24),
    breaks = seq(-0.2, 0.2, by = 0.1)
  ) +
  rtx_theme() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.spacing.x = unit(0.20, "cm"),
    legend.key.width = unit(0.30, "cm"),
    legend.text = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_LEGEND_TEXT_SIZE),
    axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE)
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE),
         shape = guide_legend(nrow = 1, byrow = TRUE))
factor_plot <- panel_label(
  factor_plot, "A", "Geometry contrasts",
  "Paired factor effects; 30 seed clusters"
)

heat_data <- GEOMETRY_CONDITION_ROWS
condition_levels <- unique(heat_data$condition_short)
heat_data$condition_short <- factor(
  heat_data$condition_short, levels = rev(condition_levels)
)
heat_data$force_label <- factor(
  heat_data$force_label,
  levels = geometry_force_label(sort(unique(heat_data$force_N)))
)

heat_plot <- ggplot(
  heat_data,
  aes(force_label, condition_short, fill = delta)
) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.2f", delta)),
            size = FIGURE_CELL_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey15") +
  scale_fill_gradient2(
    name = "Paired gain",
    low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
    midpoint = 0, limits = c(-0.1, 0.75),
    breaks = c(0, 0.3, 0.6)
  ) +
  labs(
    x = "True horizontal force (N)",
    y = NULL
  ) +
  rtx_theme() +
  theme(
    axis.text.y = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE),
    axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE),
    legend.position = "right",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.width = unit(0.22, "cm"),
    legend.key.height = unit(0.55, "cm"),
    legend.title = element_text(family = FIGURE_FONT_FAMILY,
                                size = FIGURE_LEGEND_TITLE_SIZE),
    legend.text = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_LEGEND_TEXT_SIZE)
  )
heat_plot <- panel_label(
  heat_plot, "B", "Condition map",
  "Eight declared cells"
)

save_fig(
  (factor_plot | heat_plot) + plot_layout(widths = c(1.28, 1), guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal"),
  "fig10_geometry_factorial", FIGURE_CANVAS_WIDTH_IN, 3.90
)
