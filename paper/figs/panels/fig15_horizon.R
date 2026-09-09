# Figure 15. Horizon sweep under the stock controller: paired gain and
# baseline completion at 0.15 N against episode duration.

gain <- ggplot(AUDIT_A2_HORIZON, aes(horizon, gain)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.28, linewidth = 0.3,
                colour = "#2166AC") +
  geom_line(linewidth = 0.5, colour = "#2166AC") +
  geom_point(size = 1.9, shape = 21, fill = "white", stroke = 0.55,
             colour = "#2166AC") +
  scale_x_continuous(name = "Episode horizon (s)",
                     breaks = AUDIT_A2_HORIZON$horizon) +
  scale_y_continuous(name = "Paired completion gain at 0.15 N",
                     limits = c(-0.22, 0.78), breaks = seq(-0.2, 0.7, 0.2)) +
  rtx_theme()
gain <- panel_label(
  gain, "A", "Paired gain",
  "Stock controller, 95% t interval"
)

base <- ggplot(AUDIT_A2_HORIZON, aes(horizon, baseline)) +
  geom_line(linewidth = 0.5, colour = "#B2182B") +
  geom_point(size = 1.9, shape = 21, fill = "white", stroke = 0.55,
             colour = "#B2182B") +
  scale_x_continuous(name = "Episode horizon (s)",
                     breaks = AUDIT_A2_HORIZON$horizon) +
  scale_y_continuous(name = "Baseline completion at 0.15 N",
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  rtx_theme()
base <- panel_label(
  base, "B", "Baseline completion",
  "Same stock-controller sweep"
)

save_fig(gain / base + plot_layout(heights = c(1, 1)),
         "fig15_horizon", FIGURE_CANVAS_WIDTH_IN, 5.90)
