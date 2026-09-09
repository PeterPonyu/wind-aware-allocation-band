# Figure 14 (integral-gain audit). Paired completion gain at 0.15 N against
# the baseline integral gain, with clamp settings as distinct markers.

p <- ggplot(AUDIT_A1_GAIN, aes(ki, gain, colour = clamp_label, shape = clamp_label)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.028), width = 0.018,
                linewidth = 0.3) +
  geom_point(position = position_dodge(width = 0.028), size = 2.0,
             fill = "white", stroke = 0.5) +
  scale_colour_manual(values = c("clamp = 2 m/s" = "#2166AC",
                                 "clamp = 4 m/s" = "#1B7837",
                                 "clamp = 8 m/s" = "#B2182B"),
                      name = NULL) +
  scale_shape_manual(values = c("clamp = 2 m/s" = 21,
                                "clamp = 4 m/s" = 22,
                                "clamp = 8 m/s" = 24),
                     name = NULL) +
  scale_x_continuous(name = "Baseline integral gain Ki (N/m/s)",
                     breaks = sort(unique(AUDIT_A1_GAIN$ki))) +
  scale_y_continuous(name = "Paired completion gain at 0.15 N",
                     limits = c(-0.20, 0.78), breaks = seq(-0.2, 0.7, 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0))

save_fig(p, "fig14_integral_gain", FIGURE_CANVAS_WIDTH_IN, 3.55)
