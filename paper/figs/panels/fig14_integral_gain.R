# Figure 14 (integral-gain audit). Paired completion gain at 0.15 N against
# the baseline integral gain, with clamp settings as distinct markers.
#
# The tested Ki values double from one to the next, so the axis is logarithmic
# (base 2): on a linear axis the three lower settings crowd into the left
# quarter of the panel and the right half is empty.  The dodge that separates
# the clamp markers at the stock Ki is therefore expressed in log2 units.

CLAMP_LEVELS <- levels(AUDIT_A1_GAIN$clamp_label)
clamp_colours <- secondary_colours(CLAMP_LEVELS)
clamp_shapes <- secondary_shapes(CLAMP_LEVELS)

p <- ggplot(AUDIT_A1_GAIN, aes(ki, gain, colour = clamp_label, shape = clamp_label)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.45), width = 0.22,
                linewidth = 0.3) +
  geom_point(position = position_dodge(width = 0.45), size = 2.0,
             fill = "white", stroke = 0.5) +
  scale_colour_manual(values = clamp_colours, name = NULL) +
  scale_shape_manual(values = clamp_shapes, name = NULL) +
  scale_x_continuous(name = "Baseline integral gain Ki (N/m/s), log axis",
                     transform = "log2",
                     breaks = sort(unique(AUDIT_A1_GAIN$ki)),
                     labels = function(x) formatC(x, format = "f", digits = 2),
                     expand = expansion(mult = 0.12)) +
  scale_y_continuous(name = "Paired completion gain at 0.15 N",
                     limits = c(-0.20, 0.78), breaks = seq(-0.2, 0.6, 0.2)) +
  rtx_theme() +
  # The upper-right quarter of the panel carries no data at any Ki, so the key
  # sits there rather than taking a row of the single-column canvas.
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.97),
        legend.justification = c(1, 1),
        legend.margin = margin(1, 3, 1, 3),
        legend.key.size = unit(0.32, "cm"),
        legend.key.spacing.y = unit(0, "pt"))

save_fig(p, "fig14_integral_gain", FIGURE_COLUMN_WIDTH_IN, 2.75)
