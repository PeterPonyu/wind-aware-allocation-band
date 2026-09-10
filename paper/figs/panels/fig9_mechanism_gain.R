# Figure 9. Gain sensitivity and the feedforward mechanism check.

gain_plot_data <- GAIN_TRANSITION
gain_plot_data$profile_label <- factor(
  unname(profile_label[as.character(gain_plot_data$profile)]),
  levels = unname(profile_label[TEMPORAL_PROFILES])
)

# One colour per true-force profile from the paper-wide secondary palette; the
# arm red/blue are reserved for the two allocators drawn in Panel B.
PROFILE_LEVELS <- unname(profile_label[TEMPORAL_PROFILES])
profile_colours <- secondary_colours(PROFILE_LEVELS)

left <- ggplot(gain_plot_data,
               aes(gain_m_per_N, delta, colour = profile_label, group = profile_label)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_vline(xintercept = FEEDFORWARD_GAIN_DEFAULT, linetype = "22", colour = "grey35", linewidth = 0.35) +
  # The rule marks the gain every other campaign inherited, which is the reason
  # this sweep is readable as a sensitivity check rather than a tuning curve.
  # Unlabelled it is just a line, so it is named on the panel in the same words
  # the caption uses. The low corner to its right is the only region of the panel
  # no profile passes through.
  annotate("text", x = FEEDFORWARD_GAIN_DEFAULT, y = -0.115, label = "inherited gain",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = -0.06) +
  geom_ribbon(aes(ymin = delta_lo, ymax = delta_hi, fill = profile_label), alpha = 0.10,
              colour = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.5) +
  geom_pointrange(aes(ymin = delta_lo, ymax = delta_hi), linewidth = 0.35, size = 0.3) +
  scale_colour_manual(values = profile_colours, name = "True-force profile") +
  scale_fill_manual(values = profile_colours, guide = "none") +
  # The grid is printed with only the digits each value needs (0, 0.175, 0.35,
  # ...) so the seven labels fit upright without a rotation.
  scale_x_continuous(name = "Target feedforward gain (m/N)",
                     breaks = sort(unique(gain_plot_data$gain_m_per_N)),
                     labels = function(x) formatC(x, format = "g")) +
  scale_y_continuous(name = "Paired completion gain", limits = c(-0.16, 0.86),
                     breaks = seq(0, 0.8, by = 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE))
left <- panel_label(
  left, "A", "Gain sensitivity",
  "Seven tested feedforward values"
)

ab <- ABLATION_CAUSAL_TRANSITION
ab_long <- rbind(
  data.frame(profile = ab$profile, profile_label = unname(profile_label[as.character(ab$profile)]),
             mechanism = "Allocation cost only", mean = ab$alloc_mean,
             lo = ab$alloc_lo, hi = ab$alloc_hi),
  data.frame(profile = ab$profile, profile_label = unname(profile_label[as.character(ab$profile)]),
             mechanism = "Complete arm", mean = ab$complete_mean,
             lo = ab$complete_lo, hi = ab$complete_hi)
)
ab_long$profile_label <- factor(ab_long$profile_label,
                                levels = unname(profile_label[TEMPORAL_PROFILES]))

right <- ggplot(ab_long, aes(profile_label, mean, fill = mechanism)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, alpha = 0.85) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.72),
                width = 0.16, linewidth = 0.35) +
  # Untitled: the two panels put their legends on one shared row, and a second
  # title pushes the row past the canvas and clips the outer key of each.
  scale_fill_manual(values = c("Allocation cost only" = "#999999", "Complete arm" = "#2166AC"),
                    name = NULL) +
  scale_y_continuous(name = "Paired completion gain", limits = c(-0.16, 0.86),
                     breaks = seq(0, 0.8, by = 0.2)) +
  labs(x = NULL) +
  rtx_theme() +
  theme(axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE),
        legend.position = "bottom")
right <- panel_label(
  right, "B", "Pathway ablation",
  "Allocation-only versus complete arm"
)

save_fig((left | right) + plot_layout(widths = c(1.25, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig9_mechanism_gain", FIGURE_CANVAS_WIDTH_IN, 3.70)
