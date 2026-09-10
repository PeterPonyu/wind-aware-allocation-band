# Figure 8. Temporal force profiles and causal directional estimates.

temporal_plot_data <- temp_rows
temporal_plot_data$profile_label <- factor(
  unname(profile_label[as.character(temporal_plot_data$profile)]),
  levels = unname(profile_label[TEMPORAL_PROFILES])
)
temporal_plot_data$estimator_label <- unname(estimator_label[as.character(temporal_plot_data$estimator)])

# One colour and one marker per estimate condition, shared by both panels and
# drawn from the paper-wide secondary palette (no red or blue, which belong to
# the two allocation arms).
ESTIMATOR_LEVELS <- unname(estimator_label[TEMPORAL_ESTIMATORS])
estimator_colours <- secondary_colours(ESTIMATOR_LEVELS)
estimator_shapes <- secondary_shapes(ESTIMATOR_LEVELS)
temporal_plot_data$estimator_label <- factor(temporal_plot_data$estimator_label,
                                             levels = ESTIMATOR_LEVELS)

left <- ggplot(temporal_plot_data,
               aes(force_N, delta, colour = estimator_label, shape = estimator_label,
                   group = estimator_label)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = delta_lo, ymax = delta_hi),
                position = position_dodge(width = 0.006), width = 0.006, linewidth = 0.3) +
  geom_line(position = position_dodge(width = 0.006), linewidth = 0.45) +
  geom_point(position = position_dodge(width = 0.006), size = 1.65, fill = "white", stroke = 0.45) +
  facet_wrap(~profile_label, nrow = 1) +
  scale_colour_manual(values = estimator_colours, breaks = ESTIMATOR_LEVELS,
                      name = "Estimate") +
  scale_shape_manual(values = estimator_shapes, breaks = ESTIMATOR_LEVELS,
                     name = "Estimate") +
  scale_x_continuous(name = "True horizontal force (N)",
                     breaks = sort(unique(temporal_plot_data$force_N)),
                     expand = expansion(mult = 0.16)) +
  scale_y_continuous(name = "Paired completion gain", limits = c(-0.16, 0.86),
                     breaks = seq(0, 0.8, by = 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        strip.text = element_text(family = FIGURE_FONT_FAMILY,
                                  size = FIGURE_STRIP_TEXT_SIZE),
        # The outermost tick of one facet and the innermost tick of the next
        # each overhang their panel by half a label, so the gap has to be wider
        # than one label or 0.20 and 0.10 print as a single run of digits.
        panel.spacing.x = grid::unit(22, "pt"),
        axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE))
left <- panel_label(
  left, "A", "Temporal gain",
  "Constant, slow and fast profiles"
)

tracking_plot_data <- tracking_rows
tracking_plot_data$profile_label <- factor(
  unname(profile_label[as.character(tracking_plot_data$profile)]),
  levels = unname(profile_label[TEMPORAL_PROFILES])
)
tracking_plot_data$estimator_label <- factor(
  unname(estimator_label[as.character(tracking_plot_data$estimator)]),
  levels = ESTIMATOR_LEVELS
)
tracking_plot_data$error_pct <- tracking_plot_data$mean_relative_error_mean * 100

right <- ggplot(tracking_plot_data,
                aes(profile_label, error_pct, fill = estimator_label, colour = estimator_label)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, alpha = 0.82, colour = NA) +
  # A marker on top of a bar restates the height the bar already gives. The one
  # slot where it carries anything is the oracle, whose error is zero by
  # construction and so has no bar at all; unmarked, its dodge slot reads as a
  # condition that was not run rather than one that was measured at zero. Every
  # row is kept so the slots stay aligned with the bars, and only the zero rows
  # are drawn.
  geom_point(aes(alpha = error_pct == 0), position = position_dodge(width = 0.72),
             shape = 21, size = 1.5, colour = "grey15", fill = "white", stroke = 0.4) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0), guide = "none") +
  annotate("text", x = 0.42, y = 31.2,
           label = "No oracle bar: its error is zero by construction",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = 0) +
  scale_fill_manual(values = estimator_colours, guide = "none") +
  scale_colour_manual(values = estimator_colours, guide = "none") +
  scale_y_continuous(name = "Mean relative tracking error (%)", limits = c(0, 33.5),
                     breaks = seq(0, 30, by = 10)) +
  labs(x = NULL) +
  rtx_theme() +
  theme(axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE),
        legend.position = "none")
right <- panel_label(
  right, "B", "Tracking error",
  "Causal estimate across profiles"
)

save_fig((left | right) + plot_layout(widths = c(1.25, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig8_temporal_envelope", FIGURE_CANVAS_WIDTH_IN, 3.70)
