# Figure 11. Swarm-size sensitivity of the directional operating envelope.
#
# Panel A keeps the force axis explicit and asks whether the paired transition
# survives as the homogeneous team grows.  Panel B opens the transition force
# itself and reports the two endpoint distributions, so a positive paired gain
# cannot be mistaken for a change in the scale of the completion metric.

scale_plot_data <- SCALE_ROWS
scale_plot_data$force_label <- factor(
  sprintf("%.2f N", scale_plot_data$force_N),
  levels = sprintf("%.2f N", SCALE_FORCES)
)

curve <- ggplot(
  scale_plot_data,
  aes(num_drones, delta, colour = force_label, shape = force_label,
      group = force_label)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = delta_lo, ymax = delta_hi), width = 0.16,
                linewidth = 0.35) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.9, fill = "white", stroke = 0.5) +
  scale_colour_manual(values = c("0.10 N" = "#4D9221", "0.15 N" = "#2166AC",
                                 "0.20 N" = "#B2182B"), name = "True force") +
  scale_shape_manual(values = c("0.10 N" = 21, "0.15 N" = 22, "0.20 N" = 24),
                     name = "True force") +
  scale_x_continuous(name = "Swarm size (airframes)", breaks = SCALE_SIZES,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Paired completion gain", limits = c(-0.12, 0.84),
                     breaks = seq(-0.1, 0.8, by = 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.box = "horizontal")
curve <- panel_label(
  curve, "A", "Scale operating envelope",
  "Paired gain across team sizes"
)

transition <- SCALE_TRANSITION
completion_long <- rbind(
  data.frame(
    num_drones = transition$num_drones,
    arm = "Wind-agnostic allocation",
    mean = nested_numeric(transition$baseline_completion, "mean"),
    lo = ci_component(transition$baseline_completion$ci95, 1L),
    hi = ci_component(transition$baseline_completion$ci95, 2L)
  ),
  data.frame(
    num_drones = transition$num_drones,
    arm = "Wind-aware allocation",
    mean = nested_numeric(transition$aware_completion, "mean"),
    lo = ci_component(transition$aware_completion$ci95, 1L),
    hi = ci_component(transition$aware_completion$ci95, 2L)
  )
)
completion_long$arm <- factor(
  completion_long$arm,
  levels = c("Wind-agnostic allocation", "Wind-aware allocation")
)

endpoint <- ggplot(completion_long, aes(num_drones, mean, colour = arm, shape = arm)) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.34), width = 0.18,
                linewidth = 0.4) +
  geom_line(aes(group = arm), position = position_dodge(width = 0.34),
            linewidth = 0.45) +
  geom_point(position = position_dodge(width = 0.34), size = 2.0,
             fill = "white", stroke = 0.55) +
  # The narrower of the two panels carries this legend, so the arm names are
  # shortened to their distinguishing word; the caption gives them in full.
  scale_colour_manual(values = arm_colours, name = NULL,
                      labels = c("Wind-agnostic", "Wind-aware")) +
  scale_shape_manual(values = c(21, 24), name = NULL,
                     labels = c("Wind-agnostic", "Wind-aware")) +
  scale_x_continuous(name = "Swarm size (airframes)", breaks = SCALE_SIZES,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Completion rate at 0.15 N", limits = c(-0.04, 1.04),
                     breaks = seq(0, 1, by = 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.box = "horizontal")
endpoint <- panel_label(
  endpoint, "B", "Transition endpoint",
  "95% intervals over 30 paired seeds"
)

save_fig((curve | endpoint) + plot_layout(widths = c(1.18, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig11_swarm_scale", FIGURE_CANVAS_WIDTH_IN, 3.70)
