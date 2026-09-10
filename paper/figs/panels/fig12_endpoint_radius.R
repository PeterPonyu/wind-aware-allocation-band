# Figure 12. Is the transition a property of the task, or of the capture radius
# that scores it?
#
# Completion is a thresholded endpoint, so every number in this paper depends on
# a distance the experimenter chose. Panel A puts the paired gain against force
# once per radius. The transition does not disappear when the radius moves and
# it does not stay put either: it slides along the force axis, so the force at
# which directional information pays is a property of the operating point rather
# than a constant of the allocator. Panel B shows why, by drawing the two
# completion ladders the gains are differences of. A tighter radius makes the
# same disturbance harder and moves the whole envelope left; a looser one moves
# it right. Reporting only the anchor would have presented one column of this
# grid as if it were the whole result.

radius_label <- function(r) sprintf("%.2f m", r)
RADIUS_COMPLETION$radius_label <- factor(radius_label(RADIUS_COMPLETION$radius_m),
                                         levels = radius_label(RADIUS_GRID_M))
RADIUS_PEAK$radius_label <- factor(radius_label(RADIUS_PEAK$radius_m),
                                   levels = radius_label(RADIUS_GRID_M))

# Radii take the paper-wide secondary palette; the arm red/blue are reserved
# for the two allocators in Panel B.
radius_colours <- secondary_colours(radius_label(RADIUS_GRID_M))
radius_shapes <- secondary_shapes(radius_label(RADIUS_GRID_M))

gain <- ggplot(RADIUS_COMPLETION,
               aes(force_N, delta, colour = radius_label, shape = radius_label,
                   group = radius_label)) +
  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.011), width = 0.010,
                linewidth = 0.35) +
  geom_line(position = position_dodge(width = 0.011), linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.011), size = 1.8,
             fill = "white", stroke = 0.5) +
  # One label per radius, on that radius' own peak. Three peaks at three
  # different forces are the entire finding, and a reader should not have to
  # trace three lines back to the axis to recover them.
  geom_text(data = RADIUS_PEAK, inherit.aes = FALSE,
            aes(x = force_N, y = hi + 0.055, colour = radius_label,
                label = sprintf("%s N: %+.2f", fmt(force_N, 2), delta)),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            hjust = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = radius_colours, name = "Capture radius") +
  scale_shape_manual(values = radius_shapes, name = "Capture radius") +
  # Panel B below is faceted on a different force grid, so this axis is titled
  # in its own right rather than borrowing the facets' title.
  scale_x_continuous(name = "Steady horizontal wind force (N)",
                     breaks = sort(unique(RADIUS_COMPLETION$force_N))) +
  scale_y_continuous(name = "Paired advantage",
                     limits = c(-0.14, 0.88), breaks = seq(0, 0.8, 0.2)) +
  rtx_theme() +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.margin = margin(2, 4, 2, 4),
        legend.title = element_text(family = FIGURE_FONT_FAMILY,
                                    size = FIGURE_LEGEND_TITLE_SIZE),
        legend.text = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_LEGEND_TEXT_SIZE),
        legend.key.size = unit(0.30, "cm"))
gain <- panel_label(
  gain, "A", "Paired advantage by capture radius",
  sprintf("95%% intervals over %d paired seeds", RADIUS_SEEDS)
)

ladder_data <- rbind(
  data.frame(radius_label = RADIUS_COMPLETION$radius_label,
             force_N = RADIUS_COMPLETION$force_N,
             arm = factor(unname(arm_labels[["agnostic"]]), levels = unname(arm_labels)),
             rate = RADIUS_COMPLETION$baseline),
  data.frame(radius_label = RADIUS_COMPLETION$radius_label,
             force_N = RADIUS_COMPLETION$force_N,
             arm = factor(unname(arm_labels[["aware"]]), levels = unname(arm_labels)),
             rate = RADIUS_COMPLETION$aware)
)
peak_rule <- data.frame(radius_label = RADIUS_PEAK$radius_label,
                        force_N = RADIUS_PEAK$force_N)

ladders <- ggplot(ladder_data, aes(force_N, rate, colour = arm, shape = arm)) +
  geom_vline(data = peak_rule, inherit.aes = FALSE, aes(xintercept = force_N),
             linetype = "22", colour = "grey45", linewidth = 0.3) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 1.5, fill = "white", stroke = 0.45) +
  facet_wrap(~radius_label, nrow = 1) +
  scale_colour_manual(values = arm_colours, name = NULL) +
  scale_shape_manual(values = c(21, 24), name = NULL) +
  scale_x_continuous(name = "Steady horizontal wind force (N)",
                     breaks = c(0, 0.1, 0.2, 0.3)) +
  scale_y_continuous(name = "Task completion rate", limits = c(-0.02, 1.05),
                     breaks = seq(0, 1, 0.25)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0),
        strip.text = element_text(family = FIGURE_FONT_FAMILY,
                                  size = FIGURE_STRIP_TEXT_SIZE),
        panel.spacing.x = grid::unit(12, "pt"))
ladders <- panel_label(
  ladders, "B", "Completion ladders behind those gains",
  "Dashed rule marks each radius' own peak"
)

save_fig(gain / ladders + plot_layout(heights = c(1.15, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig12_endpoint_radius", FIGURE_CANVAS_WIDTH_IN, 5.80)
