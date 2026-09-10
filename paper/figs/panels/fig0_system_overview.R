# Figure overview. Panel A makes the research object visible; Panel B bridges
# the thresholded task endpoint to the continuous closest-approach endpoint.
# Both panels use the already bound primary record and preserve the seed-level
# pairing. The figure is explanatory plus descriptive, not a new statistical
# test.

library(grid)

launch <- data.frame(
  id = paste0("D", 1:4),
  x = c(-0.72, -0.72, -0.42, -0.42),
  y = c(-0.40, -0.10, -0.40, -0.10),
  stringsAsFactors = FALSE
)
targets <- data.frame(
  id = paste0("W", 1:4),
  x = c(0.42, 0.78, 0.48, 0.88),
  y = c(0.52, 0.42, -0.18, -0.52),
  stringsAsFactors = FALSE
)

# Two short rotor bars per airframe make the object recognisable without using
# an image asset or introducing a second font family.
rotor_segments <- do.call(rbind, lapply(seq_len(nrow(launch)), function(i) {
  data.frame(
    x = c(launch$x[i] - 0.085, launch$x[i]),
    y = c(launch$y[i], launch$y[i] - 0.085),
    xend = c(launch$x[i] + 0.085, launch$x[i]),
    yend = c(launch$y[i], launch$y[i] + 0.085)
  )
}))

assignment <- data.frame(
  x = launch$x, y = launch$y,
  xend = targets$x, yend = targets$y
)

scene <- ggplot() +
  geom_rect(aes(xmin = -1.02, xmax = 1.08, ymin = -0.78, ymax = 0.80),
            fill = "grey98", colour = "grey70", linewidth = 0.35) +
  geom_segment(data = assignment, aes(x, y, xend = xend, yend = yend),
               colour = "grey55", linewidth = 0.45, linetype = "22",
               arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")) +
  geom_segment(data = rotor_segments, aes(x, y, xend = xend, yend = yend),
               colour = "#2166AC", linewidth = 0.65, lineend = "round") +
  geom_point(data = launch, aes(x, y), shape = 21, size = 3.2,
             fill = "white", colour = "#2166AC", stroke = 0.85) +
  geom_point(data = targets, aes(x, y), shape = 22, size = 3.1,
             fill = "white", colour = "#B2182B", stroke = 0.85) +
  geom_text(data = launch, aes(x, y, label = id), nudge_y = -0.13,
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "#2166AC") +
  geom_text(data = targets, aes(x, y, label = id), nudge_y = 0.13,
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "#B2182B") +
  geom_segment(aes(x = -0.88, y = 0.66, xend = -0.18, yend = 0.66),
               colour = "#B2182B", linewidth = 0.75,
               arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")) +
  annotate("text", x = -0.53, y = 0.74, label = "wind vector w",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "#B2182B") +
  # Both keys sit on one line inside a half-width panel, so they carry only the
  # word that separates the two costs; the caption states them in full.
  annotate("label", x = -0.58, y = -0.69, label = "Agnostic: Euclidean",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           fill = "white", colour = "#B2182B") +
  annotate("label", x = 0.50, y = -0.69, label = "Aware: upwind + bias",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           fill = "white", colour = "#2166AC") +
  annotate("label", x = 0.04, y = 0.18,
           label = "assignment \u2192 controller \u2192 capture",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           fill = "white", colour = "grey30") +
  coord_equal(xlim = c(-1.08, 1.14), ylim = c(-0.84, 0.86), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  rtx_theme() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank())
scene <- panel_label(
  scene, "A", "Research object",
  "Four airframes, four waypoints"
)

bridge_long <- rbind(
  data.frame(seed = BRIDGE_PAIRS$seed, arm = "Wind-agnostic",
             completion = BRIDGE_PAIRS$baseline_completion,
             distance = BRIDGE_PAIRS$baseline_distance),
  data.frame(seed = BRIDGE_PAIRS$seed, arm = "Wind-aware",
             completion = BRIDGE_PAIRS$aware_completion,
             distance = BRIDGE_PAIRS$aware_distance)
)
bridge_segments <- data.frame(
  seed = BRIDGE_PAIRS$seed,
  x = BRIDGE_PAIRS$baseline_distance,
  y = BRIDGE_PAIRS$baseline_completion,
  xend = BRIDGE_PAIRS$aware_distance,
  yend = BRIDGE_PAIRS$aware_completion
)
bridge_x_limits <- c(max(0.13, BRIDGE_DISTANCE_RANGE[1] - 0.025),
                     BRIDGE_DISTANCE_RANGE[2] + 0.025)

endpoint <- ggplot() +
  geom_vline(xintercept = CAPTURE_RADIUS_M, linetype = "22",
             colour = "grey40", linewidth = 0.4) +
  geom_segment(data = bridge_segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey62", linewidth = 0.45,
               arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")) +
  geom_point(data = bridge_long,
             aes(x = distance, y = completion, colour = arm, fill = arm),
             shape = 21, size = 2.35, stroke = 0.55) +
  # Set horizontally in the clear band above the data rather than rotated up the
  # rule. Rotated, the string runs the full height of the region the pairing
  # arrows converge into, and the arrowhead landing on the shortest wind-aware
  # distance prints across its last glyphs. Nothing is drawn above the highest
  # completion value, so the label sits there on the same line as the summary
  # block at the other end of the panel.
  annotate("text", x = CAPTURE_RADIUS_M + 0.005, y = 0.96,
           label = "capture radius", hjust = 0, vjust = 0.5,
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30") +
  annotate("label", x = bridge_x_limits[2] - 0.006, y = 0.96,
           hjust = 1, label = sprintf("%d/%d paired distances lower\nmean shift %+.3f m",
                                      BRIDGE_N_LOWER, N_SEEDS, BRIDGE_DISTANCE_DELTA_MEAN),
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           fill = "white", colour = "grey25",
           lineheight = 0.95) +
  scale_colour_manual(values = c("Wind-agnostic" = "#B2182B",
                                 "Wind-aware" = "#2166AC"), name = NULL) +
  scale_fill_manual(values = c("Wind-agnostic" = "#B2182B",
                               "Wind-aware" = "#2166AC"), name = NULL) +
  scale_x_continuous(name = "Mean closest approach (m)", limits = bridge_x_limits,
                     breaks = pretty(bridge_x_limits, n = 4)) +
  scale_y_continuous(name = "Completion rate", limits = c(-0.05, 1.05),
                     breaks = c(0, 0.25, 0.50, 0.75, 1.00)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.key.width = grid::unit(0.42, "cm"),
        legend.text = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_LEGEND_TEXT_SIZE))
endpoint <- panel_label(
  endpoint, "B", "Endpoint bridge",
  sprintf("Arrows connect matched seeds at %.2f N", KNEE_N)
)

save_fig(
  (scene | endpoint) + plot_layout(widths = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal"),
  "fig0_system_overview", FIGURE_CANVAS_WIDTH_IN, 3.50
)
