# Figure 4. The thresholded endpoint hides a continuous one. Completion is a step
# function at the capture radius, so residual miss distance shows what the step
# throws away -- most visibly where completion is pinned at zero for both arms
# and the underlying distances are still separated.

mr <- miss$rows
f4_data <- data.frame(
  wind = rep(mr$wind_N, 2L),
  arm = factor(rep(arm_labels, each = nrow(mr)), levels = arm_labels),
  mean = c(mr$baseline_mean_min_dist_m, mr$wind_aware_mean_min_dist_m),
  sd = c(mr$baseline_std_min_dist_m, mr$wind_aware_std_min_dist_m)
)

# The two arms are measured at identical forces and their dispersions are wide
# enough at the top of the sweep to run through one another, which leaves the
# reader unable to tell which cap belongs to which arm. A fixed horizontal offset
# an order of magnitude below the level spacing separates them without moving any
# point onto a neighbouring force; the caption states that the offset is a display
# device.
ARM_OFFSET_N <- 0.0055
f4_data$wind_shown <- f4_data$wind +
  ifelse(f4_data$arm == arm_labels[1], -ARM_OFFSET_N, ARM_OFFSET_N)

p <- ggplot(f4_data, aes(wind_shown, mean, colour = arm, shape = arm)) +
  geom_hline(yintercept = CAPTURE_RADIUS_M, linetype = "22", colour = "grey35",
             linewidth = 0.35) +
  annotate("text", x = 0.005, y = CAPTURE_RADIUS_M, label = "capture radius",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = 0, vjust = -0.6) +
  geom_errorbar(aes(ymin = pmax(0, mean - sd), ymax = mean + sd),
                width = 0.008, linewidth = 0.35) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.9, fill = "white", stroke = 0.5) +
  scale_colour_manual(values = arm_colours, name = NULL) +
  scale_shape_manual(values = c(21, 24), name = NULL) +
  scale_x_continuous(name = "Steady horizontal wind force (N)", breaks = mr$wind_N) +
  scale_y_continuous(name = "Mean closest approach to target (m)") +
  rtx_theme() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.margin = margin(2, 4, 2, 4))

save_fig(p, "fig4_miss_distance", FIGURE_CANVAS_WIDTH_IN, 3.75)
