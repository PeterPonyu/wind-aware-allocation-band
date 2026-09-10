# Figure 2. Does the band survive a noisy wind estimate?
#
# Panel A is the paired advantage at the one discriminating level against
# estimate noise. Panel B is the two neighbouring levels, drawn on the same
# vertical scale so that "flat everywhere" can be read off rather than asserted:
# if the band moved under noise, it would move onto one of these.

annotated <- f2a_data
annotated$p_label <- vapply(annotated$p, function(p) paste0("p = ", pval(p)), character(1))

# Adjacent noise levels are close on a linear axis, so alternate the annotation
# above and below the interval rather than letting neighbouring labels collide.
above <- seq_len(nrow(annotated)) %% 2L == 1L
annotated$lab_y <- ifelse(above, annotated$hi + 0.045, annotated$lo - 0.055)

# The pre-set level is drawn as a vertical rule, and a centred label on the first
# point to its right reaches back far enough to print across it. Those labels are
# left-anchored instead, which clears the rule without moving the annotation off
# the point it belongs to.
near_rule <- annotated$sigma > PREREG_SIGMA &
  annotated$sigma - PREREG_SIGMA < 0.12
annotated$lab_h <- ifelse(annotated$sigma == min(annotated$sigma), 0,
                          ifelse(annotated$sigma == max(annotated$sigma), 1,
                                 ifelse(near_rule, 0, 0.5)))

# Both panels share one vertical range. Panel B is only evidence that the band
# did not move if a unit on it is a unit on panel A.
Y_RANGE <- c(-0.13, 0.90)

top <- ggplot(annotated, aes(sigma, delta)) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "grey20", linewidth = 0.4) +
  geom_vline(xintercept = PREREG_SIGMA, linetype = "22", colour = "#B2182B", linewidth = 0.35) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#2166AC", alpha = 0.15) +
  geom_line(colour = "#2166AC", linewidth = 0.5) +
  geom_pointrange(aes(ymin = lo, ymax = hi), colour = "#2166AC",
                  size = 0.32, linewidth = 0.4) +
  geom_text(aes(y = lab_y, label = p_label, hjust = lab_h),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey25") +
  annotate("text", x = PREREG_SIGMA, y = -0.10, label = "pre-set level",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "#B2182B", hjust = -0.04) +
  annotate("text", x = 0, y = 0.035, label = "zero gain",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey25", hjust = 0) +
  scale_x_continuous(name = "Relative wind-estimate noise σ/|w|",
                     breaks = annotated$sigma, expand = expansion(mult = 0.07)) +
  scale_y_continuous(name = "Paired advantage", limits = Y_RANGE) +
  rtx_theme() +
  # The two panels are stacked on one noise axis; the axis title is printed
  # once, under the lower panel, and the tick labels stay on both.
  theme(axis.title.x = element_blank())
top <- panel_label(
  top, "A", "Transition-force noise profile",
  "Paired gain; 95% interval"
)

# The three force levels that recur across the paper keep one colour each
# (0.10 N, 0.15 N, 0.20 N in shared-palette order); this panel draws two.
force_level_colours <- secondary_colours(c("0.10 N", "0.15 N", "0.20 N"))
force_level_shapes <- secondary_shapes(c("0.10 N", "0.15 N", "0.20 N"))

bottom <- ggplot(f2b_data, aes(sigma, delta, colour = level, shape = level)) +
  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.8, fill = "white", stroke = 0.5) +
  scale_colour_manual(values = force_level_colours[levels(f2b_data$level)],
                      name = "Wind force") +
  scale_shape_manual(values = force_level_shapes[levels(f2b_data$level)],
                     name = "Wind force") +
  scale_x_continuous(name = "Relative wind-estimate noise σ/|w|",
                     breaks = unique(f2b_data$sigma), expand = expansion(mult = 0.07)) +
  scale_y_continuous(name = "Paired advantage", limits = Y_RANGE) +
  rtx_theme() +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.margin = margin(2, 4, 2, 4))
bottom <- panel_label(
  bottom, "B", "Neighbouring forces",
  "Same scale; transition localized"
)

save_fig(top / bottom + plot_layout(heights = c(1.3, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig2_noise_sensitivity", FIGURE_CANVAS_WIDTH_IN, 5.20)
