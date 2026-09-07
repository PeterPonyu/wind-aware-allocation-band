# Figure 13. Which pathway carries the gain, and what was measured about safety.
#
# Panel A completes a 2 x 2 that the paper previously reported from one side
# only. An earlier figure removes the target-position feedforward and shows the
# gain collapse, which is consistent with the feedforward carrying the effect and
# equally consistent with the two pathways only working together. Running the
# fourth cell separates those readings: the assignment main effect and the
# interaction are drawn on the same axis as the feedforward effect, so a reader
# can see that one term is large and the other two are not.
#
# Panel B is descriptive and is drawn so it cannot be read as anything else. It
# carries the worst case rather than the average, because a clearance statistic
# summarised by its mean answers a question nobody asked about a swarm.

effect_colours <- c("Assignment cost" = "#999999",
                    "Target feedforward" = "#2166AC",
                    "Interaction" = "#B2182B")
effect_shapes <- c("Assignment cost" = 22, "Target feedforward" = 21,
                   "Interaction" = 24)

pathway <- ggplot(FACTORIAL_EFFECTS,
                  aes(force_N, delta, colour = effect_label, shape = effect_label,
                      group = effect_label)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.022), width = 0.016,
                linewidth = 0.35) +
  geom_line(position = position_dodge(width = 0.022), linewidth = 0.45) +
  geom_point(position = position_dodge(width = 0.022), size = 1.6,
             fill = "white", stroke = 0.45) +
  facet_wrap(~profile_label, nrow = 1) +
  scale_colour_manual(values = effect_colours, name = "Factorial term") +
  scale_shape_manual(values = effect_shapes, name = "Factorial term") +
  scale_x_continuous(name = "True horizontal force (N)",
                     breaks = FACTORIAL_FORCES) +
  scale_y_continuous(name = "Paired completion effect", limits = c(-0.16, 0.86),
                     breaks = seq(-0.1, 0.8, by = 0.2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0),
        strip.text = element_text(family = FIGURE_FONT_FAMILY,
                                  size = FIGURE_STRIP_TEXT_SIZE),
        panel.spacing.x = grid::unit(14, "pt"),
        legend.title = element_text(family = FIGURE_FONT_FAMILY,
                                    size = FIGURE_LEGEND_TITLE_SIZE),
        legend.text = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_LEGEND_TEXT_SIZE),
        legend.key.size = unit(0.30, "cm"))
pathway <- panel_label(
  pathway, "A", "Complete 2 x 2 pathway decomposition",
  sprintf("95%% intervals over %d paired seeds", FACTORIAL_SEEDS)
)

# Mean clearance is not the quantity of interest, so the segment is the observed
# range across episodes and the point is the mean inside it. The reader should be
# able to find the single closest approach any pair of airframes made, because
# that is the number a deployment question would start from.
separation <- ggplot(SAFETY_SEPARATION,
                     aes(factor(fmt(force_N, 2)), mean, colour = arm)) +
  geom_linerange(aes(ymin = min, ymax = max),
                 position = position_dodge(width = 0.55), linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.55), size = 1.7,
             shape = 21, fill = "white", stroke = 0.5) +
  geom_hline(yintercept = min(SAFETY_SEPARATION$min), linetype = "22",
             colour = "grey45", linewidth = 0.3) +
  annotate("text", x = 0.5, y = min(SAFETY_SEPARATION$min),
           label = sprintf("closest airframe pair observed: %.3f m",
                           min(SAFETY_SEPARATION$min)),
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = 0, vjust = -0.7) +
  scale_colour_manual(values = arm_colours, name = NULL) +
  scale_x_discrete(name = "Steady horizontal wind force (N)") +
  scale_y_continuous(name = "Minimum pairwise separation (m)",
                     limits = c(0, 0.46), breaks = seq(0, 0.4, 0.1)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0))
separation <- panel_label(
  separation, "B", "Descriptive airframe clearance",
  sprintf("Range over %d seeds; point is the mean", SAFETY_SEEDS)
)

save_fig(pathway / separation + plot_layout(heights = c(1, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig13_pathway_safety", FIGURE_CANVAS_WIDTH_IN, 6.30)
