# Figure 3. What the two means are actually made of. Every episode flies the same
# small swarm, so a per-episode completion rate can only take as many values as
# there are airframes, plus zero. Drawing the counts shows that the mean the
# comparison rests on is a mixture over seeds rather than a stable rate.

levels_seen <- names(spread$baseline_value_counts)
f3_data <- data.frame(
  value = factor(rep(levels_seen, 2L), levels = levels_seen),
  arm = factor(rep(arm_labels, each = length(levels_seen)), levels = arm_labels),
  count = c(unlist(spread$baseline_value_counts, use.names = FALSE),
            unlist(spread$wind_aware_value_counts, use.names = FALSE))
)

# An empty cell is still a row, and a bordered zero-height bar draws its outline
# flat on the axis. Those stubs read as small nonzero counts at exactly the values
# where the count is nothing, so the border is dropped where there is no bar to
# border. The row is kept so the dodge slots stay in place and an absent arm
# cannot be mistaken for the other arm shifted over.
p <- ggplot(f3_data, aes(value, count, fill = arm)) +
  geom_col(aes(colour = count > 0), position = position_dodge(width = 0.72),
           width = 0.66, linewidth = 0.25) +
  scale_colour_manual(values = c(`TRUE` = "grey20", `FALSE` = NA), guide = "none") +
  geom_text(aes(label = ifelse(count > 0, count, "")),
            position = position_dodge(width = 0.72), vjust = -0.45,
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY) +
  # A single-column canvas carries the arm names by their distinguishing word,
  # as the other half-width panels do; the caption gives them in full.
  scale_fill_manual(values = arm_colours, name = NULL,
                    labels = c("Wind-agnostic", "Wind-aware")) +
  scale_x_discrete(name = "Per-episode completion rate") +
  scale_y_continuous(name = sprintf("Seed count (of %d)", N_SEEDS), limits = c(0, 9.4),
                     breaks = seq(0, 8, 2)) +
  rtx_theme() +
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0))

save_fig(p, "fig3_seed_spread", FIGURE_COLUMN_WIDTH_IN, 2.60)
