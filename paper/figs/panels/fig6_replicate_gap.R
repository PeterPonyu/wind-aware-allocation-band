# Figure 6. Where the outcome stops being a function of the disturbance.
#
# The bar at each force level is the largest completion difference between two
# episodes of the same arm whose headings differ by less than the cut: a
# repetition of one condition, and therefore a lower bound on the outcome
# variation the recorded conditions do not account for.
#
# The strip below the axis carries the fact that makes the bars readable. At
# four of the levels every seed of both arms returned the same value, so no
# pairing of seeds could have disagreed and a bar of zero there is the ceiling
# or the floor rather than evidence of repeatability. Those levels are marked in
# the strip instead of being left to look like clean repetitions, which is the
# reading this figure exists to prevent.

f6_data <- GAPS
f6_data$informative <- GAPS$seed_spread > 0
f6_data$drones <- f6_data$gap * N_DRONES
# A level where the repetitions agree is only worth printing when they could
# have disagreed, so the count is shown wherever the seeds differ at all and
# withheld, rather than shown as zero, wherever they do not.
f6_data$label <- ifelse(f6_data$informative,
                        sprintf("%d/%d\ndiffer",
                                vapply(f6_data$wind_N, disagreeing_at, integer(1)),
                                nrow(REPLICATES$pairs)),
                        "")

STRIP <- c(-0.42, -0.14)

p <- ggplot(f6_data, aes(wind_N, drones)) +
  geom_rect(aes(xmin = wind_N - 0.017, xmax = wind_N + 0.017,
                ymin = STRIP[1], ymax = STRIP[2], fill = informative),
            colour = "grey30", linewidth = 0.2) +
  # A zero gap has no bar, but a bordered bar of zero height still draws its
  # outline flat on the baseline, and that stub reads as a small nonzero gap at
  # exactly the levels where the gap is nothing. The border is dropped where
  # there is no bar to border; the strip below the axis carries what those
  # levels mean.
  geom_col(aes(colour = drones > 0), width = 0.030, fill = "#B2182B",
           linewidth = 0.25) +
  scale_colour_manual(values = c(`TRUE` = "grey20", `FALSE` = NA), guide = "none") +
  # On a single-column canvas a bar is narrower than the word "differ", so
  # every count sits above its bar in the same grey, and the transition
  # annotation is lifted clear of the tallest bar's label.
  geom_text(aes(label = label, y = drones + 0.12),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey25", vjust = 0, lineheight = 0.95) +
  annotate("text", x = KNEE_N, y = 4.05, label = "transition",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey20", hjust = 0.5) +
  scale_fill_manual(values = c(`TRUE` = "grey55", `FALSE` = "grey92"),
                    breaks = c(TRUE, FALSE),
                    labels = c("repeat variation", "uniform outcome"),
                    name = "Strip below the axis:") +
  scale_x_continuous(name = "Steady horizontal wind force (N)",
                     breaks = f6_data$wind_N, limits = c(-0.028, 0.328)) +
  scale_y_continuous(name = sprintf("Repeat gap (airframes, of %d)", N_DRONES),
                     limits = c(STRIP[1], 4.4), breaks = 0:4) +
  rtx_theme() +
  guides(fill = guide_legend(nrow = 1, title.position = "top")) +
  # The key belongs under the axis it describes, and every other legend in the
  # figure set sits below its panel; centred above, it reads as a floating
  # caption that lines up with nothing.
  theme(legend.position = "bottom", legend.margin = margin(2, 0, 0, 0),
        legend.title = element_text(family = FIGURE_FONT_FAMILY,
                                    size = FIGURE_LEGEND_TITLE_SIZE),
        legend.text = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_LEGEND_TEXT_SIZE),
        legend.key.size = unit(0.30, "cm"))

save_fig(p, "fig6_replicate_gap", FIGURE_COLUMN_WIDTH_IN, 2.95)
