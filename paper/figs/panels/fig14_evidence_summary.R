# Figure 12. Cross-campaign evidence synthesis.
#
# Panel A keeps the paired completion-gain scale fixed while showing the
# transition estimate from each already-bound campaign.  The rows are not
# pooled: each interval retains the inferential unit declared by its source
# receipt, and the crossed design is shown without inventing a p-value that its
# receipt does not provide.
#
# Panel B reports the volume of episode rows behind each evidence block.  The
# labels name the corresponding seed/layout unit so a large row count cannot be
# mistaken for an inflated independent sample size.

forest <- ggplot(EVIDENCE_FOREST, aes(y = label, x = estimate, colour = group)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 0.45,
                orientation = "y") +
  geom_point(size = 2.0, fill = "white", stroke = 0.55, shape = 21) +
  geom_text(
    aes(x = pmin(0.86, hi + 0.025), label = p_label),
    hjust = 0, size = FIGURE_ANNOTATION_SIZE, colour = "grey25", family = FIGURE_FONT_FAMILY,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("Primary" = "#B2182B", "Estimate noise" = "#762A83",
               "Crossed design" = "#4D9221", "Causal temporal" = "#2166AC",
               "Team-size transfer" = "#1B7837"),
    name = "Evidence block"
  ) +
  scale_x_continuous(
    name = "Paired completion gain (rate units)",
    # The upper limit is headroom for the p-value column, not a data range: the
    # widest label is set to the right of the widest interval and would
    # otherwise be clipped at the panel edge.
    limits = c(-0.20, 1.22), breaks = seq(-0.2, 1.0, by = 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(y = NULL) +
  rtx_theme() +
  theme(
    axis.text.y = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE, hjust = 1),
    axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE),
    legend.position = "none"
  )
forest <- panel_label(
  forest, "A", "Transition estimates",
  "Campaign-specific intervals"
)

# The floor has to sit under the smallest block, not under the smallest block
# that existed when the panel was written: the descriptive safety campaign is
# deliberately the shortest row in the set, and a floor above it silently drops
# the row instead of showing how much smaller it is.
volume_start <- 40
volume_colours <- c(
  "Primary force ladder" = "#B2182B", "Estimate noise" = "#762A83",
  "Crossed design" = "#4D9221", "Temporal profiles" = "#2166AC",
  "Pathway ablation" = "#999999", "Gain sensitivity" = "#6A3D9A",
  "Scene geometry" = "#1B7837", "Swarm-size scale" = "#2166AC",
  "Endpoint radius" = "#D6604D", "Pathway factorial" = "#35978F",
  "Safety diagnostics" = "#8C6D31"
)
volume <- ggplot(EVIDENCE_VOLUME, aes(y = campaign, x = episodes, colour = campaign)) +
  geom_segment(aes(x = volume_start, xend = episodes, yend = campaign),
               linewidth = 3.2, lineend = "butt", show.legend = FALSE) +
  geom_point(size = 2.25, show.legend = FALSE) +
  geom_text(
    # The row ends in a point marker, not the bar edge, so the label is cleared
    # past the marker radius rather than past `episodes`.  A multiplicative
    # offset is a constant distance on this log scale, so one factor holds for
    # the short and long rows alike.
    aes(x = episodes * 1.16, label = paste0(format(episodes, big.mark = ",", scientific = FALSE),
                                      "\n", unit)),
    hjust = 0, lineheight = 0.92, size = FIGURE_ANNOTATION_SIZE,
    colour = "grey20", family = FIGURE_FONT_FAMILY
  ) +
  scale_colour_manual(values = volume_colours) +
  scale_x_log10(
    name = "Episode rows (log scale)", limits = c(volume_start, 60000),
    # Whole decades only. This panel is roughly a third of the text width, so a
    # half-decade tick sits closer to its neighbour than the label is wide and
    # the two print as one blur; and mixing plain hundreds with exponent
    # notation on the same axis makes the reader convert between two formats to
    # compare four ticks. The largest block (4,032 rows) still sits inside the
    # scale, and the exact count is printed beside every row regardless.
    breaks = c(100, 1000, 10000),
    labels = c("100", "1,000", "10,000"),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(y = NULL) +
  rtx_theme() +
  theme(
    axis.text.y = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE, hjust = 1),
    axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                               size = FIGURE_AXIS_TEXT_SIZE),
    axis.title.x = element_text(family = FIGURE_FONT_FAMILY,
                                size = FIGURE_AXIS_TITLE_SIZE)
  )
volume <- panel_label(
  volume, "B", "Evidence volume",
  "Unit beside each row"
)

save_fig(
  (forest | volume) + plot_layout(widths = c(1.5, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(size = 6.8, family = FIGURE_FONT_FAMILY),
      legend.title = element_text(size = 7.2, face = "bold", family = FIGURE_FONT_FAMILY),
      legend.key.width = unit(0.35, "cm"),
      legend.spacing.x = unit(0.12, "cm")
    ),
  "fig14_evidence_summary", FIGURE_CANVAS_WIDTH_IN, 5.05
)
