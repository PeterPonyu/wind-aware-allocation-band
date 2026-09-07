# Figure 5. What the seeds sampled, and what they did not.
#
# Panel A puts the ten drawn headings on the circle they were drawn from. Four
# of them land inside a two-degree arc and two more inside another, so the ten
# seeds are not ten conditions, and the widest stretch of the circle nothing was
# drawn from is wider than any stretch that was.
#
# Panel B is why the grouping in panel A is not a choice. The pairwise
# separations fall into two sets with more than an order of magnitude between
# them, and the cut used in the analysis sits in the empty space between the two
# rather than anywhere near a separation it would have to adjudicate.

# Seeds that repeat each other are labelled once, as the group they are, because
# their spokes are too close to carry ten separate labels and pretending
# otherwise would hide the very crowding the panel is about.
f5_labels <- do.call(rbind, lapply(HEADING_GROUPS$members, function(members) {
  theta <- as.numeric(HEADINGS[as.character(members)])
  mid <- atan2(mean(sin(theta)), mean(cos(theta)))
  data.frame(
    theta = mid,
    radius = if (length(members) > 1L) 1.46 else 1.22,
    label = if (length(members) > 1L) {
      sprintf("%s\n<%s\u00b0", paste(members, collapse = ","),
              formatC(degrees(max(angular_separation(
                rep(theta, each = length(theta)), rep(theta, times = length(theta))))),
                format = "f", digits = 2))
    } else {
      sprintf("s%d", members)
    }
  )
}))
f5_labels$x <- f5_labels$radius * cos(f5_labels$theta)
f5_labels$y <- f5_labels$radius * sin(f5_labels$theta)

f5_heads <- data.frame(theta = as.numeric(HEADINGS))
f5_heads$x <- cos(f5_heads$theta)
f5_heads$y <- sin(f5_heads$theta)

f5_circle <- data.frame(t = seq(0, TWO_PI, length.out = 361))
f5_circle$x <- cos(f5_circle$t)
f5_circle$y <- sin(f5_circle$t)

# The empty arc is drawn as the wedge it is, rather than left to be inferred
# from where the spokes are absent.
f5_wedge <- seq(UNSAMPLED_ARC$from, UNSAMPLED_ARC$to, length.out = 120)
f5_wedge <- rbind(data.frame(x = 0, y = 0),
                  data.frame(x = cos(f5_wedge), y = sin(f5_wedge)))
f5_mid <- (UNSAMPLED_ARC$from + UNSAMPLED_ARC$to) / 2

dial <- ggplot() +
  geom_polygon(data = f5_wedge, aes(x, y), fill = "grey88", colour = NA) +
  geom_path(data = f5_circle, aes(x, y), colour = "grey55", linewidth = 0.3) +
  geom_segment(data = f5_heads, aes(x = 0, y = 0, xend = x, yend = y),
               colour = "#2166AC", linewidth = 0.4) +
  geom_point(data = f5_heads, aes(x, y), size = 1.4, colour = "#2166AC") +
  geom_text(data = f5_labels, aes(x, y, label = label),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey20", lineheight = 0.95) +
  annotate("text", x = 0.55 * cos(f5_mid), y = 0.55 * sin(f5_mid),
           label = sprintf("%s\u00b0 unvisited",
                           formatC(degrees(UNSAMPLED_ARC$width), format = "f", digits = 0)),
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30") +
  coord_fixed(xlim = c(-1.95, 1.95), ylim = c(-1.62, 1.62)) +
  rtx_theme() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank())
dial <- panel_label(
  dial, "A", "Seed headings",
  sprintf("%d seeds; %d heading groups", length(HEADINGS), HEADING_GROUPS$n_groups)
)

f5_spectrum <- data.frame(rank = seq_along(SEPARATION_SPECTRUM),
                          sep = degrees(SEPARATION_SPECTRUM))
f5_spectrum$kept <- f5_spectrum$sep <= REPLICATE_TOL_DEG

spectrum <- ggplot(f5_spectrum, aes(rank, sep, colour = kept, shape = kept)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = degrees(REPLICATES$widest_kept),
           ymax = degrees(REPLICATES$nearest_dropped),
           fill = "grey90", alpha = 0.85) +
  geom_hline(yintercept = REPLICATE_TOL_DEG, linetype = "22",
             colour = "#B2182B", linewidth = 0.35) +
  geom_point(size = 1.3, fill = "white", stroke = 0.45) +
  annotate("text", x = nrow(f5_spectrum), y = REPLICATE_TOL_DEG, label = "cut",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "#B2182B", hjust = 1, vjust = -0.8) +
  annotate("text", x = 1, y = sqrt(degrees(REPLICATES$widest_kept) *
                                     degrees(REPLICATES$nearest_dropped)),
           label = "no pair in gap", size = FIGURE_ANNOTATION_SIZE,
           family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = 0, lineheight = 0.95) +
  scale_colour_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "grey45"), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  scale_x_continuous(name = "Pair rank") +
  scale_y_log10(name = "Heading separation (degrees)") +
  rtx_theme() +
  theme()
spectrum <- panel_label(
  spectrum, "B", "Pairwise separations",
  "Repeat cluster and next gap"
)

save_fig((dial | spectrum) + plot_layout(widths = c(1, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig5_heading_coverage", FIGURE_CANVAS_WIDTH_IN, 3.05)
