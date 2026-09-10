# Figure 7. The same claim, measured against a replicate that was designed.
#
# Left: the paired advantage at every force level. The drawn-heading record put
# ten points on this axis, one per seed, with the heading and the scene moving
# together. The crossed record puts 288 cells on it at every level, with the
# heading assigned and every scene flown at every heading. For inference, the
# 24 fixed headings are averaged within each of the 12 layout clusters and an
# ordinary bootstrap is taken over those 12 layout-level means; cells are not
# treated as independent replicates.
#
# Right: the discriminating level opened out into the rectangle the left panel
# averages over. Each cell is one paired episode. If the advantage were a
# function of the wind the rows would be flat bands; if it were a function of the
# scene the columns would be. Neither is what the record shows, and the share of
# the variation carried by each margin is printed under the panel.

f7_curve <- data.frame(
  wind_N = DESIGNED_BY_FORCE$wind_N,
  delta = DESIGNED_BY_FORCE$mean_advantage,
  lo = DESIGNED_BY_FORCE$ci95_lo_over_scenes,
  hi = DESIGNED_BY_FORCE$ci95_hi_over_scenes
)
f7_drawn <- data.frame(wind_N = kr$wind_N, delta = kr$paired_delta_mean)

# The crossed series is the paired advantage of the wind-aware arm and keeps
# the blue that quantity carries throughout; the drawn-seed crosses are a
# second record of the same quantity, not the wind-agnostic arm, so they take
# a shared secondary colour (the one dark enough to carry its own text label)
# rather than the arm red.
DRAWN_SEED_COLOUR <- unname(OKABE_ITO["green"])

curve <- ggplot(f7_curve, aes(wind_N, delta)) +
  geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey55") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#2166AC", alpha = 0.18) +
  geom_line(colour = "#2166AC", linewidth = 0.5) +
  geom_point(colour = "#2166AC", size = 1.5) +
  geom_point(data = f7_drawn, shape = 4, size = 1.7, stroke = 0.6,
             colour = DRAWN_SEED_COLOUR) +
  annotate("text", x = 0.238, y = 0.50, size = FIGURE_ANNOTATION_SIZE,
           family = FIGURE_FONT_FAMILY, hjust = 0, colour = "#2166AC",
           label = sprintf("%d pairs\nper level", N_PAIRS_PER_FORCE)) +
  annotate("text", x = 0.238, y = 0.29, size = FIGURE_ANNOTATION_SIZE,
           family = FIGURE_FONT_FAMILY, hjust = 0, colour = DRAWN_SEED_COLOUR,
           label = sprintf("%d drawn\nseeds", N_SEEDS)) +
  scale_x_continuous(name = "Steady horizontal wind force (N)",
                     breaks = f7_curve$wind_N) +
  scale_y_continuous(name = "Paired advantage",
                     limits = c(-0.06, 0.80)) +
  rtx_theme() +
  theme(axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE))
curve <- panel_label(
  curve, "A", "Crossed gain",
  "Paired gain; 12-layout cluster bootstrap"
)

cells <- expand.grid(scene = as.integer(colnames(KNEE_MATRIX)),
                     heading = as.numeric(rownames(KNEE_MATRIX)))
cells$delta <- as.vector(t(KNEE_MATRIX))

rect <- ggplot(cells, aes(factor(scene), heading, fill = delta)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_gradient2(name = "Paired advantage",
                       low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1),
                       breaks = c(-1, 0, 1)) +
  scale_x_discrete(name = "Scene") +
  scale_y_continuous(name = "Assigned wind heading (deg)",
                     breaks = seq(0, 345, by = 45), expand = c(0, 0)) +
  rtx_theme() +
  theme(
        axis.text.x = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_AXIS_TEXT_SIZE),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.width = unit(0.9, "cm"),
        legend.key.height = unit(0.26, "cm"),
        legend.title = element_text(family = FIGURE_FONT_FAMILY,
                                    size = FIGURE_LEGEND_TITLE_SIZE, vjust = 0.85),
        legend.text = element_text(family = FIGURE_FONT_FAMILY,
                                   size = FIGURE_LEGEND_TEXT_SIZE))
rect <- panel_label(
  rect, "B", "Transition map",
  "Interaction shown by cell"
)

# The colour bar is the only guide in the figure; collecting it centres it
# under both panels instead of leaving Panel A a blank strip beside it.
save_fig((curve | rect) + plot_layout(widths = c(1.08, 1), guides = "collect") &
           theme(legend.position = "bottom"),
         "fig7_designed_replicates", FIGURE_CANVAS_WIDTH_IN, 3.35)
