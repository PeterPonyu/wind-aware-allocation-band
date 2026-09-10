# Figure 1. The operating envelope, and the paired contrast the claim rests on.
#
# Panel A is the completion ladder for the two allocators. Panel B is the
# seed-level paired difference on the same force axis. Both are needed: the
# ladder carries the shape of the envelope but its whiskers are standard
# deviations of two marginal means, and at the transition those overlap even
# though every seed moves the same way. Reading the anchor figure from panel A
# alone therefore understates the contrast the paper is about, so the quantity
# the test is computed on is drawn beside it rather than deferred to a table.

kr <- knee$rows
f1_data <- data.frame(
  wind = rep(kr$wind_N, 2L),
  arm = factor(rep(arm_labels, each = nrow(kr)), levels = arm_labels),
  mean = c(kr$baseline_mean, kr$wind_aware_mean),
  sd = c(kr$baseline_std, kr$wind_aware_std)
)

band_interval <- paired_interval(kr$paired_delta_mean, kr$paired_delta_std, kr$n)
f1_delta <- data.frame(
  wind = kr$wind_N,
  delta = kr$paired_delta_mean,
  lo = band_interval$lo,
  hi = band_interval$hi
)

# The shading boundaries sit midway between the declared levels rather than on
# them. On the level itself an edge bisects the point it is describing, and the
# reader cannot tell which side of the boundary that point belongs to.
SHOULDER_EDGE_N <- mean(c(SATURATION_N, KNEE_N))
COLLAPSE_EDGE_N <- mean(c(KNEE_N, COLLAPSE_N))

regions <- data.frame(
  xmin = c(-Inf, COLLAPSE_EDGE_N),
  xmax = c(SHOULDER_EDGE_N, Inf)
)

# One row of zone labels along the top, clear of the data in both panels. The
# earlier layout put them at mid-height, where "shared ceiling" printed a long
# way below the ceiling it names.
zone_labels <- data.frame(
  x = c(SATURATION_N / 2, KNEE_N, (COLLAPSE_N + max(kr$wind_N)) / 2),
  label = c("near-complete shoulder", "transition", "high-force edge")
)

band_layers <- list(
  geom_rect(data = regions, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey92", alpha = 0.9),
  geom_vline(xintercept = KNEE_N, linetype = "22", colour = "grey35",
             linewidth = 0.3)
)

force_axis <- scale_x_continuous(
  name = "Steady horizontal wind force (N)",
  breaks = kr$wind_N,
  sec.axis = sec_axis(~ . / HOVER_WEIGHT_N * 100,
                      name = "Wind force (% of hover thrust)")
)

ladder <- ggplot(f1_data, aes(wind, mean, colour = arm, shape = arm)) +
  band_layers +
  geom_text(data = zone_labels, inherit.aes = FALSE,
            aes(x = x, y = 1.10, label = label),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey30") +
  geom_errorbar(aes(ymin = pmax(0, mean - sd), ymax = pmin(1, mean + sd)),
                width = 0.008, linewidth = 0.35) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.9, fill = "white", stroke = 0.5) +
  scale_colour_manual(values = arm_colours, name = NULL) +
  scale_shape_manual(values = c(21, 24), name = NULL) +
  force_axis +
  scale_y_continuous(name = "Task completion rate", limits = c(-0.02, 1.16),
                     breaks = seq(0, 1, 0.25)) +
  rtx_theme() +
  theme(axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.margin = margin(2, 4, 2, 4))
ladder <- panel_label(
  ladder, "A", "Completion ladder",
  "Seed means ± one standard deviation"
)

# The exact test at the transition is quoted on the panel because the figure
# exists to stop a reader inferring significance from whisker overlap in panel
# A. Every other level is drawn on the same axis so the reader can see that the
# interval clears zero at one level only.
knee_delta <- f1_delta[abs(f1_delta$wind - KNEE_N) < 1e-12, ]
knee_p <- kr$wilcoxon$p_two_sided[abs(kr$wind_N - KNEE_N) < 1e-12]

contrast <- ggplot(f1_delta, aes(wind, delta)) +
  band_layers +
  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.008, linewidth = 0.4,
                colour = "#2166AC") +
  geom_line(linewidth = 0.5, colour = "#2166AC") +
  geom_point(size = 1.9, shape = 21, fill = "white", stroke = 0.55,
             colour = "#2166AC") +
  geom_text(data = knee_delta, inherit.aes = FALSE,
            aes(x = wind + 0.006, y = hi - 0.02,
                label = sprintf("+%s, exact p = %s", fmt(delta, 2), pval(knee_p))),
            size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
            colour = "grey20", hjust = 0) +
  annotate("text", x = 0, y = 0.055, label = "no paired difference",
           size = FIGURE_ANNOTATION_SIZE, family = FIGURE_FONT_FAMILY,
           colour = "grey30", hjust = 0) +
  force_axis +
  scale_y_continuous(name = "Paired advantage",
                     limits = c(-0.14, 0.84), breaks = seq(0, 0.8, 0.2)) +
  rtx_theme() +
  theme(axis.title.x.top = element_blank(), axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank())
contrast <- panel_label(
  contrast, "B", "Seed-paired contrast",
  sprintf("95%% t interval over %d paired seeds", N_SEEDS)
)

save_fig(ladder / contrast + plot_layout(heights = c(1.06, 1), guides = "collect") &
           theme(legend.position = "bottom", legend.box = "horizontal"),
         "fig1_wind_band", FIGURE_CANVAS_WIDTH_IN, 5.60)
