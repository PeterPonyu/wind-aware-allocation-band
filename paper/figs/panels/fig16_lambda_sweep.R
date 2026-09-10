# Figure 16. Pre-declared assignment-penalty sweep: aware-arm episodes whose
# initial (t = 0) and used (any call) assignment differs from Euclidean, against
# the penalty weight lambda.  The x axis is pseudo-logarithmic so lambda = 0 has
# a place on the same axis as the two-decade grid; the bound weight is marked.

LAMBDA_LONG <- rbind(
  data.frame(lambda = LAMBDA_TABLE$lambda, count = LAMBDA_TABLE$initial,
             endpoint = "Initial assignment (t = 0)", stringsAsFactors = FALSE),
  data.frame(lambda = LAMBDA_TABLE$lambda, count = LAMBDA_TABLE$used,
             endpoint = "Used assignment (any call)", stringsAsFactors = FALSE)
)
LAMBDA_LONG$endpoint <- factor(LAMBDA_LONG$endpoint,
                               levels = c("Initial assignment (t = 0)",
                                          "Used assignment (any call)"))
lambda_axis <- scales::pseudo_log_trans(sigma = 0.02, base = 10)

# Both series are counts on the wind-aware arm, so neither takes an arm
# colour; they draw from the paper-wide secondary palette.
endpoint_colours <- secondary_colours(levels(LAMBDA_LONG$endpoint))
endpoint_shapes <- secondary_shapes(levels(LAMBDA_LONG$endpoint))

p <- ggplot(LAMBDA_LONG, aes(lambda, count, colour = endpoint, shape = endpoint)) +
  geom_vline(xintercept = LAMBDA_REF, colour = "grey55", linewidth = 0.3,
             linetype = "22") +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.0, fill = "white", stroke = 0.55) +
  scale_colour_manual(values = endpoint_colours, name = NULL) +
  scale_shape_manual(values = endpoint_shapes, name = NULL) +
  scale_x_continuous(name = "Penalty weight \u03bb (m/N), pseudo-log axis",
                     transform = lambda_axis,
                     breaks = LAMBDA_TABLE$lambda,
                     labels = format(LAMBDA_TABLE$lambda, drop0trailing = TRUE,
                                     trim = TRUE)) +
  # Wrapped onto two lines so the title fits the height of a single-column
  # panel.
  scale_y_continuous(name = sprintf("Aware episodes differing\nfrom Euclidean (of %d)",
                                    as.integer(A7_TOTALS$aware_arm_episodes)),
                     limits = c(-2, 90), breaks = seq(0, 80, 20)) +
  rtx_theme() +
  # Counts are zero or small until the largest weights, so the upper-left
  # quarter of the panel is empty and carries the key.
  theme(legend.position = "inside",
        legend.position.inside = c(0.03, 0.97),
        legend.justification = c(0, 1),
        legend.margin = margin(1, 3, 1, 3),
        legend.key.size = unit(0.32, "cm"),
        legend.key.spacing.y = unit(0, "pt"))

save_fig(p, "fig16_lambda_sweep", FIGURE_COLUMN_WIDTH_IN, 2.90)
