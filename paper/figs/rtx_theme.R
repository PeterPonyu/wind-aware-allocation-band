# Shared figure theme. R-first by workspace convention; do not add a
# matplotlib path to this tree.
#
# Keep one explicitly installed family for every glyph in the exported PDF.
# Arial is the manuscript's approved visual family.  The regular and bold
# files are resolved and checked before any panel is built; Cairo is then
# allowed to embed that family rather than asking fontconfig to substitute a
# second family for plot annotations.
FIGURE_FONT_FAMILY <- "Arial"

# Keep the visual hierarchy in one place.  The values are deliberately shared
# by every panel so a composed patchwork figure does not mix tiny inherited
# labels with larger ad-hoc annotations.  ggplot2 sizes are in points for
# theme elements and millimetres for geom text.
#
# These are printed sizes, not nominal ones.  Every canvas is emitted at
# FIGURE_CANVAS_WIDTH_IN and included at \linewidth, so the manuscript scales
# each figure by 1.0 and a value here reaches the page unchanged.  A panel that
# emits a different canvas width would silently rescale its own type and
# reintroduce the size drift this constant exists to prevent.
FIGURE_CANVAS_WIDTH_IN <- 6.5
FIGURE_BASE_SIZE <- 9.8
FIGURE_AXIS_TITLE_SIZE <- 8.9
FIGURE_AXIS_TEXT_SIZE <- 8.0
FIGURE_LEGEND_TITLE_SIZE <- 8.2
FIGURE_LEGEND_TEXT_SIZE <- 7.8
FIGURE_STRIP_TEXT_SIZE <- 8.4
FIGURE_TITLE_SIZE <- 10.7
FIGURE_SUBTITLE_SIZE <- 8.2
FIGURE_ANNOTATION_SIZE <- 2.40
FIGURE_CELL_SIZE <- 2.25
FIGURE_PANEL_LABEL_SIZE <- 11.6

# Clearance between the panel label and the panel's top-left spine corner.  A
# label flush on the corner reads as part of the frame at print size, so the
# glyph is held off the corner in both directions.  The title is centred
# independently below it; the tag is never used as a title prefix.
FIGURE_PANEL_LABEL_GAP <- 4.5
FIGURE_PANEL_LABEL_LIFT <- 0.0

# Wrap width for panel subtitles, in characters.  Two bounds meet here: wrap too
# late and a subtitle runs under the neighbouring panel, wrap too early and the
# break strands a one-word line under a full one.  Measured against the widest
# subtitle in the set, every value from 42 up clears the first bound and every
# value from 42 down avoids the second, so 42 is the setting that satisfies both
# with the most room to spare (worst observed clearance 27 pt).
FIGURE_SUBTITLE_WRAP <- 42

# Resolve the family before any panel is built.  A `family` string alone is not
# enough: on a different host Cairo can silently substitute a fallback when a
# regular or bold face is absent.  The generator therefore fails closed if the
# two Arial faces or the Unicode glyphs used by the annotations cannot be
# resolved.
validate_figure_font <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("systemfonts is required to resolve the embedded figure font")
  }
  matches <- suppressWarnings(systemfonts::match_fonts(
    family = FIGURE_FONT_FAMILY,
    weight = c("normal", "bold")
  ))
  if (nrow(matches) != 2L || any(!file.exists(matches$path))) {
    stop("figure font family must provide installed regular and bold faces: ",
         FIGURE_FONT_FAMILY)
  }
  font_info <- systemfonts::font_info(path = matches$path)
  if (nrow(font_info) != 2L || any(font_info$family != FIGURE_FONT_FAMILY) ||
      !identical(as.logical(font_info$bold), c(FALSE, TRUE)) ||
      !identical(as.character(font_info$name), c("ArialMT", "Arial-BoldMT"))) {
    stop("figure font resolver returned a fallback or unexpected Arial face")
  }
  glyphs <- systemfonts::glyph_info(
    c("σ", "×", "±", "°", "→"), family = FIGURE_FONT_FAMILY,
    weight = "normal"
  )
  if (nrow(glyphs) != 5L || any(is.na(glyphs$index)) || any(glyphs$width <= 0)) {
    stop("figure font lacks one or more required Unicode glyphs: ",
         FIGURE_FONT_FAMILY)
  }
  invisible(matches$path)
}

validate_figure_font()

# ggplot2's newer defaults inherit `family` from the theme, but older releases
# leave text geoms at the host default.  Set both common text geoms explicitly so
# annotations that do not repeat `family =` cannot reintroduce a fallback.
ggplot2::update_geom_defaults("text", list(family = FIGURE_FONT_FAMILY))
ggplot2::update_geom_defaults("label", list(family = FIGURE_FONT_FAMILY))

rtx_theme <- function(base_size = FIGURE_BASE_SIZE) {
  ggplot2::theme_bw(base_size = base_size, base_family = FIGURE_FONT_FAMILY) +
    ggplot2::theme(
      text = ggplot2::element_text(family = FIGURE_FONT_FAMILY, colour = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(linewidth = 0.3),
      axis.title = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                         size = FIGURE_AXIS_TITLE_SIZE),
      axis.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                        size = FIGURE_AXIS_TEXT_SIZE),
      legend.title = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                           size = FIGURE_LEGEND_TITLE_SIZE),
      legend.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                           size = FIGURE_LEGEND_TEXT_SIZE),
      strip.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                         size = FIGURE_STRIP_TEXT_SIZE),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 7.5, r = 6, b = 6, l = 8)
  )
}

# Apply a panel label independently from the title/subtitle.  Label and title
# are both anchored to the plot viewport, never the panel viewport, and that
# shared reference is the whole point: `coord_fixed` letterboxes the panel
# inside its allotted cell, so a panel-anchored tag is measured from the inset
# drawing rectangle while a panel-anchored title is measured from the cell.  On
# a dial or a schematic -- fixed aspect, no axis furniture to push the spine
# inward -- the two references diverge far enough that the tag lands on top of
# the subtitle.  Anchoring both to the plot keeps them in step whatever the
# coord, scale or facet does.
#
# The tag is deliberately independent from the title block.  Keeping the tag
# at the upper-left and centring the title/subtitle makes the panel identity
# readable without baking the letter into the title text.
panel_label <- function(plot, label, title, subtitle = NULL) {
  # Subtitles remain script arguments for auditability, but their quantitative
  # content belongs in the self-contained caption rather than inside the plot.
  # This leaves the data region and the shared bottom legend uncluttered.
  plot +
    ggplot2::labs(title = title, tag = label) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        family = FIGURE_FONT_FAMILY, hjust = 0.5,
        size = FIGURE_TITLE_SIZE, face = "plain",
        margin = ggplot2::margin(b = 2.5, l = 0)
      ),
      plot.subtitle = ggplot2::element_blank(),
      plot.tag.location = "plot",
      plot.tag.position = c(0, 1),
      plot.tag = ggplot2::element_text(
        family = FIGURE_FONT_FAMILY, hjust = 0, vjust = 1,
        size = FIGURE_PANEL_LABEL_SIZE, face = "bold",
        margin = ggplot2::margin(l = FIGURE_PANEL_LABEL_GAP,
                                 b = FIGURE_PANEL_LABEL_LIFT)
      ),
      # Keep enough outer room for the label's ascender and leftward extent.
      plot.margin = ggplot2::margin(t = 7.5, r = 6, b = 6, l = 8)
    )
}
