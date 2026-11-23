#------------------------------------------
# Build one map from its "recipe" (the list of things the map needs):
#   data          the sf data, with the column to colour by already added
#   fill_column   the column to colour by
#   colours       one colour per group, in the same order as the groups
#                 (or the name of a tmap palette, for a map of numbers)
#   numbers       TRUE if fill_column holds numbers, to be cut into 5 groups (Jenks)
#   legend_title  the heading of the legend
#   map_title     the title above the map
#   notes         the small print underneath the map
#   label         the column used to name each area
#   basemap       FALSE, TRUE or the name of a background map
#   interactive   TRUE/FALSE, make a zoom-and-click map?
#
# panel_width is how wide the plot is, in inches. The title and notes
# are split into lines that fit inside that width.
# in_panel = TRUE means the map is one of several in a figure: it is then
# drawn without its notes, because the figure has one shared caption.
# scale_bar = FALSE leaves the scale bar off (used when the plot is very small).
# legend_below = TRUE puts the legend under the map (used for wide study areas).
#------------------------------------------
build_autocorrelation_map <- function(recipe, panel_width = 7, in_panel = FALSE, panel_title = NULL,
                                      scale_bar = TRUE, legend_below = FALSE) {
  
  data <- recipe$data
  
  if (recipe$numbers) {
    # Jenks natural breaks: 5 groups, with the breaks placed in the gaps in the data
    fill_scale <- tmap::tm_scale_intervals(n = 5, style = "jenks", values = recipe$colours)
  } else {
    fill_scale <- tmap::tm_scale_categorical(values = recipe$colours)
  }
  
  #------------------------------------------
  # INTERACTIVE VERSION: a simple map to zoom and click on.
  # The colours are half see-through, so the background map shows
  #------------------------------------------
  if (recipe$interactive) {
    
    # A view map has no caption, so the seed goes under the legend title
    legend_title <- recipe$legend_title
    if (!is.null(recipe$seed)) {
      legend_title <- paste0(legend_title, "\nRandom seed: ", recipe$seed)
    }
    
    map <- tmap::tm_shape(data) +
      tmap::tm_polygons(fill        = recipe$fill_column,
                        fill_alpha  = 0.5,
                        col         = "grey35",
                        lwd         = 0.3,
                        id          = recipe$label,
                        fill.scale  = fill_scale,
                        fill.legend = tmap::tm_legend(title = legend_title))
    
    if (is.character(recipe$basemap)) {
      map <- basemap_layer(recipe$basemap) + map
    }
    return(map)
  }
  
  #------------------------------------------
  # STATIC VERSION
  #
  # The outline of the whole study area, and the same outline with any
  # gaps filled in. Gaps are areas that were removed from the data
  # (for example, tracts with no people), so we shade them as "No data"
  #------------------------------------------
  outline <- sf::st_union(sf::st_geometry(data))
  pieces  <- suppressWarnings(sf::st_cast(outline, "POLYGON"))
  filled  <- sf::st_sfc(lapply(pieces, function(piece) sf::st_polygon(piece[1])),
                        crs = sf::st_crs(data))
  
  gap_area <- sum(as.numeric(sf::st_area(filled))) - sum(as.numeric(sf::st_area(outline)))
  has_gaps <- gap_area > 0.00001 * sum(as.numeric(sf::st_area(outline)))
  
  # The shape of the gaps themselves (the filled-in outline minus the real outline)
  if (has_gaps) {
    gaps <- sf::st_difference(sf::st_union(filled), outline)
  }
  
  #------------------------------------------
  # How many letters fit on one line?
  # The legend sits on the right, and the title and notes get the space to its left.
  # (0.062, 0.082 and 0.05 are the widths of one letter, in inches, at each text size)
  #------------------------------------------
  legend_width <- legend_width_inches(recipe)
  text_room    <- panel_width - legend_width - 0.3
  if (legend_below) {
    text_room <- panel_width - 0.3
  }
  
  title_letters <- max(20, floor(text_room / 0.082))
  note_letters  <- max(30, floor(text_room / 0.05))
  
  #------------------------------------------
  # The colours are always solid on a static map, even over a background map
  #------------------------------------------
  has_basemap <- !identical(recipe$basemap, FALSE)
  fill_alpha  <- 1
  
  legend_place <- tmap::tm_pos_out("right", "center")
  if (legend_below) {
    legend_place <- tmap::tm_pos_out("center", "bottom", pos.h = "left")
  }
  
  #------------------------------------------
  # Layers, from the bottom up:
  #   the coloured areas, the grey "No data" gaps, the black outline
  #------------------------------------------
  map <- tmap::tm_shape(data) +
    tmap::tm_polygons(fill        = recipe$fill_column,
                      fill_alpha  = fill_alpha,
                      col         = "grey35",
                      lwd         = 0.3,
                      id          = recipe$label,
                      fill.scale  = fill_scale,
                      fill.legend = tmap::tm_legend(title      = recipe$legend_title,
                                                    position   = legend_place,
                                                    frame      = FALSE,
                                                    title.size = 0.9,
                                                    text.size  = 0.75))
  
  if (has_gaps) {
    map <- map +
      tmap::tm_shape(gaps) +
      tmap::tm_polygons(fill = "grey75", col = "grey35", lwd = 0.3) +
      tmap::tm_add_legend(type      = "polygons",
                          labels    = "No data (excluded)",
                          fill      = "grey75",
                          col       = "grey35",
                          lwd       = 0.3,
                          position  = legend_place,
                          frame     = FALSE,
                          text.size = 0.75)
  }
  
  map <- map +
    tmap::tm_shape(filled) +
    tmap::tm_borders(col = "black", lwd = 1)
  
  #------------------------------------------
  # Title, scale bar and notes.
  # component.autoscale = FALSE stops tmap shrinking the text to odd sizes
  # padding = 0.6 gives the notes enough room (without it, the top line can be cut)
  #------------------------------------------
  if (is.null(panel_title)) {
    panel_title <- recipe$map_title
  }
  
  map <- map +
    tmap::tm_layout(frame = FALSE) +
    tmap::tm_options(component.autoscale = FALSE)
  
  # In a figure with several maps, the panel title is written by draw_map_panels()
  if (!in_panel) {
    map <- map + tmap::tm_title(wrap_text(panel_title, width = title_letters), size = 1)
  }
  
  if (scale_bar) {
    map <- map +
      tmap::tm_scalebar(position = tmap::tm_pos_out("center", "bottom"), text.size = 0.6)
  }
  
  if (!in_panel) {
    map <- map +
      # The blank lines at the end stop tmap cutting off the bottom line of the notes
      tmap::tm_credits(paste0(wrap_text(recipe_note_lines(recipe), width = note_letters), "\n\n\u00a0"),
                       size = 0.6, padding = 0.6,
                       position = tmap::tm_pos_out("center", "bottom"))
  }
  
  if (has_basemap) {
    map <- basemap_layer(recipe$basemap) + map
  }
  
  map
}


#------------------------------------------
# Draw several static maps in one figure, with one shared caption underneath.
#   layout$ncol          how many maps in each row
#   layout$figure_title  a title for the whole figure (or NULL)
#   layout$panel_titles  a short title for each map (or NULL to use each map's own title)
#------------------------------------------
draw_map_panels <- function(recipes, layout) {
  
  figure_size   <- grDevices::dev.size("in")
  figure_width  <- figure_size[1]
  figure_height <- figure_size[2]
  
  n_columns <- layout$ncol
  n_rows    <- ceiling(length(recipes) / n_columns)
  
  #------------------------------------------
  # The shared caption, and how much height it and the figure title need
  #------------------------------------------
  caption        <- wrap_text(combined_caption(recipes, explanation = !identical(layout$explanation, FALSE)),
                              width = floor((figure_width - 0.6) / 0.06))
  caption_height <- 0.3 + 0.155 * length(strsplit(caption, "\n")[[1]])
  
  title_height <- 0
  if (!is.null(layout$figure_title)) {
    figure_title <- wrap_text(layout$figure_title, width = floor((figure_width - 0.6) / 0.105))
    title_height <- 0.25 + 0.27 * length(strsplit(figure_title, "\n")[[1]])
  }
  
  #------------------------------------------
  # Make each panel the same shape as the study area, so that there is no
  # empty space between the maps.
  #   A panel holds: the map, with the legend to its right, and a title above.
  #   (There is no scale bar in a figure with several maps.)
  #   The map is made as big as it can be while everything still fits
  #   in the width AND the height of the plot.
  #------------------------------------------
  box          <- sf::st_bbox(recipes[[1]]$data)
  shape        <- as.numeric(box["xmax"] - box["xmin"]) / as.numeric(box["ymax"] - box["ymin"])   # width / height
  
  legend_width  <- max(sapply(recipes, legend_width_inches)) + 0.25
  legend_height <- max(sapply(recipes, legend_height_inches))
  title_strip   <- 0.3                                    # the panel title
  extra_height  <- title_strip + 0.25                     # panel title + a small gap below the map
  left_margin   <- 0.3                                    # the same left edge as the title and caption
  right_margin  <- 0.15
  room_width    <- (figure_width - left_margin - right_margin) / n_columns
  room_height   <- (figure_height - caption_height - title_height) / n_rows
  
  # Option 1: legend to the RIGHT of each map
  right_map_width <- min(room_width - legend_width,                       # limited by the plot's width
                         (room_height - extra_height) * shape)            # limited by the plot's height
  
  # Option 2: legend BELOW each map
  below_map_width <- min(room_width - 0.2,
                         (room_height - extra_height - legend_height) * shape)
  
  # Use whichever gives the bigger map. Wide study areas get the legend below.
  legend_below <- below_map_width > right_map_width
  
  if (legend_below) {
    map_width    <- below_map_width
    map_height   <- map_width / shape
    panel_width  <- max(map_width + 0.2, legend_width)
    panel_height <- map_height + extra_height + legend_height
  } else {
    map_width    <- right_map_width
    map_height   <- map_width / shape
    panel_width  <- map_width + legend_width
    # (a short, wide map still needs a panel tall enough for its legend)
    panel_height <- max(map_height, legend_height) + extra_height
  }
  
  too_small   <- map_width < 0.8 || map_height < 0.8
  bigger_plot <- "In R Markdown, use chunk options like fig.width=9, fig.height=11"
  # If the plot is too small, say so (in the plot itself) and stop here
  if (too_small) {
    message("This figure needs a bigger plot. ", bigger_plot)
    grid::grid.newpage()
    grid::grid.text(wrap_text(paste("This figure needs a bigger plot.", bigger_plot),
                              width = max(20, floor(figure_width / 0.11))),
                    gp = grid::gpar(fontsize = 11))
    return(invisible(NULL))
  }
  
  #------------------------------------------
  # Lay out the page: title row, one row per row of maps, caption row
  #------------------------------------------
  grid::grid.newpage()
  # The panels are packed together at the top left, starting at the left
  # margin so that the maps line up with the title and the caption.
  # Any space left over goes to the right of the maps and below the caption.
  grid::pushViewport(grid::viewport(
    x = grid::unit(left_margin, "inches"), y = 1, just = c("left", "top"),
    width  = grid::unit(panel_width * n_columns, "inches"),
    height = grid::unit(title_height + panel_height * n_rows + caption_height, "inches"),
    layout = grid::grid.layout(
      nrow    = n_rows + 2,
      ncol    = n_columns,
      widths  = grid::unit(rep(panel_width, n_columns), "inches"),
      heights = grid::unit(c(title_height, rep(panel_height, n_rows), caption_height), "inches"))))
  
  if (!is.null(layout$figure_title)) {
    grid::grid.text(figure_title,
                    x = grid::unit(0, "inches"), y = grid::unit(0.5, "npc"),
                    just = c("left", "centre"),
                    gp = grid::gpar(fontsize = 15, lineheight = 1.05),
                    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = seq_len(n_columns)))
  }
  
  for (i in seq_along(recipes)) {
    
    panel_row <- 1 + ceiling(i / n_columns)
    panel_col <- 1 + (i - 1) %% n_columns
    
    # The panel title, in a strip along the top of the panel
    panel_title <- layout$panel_titles[i]
    if (is.null(panel_title) || is.na(panel_title)) {
      panel_title <- recipes[[i]]$map_title
    }
    grid::grid.text(panel_title,
                    x = grid::unit(0, "inches"),
                    y = grid::unit(1, "npc") - grid::unit(title_strip / 2, "inches"),
                    just = c("left", "centre"),
                    gp = grid::gpar(fontsize = 11),
                    vp = grid::viewport(layout.pos.row = panel_row, layout.pos.col = panel_col))
    
    # The map and its legend fill the rest of the panel. There is no scale bar.
    # outer.margins = 0 stops tmap adding its own space around the map,
    # so the map starts exactly at the left edge of the panel.
    draw_one <- function() {
      map <- build_autocorrelation_map(recipes[[i]],
                                       panel_width  = panel_width,
                                       in_panel     = TRUE,
                                       scale_bar    = FALSE,
                                       legend_below = legend_below) +
        tmap::tm_layout(outer.margins = c(0, 0, 0, 0))
      grid::pushViewport(grid::viewport(layout.pos.row = panel_row, layout.pos.col = panel_col))
      on.exit(grid::popViewport())
      # (the 0.25 inches left empty at the bottom is the gap before the next row)
      print(map, vp = grid::viewport(x = 0, y = grid::unit(0.25, "inches"), just = c("left", "bottom"),
                                     width  = grid::unit(1, "npc"),
                                     height = grid::unit(1, "npc") - grid::unit(title_strip + 0.25, "inches")))
      TRUE
    }
    drawn <- tryCatch(draw_one(), error = function(e) FALSE)
    
    if (!drawn) {
      stop("The plot is too small to draw this figure. ", bigger_plot, call. = FALSE)
    }
  }
  
  grid::grid.text(caption,
                  x = grid::unit(0, "inches"),
                  y = grid::unit(1, "npc") - grid::unit(0.1, "inches"),
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 8.5, lineheight = 1.15),
                  vp = grid::viewport(layout.pos.row = n_rows + 2, layout.pos.col = seq_len(n_columns)))
  
  grid::popViewport()
  invisible(NULL)
}
