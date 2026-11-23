#============================================================================
# MORAN'S SCATTERPLOT
#
#   x axis: the value in each area
#   y axis: the average value in that area's neighbours (the "spatial lag")
#   The most unusual points are labelled using your label column.
#   Areas with no neighbours, or with a missing value, are left off the plot
#   and a message says which ones.
#   With row-standardised weights (style = "W"), the slope of the line is
#   Moran's I.
#
#   OPTIONS
#     standardise = TRUE   turns the values into z-scores (the number of
#                          standard deviations above/below the mean) and makes
#                          the plot square. The slope of the line stays the same.
#     annotate    = TRUE   colours in the four quadrants and names them
#                          (High-High, High-Low, Low-High, Low-Low), and labels
#                          the two mean lines.
#     palette     = 1      the colours used by annotate. These are the same
#                          palettes as lisa_map(): a palette number, a palette
#                          name, or colours of your own in the order
#                          High-High, High-Low, Low-High, Low-Low.
#============================================================================
#' Moran's scatterplot
#'
#' Plots the value in each area (x axis) against the average value in that
#' area's neighbours, the "spatial lag" (y axis). The most unusual points are
#' labelled using your label column.
#'
#' @param data Your sf object, for example `Census.sf`.
#' @param variable The NAME of the column you want to study, in "quotes".
#' @param label The NAME of a column that labels each row, in "quotes" (for example `"GEOID"`).
#' @param neighbours The neighbours you made, for example with `sfdep::st_contiguity()`.
#' @param weights The weights you made with `sfdep::st_weights()`.
#' @param title A description of your variable, used on the plots.
#' @param standardise `TRUE` turns the values into z-scores (the number of standard
#'   deviations above/below the mean) and makes the plot square. The slope of the line
#'   stays the same. The default is `FALSE`.
#' @param annotate `TRUE` colours in the four quadrants, names them (High-High, High-Low,
#'   Low-High, Low-Low) and labels the two mean lines. The default is `FALSE`.
#' @param palette The colours used by `annotate`. These are the same palettes as
#'   [lisa_map()]: a palette number (1 to 6), a palette name, or colours of your own in
#'   the order High-High, High-Low, Low-High, Low-Low.
#'
#' @details Areas with no neighbours, or with a missing value, are left off the plot, and a
#'   message says which ones. With row-standardised weights (`style = "W"`), the slope of
#'   the line is Moran's I for the areas on the plot.
#'
#' @return The plot is drawn. The numbers behind it are returned invisibly: the x value,
#'   the spatial lag (`wx`), whether the point was labelled (`is_inf`) and the label.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' moran_scatterplot(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                   title = "Births in 1974")
#' moran_scatterplot(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                   title = "Births in 1974", standardise = TRUE, annotate = TRUE)
#' @export
moran_scatterplot <- function(data, variable, label, neighbours, weights, title = variable,
                              standardise = FALSE, annotate = FALSE, palette = 1) {
  
  draw_scatterplot(data, variable, label, neighbours, weights, title,
                   standardise, annotate, palette, lisa = NULL)
}


#------------------------------------------
# LISA SCATTERPLOT
#
#   The same plot as moran_scatterplot(), with every point coloured by its
#   LISA group (High-High, High-Low, Low-High, Low-Low). Points that are not
#   significant at your chosen p-value are grey.
#
#   The groups and p-values are the same ones lisa_map() uses, so with the
#   same significance, nsim and seed, the plot matches the map.
#   The seed that was used is always written under the plot.
#------------------------------------------
#' LISA scatterplot
#'
#' The same plot as [moran_scatterplot()], with every point coloured by its LISA
#' group (High-High, High-Low, Low-High, Low-Low). Points that are not significant
#' at your chosen p-value are grey. The groups and p-values are the ones
#' [lisa_map()] uses, so with the same `significance`, `nsim` and `seed` the plot
#' matches the map. The seed that was used is always written under the plot.
#'
#' @param data Your sf object, for example `Census.sf`.
#' @param variable The NAME of the column you want to study, in "quotes".
#' @param label The NAME of a column that labels each row, in "quotes" (for example `"GEOID"`).
#' @param neighbours The neighbours you made, for example with `sfdep::st_contiguity()`.
#' @param weights The weights you made with `sfdep::st_weights()`.
#' @param title A description of your variable, used on the plots.
#' @param standardise `TRUE` turns the values into z-scores and makes the plot square.
#' @param annotate `TRUE` colours in the four quadrants, names them and labels the two
#'   mean lines.
#' @param palette The colours for the four LISA groups. These are the same palettes as
#'   [lisa_map()]: a palette number (1 to 6), a palette name, or colours of your own in
#'   the order High-High, High-Low, Low-High, Low-Low.
#' @param significance The p-value cut-off. The default is 0.05.
#' @param nsim How many random shuffles are used for the p-values. The default is 499.
#' @param seed The random seed. Leave it out and R picks a new one each time.
#'
#' @details A point's LISA group can differ from the quadrant it sits in when it is very
#'   close to one of the dashed lines, because the LISA calculation measures the
#'   neighbours' average against the overall mean and the dashed line shows the mean of
#'   the neighbours' averages.
#'
#' @return The plot is drawn. The numbers behind it are returned invisibly, with a
#'   `lisa_group` column.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' lisa_scatterplot(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                  title = "Births in 1974", seed = 364)
#' @export
lisa_scatterplot <- function(data, variable, label, neighbours, weights, title = variable,
                             standardise = FALSE, annotate = FALSE, palette = 1,
                             significance = 0.05, nsim = 499, seed = NULL) {
  
  result <- make_lisa(data, variable, label, neighbours, weights, title, data_source = NULL,
                      mode = "plot", significance, nsim, global_stats = FALSE,
                      colours = if (is.character(palette) && length(palette) == 4) c(palette, "white") else palette,
                      basemap = FALSE, seed = seed)
  
  lisa <- list(groups       = result$data$Quadrant_sig,
               significance = significance,
               nsim         = nsim,
               seed         = result$seed)
  
  draw_scatterplot(data, variable, label, neighbours, weights, title,
                   standardise, annotate, palette, lisa = lisa)
}


#------------------------------------------
# The drawing code behind moran_scatterplot() and lisa_scatterplot().
#   lisa = NULL   plain points
#   lisa = a list with the LISA group of every area: coloured points
#------------------------------------------
draw_scatterplot <- function(data, variable, label, neighbours, weights, title,
                             standardise, annotate, palette, lisa = NULL) {
  
  check_autocorrelation_inputs(data, variable, label, neighbours, weights)
  
  if (!isTRUE(standardise) && !isFALSE(standardise)) {
    stop("'standardise' needs to be TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(annotate) && !isFALSE(annotate)) {
    stop("'annotate' needs to be TRUE or FALSE", call. = FALSE)
  }
  
  #------------------------------------------
  # The colours for the four quadrants (only used if annotate = TRUE).
  # 4 colours of your own are allowed, as well as the 5 that lisa_map() takes
  #------------------------------------------
  quadrants <- c("High-High", "High-Low", "Low-High", "Low-Low")
  if (is.character(palette) && length(palette) == 4) {
    palette <- c(palette, "white")
  }
  quadrant_colours <- choose_colours(palette, lisa_palettes, c(quadrants, "Not significant"))[1:4]
  
  #------------------------------------------
  # Join the neighbours and weights into the format the calculation needs
  #------------------------------------------
  neighbours_and_weights <- sfdep::recreate_listw(neighbours, weights)
  
  #------------------------------------------
  # Areas with no neighbours, or with a missing value, are left off the plot.
  # (An area with no neighbours has no "average of its neighbours" to plot.)
  # A message says which areas were left off.
  #------------------------------------------
  all_x      <- data[[variable]]
  all_labels <- as.character(data[[label]])
  has_neighbours <- spdep::card(neighbours) > 0
  
  #------------------------------------------
  # x is the value in each area. lag_x is the average in its neighbours.
  # If you ask for z-scores, they use only the areas on the plot.
  #------------------------------------------
  use <- has_neighbours & !is.na(all_x)
  if (standardise) {
    all_x <- (all_x - mean(all_x[use])) / stats::sd(all_x[use])
  }
  all_lag <- spdep::lag.listw(neighbours_and_weights, all_x, zero.policy = TRUE)
  use     <- use & !is.na(all_lag)
  
  if (sum(use) < 3) {
    stop("There are fewer than 3 areas with a value and at least one neighbour, ",
         "so there is nothing to plot.", call. = FALSE)
  }
  if (any(!use)) {
    left_off <- all_labels[!use]
    shown    <- utils::head(left_off, 10)
    message(length(left_off), " area(s) left off the scatterplot ",
            "(no neighbours, or a missing value): ", paste(shown, collapse = ", "),
            if (length(left_off) > 10) ", ..." else "")
  }
  x          <- all_x[use]
  lag_x      <- all_lag[use]
  point_labels <- all_labels[use]
  
  #------------------------------------------
  # For a LISA scatterplot: the colour of every point.
  # Not significant points are grey.
  #------------------------------------------
  point_colours <- NULL
  if (!is.null(lisa)) {
    group_colours <- c(quadrant_colours, "grey80")
    point_colours <- group_colours[as.integer(lisa$groups)][use]
  }
  mean_x   <- mean(x)
  mean_lag <- mean(lag_x)
  
  #------------------------------------------
  # The line of best fit, and the points that pull hardest on it.
  # Those points get a label.
  #------------------------------------------
  best_fit <- stats::lm(lag_x ~ x)
  unusual  <- apply(stats::influence.measures(best_fit)$is.inf, 1, any)
  
  #------------------------------------------
  # Axis titles and limits
  #------------------------------------------
  if (standardise) {
    x_title <- paste0(title, " (z-score)")
    y_title <- paste0("Spatial lag of: ", title, " (z-score)")
    x_limits <- (max(abs(c(x, lag_x))) + 0.5) * c(-1, 1)   # half a standard deviation beyond the data
    y_limits <- x_limits
  } else {
    x_title <- title
    y_title <- paste("Spatial lag of:", title)
    x_limits <- grDevices::extendrange(x, f = 0.06)
    y_limits <- grDevices::extendrange(lag_x, f = 0.06)
  }
  
  #------------------------------------------
  # Plot settings. They are put back to normal when the function finishes.
  #------------------------------------------
  old_settings <- graphics::par(c("mar", "mgp", "pty"))
  on.exit(graphics::par(old_settings))
  
  note_line <- if (annotate && standardise) 4.9 else 3.4      # where the LISA note goes
  margins <- c(4, 4, 1, 1)                            # bottom, left, top, right
  if (annotate)                margins <- c(4, 4, 2, 6.5)
  if (annotate && standardise) margins <- c(5, 4, 2, 6.5)
  if (!is.null(lisa)) margins[1] <- note_line + 1.3
  graphics::par(mar = margins,
                mgp = c(2.2, 0.6, 0))                 # distance of: axis label, tick numbers, axis line
  if (standardise) graphics::par(pty = "s")           # a square plot
  
  #------------------------------------------
  # An empty plot first, so the quadrant colours can go behind the points
  #------------------------------------------
  plot(x, lag_x, type = "n",
       xlim = x_limits, ylim = y_limits, xaxs = "i", yaxs = "i",
       asp  = if (standardise) 1 else NA,
       xlab = list(x_title, cex = 0.9),
       ylab = list(y_title, cex = 0.8),
       cex.axis = 0.9)
  edges <- graphics::par("usr")                       # left, right, bottom, top
  
  if (annotate) {
    pale <- grDevices::adjustcolor(quadrant_colours, alpha.f = 0.13)
    graphics::rect(mean_x,   mean_lag, edges[2], edges[4], col = pale[1], border = NA)  # High-High
    graphics::rect(mean_x,   edges[3], edges[2], mean_lag, col = pale[2], border = NA)  # High-Low
    graphics::rect(edges[1], mean_lag, mean_x,   edges[4], col = pale[3], border = NA)  # Low-High
    graphics::rect(edges[1], edges[3], mean_x,   mean_lag, col = pale[4], border = NA)  # Low-Low
  }
  
  #------------------------------------------
  # The mean lines, the line of best fit and the points
  #------------------------------------------
  graphics::abline(v = mean_x,   lty = 2)
  graphics::abline(h = mean_lag, lty = 2)
  graphics::abline(best_fit)
  if (is.null(lisa)) {
    graphics::points(x[!unusual], lag_x[!unusual], cex = 0.8)
    graphics::points(x[unusual],  lag_x[unusual],  cex = 1.1, pch = 9)
  } else {
    # filled circles, and filled diamonds for the labelled points
    graphics::points(x[!unusual], lag_x[!unusual], cex = 0.9, pch = 21,
                     bg = point_colours[!unusual], col = "grey30", lwd = 0.6)
    graphics::points(x[unusual],  lag_x[unusual],  cex = 1.1, pch = 23,
                     bg = point_colours[unusual],  col = "black",  lwd = 0.8)
  }
  if (any(unusual)) {
    # labels go to the left of the point, or to the right if the point is
    # close to the left edge of the plot
    near_left_edge <- x[unusual] < edges[1] + 0.25 * (edges[2] - edges[1])
    graphics::text(x[unusual], lag_x[unusual], labels = point_labels[unusual],
                   pos = ifelse(near_left_edge, 4, 2), cex = 0.7)
  }
  graphics::box()
  
  #------------------------------------------
  # The quadrant names and the labels on the mean lines
  #------------------------------------------
  if (annotate) {
    gap_x <- 0.025 * (edges[2] - edges[1])
    gap_y <- 0.035 * (edges[4] - edges[3])
    graphics::text(edges[2] - gap_x, edges[4] - gap_y, "HIGH-HIGH", adj = c(1, 0.5),
                   col = quadrant_colours[1], font = 2, cex = 0.7)
    graphics::text(edges[2] - gap_x, edges[3] + gap_y, "HIGH-LOW",  adj = c(1, 0.5),
                   col = quadrant_colours[2], font = 2, cex = 0.7)
    graphics::text(edges[1] + gap_x, edges[4] - gap_y, "LOW-HIGH",  adj = c(0, 0.5),
                   col = quadrant_colours[3], font = 2, cex = 0.7)
    graphics::text(edges[1] + gap_x, edges[3] + gap_y, "LOW-LOW",   adj = c(0, 0.5),
                   col = quadrant_colours[4], font = 2, cex = 0.7)
    
    graphics::mtext("MEAN(X)", side = 3, at = mean_x, line = 0.3, cex = 0.65, font = 2)
    graphics::mtext("MEAN(LAG(X))", side = 4, at = mean_lag, line = 0.4, cex = 0.65, font = 2, las = 1)
    if (standardise) {
      graphics::mtext("(NUMBER OF STANDARD DEVIATIONS ABOVE/BELOW MEAN)",
                      side = 1, line = 3.6, cex = 0.6, font = 2)
    }
  }
  
  #------------------------------------------
  # For a LISA scatterplot: a note under the plot, which includes the seed
  #------------------------------------------
  if (!is.null(lisa)) {
    graphics::mtext(paste0("Point colour = LISA group where p <= ", lisa$significance,
                           ". Grey = not significant. ",
                           lisa$nsim, " random permutations (seed = ", lisa$seed, ")."),
                    side = 1, line = note_line, cex = 0.6, col = "grey30")
  }
  
  #------------------------------------------
  # Give back the numbers behind the plot (they are not printed)
  #------------------------------------------
  result <- data.frame(x = x, wx = lag_x, is_inf = unusual,
                       labels = point_labels)
  if (!is.null(lisa)) {
    result$lisa_group <- lisa$groups[use]
  }
  invisible(result)
}
