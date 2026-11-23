#============================================================================
# MORAN'S SCATTERPLOT
#
#   x axis: the value in each area
#   y axis: the average value in that area's neighbours (the "spatial lag")
#   The most unusual points are labelled using your label column.
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
#' @details With row-standardised weights (`style = "W"`) and at least one neighbour for
#'   every area, the slope of the line is Moran's I.
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
  # x is the value in each area. lag_x is the average in its neighbours.
  #------------------------------------------
  x <- data[[variable]]
  if (standardise) {
    x <- (x - mean(x)) / stats::sd(x)
  }
  lag_x  <- spdep::lag.listw(neighbours_and_weights, x, zero.policy = TRUE)
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
    x_limits <- max(abs(c(x, lag_x))) * c(-1.08, 1.08)
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
  
  margins <- c(4, 3.5, 1, 1)                          # bottom, left, top, right
  if (annotate)                margins <- c(4, 3.5, 2, 6.5)
  if (annotate && standardise) margins <- c(5, 3.5, 2, 6.5)
  graphics::par(mar = margins,
                mgp = c(1.8, 0.6, 0))                 # distance of: axis label, tick numbers, axis line
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
  graphics::points(x[!unusual], lag_x[!unusual], cex = 0.8)
  graphics::points(x[unusual],  lag_x[unusual],  cex = 1.1, pch = 9)
  if (any(unusual)) {
    graphics::text(x[unusual], lag_x[unusual], labels = as.character(data[[label]])[unusual],
                   pos = 2, cex = 0.7, xpd = NA)
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
                      side = 1, line = 3.2, cex = 0.6, font = 2)
    }
  }
  
  #------------------------------------------
  # Give back the numbers behind the plot (they are not printed)
  #------------------------------------------
  result <- data.frame(x = x, wx = lag_x, is_inf = unusual,
                       labels = as.character(data[[label]]))
  invisible(result)
}
