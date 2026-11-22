#============================================================================
# MORAN'S SCATTERPLOT
#
#   x axis: the value in each area
#   y axis: the average value in that area's neighbours (the "spatial lag")
#   The most unusual points are labelled using your label column
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
#'
#' @return The plot is drawn. The numbers behind it are returned invisibly.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' moran_scatterplot(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                   title = "Births in 1974")
#' @export
moran_scatterplot <- function(data, variable, label, neighbours, weights, title = variable) {
  
  check_autocorrelation_inputs(data, variable, label, neighbours, weights)
  
  #------------------------------------------
  # Join the neighbours and weights into the format the plot needs
  #------------------------------------------
  neighbours_and_weights <- sfdep::recreate_listw(neighbours, weights)
  
  #------------------------------------------
  # Make the plot
  #------------------------------------------
  result <- spdep::moran.plot(x      = data[[variable]],
                              listw  = neighbours_and_weights,
                              labels = as.character(data[[label]]),
                              xlab   = list(title, cex = 0.9),
                              ylab   = list(paste("Spatial lag of:", title), cex = 0.8),
                              cex = 0.8, cex.axis = 0.9,
                              zero.policy = TRUE,
                              mgp = c(1.8, 0.6, 0))   # distance of: axis label, tick numbers, axis line
  
  invisible(result)
}
