#============================================================================
# THE FOUR-MAP SUMMARY FIGURE
#
#   One figure, made for you with the standard settings:
#     A. your data, as a choropleth map (five groups, Jenks natural breaks)
#     B. LISA clusters
#     C. Gi* hot and cold spots (each area counted with its neighbours)
#     D. Gi  hot and cold spots (each area's neighbours only)
#   with one caption underneath, including the global statistics.
#
#         autocorrelation_summary(data       = Census.sf,
#                                 variable   = "percent_income_gt75",
#                                 label      = "GEOID",
#                                 neighbours = NeighbourMatrix,
#                                 weights    = WeightsMatrix,
#                                 title      = "Percentage of people making >$75K")
#
#   Options: data_source, mode, basemap, significance, nsim, seed  (as described above)
#
#   An option for map A:
#     units = "Percentage"   what your numbers are. It is used as the title of
#                            the legend, for example "Percentage (Jenks)".
#                            Leave it out and the legend says "Value (Jenks)".
#
#   Two more options choose what is written in the caption under the figure:
#     explanation  = FALSE   leave out the lines that explain maps A to D
#     global_stats = FALSE   leave out the global Moran's I, Geary's C and General G
#   The random seed (for the LISA map) and your data source are always written.
#
#   In R Markdown, give the chunk a big plot:
#         ```{r, fig.width=9, fig.height=11}
#============================================================================
#' Four maps in one figure: your data, LISA, Gi* and Gi
#'
#' Makes one figure with four maps and one caption underneath:
#' A. your data as a choropleth map (five groups, Jenks natural breaks),
#' B. LISA clusters, C. Gi* hot and cold spots, D. Gi hot and cold spots.
#' In R Markdown, a chunk with `fig.width=9, fig.height=11` gives it enough room.
#'
#' @inheritParams lisa_map
#' @param explanation `FALSE` leaves out the lines of the caption that explain maps A to D.
#' @param global_stats `FALSE` leaves out the global Moran's I, Geary's C and General G.
#' @param units What your numbers are, for example `"Percentage"`. It is used as the
#'   title of the legend on map A.
#'
#' @return A figure. Type its name to see it.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' Summary <- autocorrelation_summary(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                                    title = "Births in 1974", units = "Births", seed = 364)
#' @export
autocorrelation_summary <- function(data, variable, label, neighbours, weights, title = variable,
                                    data_source = NULL, mode = "plot", basemap = FALSE,
                                    significance = 0.05, nsim = 499, seed = NULL,
                                    explanation = TRUE, global_stats = TRUE, units = "Value") {
  
  if (!is.character(units) || length(units) != 1 || is.na(units)) {
    stop("'units' needs to be one piece of text in quotes, for example units = \"Percentage\"", call. = FALSE)
  }
  
  for (option in list(explanation, global_stats)) {
    if (!is.logical(option) || length(option) != 1 || is.na(option)) {
      stop("'explanation' and 'global_stats' each need to be TRUE or FALSE", call. = FALSE)
    }
  }
  
  data_source <- tidy_data_source(data_source)
  
  # One seed for the whole figure. Only the LISA map (B) uses random numbers
  seed <- use_seed(seed)
  
  lisa      <- make_lisa(data, variable, label, neighbours, weights, title, data_source,
                         mode, significance, nsim, global_stats = global_stats, colours = 1, basemap, seed)
  getisstar <- make_getis(star = TRUE,  data, variable, label, neighbours, weights, title,
                          data_source, mode, colours = 1, basemap)
  getis     <- make_getis(star = FALSE, data, variable, label, neighbours, weights, title,
                          data_source, mode, colours = 1, basemap)
  
  #------------------------------------------
  # The first map shows the data itself, cut into 5 groups (Jenks natural breaks)
  #------------------------------------------
  choropleth <- list(data         = data,
                     fill_column  = variable,
                     colours      = "brewer.greens",
                     numbers      = TRUE,
                     legend_title = paste(units, "(Jenks)"),
                     map_title    = title,
                     notes        = list(method = "A: Choropleth of the raw data (Jenks).",
                                         global = NULL,
                                         shared = NULL,
                                         source = data_source),
                     label        = label,
                     basemap      = basemap,
                     interactive  = (mode == "view"))
  
  recipes <- list(choropleth, lisa$recipe, getisstar$recipe, getis$recipe)
  
  recipes[[2]]$notes$method <- paste("B:", recipes[[2]]$notes$method)
  recipes[[3]]$notes$method <- paste("C:", recipes[[3]]$notes$method)
  recipes[[4]]$notes$method <- paste("D:", recipes[[4]]$notes$method)
  
  new_autocorrelation_map(recipes,
                          layout = list(ncol         = 2,
                                        explanation  = explanation,
                                        figure_title = title,
                                        panel_titles = c("A. Raw data",
                                                         "B. LISA clusters",
                                                         "C. Gi* hot and cold spots",
                                                         "D. Gi hot and cold spots")))
}
