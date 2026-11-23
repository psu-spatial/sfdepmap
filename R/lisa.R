#============================================================================
# LISA MAP (Local Moran's I)
#
#   Each area is put into a group:
#     High-High = high value, surrounded by high values
#     High-Low  = high value, surrounded by low values
#     Low-High  = low value,  surrounded by high values
#     Low-Low   = low value,  surrounded by low values
#   Areas that are not significant get the last colour (white, in most palettes).
#
#   Extra options, on top of the shared ones at the top of this file:
#     significance = 0.01    the p-value cut-off (the default is 0.05)
#     nsim         = 999     how many random shuffles are used for the p-values
#     global_stats = TRUE    add the global Moran's I, Geary's C and Getis-Ord
#                            General G to the notes under the map
#     seed         = 364     the random seed. Leave it out and R picks a new one
#                            each time. Give a number to get the same map every time.
#
#   COLOURS. Pick a built-in palette by its number or its name,
#   for example  palette = 3  or  palette = "bright":
#
#                        High-High  High-Low   Low-High   Low-Low    Not significant
#     1 "red_blue"       #ca0020    #f4a582    #92c5de    #0571b0    white   (the default)
#     2 "pastel"         #e78ac3    #fc8d62    #66c2a5    #8da0cb    white
#     3 "bright"         #e41a1c    #ff7f00    #4daf4a    #377eb8    white
#     4 "black_gold"     #000000    #EAB73E    #8dd3c7    #639BC0    #7F7F7F
#     5 "green_blue"     #33a02c    #b2df8a    #a6cee3    #1f78b4    white
#     6 "brown_teal"     #a6611a    #dfc27d    #80cdc1    #018571    white
#
#   Or give 5 colours of your own, in the same order as the table above:
#     palette = c("purple", "orchid", "lightgreen", "darkgreen", "grey90")
#
#   The p-values come from random shuffles, so they change slightly each time.
#   To get the same map every time, give a seed, for example  seed = 364
#   The seed that was used is always written under the map.
#============================================================================
lisa_palettes <- list(
  red_blue   = c("#ca0020", "#f4a582", "#92c5de", "#0571b0", "white"),
  pastel     = c("#e78ac3", "#fc8d62", "#66c2a5", "#8da0cb", "white"),
  bright     = c("#e41a1c", "#ff7f00", "#4daf4a", "#377eb8", "white"),
  black_gold = c("#000000", "#EAB73E", "#8dd3c7", "#639BC0", "#7F7F7F"),
  green_blue = c("#33a02c", "#b2df8a", "#a6cee3", "#1f78b4", "white"),
  brown_teal = c("#a6611a", "#dfc27d", "#80cdc1", "#018571", "white"))


#------------------------------------------
# The calculation behind lisa_map(). It gives back the data with the LISA
# columns added, and the recipe for the map.
#------------------------------------------
make_lisa <- function(data, variable, label, neighbours, weights, title, data_source,
                      mode, significance, nsim, global_stats, colours, basemap, seed = NULL) {
  
  check_autocorrelation_inputs(data, variable, label, neighbours, weights)
  check_map_options(mode, basemap)
  data_source <- tidy_data_source(data_source)
  seed <- use_seed(seed)
  
  if (!is.numeric(significance) || length(significance) != 1 || significance <= 0 || significance > 1) {
    stop("'significance' needs to be one number between 0 and 1, for example 0.05", call. = FALSE)
  }
  
  quadrants    <- c("High-High", "High-Low", "Low-High", "Low-Low")
  lisa_colours <- choose_colours(colours, lisa_palettes, c(quadrants, "Not significant"))
  
  #------------------------------------------
  # Calculate Local Moran's I
  # zero.policy = TRUE stops an error if an area has no neighbours
  #------------------------------------------
  LocalMoran <- sfdep::local_moran(data[[variable]],
                                   neighbours,
                                   weights,
                                   alternative = "two.sided",
                                   nsim = nsim,
                                   zero.policy = TRUE)
  
  #------------------------------------------
  # Add the results to the data
  #------------------------------------------
  data$LocalMoran          <- LocalMoran$ii
  data$LocalMoran_pvalue   <- LocalMoran$p_folded_sim
  data$LocalMoran_Quadrant <- factor(LocalMoran$pysal, levels = quadrants)
  
  #------------------------------------------
  # Make the "significant only" group.
  # An area with no neighbours has no p-value, so it counts as not significant
  #------------------------------------------
  not_significant_label <- paste0("Not significant (p > ", significance, ")")
  not_significant       <- is.na(data$LocalMoran_pvalue) | data$LocalMoran_pvalue > significance
  
  Quadrant_sig                  <- as.character(data$LocalMoran_Quadrant)
  Quadrant_sig[not_significant] <- not_significant_label
  data$Quadrant_sig             <- factor(Quadrant_sig, levels = c(quadrants, not_significant_label))
  
  #------------------------------------------
  # The notes under the map
  #------------------------------------------
  global_note <- NULL
  if (global_stats) {
    global_note <- global_statistics_note(data[[variable]], neighbours, weights)
  }
  
  notes <- list(method = paste0("LISA (Local Moran's I): groups shown where p <= ", significance,
                                ", using p-values from ", nsim, " random permutations (seed = ", seed, ")."),
                global = global_note,
                shared = "No correction for multiple tests.",
                source = data_source)
  
  recipe <- list(data         = data,
                 fill_column  = "Quadrant_sig",
                 colours      = lisa_colours,
                 numbers      = FALSE,
                 legend_title = "LISA group",
                 map_title    = paste("LISA clusters:", title),
                 notes        = notes,
                 label        = label,
                 basemap      = basemap,
                 seed         = seed,
                 interactive  = (mode == "view"))
  
  list(data = data, recipe = recipe, seed = seed)
}


#' LISA map (Local Moran's I)
#'
#' Puts each area into a group (High-High, High-Low, Low-High, Low-Low or
#' not significant), prints a table of how many areas are in each group, and
#' makes a finished map. The p-values come from random shuffles of the data,
#' so the map changes slightly each time unless you give a `seed`. The seed
#' that was used is always written under the map.
#'
#' @param data Your sf object, for example `Census.sf`.
#' @param variable The NAME of the column you want to study, in "quotes".
#' @param label The NAME of a column that labels each row, in "quotes" (for example `"GEOID"`).
#' @param neighbours The neighbours you made, for example with `sfdep::st_contiguity()`.
#' @param weights The weights you made with `sfdep::st_weights()`.
#' @param title A description of your variable, used on the plots.
#' @param data_source A line of text saying where your data came from, shown under the map.
#'   Leave it out (or use `FALSE`) for no line.
#' @param mode `"plot"` for a static map for a report (the default), or `"view"` for a map
#'   you can zoom and click on.
#' @param basemap `FALSE` for no background map (the default), `TRUE` for tmap's standard
#'   background map, or the name of a background map such as `"OpenStreetMap"`.
#'   Background maps need the internet.
#' @param significance The p-value cut-off. The default is 0.05.
#' @param nsim How many random shuffles are used for the p-values. The default is 499.
#' @param global_stats `TRUE` adds the global Moran's I, Geary's C and Getis-Ord General G
#'   to the notes under the map.
#' @param palette Which colours to use: a palette number (1 to 6), a palette name
#'   (`"red_blue"`, `"pastel"`, `"bright"`, `"black_gold"`, `"green_blue"`, `"brown_teal"`),
#'   or 5 colours of your own in the order High-High, High-Low, Low-High, Low-Low,
#'   not significant.
#' @param seed The random seed. Leave it out and R picks a new one each time.
#'   Give a whole number to get the same map every time.
#' @param silent `TRUE` means do not print the table of groups.
#' @param return_data `TRUE` gives back your data with the results added as new columns
#'   (`LocalMoran`, `LocalMoran_pvalue`, `LocalMoran_Quadrant`, `Quadrant_sig`),
#'   in place of the map.
#'
#' @return A map. Type its name to see it. In R Markdown, a chunk with
#'   `fig.width=7, fig.height=6` gives it enough room.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' LISAmap <- lisa_map(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                     title = "Births in 1974", seed = 364)
#' @export
lisa_map <- function(data, variable, label, neighbours, weights, title = variable,
                     data_source = NULL, mode = "plot", significance = 0.05, nsim = 499,
                     global_stats = FALSE, palette = 1, basemap = FALSE,
                     seed = NULL, silent = FALSE, return_data = FALSE) {
  
  result <- make_lisa(data, variable, label, neighbours, weights, title, data_source,
                      mode, significance, nsim, global_stats, colours = palette, basemap, seed)
  
  if (!silent) {
    print_group_table(paste("LISA groups for:", title),
                      paste("number of areas in each group, using p <=", significance),
                      result$data$Quadrant_sig,
                      result$seed)
  }
  
  if (return_data) {
    return(result$data)
  }
  
  new_autocorrelation_map(list(result$recipe))
}
