#============================================================================
# GETIS-ORD HOT SPOT MAPS: Gi* AND Gi
#
#   Both give every area a z-score:
#     large positive z-score = hot spot  (high values clustered together)
#     large negative z-score = cold spot (low values clustered together)
#
#   THE DIFFERENCE BETWEEN THE TWO
#     getisstar_map()  Gi* ("G-i-star"). Each area is counted TOGETHER WITH its
#                      neighbours. It asks: "is this area part of a cluster?"
#                      This is the one used by the Esri Hot Spot Analysis tool.
#
#     getis_map()      Gi (no star). Each area's own value is LEFT OUT, and only
#                      its neighbours are used. It asks: "is this area
#                      surrounded by a cluster?"
#
#   The z-scores are cut into 7 groups, like the Esri hot spot maps.
#   A z-score beyond:
#     +/- 1.645 is significant at 90% confidence
#     +/- 1.96  is significant at 95% confidence
#     +/- 2.576 is significant at 99% confidence
#
#   COLOURS. Pick a built-in palette by its number or its name,
#   for example  palette = 3  or  palette = "brewer_RdYlBu_DarkSig":
#
#     1 "red_yellow_blue"         dark blue to dark red, pale yellow in the middle (the default)
#     2 "brewer_RdYlBu"           the standard ColorBrewer red-yellow-blue, flipped
#     3 "brewer_RdYlBu_DarkSig"   the same, with dark grey for "not significant"
#     4 "brewer_PuOr_DarkSig"     ColorBrewer purple-orange, flipped, with dark grey for "not significant"
#     5 "brewer_PuOr"             the standard ColorBrewer purple-orange, flipped
#
#   Or give 7 colours of your own, in this order:
#     cold spot 99%, cold spot 95%, cold spot 90%, not significant,
#     hot spot 90%, hot spot 95%, hot spot 99%
#============================================================================
getis_palettes <- list(
  red_yellow_blue = c("#313695", "#74ADD1", "#ABD9E9",    # cold spots: 99%, 95%, 90%
                      "#FFFFBF",                          # not significant
                      "#FDAE61", "#F46D43", "#A50026"),   # hot spots:  90%, 95%, 99%
  
  # the same as rev(brewer.pal(7, "RdYlBu"))
  brewer_RdYlBu         = c("#4575B4", "#91BFDB", "#E0F3F8", "#FFFFBF", "#FEE090", "#FC8D59", "#D73027"),
  brewer_RdYlBu_DarkSig = c("#4575B4", "#91BFDB", "#E0F3F8", "#7F7F7F", "#FEE090", "#FC8D59", "#D73027"),
  
  # the same as rev(brewer.pal(7, "PuOr"))
  brewer_PuOr_DarkSig   = c("#542788", "#998EC3", "#D8DAEB", "#7F7F7F", "#FEE0B6", "#F1A340", "#B35806"),
  brewer_PuOr           = c("#542788", "#998EC3", "#D8DAEB", "#F7F7F7", "#FEE0B6", "#F1A340", "#B35806"))


#------------------------------------------
# The calculation behind getisstar_map() and getis_map().
#   star = TRUE   Gi*: each area is counted with its neighbours
#   star = FALSE  Gi:  each area's neighbours only
# It gives back the data with the new columns added, and the recipe for the map.
#------------------------------------------
make_getis <- function(star, data, variable, label, neighbours, weights, title, data_source,
                       mode, colours, basemap) {
  
  check_autocorrelation_inputs(data, variable, label, neighbours, weights)
  check_map_options(mode, basemap)
  data_source <- tidy_data_source(data_source)
  
  group_names <- c("Cold Spot - 99% Confidence",
                   "Cold Spot - 95% Confidence",
                   "Cold Spot - 90% Confidence",
                   "Not Significant",
                   "Hot Spot - 90% Confidence",
                   "Hot Spot - 95% Confidence",
                   "Hot Spot - 99% Confidence")
  
  getis_colours <- choose_colours(colours, getis_palettes, group_names)
  
  #------------------------------------------
  # Calculate the z-score for every area
  #------------------------------------------
  if (star) {
    # Gi*: local_gstar() adds each area to its own list of neighbours for you
    z_scores <- sfdep::local_gstar(data[[variable]], neighbours, weights)
  } else {
    # Gi: neighbours only. zero.policy = TRUE stops an error if an area has no neighbours
    z_scores <- sfdep::local_g(data[[variable]], neighbours, weights, zero.policy = TRUE)
  }
  z_scores <- as.numeric(z_scores)
  
  #------------------------------------------
  # Cut the z-scores into 7 groups.
  # An area with no neighbours has no Gi z-score, so it counts as not significant
  #------------------------------------------
  groups <- cut(z_scores,
                breaks = c(-Inf, -2.576, -1.96, -1.645, 1.645, 1.96, 2.576, Inf),
                labels = group_names)
  groups[is.na(groups)] <- "Not Significant"
  
  #------------------------------------------
  # Add the results to the data, and write the labels for this version
  #------------------------------------------
  if (star) {
    data$getisstar.z       <- z_scores
    data$getisstar.cluster <- groups
    fill_column  <- "getisstar.cluster"
    legend_title <- "Getis-Ord Gi*\n(area + its neighbours)"
    map_title    <- paste("Gi* hot and cold spots:", title)
    method       <- "Gi*: z-scores for each area together with its neighbours."
  } else {
    data$getis.z       <- z_scores
    data$getis.cluster <- groups
    fill_column  <- "getis.cluster"
    legend_title <- "Getis-Ord Gi\n(neighbours only)"
    map_title    <- paste("Gi hot and cold spots:", title)
    method       <- "Gi: z-scores for each area's neighbours only (its own value is left out)."
  }
  
  notes <- list(method = method,
                global = NULL,
                # Two separate sentences, so that a figure with several maps
                # can write "No correction for multiple tests." just once
                shared = c("Confidence levels are two-sided Z-scores.",
                           "No correction for multiple tests."),
                source = data_source)
  
  recipe <- list(data         = data,
                 fill_column  = fill_column,
                 colours      = getis_colours,
                 numbers      = FALSE,
                 legend_title = legend_title,
                 map_title    = map_title,
                 notes        = notes,
                 label        = label,
                 basemap      = basemap,
                 interactive  = (mode == "view"))
  
  list(data = data, recipe = recipe, groups = groups)
}


#------------------------------------------
# Gi* : each area counted WITH its neighbours
#------------------------------------------
#' Getis-Ord hot spot maps: Gi* and Gi
#'
#' Both give every area a z-score. A large positive z-score is a hot spot
#' (high values clustered together) and a large negative z-score is a cold
#' spot. The z-scores are cut into 7 groups at 90%, 95% and 99% confidence.
#'
#' `getisstar_map()` makes a Gi* ("G-i-star") map: each area is counted
#' TOGETHER WITH its neighbours. `getis_map()` makes a Gi map: each area's own
#' value is LEFT OUT and only its neighbours are used. These maps use no
#' random numbers, so they have no seed.
#'
#' @inheritParams lisa_map
#' @param palette Which colours to use: a palette number (1 to 5), a palette name
#'   (`"red_yellow_blue"`, `"brewer_RdYlBu"`, `"brewer_RdYlBu_DarkSig"`,
#'   `"brewer_PuOr_DarkSig"`, `"brewer_PuOr"`), or 7 colours of your own, from
#'   cold spot 99% to hot spot 99%.
#' @param return_data `TRUE` gives back your data with the results added as new columns
#'   (`getisstar.z` and `getisstar.cluster`, or `getis.z` and `getis.cluster`),
#'   in place of the map.
#'
#' @return A map. Type its name to see it.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' GETISSTARmap <- getisstar_map(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                               title = "Births in 1974")
#' GETISmap     <- getis_map(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                           title = "Births in 1974")
#' @export
getisstar_map <- function(data, variable, label, neighbours, weights, title = variable,
                          data_source = NULL, mode = "plot", palette = 1, basemap = FALSE,
                          silent = FALSE, return_data = FALSE) {
  
  result <- make_getis(star = TRUE, data, variable, label, neighbours, weights, title,
                       data_source, mode, colours = palette, basemap)
  
  if (!silent) {
    print_group_table(paste("Getis-Ord Gi* groups for:", title),
                      "Gi*: each area is counted together with its neighbours",
                      result$groups)
  }
  
  if (return_data) {
    return(result$data)
  }
  
  new_autocorrelation_map(list(result$recipe))
}


#------------------------------------------
# Gi : each area's neighbours ONLY
#------------------------------------------
#' @rdname getisstar_map
#' @export
getis_map <- function(data, variable, label, neighbours, weights, title = variable,
                      data_source = NULL, mode = "plot", palette = 1, basemap = FALSE,
                      silent = FALSE, return_data = FALSE) {
  
  result <- make_getis(star = FALSE, data, variable, label, neighbours, weights, title,
                       data_source, mode, colours = palette, basemap)
  
  if (!silent) {
    print_group_table(paste("Getis-Ord Gi groups for:", title),
                      "Gi: each area's own value is left out, only its neighbours are used",
                      result$groups)
  }
  
  if (return_data) {
    return(result$data)
  }
  
  new_autocorrelation_map(list(result$recipe))
}
