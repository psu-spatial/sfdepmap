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
#   HOW SIGNIFICANCE IS DECIDED
#   Each area gets a p-value by shuffling the data (random permutations),
#   using local_g_perm() and local_gstar_perm() from the sfdep package.
#   The areas are put into 7 groups, like the Esri hot spot maps:
#     p <= 0.10 is a hot or cold spot at 90% confidence
#     p <= 0.05 is a hot or cold spot at 95% confidence
#     p <= 0.01 is a hot or cold spot at 99% confidence
#   The sign of the z-score says which: positive = hot, negative = cold.
#
#   Extra options, on top of the shared ones at the top of this file:
#     nsim     = 999    how many random shuffles are used for the p-values (default 499)
#     seed     = 364    the random seed. Leave it out and R picks a new one each time.
#                       The seed that was used is always written under the map.
#     adjust_p = TRUE   adjust the p-values for the number of tests
#                       (Benjamini-Hochberg). This is stricter, so expect
#                       fewer hot and cold spots. The default is FALSE.
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
                       mode, colours, basemap, nsim = 499, seed = NULL, adjust_p = FALSE,
                       show_seed = TRUE) {
  
  check_autocorrelation_inputs(data, variable, label, neighbours, weights)
  check_map_options(mode, basemap)
  data_source <- tidy_data_source(data_source)
  seed <- use_seed(seed)
  
  if (!is.logical(adjust_p) || length(adjust_p) != 1 || is.na(adjust_p)) {
    stop("'adjust_p' needs to be TRUE or FALSE", call. = FALSE)
  }
  
  group_names <- c("Cold Spot - 99% Confidence",
                   "Cold Spot - 95% Confidence",
                   "Cold Spot - 90% Confidence",
                   "Not Significant",
                   "Hot Spot - 90% Confidence",
                   "Hot Spot - 95% Confidence",
                   "Hot Spot - 99% Confidence")
  
  getis_colours <- choose_colours(colours, getis_palettes, group_names)
  
  #------------------------------------------
  # Calculate the statistic for every area, with p-values from random shuffles.
  # The result is a table with one row per area. We use two of its columns:
  #    std_dev = the z-score        p_sim = the p-value from the shuffles
  # zero.policy = TRUE stops an error if an area has no neighbours
  #------------------------------------------
  if (star) {
    # Gi*: local_gstar_perm() adds each area to its own list of neighbours for you
    results <- sfdep::local_gstar_perm(data[[variable]], neighbours, weights,
                                       nsim = nsim, zero.policy = TRUE)
  } else {
    # Gi: neighbours only
    results <- sfdep::local_g_perm(data[[variable]], neighbours, weights,
                                   nsim = nsim, zero.policy = TRUE)
  }
  z_scores <- as.numeric(results$std_dev)
  p_values <- as.numeric(results$p_sim)
  
  #------------------------------------------
  # Optional: adjust the p-values for the number of tests (Benjamini-Hochberg)
  #------------------------------------------
  if (adjust_p) {
    p_values <- stats::p.adjust(p_values, method = "BH")
  }
  
  #------------------------------------------
  # Put every area into one of the 7 groups.
  # Everything starts as "Not Significant". The later lines overwrite the
  # earlier ones, so an area ends up in the strongest group it qualifies for.
  # An area with no neighbours has no p-value, so it stays not significant
  #------------------------------------------
  hot  <- !is.na(p_values) & !is.na(z_scores) & z_scores > 0
  cold <- !is.na(p_values) & !is.na(z_scores) & z_scores < 0
  
  groups <- rep("Not Significant", length(p_values))
  groups[hot  & p_values <= 0.10] <- "Hot Spot - 90% Confidence"
  groups[hot  & p_values <= 0.05] <- "Hot Spot - 95% Confidence"
  groups[hot  & p_values <= 0.01] <- "Hot Spot - 99% Confidence"
  groups[cold & p_values <= 0.10] <- "Cold Spot - 90% Confidence"
  groups[cold & p_values <= 0.05] <- "Cold Spot - 95% Confidence"
  groups[cold & p_values <= 0.01] <- "Cold Spot - 99% Confidence"
  groups <- factor(groups, levels = group_names)
  
  #------------------------------------------
  # Add the results to the data, and write the labels for this version
  #------------------------------------------
  seed_text <- ""
  if (show_seed) {
    seed_text <- paste0(" (seed = ", seed, ")")
  }
  permutation_text <- paste0(", using p-values from ", nsim, " random permutations", seed_text, ".")
  
  if (star) {
    data$getisstar.z       <- z_scores
    data$getisstar.pvalue  <- p_values
    data$getisstar.cluster <- groups
    fill_column  <- "getisstar.cluster"
    legend_title <- "Getis-Ord Gi*\n(area + its neighbours)"
    map_title    <- paste("Gi* hot and cold spots:", title)
    method       <- paste0("Gi*: each area together with its neighbours", permutation_text)
  } else {
    data$getis.z       <- z_scores
    data$getis.pvalue  <- p_values
    data$getis.cluster <- groups
    fill_column  <- "getis.cluster"
    legend_title <- "Getis-Ord Gi\n(neighbours only)"
    map_title    <- paste("Gi hot and cold spots:", title)
    method       <- paste0("Gi: each area's neighbours only (its own value is left out)", permutation_text)
  }
  
  if (adjust_p) {
    correction_note <- "Getis-Ord p-values adjusted for multiple tests (Benjamini-Hochberg)."
  } else {
    correction_note <- "No correction for multiple tests."
  }
  
  notes <- list(method = method,
                global = NULL,
                # Separate sentences, so that a figure with several maps
                # can write each one just once
                shared = c("Confidence levels: 90% is p <= 0.10, 95% is p <= 0.05, 99% is p <= 0.01 (two-sided).",
                           correction_note),
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
                 seed         = seed,
                 interactive  = (mode == "view"))
  
  list(data = data, recipe = recipe, groups = groups, seed = seed)
}


#------------------------------------------
# Gi* : each area counted WITH its neighbours
#------------------------------------------
#' Getis-Ord hot spot maps: Gi* and Gi
#'
#' Both give every area a z-score and a p-value. A positive z-score with a
#' small p-value is a hot spot (high values clustered together) and a negative
#' z-score with a small p-value is a cold spot. The p-values come from random
#' shuffles of the data, using `sfdep::local_g_perm()` and
#' `sfdep::local_gstar_perm()`. Areas are put into 7 groups: 90%, 95% and 99%
#' confidence mean p <= 0.10, 0.05 and 0.01.
#'
#' `getisstar_map()` makes a Gi* ("G-i-star") map: each area is counted
#' TOGETHER WITH its neighbours. `getis_map()` makes a Gi map: each area's own
#' value is LEFT OUT and only its neighbours are used. The map changes
#' slightly each time unless you give a `seed`. The seed that was used is
#' always written under the map.
#'
#' @inheritParams lisa_map
#' @param palette Which colours to use: a palette number (1 to 5), a palette name
#'   (`"red_yellow_blue"`, `"brewer_RdYlBu"`, `"brewer_RdYlBu_DarkSig"`,
#'   `"brewer_PuOr_DarkSig"`, `"brewer_PuOr"`), or 7 colours of your own, from
#'   cold spot 99% to hot spot 99%.
#' @param adjust_p `TRUE` adjusts the p-values for the number of tests
#'   (Benjamini-Hochberg). This is stricter, so expect fewer hot and cold spots.
#'   The default is `FALSE`.
#' @param return_data `TRUE` gives back your data with the results added as new columns
#'   (`getisstar.z`, `getisstar.pvalue` and `getisstar.cluster`, or `getis.z`,
#'   `getis.pvalue` and `getis.cluster`), in place of the map.
#'
#' @return A map. Type its name to see it.
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- sf::st_transform(nc, 32019)   # a flat (projected) map of North Carolina
#' NeighbourMatrix <- sfdep::st_contiguity(nc)
#' WeightsMatrix   <- sfdep::st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)
#' GETISSTARmap <- getisstar_map(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                               title = "Births in 1974", seed = 364)
#' GETISmap     <- getis_map(nc, "BIR74", "NAME", NeighbourMatrix, WeightsMatrix,
#'                           title = "Births in 1974", seed = 364, adjust_p = TRUE)
#' @export
getisstar_map <- function(data, variable, label, neighbours, weights, title = variable,
                          data_source = NULL, mode = "plot", palette = 1, basemap = FALSE,
                          nsim = 499, seed = NULL, adjust_p = FALSE,
                          silent = FALSE, return_data = FALSE) {
  
  result <- make_getis(star = TRUE, data, variable, label, neighbours, weights, title,
                       data_source, mode, colours = palette, basemap, nsim, seed, adjust_p)
  
  if (!silent) {
    print_group_table(paste("Getis-Ord Gi* groups for:", title),
                      "Gi*: each area is counted together with its neighbours",
                      result$groups,
                      result$seed)
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
                      nsim = 499, seed = NULL, adjust_p = FALSE,
                      silent = FALSE, return_data = FALSE) {
  
  result <- make_getis(star = FALSE, data, variable, label, neighbours, weights, title,
                       data_source, mode, colours = palette, basemap, nsim, seed, adjust_p)
  
  if (!silent) {
    print_group_table(paste("Getis-Ord Gi groups for:", title),
                      "Gi: each area's own value is left out, only its neighbours are used",
                      result$groups,
                      result$seed)
  }
  
  if (return_data) {
    return(result$data)
  }
  
  new_autocorrelation_map(list(result$recipe))
}
