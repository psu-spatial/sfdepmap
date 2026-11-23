#' sfdepmap: Moran, LISA and Getis-Ord maps for teaching spatial statistics
#'
#' Functions that each make one plot from your data:
#'
#' * [moran_scatterplot()]: Moran's scatterplot
#' * [lisa_scatterplot()]: Moran's scatterplot with points coloured by LISA group
#' * [lisa_map()]: LISA map (Local Moran's I)
#' * [getisstar_map()]: Getis-Ord Gi* hot spot map (each area counted WITH its neighbours)
#' * [getis_map()]: Getis-Ord Gi hot spot map (each area's neighbours ONLY)
#' * [autocorrelation_summary()]: one figure with four maps: your data, LISA, Gi* and Gi
#'
#' Every function is given the same things: your sf data, the name of the
#' column to study, the name of a label column, your neighbours and your
#' weights.
#'
#' @keywords internal
"_PACKAGE"
