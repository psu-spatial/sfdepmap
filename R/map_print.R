#------------------------------------------
# Wrap a map (or several) up with its recipe, so that it can be re-drawn
# to fit whatever size of plot it ends up in
#------------------------------------------
new_autocorrelation_map <- function(recipes, layout = NULL) {
  map <- build_autocorrelation_map(recipes[[1]])
  class(map) <- c("autocorrelation_map", class(map))
  attr(map, "recipes") <- recipes
  attr(map, "layout")  <- layout
  map
}


#------------------------------------------
# Show a map from this script.
#   - A static map is always drawn in tmap's "plot" mode, and an interactive
#     map in "view" mode. Your own tmap mode is put back afterwards.
#   - A static map is re-built to fit the size of the plot it is drawn in.
#   - Several recipes means several maps in one figure.
#------------------------------------------
show_autocorrelation_map <- function(x, printer, ...) {
  
  recipes <- attr(x, "recipes")
  layout  <- attr(x, "layout")
  static  <- !recipes[[1]]$interactive
  
  old_mode <- suppressMessages(tmap::tmap_mode(if (static) "plot" else "view"))
  on.exit(suppressMessages(tmap::tmap_mode(old_mode)))
  
  # One map
  if (length(recipes) == 1) {
    if (!static) {
      return(printer(build_autocorrelation_map(recipes[[1]]), ...))
    }
    #------------------------------------------
    # tmap sometimes cannot fit the scale bar (error: object 'sbW' not found).
    # This depends on the plot size and on the computer's fonts and screen
    # settings. If that happens, draw the map again without the scale bar.
    #------------------------------------------
    plot_width <- grDevices::dev.size("in")[1]
    map    <- build_autocorrelation_map(recipes[[1]], panel_width = plot_width)
    result <- tryCatch(printer(map, ...), error = function(e) e)
    if (inherits(result, "error")) {
      message("The map was drawn without a scale bar because there was not enough room for it. ",
              "To get the scale bar back, make the plot bigger (in R Markdown, use chunk options ",
              "like fig.width=7, fig.height=6).")
      map    <- build_autocorrelation_map(recipes[[1]], panel_width = plot_width, scale_bar = FALSE)
      result <- printer(map, ...)
    }
    return(result)
  }
  
  # Several interactive maps: tmap links them, so they zoom and move together
  if (!static) {
    maps <- lapply(recipes, build_autocorrelation_map)
    return(printer(do.call(tmap::tmap_arrange, c(maps, list(ncol = layout$ncol, sync = TRUE))), ...))
  }
  
  # Several static maps
  draw_map_panels(recipes, layout)
}

#' Show a map made by this package
#'
#' These run for you when you type the name of a map, or when a map is
#' printed in R Markdown or Quarto. You do not need to call them yourself.
#'
#' @param x A map made by [lisa_map()], [getisstar_map()], [getis_map()] or
#'   [autocorrelation_summary()].
#' @param ... Not used.
#' @return The map, invisibly.
#' @export
print.autocorrelation_map <- function(x, ...) {
  show_autocorrelation_map(x, printer = print, ...)
  invisible(x)
}

#' @rdname print.autocorrelation_map
#' @exportS3Method knitr::knit_print
knit_print.autocorrelation_map <- function(x, ...) {
  show_autocorrelation_map(x, printer = knitr::knit_print, ...)
}



#------------------------------------------
# The data given back by return_data = TRUE: the label column, the column
# that was studied and the new columns. (An sf object always keeps its geometry.)
#------------------------------------------
results_only <- function(data, label, variable, new_columns) {
  data[, unique(c(label, variable, new_columns))]
}


#------------------------------------------
# Print a table of how many areas are in each group
#------------------------------------------
print_group_table <- function(heading, detail, groups, seed = NULL) {
  cat("\n", heading, "\n", sep = "")
  cat("(", detail, ")\n", sep = "")
  if (!is.null(seed)) {
    cat("(random seed: ", seed, ")\n", sep = "")
  }
  print(as.data.frame(table(Group = groups), responseName = "Areas"), row.names = FALSE)
  cat("\n")
}
