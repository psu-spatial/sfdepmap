#============================================================================
# HELPERS FOR THE MAPS
# The map functions use these. You do not need to run them yourself.
#============================================================================

#------------------------------------------
# Split long text over several lines, so it is never cut off at the edge
#------------------------------------------
wrap_text <- function(text, width) {
  # An empty piece of text is kept as a blank line
  wrap_one <- function(piece) {
    if (nchar(piece) == 0) "" else strwrap(piece, width = width)
  }
  lines <- unlist(lapply(text, wrap_one))
  paste(lines, collapse = "\n")
}


#------------------------------------------
# Write a p-value neatly, for example "p = 0.012" or "p < 0.001"
#------------------------------------------
format_p <- function(p) {
  if (is.na(p)) {
    return("p not available")
  }
  if (p < 0.001) {
    return("p < 0.001")
  }
  paste0("p = ", formatC(p, format = "f", digits = 3))
}


#------------------------------------------
# Work out which colours to use.
#   colours      what was asked for in the 'palette' option: a palette number,
#                a palette name, or your own colours
#   palettes     the built-in palettes for this kind of map
#   group_names  the groups being coloured, in order
#------------------------------------------
choose_colours <- function(colours, palettes, group_names) {
  
  how_to <- paste0("'palette' can be a palette number (1 to ", length(palettes), "), ",
                   "a palette name (", paste0("\"", names(palettes), "\"", collapse = ", "), "), ",
                   "or ", length(group_names), " colours of your own in this order: ",
                   paste(group_names, collapse = ", "), ".")
  
  # A number or a name picks one of the built-in palettes
  if (length(colours) == 1 && is.numeric(colours)) {
    if (!(colours %in% seq_along(palettes))) {
      stop("There is no palette number ", colours, ". ", how_to, call. = FALSE)
    }
    return(palettes[[colours]])
  }
  if (length(colours) == 1 && is.character(colours) && colours %in% names(palettes)) {
    return(palettes[[colours]])
  }
  
  # Otherwise it needs to be the right number of real colours
  if (!is.character(colours) || length(colours) != length(group_names)) {
    stop(how_to, call. = FALSE)
  }
  real_colours <- tryCatch({ grDevices::col2rgb(colours); TRUE }, error = function(e) FALSE)
  if (!real_colours) {
    stop("At least one of your colours is not a colour that R knows. ",
         "Use colour names like \"white\" or codes like \"#ca0020\".", call. = FALSE)
  }
  colours
}


#------------------------------------------
# Check the options that all the map functions share
#------------------------------------------
check_map_options <- function(mode, basemap) {
  
  if (!is.character(mode) || length(mode) != 1 || !(mode %in% c("plot", "view"))) {
    stop("'mode' needs to be \"plot\" (a static map) or \"view\" (a zoom-and-click map)", call. = FALSE)
  }
  
  basemap_ok <- length(basemap) == 1 && !is.na(basemap) && (is.logical(basemap) || is.character(basemap))
  if (!basemap_ok) {
    stop("'basemap' needs to be FALSE (no background map), TRUE (tmap's standard background map) ",
         "or the name of a background map in quotes, for example \"OpenStreetMap\"", call. = FALSE)
  }
  invisible(TRUE)
}


#------------------------------------------
# The background map: the tmap layer, and the credit line for it
#------------------------------------------
basemap_layer <- function(basemap) {
  if (is.character(basemap)) {
    return(tmap::tm_basemap(basemap))
  }
  tmap::tm_basemap()
}

basemap_credit <- function(basemap) {
  if (identical(basemap, FALSE)) {
    return(NULL)
  }
  if (identical(basemap, TRUE)) {
    return("Background map: Esri World Gray Canvas (tiles (c) Esri).")
  }
  if (grepl("OpenStreetMap", basemap)) {
    return(paste0("Background map: ", basemap, " ((c) OpenStreetMap contributors)."))
  }
  paste0("Background map: ", basemap, ".")
}


#------------------------------------------
# The three global statistics, written as one sentence.
#   Moran's I and Geary's C measure clustering of similar values.
#   The Getis-Ord General G measures clustering of HIGH values. It only
#   makes sense when no value is negative, so it is left out otherwise.
#
# Moran's I and Geary's C use your own weights.
# The General G is always calculated with BINARY weights (every neighbour
# gets a weight of 1), because that is what is recommended for this test.
# It uses your neighbours, so only the weights are different.
# The p-values are two-sided.
#------------------------------------------
global_statistics_note <- function(values, neighbours, weights) {
  
  neighbours_and_weights <- sfdep::recreate_listw(neighbours, weights)
  
  moran <- spdep::moran.test(values, neighbours_and_weights,
                             alternative = "two.sided", zero.policy = TRUE)
  geary <- spdep::geary.test(values, neighbours_and_weights,
                             alternative = "two.sided", zero.policy = TRUE)
  
  note <- paste0("Global statistics (two-sided): ",
                 "Moran's I = ", formatC(moran$estimate[1], format = "f", digits = 3),
                 " (", format_p(moran$p.value), "); ",
                 "Geary's C = ", formatC(geary$estimate[1], format = "f", digits = 3),
                 " (", format_p(geary$p.value), ")")
  
  if (all(values >= 0)) {
    # Your neighbours, with binary weights (style = "B")
    binary_weights <- spdep::nb2listw(neighbours, style = "B", zero.policy = TRUE)
    general_g <- spdep::globalG.test(values, binary_weights,
                                     alternative = "two.sided", zero.policy = TRUE)
    note <- paste0(note, "; Getis-Ord General G (binary weights) = ", signif(general_g$estimate[1], 3),
                   " (", format_p(general_g$p.value), ")")
  }
  
  paste0(note, ".")
}


#------------------------------------------
# Choose and set the random seed.
#   seed = NULL (the default): R picks a new seed, so the answer changes each time
#   seed = a whole number:     that seed is used, so the answer is the same each time
# Either way the seed is set here and given back, so it can be written under the map.
#------------------------------------------
use_seed <- function(seed) {
  
  if (is.null(seed)) {
    seed <- sample.int(999999, 1)
  }
  
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) || seed != round(seed)) {
    stop("'seed' needs to be one whole number, for example seed = 364. ",
         "Leave it out to let R pick a new one each time.", call. = FALSE)
  }
  
  seed <- as.integer(seed)
  set.seed(seed)
  seed
}


#------------------------------------------
# Tidy up the data_source option. It should be a line of text. Anything
# else (FALSE, NA, "" or nothing) means "no data source line".
#------------------------------------------
tidy_data_source <- function(data_source) {
  if (!is.character(data_source) || length(data_source) != 1 || is.na(data_source) || nchar(data_source) == 0) {
    return(NULL)
  }
  data_source
}


#------------------------------------------
# All the notes for one map, in order, joined into ONE paragraph
# (the paragraph is split over lines only where it runs out of room)
#------------------------------------------
recipe_note_lines <- function(recipe) {
  paste(c(recipe$notes$method,
          recipe$notes$global,
          recipe$notes$shared,
          recipe$notes$source,
          basemap_credit(recipe$basemap)),
        collapse = " ")
}


#------------------------------------------
# One shared caption for a figure with several maps.
# Each sentence is written once, even if several maps use it.
#
# The caption is written in three blocks, with a blank line between them:
#   1. the explanation: the sentence for each map, joined by ";"
#      then (on a new line) the notes the maps share
#   2. the global statistics
#   3. the end block: the data source and background map credit
#
# explanation = FALSE leaves out block 1. The random seed is normally inside
# the LISA sentence in block 1, so it is then written in block 3 instead.
# Block 2 is only there if the global statistics were calculated.
#------------------------------------------
combined_caption <- function(recipes, explanation = TRUE) {
  
  pick <- function(part) {
    unique(unlist(lapply(recipes, function(recipe) recipe$notes[[part]])))
  }
  credits <- unique(unlist(lapply(recipes, function(recipe) basemap_credit(recipe$basemap))))
  seed    <- unique(unlist(lapply(recipes, function(recipe) recipe$seed)))
  
  #------------------------------------------
  # Block 1: the explanation
  #------------------------------------------
  explanation_block <- NULL
  if (explanation) {
    
    # The A, B, C, D sentences, joined into one paragraph.
    # Each one loses its full stop, and they are joined with a ";"
    # (\u00a0 is a space that is not squashed when the text is wrapped)
    methods      <- sub("\\.$", "", pick("method"))
    method_line  <- paste0(paste(methods, collapse = ";\u00a0\u00a0 "), ".")
    
    # The shared notes, on one new line. "No correction..." goes last
    shared      <- pick("shared")
    last        <- grepl("multiple tests", shared)
    shared_line <- paste(c(shared[!last], shared[last]), collapse = " ")
    
    explanation_block <- c(method_line, shared_line)
  }
  
  #------------------------------------------
  # Block 2: the global statistics
  #------------------------------------------
  statistics_block <- pick("global")
  
  #------------------------------------------
  # Block 3: the end block
  #------------------------------------------
  end_block <- paste(c(pick("source"), credits), collapse = " ")
  if (!explanation && length(seed) > 0) {
    end_block <- c(paste0("Random seed: ", seed, "."), end_block)
  }
  
  #------------------------------------------
  # Join the blocks that have something in them, with a blank line between
  #------------------------------------------
  blocks <- list(explanation_block, statistics_block, end_block)
  blocks <- lapply(blocks, function(block) block[nchar(block) > 0])
  blocks <- blocks[lengths(blocks) > 0]
  
  lines <- character(0)
  for (block in blocks) {
    if (length(lines) > 0) {
      lines <- c(lines, "")
    }
    lines <- c(lines, block)
  }
  lines
}


#------------------------------------------
# How wide is a map's legend, in inches? (0.062 is the width of one letter)
#------------------------------------------
legend_width_inches <- function(recipe) {
  if (recipe$numbers) {
    legend_words <- c("00,000 to 00,000", "No data (excluded)")
  } else {
    legend_words <- c(levels(recipe$data[[recipe$fill_column]]), "No data (excluded)")
  }
  legend_words <- c(legend_words, strsplit(recipe$legend_title, "\n")[[1]])
  0.6 + 0.062 * max(nchar(legend_words))
}


#------------------------------------------
# How tall is a map's legend, in inches? (one row for each group, plus the title)
#------------------------------------------
legend_height_inches <- function(recipe) {
  if (recipe$numbers) {
    n_groups <- 5
  } else {
    n_groups <- nlevels(recipe$data[[recipe$fill_column]])
  }
  n_title_lines <- length(strsplit(recipe$legend_title, "\n")[[1]])
  0.35 + 0.2 * (n_groups + 1) + 0.2 * n_title_lines
}
