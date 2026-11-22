#============================================================================
# A helper that checks the inputs and stops with a clear message if
# something is wrong. The three main functions all run this first.
# You do not need to run it yourself.
#============================================================================
check_autocorrelation_inputs <- function(data, variable, label, neighbours, weights) {
  
  #------------------------------------------
  # Are the packages installed?
  #------------------------------------------
  for (package in c("sf", "sfdep", "spdep", "tmap")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("The package '", package, "' is not installed. ",
           "Run install.packages(\"", package, "\") and try again.", call. = FALSE)
    }
  }
  
  #------------------------------------------
  # Were the column names given in quotes?
  #------------------------------------------
  quote_message <- paste("The column names for 'variable' and 'label' need to be in quotes,",
                         "for example variable = \"percent_income_gt75\", label = \"GEOID\"")
  variable <- tryCatch(variable, error = function(e) stop(quote_message, call. = FALSE))
  label    <- tryCatch(label,    error = function(e) stop(quote_message, call. = FALSE))
  
  #------------------------------------------
  # Is the data an sf object, and are the column names real?
  #------------------------------------------
  if (!inherits(data, "sf")) {
    stop("'data' needs to be an sf object, for example Census.sf", call. = FALSE)
  }
  if (!is.character(variable) || !(variable %in% names(data))) {
    stop("I can't find the column for 'variable' in your data. ",
         "Check the spelling and put the name in quotes, ",
         "for example variable = \"percent_income_gt75\"", call. = FALSE)
  }
  if (!is.character(label) || !(label %in% names(data))) {
    stop("I can't find the column for 'label' in your data. ",
         "Check the spelling and put the name in quotes, ",
         "for example label = \"GEOID\"", call. = FALSE)
  }
  
  #------------------------------------------
  # Is the variable made of numbers, with nothing missing?
  #------------------------------------------
  values <- data[[variable]]
  if (!is.numeric(values)) {
    stop("The column \"", variable, "\" is not numeric, so it can't be analysed.", call. = FALSE)
  }
  if (any(!is.finite(values))) {
    stop("The column \"", variable, "\" has ", sum(!is.finite(values)),
         " missing or infinite values. Remove those rows first (for example with drop_na()), ",
         "then make your neighbours and weights again.", call. = FALSE)
  }
  
  #------------------------------------------
  # Do the neighbours and weights match the data?
  # (They stop matching if you remove rows AFTER making them)
  #------------------------------------------
  if (length(neighbours) != nrow(data) || length(weights) != nrow(data)) {
    stop("Your neighbours/weights do not match your data: the data has ", nrow(data),
         " rows, but the neighbours have ", length(neighbours),
         " and the weights have ", length(weights), ". ",
         "Run st_contiguity() and st_weights() again on this data.", call. = FALSE)
  }
  
  invisible(TRUE)
}
