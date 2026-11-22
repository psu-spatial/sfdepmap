# The tests use the North Carolina counties that come with the sf package
try(sf::sf_proj_network(FALSE), silent = TRUE)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc <- sf::st_transform(nc, 32019)
nb <- sfdep::st_contiguity(nc)
wt <- sfdep::st_weights(nb, style = "W", allow_zero = TRUE)

caption_of <- function(map) {
  paste(sfdepmap:::recipe_note_lines(attr(map, "recipes")[[1]]), collapse = " ")
}

test_that("lisa_map makes a map and returns data when asked", {
  map <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 364, silent = TRUE)
  expect_s3_class(map, "autocorrelation_map")

  out <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 364, silent = TRUE, return_data = TRUE)
  expect_true(all(c("LocalMoran", "LocalMoran_pvalue", "LocalMoran_Quadrant", "Quadrant_sig") %in% names(out)))
  expect_equal(nrow(out), nrow(nc))
})

test_that("the same seed gives the same LISA result, and the seed is in the caption", {
  a <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 364, silent = TRUE, return_data = TRUE)
  b <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 364, silent = TRUE, return_data = TRUE)
  expect_identical(a$LocalMoran_pvalue, b$LocalMoran_pvalue)

  map <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 364, silent = TRUE)
  expect_match(caption_of(map), "seed = 364", fixed = TRUE)

  # with no seed given, one is picked and written down
  map <- lisa_map(nc, "BIR74", "NAME", nb, wt, silent = TRUE)
  expect_match(caption_of(map), "seed = [0-9]+")
})

test_that("data_source = FALSE does not write FALSE under the map", {
  map <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 1, silent = TRUE, data_source = FALSE)
  expect_no_match(caption_of(map), "FALSE", fixed = TRUE)

  map <- lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 1, silent = TRUE, data_source = "Data: test.")
  expect_match(caption_of(map), "Data: test.", fixed = TRUE)
})

test_that("the Getis-Ord maps work and have no seed", {
  star <- getisstar_map(nc, "BIR74", "NAME", nb, wt, silent = TRUE, return_data = TRUE)
  expect_equal(nlevels(star$getisstar.cluster), 7)

  plain <- getis_map(nc, "BIR74", "NAME", nb, wt, silent = TRUE, return_data = TRUE)
  expect_equal(nlevels(plain$getis.cluster), 7)

  map <- getisstar_map(nc, "BIR74", "NAME", nb, wt, silent = TRUE)
  expect_no_match(caption_of(map), "seed", fixed = TRUE)
  expect_error(getis_map(nc, "BIR74", "NAME", nb, wt, seed = 1))
})

test_that("mistakes give plain-language errors", {
  expect_error(lisa_map(nc, "NOT_A_COLUMN", "NAME", nb, wt), "can't find the column")
  expect_error(lisa_map(nc, "BIR74", "NAME", nb, wt, palette = 99), "palette")
  expect_error(lisa_map(nc, "BIR74", "NAME", nb, wt, seed = "abc"), "seed")
  expect_error(lisa_map(nc, "BIR74", "NAME", nb, wt, mode = "interactive"), "mode")
  expect_error(lisa_map(nc[1:50, ], "BIR74", "NAME", nb, wt), "do not match")
})

test_that("the summary figure builds its caption and draws", {
  fig <- autocorrelation_summary(nc, "BIR74", "NAME", nb, wt, title = "Births",
                                 units = "Births", seed = 364, data_source = "Data: test.")
  caption <- paste(sfdepmap:::combined_caption(attr(fig, "recipes")), collapse = "\n")
  expect_match(caption, "A: Choropleth of the raw data (Jenks)", fixed = TRUE)
  expect_match(caption, "seed = 364", fixed = TRUE)
  expect_match(caption, "Global statistics", fixed = TRUE)
  expect_match(caption, "General G (binary weights)", fixed = TRUE)
  expect_equal(lengths(regmatches(caption, gregexpr("No correction for multiple tests", caption))), 1)

  no_text <- autocorrelation_summary(nc, "BIR74", "NAME", nb, wt, seed = 364,
                                     explanation = FALSE, global_stats = FALSE)
  caption <- paste(sfdepmap:::combined_caption(attr(no_text, "recipes"), explanation = FALSE), collapse = "\n")
  expect_match(caption, "Random seed for the LISA p-values: 364", fixed = TRUE)

  # draw it at the recommended size (nothing is saved)
  grDevices::pdf(NULL, width = 9, height = 11)
  on.exit(grDevices::dev.off())
  expect_no_error(print(fig))
})

test_that("single maps draw", {
  grDevices::pdf(NULL, width = 7, height = 6)
  on.exit(grDevices::dev.off())
  expect_no_error(print(lisa_map(nc, "BIR74", "NAME", nb, wt, seed = 1, silent = TRUE, global_stats = TRUE)))
  expect_no_error(print(getisstar_map(nc, "BIR74", "NAME", nb, wt, silent = TRUE)))
  expect_no_error(moran_scatterplot(nc, "BIR74", "NAME", nb, wt))
})

test_that("the General G uses binary weights, whatever weights are given", {
  values <- nc$BIR74
  wt_b   <- sfdep::st_weights(nb, style = "B", allow_zero = TRUE)

  note_w <- sfdepmap:::global_statistics_note(values, nb, wt)
  note_b <- sfdepmap:::global_statistics_note(values, nb, wt_b)
  g_part <- function(note) sub(".*General G", "General G", note)
  expect_identical(g_part(note_w), g_part(note_b))

  # and it matches spdep's own answer with binary weights
  direct <- spdep::globalG.test(values, spdep::nb2listw(nb, style = "B", zero.policy = TRUE),
                                alternative = "two.sided", zero.policy = TRUE)
  expect_match(note_w, paste0("= ", signif(direct$estimate[1], 3)), fixed = TRUE)
  expect_no_warning(sfdepmap:::global_statistics_note(values, nb, wt))
})
