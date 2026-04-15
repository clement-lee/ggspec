library(ggplot2)

# ---------------------------------------------------------------------------
# enrich_spec() — basic structure
# ---------------------------------------------------------------------------

test_that("enrich_spec() returns a tibble with params_tbl and built_aes columns", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  es <- enrich_spec(p)
  expect_s3_class(es, "tbl_df")
  expect_true("params_tbl" %in% names(es))
  expect_true("built_aes"  %in% names(es))
  expect_equal(nrow(es), 1L)
})

test_that("enrich_spec() returns empty tibble for a plot with no layers", {
  p  <- ggplot(mpg, aes(displ, hwy))
  es <- enrich_spec(p)
  expect_equal(nrow(es), 0L)
  expect_true("params_tbl" %in% names(es))
  expect_true("built_aes"  %in% names(es))
})

test_that("enrich_spec() has one row per layer for multi-layer plots", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
  es <- enrich_spec(p)
  expect_equal(nrow(es), 2L)
  expect_equal(length(es$params_tbl), 2L)
  expect_equal(length(es$built_aes),  2L)
})

# ---------------------------------------------------------------------------
# params_tbl — explicit flag for non-aesthetic parameters
# ---------------------------------------------------------------------------

test_that("params_tbl has required columns", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  es <- enrich_spec(p)
  tbl <- es$params_tbl[[1]]
  expect_true(all(c("param", "value", "explicit", "source") %in% names(tbl)))
})

test_that("explicitly set params have explicit = TRUE", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_smooth(se = FALSE, method = "lm")
  es <- enrich_spec(p)
  tbl <- es$params_tbl[[1]]
  expect_true(tbl$explicit[tbl$param == "se"])
  expect_true(tbl$explicit[tbl$param == "method"])
})

test_that("explicitly set param carries the user-supplied value", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_smooth(se = FALSE)
  es <- enrich_spec(p)
  tbl <- es$params_tbl[[1]]
  se_val <- tbl$value[[which(tbl$param == "se")]]
  expect_identical(se_val, FALSE)
})

test_that("default params (not set by user) have explicit = FALSE", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  es <- enrich_spec(p)
  tbl <- es$params_tbl[[1]]
  # se should be present in the params list but marked as non-explicit
  if ("se" %in% tbl$param) {
    expect_false(tbl$explicit[tbl$param == "se"])
  }
})

test_that("histogram bins param is explicit when set", {
  p  <- ggplot(mpg, aes(hwy)) + geom_histogram(bins = 20L)
  es <- enrich_spec(p)
  tbl <- es$params_tbl[[1]]
  expect_true(tbl$explicit[tbl$param == "bins"])
  expect_identical(tbl$value[[which(tbl$param == "bins")]], 20L)
})

# ---------------------------------------------------------------------------
# built_aes — explicit flag for aesthetics
# ---------------------------------------------------------------------------

test_that("built_aes has required columns", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  es <- enrich_spec(p)
  tbl <- es$built_aes[[1]]
  expect_true(all(c("aesthetic", "value", "explicit") %in% names(tbl)))
})

test_that("mapped aesthetics are explicit in built_aes", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  es <- enrich_spec(p)
  tbl <- es$built_aes[[1]]
  expect_true(tbl$explicit[tbl$aesthetic == "x"])
  expect_true(tbl$explicit[tbl$aesthetic == "y"])
  expect_true(tbl$explicit[tbl$aesthetic == "colour"])
})

test_that("default aesthetics (e.g. colour) are non-explicit when not mapped", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  es <- enrich_spec(p)
  tbl <- es$built_aes[[1]]
  if ("colour" %in% tbl$aesthetic) {
    expect_false(tbl$explicit[tbl$aesthetic == "colour"])
  }
})

test_that("constant aesthetic set via aes_params is explicit", {
  p  <- ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "steelblue")
  es <- enrich_spec(p)
  tbl <- es$built_aes[[1]]
  if ("colour" %in% tbl$aesthetic) {
    expect_true(tbl$explicit[tbl$aesthetic == "colour"])
  }
})

test_that("globally mapped aesthetics are explicit in built_aes", {
  p  <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
  es <- enrich_spec(p)
  tbl <- es$built_aes[[1]]
  expect_true(tbl$explicit[tbl$aesthetic == "colour"])
})

# ---------------------------------------------------------------------------
# .reference_layer_params helper
# ---------------------------------------------------------------------------

test_that(".reference_layer_params() returns a named list of default values", {
  ref <- ggspec:::.reference_layer_params("smooth", "smooth")
  expect_type(ref, "list")
  expect_true(length(ref) > 0L)
  expect_true(!is.null(names(ref)))
})

test_that(".reference_layer_params() returns empty list for unknown geom", {
  ref <- ggspec:::.reference_layer_params("unknowngeomxyz", "identity")
  expect_type(ref, "list")
  expect_equal(length(ref), 0L)
})
