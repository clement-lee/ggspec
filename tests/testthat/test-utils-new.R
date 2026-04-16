# ---------------------------------------------------------------------------
# flat_mappings()
# ---------------------------------------------------------------------------

test_that("flat_mappings() returns a data.frame with 'aes' and 'var' columns", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- flat_mappings(p)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("aes", "var"))
})

test_that("flat_mappings() captures global and local aesthetics", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- flat_mappings(p)
  expect_true("x" %in% result$aes)
  expect_true("colour" %in% result$aes)
  expect_equal(result$var[result$aes == "x"], "displ")
  expect_equal(result$var[result$aes == "colour"], "class")
})

test_that("flat_mappings() returns global mappings even for plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- flat_mappings(p)
  # Global mapping (layer 0) contributes x=displ, y=hwy
  expect_true(nrow(result) >= 2L)
  expect_true("x" %in% result$aes)
  expect_true("y" %in% result$aes)
})

test_that("flat_mappings() returns distinct rows", {
  # Both layers inherit x and y from global
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
  result <- flat_mappings(p)
  expect_equal(nrow(result), nrow(unique(result)))
})

# ---------------------------------------------------------------------------
# mapping_exists()
# ---------------------------------------------------------------------------

test_that("mapping_exists() returns TRUE when mapping is present", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  expect_true(mapping_exists(p, "x", "displ"))
  expect_true(mapping_exists(p, "colour", "class"))
})

test_that("mapping_exists() returns FALSE when mapping is absent", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_false(mapping_exists(p, "colour", "class"))
  expect_false(mapping_exists(p, "x", "cty"))
})

# ---------------------------------------------------------------------------
# layer_data_index()
# ---------------------------------------------------------------------------

test_that("layer_data_index() returns 0 for global data", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_equal(layer_data_index(p, mpg), 0L)
})

test_that("layer_data_index() returns the layer index for local data", {
  local_df <- data.frame(x = 1:3, y = 4:6)
  p <- ggplot() +
    geom_point(data = mpg, aes(displ, hwy)) +
    geom_point(data = local_df, aes(x, y))
  expect_equal(layer_data_index(p, local_df), 2L)
})

test_that("layer_data_index() returns NA when data is not found", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  other_df <- data.frame(x = 1)
  expect_identical(layer_data_index(p, other_df), NA_integer_)
})

# ---------------------------------------------------------------------------
# spec_layers() data_id column
# ---------------------------------------------------------------------------

test_that("spec_layers() includes data_id column (integer)", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_true("data_id" %in% names(result))
  expect_type(result$data_id, "integer")
})

test_that("spec_layers() layer 0 has data_id for global data", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_layers(p)
  layer0 <- result[result$layer == 0L, ]
  expect_false(is.na(layer0$data_id))
})

test_that("spec_layers() non-zero layers inheriting global data have NA data_id", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_layers(p)
  layer1 <- result[result$layer == 1L, ]
  expect_true(is.na(layer1$data_id))
})

test_that("spec_layers() non-zero layers with local data have non-NA data_id", {
  local_df <- data.frame(x = 1:3, y = 4:6)
  p <- ggplot() + geom_point(data = local_df, aes(x, y))
  result <- spec_layers(p)
  layer1 <- result[result$layer == 1L, ]
  expect_false(is.na(layer1$data_id))
})

test_that("spec_layers() correctly distinguishes global and local data in multi-dataset plot", {
  local_df <- data.frame(x = c(1, 3), y = c(5, 2))
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_point(data = local_df, aes(x, y))
  result <- spec_layers(p)
  # layer 0: mpg (data_id = integer), layer 1: inherits global (NA), layer 2: local_df
  non_zero <- result[result$layer > 0L, ]
  expect_true(is.na(non_zero$data_id[[1L]]))    # layer 1 inherits global
  expect_false(is.na(non_zero$data_id[[2L]]))   # layer 2 has local data
})
