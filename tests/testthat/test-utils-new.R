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

test_that("flat_mappings() returns empty data.frame for plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- flat_mappings(p)
  expect_equal(nrow(result), 0L)
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
# spec_layers() data_source column
# ---------------------------------------------------------------------------

test_that("spec_layers() includes data_source column", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_true("data_source" %in% names(result))
})

test_that("spec_layers() marks global-data layers as 'global'", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_layers(p)
  expect_equal(result$data_source, "global")
})

test_that("spec_layers() marks local-data layers as 'local'", {
  local_df <- data.frame(x = 1:3, y = 4:6)
  p <- ggplot() + geom_point(data = local_df, aes(x, y))
  result <- spec_layers(p)
  expect_equal(result$data_source, "local")
})

test_that("spec_layers() correctly distinguishes global and local data in multi-dataset plot", {
  local_df <- data.frame(x = c(1, 3), y = c(5, 2))
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_point(data = local_df, aes(x, y))
  result <- spec_layers(p)
  expect_equal(result$data_source, c("global", "local"))
})
