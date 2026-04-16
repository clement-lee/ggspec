test_that("spec_layers() returns a tibble with one row per layer plus layer 0", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3L)  # layer 0 + 2 layers
})

test_that("spec_layers() returns correct geom names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$geom, c(NA, "point", "smooth"))
})

test_that("spec_layers() returns correct stat names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$stat[[2L]], "identity")
  expect_equal(result$stat[[3L]], "smooth")
})

test_that("spec_layers() returns correct column names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_named(result, c("layer", "geom", "stat", "position",
                          "mapping", "params", "inherit_aes", "data_id"))
})

test_that("spec_layers() returns one row (layer 0) for plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- spec_layers(p)
  expect_equal(nrow(result), 1L)
  expect_equal(result$layer, 0L)
  expect_named(result, c("layer", "geom", "stat", "position",
                          "mapping", "params", "inherit_aes", "data_id"))
})

test_that("spec_layers() mapping column is a list of character vectors", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- spec_layers(p)
  expect_type(result$mapping, "list")
  # Non-zero layer row (row 2, layer 1) has the colour mapping
  mapping <- result$mapping[[2L]]
  expect_type(mapping, "character")
  expect_true("colour" %in% names(mapping))
  expect_equal(unname(mapping["colour"]), "class")
})

test_that("spec_layers() layer column starts at 0 and is sequential", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$layer, 0:2)
})

test_that("spec_layers() inherit = FALSE returns only local mappings for non-zero layers", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(colour = class)) +
    geom_smooth()
  result_local  <- spec_layers(p, inherit = FALSE)
  result_resolve <- spec_layers(p, inherit = "resolve")

  # smooth layer (row 3) has no local mappings
  local_smooth   <- result_local$mapping[[3L]]
  resolve_smooth <- result_resolve$mapping[[3L]]
  expect_equal(length(local_smooth), 0L)
  expect_gt(length(resolve_smooth), 0L)
})
