test_that("spec_layers() returns a tibble with one row per layer", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
})

test_that("spec_layers() returns correct geom names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$geom, c("point", "smooth"))
})

test_that("spec_layers() returns correct stat names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$stat[[1L]], "identity")
  expect_equal(result$stat[[2L]], "smooth")
})

test_that("spec_layers() returns correct column names", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_named(result, c("layer_idx", "geom", "stat", "position",
                          "mapping", "params", "inherit_aes"))
})

test_that("spec_layers() returns empty tibble for plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- spec_layers(p)
  expect_equal(nrow(result), 0L)
  expect_named(result, c("layer_idx", "geom", "stat", "position",
                          "mapping", "params", "inherit_aes"))
})

test_that("spec_layers() mapping column is a list of character vectors", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- spec_layers(p)
  expect_type(result$mapping, "list")
  mapping <- result$mapping[[1L]]
  expect_type(mapping, "character")
  expect_true("colour" %in% names(mapping))
  expect_equal(unname(mapping["colour"]), "class")
})

test_that("spec_layers() layer_idx is 1-based sequential integers", {
  p <- make_base_plot()
  result <- spec_layers(p)
  expect_equal(result$layer_idx, 1:2)
})

test_that("spec_layers() inherit = FALSE returns only local mappings", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(colour = class)) +
    geom_smooth()
  result_local  <- spec_layers(p, inherit = FALSE)
  result_resolve <- spec_layers(p, inherit = "resolve")

  # smooth layer has no local mappings
  local_smooth   <- result_local$mapping[[2L]]
  resolve_smooth <- result_resolve$mapping[[2L]]
  expect_equal(length(local_smooth), 0L)
  expect_gt(length(resolve_smooth), 0L)
})
