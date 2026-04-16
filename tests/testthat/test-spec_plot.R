test_that("spec_plot() returns a tibble with one row per layer plus layer 0", {
  p <- make_base_plot()
  result <- spec_plot(p)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3L)  # layer 0 + 2 layers
})

test_that("spec_plot() includes all expected columns", {
  p <- make_base_plot()
  result <- spec_plot(p)
  expected_cols <- c("layer", "geom", "stat", "position",
                     "mapping", "params", "inherit_aes", "data_id",
                     "aes_long", "datasets", "scales", "facets", "coord", "labels")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("spec_plot() facets list-column contains facet spec", {
  p <- make_faceted_plot()
  result <- spec_plot(p)
  facets <- result$facets[[1L]]
  expect_s3_class(facets, "tbl_df")
  expect_equal(facets$facet_type, "wrap")
})

test_that("spec_plot() labels list-column contains label spec", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    labs(title = "Test")
  result <- spec_plot(p)
  labels <- result$labels[[1L]]
  expect_s3_class(labels, "tbl_df")
  title_row <- labels[labels$aesthetic == "title", ]
  expect_equal(title_row$label, "Test")
})

test_that("spec_plot() returns one-row tibble (layer 0) for plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- spec_plot(p)
  expect_equal(nrow(result), 1L)
  expect_equal(result$layer, 0L)
})

test_that("spec_plot() datasets list-column contains spec_data result", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_plot(p)
  datasets <- result$datasets[[1L]]
  expect_s3_class(datasets, "tbl_df")
  expect_true("data_id" %in% names(datasets))
  expect_true("label"   %in% names(datasets))
})
