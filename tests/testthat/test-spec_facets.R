test_that("spec_facets() returns facet_type 'null' for unfaceted plot", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_facets(p)
  expect_equal(result$facet_type, "null")
})

test_that("spec_facets() returns facet_type 'wrap' for facet_wrap()", {
  p <- make_faceted_plot()
  result <- spec_facets(p)
  expect_equal(result$facet_type, "wrap")
})

test_that("spec_facets() captures wrap variable", {
  p <- make_faceted_plot()
  result <- spec_facets(p)
  expect_true(grepl("class", result$cols))
})

test_that("spec_facets() returns facet_type 'grid' for facet_grid()", {
  p <- make_grid_plot()
  result <- spec_facets(p)
  expect_equal(result$facet_type, "grid")
})

test_that("spec_facets() captures grid row and col variables", {
  p <- make_grid_plot()
  result <- spec_facets(p)
  expect_true(grepl("drv", result$rows))
  expect_true(grepl("cyl", result$cols))
})

test_that("spec_facets() returns a single-row tibble", {
  p <- make_faceted_plot()
  result <- spec_facets(p)
  expect_equal(nrow(result), 1L)
})

test_that("spec_facets() column names are correct", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_facets(p)
  expect_named(result, c("facet_type", "rows", "cols", "scales", "space", "labeller"))
})
