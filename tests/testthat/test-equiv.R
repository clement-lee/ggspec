test_that("equiv_layers() passes when plots have the same geoms", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_layers(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_layers() passes (subset) when p2 has more layers", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- p1 + geom_smooth()
  result <- equiv_layers(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_layers() fails (exact) when p2 has more layers", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- p1 + geom_smooth()
  result <- equiv_layers(p1, p2, exact = TRUE)
  expect_false(result$pass)
})

test_that("equiv_layers() fails when expected geom is missing", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_layers(p1, p2)
  expect_false(result$pass)
  expect_match(result$message, "smooth")
})

test_that("equiv_aes() passes when mappings are equal", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_aes(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_aes() fails when a mapping is different", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(cty,   hwy)) + geom_point()
  result <- equiv_aes(p1, p2)
  expect_false(result$pass)
})

test_that("equiv_facets() passes for matching facets", {
  p1 <- make_faceted_plot()
  p2 <- make_faceted_plot()
  result <- equiv_facets(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_facets() fails for different facet types", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_facets(p1, p2)
  expect_false(result$pass)
})

test_that("equiv_labels() passes for matching labels", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + labs(title = "Test")
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + labs(title = "Test")
  result <- equiv_labels(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_labels() fails for different title", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + labs(title = "A")
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + labs(title = "B")
  result <- equiv_labels(p1, p2)
  expect_false(result$pass)
})

test_that("equiv_coord() passes for same coordinate system", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_coord(p1, p2)
  expect_true(result$pass)
})

test_that("equiv_coord() fails for different coordinate systems", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- p1 + coord_flip()
  result <- equiv_coord(p1, p2)
  expect_false(result$pass)
})

test_that("equiv_plot() returns combined result", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_plot(p1, p2)
  expect_s3_class(result, "ggspec_result")
  expect_true(result$pass)
})

test_that("equiv_plot() fails when any check fails", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + coord_flip()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- equiv_plot(p1, p2, check = c("layers", "coord"))
  expect_false(result$pass)
})

test_that("as.logical() on ggspec_result works", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_true(as.logical(equiv_layers(p1, p2)))
})
