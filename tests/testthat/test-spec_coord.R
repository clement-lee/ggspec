test_that("spec_coord() returns 'cartesian' for default plot", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_coord(p)
  expect_equal(result$coord_type, "cartesian")
})

test_that("spec_coord() detects coord_flip()", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + coord_flip()
  result <- spec_coord(p)
  expect_equal(result$coord_type, "flip")
})

test_that("spec_coord() detects coord_polar()", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + coord_polar()
  result <- spec_coord(p)
  expect_equal(result$coord_type, "polar")
})

test_that("spec_coord() returns a single-row tibble", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_coord(p)
  expect_equal(nrow(result), 1L)
})

test_that("spec_coord() returns correct column names", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_coord(p)
  expect_named(result, c("coord_type", "xlim", "ylim", "expand", "clip"))
})
