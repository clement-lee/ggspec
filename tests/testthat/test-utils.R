test_that("is_ggplot() returns TRUE for ggplot objects", {
  p <- ggplot(mpg, aes(displ, hwy))
  expect_true(is_ggplot(p))
})

test_that("is_ggplot() returns FALSE for non-ggplot objects", {
  expect_false(is_ggplot(list()))
  expect_false(is_ggplot(NULL))
  expect_false(is_ggplot(42))
})

test_that("assert_ggplot() passes silently for ggplot objects", {
  p <- ggplot(mpg, aes(displ, hwy))
  expect_invisible(assert_ggplot(p))
})

test_that("assert_ggplot() errors informatively for non-ggplot objects", {
  expect_error(assert_ggplot(list()), class = "ggspec_not_ggplot")
  expect_error(assert_ggplot(42),     class = "ggspec_not_ggplot")
})

test_that("n_layers() counts layers correctly", {
  p0 <- ggplot(mpg, aes(displ, hwy))
  p1 <- p0 + geom_point()
  p2 <- p1 + geom_smooth()
  expect_equal(n_layers(p0), 0L)
  expect_equal(n_layers(p1), 1L)
  expect_equal(n_layers(p2), 2L)
})

test_that("has_layer() detects layers by geom", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_true(has_layer(p, geom = "point"))
  expect_false(has_layer(p, geom = "smooth"))
})

test_that("has_layer() is case-insensitive", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_true(has_layer(p, geom = "Point"))
  expect_true(has_layer(p, geom = "POINT"))
})

test_that("has_layer() detects layers by stat", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  expect_true(has_layer(p, stat = "smooth"))
  expect_false(has_layer(p, stat = "identity"))
})

test_that("has_layer() returns FALSE for a plot with no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  expect_false(has_layer(p, geom = "point"))
})
