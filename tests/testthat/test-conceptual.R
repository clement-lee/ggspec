# ---------------------------------------------------------------------------
# compare_conceptual() — high-level dispatch
# ---------------------------------------------------------------------------

test_that("compare_conceptual() returns ggspec_compare with mode conceptual", {
  p1 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
  p2 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_violin()
  result <- compare_conceptual(p1, p2)
  expect_s3_class(result, "ggspec_compare")
  expect_equal(result$mode, "conceptual")
})

test_that("compare_conceptual() passes for boxplot vs violin (same variables)", {
  p1 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
  p2 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_violin()
  expect_true(as.logical(compare_conceptual(p1, p2)))
})

test_that("compare_conceptual() passes for boxplot vs jitter (same variables)", {
  p1 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
  p2 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_jitter()
  expect_true(as.logical(compare_conceptual(p1, p2)))
})

test_that("compare_conceptual() fails for boxplot vs violin with different y", {
  p1 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
  p2 <- ggplot(mpg, aes(x = class, y = cty)) + geom_violin()
  expect_false(as.logical(compare_conceptual(p1, p2)))
})

# ---------------------------------------------------------------------------
# 1-D distribution family
# ---------------------------------------------------------------------------

test_that("compare_conceptual() passes for density vs histogram (same variable)", {
  p1 <- ggplot(mpg, aes(x = hwy)) + geom_density()
  p2 <- ggplot(mpg, aes(x = hwy)) + geom_histogram()
  expect_true(as.logical(compare_conceptual(p1, p2)))
})

test_that("compare_conceptual() passes for histogram vs freqpoly (same variable)", {
  p1 <- ggplot(mpg, aes(x = hwy)) + geom_histogram()
  p2 <- ggplot(mpg, aes(x = hwy)) + geom_freqpoly()
  expect_true(as.logical(compare_conceptual(p1, p2)))
})

test_that("compare_conceptual() fails for density with different variables", {
  p1 <- ggplot(mpg, aes(x = hwy)) + geom_density()
  p2 <- ggplot(mpg, aes(x = cty)) + geom_density()
  expect_false(as.logical(compare_conceptual(p1, p2)))
})

# ---------------------------------------------------------------------------
# 2-D count family
# ---------------------------------------------------------------------------

test_that("compare_conceptual() passes for geom_count vs geom_point(aes(size=n))", {
  library(dplyr)
  p1 <- ggplot(mpg, aes(x = class, y = drv)) + geom_count()
  p2 <- mpg |> count(class, drv) |>
    ggplot(aes(x = class, y = drv, size = n)) + geom_point()
  expect_true(as.logical(compare_conceptual(p1, p2)))
})

test_that("compare_conceptual() fails for geom_count with different variables", {
  p1 <- ggplot(mpg, aes(x = class, y = drv))   + geom_count()
  p2 <- ggplot(mpg, aes(x = class, y = trans)) + geom_count()
  expect_false(as.logical(compare_conceptual(p1, p2)))
})

# ---------------------------------------------------------------------------
# compare_plots(mode = "conceptual") dispatches correctly
# ---------------------------------------------------------------------------

test_that("compare_plots(mode='conceptual') dispatches to compare_conceptual()", {
  p1 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot()
  p2 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_violin()
  result <- compare_plots(p1, p2, mode = "conceptual")
  expect_equal(result$mode, "conceptual")
  expect_true(as.logical(result))
})
