test_that("spec_scales() returns empty tibble for plot with no explicit scales", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_scales(p)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("spec_scales() returns one row per explicitly added scale", {
  p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point() +
    scale_colour_brewer(palette = "Set1") +
    scale_x_log10()
  result <- spec_scales(p)
  expect_equal(nrow(result), 2L)
})

test_that("spec_scales() returns correct column names", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_x_log10()
  result <- spec_scales(p)
  expect_named(result, c("aesthetic", "scale_class", "scale_type",
                          "name", "transform", "guide"))
})

test_that("spec_scales() reports log10 transform", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_x_log10()
  result <- spec_scales(p)
  x_scale <- result[grepl("x", result$aesthetic), ]
  expect_false(is.na(x_scale$transform))
  expect_true(grepl("log", x_scale$transform, ignore.case = TRUE))
})
