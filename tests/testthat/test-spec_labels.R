test_that("spec_labels() returns a tibble with aesthetic and label columns", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    labs(title = "Test", x = "Displacement")
  result <- spec_labels(p)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("aesthetic", "label"))
})

test_that("spec_labels() captures title", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    labs(title = "My Title")
  result <- spec_labels(p)
  title_row <- result[result$aesthetic == "title", ]
  expect_equal(title_row$label, "My Title")
})

test_that("spec_labels() captures x and y axis labels", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    labs(x = "Displacement (L)", y = "Highway MPG")
  result <- spec_labels(p)
  x_row <- result[result$aesthetic == "x", ]
  y_row <- result[result$aesthetic == "y", ]
  expect_equal(x_row$label, "Displacement (L)")
  expect_equal(y_row$label, "Highway MPG")
})
