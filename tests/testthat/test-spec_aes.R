test_that("spec_aes() returns a tibble in long format", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- spec_aes(p)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("layer_idx", "geom", "aesthetic", "variable", "source"))
})

test_that("spec_aes() captures global aesthetics resolved into layers", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_aes(p, layer = 1L)
  expect_true("x" %in% result$aesthetic)
  expect_true("y" %in% result$aesthetic)
  x_row <- result[result$aesthetic == "x", ]
  expect_equal(x_row$variable, "displ")
})

test_that("spec_aes() captures local-only aesthetics with source = 'local'", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = class))
  result <- spec_aes(p, layer = 1L)
  colour_row <- result[result$aesthetic == "colour", ]
  expect_equal(colour_row$source, "local")
  expect_equal(colour_row$variable, "class")
})

test_that("spec_aes() layer argument filters correctly", {
  p <- make_base_plot()
  all_layers <- spec_aes(p)
  layer1     <- spec_aes(p, layer = 1L)
  expect_true(all(layer1$layer_idx == 1L))
  expect_true(nrow(layer1) < nrow(all_layers))
})

test_that("spec_aes() returns empty tibble when plot has no layers", {
  p <- ggplot(mpg, aes(displ, hwy))
  result <- spec_aes(p)
  expect_equal(nrow(result), 0L)
})

test_that("spec_aes() source is 'resolved' for aesthetics present in both global and local", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(x = cty))  # override x locally
  result <- spec_aes(p, layer = 1L)
  x_row  <- result[result$aesthetic == "x", ]
  expect_equal(x_row$source, "resolved")
  expect_equal(x_row$variable, "cty")  # local takes precedence
})
