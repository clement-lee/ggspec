## Rule: mapping & data specified in different layers are equivalent WHEN canonised.

## Equivalent according to following modes?
## strict: true
## structural: true
## visual: true
## conceptual: true

## Examples
test_that("Mapping & data specified in different layers are strictly, structurally, visually and conceptually equivalent when canonised", {
  p1 <- make_point_plot()
  p2 <- penguins |>
    ggplot(aes(x = flipper_len)) +
    geom_point(aes(y = body_mass))
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Mapping & data specified in different layers are strictly, structurally, visually and conceptually equivalent when canonised", {
  p1 <- make_point_plot()
  p2 <- penguins |>
    ggplot(aes(y = body_mass)) +
    geom_point(aes(x = flipper_len))
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Mapping & data specified in different layers are strictly, structurally, visually and conceptually equivalent when canonised", {
  p1 <- make_point_plot()
  p2 <- penguins |>
    ggplot() +
    geom_point(aes(x = flipper_len, y = body_mass))
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Mapping & data specified in different layers are strictly, structurally, visually and conceptually equivalent when canonised", {
  p1 <- make_point_plot()
  p2 <- ggplot() +
    geom_point(aes(x = flipper_len, y = body_mass), data = penguins)
  expect_true(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

