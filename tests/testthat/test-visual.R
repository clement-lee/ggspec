# ---------------------------------------------------------------------------
# equiv_rendered() — basic
# ---------------------------------------------------------------------------

test_that("equiv_rendered() returns ggspec_result", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- equiv_rendered(p1, p2)
  expect_s3_class(result, "ggspec_result")
  expect_equal(result$check, "rendered")
})

test_that("equiv_rendered() passes for identical plots", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  expect_true(as.logical(equiv_rendered(p1, p2)))
})

test_that("equiv_rendered() passes for geom_bar vs geom_col on pre-counted data", {
  library(dplyr)
  p1 <- ggplot(mpg, aes(x = class)) + geom_bar()
  p2 <- mpg |> count(class) |> ggplot(aes(x = class, y = n)) + geom_col()
  expect_true(as.logical(equiv_rendered(p1, p2)))
})

test_that("equiv_rendered() fails for plots with different data", {
  p1 <- ggplot(mpg, aes(x = class)) + geom_bar()
  p2 <- ggplot(mpg, aes(x = drv))   + geom_bar()
  expect_false(as.logical(equiv_rendered(p1, p2)))
})

test_that("equiv_rendered() fails for different number of layers", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
  expect_false(as.logical(equiv_rendered(p1, p2)))
})

# ---------------------------------------------------------------------------
# .norm_coord_flip() — internal normaliser
# ---------------------------------------------------------------------------

test_that(".norm_coord_flip() returns unchanged plot without coord_flip", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  pn <- ggspec:::.norm_coord_flip(p)
  expect_identical(p$mapping, pn$mapping)
  expect_s3_class(pn$coordinates, "CoordCartesian")
})

test_that(".norm_coord_flip() swaps x/y in global mapping", {
  d <- data.frame(g = letters[1:3], v = 1:3)
  p <- ggplot(d, aes(x = g, y = v)) + geom_col() + coord_flip()
  pn <- ggspec:::.norm_coord_flip(p)
  expect_equal(rlang::as_label(pn$mapping$x), "v")
  expect_equal(rlang::as_label(pn$mapping$y), "g")
  expect_false(inherits(pn$coordinates, "CoordFlip"))
})

test_that(".norm_coord_flip() renames x->y when only x mapped", {
  p <- ggplot(mpg, aes(x = class)) + geom_bar() + coord_flip()
  pn <- ggspec:::.norm_coord_flip(p)
  expect_equal(rlang::as_label(pn$mapping$y), "class")
  expect_null(pn$mapping$x)
})

# ---------------------------------------------------------------------------
# .norm_scale_names() — internal normaliser
# ---------------------------------------------------------------------------

test_that(".norm_scale_names() moves scale name into p$labels", {
  p <- make_scale_name_plot()
  pn <- ggspec:::.norm_scale_names(p)
  expect_equal(pn$labels$fill, "Vehicle class")
})

# ---------------------------------------------------------------------------
# compare_visual() — high-level
# ---------------------------------------------------------------------------

test_that("compare_visual() returns ggspec_compare with mode visual", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- compare_visual(p1, p2)
  expect_s3_class(result, "ggspec_compare")
  expect_equal(result$mode, "visual")
})

test_that("compare_visual() passes for coord_flip vs swapped aesthetics", {
  p1 <- make_flip_plot()    # aes(x=g, y=v) + coord_flip
  p2 <- make_swapped_plot() # aes(x=v, y=g), no flip
  result <- compare_visual(p1, p2, check = c("rendered", "coord"))
  expect_true(as.logical(result))
})

test_that("compare_visual() passes for scale_fill name vs labs(fill=)", {
  p1 <- make_scale_name_plot()
  p2 <- make_labs_fill_plot()
  result <- compare_visual(p1, p2, check = "labels")
  expect_true(as.logical(result))
})

test_that("compare_visual() fails for plots with different data", {
  p1 <- ggplot(mpg, aes(x = class)) + geom_bar()
  p2 <- ggplot(mpg, aes(x = drv))   + geom_bar()
  result <- compare_visual(p1, p2, check = "rendered")
  expect_false(as.logical(result))
})

test_that("compare_visual() accepts subset of checks", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- compare_visual(p1, p2, check = "labels")
  expect_s3_class(result, "ggspec_compare")
})
