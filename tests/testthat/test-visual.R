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

# ---------------------------------------------------------------------------
# .norm_scale_names() — NULL name
# ---------------------------------------------------------------------------

test_that(".norm_scale_names() makes scale(name=NULL) equivalent to labs(x=NULL)", {
  p1 <- make_point_plot() + scale_x_continuous(name = NULL)
  p2 <- make_point_plot() + labs(x = NULL)
  expect_true(as.logical(compare_visual(p1, p2, check = "labels")))
})

# ---------------------------------------------------------------------------
# .norm_guide_labels()
# ---------------------------------------------------------------------------

test_that(".norm_guide_labels() makes guides(colour=guide_legend()) equivalent to scale name", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = "Species")
  p2 <- make_point_colour_plot() + guides(colour = guide_legend("Species"))
  expect_true(as.logical(compare_visual(p1, p2, check = "labels")))
})

test_that(".norm_guide_labels() makes guides(colour=guide_legend(NULL)) equivalent to labs(colour=NULL)", {
  p1 <- make_point_colour_plot() + guides(colour = guide_legend(NULL))
  p2 <- make_point_colour_plot() + labs(colour = NULL)
  expect_true(as.logical(compare_visual(p1, p2, check = "labels")))
})

# ---------------------------------------------------------------------------
# .norm_theme_labels()
# ---------------------------------------------------------------------------

test_that(".norm_theme_labels() makes theme(axis.title.x=element_blank()) equivalent to labs(x=NULL)", {
  p1 <- make_point_plot() + theme(axis.title.x = element_blank())
  p2 <- make_point_plot() + labs(x = NULL)
  expect_true(as.logical(compare_visual(p1, p2, check = "labels")))
})

test_that(".norm_theme_labels() makes theme(legend.title=element_blank()) equivalent to scale name NULL", {
  p1 <- make_point_colour_plot() + theme(legend.title = element_blank())
  p2 <- make_point_colour_plot() + scale_colour_discrete(name = NULL)
  expect_true(as.logical(compare_visual(p1, p2, check = "labels")))
})

# ---------------------------------------------------------------------------
# equiv_rendered() — path-order sensitivity
# ---------------------------------------------------------------------------

test_that("equiv_rendered() correctly equates time-sorted path and line", {
  p1 <- ggplot(economics, aes(x = date, y = pce)) + geom_path()
  p2 <- ggplot(economics, aes(x = date, y = pce)) + geom_line()
  expect_true(as.logical(equiv_rendered(p1, p2)))
})

test_that("equiv_rendered() distinguishes unsorted path from line", {
  p1 <- ggplot(economics, aes(x = psavert, y = pce)) + geom_path()
  p2 <- ggplot(economics, aes(x = psavert, y = pce)) + geom_line()
  expect_false(as.logical(equiv_rendered(p1, p2)))
})

# ---------------------------------------------------------------------------
# compare_visual() — non-spanning layer order transparency
# ---------------------------------------------------------------------------

test_that("compare_visual() passes for geom_smooth(se=FALSE)+point vs point+geom_smooth(se=FALSE)", {
  expect_true(as.logical(compare_visual(
    make_smooth_point_plot(), make_point_smooth_plot(), check = "rendered")))
})

test_that("compare_visual() fails for geom_smooth(se=TRUE)+point vs point+geom_smooth(se=TRUE)", {
  expect_false(as.logical(compare_visual(
    plot_smooth_then_point(), plot_point_then_smooth(), check = "rendered")))
})

test_that("compare_plots(mode='visual') passes for geom_smooth(se=FALSE)+point vs reversed", {
  expect_true(as.logical(compare_plots(
    make_smooth_point_plot(), make_point_smooth_plot(), mode = "visual")))
})
