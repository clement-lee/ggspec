## Rule: specifying or removing axis titles by scale_* and labs and theme are strictly and structurally not equivalent, but visually and conceptually equivalent.

## Equivalent according to the following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
test_that("Specifying axis titles by scale_* and labs are visually and conceptually equivalent", {
  p1 <- make_point_plot() + scale_x_continuous(name = "Flipper length (mm)")
  p2 <- make_point_plot() + labs(x = "Flipper length (mm)")
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing axis titles by scale_* and labs are visually and conceptually equivalent", {
  p1 <- make_point_plot() + scale_x_continuous(name = NULL)
  p2 <- make_point_plot() + labs(x = NULL)
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing axis titles by scale_* and theme are visually and conceptually equivalent", {
  p1 <- make_point_plot() + scale_x_continuous(name = NULL)
  p2 <- make_point_plot() + theme(axis.title.x = element_blank())
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing axis titles by labs and theme are visually and conceptually equivalent", {
  p1 <- make_point_plot() + labs(x = NULL)
  p2 <- make_point_plot() + theme(axis.title.x = element_blank())
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})


## Rule: specifying legend titles by scale_* and labs and theme and guides are strictly and structurally not equivalent, but visually and conceptually equivalent.

## Equivalent according to the following modes?
## strict: false
## structural: false
## visual: true
## conceptual: true

## Examples
## colour, non-null (scale, labs, guides)
test_that("Specifying legend title by scale_* and labs are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = "Species")
  p2 <- make_point_colour_plot() + labs(colour = "Species")
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Specifying legend title by scale_* and guides are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = "Species")
  p2 <- make_point_colour_plot() + guides(colour = guide_legend("Species"))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Specifying legend title by labs and guides are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + labs(colour = "Species")
  p2 <- make_point_colour_plot() + guides(colour = guide_legend("Species"))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

## colour, null (scale, labs, guides, theme)
test_that("Removing legend title by scale_* and labs are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = NULL)
  p2 <- make_point_colour_plot() + labs(colour = NULL)
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing legend title by scale_* and guides are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = NULL)
  p2 <- make_point_colour_plot() + guides(colour = guide_legend(NULL))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing legend title by scale_* and theme are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + scale_colour_discrete(name = NULL)
  p2 <- make_point_colour_plot() + theme(legend.title = element_blank())
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing legend title by labs and guides are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + labs(colour = NULL)
  p2 <- make_point_colour_plot() + guides(colour = guide_legend(NULL))
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing legend title by labs and theme are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + labs(colour = NULL)
  p2 <- make_point_colour_plot() + theme(legend.title = element_blank())
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

test_that("Removing legend title by guides and theme are visually and conceptually equivalent", {
  p1 <- make_point_colour_plot() + guides(colour = guide_legend(NULL))
  p2 <- make_point_colour_plot() + theme(legend.title = element_blank())
  expect_false(as.logical(compare_plots(p1, p2, mode = "strict")))
  expect_false(as.logical(compare_plots(p1, p2, mode = "structural")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual")))
  expect_true(as.logical(compare_plots(p1, p2, mode = "conceptual")))
  expect_identical(p1$data, p2$data)
})

