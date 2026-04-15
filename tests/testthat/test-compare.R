# ---------------------------------------------------------------------------
# compare_plots() — basic structure
# ---------------------------------------------------------------------------

test_that("compare_plots() returns a ggspec_compare object", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- compare_plots(p1, p2)
  expect_s3_class(result, "ggspec_compare")
  expect_s3_class(result, "ggspec_result")
})

test_that("compare_plots() result has canon_p1, canon_p2, mode fields", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- compare_plots(p1, p2)
  expect_s3_class(result$canon_p1, "ggspec_canon")
  expect_s3_class(result$canon_p2, "ggspec_canon")
  expect_equal(result$mode, "structural")
})

test_that("compare_plots() passes for identical plots", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  expect_true(as.logical(compare_plots(p1, p2)))
})

test_that("compare_plots() errors for non-ggplot input", {
  p <- make_base_plot()
  expect_error(compare_plots(list(), p), class = "ggspec_not_ggplot")
  expect_error(compare_plots(p, list()), class = "ggspec_not_ggplot")
})

# ---------------------------------------------------------------------------
# geom_col ≡ geom_bar(stat = "identity") — equivalent after canonicalisation
# ---------------------------------------------------------------------------

test_that("geom_col and geom_bar(stat='identity') differ at spec level", {
  # geom_col() uses GeomCol; geom_bar() uses GeomBar — different geom names
  p1 <- make_col_plot()
  p2 <- make_bar_identity_plot()
  expect_false(as.logical(equiv_layers(p1, p2, exact = TRUE)))
})

test_that("compare_plots() detects geom_col / geom_bar(stat='identity') equivalence", {
  p1 <- make_col_plot()
  p2 <- make_bar_identity_plot()
  expect_true(as.logical(compare_plots(p1, p2, check = "layers")))
})

# ---------------------------------------------------------------------------
# Layer order equivalence (structural mode)
# ---------------------------------------------------------------------------

test_that("equiv_layers exact=TRUE fails for different layer order", {
  p1 <- make_point_smooth_plot()
  p2 <- make_smooth_point_plot()
  expect_false(as.logical(equiv_layers(p1, p2, exact = TRUE)))
})

test_that("compare_plots() passes for different layer order in structural mode", {
  p1 <- make_point_smooth_plot()
  p2 <- make_smooth_point_plot()
  expect_true(as.logical(compare_plots(p1, p2, mode = "structural", check = "layers")))
})

test_that("compare_plots() passes for different layer order in visual mode", {
  p1 <- make_point_smooth_plot()
  p2 <- make_smooth_point_plot()
  expect_true(as.logical(compare_plots(p1, p2, mode = "visual", check = "layers")))
})

# ---------------------------------------------------------------------------
# Aesthetic inheritance equivalence
# ---------------------------------------------------------------------------

test_that("global aes vs local aes are already equivalent at spec level", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg) + geom_point(aes(displ, hwy))
  # equiv_aes with inherit="resolve" already handles this
  expect_true(as.logical(equiv_aes(p1, p2)))
})

test_that("compare_plots() passes for global vs local aes mapping", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg) + geom_point(aes(displ, hwy))
  expect_true(as.logical(compare_plots(p1, p2, check = "aes")))
})

# ---------------------------------------------------------------------------
# Coord flip / aes swap equivalence (visual mode)
# ---------------------------------------------------------------------------

test_that("equiv_coord fails for coord_flip vs cartesian", {
  p1 <- make_flip_plot()
  p2 <- make_swapped_plot()
  expect_false(as.logical(equiv_coord(p1, p2)))
})

test_that("compare_plots() passes for coord_flip vs swapped aes in visual mode", {
  p1 <- make_flip_plot()   # aes(x=g, y=v) + coord_flip
  p2 <- make_swapped_plot() # aes(x=v, y=g) + no flip
  result <- compare_plots(p1, p2, mode = "visual", check = c("layers", "aes", "coord"))
  expect_true(as.logical(result))
})

test_that("compare_plots() fails for coord_flip vs swapped aes in structural mode", {
  # structural mode doesn't absorb coord_flip
  p1 <- make_flip_plot()
  p2 <- make_swapped_plot()
  result <- compare_plots(p1, p2, mode = "structural", check = "coord")
  expect_false(as.logical(result))
})

# ---------------------------------------------------------------------------
# Scale name vs labs equivalence (visual mode)
# ---------------------------------------------------------------------------

test_that("equiv_labels fails when reference uses labs but observation uses scale name", {
  # p1 (reference) has fill label in p$labels via labs(); spec_labels() sees it.
  # p2 (observation) has fill label only in the scale object; spec_labels() returns
  # nothing for fill.  The left-join on p1's label detects label_obs = NA -> FAIL.
  p1 <- make_labs_fill_plot()
  p2 <- make_scale_name_plot()
  result_direct <- equiv_labels(p1, p2, aesthetics = "fill")
  expect_false(as.logical(result_direct))
})

test_that("compare_plots() passes for scale name vs labs(fill=) in visual mode", {
  # Either direction works: visual mode promotes scale names into the labels table
  # before comparison, so both plots end up with fill = "Vehicle class" in labels.
  p1 <- make_labs_fill_plot()
  p2 <- make_scale_name_plot()
  result <- compare_plots(p1, p2, mode = "visual", check = c("scales", "labels"))
  expect_true(as.logical(result))
})

test_that("compare_plots() fails for scale name vs labs in structural mode", {
  # Structural mode does not apply .rule_scale_name_to_labels, so the scale-name
  # plot's fill label remains absent from spec_labels() -> same failure as direct.
  p1 <- make_labs_fill_plot()
  p2 <- make_scale_name_plot()
  result <- compare_plots(p1, p2, mode = "structural", check = "labels")
  expect_false(as.logical(result))
})

# ---------------------------------------------------------------------------
# check_plot() with mode argument
# ---------------------------------------------------------------------------

test_that("check_plot() passes silently in structural mode for reordered layers", {
  p1 <- make_point_smooth_plot()
  p2 <- make_smooth_point_plot()
  expect_invisible(check_plot(p2, p1, check = "layers", mode = "structural"))
})

test_that("check_plot() errors in structural mode for genuinely wrong geom", {
  ref <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  obs <- ggplot(mpg, aes(displ, hwy)) + geom_line()
  expect_error(check_plot(obs, ref, check = "layers", mode = "structural"))
})

test_that("check_plot() passes in visual mode for coord_flip equivalence", {
  p1 <- make_flip_plot()
  p2 <- make_swapped_plot()
  expect_invisible(
    check_plot(p2, p1, check = c("layers", "aes", "coord"), mode = "visual")
  )
})

# ---------------------------------------------------------------------------
# print method
# ---------------------------------------------------------------------------

test_that("print.ggspec_compare() produces output without error", {
  p1 <- make_base_plot()
  p2 <- make_base_plot()
  result <- compare_plots(p1, p2)
  expect_output(print(result), "mode=")
})
