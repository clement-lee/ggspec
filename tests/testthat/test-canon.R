# ---------------------------------------------------------------------------
# canon() — basic structure
# ---------------------------------------------------------------------------

test_that("canon() returns a ggspec_canon object", {
  p <- make_base_plot()
  result <- canon(p)
  expect_s3_class(result, "ggspec_canon")
})

test_that("ggspec_canon has required components", {
  p <- make_base_plot()
  result <- canon(p)
  expect_named(result, c("spec", "changes", "mode", "original"), ignore.order = TRUE)
  expect_s3_class(result$spec, "tbl_df")
  expect_s3_class(result$changes, "tbl_df")
  expect_named(result$changes, c("rule", "dimension", "layer", "from", "to"))
})

test_that("canon() accepts a spec_plot() tibble as input", {
  p <- make_base_plot()
  sp <- spec_plot(p)
  result <- canon(sp)
  expect_s3_class(result, "ggspec_canon")
})

test_that("canon() accepts a ggspec_canon object as input", {
  p <- make_base_plot()
  c1 <- canon(p)
  expect_no_error(canon(c1))
})

test_that("canon() errors on non-ggplot, non-canon, non-tibble input", {
  expect_error(canon(list()), class = "ggspec_not_ggplot")
  expect_error(canon(42),     class = "ggspec_not_ggplot")
})

# ---------------------------------------------------------------------------
# Idempotency: canon(canon(x)) == canon(x)
# ---------------------------------------------------------------------------

test_that("canon() is idempotent for structural mode", {
  p <- make_smooth_point_plot()  # layers out of alphabetical order
  c1 <- canon(p, mode = "structural")
  c2 <- canon(c1, mode = "structural")
  expect_equal(c1$spec, c2$spec)
  expect_equal(nrow(c2$changes), 0L)
})

test_that("canon() is idempotent for structural mode with coord_flip", {
  p <- make_flip_plot()
  c1 <- canon(p, mode = "structural")
  c2 <- canon(c1, mode = "structural")
  expect_equal(c1$spec, c2$spec)
  expect_equal(nrow(c2$changes), 0L)
})

test_that("canon() is idempotent for structural mode with scale name plot", {
  p <- make_scale_name_plot()
  c1 <- canon(p, mode = "structural")
  c2 <- canon(c1, mode = "structural")
  expect_equal(c1$spec, c2$spec)
  expect_equal(nrow(c2$changes), 0L)
})

test_that("canon() already-ordered plot records only fold_global change", {
  # point before smooth is already alphabetical; only fold_global fires
  p <- make_point_smooth_plot()
  c1 <- canon(p, mode = "structural")
  # fold_global fires because p has a global mapping (aes(displ, hwy))
  rule_names <- c1$changes$rule
  expect_false("layer_order" %in% rule_names)
  expect_true("fold_global" %in% rule_names)
})

# ---------------------------------------------------------------------------
# Transparency: $changes records what was normalised
# ---------------------------------------------------------------------------

test_that("layer reordering is recorded in $changes", {
  p <- make_smooth_point_plot()  # smooth before point — not alphabetical
  c1 <- canon(p, mode = "structural")
  expect_true(any(c1$changes$rule == "layer_order"))
})

test_that("canon() rejects visual mode", {
  p <- make_base_plot()
  expect_error(canon(p, mode = "visual"))
})

test_that("canon() rejects conceptual mode", {
  p <- make_base_plot()
  expect_error(canon(p, mode = "conceptual"))
})

# ---------------------------------------------------------------------------
# Layer ordering rule (structural mode)
# ---------------------------------------------------------------------------

test_that("canon() sorts non-zero layers alphabetically by geom in structural mode", {
  p <- make_smooth_point_plot()  # smooth, then point
  c1 <- canon(p, mode = "structural")
  # layer 0 is NA, then sorted non-zero layers
  expect_equal(c1$spec$geom, c(NA, "point", "smooth"))
})

test_that("canon() updates layer after reordering", {
  p <- make_smooth_point_plot()
  c1 <- canon(p, mode = "structural")
  # layer 0 stays 0, non-zero layers are renumbered 1:2
  expect_equal(c1$spec$layer, 0:2)
})

test_that("canon() updates aes_long layer after reordering", {
  p <- make_smooth_point_plot()
  c1 <- canon(p, mode = "structural")
  # After reorder, row 2 (point) should have aes_long with layer = 1
  # Row 3 (smooth) should have aes_long with layer = 2
  expect_true(all(c1$spec$aes_long[[2]]$layer == 1L))
  expect_true(all(c1$spec$aes_long[[3]]$layer == 2L))
})

# ---------------------------------------------------------------------------
# Coord flip: structural mode still records no coord_flip rule
# (coord_flip normalisation is now handled by .norm_coord_flip() in visual.R)
# ---------------------------------------------------------------------------

test_that("canon() structural mode does not apply coord_flip rule", {
  p <- make_flip_plot()
  c1 <- canon(p, mode = "structural")
  expect_false(any(c1$changes$rule == "coord_flip"))
})

# ---------------------------------------------------------------------------
# print method
# ---------------------------------------------------------------------------

test_that("print.ggspec_canon() produces output without error", {
  p <- make_smooth_point_plot()
  c1 <- canon(p)
  expect_output(print(c1), "CANON")
})

test_that("print.ggspec_canon() mentions number of changes", {
  p <- make_base_plot()
  c1 <- canon(p, mode = "structural")
  expect_output(print(c1), "change")
})
