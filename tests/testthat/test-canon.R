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

test_that("canon() is idempotent for visual mode with coord_flip", {
  p <- make_flip_plot()
  c1 <- canon(p, mode = "visual")
  c2 <- canon(c1, mode = "visual")
  expect_equal(c1$spec, c2$spec)
  expect_equal(nrow(c2$changes), 0L)
})

test_that("canon() is idempotent for visual mode with scale name", {
  p <- make_scale_name_plot()
  c1 <- canon(p, mode = "visual")
  c2 <- canon(c1, mode = "visual")
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

test_that("coord_flip normalisation is recorded in $changes", {
  p <- make_flip_plot()
  c1 <- canon(p, mode = "visual")
  expect_true(any(c1$changes$rule == "coord_flip"))
})

test_that("scale name promotion is recorded in $changes", {
  p <- make_scale_name_plot()
  c1 <- canon(p, mode = "visual")
  expect_true(any(c1$changes$rule == "scale_name_to_labels"))
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
# Coord flip rule (visual mode)
# ---------------------------------------------------------------------------

test_that("canon() converts coord_flip to cartesian in visual mode", {
  p <- make_flip_plot()
  c1 <- canon(p, mode = "visual")
  expect_equal(c1$spec$coord[[2]]$coord_type, "default")  # cartesian -> default by next rule
})

test_that("canon() swaps x and y aesthetics when flipping", {
  p <- make_flip_plot()  # aes(x = g, y = v) + coord_flip
  c1 <- canon(p, mode = "visual")
  # First non-zero layer is row 2
  layer1_mapping <- c1$spec$mapping[[2]]
  expect_equal(unname(layer1_mapping["x"]), "v")
  expect_equal(unname(layer1_mapping["y"]), "g")
})

test_that("canon() handles coord_flip when only y is mapped (no x)", {
  # geom_bar(aes(y = class)) + coord_flip is a common teaching pattern
  p <- ggplot(mpg, aes(y = class)) + geom_bar() + coord_flip()
  c1 <- canon(p, mode = "visual")
  # y should be renamed to x (no x to swap with); check non-zero layer row
  non_zero_rows <- c1$spec[c1$spec$layer > 0L, ]
  layer1_mapping <- non_zero_rows$mapping[[1]]
  expect_equal(unname(layer1_mapping["x"]), "class")
  expect_false("y" %in% names(layer1_mapping) && !is.na(layer1_mapping[["y"]]))
})

test_that("canon() handles coord_flip when only x is mapped (no y)", {
  p <- ggplot(mpg, aes(x = class)) + geom_bar() + coord_flip()
  c1 <- canon(p, mode = "visual")
  non_zero_rows <- c1$spec[c1$spec$layer > 0L, ]
  layer1_mapping <- non_zero_rows$mapping[[1]]
  expect_equal(unname(layer1_mapping["y"]), "class")
  expect_false("x" %in% names(layer1_mapping) && !is.na(layer1_mapping[["x"]]))
})

# ---------------------------------------------------------------------------
# Scale name -> labels rule (visual mode)
# ---------------------------------------------------------------------------

test_that("canon() moves scale name to labels in visual mode", {
  p <- make_scale_name_plot()
  c1 <- canon(p, mode = "visual")
  labels <- c1$spec$labels[[2]]  # first non-zero layer
  fill_label <- labels$label[labels$aesthetic == "fill"]
  expect_equal(fill_label, "Vehicle class")
})

test_that("canon() sets scale name to NA after promotion", {
  p <- make_scale_name_plot()
  c1 <- canon(p, mode = "visual")
  scales <- c1$spec$scales[[2]]  # first non-zero layer
  fill_name <- scales$name[grepl("fill", scales$aesthetic)]
  expect_true(all(is.na(fill_name)))
})

# ---------------------------------------------------------------------------
# Default coord rule (visual mode)
# ---------------------------------------------------------------------------

test_that("canon() marks default cartesian coord as 'default' in visual mode", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  c1 <- canon(p, mode = "visual")
  expect_equal(c1$spec$coord[[2]]$coord_type, "default")  # first non-zero layer
})

# ---------------------------------------------------------------------------
# Pedagogical mode
# ---------------------------------------------------------------------------

test_that("canon() flags bins parameter in pedagogical mode", {
  p <- ggplot(mpg, aes(displ)) + geom_histogram(bins = 20)
  c1 <- canon(p, mode = "pedagogical")
  expect_true(any(c1$changes$rule == "histogram_bin_param"))
})

test_that("canon() does not flag binwidth parameter in pedagogical mode", {
  p <- ggplot(mpg, aes(displ)) + geom_histogram(binwidth = 0.5)
  c1 <- canon(p, mode = "pedagogical")
  expect_false(any(c1$changes$rule == "histogram_bin_param"))
})

test_that("canon() flags after_stat mapping in pedagogical mode", {
  p <- ggplot(mpg, aes(displ)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.5) +
    geom_density()
  c1 <- canon(p, mode = "pedagogical")
  expect_true(any(c1$changes$rule == "after_stat_flag"))
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
