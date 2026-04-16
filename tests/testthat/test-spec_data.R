test_that("spec_data() returns a tibble with data_id and label columns", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_data(p)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("data_id", "label"))
})

test_that("spec_data() data_id is an integer vector", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_data(p)
  expect_type(result$data_id, "integer")
})

test_that("spec_data() label is a character vector", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_data(p)
  expect_type(result$label, "character")
})

test_that("spec_data() returns one row for a single dataset", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- spec_data(p)
  expect_equal(nrow(result), 1L)
})

test_that("spec_data() deduplicates identical datasets across global and layer", {
  # global and layer use the same dataset — should be one row
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(data = mpg, aes(displ, hwy))
  result <- spec_data(p)
  expect_equal(nrow(result), 1L)
})

test_that("spec_data() returns two rows for two distinct datasets", {
  local_df <- data.frame(x = 1:3, y = 4:6)
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point(data = local_df, aes(x, y))
  result <- spec_data(p)
  expect_equal(nrow(result), 2L)
  expect_equal(result$data_id, 1:2)
})

test_that("spec_data() returns empty tibble for plot with no data", {
  p <- ggplot() + geom_point(aes(x = 1, y = 2))
  result <- spec_data(p)
  expect_equal(nrow(result), 0L)
})

test_that("spec_data() same data yields same data_id across two calls", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  r1 <- spec_data(p1)
  r2 <- spec_data(p2)
  expect_equal(r1$label, r2$label)
})
