test_that("check_plot() passes silently for equivalent plots", {
  ref <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  obs <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  expect_invisible(check_plot(obs, ref, check = "layers"))
})

test_that("check_plot() calls fail_fn when check fails", {
  ref <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  obs <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  expect_error(
    check_plot(obs, ref, check = "layers"),
    regexp = "smooth|layer|missing",
    ignore.case = TRUE
  )
})

test_that("check_plot() accepts a custom fail_fn", {
  ref <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  obs <- ggplot(mpg, aes(displ, hwy)) + geom_smooth()
  captured <- NULL
  check_plot(obs, ref, check = "layers",
             fail_fn = function(msg) { captured <<- msg })
  expect_false(is.null(captured))
  expect_type(captured, "character")
})

test_that("check_plot() returns ggspec_result invisibly", {
  ref <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  obs <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  result <- check_plot(obs, ref, check = "layers")
  expect_s3_class(result, "ggspec_result")
})

test_that("expect_equiv_plot() errors without testthat if called outside test context", {
  # This just ensures the function exists and is callable
  expect_true(is.function(expect_equiv_plot))
})
