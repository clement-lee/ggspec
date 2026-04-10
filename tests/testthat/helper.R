# Shared test fixtures
library(ggplot2)

# A simple two-layer plot used across multiple test files
make_base_plot <- function() {
  ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(colour = class)) +
    geom_smooth(method = "lm", se = FALSE)
}

make_faceted_plot <- function() {
  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    facet_wrap(~class)
}

make_grid_plot <- function() {
  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    facet_grid(drv ~ cyl)
}

# Fixtures for canonicalisation / compare tests
make_col_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(g, v)) + geom_col()
}

make_bar_identity_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(g, v)) + geom_bar(stat = "identity")
}

make_flip_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(g, v)) + geom_col() + coord_flip()
}

make_swapped_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(v, g)) + geom_col()
}

make_scale_name_plot <- function() {
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_brewer(name = "Vehicle class")
}

make_labs_fill_plot <- function() {
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_brewer() +
    labs(fill = "Vehicle class")
}

make_smooth_point_plot <- function() {
  # layers in reverse alphabetical order
  ggplot(mpg, aes(displ, hwy)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point()
}

make_point_smooth_plot <- function() {
  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
}
