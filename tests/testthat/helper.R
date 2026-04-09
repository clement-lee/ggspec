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
