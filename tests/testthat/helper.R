# Shared test fixtures
library(ggplot2)
library(dplyr)
library(MASS)

penguins <- if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  p <- palmerpenguins::penguins
  names(p)[match(
    c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
    names(p)
  )] <- c("bill_len", "bill_dep", "flipper_len", "body_mass")
  p
} else {
  get("penguins", envir = loadNamespace("datasets"))
}

# ---------------------------------------------------------------------------
# mpg-based fixtures (used by canon, compare, visual tests)
# ---------------------------------------------------------------------------

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
  ggplot(mpg, aes(displ, hwy)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point()
}

make_point_smooth_plot <- function() {
  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
}

# ---------------------------------------------------------------------------
# Simple d-frame fixtures (coord_flip and geom_col/bar canonical tests)
# ---------------------------------------------------------------------------

make_flip_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(g, v)) + geom_col() + coord_flip()
}

make_swapped_plot <- function() {
  d <- data.frame(g = letters[1:3], v = c(1, 2, 3))
  ggplot(d, aes(v, g)) + geom_col()
}

# ---------------------------------------------------------------------------
# Penguins — 1-D count (species bar / col / bar-identity)
# ---------------------------------------------------------------------------

make_bar_plot <- function() {
  penguins |>
    ggplot() +
    geom_bar(aes(x = species))
}

make_col_plot <- function() {
  penguins |>
    count(species) |>
    ggplot() +
    geom_col(aes(x = species, y = n))
}

make_bar_identity_plot <- function() {
  penguins |>
    count(species) |>
    ggplot() +
    geom_bar(aes(x = species, y = n), stat = "identity")
}

make_bar_y_plot <- function() {
  penguins |>
    ggplot() +
    geom_bar(aes(y = species))
}

make_bar_y_flip_plot <- function() {
  penguins |>
    ggplot() +
    geom_bar(aes(y = species)) +
    coord_flip()
}

# ---------------------------------------------------------------------------
# Penguins — 1-D distribution (bill_len)
# ---------------------------------------------------------------------------

make_density_plot <- function() {
  penguins |>
    ggplot() +
    geom_density(aes(x = bill_len))
}

make_dotplot_plot <- function() {
  penguins |>
    ggplot() +
    geom_dotplot(aes(x = bill_len))
}

make_freqpoly_plot <- function() {
  penguins |>
    ggplot() +
    geom_freqpoly(aes(x = bill_len))
}

make_histogram_plot <- function() {
  penguins |>
    ggplot() +
    geom_histogram(aes(x = bill_len))
}

make_area_bin_plot <- function() {
  penguins |>
    ggplot() +
    geom_area(aes(x = bill_len), stat = "bin")
}

# ---------------------------------------------------------------------------
# Penguins — 1-D continuous by 1-D discrete (species by body_mass)
# ---------------------------------------------------------------------------

make_boxplot_plot <- function() {
  penguins |>
    ggplot(aes(x = species, y = body_mass)) +
    geom_boxplot()
}

make_violin_plot <- function() {
  penguins |>
    ggplot(aes(x = species, y = body_mass)) +
    geom_violin()
}

make_jitter_plot <- function(seed = 123L) {
  set.seed(seed)
  penguins |>
    ggplot(aes(x = species, y = body_mass)) +
    geom_jitter()
}

make_point_jitter_plot <- function(seed = 123L) {
  set.seed(seed)
  penguins |>
    ggplot(aes(x = species, y = body_mass)) +
    geom_point(position = "jitter")
}

make_point_position_jitter_plot <- function(seed = 123L) {
  set.seed(seed)
  penguins |>
    ggplot(aes(x = species, y = body_mass)) +
    geom_point(position = position_jitter())
}

# Transposed axes variants
make_boxplot_yx_plot <- function() {
  penguins |>
    ggplot(aes(y = species, x = body_mass)) +
    geom_boxplot()
}

make_boxplot_yx_flip_plot <- function() {
  penguins |>
    ggplot(aes(y = species, x = body_mass)) +
    geom_boxplot() +
    coord_flip()
}

# ---------------------------------------------------------------------------
# Penguins — 2-D discrete counts (species by island)
# ---------------------------------------------------------------------------

make_2d_count_plot <- function() {
  penguins |>
    ggplot() +
    geom_count(aes(x = species, y = island))
}

# Jitter over 2-D discrete grid — distinct from the 1-D-by-category jitter above
make_jitter_2d_plot <- function() {
  penguins |>
    ggplot() +
    geom_jitter(aes(x = species, y = island))
}

make_2d_point_plot <- function() {
  penguins |>
    count(species, island) |>
    ggplot() +
    geom_point(aes(x = species, y = island, size = n))
}

make_2d_count_identity_plot <- function() {
  penguins |>
    count(species, island) |>
    ggplot() +
    geom_count(aes(x = species, y = island, size = n), stat = "identity")
}

# ---------------------------------------------------------------------------
# Penguins — 1-layer & 2-layer smooth & with colour (flipper_len by body_mass)
# ---------------------------------------------------------------------------

plot_point_then_smooth <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
    geom_point() +
    geom_smooth()
}

plot_smooth_then_point <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
    geom_smooth() +
    geom_point()
}

make_point_plot <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
    geom_point()
}

make_line_plot <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
    geom_line()
}

make_path_plot <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass)) +
    geom_path()
}

make_point_colour_plot <- function() {
  penguins |>
    ggplot(aes(x = flipper_len, y = body_mass, colour = species)) +
    geom_point()
}

# ---------------------------------------------------------------------------
# economics — 1-layer & 2-layer point + line
# ---------------------------------------------------------------------------

plot_point_then_line <- function() {
  economics |>
    ggplot(aes(x = date, y = pce)) +
    geom_point() +
    geom_line()
}

plot_line_then_point <- function() {
  economics |>
    ggplot(aes(x = date, y = pce)) +
    geom_line() +
    geom_point()
}

make_path_time_plot <- function() {
  economics |>
    ggplot(aes(x = date, y = pce)) +
    geom_path()
}

make_line_time_plot <- function() {
  economics |>
    ggplot(aes(x = date, y = pce)) +
    geom_line()
}

make_path_notime_plot <- function() {
  economics |>
    ggplot(aes(x = psavert, y = pce)) +
    geom_path()
}

make_line_notime_plot <- function() {
  economics |>
    ggplot(aes(x = psavert, y = pce)) +
    geom_line()
}

make_point_notime_plot <- function() {
  economics |>
    ggplot(aes(x = psavert, y = pce)) +
    geom_point()
}

# ---------------------------------------------------------------------------
# Multi-dataset: mpg + Cars93 (MASS)
# ---------------------------------------------------------------------------

plot_mpg_then_Cars93 <- function() {
  ggplot() +
    geom_point(aes(cty, hwy), data = mpg, colour = "red") +
    geom_point(aes(MPG.city, MPG.highway), data = Cars93, colour = "blue")
}

plot_Cars93_then_mpg <- function() {
  ggplot() +
    geom_point(aes(MPG.city, MPG.highway), data = Cars93, colour = "blue") +
    geom_point(aes(cty, hwy), data = mpg, colour = "red")
}
