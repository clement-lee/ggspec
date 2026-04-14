library(ggplot2)
library(dplyr)

## Ground truth A: body_mass vs flipper_len, regression line overlaying scatterplot
ggplot(penguins, aes(x = flipper_len, y = body_mass)) +
  geom_point() +
  geom_smooth()
penguins |>
ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_point() +
  geom_smooth()

## A-equivalent
ggplot(penguins, aes(x = flipper_len)) +
  geom_point(aes(y = body_mass)) +
  geom_smooth(aes(y = body_mass))
penguins |>
ggplot(aes(x = flipper_len)) +
  geom_point(aes(y = body_mass)) +
  geom_smooth(aes(y = body_mass))

## A-equivalent
ggplot(penguins, aes(y = body_mass)) +
  geom_point(aes(x = flipper_len)) +
  geom_smooth(aes(x = flipper_len))
penguins |>
ggplot(aes(y = body_mass)) +
  geom_point(aes(x = flipper_len)) +
  geom_smooth(aes(x = flipper_len))

## A-equivalent
ggplot(penguins) +
  geom_point(aes(x = flipper_len, y = body_mass)) +
  geom_smooth(aes(x = flipper_len, y = body_mass))
penguins |>
ggplot() +
  geom_point(aes(x = flipper_len, y = body_mass)) +
  geom_smooth(aes(x = flipper_len, y = body_mass))

## A-equivalent
ggplot() +
  geom_point(aes(x = flipper_len, y = body_mass), data = penguins) +
  geom_smooth(aes(x = flipper_len, y = body_mass), data = penguins)

## Ground truth B: body_mass vs flipper_len, scatterplot overlaying regression line, similar to ground truth A
ggplot(penguins, aes(x = flipper_len, y = body_mass)) +
  geom_smooth() +
  geom_point()
penguins |>
ggplot(aes(x = flipper_len, y = body_mass)) +
  geom_smooth() +
  geom_point()

## B-equivalent
ggplot(penguins, aes(x = flipper_len)) +
  geom_smooth(aes(y = body_mass)) +
  geom_point(aes(y = body_mass))
penguins |>
ggplot(aes(x = flipper_len)) +
  geom_smooth(aes(y = body_mass)) +
  geom_point(aes(y = body_mass))

## B-equivalent
ggplot(penguins, aes(y = body_mass)) +
  geom_smooth(aes(x = flipper_len)) +
  geom_point(aes(x = flipper_len))
penguins |>
ggplot(aes(y = body_mass)) +
  geom_smooth(aes(x = flipper_len)) +
  geom_point(aes(x = flipper_len))

## B-equivalent
ggplot(penguins) +
  geom_smooth(aes(x = flipper_len, y = body_mass)) +
  geom_point(aes(x = flipper_len, y = body_mass))
penguins |>
ggplot() +
  geom_smooth(aes(x = flipper_len, y = body_mass)) +
  geom_point(aes(x = flipper_len, y = body_mass))

## B-equivalent
ggplot() +
  geom_smooth(aes(x = flipper_len, y = body_mass), data = penguins) +
  geom_point(aes(x = flipper_len, y = body_mass), data = penguins)
