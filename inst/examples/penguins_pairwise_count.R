library(ggplot2)

## Ground truth: count plot of species & island by size
ggplot(penguins, aes(x = island, y = species)) +
  geom_count()
penguins |>
ggplot(aes(x = island, y = species)) +
  geom_count()

## equivalent
ggplot(penguins) +
  geom_count(aes(x = island, y = species))
penguins |>
ggplot() +
  geom_count(aes(x = island, y = species))

## equivalent
ggplot(penguins, aes(x = island)) +
  geom_count(aes(y = species))
penguins |>
ggplot(aes(x = island)) +
  geom_count(aes(y = species))

## equivalent
ggplot(penguins, aes(y = species)) +
  geom_count(aes(x = island))
penguins |>
ggplot(aes(y = species)) +
  geom_count(aes(x = island))

## Ground truth B: count plot using geom_point with pre-counted data
## (visually similar to ground truth A; geom class differs — GeomCount vs GeomPoint —
##  so these are NOT directly equivalent to A via equiv_layers without a custom rule)
ggplot(count(penguins, island, species), aes(x = island, y = species, size = n)) +
  geom_point()
penguins |>
count(island, species) |>
ggplot(aes(x = island, y = species, size = n)) +
  geom_point()

## equivalent
ggplot(count(penguins, island, species), aes(x = island, y = species)) +
  geom_point(aes(size = n))
penguins |>
count(island, species) |>
ggplot(aes(x = island, y = species)) +
  geom_point(aes(size = n))

## equivalent
ggplot(count(penguins, island, species), aes(x = island, size = n)) +
  geom_point(aes(y = species))
penguins |>
count(island, species) |>
ggplot(aes(x = island, size = n)) +
  geom_point(aes(y = species))

## equivalent
ggplot(count(penguins, island, species), aes(y = species, size = n)) +
  geom_point(aes(x = island))
penguins |>
count(island, species) |>
ggplot(aes(y = species, size = n)) +
  geom_point(aes(x = island))

## equivalent
ggplot(count(penguins, island, species), aes(x = island)) +
  geom_point(aes(y = species, size = n))
penguins |>
count(island, species) |>
ggplot(aes(x = island)) +
  geom_point(aes(y = species, size = n))

## equivalent
ggplot(count(penguins, island, species), aes(y = species)) +
  geom_point(aes(x = island, size = n))
penguins |>
count(island, species) |>
ggplot(aes(y = species)) +
  geom_point(aes(x = island, size = n))

## equivalent
ggplot(count(penguins, island, species), aes(size = n)) +
  geom_point(aes(x = island, y = species))
penguins |>
count(island, species) |>
ggplot(aes(size = n)) +
  geom_point(aes(x = island, y = species))

## equivalent
ggplot(count(penguins, island, species)) +
  geom_point(aes(x = island, y = species, size = n))
penguins |>
count(island, species) |>
ggplot() +
  geom_point(aes(x = island, y = species, size = n))
