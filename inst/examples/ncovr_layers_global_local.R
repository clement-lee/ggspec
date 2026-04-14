library(ggplot2)
library(dplyr)
library(geodaData)
library(rnaturalearth)
library(rnaturalearthdata)
usa_map <- ne_countries(country = "united states of america", returnclass = "sf")

## Ground truth: ncovr's POL60 overlaying map of USA
ggplot() +
  geom_sf(data = usa_map) +
  geom_sf(aes(fill = POL60), data = ncovr)

## equivalent
ggplot(usa_map) +
  geom_sf() +
  geom_sf(aes(fill = POL60), data = ncovr)
usa_map |>
ggplot() +
  geom_sf() +
  geom_sf(aes(fill = POL60), data = ncovr)

## equivalent
ggplot(ncovr) +
  geom_sf(data = usa_map) +
  geom_sf(aes(fill = POL60))
ncovr |>
ggplot() +
  geom_sf(data = usa_map) +
  geom_sf(aes(fill = POL60))
