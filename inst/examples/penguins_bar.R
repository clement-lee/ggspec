library(ggplot2)
library(dplyr)

## Ground truth: bar plot of species

## equivalent
ggplot(penguins, aes(x = species)) + geom_bar()
penguins |> ggplot(aes(x = species)) + geom_bar()

## equivalent
ggplot(penguins) + geom_bar(aes(x = species))
penguins |> ggplot() + geom_bar(aes(x = species))

## equivalent
ggplot(count(penguins, species)) + geom_bar(aes(x = species, y = n), stat = "identity") + labs(y = "count")
penguins |> count(species) |> ggplot() + geom_bar(aes(x = species, y = n), stat = "identity") + labs(y = "count")

## equivalent
ggplot(count(penguins, species), aes(x = species, y = n)) + geom_bar(stat = "identity") + labs(y = "count")
penguins |> count(species) |> ggplot(aes(x = species, y = n)) + geom_bar(stat = "identity") + labs(y = "count")

## equivalent
ggplot(count(penguins, species, name = "count")) + geom_bar(aes(x = species, y = count), stat = "identity")
penguins |> count(species, name = "count") |> ggplot(aes(x = species, y = count)) + geom_bar(stat = "identity")

## equivalent under compare_plots(mode = "structural") — geom_col normalised to geom_bar
ggplot(count(penguins, species), aes(x = species, y = n)) + geom_col() + labs(y = "count")
penguins |> count(species) |> ggplot(aes(x = species, y = n)) + geom_col() + labs(y = "count")

## equivalent under compare_plots(mode = "structural") — geom_col normalised to geom_bar
ggplot(count(penguins, species, name = "count")) + geom_col(aes(x = species, y = count))
penguins |> count(species, name = "count") |> ggplot() + geom_col(aes(x = species, y = count))

## equivalent
ggplot(count(penguins, species), aes(x = species, y = n)) + geom_bar(stat = "identity") + scale_y_continuous(name = "count")
penguins |> count(species) |> ggplot(aes(x = species, y = n)) + geom_bar(stat = "identity") + scale_y_continuous(name = "count")

## equivalent
ggplot(count(penguins, species)) + geom_bar(aes(x = species, y = n), stat = "identity") + scale_y_continuous(name = "count")
penguins |> count(species) |> ggplot() + geom_bar(aes(x = species, y = n), stat = "identity") + scale_y_continuous(name = "count")

## equivalent under compare_plots(mode = "structural") — geom_col normalised to geom_bar
ggplot(count(penguins, species), aes(x = species, y = n)) + geom_col() + scale_y_continuous(name = "count")
penguins |> count(species) |> ggplot(aes(x = species, y = n)) + geom_col() + scale_y_continuous(name = "count")

## equivalent under compare_plots(mode = "structural") — geom_col normalised to geom_bar
ggplot(count(penguins, species)) + geom_col(aes(x = species, y = n)) + scale_y_continuous(name = "count")
penguins |> count(species) |> ggplot() + geom_col(aes(x = species, y = n)) + scale_y_continuous(name = "count")

## The following cases use coord_flip(). Equivalence to the non-flipped ground truth
## requires compare_plots(mode = "visual") — coord_flip is absorbed by the
## .rule_coord_flip canonicalisation rule (swaps x <-> y aesthetics, removes coord_flip).
## Within this section each pair is equivalent to the others.
ggplot(penguins, aes(y = species)) + geom_bar() + coord_flip()
penguins |> ggplot(aes(y = species)) + geom_bar() + coord_flip()

## equivalent
ggplot(penguins) + geom_bar(aes(y = species)) + coord_flip()
penguins |> ggplot() + geom_bar(aes(y = species)) + coord_flip()

## equivalent
ggplot(count(penguins, species)) + geom_bar(aes(y = species, x = n), stat = "identity") + labs(x = "count") + coord_flip()
penguins |> count(species) |> ggplot() + geom_bar(aes(y = species, x = n), stat = "identity") + labs(x = "count") + coord_flip()

## equivalent
ggplot(count(penguins, species), aes(y = species, x = n)) + geom_bar(stat = "identity") + labs(x = "count")  + coord_flip()
penguins |> count(species) |> ggplot(aes(y = species, x = n)) + geom_bar(stat = "identity") + labs(x = "count" ) + coord_flip()

## equivalent
ggplot(count(penguins, species, name = "count")) + geom_bar(aes(y = species, x = count), stat = "identity")  + coord_flip()
penguins |> count(species, name = "count") |> ggplot(aes(y = species, x = count)) + geom_bar(stat = "identity")  + coord_flip()

## equivalent
ggplot(count(penguins, species), aes(y = species, x = n)) + geom_col() + labs(x = "count") + coord_flip()
penguins |> count(species) |> ggplot(aes(y = species, x = n)) + geom_col() + labs(x = "count") + coord_flip()

## equivalent
ggplot(count(penguins, species, name = "count")) + geom_col(aes(y = species, x = count)) + coord_flip()
penguins |> count(species, name = "count") |> ggplot() + geom_col(aes(y = species, x = count)) + coord_flip()

## equivalent
ggplot(count(penguins, species), aes(y = species, x = n)) + geom_bar(stat = "identity") + scale_x_continuous(name = "count") + coord_flip()
penguins |> count(species) |> ggplot(aes(y = species, x = n)) + geom_bar(stat = "identity") + scale_x_continuous(name = "count") + coord_flip()

## equivalent
ggplot(count(penguins, species)) + geom_bar(aes(y = species, x = n), stat = "identity") + scale_x_continuous(name = "count") + coord_flip()
penguins |> count(species) |> ggplot() + geom_bar(aes(y = species, x = n), stat = "identity") + scale_x_continuous(name = "count") + coord_flip()

## equivalent
ggplot(count(penguins, species), aes(y = species, x = n)) + geom_col() + scale_x_continuous(name = "count") + coord_flip()
penguins |> count(species) |> ggplot(aes(y = species, x = n)) + geom_col() + scale_x_continuous(name = "count") + coord_flip()

## equivalent
ggplot(count(penguins, species)) + geom_col(aes(y = species, x = n)) + scale_x_continuous(name = "count") + coord_flip()
penguins |> count(species) |> ggplot() + geom_col(aes(y = species, x = n)) + scale_x_continuous(name = "count") + coord_flip()
