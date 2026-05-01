## maybe data dependent
## geom_bar(position = "stack") vs geom_bar()
## geom_bar(position = "dodge") vs geom_bar(position = position_dodge(width = ...))
## geom_bar() vs geom_bar(stat = "count")
## geom_histogram(binwidth)

library(ggplot2)

## Ground truth: histogram of flipper_len
ggplot(penguins, aes(x = flipper_len)) + geom_histogram()
penguins |> ggplot(aes(x = flipper_len)) + geom_histogram()
ggplot(penguins) + geom_histogram(aes(x = flipper_len))
penguins |> ggplot() + geom_histogram(aes(x = flipper_len))

## equivalent
ggplot(penguins, aes(x = flipper_len)) + geom_histogram(bins = 30)
penguins |> ggplot(aes(x = flipper_len)) + geom_histogram(bins = 30)
ggplot(penguins) + geom_histogram(aes(x = flipper_len), bins = 30)
penguins |> ggplot() + geom_histogram(aes(x = flipper_len), bins = 30)

## similar (with 30 bins)
ggplot(penguins, aes(x = flipper_len)) + geom_histogram(binwidth = 2.05)
penguins |> ggplot(aes(x = flipper_len)) + geom_histogram(binwidth = 2.05)
ggplot(penguins) + geom_histogram(aes(x = flipper_len), binwidth = 2.05)
penguins |> ggplot() + geom_histogram(aes(x = flipper_len), binwidth = 2.05)
