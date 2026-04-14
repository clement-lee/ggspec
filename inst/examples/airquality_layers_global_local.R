library(ggplot2)
library(lubridate)
airquality1 <-
  airquality |>
  mutate(date = make_date(1973, Month, Day))

## Ground truth: line & point plot of Wind over time (date)
ggplot(airquality1, aes(x = date, y = Wind)) +
  geom_line() +
  geom_point()
airquality1 |>
ggplot(aes(x = date, y = Wind)) +
  geom_line() +
  geom_point()

## equivalent
ggplot(airquality1, aes(x = date, y = Wind)) +
  geom_point() +
  geom_line()
airquality1 |>
ggplot(aes(x = date, y = Wind)) +
  geom_point() +
  geom_line()

## equivalent
ggplot(airquality1) +
  geom_line(aes(x = date, y = Wind)) +
  geom_point(aes(x = date, y = Wind))
airquality1 |>
ggplot() +
  geom_line(aes(x = date, y = Wind)) +
  geom_point(aes(x = date, y = Wind))

## equivalent
ggplot(airquality1) +
  geom_point(aes(x = date, y = Wind)) +
  geom_line(aes(x = date, y = Wind))
airquality1 |>
ggplot() +
  geom_point(aes(x = date, y = Wind)) +
  geom_line(aes(x = date, y = Wind))

## equivalent
ggplot(airquality1, aes(x = date)) +
  geom_line(aes(y = Wind)) +
  geom_point(aes(y = Wind))
airquality1 |>
ggplot(aes(x = date)) +
  geom_line(aes(y = Wind)) +
  geom_point(aes(y = Wind))

## equivalent
ggplot(airquality1, aes(x = date)) +
  geom_point(aes(y = Wind)) +
  geom_line(aes(y = Wind))
airquality1 |>
ggplot(aes(x = date)) +
  geom_point(aes(y = Wind)) +
  geom_line(aes(y = Wind))

## equivalent
ggplot(airquality1, aes(y = Wind)) +
  geom_line(aes(x = date)) +
  geom_point(aes(x = date))
airquality1 |>
ggplot(aes(y = Wind)) +
  geom_line(aes(x = date)) +
  geom_point(aes(x = date))

## equivalent
ggplot(airquality1, aes(y = Wind)) +
  geom_point(aes(x = date)) +
  geom_line(aes(x = date))
airquality1 |>
ggplot(aes(y = Wind)) +
  geom_point(aes(x = date)) +
  geom_line(aes(x = date))

## equivalent
ggplot() +
  geom_line(aes(x = date, y = Wind), data = airquality1) +
  geom_point(aes(x = date, y = Wind), data = airquality1)

## equivalent
ggplot() +
  geom_point(aes(x = date, y = Wind), data = airquality1) +
  geom_line(aes(x = date, y = Wind), data = airquality1)
  
