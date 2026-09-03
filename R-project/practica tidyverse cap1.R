4library(tidyverse)

install.packages("palmerpenguins")
library(palmerpenguins)

glimpse(penguins)


install.packages("ggthemes")
library(ggthemes)


penguins?


install.packages("ggthemes")

?penguins


glimpse(penguins)
penguins$species
ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
install.packages("ggplot")
library(ggplot)

install.packages(c("pkg1", "pkg2"))
ggplot(penguins, aes(x = species)) +
  geom_bar()
ggplot(data = penguins) + 
  geom_point()

# visualizacion de distribuciones -----------------------------------------
penguins
glimpse(penguins)
glimpse(penguins)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot2::ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
intall.packages("ggplot")
