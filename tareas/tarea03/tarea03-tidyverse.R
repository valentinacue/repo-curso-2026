
# Data visualization ------------------------------------------------------


install.packages("tidyverse")
library(tidyverse)

library(palmerpenguins)
#> 
#> Attaching package: 'palmerpenguins'
#> The following objects are masked from 'package:datasets':
#> 
#>     penguins, penguins_raw
library(ggthemes)
penguins
#> # A tibble: 344 × 8
#>   species island    bill_length_mm bill_depth_mm flipper_length_mm
#>   <fct>   <fct>              <dbl>         <dbl>             <int>
#> 1 Adelie  Torgersen           39.1          18.7               181
#> 2 Adelie  Torgersen           39.5          17.4               186
#> 3 Adelie  Torgersen           40.3          18                 195
#> 4 Adelie  Torgersen           NA            NA                  NA
#> 5 Adelie  Torgersen           36.7          19.3               193
#> 6 Adelie  Torgersen           39.3          20.6               190
#> # ℹ 338 more rows
#> # ℹ 3 more variables: body_mass_g <int>, sex <fct>, year <int>
glimpse(penguins)
#> Rows: 344
#> Columns: 8
#> $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, A…
#> $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torge…
#> $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.…
#> $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.…
#> $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, …
#> $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 347…
#> $ sex               <fct> male, female, female, NA, female, male, female, m…
#> $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2…
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
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
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
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Visualizating distributions ---------------------------------------------

ggplot(penguins, aes(x = species)) +
  geom_bar()
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_density()`).

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")



ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
ggsave(filename = "finalpenguin-plot.png")


# 1.2.5 Exercises ---------------------------------------------------------

library(tidyverse)
library(palmerpenguins)



# 1. ¿Cuántas filas y columnas tiene penguins?


nrow(penguins)
ncol(penguins)

# penguins tiene 344 filas y 8 columnas.



# 2. ¿Qué describe la variable bill_depth_mm?


?penguins

# bill_depth_mm describe la profundidad del pico del pingüino,
# medida en milímetros.



# 3. Scatterplot de bill_depth_mm vs. bill_length_mm


ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point()

# Se observa una relación entre la longitud y la profundidad
# del pico. La relación no es perfectamente lineal y existe
# bastante dispersión entre los puntos.



# 4. Scatterplot de species vs. bill_depth_mm


ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm)
) +
  geom_point()

# En este gráfico los puntos quedan agrupados verticalmente
# según cada especie.
#
# Como species es una variable categórica, un geom_point()
# no es la mejor opción para comparar su distribución.
# Un mejor geom sería geom_boxplot().

ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm)
) +
  geom_boxplot()



# 5. ¿Por qué ggplot(data = penguins) + geom_point() da error?


ggplot(data = penguins) +
  geom_point()

# Da error porque geom_point() necesita saber qué variables
# utilizar para los ejes x e y.
#
# Para solucionarlo debemos especificar las variables dentro
# de aes().

ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point()



# 6. ¿Qué hace el argumento na.rm?


# El argumento na.rm indica qué hacer con los valores faltantes
# (NA).
#
# Su valor predeterminado es FALSE.
#
# Cuando usamos na.rm = TRUE, los valores faltantes se eliminan
# y no se muestra la advertencia correspondiente.

ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(na.rm = TRUE)


# 7. Agregar un caption al gráfico

ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(na.rm = TRUE) +
  labs(
    caption = "Data come from the palmerpenguins package."
  )



# 8. Recrear la visualización


ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

# bill_depth_mm se asigna a la estética color.
#
# Se asigna a nivel del geom_point() y no a nivel global,
# porque queremos que el color afecte solamente a los puntos
# y no al geom_smooth().
#
# Se observa una relación positiva entre la longitud de la
# aleta y la masa corporal.



# 9. Predecir el resultado del gráfico


ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = island
  )
) +
  geom_point() +
  geom_smooth(se = FALSE)

# El eje x representa la longitud de la aleta y el eje y
# representa la masa corporal.
#
# Los puntos aparecen con diferentes colores según la isla.
#
# Como color = island está definido en el mapping global,
# tanto geom_point() como geom_smooth() utilizan island.
#
# Por lo tanto, geom_smooth() genera una línea de tendencia
# para cada isla.
#
# se = FALSE elimina la banda de confianza alrededor de
# las líneas.
#
# En general, se observa una relación positiva: los pingüinos
# con aletas más largas tienden a tener mayor masa corporal.



# 10. ¿Los dos gráficos se ven diferentes?

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()


ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g
    )
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g
    )
  )

# Los dos gráficos se ven iguales porque utilizan los mismos
# datos y las mismas variables para los ejes x e y.
#
# La diferencia está en dónde se especifican los datos y
# el mapping.
#
# En el primer gráfico, data y mapping se especifican
# globalmente en ggplot(), por lo que ambos geoms los heredan.
#
# En el segundo gráfico, data y mapping se especifican
# individualmente dentro de cada geom.
#
# Como ambos geoms utilizan los mismos datos y variables,
# el resultado visual es el mismo.



# 1.4.3 Exercises ---------------------------------------------------------


# 1. Gráfico de barras de species usando species en el eje y


ggplot(
  data = penguins,
  mapping = aes(y = species)
) +
  geom_bar()

# El gráfico muestra las especies en el eje y y la cantidad
# de pingüinos de cada especie en el eje x.
#
# La diferencia con un gráfico de barras convencional es que
# las barras aparecen de manera horizontal en lugar de vertical.



# 2. Diferencia entre color y fill


ggplot(
  penguins,
  aes(x = species)
) +
  geom_bar(color = "red")


ggplot(
  penguins,
  aes(x = species)
) +
  geom_bar(fill = "red")

# En el primer gráfico, color = "red" cambia el color del
# borde de las barras a rojo, pero el interior permanece
# con el color predeterminado.
#
# En el segundo gráfico, fill = "red" cambia el color del
# interior de las barras a rojo.
#
# Por lo tanto, fill es más útil para cambiar el color de
# las barras.



# 3. ¿Qué hace el argumento bins en geom_histogram()?


# El argumento bins determina la cantidad de intervalos
# (bins) en los que se divide el eje x de un histograma.
#
# Una mayor cantidad de bins produce intervalos más pequeños
# y permite observar más detalles de la distribución.
#
# Una menor cantidad de bins produce intervalos más grandes
# y muestra una distribución más general.



# 4. Histograma de la variable carat de diamonds


ggplot(
  data = diamonds,
  mapping = aes(x = carat)
) +
  geom_histogram()

# Podemos experimentar con diferentes valores de binwidth.

ggplot(
  data = diamonds,
  mapping = aes(x = carat)
) +
  geom_histogram(binwidth = 0.1)


ggplot(
  data = diamonds,
  mapping = aes(x = carat)
) +
  geom_histogram(binwidth = 0.5)


ggplot(
  data = diamonds,
  mapping = aes(x = carat)
) +
  geom_histogram(binwidth = 0.05)


# Un binwidth de 0.1 permite observar con bastante claridad
# la distribución de carat y sus patrones, sin que el gráfico
# quede demasiado agrupado ni demasiado detallado.
#
# Con un binwidth muy pequeño se observan más detalles, pero
# el gráfico puede resultar demasiado fragmentado.
#
# Con un binwidth muy grande se pierde parte de la información
# sobre la distribución.



# 1.5.5 Exercises --------------------------------------------------------


# 1. Variables categóricas y numéricas de mpg


?mpg

# Las variables categóricas son:
# manufacturer, model, trans, drv, fl, class

# Las variables numéricas son:
# displ, year, cyl, cty, hwy



mpg

glimpse(mpg)

# En glimpse(), <chr> representa variables de texto/categóricas
# y <dbl> o <int> representan variables numéricas.



# 2. Scatterplot de hwy vs. displ y diferentes aesthetics


# Scatterplot básico.

ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy)
) +
  geom_point()


# Mapear una variable numérica a color.

ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    color = cty
  )
) +
  geom_point()


# Mapear una variable numérica a size.

ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    size = cty
  )
) +
  geom_point()


# Mapear una variable numérica a color y size.

ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    color = cty,
    size = cty
  )
) +
  geom_point()


# Mapear una variable numérica a shape.

ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    shape = cty
  )
) +
  geom_point()

# color y size pueden utilizar variables numéricas.
# En el caso de color, los valores numéricos se representan
# mediante una escala continua de colores.
#
# En size, los valores numéricos producen puntos de diferentes
# tamaños según el valor de la variable.
#
# shape funciona principalmente con variables categóricas.
# No es apropiado para representar una variable numérica
# continua, porque existen pocas formas disponibles.



# 3. ¿Qué sucede al mapear una variable a linewidth?


ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    linewidth = cty
  )
) +
  geom_point()

# linewidth está pensado principalmente para controlar el
# grosor de líneas.
#
# Como geom_point() utiliza puntos y no líneas, linewidth
# no es una estética apropiada para este geom y puede producir
# una advertencia o no tener el efecto esperado.
#
# linewidth sería más apropiado con geoms que dibujan líneas,
# como geom_line().



# 4. Mapear la misma variable a múltiples aesthetics


ggplot(
  data = mpg,
  mapping = aes(
    x = displ,
    y = hwy,
    color = cty,
    size = cty
  )
) +
  geom_point()

# La misma variable puede asignarse a varias aesthetics.
#
# En este caso, cty controla simultáneamente el color y el
# tamaño de los puntos.
#
# Los valores más altos de cty se representan con un color
# diferente y con un tamaño diferente.



# 5. bill_depth_mm vs. bill_length_mm coloreado por species


ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species
  )
) +
  geom_point()

# Al agregar el color según species podemos observar que
# existen diferentes grupos de pingüinos.
#
# La relación entre bill_length_mm y bill_depth_mm cambia
# entre las diferentes especies y los grupos se pueden
# distinguir mejor gracias al color.


# También podemos utilizar faceting por species.

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point() +
  facet_wrap(~species)

# El facetado crea un gráfico separado para cada especie.
#
# Esto permite analizar la relación dentro de cada especie
# sin que los puntos de las diferentes especies se mezclen.
#
# El color permite comparar las especies en un mismo gráfico,
# mientras que facet_wrap() permite observar cada especie
# por separado.



# 6. ¿Por qué aparecen dos leyendas?


ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species,
    shape = species
  )
) +
  geom_point() +
  labs(color = "Species")

# Aparecen dos leyendas porque color y shape tienen
# diferentes nombres de escala.
#
# Aunque ambas aesthetics utilizan la misma variable,
# ggplot2 crea una escala para color y otra para shape.
#
# Para combinar las dos leyendas debemos darles el mismo
# nombre utilizando labs().

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species,
    shape = species
  )
) +
  geom_point() +
  labs(
    color = "Species",
    shape = "Species"
  )

# Al utilizar el mismo nombre para las dos escalas,
# ggplot2 combina las leyendas en una sola.



# 7. Gráficos de barras apiladas en proporciones


# Primer gráfico

ggplot(
  penguins,
  aes(x = island, fill = species)
) +
  geom_bar(position = "fill")


# Segundo gráfico

ggplot(
  penguins,
  aes(x = species, fill = island)
) +
  geom_bar(position = "fill")


# En el primer gráfico podemos responder:
#
# "¿Cuál es la proporción de cada especie dentro de cada isla?"
#
# Cada barra representa una isla y los diferentes colores
# muestran qué proporción corresponde a cada especie.


# En el segundo gráfico podemos responder:
#
# "¿Cuál es la proporción de pingüinos de cada isla dentro
# de cada especie?"
#
# Cada barra representa una especie y los diferentes colores
# muestran qué proporción corresponde a cada isla.



# 1.6.1 Exercises ---------------------------------------------------------

# 1. ¿Cuál de los dos gráficos se guarda como mpg-plot.png?


ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()

ggsave("mpg-plot.png")

# El gráfico que se guarda como mpg-plot.png es el segundo:
#
# ggplot(mpg, aes(x = cty, y = hwy)) +
#   geom_point()
#
# Esto ocurre porque ggsave() guarda por defecto el último
# gráfico que se creó.
#
# Por lo tanto, ggsave() guarda el gráfico de dispersión
# de cty vs. hwy y no el gráfico de barras.



# 2. ¿Cómo guardar el gráfico como PDF?


ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()

ggsave("mpg-plot.pdf")

# Para guardar el gráfico como PDF simplemente cambiamos
# la extensión del archivo de .png a .pdf.


?ggsave


