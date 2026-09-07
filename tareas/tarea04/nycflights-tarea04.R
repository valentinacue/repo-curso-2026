install.packages("nycflights")
library(nycflights13)
library(tidyverse)


flights
glimpse(flights)

flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )


# Rows --------------------------------------------------------------------


flights |> 
  filter(dep_delay > 120)

# Flights that departed on January 1
flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month %in% c(1, 2))

jan1 <- flights |> 
  filter(month == 1 & day == 1)

flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

flights |> 
  distinct()

flights |> 
  distinct(origin, dest)

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

# Rows exercise -----------------------------------------------------------

# a) Arrival delay >= 2 hours
flights |> 
  filter(arr_delay >= 120)

# b) Volaron hacia Houston: IAH o HOU
flights |> 
  filter(dest %in% c("IAH", "HOU"))

# c) Operados por United, American o Delta
flights |> 
  filter(carrier %in% c("UA", "AA", "DL"))

# d) Salieron en verano: julio, agosto o septiembre
flights |> 
  filter(month %in% c(7, 8, 9))

# e) Llegaron > 2 horas tarde pero no salieron tarde
flights |> 
  filter(arr_delay > 120, dep_delay <= 0)

# f) Retrasados al menos 1 hora, pero recuperaron > 30 min en vuelo
flights |> 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)

# Mayor departure delay
flights |> 
  arrange(desc(dep_delay))

# Salieron mas temprano en la mañana
flights |> 
  arrange(hour, minute)

# Primeros vuelos
flights |> 
  arrange(hour, minute) |> 
  slice_head(n = 10)

#Vuelos mas rapidos
flights |> 
  arrange(air_time)
flights |> 
  arrange(air_time / 60)

# ¿Hubo un vuelo todos los días de 2013?
flights |> 
  distinct(year, month, day) |> 
  nrow()

# Vuelos que recorrieron mayor distancia
flights |> 
  arrange(desc(distance))

# Vuelos que recorrieron menor distancia
flights |> 
  arrange(distance)

# Más largo
flights |> 
  arrange(desc(distance)) |> 
  slice_head(n = 1)

# Más corto
flights |> 
  arrange(distance) |> 
  slice_head(n = 1)


# Columns -----------------------------------------------------------------

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

flights |> 
  select(year, month, day)

flights |> 
  select(year:day)

flights |> 
  select(!year:day)

flights |> 
  select(where(is.character))

flights |> 
  select(tail_num = tailnum)

flights |> 
  rename(tail_num = tailnum)

flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)

# Columns exercise --------------------------------------------------------

flights |> 
  select(dep_time, dep_delay, arr_time, arr_delay)
flights |> 
  select(dep_time:arr_delay)
flights |> 
  select(starts_with("dep"), starts_with("arr"))
flights |> 
  select(contains("time"), contains("delay"))
flights |> 
  select(contains("_time"), contains("_delay"))
flights |> 
  select(matches("^(dep|arr)_(time|delay)$"))


flights |> 
  select(dep_time, dep_delay, dep_time, arr_time)


variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> 
  select(any_of(variables))

flights |> 
  select(contains("TIME"))
flights |> 
  select(contains("time"))

flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min)

flights |> 
  select(tailnum, arr_delay) |> 
  arrange(arr_delay)
flights |> 
  arrange(arr_delay) |> 
  select(tailnum)

# Pipe --------------------------------------------------------------------

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

# Groups ------------------------------------------------------------------

flights |> 
  group_by(month)

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  )

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

daily <- flights |>  
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )

daily |> 
  ungroup()

daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# Pipe exercise ----------------------------------------------------------

flights |>
  group_by(carrier) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  ) |>
  arrange(desc(avg_delay))

flights |>
  group_by(carrier, dest) |>
  summarize(
    n = n(),
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_delay))

flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1)

flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1, na_rm = TRUE)

flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1, na_rm = TRUE) |>
  select(dest, carrier, flight, dep_delay)

flights |>
  group_by(hour) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

library(ggplot2)

flights |>
  group_by(hour) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  ) |>
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hora de salida",
    y = "Retraso promedio (minutos)",
    title = "Retraso promedio según la hora de salida"
  )

slice_min(x, n = 5)

df |>
  slice_min(x, n = -2)


group_by()

flights |>
  count(carrier)
flights |>
  group_by(carrier) |>
  summarize(n = n())
flights |>
  count(carrier, sort = TRUE)


df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df |>
  group_by(y)
df |>
  arrange(y)
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  summarize(
    mean_x = mean(x),
    .groups = "drop"
  )
.groups = "drop"
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# Case study --------------------------------------------------------------
install.packages("Lahman")
library(Lahman)
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)

batters |> 
  arrange(desc(performance))

# 19.2.4 exercises: Keys --------------------------------------------------

# 1. We forgot to draw the relationship between weather and airports in Figure 19.1. What is the relationship and how should it appear in the diagram?

weather |>
  left_join(airports, by = c("origin" = "faa"))

# 2. weather only contains information for the three origin airports in NYC. If it contained weather records for all airports in the USA, what additional connection would it make to flights?

# Si weather tuviera información de todos los aeropuertos de EE.UU., habría una conexión adicional entre:

flights$dest

# y

weather$origin

# 3. The year, month, day, hour, and origin variables almost form a compound key for weather, but there’s one hour that has duplicate observations. Can you figure out what’s special about that hour?

weather |>
  count(year, month, day, hour, origin) |>
  filter(n > 1)

# 4. We know that some days of the year are special and fewer people than usual fly on them (e.g., Christmas eve and Christmas day). How might you represent that data as a data frame? What would be the primary key? How would it connect to the existing data frames?

special_days <- tibble(
  month = c(12, 12, 1, 7),
  day = c(24, 25, 1, 4),
  holiday = c(
    "Christmas Eve",
    "Christmas Day",
    "New Year's Day",
    "Independence Day"
  )
)
special_days

# 5. Draw a diagram illustrating the connections between the Batting, People, and Salaries data frames in the Lahman package. Draw another diagram that shows the relationship between People, Managers, AwardsManagers. How would you characterize the relationship between the Batting, Pitching, and Fielding data frames?

library(ggplot2)

# Diagrama: Batting, People y Salaries

ggplot() +
  geom_point(aes(x = 2, y = 2), size = 8) +
  geom_point(aes(x = 1, y = 1), size = 8) +
  geom_point(aes(x = 3, y = 1), size = 8) +
  annotate("text", x = 2, y = 2, label = "People") +
  annotate("text", x = 1, y = 1, label = "Batting") +
  annotate("text", x = 3, y = 1, label = "Salaries") +
  geom_segment(
    aes(x = 2, y = 1.9, xend = 1.1, yend = 1.1),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(
    aes(x = 2, y = 1.9, xend = 2.9, yend = 1.1),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate("text", x = 1.5, y = 1.5, label = "playerID") +
  annotate("text", x = 2.5, y = 1.5, label = "playerID") +
  theme_void()


# Diagrama: People, Managers y AwardsManagers

ggplot() +
  geom_point(aes(x = 2, y = 3), size = 8) +
  geom_point(aes(x = 2, y = 2), size = 8) +
  geom_point(aes(x = 2, y = 1), size = 8) +
  annotate("text", x = 2, y = 3, label = "People") +
  annotate("text", x = 2, y = 2, label = "Managers") +
  annotate("text", x = 2, y = 1, label = "AwardsManagers") +
  geom_segment(
    aes(x = 2, y = 2.9, xend = 2, yend = 2.1),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_segment(
    aes(x = 2, y = 1.9, xend = 2, yend = 1.1),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate("text", x = 2.3, y = 2.5, label = "playerID") +
  annotate("text", x = 2.5, y = 1.5, label = "playerID") +
  theme_void()


# Batting, People y Salaries
#
#                 playerID
# People ---------------------- Batting
#   |
#   | playerID
#   |
# Salaries
#
# People se conecta con Batting y Salaries mediante playerID.


# People, Managers y AwardsManagers
#
#                 playerID
# People ---------------------- Managers
#                                  |
#                                  | playerID
#                                  |
#                            AwardsManagers
#
# People se conecta con Managers mediante playerID.
# Managers y AwardsManagers también se conectan mediante playerID.


# Batting, Pitching y Fielding
#
# Las tres son tablas paralelas.
# Todas contienen estadísticas de jugadores,
# pero de diferentes aspectos del juego:
#
# Batting  -> bateo
# Pitching -> lanzamiento
# Fielding -> defensa
#
# Se relacionan principalmente mediante playerID
# y yearID.


# 19.3.4 exercises: Basic joins -------------------------------------------

# 1. Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

worst_hours <- flights |>
  group_by(year, month, day, hour) |>
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_delay)) |>
  slice_head(n = 48)

worst_hours

worst_hours |>
  left_join(
    weather,
    join_by(year, month, day, hour)
  )

# 2. Imagine you’ve found the top 10 most popular destinations using this code: top_dest <- flights2 |> count(dest, sort = TRUE) |> head(10) How can you find all flights to those destinations?

top_dest <- flights |>
  count(dest, sort = TRUE) |>
  head(10)
flights |>
  semi_join(top_dest, join_by(dest))

# 3. Does every departing flight have corresponding weather data for that hour?

flights |>
  anti_join(
    weather,
    join_by(year, month, day, hour, origin)
  )

flights |>
  anti_join(
    weather,
    join_by(year, month, day, hour, origin)
  ) |>
  nrow()

# 4. What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)

flights |>
  distinct(tailnum) |>
  anti_join(planes, join_by(tailnum))

flights |>
  distinct(tailnum) |>
  anti_join(planes, join_by(tailnum)) |>
  count(is.na(tailnum))

# 5. Add a column to planes that lists every carrier that has flown that plane. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned in previous chapters.

plane_carriers <- flights |>
  distinct(tailnum, carrier) |>
  group_by(tailnum) |>
  summarise(
    carriers = paste(carrier, collapse = ", "),
    .groups = "drop"
  )

planes2 <- planes |>
  left_join(plane_carriers, join_by(tailnum))

planes2

flights |>
  distinct(tailnum, carrier) |>
  count(tailnum) |>
  filter(n > 1)

# 6. Add the latitude and the longitude of the origin and destination airport to flights. Is it easier to rename the columns before or after the join?

origin_airports <- airports |>
  select(
    origin = faa,
    origin_lat = lat,
    origin_lon = lon
  )

dest_airports <- airports |>
  select(
    dest = faa,
    dest_lat = lat,
    dest_lon = lon
  )

flights_airports <- flights |>
  left_join(origin_airports, join_by(origin)) |>
  left_join(dest_airports, join_by(dest))

flights_airports

flights_airports |>
  select(origin, origin_lat, origin_lon,
         dest, dest_lat, dest_lon)

origin_airports

# 7. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. 

avg_delay <- flights |>
  group_by(dest) |>
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    .groups = "drop"
  )
avg_delay_airports <- avg_delay |>
  left_join(
    airports,
    join_by(dest == faa)
  )
avg_delay_airports |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(aes(color = avg_delay, size = avg_delay)) +
  coord_quickmap()

# 8. What happened on June 13 2013? Draw a map of the delays, and then use Google to cross-reference with the weather.

june13 <- flights |>
  filter(month == 6, day == 13)
june13_delay <- june13 |>
  group_by(origin) |>
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  )
june13_map <- june13_delay |>
  left_join(
    airports,
    join_by(origin == faa)
  )
june13_map |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(aes(size = avg_delay, color = avg_delay)) +
  coord_quickmap()
weather |>
  filter(month == 6, day == 13)
