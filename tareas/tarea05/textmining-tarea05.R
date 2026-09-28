library(tidyverse)
library(tidytext)
mckinsey <- read_csv(
  "https://gitlab.com/uploads/-/system/personal_snippet/4897254/d83847870c9577a22a20063379f91120/DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv"
)
glimpse(mckinsey)

mckinsey <- mckinsey %>%
  select(-...1)
glimpse(mckinsey)

mckinsey %>%
  select(title, date, description) %>%
  print(n = 10)
range(mckinsey$date)

mckinsey %>%
  summarise(
    n_articulos = n(),
    titulos_na = sum(is.na(title)),
    fechas_na = sum(is.na(date)),
    textos_na = sum(is.na(article_text))
  )
mckinsey <- mckinsey %>%
  mutate(
    n_palabras = str_count(article_text, "\\S+")
  )

mckinsey %>%
  summarise(
    minimo = min(n_palabras),
    promedio = mean(n_palabras),
    mediana = median(n_palabras),
    maximo = max(n_palabras)
  )
ggplot(mckinsey, aes(x = n_palabras)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución de la longitud de los artículos",
    x = "Cantidad de palabras",
    y = "Cantidad de artículos"
  )
mckinsey_words <- mckinsey %>%
  unnest_tokens(word, article_text)



### Descripción del corpus

#El corpus está compuesto por 146 artículos de *McKinsey Mind the Gap*, publicados entre marzo de 2022 y octubre de 2025. No se observan valores faltantes en las variables de título, fecha ni texto de los artículos.

#En cuanto a su extensión, los documentos contienen en promedio 626 palabras, con una mediana de 612, un mínimo de 155 y un máximo de 1.026 palabras.

#En términos generales, los documentos resultan comparables por pertenecer a una misma colección y presentar extensiones relativamente similares, aunque existe cierta variabilidad en su longitud que debe considerarse al comparar frecuencias absolutas.



mckinsey_words <- mckinsey %>%
  select(title, date, article_text) %>%
  unnest_tokens(word, article_text)
mckinsey_words
glimpse(mckinsey_words)

mckinsey_words_clean <- mckinsey_words %>%
  anti_join(stop_words, by = "word")
mckinsey_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

custom_stop_words <- tibble(
  word = c(
    "mckinsey",
    "partner",
    "managing",
    "client",
    "senior",
    "it’s"
  )
)
mckinsey_words_clean <- mckinsey_words %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word")
top_words <- mckinsey_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

top_words

top_words %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  labs(
    title = "Palabras más frecuentes en el corpus",
    x = "Frecuencia",
    y = NULL
  )


mckinsey_words_clean %>%
  summarise(
    total_tokens = n(),
    palabras_unicas = n_distinct(word)
  )
mckinsey_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 30)


get_sentiments("bing")
sentimiento_bing <- mckinsey_words_clean %>%
  inner_join(get_sentiments("bing"), by = "word")
sentimiento_bing %>%
  count(sentiment)

sentimiento_por_nota <- sentimiento_bing %>%
  count(title, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    sentimiento_neto = positive - negative
  )

sentimiento_por_nota <- sentimiento_por_nota %>%
  left_join(
    mckinsey %>% select(title, n_palabras),
    by = "title"
  ) %>%
  mutate(
    sentimiento_normalizado =
      (positive - negative) / n_palabras * 100
  )

sentimiento_bing %>%
  count(sentiment)
sentimiento_por_nota %>%
  arrange(desc(sentimiento_normalizado)) %>%
  select(title, positive, negative, sentimiento_normalizado) %>%
  print(n = 10)

sentimiento_general <- sentimiento_bing %>%
  count(sentiment) %>%
  mutate(
    proporcion = n / sum(n),
    porcentaje = proporcion * 100
  )

sentimiento_general

install.packages("textdata")
library(textdata)
get_sentiments("afinn")

sentimiento_afinn <- mckinsey_words_clean %>%
  inner_join(get_sentiments("afinn"), by = "word")
sentimiento_afinn %>%
  select(word, value) %>%
  head(20)


afinn_por_nota <- sentimiento_afinn %>%
  group_by(title) %>%
  summarise(
    sentimiento_afinn = sum(value),
    .groups = "drop"
  ) %>%
  left_join(
    mckinsey %>% select(title, n_palabras),
    by = "title"
  ) %>%
  mutate(
    afinn_normalizado = sentimiento_afinn / n_palabras * 100
  )
afinn_por_nota %>%
  arrange(desc(afinn_normalizado)) %>%
  select(title, sentimiento_afinn, afinn_normalizado) %>%
  print(n = 10)
afinn_por_nota %>%
  arrange(afinn_normalizado) %>%
  select(title, sentimiento_afinn, afinn_normalizado) %>%
  print(n = 10)
sentimiento_afinn %>%
  summarise(
    sentimiento_total = sum(value),
    promedio = mean(value),
    mediana = median(value)
  )


### Análisis de sentimiento

#El análisis de sentimiento mediante el diccionario binario **Bing** muestra un predominio de términos positivos: 61,5% de las palabras clasificadas presentan sentimiento positivo y 38,5% negativo.

#Al incorporar **AFINN**, que asigna valores graduados según la intensidad del sentimiento, se mantiene esta tendencia. El corpus presenta un puntaje total de 2.320 y un valor promedio de 0,662 entre los términos reconocidos.

#Sin embargo, se observa heterogeneidad entre las notas, con artículos marcadamente positivos y otros con puntuaciones negativas. En conjunto, ambos diccionarios sugieren un tono predominantemente positivo en el corpus.


install.packages("tm")
library(tm)
mckinsey_dtm <- mckinsey_words_clean %>%
  count(title, word) %>%
  cast_dtm(
    document = title,
    term = word,
    value = n
  )

mckinsey_dtm

install.packages("topicmodels")
library(topicmodels)

install.packages("reshape2")
library(reshape2)

lda_10 <- LDA(
  mckinsey_dtm,
  k = 10,
  control = list(seed = 123)
)
beta_10 <- tidy(lda_10, matrix = "beta")
beta_10

top_terms_10 <- beta_10 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

top_terms_10
print(top_terms_10, n = 100)



set.seed(123)

lda_15 <- LDA(
  mckinsey_dtm,
  k = 15,
  control = list(seed = 123)
)
beta_15 <- tidy(lda_15, matrix = "beta")
top_terms_15 <- beta_15 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

print(top_terms_15, n = 150)

top_terms_15 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(
    title = "Principales términos por tópico - LDA (k = 15)",
    x = "Beta",
    y = NULL
  )
top_terms_10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(
    title = "Principales términos por tópico - LDA (k = 10)",
    x = "Beta",
    y = NULL
  )


### Modelado de tópicos mediante LDA

#El modelado de tópicos mediante **LDA** se realizó considerando alternativamente **10 y 15 tópicos**. Para interpretar los resultados se analizaron los términos con mayor probabilidad β dentro de cada tópico.

#Con **k = 10** se identificaron dimensiones relativamente amplias vinculadas, entre otras, con inteligencia artificial, empleo, consumo, moda, bienestar y salud mental.

#Al aumentar a **k = 15**, algunos de estos tópicos se desagregan en dimensiones más específicas. Por ejemplo, dentro de la temática laboral aparecen tópicos diferenciados relacionados con empleo, liderazgo, educación y habilidades, trabajo híbrido/remoto y el impacto de la inteligencia artificial en el trabajo. De forma similar, las cuestiones de salud se diferencian entre bienestar, salud mental y *healthcare*.

#Sin embargo, algunos de los 15 tópicos presentan mayor superposición y resultan menos claramente interpretables.



mckinsey_bigrams <- mckinsey %>%
  select(title, article_text) %>%
  unnest_tokens(
    bigram,
    article_text,
    token = "ngrams",
    n = 2
  )

mckinsey_bigrams

bigrams_separados <- mckinsey_bigrams %>%
  separate(
    bigram,
    into = c("word1", "word2"),
    sep = " "
  )
bigrams_limpios <- bigrams_separados %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  )
bigrams_frecuentes <- bigrams_limpios %>%
  count(word1, word2, sort = TRUE)

bigrams_frecuentes %>%
  slice_head(n = 30)
top_bigrams <- bigrams_frecuentes %>%
  slice_head(n = 20) %>%
  unite(bigram, word1, word2, sep = " ")
ggplot(
  top_bigrams,
  aes(x = n, y = reorder(bigram, n))
) +
  geom_col() +
  labs(
    title = "Bigrams más frecuentes del corpus",
    x = "Frecuencia",
    y = NULL
  )


install.packages("igraph")
install.packages("ggraph")
library(igraph)
library(ggraph)

bigram_graph <- bigrams_frecuentes %>%
  filter(n >= 10) %>%
  graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.5) +
  geom_node_point() +
  geom_node_text(
    aes(label = name),
    repel = TRUE
  ) +
  theme_void() +
  labs(
    title = "Red de bigrams frecuentes"
  )
bigrams_frecuentes %>%
  slice_head(n = 30)

ruido_bigrams <- c(
  "mckinsey",
  "partner",
  "managing",
  "senior",
  "hilton",
  "segel"
)

bigrams_red <- bigrams_limpios %>%
  filter(
    !word1 %in% ruido_bigrams,
    !word2 %in% ruido_bigrams
  ) %>%
  count(word1, word2, sort = TRUE)
bigrams_red %>%
  slice_head(n = 30)

library(igraph)
library(ggraph)

bigram_graph <- bigrams_red %>%
  filter(n >= 15) %>%
  graph_from_data_frame()

set.seed(123)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(
    aes(width = n),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_node_point() +
  geom_node_text(
    aes(label = name),
    repel = TRUE
  ) +
  theme_void() +
  labs(
    title = "Red de bigrams frecuentes"
  )


negaciones <- bigrams_separados %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

negaciones %>%
  slice_head(n = 20)
negaciones_afinn <- negaciones %>%
  inner_join(
    get_sentiments("afinn"),
    by = c("word2" = "word")
  )

negaciones_afinn %>%
  arrange(desc(n))

negaciones_afinn <- negaciones_afinn %>%
  mutate(
    contribucion_original = n * value,
    contribucion_negada = n * (-value)
  )

negaciones_afinn %>%
  arrange(desc(n))

### Negaciones y análisis de sentimiento

#El análisis de las construcciones `not + word` evidencia una limitación del análisis de sentimiento basado en unigramas. Por ejemplo, `not alone` aparece diez veces en el corpus, mientras que **AFINN** asigna a *alone* un valor de −2. Al considerar únicamente la palabra individual, estas apariciones contribuirían negativamente al sentimiento, aunque la expresión *not alone* puede transmitir una idea positiva.

#El problema también aparece en sentido contrario: *like* tiene una puntuación positiva (+2), mientras que *not like* puede expresar una valoración negativa.

#Por lo tanto, la presencia de negaciones muestra que los resultados obtenidos mediante diccionarios deben interpretarse con cautela, ya que la tokenización por palabras puede perder información contextual relevante.