#EDGAR y text mining:Reportes anuales de empresas argentinas que cotizan en NYSE

# 1. PAQUETES ------------------------------------------------

library(edgar)
library(tidyverse)
library(tidytext)
library(stringr)
library(rvest)
library(jsonlite)
library(textdata)

# 2. EMPRESAS ------------------------------------------------

empresas <- tibble(
  empresa = c(
    "YPF",
    "Banco Macro",
    "Pampa Energia",
    "Telecom Argentina",
    "Transportadora de Gas del Sur"
  ),
  ticker = c("YPF", "BMA", "PAM", "TEO", "TGS")
)

# 3. OBTENER LOS CIK DESDE LA SEC -----------------------------

url_sec <- "https://www.sec.gov/files/company_tickers.json"

ua <- "Valentina Cue valentinacue10@gmail.com"

conexion <- url(
  url_sec,
  headers = c("User-Agent" = ua)
)

datos_sec <- fromJSON(
  paste(readLines(conexion, warn = FALSE), collapse = "")
)

close(conexion)

cik_empresas <- bind_rows(datos_sec) %>%
  mutate(cik = as.integer(cik_str)) %>%
  filter(ticker %in% empresas$ticker) %>%
  left_join(empresas, by = "ticker")

print(cik_empresas)


# 4. DESCARGAR REPORTES 20-F ----------------------------------

# Se buscan reportes presentados en 2026.
# Generalmente corresponden al ejercicio fiscal 2025.

reportes <- map_dfr(
  cik_empresas$cik,
  function(cik_actual) {
    
    resultado <- getFilings(
      cik.no = cik_actual,
      form.type = "20-F",
      filing.year = 2026,
      quarter = c(1, 2),
      downl.permit = "y",
      useragent = ua
    )
    
    as_tibble(resultado)
  }
)

reportes <- reportes %>%
  mutate(cik = as.integer(cik))

print(reportes)

# 5. LEER ARCHIVOS DESCARGADOS -------------------------------

archivos <- list.files(
  "edgar_Filings",
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Conservar solo archivos 20-F de los CIK seleccionados
archivos_info <- tibble(
  archivo = archivos,
  cik = as.integer(
    str_extract(
      archivos,
      "(?<=Form 20-F/)\\d+"
    )
  )
) %>%
  filter(cik %in% cik_empresas$cik)

textos <- archivos_info %>%
  mutate(
    texto = map_chr(
      archivo,
      ~ paste(
        readLines(.x, warn = FALSE),
        collapse = "\n"
      )
    )
  ) %>%
  left_join(
    cik_empresas %>% select(cik, empresa, ticker),
    by = "cik"
  )

# 6. EXTRAER EL DOCUMENTO PRINCIPAL 20-F ---------------------

extraer_20f <- function(x) {
  
  documentos <- str_extract_all(
    x,
    regex(
      "<DOCUMENT>.*?</DOCUMENT>",
      dotall = TRUE,
      ignore_case = TRUE
    )
  )[[1]]
  
  principal <- documentos[
    str_detect(
      documentos,
      regex(
        "<TYPE>\\s*20-F\\s*(?:\\r?\\n|<)",
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(principal) == 0) {
    return(NA_character_)
  }
  
  principal[1]
}

textos <- textos %>%
  mutate(
    documento_20f = map_chr(texto, extraer_20f)
  )

# 7. LIMPIAR EL TEXTO ----------------------------------------

limpiar_20f <- function(x) {
  
  if (is.na(x)) {
    return(NA_character_)
  }
  
  # Extraer el contenido de la etiqueta TEXT
  contenido <- str_match(
    x,
    regex(
      "<TEXT[^>]*>(.*?)</TEXT>",
      dotall = TRUE,
      ignore_case = TRUE
    )
  )[, 2]
  
  if (is.na(contenido)) {
    return(NA_character_)
  }
  
  # Convertir HTML en texto
  resultado <- tryCatch(
    {
      read_html(contenido) %>%
        html_text2() %>%
        str_squish()
    },
    error = function(e) NA_character_
  )
  
  resultado
}

textos <- textos %>%
  mutate(
    texto_limpio = map_chr(documento_20f, limpiar_20f)
  )

# 8. VERIFICAR LOS REPORTES ----------------------------------

control <- textos %>%
  transmute(
    empresa,
    archivo,
    documento_encontrado = !is.na(documento_20f),
    caracteres = str_length(texto_limpio),
    palabras = str_count(texto_limpio, "\\b\\w+\\b")
  )

print(control, n = Inf)

# Detener si falta algún documento
if (any(is.na(textos$texto_limpio))) {
  stop("Hay documentos 20-F que no pudieron extraerse.")
}

# Un reporte por empresa para una comparación homogénea.
# Si hay duplicados, revisar las fechas antes de elegir.
if (anyDuplicated(textos$ticker)) {
  stop("Hay varios archivos para alguna empresa. Revisar antes de continuar.")
}

if (n_distinct(textos$ticker) != nrow(empresas)) {
  stop("No se encontraron reportes válidos para las cinco empresas.")
}

# 9. INCIDENCIA DE PALABRAS CLAVE -----------------------------

palabras_clave <- c(
  "inflation",
  "devaluation",
  "exchange controls",
  "default",
  "restructuring",
  "imf",
  "capital controls",
  "price controls",
  "country risk",
  "embi"
)

# Crear expresiones regulares


patrones <- tibble(
  palabra = palabras_clave
) %>%
  mutate(
    patron = paste0(
      "\\b",
      str_replace_all(palabra, " ", "\\\\s+"),
      "\\b"
    )
  )

incidencia <- textos %>%
  transmute(
    empresa,
    texto_limpio,
    total_palabras = str_count(
      texto_limpio,
      "\\b\\w+\\b"
    )
  ) %>%
  crossing(patrones) %>%
  mutate(
    frecuencia = map2_int(
      texto_limpio,
      patron,
      ~ str_count(
        .x,
        regex(.y, ignore_case = TRUE)
      )
    ),
    frecuencia_relativa =
      frecuencia / total_palabras * 10000
  ) %>%
  select(
    empresa,
    palabra,
    frecuencia,
    frecuencia_relativa
  )

print(incidencia, n = Inf)

# Gráfico de incidencia

ggplot(
  incidencia,
  aes(
    x = palabra,
    y = frecuencia_relativa,
    fill = empresa
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Incidencia de términos económicos",
    subtitle = "Frecuencia por cada 10.000 palabras",
    x = "Término",
    y = "Frecuencia relativa",
    fill = "Empresa"
  ) +
  theme_minimal()

# 10. TOKENIZACIÓN -------------------------------------------

palabras <- textos %>%
  select(empresa, texto_limpio) %>%
  unnest_tokens(
    word,
    texto_limpio
  ) %>%
  anti_join(
    stop_words,
    by = "word"
  ) %>%
  filter(
    str_detect(word, "^[a-z]+$"),
    str_length(word) > 2
  )

# Palabras más frecuentes por empresa

frecuencias <- palabras %>%
  count(empresa, word, sort = TRUE)

frecuencias %>%
  group_by(empresa) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  print(n = Inf)

# 11. TF-IDF -------------------------------------------------

tfidf <- frecuencias %>%
  bind_tf_idf(
    term = word,
    document = empresa,
    n = n
  )

top_tfidf <- tfidf %>%
  group_by(empresa) %>%
  slice_max(
    tf_idf,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

print(top_tfidf, n = Inf)

# Gráfico TF-IDF

ggplot(
  top_tfidf,
  aes(
    x = tf_idf,
    y = reorder_within(word, tf_idf, empresa)
  )
) +
  geom_col(fill = "skyblue") +
  facet_wrap(
    ~ empresa,
    scales = "free"
  ) +
  scale_y_reordered() +
  labs(
    title = "Palabras distintivas por empresa",
    subtitle = "Análisis TF-IDF",
    x = "TF-IDF",
    y = NULL
  ) +
  theme_minimal()

# 12. DICCIONARIO LOUGHRAN-MCDONALD ---------------------------

loughran <- get_sentiments("loughran")

# Ver categorías del diccionario
loughran %>%
  count(sentiment)

# Identificar palabras asociadas a sentimientos

sentimientos <- palabras %>%
  inner_join(
    loughran,
    by = "word"
  )

# Frecuencia de categorías por empresa

sentimientos_empresa <- sentimientos %>%
  count(
    empresa,
    sentiment,
    name = "frecuencia"
  ) %>%
  group_by(empresa) %>%
  mutate(
    porcentaje =
      frecuencia / sum(frecuencia) * 100
  ) %>%
  ungroup()

print(sentimientos_empresa, n = Inf)

# 13. GRÁFICO DE SENTIMIENTOS --------------------------------

ggplot(
  sentimientos_empresa,
  aes(
    x = empresa,
    y = porcentaje,
    fill = sentiment
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentimientos financieros por empresa",
    subtitle = "Diccionario Loughran-McDonald",
    x = "Empresa",
    y = "Porcentaje de términos clasificados",
    fill = "Categoría"
  ) +
  theme_minimal()

# 14. SENTIMIENTOS PREDOMINANTES ------------------------------

principales_sentimientos <- sentimientos_empresa %>%
  group_by(empresa) %>%
  slice_max(
    porcentaje,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  arrange(empresa)

print(principales_sentimientos)

# 15. PALABRAS FINANCIERAS MÁS FRECUENTES ---------------------

top_sentimientos <- sentimientos %>%
  count(
    empresa,
    sentiment,
    word,
    sort = TRUE
  ) %>%
  group_by(empresa, sentiment) %>%
  slice_max(
    n,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

print(top_sentimientos, n = Inf)



# 16. CONCLUSION ----------------------------------------------------------

# El análisis de los reportes 20-F muestra que las empresas 
# argentinas que cotizan en el NYSE hacen referencia recurrente 
# a términos vinculados con riesgos macroeconómicos como 
# "inflation" y "exchange controls", siendo YPF y Banco Macro 
# las que más los mencionan. 
# El análisis TF-IDF evidencia vocabulario distintivo por empresa, 
# reflejando diferencias en el enfoque de cada sector frente a 
# la coyuntura económica. 
# El estudio de sentimientos con el diccionario Loughran-McDonald 
# indica predominio de categorías como "uncertainty" y "negative", 
# lo que refleja la percepción de riesgo en el entorno financiero 
# argentino. 
# En conjunto, los resultados permiten comparar cómo cada compañía 
# comunica y enfrenta los desafíos económicos en sus reportes 
# anuales, aportando una visión comparativa útil para el análisis 
# financiero.


