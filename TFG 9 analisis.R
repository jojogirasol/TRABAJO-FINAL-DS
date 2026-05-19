# TFG - Analisis comentarios  - hierarchical clustering dendrogram
# Mon Apr 27 18:10:18 2026 ------------------------------

install.packages("tidytext")
install.packages("SnowballC")
install.packages("quanteda")
install.packages("quanteda.textplots")

library(tidyverse)
library(dplyr)
library(tidytext)
library(SnowballC) # Para raíces de palabras

library(tidyr)

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(rainette)    # Para el análisis de clústeres

# 1. Preparación del Corpus y Segmentación
# Usamos split_segments para evitar el error de doc_id
corpus_tdah <- corpus(df_ig_tdah, text_field = "comentario_original")
segmentos <- split_segments(corpus_tdah, segment_size = 15)

# Tue Apr 28 15:09:33 2026 ------------------------------


# 1. Definimos palabras que ensucian (añade aquí las que veas en tu gráfico)
ruido_social <- c("the", "vamos", "perfil", "foto", "crees", "mejor", "xdd", 
                  "digo", "cualquiera", "tan", "algún", "día", "lado", "diciendo",
                  "llama", "nombre", "know", "solo", "así", "después", "jaja")
# 1. Lista de limpieza final (basada en tu última imagen)
quitar_final <- c("quería", "dijo", "visto", "cosas", "ambas", "mas", "ser", "i'm",
                  "hacer", "puede", "forma", "literalmente", "sonic", "peluche", "vez", 
                  "donde", "jamás", "claro", "bien", "hecho", "judios", "shadow", "pues",
                  "cancion", "nuevamente", "dije", "dice", "mmda", "pasa", "sabes", "mierda")

# 1. Limpieza enfocada en el contenido real
dtm_rainette <- tokens(segmentos, 
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE) %>%
  tokens_remove(stopwords("es")) %>%
  # Eliminamos ruido de usuario y palabras vacías que vimos en tus fotos
  tokens_remove(pattern = c("jajaja*", "lit", "oye", "_sakidss","l.pocoyo", "creo", "hace", "chiko"), valuetype = "glob") %>%
  tokens_remove(ruido_social) %>% # Quitamos el ruido detectado
  tokens_remove(quitar_final) %>% # Quitamos el ruido detectado
  tokens_remove(pattern = "^@.*", valuetype = "regex") %>% 
  tokens_select(min_nchar = 3) %>% 
  dfm() %>%
  # FILTRO CLAVE: Eliminamos documentos vacíos
  #dfm_subset(ntoken(.) > 0) %>%
  # SUBIMOS LA VARA: Solo palabras que aparezcan al menos 3 o 4 veces
  dfm_trim(min_termfreq = 2)

# 2. Obligamos a Rainette a dividir el grupo grande
# Probemos con k = 4 para forzar a que el grupo del 98% se rompa en temas
res <- rainette(dtm_rainette, k = 9, min_segment_size = 1) 

# 3. Gráfico con zoom
rainette_plot(res, dtm_rainette, n_terms = 20, measure = "chi2")

# Tue Apr 28 16:31:49 2026 ------------------------------

summary(res)


# 1. Extraer a qué clúster pertenece cada documento/segmento
grupos_asignados <- cutree_rainette(res, k = 9)

# 2. Convertir el DFM a un formato largo para poder sumar
dfm_long <- convert(dtm_rainette, to = "data.frame") %>%
  pivot_longer(-doc_id, names_to = "feature", values_to = "count") %>%
  filter(count > 0)

# 3. Unir los grupos con las palabras y calcular la frecuencia (n)
tabla_n <- dfm_long %>%
  mutate(cluster = grupos_asignados[doc_id]) %>%
  group_by(cluster, feature) %>%
  summarise(n = sum(count), .groups = 'drop') %>%
  arrange(cluster, desc(n))

# 5. Si quieres ver solo las top 15 por clúster (como en tu gráfico)
tabla_final <- tabla_n %>%
  group_by(cluster) %>%
  slice_max(n, n = 15)

#View(tabla_final)

# Tue Apr 28 16:14:49 2026 ------------------------------


# 1. Extraer los clústeres (usamos k=9 porque es lo que veo en tu último gráfico)
grupos <- cutree_rainette(res, k = 9) 

# 2. Crear el dataframe de grupos usando el orden de las filas
# Esto evita el error de "nombres vacíos"
df_grupos <- data.frame(
  doc_id = paste0("text", 1:length(grupos)), 
  cluster = as.vector(grupos)
)

# 3. Convertir el DFM asegurándonos de que los doc_id coincidan
dfm_long <- convert(dtm_rainette, to = "data.frame")
# Forzamos los nombres de los documentos para que coincidan con df_grupos
dfm_long$doc_id <- paste0("text", 1:nrow(dfm_long))

# 4. Transformar a formato largo y unir
tabla_n_final <- dfm_long %>%
  pivot_longer(-doc_id, names_to = "feature", values_to = "count") %>%
  filter(count > 0) %>%
  inner_join(df_grupos, by = "doc_id") %>%
  group_by(cluster, feature) %>%
  summarise(n = sum(count), .groups = 'drop') %>%
  arrange(cluster, desc(n))

#View(tabla_n_final)

tabla_n_limpia <- tabla_n_final %>%
  filter(!is.na(cluster))
View(tabla_n_limpia)


