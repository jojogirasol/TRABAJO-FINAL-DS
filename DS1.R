# TFG - Analisis de datos de videos (descripciones)

# Tue May 26 01:14:46 2026 ------------------------------

library(tidyverse)
library(dplyr)

# A partir de las tablas creadas en TFG6 y TFG8, se unificaran es una sola tabla
# para así aplicar el script TFG9, generando un cluster con la informacion de 
# las descripciones de los videos

df_ig <- mi_df_total # videos de intagram
df_tt <- tabla_videos_completa # videos de tiktok

df_tfg <- bind_rows(df_ig,df_tt)
view(df_tfg)

# TFG 9 modificado para aplicarlo a df_tfg
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(rainette)

corpus_tfg <- corpus(df_tfg, text_field = "titulo")
segmentos <- split_segments(corpus_tfg, segment_size = 15)

# eliminación de ruido, palabras que no proporcionen valor analitico
# se efectuó un primer cluster sin aplicar ruido_social para posteriormente 
# incluir en la palabras irrelevantes que destaca el cluster
ruido_social <- c("wolff", "eilish", "posiblemente", "ducks", "#catanimation",
                  "the", "aunque", "are", "and", "minguet","emma", "día", 
                  "veces", "muchas", "lado", "mismo", "alguna", "video", 
                  "tiktok", "instagram", "creo", "incluso")

# creaccion del cluster
dtm_rainette <- tokens(segmentos, 
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE) %>%
  tokens_remove(stopwords("es")) %>%
  tokens_remove(ruido_social) %>% # Quitamos el ruido detectado
  tokens_remove(pattern = "^@.*", valuetype = "regex") %>% 
  tokens_select(min_nchar = 3) %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 2)

res <- rainette(dtm_rainette, k = 5, min_segment_size = 2) 
rainette_plot(res, dtm_rainette, n_terms = 20, measure = "chi2")


grupos <- cutree_rainette(res, k = 5) 

df_grupos <- data.frame(
  doc_id = paste0("text", 1:length(grupos)), 
  cluster = as.vector(grupos)
)

dfm_long <- convert(dtm_rainette, to = "data.frame")
dfm_long$doc_id <- paste0("text", 1:nrow(dfm_long))

tabla_cluster <- dfm_long %>%
  pivot_longer(-doc_id, names_to = "feature", values_to = "count") %>%
  filter(count > 0) %>%
  inner_join(df_grupos, by = "doc_id") %>%
  group_by(cluster, feature) %>%
  summarise(n = sum(count), .groups = 'drop') %>%
  arrange(cluster, desc(n))

tabla_cluster%>%
  filter(!is.na(cluster))
View(tabla_cluster)

### AÑADIR PORCENTAJES A LA TABLA

tabla_cluster_porcentajes <- tabla_cluster %>%
  group_by(cluster) %>%
  mutate(
    total_cluster = sum(n),
    porcentaje = round((n / total_cluster) * 100, 2)
  ) %>%
  arrange(cluster, desc(porcentaje))

View(tabla_cluster_porcentajes)

top_cluster <- tabla_cluster_porcentajes %>%
  group_by(cluster) %>%
  slice_max(order_by = porcentaje, n = 10) %>%
  ungroup()

View(top_cluster)

