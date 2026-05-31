###
##TFG - Analisis de datos de videos (lexical)
# Tue May 26 03:16:34 2026 ------------------------------

# A partir de la tabla excel_tfg1, se aplicara el script TFG9.2, generando un 
# lexical con la informacion de las descripciones de los videos

library(tidyverse)
library(dplyr)
library(quanteda)
library(quanteda.textplots)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggforce) 
library(readxl)

excel_tfg1 <- read_excel("Copia de Base de datos TFG.xlsx")

# la tabla excel_tfg1 recoge en una tabla el contenido de los videos recogidos 
# manualmente (transcripciones), además de otros datos, entre ellos los datos
# obtenido a través de TFG6 y TFG8

corpus_tfg3 <- corpus(excel_tfg1, text_field = "DESCRIPCIÓN")
tokens3 <- tokens(corpus_tfg3, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("spanish"))

fcm_tfg <- fcm(tokens3, context = "window", window = 5, count = "frequency", tri = FALSE)
frequencias <- colSums(fcm_tfg)
top_words <- names(sort(frequencias, decreasing = TRUE)[1:50])
fcm_select <- fcm_select(fcm_tfg, pattern = top_words)

matrix_tfg <- as.matrix(fcm_select)
graph_tfg <- graph_from_adjacency_matrix(matrix_tfg, mode = "undirected", weighted = TRUE, diag = FALSE)

clusters <- cluster_louvain(graph_tfg)
V(graph_tfg)$community <- as.factor(membership(clusters))
V(graph_tfg)$degree <- degree(graph_tfg)

ggraph(graph_tfg, layout = "nicely") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE, color = "grey70") +
  # El "hull" crea las áreas sombreadas de la imagen.jpg
  geom_mark_hull(aes(x, y, group = community, fill = community, label = community), 
                 alpha = 0.15, color = NA, concavity = 5) +
  geom_node_point(aes(size = degree, color = community), show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme_void()

# Tue May 26 03:35:15 2026 ------------------------------
# Tue May 26 14:21:39 2026 ------------------------------

# TFG - Analisis de datos de videos (transcripciones)

# es necesario limpiar el enviroment y volver a cargar el archivo excel_tfg1
# para obtener un nuevo mapa lexico

corpus_tfg3 <- corpus(excel_tfg1, text_field = "ANALISIS")
tokens3 <- tokens(corpus_tfg3, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("spanish"))

fcm_tfg <- fcm(tokens3, context = "window", window = 5, count = "frequency", tri = FALSE)
frequencias <- colSums(fcm_tfg)
top_words <- names(sort(frequencias, decreasing = TRUE)[1:50])
fcm_select <- fcm_select(fcm_tfg, pattern = top_words)

matrix_tfg <- as.matrix(fcm_select)
graph_tfg <- graph_from_adjacency_matrix(matrix_tfg, mode = "undirected", weighted = TRUE, diag = FALSE)

clusters <- cluster_louvain(graph_tfg)
V(graph_tfg)$community <- as.factor(membership(clusters))
V(graph_tfg)$degree <- degree(graph_tfg)

ggraph(graph_tfg, layout = "nicely") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE, color = "grey70") +
  # El "hull" crea las áreas sombreadas de la imagen.jpg
  geom_mark_hull(aes(x, y, group = community, fill = community, label = community), 
                 alpha = 0.15, color = NA, concavity = 5) +
  geom_node_point(aes(size = degree, color = community), show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme_void()
