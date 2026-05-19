# TFG - Analisis comentarios  - lexical similarity analysis
# Wed May 13 08:11:20 2026 ------------------------------

# 1. Cargar librerías necesarias
install.packages("tidygraph")
install.packages("ggraph")
install.packages("igraph")
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(tidygraph)
library(ggraph)
library(igraph)

# 2. Preparación del Corpus
# Asumiendo que tu dataframe es df_ig_tdah y la columna es comentario_original
corpus_ig <- corpus(df_ig_tdah, text_field = "comentario_original")

# 3. Limpieza y Tokenización
# Filtramos stopwords, puntuación y pasamos a minúsculas
tokens_ig <- tokens(corpus_ig, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("spanish")) # Puedes agregar palabras personalizadas aquí

# 4. Crear Matriz de Co-ocurrencia de Características (FCM)
# El 'window = 5' busca palabras que aparecen cerca una de otra
fcm_ig <- fcm(tokens_ig, context = "window", window = 5, count = "frequency", tri = FALSE)

# 5. Seleccionamos las palabras más frecuentes (Versión quanteda 4.0+)
# En lugar de topfeatures(), sumamos las filas de la matriz
frequencias <- colSums(fcm_ig)
top_words <- names(sort(frequencias, decreasing = TRUE)[1:50])

# Ahora filtramos la matriz original con esas palabras
fcm_select <- fcm_select(fcm_ig, pattern = top_words)

# 5. Convertir a objeto de red (Solución al error de UseMethod)
# Convertimos la FCM a una matriz estándar de R y luego a grafo
matrix_ig <- as.matrix(fcm_select)
graph_ig <- graph_from_adjacency_matrix(matrix_ig, mode = "undirected", weighted = TRUE, diag = FALSE)

# 6. Detectar comunidades y métricas
# Usamos Louvain para los clusters (las "burbujas" de colores)
clusters <- cluster_louvain(graph_ig)
V(graph_ig)$community <- as.factor(membership(clusters))
V(graph_ig)$degree <- degree(graph_ig)

# 7. Visualización (Estilo imagen.jpg)
library(ggforce) # Necesaria para geom_mark_hull

ggraph(graph_ig, layout = "nicely") + 
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE, color = "grey70") +
  # El "hull" crea las áreas sombreadas de la imagen.jpg
  geom_mark_hull(aes(x, y, group = community, fill = community, label = community), 
                 alpha = 0.15, color = NA, concavity = 5) +
  geom_node_point(aes(size = degree, color = community), show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme_void()

#################################

# 5. Convertir a objeto de red y detectar comunidades
graph_ig <- as.igraph(fcm_select)
# Algoritmo para agrupar palabras (las burbujas de colores)
clusters <- cluster_louvain(graph_ig)
V(graph_ig)$community <- as.factor(clusters$membership)
V(graph_ig)$degree <- degree(graph_ig) # Tamaño basado en importancia

# 6. Visualización final (Estilo imagen.jpg)
ggraph(graph_ig, layout = "fr") + # Layout Fruchterman-Reingold para dispersión
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE, color = "grey80") +
  # Crear las áreas de color (hull)
  geom_mark_hull(aes(x, y, group = community, fill = community), 
                 alpha = 0.15, color = NA, concavity = 5) +
  # Nodos (palabras)
  geom_node_point(aes(size = degree, color = community), show.legend = FALSE) +
  # Etiquetas de texto
  geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme_void() +
  labs(title = "Análisis de Similitud Léxica - Comentarios TDAH")