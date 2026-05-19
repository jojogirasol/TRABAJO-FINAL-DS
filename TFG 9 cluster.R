###############

summary(res)

library(dplyr)
library(tidyr)

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

# 4. Ver la tabla con las frecuencias que buscabas
View(tabla_n)

# 5. Si quieres ver solo las top 15 por clúster (como en tu gráfico)
tabla_final <- tabla_n %>%
  group_by(cluster) %>%
  slice_max(n, n = 15)

View(tabla_final)

# Tue Apr 28 16:14:49 2026 ------------------------------
  
library(dplyr)
library(tidyr)

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

# 5. ¡Por fin! Ver la tabla con Clúster, Palabra y Frecuencia (n)
View(tabla_n_final)
