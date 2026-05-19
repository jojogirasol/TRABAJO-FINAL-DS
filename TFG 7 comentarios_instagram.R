# TFG - Extracción de comentarios Instagram
# Sat Apr 18 17:19:09 2026 ------------------------------

# 1. Instalación y Carga de Librerías

install.packages("jsonlite")
install.packages("textclean")
install.packages("wordcloud")
install.packages("tm")

library(tidyverse)

library(jsonlite)
library(textclean)
library(wordcloud)
library(tm)

# --- SCRIPT PARA COMENTARIOS DE INSTAGRAM ---

# 1. Cargar el texto que pegaste (guárdalo como comentarios_ig.txt)
ruta_txt_ig <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios_ig.txt"
lineas_raw <- readLines(ruta_txt_ig, encoding = "UTF-8", warn = FALSE)

# 2. Limpieza de líneas vacías o de relleno
lineas_ig <- trimws(lineas_raw)
lineas_ig <- lineas_ig[lineas_ig != ""]
lineas_ig <- lineas_ig[!grepl("Foto del perfil de", lineas_ig)] # Quitamos el texto de las fotos

# 3. Algoritmo de Reconstrucción
autores <- c()
comentarios <- c()

i <- 1
while (i <= length(lineas_ig)) {
  
  # El patrón de Instagram copiado suele ser:
  # 1. Nombre de Usuario
  # 2. Tiempo (1 sem, 6 días)
  # 3. El texto del comentario (a veces en varias líneas)
  
  autor_actual <- lineas_ig[i]
  
  # Saltamos el tiempo (la siguiente línea)
  i <- i + 1
  
  # Si la línea actual es el tiempo (ej: 1 sem, 4 días), pasamos a la siguiente que es el texto
  if (i <= length(lineas_ig) && grepl("\\d+\\s(sem|días|día|h|min)", lineas_ig[i])) {
    i <- i + 1
  }
  
  # Recogemos el comentario hasta que encontremos el inicio de un nuevo bloque
  # (un nuevo usuario o el final del archivo)
  texto_acumulado <- ""
  while (i <= length(lineas_ig) && 
         !grepl("^[a-zA-Z0-9._]+$", lineas_ig[i])) { # Si no es un nombre de usuario simple
    texto_acumulado <- paste(texto_acumulado, lineas_ig[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
}

# 4. Crear el Data Frame Final
df_ig_tdah <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  plataforma = "Instagram",
  stringsAsFactors = FALSE
)

# 5. Ver resultado
View(df_ig_tdah)

