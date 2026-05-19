# TFG - Extracción de comentarios Tiktok
# Sat Apr  4 16:56:44 2026 ------------------------------

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

# Configuración de Ruta (MUY IMPORTANTE)
# Usamos la ruta que definiste para que R sepa exactamente dónde está el programa
ruta_ytdlp <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/yt-dlp.exe"
file.exists(ruta_ytdlp)


# 1. Leer el archivo
ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios_raw.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

# 2. Limpieza de espacios
lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]

# 3. Procesamiento inteligente
autores <- c()
comentarios <- c()

i <- 1
while (i <= length(lineas)) {
  
  # A. El autor suele ser la primera línea del bloque
  autor_actual <- lineas[i]
  
  # B. El comentario empieza en la siguiente línea
  # Vamos a juntar todas las líneas hasta que encontremos una fecha (ej: 2025-12-11)
  # o la palabra "Responder" o "Ocultar"
  i <- i + 1
  texto_acumulado <- ""
  
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  # Guardamos si el texto no está vacío
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  # C. Saltamos las líneas de "basura" (Fecha, Responder, Likes) 
  # hasta encontrar el siguiente nombre (que no sea un número)
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

# 4. Crear el Data Frame
df_etno_tdah <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

# 5. Limpieza final: quitar duplicados por si acaso
df_etno_tdah <- distinct(df_etno_tdah)

# Ver resultado
View(df_etno_tdah)

install.packages("writexl")
library(writexl)

write_xlsx(df_etno_tdah, "C:/Users/jshu2/OneDrive/Escritorio/TFG/tabla_comentarios_final.xlsx")

# FIN #