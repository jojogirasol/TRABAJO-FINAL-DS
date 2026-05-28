###
## TFG - Analisis de comentarios de videos (cluster)
# Tue May 26 14:39:09 2026 ------------------------------

library(tidyverse)
library(jsonlite)
library(textclean)
library(wordcloud)
library(tm)

ruta_ytdlp <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/yt-dlp.exe"
file.exists(ruta_ytdlp)

ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t1_dr.kojosarfo.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt1 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt1 <- distinct(dfc_tt1)
View(dfc_tt1)

# Tue May 26 14:50:58 2026 ------------------------------
ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t3_lifeactuator.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt2 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt2 <- distinct(dfc_tt2)
View(dfc_tt2)

# Tue May 26 14:53:37 2026 ------------------------------

ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t15_javipicornell.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt3 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt3 <- distinct(dfc_tt3)
View(dfc_tt3)

# Tue May 26 14:55:16 2026 ------------------------------
  
ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t39_maritarx.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt4 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt4 <- distinct(dfc_tt4)
View(dfc_tt4)

# Tue May 26 14:58:18 2026 ------------------------------

ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t45_shhenia.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt5 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt5 <- distinct(dfc_tt5)
View(dfc_tt5)

# Tue May 26 15:01:08 2026 ------------------------------

ruta_txt <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/comentarios/t46_michicientifico_.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8", warn = FALSE)

lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]
autores <- c()
comentarios <- c()
i <- 1
while (i <= length(lineas)) {
  autor_actual <- lineas[i]
  i <- i + 1
  texto_acumulado <- ""
  while (i <= length(lineas) && 
         !grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) && # No es fecha
         !grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i])) {  # No es botón
    texto_acumulado <- paste(texto_acumulado, lineas[i], sep = " ")
    i <- i + 1
  }
  
  if (nchar(trimws(texto_acumulado)) > 0) {
    autores <- c(autores, autor_actual)
    comentarios <- c(comentarios, trimws(texto_acumulado))
  }
  
  while (i <= length(lineas) && 
         (grepl("^\\d+$", lineas[i]) || # Es solo un número (likes)
          grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d+-\\d+$", lineas[i]) || # Es fecha
          grepl("^Responder$|^Ocultar$|^Ver\\s\\d+", lineas[i]))) { # Son botones
    i <- i + 1
  }
}

dfc_tt6 <- data.frame(
  autor = autores,
  comentario_original = comentarios,
  comentario_limpio = textclean::replace_emoji(comentarios),
  stringsAsFactors = FALSE
)

dfc_tt6 <- distinct(dfc_tt6)
View(dfc_tt6)