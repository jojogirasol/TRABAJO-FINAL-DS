# TFG - Extranccion de datos de videos (Solo sirve con tik tok)
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
ruta_ytdlp <- "C:/ordenador/usal/soc 4to usal/data science/yt-dlp.exe"
file.exists(ruta_ytdlp)


# 2. Función de Extracción
extraer_meta_video <- function(url, path_programa) {
  
  # Usamos shQuote para que las rutas con espacios (como "soc 4to usal") no den error
  comando <- paste(shQuote(path_programa), "--dump-json --no-playlist", shQuote(url))
  
  tryCatch({
    # Ejecutamos el comando
    resultado_json <- system(comando, intern = TRUE)
    
    # Convertimos JSON a lista
    datos <- fromJSON(resultado_json)
    
    # Procesamos emojis (Si la descripción es NULL, ponemos texto vacío)
    desc_original <- ifelse(is.null(datos$description), "", datos$description)
    desc_procesada <- textclean::replace_emoji(desc_original)
    
    # Creamos el Data Frame
    df_video <- data.frame(
      plataforma      = ifelse(is.null(datos$extractor_key), NA, datos$extractor_key),
      usuario         = ifelse(is.null(datos$uploader), NA, datos$uploader),
      titulo          = ifelse(is.null(datos$title), NA, datos$title),
      fecha           = as.Date(datos$upload_date, format = "%Y%m%d"),
      vistas          = ifelse(is.null(datos$view_count), NA, datos$view_count),
      likes           = ifelse(is.null(datos$like_count), NA, datos$like_count),
      descripcion_raw = desc_original,   # Mantiene los emojis
      descripcion_txt = desc_procesada,  # Emojis convertidos a texto [smiling_face]
      url_original    = url,
      stringsAsFactors = FALSE
    )
    
    return(df_video)
    
  }, error = function(e) {
    message("Error con el link: ", url)
    return(NULL)
  })
}


link_tiktok <- "https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY"

system2(ruta_ytdlp, args = c("--dump-json", "--no-playlist", "https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY"), stdout = TRUE)

extraer_meta_video <- function(url, path_programa) {
  
  # Limpiamos la URL
  url_limpia <- strsplit(url, "https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY")[[1]][1]
  
  # Comando simplificado (QUITAMOS las cookies para evitar el error de Chrome)
  comando <- paste(shQuote(path_programa), "--dump-json --no-playlist", shQuote(url_limpia))
  
  tryCatch({
    # Capturamos la salida
    resultado_vector <- system(comando, intern = TRUE)
    
    # IMPORTANTE: Pegamos el texto para que fromJSON no de error léxico
    resultado_completo <- paste(resultado_vector, collapse = "") 
    
    # Convertimos a lista
    datos <- fromJSON(resultado_completo)
    
    # Procesamos descripción
    desc_original <- ifelse(is.null(datos$description), "", datos$description)
    desc_procesada <- textclean::replace_emoji(desc_original)
    
    # Creamos el Data Frame
    df_video <- data.frame(
      plataforma      = datos$extractor_key,
      usuario         = datos$uploader,
      titulo          = datos$title,
      fecha           = as.Date(datos$upload_date, format = "%Y%m%d"),
      vistas          = ifelse(is.null(datos$view_count), NA, datos$view_count),
      likes           = ifelse(is.null(datos$like_count), NA, datos$like_count),
      comentarios     = ifelse(is.null(datos$comment_count), NA, datos$comment_count),
      descripcion_raw = desc_original,
      descripcion_txt = desc_procesada,
      url_original    = url_limpia,
      stringsAsFactors = FALSE
    )
    
    return(df_video)
    
  }, error = function(e) {
    # Este mensaje nos dirá si el problema es el JSON o el link
    message("Error procesando el video: ", e$message)
    return(NULL)
  })
}

link_tiktok <- "https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY"
mi_data_etno <- extraer_meta_video(link_tiktok, ruta_ytdlp)

print(mi_data_etno)

View(mi_data_etno) # Esto te abrirá la tabla en una pestaña nueva

extraer_comentarios_con_archivo <- function(url, path_programa, path_cookies) {
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Comando con el archivo de cookies que ya te funcionó
  comando <- paste(shQuote(path_programa), 
                   "--dump-json --no-playlist --get-comments",
                   "--cookies", shQuote(path_cookies),
                   shQuote(url_limpia))
  
  tryCatch({
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    datos <- fromJSON(resultado_completo)
    
    # Buscamos los comentarios en las dos ubicaciones posibles de yt-dlp
    comentarios_raw <- NULL
    if (!is.null(datos$comments)) {
      comentarios_raw <- datos$comments
    } else if (!is.null(datos$entries)) {
      comentarios_raw <- datos$entries
    }
    
    if (is.null(comentarios_raw) || length(comentarios_raw) == 0) {
      message("TikTok devolvió datos pero la lista de comentarios está vacía.")
      return(NULL)
    }
    
    # Creamos el dataframe adaptándonos a los nombres de columna de TikTok
    df <- data.frame(
      autor = if(!is.null(comentarios_raw$author)) comentarios_raw$author else "Anónimo",
      texto_original = comentarios_raw$text,
      texto_limpio = textclean::replace_emoji(comentarios_raw$text),
      likes = if(!is.null(comentarios_raw$like_count)) comentarios_raw$like_count else 0,
      stringsAsFactors = FALSE
    )
    
    return(df)
    
  }, error = function(e) {
    message("Error en el procesado: ", e$message)
    return(NULL)
  })
}

mis_comentarios <- extraer_comentarios_con_archivo("https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY", ruta_ytdlp, ruta_cookies)

# 1. Leer el archivo
ruta_txt <- "C:/ordenador/usal/soc 4to usal/data science/comentarios_raw.txt"
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

# Guardar en CSV
write.csv(df_etno_tdah, "C:/ordenador/usal/soc 4to usal/data science/tabla_comentarios_final.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# FIN #
# Sat Apr  4 16:27:39 2026 ------------------------------

# Crear un "corpus" de texto
corpus <- Corpus(VectorSource(df_etno_tdah$comentario_original))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
# Quitar palabras comunes (artículos, preposiciones)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

# Generar la nube
wordcloud(corpus, max.words = 50, colors = brewer.pal(8, "Dark2"))
