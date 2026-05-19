# TFG - Extranccion de datos de videos
# Mon Mar 30 21:39:13 2026 ------------------------------


# 1. Instalación y Carga de Librerías
install.packages("jsonlite")
install.packages("textclean")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("textclean")) install.packages("textclean")

library(jsonlite)
library(textclean)

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

# Ejemplo de uso real
link_tiktok <- "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW"
mi_data_etno <- extraer_meta_video(link_tiktok, ruta_ytdlp)

print(mi_data_etno)

system2(ruta_ytdlp, args = c("--dump-json", "--no-playlist", "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215"), stdout = TRUE)


extraer_meta_video <- function(url, path_programa) {
  
  # Limpiamos la URL (quitamos lo que va después del signo ?)
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Creamos el comando con COOKIES y USER AGENT
  # NOTA: Cambia 'chrome' por 'edge' o 'firefox' si usas otro navegador
  comando <- paste(
    shQuote(path_programa), 
    "--dump-json", 
    "--no-playlist", 
    "--cookies-from-browser chrome", 
    "--user-agent \"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36\"",
    shQuote(url_limpia)
  )
  
  tryCatch({
    resultado_json <- system(comando, intern = TRUE)
    datos <- fromJSON(resultado_json)
    
    desc_original <- ifelse(is.null(datos$description), "", datos$description)
    desc_procesada <- textclean::replace_emoji(desc_original)
    
    df_video <- data.frame(
      plataforma = datos$extractor_key,
      usuario = datos$uploader,
      titulo = datos$title,
      fecha = as.Date(datos$upload_date, format = "%Y%m%d"),
      vistas = ifelse(is.null(datos$view_count), NA, datos$view_count),
      likes = ifelse(is.null(datos$like_count), NA, datos$like_count),
      descripcion_raw = desc_original,
      descripcion_txt = desc_procesada,
      url_original = url_limpia,
      stringsAsFactors = FALSE
    )
    return(df_video)
    
  }, error = function(e) {
    # Si falla, pedimos a R que nos diga qué pasó exactamente
    message("Error detallado: ", e$message)
    return(NULL)
  })
}


mi_data_etno <- extraer_meta_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW", ruta_ytdlp)
View(mi_data_etno) # Esto te abrirá la tabla en una pestaña nueva



extraer_meta_video <- function(url, path_programa) {
  
  # Limpiamos la URL
  url_limpia <- strsplit(url, "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW")[[1]][1]
  
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

# Ejemplo de uso real
link_tiktok <- "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW"
mi_data_etno <- extraer_meta_video(link_tiktok, ruta_ytdlp)

print(mi_data_etno)


#mi_data_etno <- extraer_meta_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW", ruta_ytdlp)
View(mi_data_etno) # Esto te abrirá la tabla en una pestaña nueva


# 3. comentarios de los videos

extraer_comentarios_video <- function(url, path_programa) {
  
  # Limpiamos la URL
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Comando: --get-comments extrae la sección de comentarios en el JSON
  comando <- paste(shQuote(path_programa), 
                   "--dump-json", 
                   "--no-playlist", 
                   "--get-comments", 
                   shQuote(url_limpia))
  
  tryCatch({
    # Capturamos la salida
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    
    # Convertimos a lista
    datos <- fromJSON(resultado_completo)
    
    # Extraemos la lista de comentarios
    comentarios_raw <- datos$comments
    
    if (is.null(comentarios_raw) || length(comentarios_raw) == 0) {
      message("No se encontraron comentarios o están desactivados.")
      return(NULL)
    }
    
    # Creamos el Data Frame de comentarios
    df_comentarios <- data.frame(
      autor = comentarios_raw$author,
      fecha = as.Date(as.POSIXct(comentarios_raw$timestamp, origin="1970-01-01")),
      texto_raw = comentarios_raw$text,
      texto_limpio = textclean::replace_emoji(comentarios_raw$text),
      likes_comentario = comentarios_raw$like_count,
      stringsAsFactors = FALSE
    )
    
    return(df_comentarios)
    
  }, error = function(e) {
    message("Error al extraer comentarios: ", e$message)
    return(NULL)
  })
}

mis_comentarios <- extraer_comentarios_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp)

extraer_comentarios_video <- function(url, path_programa) {
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Añadimos --cookies-from-browser para que TikTok crea que somos nosotros
  comando <- paste(shQuote(path_programa), 
                   "--dump-json --no-playlist --get-comments",
                   "--cookies-from-browser chrome", # O cambia a 'edge' si usas Edge
                   shQuote(url_limpia))
  
  tryCatch({
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    datos <- fromJSON(resultado_completo)
    
    # En versiones recientes de yt-dlp, los comentarios están en datos$comments
    comentarios_raw <- datos$comments
    
    if (is.null(comentarios_raw) || length(comentarios_raw) == 0) {
      message("TikTok sigue bloqueando el acceso o no hay comentarios.")
      return(NULL)
    }
    
    df_comentarios <- data.frame(
      autor = comentarios_raw$author,
      texto_limpio = textclean::replace_emoji(comentarios_raw$text),
      likes = comentarios_raw$like_count,
      stringsAsFactors = FALSE
    )
    return(df_comentarios)
    
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

mis_comentarios <- extraer_comentarios_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp)
