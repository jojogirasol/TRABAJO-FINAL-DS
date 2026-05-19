# TFG - Extranccion de datos de videos
# Thu Mar 26 19:37:40 2026 ------------------------------

# 1. Instalación y Carga de Librerías
install.packages("jsonlite")
install.packages("textclean")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("textclean")) install.packages("textclean")

library(jsonlite)
library(textclean)

# 2. Configuración de Ruta (MUY IMPORTANTE)
# Usamos la ruta que definiste para que R sepa exactamente dónde está el programa
ruta_ytdlp <- "C:/ordenador/usal/soc 4to usal/data science/yt-dlp.exe"
file.exists(ruta_ytdlp)

# 3. Función de Extracción
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

# 4. Ejemplo de uso real
link_tiktok <- "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW"
mi_data_etno <- extraer_meta_video(link_tiktok, ruta_ytdlp)

print(mi_data_etno)


system2(ruta_ytdlp, args = c("--dump-json", "--no-playlist", "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215"), stdout = TRUE)


extraer_meta_video <- function(url, path_programa) {
  
  # 1. Limpiamos la URL (quitamos lo que va después del signo ?)
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # 2. Creamos el comando con COOKIES y USER AGENT
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

