# TFG - Extranccion de datos de videos
# Thu Mar 26 15:32:37 2026 ------------------------------

### 1. Intalación yt-dpl (progarama anexo que sirve para leer datos de plataformas cono Instagram o TikTok)

install.packages("jsonlite")  # Para leer los datos que extraiga el programa
install.packages("processx") # Para ejecutar el programa desde R de forma segura
install.packages("textclean")

library(jsonlite)
library(processx)
library(textclean)

# Ruta donde guardaste el programa (ajusta según tu PC)
ruta_ytdlp <- "C:/ordenador/usal/soc 4to usal/data science/yt-dlp.exe"
file.exists(ruta_ytdlp)

# Comando para extraer info de un link de TikTok
 url <- "https://vm.tiktok.com/ZNRUW6dPo/"
# Ejecutar y capturar resultado
 resultado <- system2(ruta_ytdlp, args = c("--dump-json", "--no-playlist", url), stdout = TRUE)

# Dentro de tu función, antes de crear el dataframe:
descripcion_limpia <- replace_emoji(datos$description)

# Luego en el dataframe usas:
descripcion = descripcion_limpia

### 2. Comando generado con IA

extraer_meta_video <- function(url) {
  # Ejecutamos yt-dlp pidiendo el JSON de metadatos sin descargar el video
  # --dump-json: devuelve la info en texto
  # --no-playlist: asegura que solo tome el video del link
  
  comando <- paste0("yt-dlp --dump-json --no-playlist ", url)
  
  tryCatch({
    # Capturamos la salida del sistema
    resultado_json <- system(comando, intern = TRUE)
    
    # Convertimos el JSON a una lista de R
    datos <- fromJSON(resultado_json)
    
    # Limpiamos la descripción convirtiendo emojis a texto descriptivo
    # Esto evita errores de encoding y permite búsquedas por palabras
    desc_procesada <- textclean::replace_emoji(datos$description)
    
    # Seleccionamos lo más relevante para etnografía
    df_video <- data.frame(
      plataforma = datos$extractor_key,
      usuario = datos$uploader,
      titulo = datos$title,
      fecha = as.Date(datos$upload_date, format = "%Y%m%d"),
      vistas = ifelse(is.null(datos$view_count), NA, datos$view_count),
      likes = ifelse(is.null(datos$like_count), NA, datos$like_count),
      descripcion_raw = datos$description,     # El emoji original (UTF-8)
      descripcion_txt = desc_procesada,         # El emoji como "[grinning_face]"
      descripcion = datos$description,
      url_original = url,
      stringsAsFactors = FALSE
    )
    
    return(df_video)
    
  }, error = function(e) {
    message("Error con el link: ", url)
    return(NULL)
  })
}

# Ejemplo de uso
link_tiktok <- "https://www.tiktok.com/@usuario/video/123456789"
mi_data_etno <- extraer_meta_video(link_tiktok)

print(mi_data_etno)






