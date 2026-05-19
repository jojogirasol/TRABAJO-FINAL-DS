# TFG - Extraccion de datos de videos Tiktok
# Creación:
# Tue Apr  7 11:39:42 2026 ------------------------------
# Aplicación final:
# Mon May 18 16:16:52 2026 ------------------------------

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



# Crea una lista con todos tus links
mis_links <- c(
  "https://www.tiktok.com/@dr.kojosarfo/video/7572065013842693406?_r=1&_t=ZN-93mIQR8TaaM",
  "https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW",
  "https://www.tiktok.com/@lifeactuator/video/7583115736814472479?_r=1&_t=ZN-93mIaJLaZRm",
  "https://www.tiktok.com/@nosolopastillas/video/7586784395470720278?_r=1&_t=ZN-93kfcpz9JNG",
  "https://www.tiktok.com/@dr.kojosarfo/video/7592020130427571487?_r=1&_t=ZN-93mIjfrovcU",
  "https://www.tiktok.com/@estercuni/video/7592711415249505558?_r=1&_t=ZN-93mJGP1w1EE",
  "https://www.tiktok.com/@fio.tdah/video/7593534419500076321?_r=1&_t=ZN-93mIg3w0CsI",
  "https://www.tiktok.com/@maritarx/video/7594504624946629910?_r=1&_t=ZN-93mILDFdWPT",
  "https://www.tiktok.com/@psicofersaras/video/7599249649643343126?_r=1&_t=ZN-93kg1X3Bn1S",
  "https://www.tiktok.com/@psicofersaras/video/7599635364126461206?_r=1&_t=ZN-93ipXqbVyAY",
  "https://www.tiktok.com/@izatia.psikologia/video/7599601530160958742?_r=1&_t=ZN-93n9MD6HPEM",
  "https://www.tiktok.com/@eirapsicologa/video/7601905797353852182?_r=1&_t=ZN-93kgAiCbfOA",
  "https://www.tiktok.com/@javipicornell/video/7602220764888173846?_r=1&_t=ZN-93mHwOTqM2I",
  "https://www.tiktok.com/@magdisorta/video/7603131641816485127?_r=1&_t=ZN-93nD7VmSiBy",
  "https://www.tiktok.com/@javipicornell/video/7603776588454612246?_r=1&_t=ZN-93mHs9gNyxK",
  "https://www.tiktok.com/@maritarx/video/7216428553708014853?_r=1&_t=ZN-966nMrxeNnC",
  "https://www.tiktok.com/@hiperlogico/video/7561510036485180694?_r=1&_t=ZN-966nPremHRL",
  "https://www.tiktok.com/@whisker.pedia/video/7515126012196228374?_r=1&_t=ZN-966nS7lELZe",
  "https://www.tiktok.com/@diplomaduck/video/7512004248188554518?_r=1&_t=ZN-966nTJK9J1q",
  "https://www.tiktok.com/@maritarx/video/7216428553708014853?_r=1&_t=ZN-966nVEMKKEY",
  "https://www.tiktok.com/@psicologiarcoiris/video/7481781711538261303?_r=1&_t=ZN-966nWE82rfa",
  "https://www.tiktok.com/@shhenia/video/7522525439118773526?_r=1&_t=ZN-966nXVpOZgE",
  "https://www.tiktok.com/@michicientifico_/video/7518061801943092502?_r=1&_t=ZN-966nYuBQMac"
)

extraer_meta_video <- function(url, path_programa) {
  
  # Limpiamos la URL de forma genérica (separa por el signo ?)
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  comando <- paste(shQuote(path_programa), "--dump-json --no-playlist", shQuote(url_limpia))
  
  tryCatch({
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    datos <- fromJSON(resultado_completo)
    
    desc_original <- ifelse(is.null(datos$description), "", datos$description)
    desc_procesada <- textclean::replace_emoji(desc_original)
    
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
    
    # Pausa de 2 segundos para evitar que TikTok nos bloquee por ir muy rápido
    Sys.sleep(2) 
    
    return(df_video)
    
  }, error = function(e) {
    message("Error procesando el video: ", url)
    return(NULL)
  })
}


# Aplicar la función a cada link y unir los resultados en una sola tabla
tabla_videos_completa <- mis_links %>% 
  map_df(~extraer_meta_video(.x, ruta_ytdlp))

# Ver el resultado final con todos los videos
View(tabla_videos_completa)
