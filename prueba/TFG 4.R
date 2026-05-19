# TFG - Extranccion de datos de videos
# Sat Apr  4 15:39:26 2026 ------------------------------


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


#mi_data_etno <- extraer_meta_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215?_r=1&_t=ZN-93kfmnSx9RW", ruta_ytdlp)
#View(mi_data_etno) # Esto te abrirá la tabla en una pestaña nueva



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

#mis_comentarios <- extraer_comentarios_video("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp)


extraer_comentarios_con_archivo <- function(url, path_programa, path_cookies) {
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Usamos --cookies para apuntar al archivo que descargaste
  comando <- paste(shQuote(path_programa), 
                   "--dump-json --no-playlist --get-comments",
                   "--cookies", shQuote(path_cookies),
                   shQuote(url_limpia))
  
  tryCatch({
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    datos <- fromJSON(resultado_completo)
    
    comentarios_raw <- datos$comments
    
    if (is.null(comentarios_raw)) return(message("Aún no hay acceso a comentarios."))
    
    df <- data.frame(
      autor = comentarios_raw$author,
      texto = textclean::replace_emoji(comentarios_raw$text),
      likes = comentarios_raw$like_count,
      stringsAsFactors = FALSE
    )
    return(df)
  }, error = function(e) { message("Error: ", e$message); return(NULL) })
}

# USO:
ruta_cookies <- "C:/ordenador/usal/soc 4to usal/data science/tiktok.com_cookies.txt"
mis_comentarios <- extraer_comentarios_con_archivo(link_tiktok, ruta_ytdlp, ruta_cookies)


extraer_comentarios_edge <- function(url, path_programa) {
  
  # 1. Limpieza de URL
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # 2. Comando usando Edge y User-Agent de Windows
  comando <- paste(
    shQuote(path_programa), 
    "--dump-json",
    "--no-playlist",
    "--get-comments",
    "--cookies-from-browser edge",
    shQuote(url_limpia)
  )
  
  tryCatch({
    # Capturamos la salida del sistema
    resultado_vector <- system(comando, intern = TRUE)
    
    # Unimos las líneas en un solo texto JSON
    resultado_completo <- paste(resultado_vector, collapse = "") 
    
    # Convertimos a lista
    datos <- fromJSON(resultado_completo)
    
    # Extraemos la tabla de comentarios
    comentarios_raw <- datos$comments
    
    if (is.null(comentarios_raw) || length(comentarios_raw) == 0) {
      message("Aviso: No se devolvieron comentarios. TikTok podría estar bloqueando el acceso temporalmente.")
      return(NULL)
    }
    
    # Creamos el dataframe final para tu análisis
    df_comentarios <- data.frame(
      autor = comentarios_raw$author,
      texto_original = comentarios_raw$text,
      texto_limpio = textclean::replace_emoji(comentarios_raw$text), # Emojis a texto
      likes_en_comentario = comentarios_raw$like_count,
      fecha_comentario = as.Date(as.POSIXct(comentarios_raw$timestamp, origin="1970-01-01")),
      stringsAsFactors = FALSE
    )
    
    return(df_comentarios)
    
  }, error = function(e) {
    message("Error en la extracción: ", e$message)
    return(NULL)
  })
}

# --- EJECUCIÓN ---
# Asegúrate de tener Edge cerrado antes de correr esto
mis_comentarios <- extraer_comentarios_edge("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp)

# Ver resultados
if(!is.null(mis_comentarios)) View(mis_comentarios)

extraer_comentarios_con_archivo <- function(url, path_programa, path_cookies) {
  url_limpia <- strsplit(url, "\\?")[[1]][1]
  
  # Usamos --cookies para apuntar directamente al archivo de texto
  comando <- paste(shQuote(path_programa), 
                   "--dump-json --no-playlist --get-comments",
                   "--cookies", shQuote(path_cookies),
                   shQuote(url_limpia))
  
  tryCatch({
    resultado_vector <- system(comando, intern = TRUE)
    resultado_completo <- paste(resultado_vector, collapse = "") 
    datos <- fromJSON(resultado_completo)
    
    comentarios_raw <- datos$comments
    
    if (is.null(comentarios_raw)) {
      message("Acceso concedido pero no se encontraron comentarios.")
      return(NULL)
    }
    
    df <- data.frame(
      autor = comentarios_raw$author,
      texto = textclean::replace_emoji(comentarios_raw$text),
      likes = comentarios_raw$like_count,
      fecha = as.Date(as.POSIXct(comentarios_raw$timestamp, origin="1970-01-01")),
      stringsAsFactors = FALSE
    )
    return(df)
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# --- EJECUCIÓN ---
ruta_cookies <- "C:/ordenador/usal/soc 4to usal/data science/tiktok.com_cookies.txt"
mis_comentarios <- extraer_comentarios_con_archivo("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp, ruta_cookies)

if(!is.null(mis_comentarios)) View(mis_comentarios)

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

mis_comentarios <- extraer_comentarios_con_archivo("https://www.tiktok.com/@eirapsicologa/video/7582288882058792215", ruta_ytdlp, ruta_cookies)

library(textclean)

# 1. Leer el archivo que pegaste manualmente
ruta_txt <- "C:/ordenador/usal/soc 4to usal/data science/comentarios_raw.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8")

# 2. Filtrar líneas vacías
lineas <- lineas[lineas != ""]

# 3. Estructurar (Aproximación etnográfica)
# TikTok suele pegar: Nombre, Texto, Fecha/Likes en líneas seguidas.
# Este código agrupa el texto para que puedas analizarlo.
df_comentarios_manual <- data.frame(
  texto_original = lineas,
  texto_limpio = textclean::replace_emoji(lineas),
  stringsAsFactors = FALSE
)

# 4. (Opcional) Eliminar filas que solo son números (likes) o nombres de usuario cortos
# Esto limpia el ruido del "copia y pega"
df_comentarios_manual <- df_comentarios_manual[nchar(df_comentarios_manual$texto_original) > 5, ]

View(df_comentarios_manual)

library(tidyverse) # O solo usa R base si prefieres

# 1. Leer el archivo
ruta_txt <- "C:/ordenador/usal/soc 4to usal/data science/comentarios_raw.txt"
lineas <- readLines(ruta_txt, encoding = "UTF-8")

# 2. Limpieza inicial: Quitar líneas vacías y espacios extra
lineas <- trimws(lineas)
lineas <- lineas[lineas != ""]

# 3. Lógica de agrupación
# En TikTok, el patrón suele ser: 
# Línea 1: Nombre de usuario (Autor)
# Línea 2: El comentario (Texto)
# Línea 3: Información extra (Responder, Likes, etc.) -> Esta la saltaremos

autores <- c()
comentarios <- c()

# Recorremos las líneas de 3 en 3 (o buscando el patrón)
# Ajustamos i + 1 para asegurar que el comentario existe
for (i in seq(1, length(lineas), by = 3)) {
  if (!is.na(lineas[i+1])) {
    autores <- c(autores, lineas[i])
    comentarios <- c(comentarios, lineas[i+1])
  }
}

# 4. Crear el Data Frame final
df_etno_final <- data.frame(
  autor = autores,
  comentario_raw = comentarios,
  stringsAsFactors = FALSE
)

# 5. Limpiar emojis para el análisis de texto
library(textclean)
df_etno_final$comentario_analisis <- textclean::replace_emoji(df_etno_final$comentario_raw)

# Ver el resultado
View(df_etno_final)

library(tidyverse)
library(textclean)

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

#¿fin?
# Sat Apr  4 16:27:39 2026 ------------------------------

install.packages("wordcloud")
install.packages("tm")
library(wordcloud)
library(tm)

# Crear un "corpus" de texto
corpus <- Corpus(VectorSource(df_etno_tdah$comentario_original))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
# Quitar palabras comunes (artículos, preposiciones)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

# Generar la nube
wordcloud(corpus, max.words = 50, colors = brewer.pal(8, "Dark2"))
