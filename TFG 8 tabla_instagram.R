# TFG - Extraccion de datos de videos Instagram - a traves txt
# Mon Apr 27 15:41:12 2026 ------------------------------

library(tidyverse)
library(rvest)
library(stringr)
library(textclean)
library(jsonlite)
library(dplyr) # Para unir los datos fácilmente

analizar_instagram_source <- function(ruta_archivo) {
  # 1. Leer el archivo
  texto_plano <- readLines(ruta_archivo, warn = FALSE, encoding = "UTF-8")
  html_raw <- read_html(paste(texto_plano, collapse = "\n"))
  
  # 2. Extraer etiquetas Meta base
  meta_desc  <- html_raw %>% html_node(xpath = "//meta[@name='description']") %>% html_attr("content")
  meta_title <- html_raw %>% html_node(xpath = "//meta[@property='og:title']") %>% html_attr("content")
  meta_url   <- html_raw %>% html_node(xpath = "//meta[@property='og:url']") %>% html_attr("content")
  
  # 3. Intentar extraer VISTAS (video_view_count)
  # Buscamos en todo el texto del archivo la etiqueta "video_view_count"
  todo_el_texto <- paste(texto_plano, collapse = " ")
  vistas_raw <- str_extract(todo_el_texto, '(?<="video_view_count":)\\d+')
  vistas_val <- as.numeric(vistas_raw)
  
  # 4. Función para limpiar números (Likes/Comments)
  limpiar_num <- function(patron, texto) {
    raw <- str_extract(texto, paste0("[\\d\\.,KkMm]+(?=\\s", patron, ")"))
    if (is.na(raw)) return(0)
    num <- str_replace_all(raw, "[,\\.]", "")
    val <- as.numeric(str_extract(num, "[\\d\\.]+"))
    if (str_detect(num, "[Kk]")) val <- val * 1000
    if (str_detect(num, "[Mm]")) val <- val * 1000000
    return(val)
  }
  
  # 5. Extraer USUARIO
  usuario_val <- str_extract(meta_desc, "(?<=\\-\\s)[^\\s]+(?=\\s(el|on)\\s)")
  
  # 6. Extraer y convertir FECHA
  fecha_raw <- str_extract(meta_desc, "([A-Z][a-z]+\\s\\d{1,2},\\s\\d{4}|\\d{1,2}\\sde\\s[a-z]+\\sde\\s\\d{4})")
  meses_en <- c("January"="01","February"="02","March"="03","April"="04","May"="05","June"="06",
                "July"="07","August"="08","September"="09","October"="10","November"="11","December"="12")
  meses_es <- c("enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
                "julio"="07","agosto"="08","septiembre"="09","octubre"="10","noviembre"="11","diciembre"="12")
  
  fecha_final <- as.Date(NA)
  if(!is.na(fecha_raw)) {
    if(str_detect(fecha_raw, ",")) {
      mes <- str_extract(fecha_raw, "^[A-Za-z]+")
      dia <- str_extract(fecha_raw, "\\d{1,2}")
      anio <- str_extract(fecha_raw, "\\d{4}")
      fecha_final <- as.Date(paste(anio, meses_en[mes], dia, sep="-"))
    } else { 
      partes <- str_split(fecha_raw, " de ")[[1]]
      fecha_final <- as.Date(paste(partes[3], meses_es[partes[2]], partes[1], sep="-"))
    }
  }
  
  # 7. Limpiar TÍTULO
  titulo_limpio <- str_extract(meta_title, "(?<=\":\\s\").*(?=\")") 
  if(is.na(titulo_limpio)) titulo_limpio <- str_extract(meta_title, "(?<=:\\s\").*(?=\")")
  
  # 8. Crear Dataframe
  df_video <- data.frame(
    plataforma      = "Instagram",
    usuario         = usuario_val,
    titulo          = ifelse(is.na(titulo_limpio), meta_title, titulo_limpio),
    fecha           = fecha_final,
    vistas          = vistas_val, # Nuevo campo intentado
    likes           = limpiar_num("(likes|Me gusta)", meta_desc),
    comentarios     = limpiar_num("(comments|comentarios)", meta_desc),
    descripcion_raw = meta_desc,
    descripcion_txt = textclean::replace_emoji(meta_desc),
    url_original    = meta_url,
    stringsAsFactors = FALSE
  )
  
  return(df_video)
}

# Ejecución
#mi_df <- analizar_instagram_source("C:/Users/jshu2/OneDrive/Escritorio/TFG/vs_ig.txt")
#print(mi_df)


# 1. Asegúrate de tener la función 'analizar_instagram_source' cargada 
# (puedes usar la última versión que te pasé arriba)

# 2. Define la carpeta donde están todos tus archivos .txt
carpeta <- "C:/Users/jshu2/OneDrive/Escritorio/TFG/vs_instagram"

# 3. Listar todos los archivos .txt de esa carpeta
archivos <- list.files(path = carpeta, pattern = "\\.txt$", full.names = TRUE)

# 4. Procesar todos los archivos y unirlos en un solo dataframe
mi_df_total <- lapply(archivos, function(archivo) {
  
  # Usamos tryCatch por si un archivo está corrupto o no es de Instagram, 
  # para que el código no se detenga
  tryCatch({
    print(paste("Procesando:", basename(archivo)))
    analizar_instagram_source(archivo)
  }, error = function(e) {
    message(paste("Error en el archivo", archivo, ":", e$message))
    return(NULL) # Si hay error, devuelve vacío
  })
  
}) %>% bind_rows() # Une todos los resultados en un solo dataframe

# 5. Ver el resultado final
View(mi_df_total)

# Opcional: Guardar el resultado en un Excel o CSV
# write.csv(mi_df_total, "analisis_instagram_completo.csv", row.names = FALSE)
