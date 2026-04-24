library(tidyverse) # lo uso para poder manipular datos y usar el pipe
library(rvest) # para el scraping y los html
library(here) #para usar las rutas relativas
library(xml2) #para guardar los archivos html


# creación del directorio /data
data_dir <- here("TP2/data") #configuro la ruta
if (!dir.exists(data_dir)) { #se crea la carpeta de data si no existe
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
}

# Configuración de la iteración
meses <- 1:4 # de enero a abril  
anio <- 2026 #en 2026 
lista_comunicados <- list() # lista vacia para que se complete en cada loop:)

message("Iniciando scraping automatizado de la OEA para el año ", anio)
#mensaje para saber si va funcionando el proceso 

for (mes in meses) { # se repíte para cada mes
  # Construimos la URL dinámica usando los parámetros de la consulta (Query Strings)
  url_indice <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", mes, "&nAnio=", anio)
  message(">>> Accediendo al índice del Mes: ", mes) #msj para ver mientras ejecuto el codigo 
  
  # Leemos la página del índice
  pagina_indice <- tryCatch(read_html(url_indice), error = function(e) return(NULL))
  if (is.null(pagina_indice)) next
  
  # Extraemos los links usando el selector .headlinelink (el que aparece seleccionando con selector gadget)
  links <- pagina_indice %>% 
    html_nodes(".headlinelink") %>% 
    html_attr("href") %>% 
    # Limpiamos los links para que sean direcciones completas
    paste0("https://www.oas.org/es/centro_noticias/", .) %>%
    unique()
  
  for (url_nota in links) {
    # CRAWL-DELAY de 3 segundos obligatorio que vi en robots.tx
    Sys.sleep(3)
    #saco y busco la iD de la noticia 
    # Buscamos el código que viene después de 'sCodigo='
    id_nota <- str_extract(url_nota, "(?<=sCodigo=)[^&]+")
    # Cambiamos la "/" por "_" para que Windows no de error al guardar el archivo
    id_nota <- str_replace(id_nota, "/", "_")
    
    # Si por alguna razón sigue siendo NA, le ponemos un nombre genérico para que se guarde asi 
    if (is.na(id_nota)) id_nota <- paste0("comunicado_", sample(1000:9999, 1))
    
    message("Descargando comunicado: ", id_nota) #msj para ver que vaya funcionando mientras lo ejecuto
    
    # Leemos la noticia individual con control de errores
    html_nota <- tryCatch(read_html(url_nota), error = function(e) return(NULL))
    if (is.null(html_nota)) next
    
    # Guardamos el HTML original en /data
    write_xml(html_nota, here("TP2/data", paste0(id_nota, ".html")))
    
    # Extraemos Titulo y Cuerpo
    # El título en h2 y el cuerpo en párrafos p ( lo hago todo junto9
    titulo <- html_nota %>% html_node("h2") %>% html_text(trim = TRUE)
    cuerpo <- html_nota %>% html_nodes("p") %>% html_text(trim = TRUE) %>% 
      paste(collapse = " ")
    
    # Guardamos en nuestra lista temporal
    lista_comunicados[[id_nota]] <- tibble(
      id = id_nota, #defino cada columna que necesito 
      titulo = titulo,
      cuerpo = cuerpo
    )
  }
}

# Consolidación y guardado en .rds
df_oea <- bind_rows(lista_comunicados)
saveRDS(df_oea, here("TP2/data/comunicados_oea.rds"))

message("¡Proceso terminado! Se guardó 'comunicados_oea.rds' en /data") #msj para ver que se termino el proceso y esta todo bien guardado!!!!! ;)
