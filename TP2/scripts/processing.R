library(stopwords)
library(tidyverse)
library(udpipe)
library(tidytext)
library(here)

# Crear el directorio /output si no existe
output_dir <- here("TP2/output")
if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir) #mensaje si se creo el direct
  dir.create(output_dir, recursive = TRUE)
}

# Cargar los datos scrapeados en el paso anterior para manejarlo sy procesarlos
message("Cargando datos de comunicados_oea.rds...") # se cargaron los datos :))
df_oea <- readRDS(here("TP2/data/comunicados_oea.rds"))

# Limpieza inicial del texto (pto a de processing)
# Pasamos a minúsculas y sacamos números y caracteres especiales antes de procesar
message("Realizando limpieza inicial de texto...") #msj de limpieza
df_oea <- df_oea %>%
  mutate(cuerpo_limpio = str_to_lower(cuerpo),
         # Sacamos números
         cuerpo_limpio = str_remove_all(cuerpo_limpio, "[0-9]+"),
         # Sacamos signos de puntuación y caracteres especiales
         cuerpo_limpio = str_remove_all(cuerpo_limpio, "[[:punct:]¿¡]"),
         # Sacamos espacios extra
         cuerpo_limpio = str_squish(cuerpo_limpio))

# Lematización y filtrado por tipo de palabra (pto b de processing)
message("Iniciando proceso de lematización (esto puede tardar unos segundos)...") #msj lematizador 

# Descargamos y cargamos el modelo de español si no lo tenemos
modelo_es <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_load_model(modelo_es$file_model)

# Anotamos el texto (lematización y etiquetado gramatical)
# Usamos doc_id para mantener la relación con el comunicado original
anotacion <- udpipe_annotate(ud_model, x = df_oea$cuerpo_limpio, doc_id = df_oea$id)
df_anotado <- as.data.frame(anotacion)

# Filtramosolo sustantivos (NOUN), verbos (VERB) y adjetivos (ADJ)
# Aseguramos minúsculas en el lemma (raíz de la palabra)
tokens_procesados <- df_anotado %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>% #aca filtramos
  select(doc_id, lemma) %>%
  mutate(lemma = str_to_lower(lemma)) %>% #aca forzamos minucula
  rename(id = doc_id, palabra = lemma)

# Remover Stopwords (pto c de processing)
message("Removiendo stopwords en español...") #msj de stopwords
# Obtenemos la lista de palabras vacías en español
palabras_vacias <- get_stopwords("es")

processed_text <- tokens_procesados %>%
  anti_join(palabras_vacias, by = c("palabra" = "word")) %>%
  # Limpieza extra filtramos palabras muy cortas que suelen ser ruidos del scraping
  filter(nchar(palabra) > 2) # esto serian cosas como la, le, de, etc

# Guardar el resultado final en /output
message("Guardando archivo de texto procesado...") #msj de guardado
saveRDS(processed_text, here("TP2/output/processed_text.rds"))

message("¡Proceso de limpieza y lematización completado con éxito!")#msj de que todo salio bien ;))