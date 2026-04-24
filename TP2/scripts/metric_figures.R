library(tidyverse)
library(tidytext)
library(here)

#Mensajes informativos y carga de datos
message("Iniciando generación de métricas y figuras...")
tokens <- readRDS(here("TP2/output/processed_text.rds")) #subo lo procesado y lo etiqueto como tokens

#Computar Frecuencia de Términos (Equivalente a DTM en formato tidy)
# Contamos cuántas veces aparece cada palabra en todo el corpus
frecuencia_total <- tokens %>%
  count(palabra, sort = TRUE)

#Selección de 5 términos relevantes para la OEA
# Basándonos en los comunicados de 2026, estos términos son centrales:
terminos_oea <- c("democracia", "misión", "electoral", "haití", "derecho")

df_final <- frecuencia_total %>%
  filter(palabra %in% terminos_oea)

# Generación del gráfico con ggplot2
message("Generando gráfico de barras...")

grafico <- ggplot(df_final, aes(x = reorder(palabra, n), y = n, fill = palabra)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  # Agregamos etiquetas de datos arriba de las barras
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  coord_flip() + # Barras horizontales para mejor lectura
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Frecuencia de términos institucionales clave",
    subtitle = "Comunicados de Prensa OEA (Enero - Abril 2026)",
    x = "Términos seleccionados",
    y = "Cantidad de menciones totales",
    caption = "Fuente: Scraping automatizado de OAS.org - TP2 Martina Boba Fernandez"
  ) +
  # Ajustamos los límites para que no se corten los números
  expand_limits(y = max(df_final$n) * 1.1)

#Guardar la figura en /output
output_file <- here("TP2/output/frecuencia_terminos.png")
message("Guardando figura en: ", output_file) #msj de que se guardo la foto

ggsave(
  filename = output_file,
  plot = grafico,
  width = 10,
  height = 6,
  dpi = 300
)

message("¡Proceso finalizado con éxito!") #msj de que se ejecuto completo:)