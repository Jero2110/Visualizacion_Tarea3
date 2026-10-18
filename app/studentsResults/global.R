# global.R — Shiny lo ejecuta primero; luego carga automáticamente los
# archivos de la carpeta R/ y por último app.R.

paquetes <- c(
  "shiny", "bslib", "bsicons", "DT", "plotly", "leaflet",
  "dplyr", "tidyr", "tibble", "ggplot2", "readr", "stringr", "janitor",
  "scales", "viridis", "RColorBrewer",
  "fmsb", "wordcloud", "tm", "slam"
)

# Revisamos todos los paquetes que no se encuentran en la lista y los instalamos
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos)

# Con esta linea importamos las librerias en el entorno de trabajo
invisible(lapply(paquetes, library, character.only = TRUE))


if (length(nuevos) > 0) {
  stop("Faltan paquetes: ", paste(nuevos, collapse = ", "),
       "\nInstálalos con: install.packages(c(",
       paste0('"', nuevos, '"', collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
})
