# =============================================================================
# departamentos_colombia.R
# Como el dataset no trae latitud/longitud, los puntos del mapa se ubican en una
# coordenada de referencia por departamento (su capital, o un punto central en
# el caso de Cundinamarca). Son coordenadas aproximadas y fijas: NO son datos
# de las instituciones, solo sirven para dibujar cada departamento en el mapa.
# =============================================================================

# Normaliza nombres para poder cruzar "Bogotá D.C.", "BOGOTA, D.C.", "Valle", etc.
normalizar_departamento <- function(x) {
  x <- chartr("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNaeiouun", x)
  x <- toupper(x)
  x <- gsub("[.,]", "", x)
  x <- stringr::str_squish(x)
  alias <- c(
    "BOGOTA DC" = "BOGOTA", "BOGOTA D C" = "BOGOTA", "SANTAFE DE BOGOTA" = "BOGOTA",
    "SANTAFE DE BOGOTA DC" = "BOGOTA", "BOGOTA DISTRITO CAPITAL" = "BOGOTA",
    "DISTRITO CAPITAL" = "BOGOTA",
    "VALLE" = "VALLE DEL CAUCA", "GUAJIRA" = "LA GUAJIRA",
    "SAN ANDRES" = "SAN ANDRES Y PROVIDENCIA",
    "SAN ANDRES PROVIDENCIA Y SANTA CATALINA" = "SAN ANDRES Y PROVIDENCIA",
    "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA" = "SAN ANDRES Y PROVIDENCIA",
    "NORTE SANTANDER" = "NORTE DE SANTANDER", "N DE SANTANDER" = "NORTE DE SANTANDER"
  )
  ifelse(x %in% names(alias), unname(alias[x]), x)
}

coordenadas_departamentos <- function() {
  tibble::tibble(
    departamento = c(
      "Amazonas", "Antioquia", "Arauca", "Atlántico", "Bogotá D.C.", "Bolívar",
      "Boyacá", "Caldas", "Caquetá", "Casanare", "Cauca", "Cesar", "Chocó",
      "Córdoba", "Cundinamarca", "Guainía", "Guaviare", "Huila", "La Guajira",
      "Magdalena", "Meta", "Nariño", "Norte de Santander", "Putumayo", "Quindío",
      "Risaralda", "San Andrés y Providencia", "Santander", "Sucre", "Tolima",
      "Valle del Cauca", "Vaupés", "Vichada"),
    lat = c(-4.215, 6.244, 7.090, 10.968, 4.711, 10.391,
            5.535, 5.070, 1.614, 5.337, 2.444, 10.464, 5.692,
            8.749, 5.030, 3.865, 2.572, 2.927, 11.544,
            11.241, 4.142, 1.214, 7.894, 1.149, 4.534,
            4.814, 12.584, 7.119, 9.305, 4.438,
            3.452, 1.253, 6.189),
    lon = c(-69.941, -75.581, -70.761, -74.781, -74.072, -75.479,
            -73.367, -75.517, -75.606, -72.396, -76.614, -73.253, -76.658,
            -75.881, -74.030, -67.924, -72.640, -75.282, -72.907,
            -74.199, -73.627, -77.281, -72.508, -76.647, -75.681,
            -75.696, -81.700, -73.123, -75.398, -75.232,
            -76.532, -70.234, -67.486)
  ) |>
    dplyr::mutate(clave = normalizar_departamento(departamento))
}
