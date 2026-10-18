# =============================================================================
# funciones_graficos.R
# Cada visualización es una función pura: recibe el data frame YA FILTRADO
# (más parámetros de control) y devuelve un gráfico/mapa. Así sirven tanto en
# Shiny como en scripts, informes Rmd/Quarto, etc.
#
# Convenciones:
#   - ggplot2 -> devuelven un objeto ggplot (la app lo envuelve con ggplotly)
#   - radar, nube de palabras -> dibujan con gráficos base (usar en renderPlot)
#   - waffle -> devuelve un ggplot estático
#   - mapa -> devuelve un widget leaflet
# =============================================================================

# ---- Constantes y utilidades ------------------------------------------------
niveles_colores <- c("Bajo" = "#D7191C", "Básico" = "#FDAE61",
                     "Alto" = "#A6D96A", "Superior" = "#1A9641")

etiquetas_indicador <- c(calificacion  = "Calificación promedio (0-5)",
                         asistencia    = "Asistencia promedio (%)",
                         n_actividades = "Actividades promedio")

etiquetas_variable <- c(calificacion  = "Calificación (0-5)",
                        asistencia    = "Asistencia (%)",
                        n_actividades = "Número de actividades")

etiquetas_nivel <- c(asignatura = "Asignatura", institucion = "Institución",
                     departamento = "Departamento")

tema_app <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}


# ---- 1. KPIs y tablas resumen -----------------------------------------------
resumen_kpis <- function(df) {
  list(
    n_registros     = nrow(df),
    n_estudiantes   = dplyr::n_distinct(df$id_estudiante),
    n_instituciones = dplyr::n_distinct(df$institucion),
    media_calif     = mean(df$calificacion),
    media_asist     = mean(df$asistencia),
    pct_aprueba     = 100 * mean(df$aprueba),
    correlacion     = if (nrow(df) > 2) stats::cor(df$asistencia, df$calificacion) else NA_real_
  )
}

resumen_instituciones <- function(df) {
  df |>
    dplyr::group_by(institucion, departamento) |>
    dplyr::summarise(
      registros    = dplyr::n(),
      calificacion = round(mean(calificacion), 2),
      asistencia   = round(mean(asistencia), 1),
      actividades  = round(mean(n_actividades), 1),
      pct_aprueba  = round(100 * mean(aprueba), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(calificacion))
}

# Resumen por departamento, con las coordenadas de referencia para el mapa
resumen_departamentos <- function(df) {
  df |>
    dplyr::group_by(departamento) |>
    dplyr::summarise(
      departamento    = dplyr::first(departamento),
      n_instituciones = dplyr::n_distinct(institucion),
      registros       = dplyr::n(),
      calificacion    = round(mean(calificacion), 2),
      asistencia      = round(mean(asistencia), 1),
      actividades     = round(mean(n_actividades), 1),
      pct_aprueba     = round(100 * mean(aprueba), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(calificacion))
}

# Departamentos del dataset que no se pudieron ubicar (nombre no reconocido)
departamentos_sin_ubicacion <- function(df) {
  r <- resumen_departamentos(df)
  r$departamento[is.na(r$latitud)]
}


# ---- 2. Barras --------------------------------------------------------------
# por: "asignatura" | "institucion" | "departamento" | "municipio"
# indicador: "calificacion" | "asistencia" | "n_actividades"
grafico_barras <- function(df, por = "asignatura", indicador = "calificacion", top_n = 20) {
  resumen <- df |>
    dplyr::group_by(grupo = .data[[por]]) |>
    dplyr::summarise(valor = mean(.data[[indicador]], na.rm = TRUE),
                     n = dplyr::n(), .groups = "drop") |>
    dplyr::slice_max(valor, n = top_n, with_ties = FALSE)
  
  ggplot2::ggplot(resumen, ggplot2::aes(
    x = stats::reorder(grupo, valor), y = valor, fill = valor,
    text = paste0(grupo, "<br>Promedio: ", round(valor, 2), "<br>Registros: ", n))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(end = 0.9) +
    ggplot2::labs(title = paste(etiquetas_indicador[[indicador]], "por", tolower(etiquetas_nivel[[por]])),
                  x = NULL, y = etiquetas_indicador[[indicador]]) +
    tema_app()
}


# ---- 3. Histograma ----------------------------------------------------------
grafico_histograma <- function(df, variable = "calificacion", bins = 20) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(bins = bins, fill = "#2C7FB8", color = "white") +
    ggplot2::geom_vline(xintercept = mean(df[[variable]], na.rm = TRUE),
                        color = "red", linetype = "dashed") +
    ggplot2::labs(title = paste("Distribución de", tolower(etiquetas_variable[[variable]])),
                  subtitle = "Línea roja = media",
                  x = etiquetas_variable[[variable]], y = "Frecuencia") +
    tema_app()
}


# ---- 4. Dispersión ----------------------------------------------------------
# x: "asistencia" | "n_actividades"
# color_por: "ninguno" | "asignatura" | "departamento" | "institucion" | "municipio"
grafico_dispersion <- function(df, x = "asistencia", color_por = "ninguno", max_puntos = 4000) {
  muestra <- if (nrow(df) > max_puntos) dplyr::slice_sample(df, n = max_puntos) else df
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = calificacion))
  if (color_por == "ninguno") {
    p <- p +
      ggplot2::geom_point(data = muestra, alpha = 0.25, color = "#2C7FB8") +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE)
  } else {
    p <- p +
      ggplot2::geom_point(data = muestra, ggplot2::aes(color = .data[[color_por]]), alpha = 0.3) +
      ggplot2::geom_smooth(ggplot2::aes(color = .data[[color_por]]),
                           method = "lm", formula = y ~ x, se = FALSE) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::labs(color = etiquetas_nivel[[color_por]])
  }
  p + ggplot2::labs(title = paste("Calificación vs.", tolower(etiquetas_variable[[x]])),
                    x = etiquetas_variable[[x]], y = "Calificación (0-5)") +
    tema_app()
}


# ---- 5. Líneas --------------------------------------------------------------
# Sin columna de periodo no hay tendencia temporal. Este gráfico muestra cómo
# cambia la calificación promedio a medida que aumenta la asistencia (tramos de
# 10 puntos) o el número de actividades (8 tramos).
# x: "asistencia" | "n_actividades"
# agrupar_por: "ninguno" | "asignatura" | "departamento" | "institucion" | "municipio"
grafico_lineas <- function(df, x = "asistencia", agrupar_por = "ninguno", min_registros = 3) {
  ancho <- if (x == "asistencia") 10 else max(1, ceiling(diff(range(df$n_actividades)) / 8))
  d0 <- df |>
    dplyr::mutate(tramo = ggplot2::cut_width(.data[[x]], width = ancho, boundary = 0))
  
  if (agrupar_por == "ninguno") {
    d <- d0 |>
      dplyr::group_by(tramo) |>
      dplyr::summarise(valor = mean(calificacion), n = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(grupo = "Todos")
  } else {
    d <- d0 |>
      dplyr::group_by(tramo, grupo = .data[[agrupar_por]]) |>
      dplyr::summarise(valor = mean(calificacion), n = dplyr::n(), .groups = "drop")
  }
  d <- dplyr::filter(d, n >= min_registros)   # evita promedios de muy pocos casos
  
  p <- ggplot2::ggplot(d, ggplot2::aes(tramo, valor, color = grupo, group = grupo,
                                       text = paste0(grupo, "<br>Tramo: ", tramo,
                                                     "<br>Calificación: ", round(valor, 2),
                                                     "<br>Registros: ", n))) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(title = paste("Calificación promedio según", tolower(etiquetas_variable[[x]])),
                  x = etiquetas_variable[[x]], y = "Calificación promedio (0-5)", color = NULL) +
    tema_app() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  if (agrupar_por == "ninguno") p <- p + ggplot2::guides(color = "none")
  p
}


# ---- 6. Box plot ------------------------------------------------------------
# por: "asignatura" | "institucion" | "departamento" | "municipio" | "categoria_asistencia"
grafico_boxplot <- function(df, por = "asignatura", variable = "calificacion") {
  d <- df |> dplyr::mutate(grupo = .data[[por]], valor = .data[[variable]])
  nombre_por <- if (por == "categoria_asistencia") "categoría de asistencia" else tolower(etiquetas_nivel[[por]])
  ggplot2::ggplot(d, ggplot2::aes(stats::reorder(grupo, valor, stats::median), valor, fill = grupo)) +
    ggplot2::geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::labs(title = paste(etiquetas_variable[[variable]], "por", nombre_por),
                  x = NULL, y = etiquetas_variable[[variable]]) +
    tema_app()
}


# ---- 7. Mapa de calor institución x asignatura ------------------------------
# El color de cada etiqueta (blanco o negro) se calcula según la luminosidad
# del color de fondo de su celda, así siempre es legible.
grafico_heatmap <- function(df) {
  d <- df |>
    dplyr::group_by(institucion, asignatura) |>
    dplyr::summarise(media = mean(calificacion), .groups = "drop")
  
  # Paleta idéntica a la del gráfico (sin los extremos casi negro/casi blanco)
  rango <- range(d$media)
  if (diff(rango) == 0) rango <- rango + c(-0.5, 0.5)
  colores <- viridis::magma(256, direction = -1, begin = 0.1, end = 0.95)
  fondo   <- scales::col_numeric(colores, domain = rango)(d$media)
  rgb     <- grDevices::col2rgb(fondo)
  lum     <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
  d$color_texto <- ifelse(lum > 0.55, "#111111", "#FFFFFF")
  
  ggplot2::ggplot(d, ggplot2::aes(asignatura, institucion, fill = media,
                                  text = paste0(institucion, "<br>", asignatura,
                                                "<br>Promedio: ", round(media, 2)))) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", media), color = color_texto),
                       size = 3.4, fontface = "bold") +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1,
                                  begin = 0.1, end = 0.95, limits = rango) +
    ggplot2::labs(title = "Calificación promedio: institución x asignatura",
                  x = NULL, y = NULL, fill = "Promedio") +
    tema_app() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
                   panel.grid = ggplot2::element_blank())
}


# ---- 8. Radar (gráficos base; usar dentro de renderPlot) --------------------
grafico_radar <- function(df, max_inst = 4) {
  top <- df |> dplyr::count(institucion, sort = TRUE) |>
    dplyr::slice_head(n = max_inst) |> dplyr::pull(institucion)
  
  m <- df |>
    dplyr::filter(institucion %in% top) |>
    dplyr::group_by(institucion, asignatura) |>
    dplyr::summarise(media = mean(calificacion), .groups = "drop") |>
    tidyr::pivot_wider(names_from = asignatura, values_from = media) |>
    tibble::column_to_rownames("institucion")
  m[is.na(m)] <- 0   # asignatura sin datos en esa institución -> 0
  
  k <- ncol(m)
  radar_df <- as.data.frame(rbind(rep(5, k), rep(0, k), as.matrix(m)))
  colnames(radar_df) <- colnames(m)
  rownames(radar_df) <- c("max", "min", rownames(m))
  
  cols <- RColorBrewer::brewer.pal(max(3, nrow(m)), "Set1")[seq_len(nrow(m))]
  op <- graphics::par(mar = c(1, 1, 3, 1)); on.exit(graphics::par(op))
  fmsb::radarchart(radar_df, axistype = 1, pcol = cols,
                   pfcol = scales::alpha(cols, 0.2), plwd = 2, plty = 1,
                   cglcol = "grey70", cglty = 1, axislabcol = "grey30",
                   caxislabels = 0:5, vlcex = 0.9,
                   title = "Calificación promedio por asignatura")
  graphics::legend("topright", legend = rownames(m), col = cols, lty = 1, lwd = 2, bty = "n")
  invisible(NULL)
}


# ---- 9. Nube de palabras (gráficos base; usar dentro de renderPlot) ---------
nube_palabras <- function(df, max_palabras = 80) {
  hay_texto <- "texto_actividad" %in% names(df) && any(!is.na(df$texto_actividad))
  
  if (hay_texto) {
    corpus <- tm::VCorpus(tm::VectorSource(stats::na.omit(df$texto_actividad)))
    corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
    corpus <- tm::tm_map(corpus, tm::removePunctuation)
    corpus <- tm::tm_map(corpus, tm::removeNumbers)
    corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("spanish"))
    corpus <- tm::tm_map(corpus, tm::stripWhitespace)
    tdm <- tm::TermDocumentMatrix(corpus)
    freqs <- sort(slam::row_sums(tdm), decreasing = TRUE)
  } else {
    # Alternativa: asignaturas ponderadas por volumen de actividades
    freqs <- df |>
      dplyr::group_by(asignatura) |>
      dplyr::summarise(f = sum(n_actividades), .groups = "drop") |>
      tibble::deframe() |> sort(decreasing = TRUE)
  }
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
  wordcloud::wordcloud(names(freqs), freqs, max.words = max_palabras,
                       random.order = FALSE, scale = c(4, 0.8),
                       colors = RColorBrewer::brewer.pal(8, "Dark2"))
  invisible(NULL)
}


# ---- 10. Waffle -------------------------------------------------------------
# Hecho solo con ggplot2 (geom_tile), sin la librería 'waffle'.
# Cuadrícula de filas x columnas (10 x 10 = 100 cuadros, 1 cuadro = 1%).
# Los cuadros se reparten con el método del resto mayor para que sumen
# exactamente 100, y se llenan por columnas desde abajo a la izquierda.
grafico_waffle <- function(df, filas = 10, columnas = 10) {
  total <- filas * columnas
  
  conteo <- df |>
    dplyr::count(nivel_desempeno, .drop = FALSE) |>
    dplyr::mutate(pct = 100 * n / sum(n))
  
  cuotas <- conteo$pct / 100 * total
  cuadros <- floor(cuotas)
  faltan <- total - sum(cuadros)
  if (faltan > 0) {
    mayores <- order(cuotas - cuadros, decreasing = TRUE)[seq_len(faltan)]
    cuadros[mayores] <- cuadros[mayores] + 1
  }
  
  nivel <- factor(rep(as.character(conteo$nivel_desempeno), cuadros),
                  levels = names(niveles_colores))
  i <- seq_len(total) - 1
  cuadricula <- data.frame(columna = i %/% filas + 1,
                           fila    = i %%  filas + 1,
                           nivel   = nivel)
  
  etiquetas <- stats::setNames(
    paste0(conteo$nivel_desempeno, " (", sprintf("%.1f", conteo$pct), "%)"),
    as.character(conteo$nivel_desempeno))
  
  ggplot2::ggplot(cuadricula, ggplot2::aes(columna, fila, fill = nivel)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::scale_fill_manual(values = niveles_colores, breaks = names(etiquetas),
                               labels = etiquetas, drop = FALSE) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = "Niveles de desempeño",
                  subtitle = "1 cuadro = 1% de los registros", fill = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}


# ---- 11. Mapa por departamento ----------------------------------------------
# Un círculo por departamento, ubicado en su coordenada de referencia
# (R/departamentos_colombia.R). Color = indicador; tamaño = nº de registros.
# indicador: "calificacion" | "asistencia" | "actividades" | "pct_aprueba"
mapa_departamentos <- function(df, indicador = "calificacion") {
  dep <- resumen_departamentos(df) |> dplyr::filter(!is.na(latitud), !is.na(longitud))
  base <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(lng = -74.3, lat = 4.6, zoom = 5)
  if (nrow(dep) == 0) return(base)
  
  etiquetas <- c(calificacion = "Calificación promedio", asistencia = "Asistencia (%)",
                 actividades = "Actividades promedio", pct_aprueba = "% que aprueba")
  paleta <- if (indicador == "actividades") "Purples" else "RdYlGn"
  dominio <- range(dep[[indicador]])
  if (diff(dominio) == 0) dominio <- dominio + c(-0.5, 0.5)
  pal <- leaflet::colorNumeric(paleta, domain = dominio)
  
  base |>
    leaflet::addCircleMarkers(
      data = dep, lng = ~longitud, lat = ~latitud,
      radius = scales::rescale(dep$registros, to = c(9, 26), from = range(dep$registros) + c(0, 1e-9)),
      color = "#333333", weight = 1,
      fillColor = pal(dep[[indicador]]), fillOpacity = 0.85,
      label = ~paste0(departamento, ": ", dep[[indicador]]),
      popup = ~paste0("<b>", departamento, "</b>",
                      "<br>Instituciones: ", n_instituciones,
                      "<br>Calificación: ", calificacion,
                      "<br>Asistencia: ", asistencia, "%",
                      "<br>Actividades: ", actividades,
                      "<br>% aprueba: ", pct_aprueba, "%",
                      "<br>Registros: ", registros)
    ) |>
    leaflet::addLegend("bottomright", pal = pal, values = dominio,
                       title = etiquetas[[indicador]], opacity = 0.9)
}

guardar <- function(p, nombre, w = 9, h = 5.5) {
  ggsave(file.path("salidas/graficos", nombre), p, width = w, height = h, dpi = 300)
}
