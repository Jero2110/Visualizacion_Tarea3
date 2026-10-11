# =============================================================================
# ANÁLISIS DEL RENDIMIENTO ACADÉMICO EN INSTITUCIONES EDUCATIVAS DE COLOMBIA
# =============================================================================
# Este código busca realizar las gráficas necesarias para el proyecto

# Secciones:
#   0. Paquetes y configuración
#   1. Carga de datos depurados
#   2. Visualizaciones: barras, histogramas, dispersión, líneas
#   3. Visualizaciones por asignatura/institución: radar, nube de palabras,
#      waffle, box plots
#   4. Mapas y patrones geográficos
# =============================================================================

# ---- 0. PAQUETES Y CONFIGURACIÓN --------------------------------------------
# Inicialmente definimos los paquetes que estaremos utilizando para el análisis de los datos

paquetes <- c(
  "tidyverse", "janitor", "skimr", "naniar", "scales", "viridis", "lubridate",
  "fmsb",        # gráficos de radar
  "wordcloud", "RColorBrewer", "tm",   # nubes de palabras
  "leaflet", "sf", "spdep",            # mapas y autocorrelación espacial
  "plotly", "corrplot", "ggplot2"
)

# Revisamos todos los paquetes que no se encuentran en la lista y los instalamos
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]

if (length(nuevos) > 0) install.packages(nuevos)
invisible(lapply(paquetes, library, character.only = TRUE))

# Creamos las rutas con las salidas de los gráficos y de los datos procesados
dir.create("salidas/graficos", recursive = TRUE, showWarnings = FALSE)

tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Definimos una función para guardar los gráficos creados en la carpeta correspondiente
guardar <- function(p, nombre, w = 9, h = 5.5) {
  ggsave(file.path("salidas/graficos", nombre), p, width = w, height = h, dpi = 300)
}






# ---- 5. VISUALIZACIONES BÁSICAS ---------------------------------------------
# 5.1 Gráfico de barras: promedio de calificación por asignatura
p_barras <- ggplot(resumen_asignatura,
                   aes(x = reorder(asignatura, media_calif), y = media_calif, fill = media_calif)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(media_calif, 2)), hjust = -0.15, size = 3.5) +
  coord_flip(ylim = c(0, 5.2)) +
  scale_fill_viridis_c(option = "D", end = 0.9) +
  labs(title = "Calificación promedio por asignatura",
       x = NULL, y = "Calificación promedio (0-5)") + tema
guardar(p_barras, "02_barras_asignatura.png")

# 5.2 Barras: promedio por institución
p_barras_inst <- ggplot(resumen_institucion,
                        aes(x = reorder(institucion, media_calif), y = media_calif, fill = municipio)) +
  geom_col() + coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Calificación promedio por institución", x = NULL,
       y = "Calificación promedio (0-5)", fill = "Municipio") + tema
guardar(p_barras_inst, "03_barras_institucion.png", h = 7)

# 5.3 Histogramas
p_hist_calif <- ggplot(datos_clave, aes(calificacion_final)) +
  geom_histogram(binwidth = 0.2, fill = "#2C7FB8", color = "white") +
  geom_vline(aes(xintercept = mean(calificacion_final)), color = "red", linetype = "dashed") +
  labs(title = "Distribución de calificaciones",
       subtitle = "Línea roja = media", x = "Calificación", y = "Frecuencia") + tema
guardar(p_hist_calif, "04_hist_calificaciones.png")

p_hist_asist <- ggplot(datos_clave, aes(asistencias)) +
  geom_histogram(binwidth = 5, fill = "#41AB5D", color = "white") +
  geom_vline(aes(xintercept = mean(asistencias)), color = "red", linetype = "dashed") +
  labs(title = "Distribución de la asistencias",
       x = "Asistencia (%)", y = "Frecuencia") + tema
guardar(p_hist_asist, "05_hist_asistencia.png")

# 5.3b Barras: distribución por nivel de desempeño
p_nivel <- datos_clave %>% count(nivel_desempeno) %>%
  ggplot(aes(nivel_desempeno, n, fill = nivel_desempeno)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Estudiantes por nivel de desempeño", x = NULL, y = "Registros") + tema
guardar(p_nivel, "05b_nivel_desempeno.png")

# 5.4 Dispersión: asistencia vs. calificación (con regresión)
p_disp <- ggplot(datos_clave, aes(asistencias, calificacion_final)) +
  geom_point(alpha = 0.25, color = "#2C7FB8") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre asistencia y calificación",
       x = "Asistencia (%)", y = "Calificación (0-5)") + tema
guardar(p_disp, "06_dispersion_asistencia_calificacion.png")

# Dispersión: carga de actividades vs. calificación, por asignatura
p_disp2 <- ggplot(datos_clave, aes(numero_actividades, calificacion_final)) +
  geom_point(alpha = 0.2, color = "#6A51A3") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ asignatura) +
  labs(title = "Carga de actividades vs. calificación, por asignatura",
       x = "Número de actividades", y = "Calificación") + tema
guardar(p_disp2, "07_dispersion_actividades.png", w = 11, h = 7)

modelo <- lm(calificacion_final ~ asistencias + numero_actividades, data = datos_clave)
cat("\n==== REGRESIÓN LINEAL MÚLTIPLE ====\n"); print(summary(modelo))

# 5.5 Líneas: tendencia por periodo
tendencia <- datos_clave %>%
  group_by(periodo) %>%
  summarise(calificacion_final = mean(calificacion_final), asistencias = mean(asistencias),
            .groups = "drop") %>%
  pivot_longer(c(calificacion_final, asistencias), names_to = "indicador", values_to = "valor")

p_lineas <- ggplot(tendencia, aes(periodo, valor, group = 1)) +
  geom_line(color = "#08519C", linewidth = 1) + geom_point(size = 2.5, color = "#08519C") +
  facet_wrap(~ indicador, scales = "free_y",
             labeller = as_labeller(c(calificacion_final = "Calificación promedio (0-5)",
                                      asistencias = "Asistencia promedio (%)"))) +
  labs(title = "Tendencia por periodo académico", x = "Periodo", y = NULL) + tema
guardar(p_lineas, "08_lineas_tendencia.png", w = 11)

# Líneas: tendencia por asignatura
p_lineas_asig <- datos_clave %>%
  group_by(periodo, asignatura) %>%
  summarise(calificacion_final = mean(calificacion_final), .groups = "drop") %>%
  ggplot(aes(periodo, calificacion_final, color = asignatura, group = asignatura)) +
  geom_line(linewidth = 0.9) + geom_point() +
  scale_color_viridis_d() +
  labs(title = "Evolución de la calificación por asignatura",
       x = "Periodo", y = "Calificación promedio", color = "Asignatura") + tema
guardar(p_lineas_asig, "09_lineas_asignatura.png", w = 10)


# ---- 6. PATRONES POR ASIGNATURA E INSTITUCIÓN -------------------------------
# 6.1 Gráfico de radar: perfil de asignaturas de las instituciones (top 4)
top_inst <- resumen_institucion %>% slice_max(n, n = 4) %>% pull(institucion)

radar_df <- datos_clave %>%
  filter(institucion %in% top_inst) %>%
  group_by(institucion, asignatura) %>%
  summarise(media = mean(calificacion_final), .groups = "drop") %>%
  pivot_wider(names_from = asignatura, values_from = media) %>%
  column_to_rownames("institucion")

radar_plot <- rbind(rep(5, ncol(radar_df)), rep(0, ncol(radar_df)), radar_df)
colores <- brewer.pal(max(3, nrow(radar_df)), "Set1")[seq_len(nrow(radar_df))]

png("salidas/graficos/10_radar.png", width = 2000, height = 1800, res = 250)
radarchart(radar_plot, axistype = 1, pcol = colores,
           pfcol = scales::alpha(colores, 0.2), plwd = 2, plty = 1,
           cglcol = "grey70", cglty = 1, axislabcol = "grey30",
           caxislabels = 0:5, vlcex = 0.85,
           title = "Calificación promedio por asignatura e institución")
legend("topright", legend = rownames(radar_df), col = colores, lty = 1, lwd = 2, bty = "n")
dev.off()

# 6.2 Nube de palabras
# Opción A: texto de actividades (columna descripcion_actividad en datos_raw)
if ("descripcion_actividad" %in% names(datos_raw)) {
  textos <- datos_raw$descripcion_actividad
  corpus <- VCorpus(VectorSource(textos)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("spanish")) %>%
    tm_map(stripWhitespace)
  tdm   <- TermDocumentMatrix(corpus)
  freqs <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
} else {
  # Opción B (alternativa): ponderar asignaturas por volumen de actividades
  freqs <- datos_clave %>%
    group_by(asignatura) %>% summarise(f = sum(numero_actividades), .groups = "drop") %>%
    deframe() %>% sort(decreasing = TRUE)
}
set.seed(123)
png("salidas/graficos/11_nube_palabras.png", width = 1800, height = 1400, res = 250)
wordcloud(names(freqs), freqs, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"), scale = c(4, 0.7))
dev.off()

# 6.3 Waffle: composición por nivel de desempeño
conteo_nivel <- datos_clave %>% count(nivel_desempeno) %>%
  mutate(prop = round(100 * n / sum(n)))
vec_waffle <- setNames(conteo_nivel$prop, conteo_nivel$nivel_desempeno)

p_waffle <- waffle(vec_waffle, rows = 10, size = 0.5,
                   colors = c("#D7191C", "#FDAE61", "#A6D96A", "#1A9641")[seq_along(vec_waffle)],
                   title = "Niveles de desempeño (1 cuadro = 1% de los registros)",
                   legend_pos = "bottom")
ggsave("salidas/graficos/12_waffle_desempeno.png", p_waffle, width = 8, height = 6, dpi = 300)

# 6.4 Box plots
p_box_asig <- ggplot(datos_clave, aes(reorder(asignatura, calificacion_final, median),
                                      calificacion_final, fill = asignatura)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  coord_flip() + scale_fill_viridis_d(option = "C") +
  labs(title = "Distribución de calificaciones por asignatura",
       x = NULL, y = "Calificación") + tema
guardar(p_box_asig, "13_box_asignatura.png")

p_box_inst <- ggplot(datos_clave, aes(reorder(institucion, calificacion_final, median),
                                      calificacion_final, fill = municipio)) +
  geom_boxplot(outlier.alpha = 0.3) + coord_flip() + scale_fill_viridis_d() +
  labs(title = "Distribución de calificaciones por institución",
       x = NULL, y = "Calificación", fill = "Municipio") + tema
guardar(p_box_inst, "14_box_institucion.png", h = 7)

p_box_asist <- ggplot(datos_clave, aes(categoria_asistencia, calificacion_final, fill = categoria_asistencia)) +
  geom_boxplot(show.legend = FALSE) + scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Calificación según categoría de asistencia",
       x = "Categoría de asistencia", y = "Calificación") + tema
guardar(p_box_asist, "15_box_asistencia.png")

# Mapa de calor asignatura x institución
p_heat <- datos_clave %>%
  group_by(institucion, asignatura) %>% summarise(media = mean(calificacion_final), .groups = "drop") %>%
  ggplot(aes(asignatura, institucion, fill = media)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(media, 1)), size = 3) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "Calificación promedio: institución x asignatura", x = NULL, y = NULL, fill = "Promedio") +
  tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(p_heat, "16_heatmap_inst_asig.png", w = 10, h = 7)


# ---- 7. MAPAS Y PATRONES GEOGRÁFICOS ----------------------------------------
inst_geo <- resumen_institucion %>% filter(!is.na(lat), !is.na(lon))

pal <- colorNumeric("RdYlGn", domain = inst_geo$media_calif)

mapa <- leaflet(inst_geo) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = ~rescale(n, to = c(6, 18)),
    color = ~pal(media_calif), fillOpacity = 0.85, stroke = TRUE, weight = 1,
    popup = ~paste0("<b>", institucion, "</b><br>",
                    "Municipio: ", municipio, "<br>",
                    "Calificación promedio: ", round(media_calif, 2), "<br>",
                    "Asistencia promedio: ", round(media_asist, 1), "%<br>",
                    "% aprueba: ", round(pct_aprueba, 1), "%<br>",
                    "Registros: ", n)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~media_calif,
            title = "Calificación<br>promedio", opacity = 0.9)
htmlwidgets::saveWidget(mapa, "salidas/mapa_calificaciones.html", selfcontained = TRUE)

# Mapa de asistencia
pal_a <- colorNumeric("YlGnBu", domain = inst_geo$media_asist)
mapa_asist <- leaflet(inst_geo) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = 9,
                   color = ~pal_a(media_asist), fillOpacity = 0.85,
                   popup = ~paste0("<b>", institucion, "</b><br>Asistencia: ",
                                   round(media_asist, 1), "%")) %>%
  addLegend("bottomright", pal = pal_a, values = ~media_asist, title = "Asistencia (%)")
htmlwidgets::saveWidget(mapa_asist, "salidas/mapa_asistencia.html", selfcontained = TRUE)

# Patrones geográficos: resumen por municipio
resumen_municipio <- datos_clave %>%
  group_by(municipio) %>%
  summarise(n_inst = n_distinct(institucion), media_calif = mean(calificacion_final),
            media_asist = mean(asistencias), pct_aprueba = mean(aprueba) * 100,
            .groups = "drop") %>%
  arrange(desc(media_calif))
print(resumen_municipio)

p_mun <- ggplot(resumen_municipio, aes(reorder(municipio, media_calif), media_calif, fill = media_asist)) +
  geom_col() + coord_flip() + scale_fill_viridis_c(option = "G", direction = -1) +
  labs(title = "Calificación promedio por municipio",
       x = NULL, y = "Calificación promedio", fill = "Asistencia (%)") + tema
guardar(p_mun, "17_barras_municipio.png")

# Autocorrelación espacial (índice de Moran, vecinos más cercanos)
if (nrow(inst_geo) >= 8) {
  pts   <- st_as_sf(inst_geo, coords = c("lon", "lat"), crs = 4326)
  k     <- min(4, nrow(inst_geo) - 1)
  nb    <- knn2nb(knearneigh(st_coordinates(pts), k = k))
  lw    <- nb2listw(nb, style = "W")
  moran <- moran.test(inst_geo$media_calif, lw)
  cat("\n==== ÍNDICE DE MORAN (calificación promedio) ====\n"); print(moran)
  # I > 0 y p < 0.05: instituciones cercanas tienden a tener desempeños similares
}

