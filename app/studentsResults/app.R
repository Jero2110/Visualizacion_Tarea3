# =============================================================================
# app.R — Dashboard interactivo de rendimiento académico (Colombia)
# Ejecutar con:  shiny::runApp("dashboard_rendimiento")
# Las funciones de la carpeta R/ se cargan solas.
# =============================================================================

library(bslib)
library(shiny)
library(leaflet)

# ---- Datos (se leen una sola vez al iniciar la app) -------------------------
datos   <- cargar_datos(here::here("app", "studentsResults", "salidas", "datos", "datos_limpios.csv"))
es_demo <- isTRUE(attr(datos, "es_demo"))

print("--- VERIFICANDO COLUMNAS EN SHINY ---")
print(colnames(datos)) 
print("-------------------------------------")

departamentos <- sort(unique(datos$departamento))
instituciones <- sort(unique(datos$institucion))
asignaturas   <- sort(unique(datos$asignatura))

# ---- Opciones reutilizables en los selectores -------------------------------
op_nivel <- c("Asignatura" = "asignatura", "Institución" = "institucion",
              "departamento" = "departamento")

op_indicador <- c("Calificación" = "calificacion", "Asistencia" = "asistencia",
                  "Actividades" = "n_actividades")
op_agrupar   <- c("Sin agrupar" = "ninguno", op_nivel)
op_boxplot   <- c(op_nivel, "Categoría de asistencia" = "categoria_asistencia")

selector <- function(id, etiqueta, opciones, ...) {
  selectInput(id, etiqueta, choices = opciones, width = "100%", ...)
}
hay_datos <- function(d) {
  validate(need(nrow(d) > 0, "No hay registros con los filtros seleccionados. Amplía los filtros."))
}
caja <- function(titulo, ..., alto = NULL) {
  card(card_header(titulo), ..., full_screen = TRUE, height = alto)
}

# =============================================================================
# INTERFAZ
# =============================================================================
ui <- bslib::page_navbar(
  title = "Rendimiento Académico — Colombia",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  
  # ---- Filtros globales (afectan todas las pestañas) ----
  sidebar = sidebar(
    width = 330, title = "Filtros",
    selectizeInput("departamento", "departamento", choices = departamentos, multiple = TRUE,
                   options = list(placeholder = "Todos")),
    selectizeInput("institucion", "Institución", choices = instituciones, multiple = TRUE,
                   options = list(placeholder = "Todas")),
    selectizeInput("asignatura", "Asignatura", choices = asignaturas, multiple = TRUE,
                   options = list(placeholder = "Todas")),
    sliderInput("rango_calif", "Rango de calificación", min = 0, max = 5,
                value = c(0, 5), step = 0.1),
    sliderInput("rango_asist", "Rango de asistencia (%)", min = 0, max = 100,
                value = c(0, 100), step = 1),
    actionButton("reset", "Restablecer filtros", icon = icon("rotate-left"),
                 class = "btn-outline-primary w-100"),
    hr(),
    textOutput("n_filtrados")
  ),
  
  # ---- 1. Resumen ----
  nav_panel("Resumen", icon = bsicons::bs_icon("speedometer2"),
            if (es_demo) div(class = "alert alert-warning",
                             strong("Datos de demostración. "),
                             "Coloca tu archivo en datos/rendimiento_academico.csv y ajusta ",
                             "config_columnas en R/preparar_datos.R para usar tus datos reales."),
            layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
                           value_box("Calificación promedio", textOutput("kpi_calif"),
                                     showcase = bsicons::bs_icon("mortarboard"), theme = "primary"),
                           value_box("Asistencia promedio", textOutput("kpi_asist"),
                                     showcase = bsicons::bs_icon("calendar-check"), theme = "success"),
                           value_box("% que aprueba (nota ≥ 3.0 y asist. > 80%)", textOutput("kpi_aprueba"),
                                     showcase = bsicons::bs_icon("check2-circle"), theme = "info"),
                           value_box("Correlación asistencia-calificación", textOutput("kpi_cor"),
                                     showcase = bsicons::bs_icon("graph-up"), theme = "secondary")
            ),
            layout_columns(col_widths = c(7, 5),
                           caja("Promedios por grupo",
                                layout_columns(col_widths = c(6, 6),
                                               selector("barras_por", "Agrupar por", op_nivel),
                                               selector("barras_ind", "Indicador", op_indicador)),
                                plotlyOutput("p_barras", height = "400px")),
                           caja("Niveles de desempeño", plotOutput("p_waffle", height = "440px"))
            )
  ),
  
  # ---- 2. Distribuciones ----
  nav_panel("Distribuciones", icon = bsicons::bs_icon("bar-chart"),
            layout_columns(col_widths = c(6, 6),
                           caja("Histograma",
                                layout_columns(col_widths = c(6, 6),
                                               selector("hist_var", "Variable", op_indicador),
                                               sliderInput("hist_bins", "Número de barras", min = 5, max = 50, value = 20)),
                                plotlyOutput("p_hist", height = "380px")),
                           caja("Box plot",
                                layout_columns(col_widths = c(6, 6),
                                               selector("box_por", "Agrupar por", op_boxplot),
                                               selector("box_var", "Variable", op_indicador)),
                                plotlyOutput("p_box", height = "380px"))
            )
  ),
  
  # ---- 3. Relaciones ----
  nav_panel("Relaciones", icon = bsicons::bs_icon("graph-up-arrow"),
            layout_columns(col_widths = c(6, 6),
                           caja("Dispersión",
                                layout_columns(col_widths = c(6, 6),
                                               selector("disp_x", "Eje X", c("Asistencia" = "asistencia",
                                                                             "Actividades" = "n_actividades")),
                                               selector("disp_color", "Colorear por", c("Sin color" = "ninguno", op_nivel))),
                                plotlyOutput("p_disp", height = "380px")),
                           caja("Calificación promedio por tramos",
                                layout_columns(col_widths = c(6, 6),
                                               selector("lin_x", "Tramos de", c("Asistencia" = "asistencia",
                                                                                "Actividades" = "n_actividades")),
                                               selector("lin_grupo", "Agrupar por", op_agrupar)),
                                plotlyOutput("p_lineas", height = "380px"))
            ),
            caja("Modelo lineal: calificación ~ asistencia + actividades",
                 verbatimTextOutput("modelo"))
  ),
  
  # ---- 4. Asignaturas e instituciones ----
  nav_panel("Asignaturas e instituciones", icon = bsicons::bs_icon("diagram-3"),
            layout_columns(col_widths = c(6, 6),
                           caja("Radar por asignatura",
                                sliderInput("radar_n", "Instituciones a comparar (las de más registros)",
                                            min = 2, max = 8, value = 4, step = 1),
                                plotOutput("p_radar", height = "420px")),
                           caja("Nube de palabras", plotOutput("p_nube", height = "480px"))
            ),
            caja("Mapa de calor: institución x asignatura",
                 plotlyOutput("p_heat", height = "520px"))
  ),
  
  # ---- 5. Mapa ----
  nav_panel("Mapa", icon = bsicons::bs_icon("geo-alt"),
            uiOutput("aviso_mapa"),
            layout_columns(col_widths = c(8, 4),
                           caja("Desempeño por departamento",
                                selector("mapa_ind", "Colorear por",
                                         c("Calificación promedio" = "calificacion", "Asistencia promedio" = "asistencia",
                                           "Actividades promedio" = "actividades", "% que aprueba" = "pct_aprueba")),
                                leafletOutput("mapa", height = "560px"),
                                p(class = "text-muted small mt-2",
                                  "Cada círculo se ubica en la capital del departamento (o un punto de referencia); ",
                                  "no representa la ubicación exacta de las instituciones. Tamaño = número de registros.")),
                           caja("Ranking de departamentos", plotlyOutput("p_depto", height = "560px"))
            ),
            navset_card_tab(
              title = "Tablas de resumen",
              nav_panel("Departamentos", DT::DTOutput("tabla_depto")),
              nav_panel("Instituciones", DT::DTOutput("tabla_inst"))
            )
  ),
  
  # ---- 6. Datos ----
  nav_panel("Datos", icon = bsicons::bs_icon("table"),
            caja("Registros filtrados",
                 downloadButton("descargar", "Descargar CSV", class = "btn-primary mb-3"),
                 DT::DTOutput("tabla_datos"))
  ),
  
  # ---- 7. Guía ----
  nav_panel("Guía", icon = bsicons::bs_icon("info-circle"),
            card(card_body(markdown("
### Cómo usar el dashboard
- **Filtros (barra izquierda):** departamento, ciudad (si el dataset la incluye), institución, asignatura y rangos de calificación y asistencia. Dejar un filtro vacío equivale a *todos*. Al elegir departamentos, las listas de ciudad e institución se acotan automáticamente.
- **Resumen:** indicadores clave, promedios por grupo y composición por nivel de desempeño (Bajo < 3.0, Básico 3.0–3.9, Alto 4.0–4.5, Superior ≥ 4.6). *Aprueba* = calificación ≥ 3.0 **y** asistencia > 80%.
- **Distribuciones:** histogramas y box plots para ver dispersión y valores atípicos.
- **Relaciones:** cómo cambia la calificación con la asistencia o la carga de actividades (dispersión y promedios por tramos).
- **Asignaturas e instituciones:** radar, nube de palabras y mapa de calor para comparar perfiles.
- **Mapa:** un círculo por departamento coloreado por el indicador elegido. Haz clic para ver el detalle.
- **Datos:** tabla filtrada y descarga en CSV.

Los gráficos interactivos permiten pasar el cursor para ver valores, hacer zoom y exportar como imagen. El ícono de expandir (esquina de cada tarjeta) los muestra a pantalla completa.
")))
  )
)

# =============================================================================
# SERVIDOR
# =============================================================================
server <- function(input, output, session) {
  
  # ---- Filtros en cascada: ciudad e institución dependen del departamento ----
  observeEvent(input$departamento, {
    base <- datos
    if (length(input$departamento)) base <- filter(base, departamento %in% input$departamento)
    ops_inst <- sort(unique(base$institucion))
    updateSelectizeInput(session, "institucion", choices = ops_inst,
                         selected = intersect(input$institucion, ops_inst))
  }, ignoreNULL = FALSE)
  
  # ---- Restablecer ----
  observeEvent(input$reset, {
    updateSelectizeInput(session, "departamento", selected = character(0))
    updateSelectizeInput(session, "institucion", selected = character(0))
    updateSelectizeInput(session, "asignatura",  selected = character(0))
    updateSliderInput(session, "rango_calif", value = c(0, 5))
    updateSliderInput(session, "rango_asist", value = c(0, 100))
  })
  
  # ---- Datos filtrados (base de todas las salidas) ----
  datos_f <- reactive({
    d <- datos |>
      filter(between(calificacion, input$rango_calif[1], input$rango_calif[2]),
             between(asistencia,   input$rango_asist[1], input$rango_asist[2]))
    if (length(input$departamento)) d <- filter(d, departamento %in% input$departamento)
    if (length(input$institucion)) d <- filter(d, institucion %in% input$institucion)
    if (length(input$asignatura))  d <- filter(d, asignatura  %in% input$asignatura)
    d
  })
  
  kpis <- reactive({ req(nrow(datos_f()) > 0); resumen_kpis(datos_f()) })
  
  output$n_filtrados <- renderText({
    paste0(format(nrow(datos_f()), big.mark = "."), " de ",
           format(nrow(datos), big.mark = "."), " registros")
  })
  
  # ---- KPIs ----
  output$kpi_calif   <- renderText(sprintf("%.2f", kpis()$media_calif))
  output$kpi_asist   <- renderText(sprintf("%.1f %%", kpis()$media_asist))
  output$kpi_aprueba <- renderText(sprintf("%.1f %%", kpis()$pct_aprueba))
  output$kpi_cor     <- renderText(sprintf("%.2f", kpis()$correlacion))
  
  # ---- Resumen ----
  output$p_barras <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_barras(d, input$barras_por, input$barras_ind), tooltip = "text")
  })
  output$p_waffle <- renderPlot({
    d <- datos_f(); hay_datos(d)
    grafico_waffle(d)
  })
  
  # ---- Distribuciones ----
  output$p_hist <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_histograma(d, input$hist_var, input$hist_bins))
  })
  output$p_box <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_boxplot(d, input$box_por, input$box_var))
  })
  
  # ---- Relaciones ----
  output$p_disp <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_dispersion(d, input$disp_x, input$disp_color))
  })
  output$p_lineas <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_lineas(d, input$lin_x, input$lin_grupo), tooltip = "text")
  })
  output$modelo <- renderPrint({
    d <- datos_f()
    validate(need(nrow(d) >= 10, "Se necesitan al menos 10 registros para ajustar el modelo."))
    summary(lm(calificacion ~ asistencia + n_actividades, data = d))
  })
  
  # ---- Asignaturas e instituciones ----
  output$p_radar <- renderPlot({
    d <- datos_f(); hay_datos(d)
    validate(need(n_distinct(d$asignatura) >= 3,
                  "El radar necesita al menos 3 asignaturas en el filtro actual."))
    grafico_radar(d, input$radar_n)
  })
  output$p_nube <- renderPlot({
    d <- datos_f(); hay_datos(d)
    set.seed(123)
    nube_palabras(d)
  })
  output$p_heat <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ggplotly(grafico_heatmap(d), tooltip = "text")
  })
  
  # ---- Mapa y patrones geográficos (por departamento) ----
  output$aviso_mapa <- renderUI({
    d <- datos_f()
    if (nrow(d) == 0) return(NULL)
    sin <- departamentos_sin_ubicacion(d)
    if (length(sin) == 0) return(NULL)
    div(class = "alert alert-warning",
        strong("No se pudieron ubicar en el mapa: "), paste(sin, collapse = ", "),
        ". Revisa que el nombre del departamento esté bien escrito.")
  })
  output$mapa <- renderLeaflet({
    d <- datos_f(); hay_datos(d)
    mapa_departamentos(d, input$mapa_ind)
  })
  output$p_depto <- renderPlotly({
    d <- datos_f(); hay_datos(d)
    ind <- switch(input$mapa_ind, calificacion = "calificacion", asistencia = "asistencia",
                  actividades = "n_actividades", pct_aprueba = "calificacion")
    ggplotly(grafico_barras(d, "departamento", ind), tooltip = "text")
  })
  output$tabla_depto <- DT::renderDT({
    d <- datos_f(); hay_datos(d)
    DT::datatable(select(resumen_departamentos(d), -departamento, -latitud, -longitud), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tabla_inst <- DT::renderDT({
    d <- datos_f(); hay_datos(d)
    DT::datatable(resumen_instituciones(d), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Datos ----
  output$tabla_datos <- DT::renderDT({
    d <- datos_f(); hay_datos(d)
    DT::datatable(select(d, -departamento), rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  output$descargar <- downloadHandler(
    filename = function() paste0("datos_limpios_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(select(datos_f(), -departamento), file)
  )
}

shinyApp(ui, server)
