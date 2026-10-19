# =============================================================================
# ANÁLISIS DEL RENDIMIENTO ACADÉMICO EN INSTITUCIONES EDUCATIVAS DE COLOMBIA
# =============================================================================
# Este código busca realizar las el proceso de limpieza del dataset

# Secciones:
#   0. Paquetes y configuración
#   1. Carga y exploración inicial
#   2. Calidad de datos
#   3. Limpieza y selección de variables clave
#   4. Estadística descriptiva (summary)
#   5. Exportar datos limpios (para el dashboard)
# =============================================================================


# ---- 0. PAQUETES Y CONFIGURACIÓN --------------------------------------------
# Inicialmente definimos los paquetes que estaremos utilizando para el análisis de los datos

paquetes <- c(
  "tidyverse","plotly", "corrplot", "ggplot2",
  "readxl", "janitor", "skimr", "naniar", "dplyr", "here"
)

# Revisamos todos los paquetes que no se encuentran en la lista y los instalamos
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos)

# Con esta linea importamos las librerias en el entorno de trabajo
invisible(lapply(paquetes, library, character.only = TRUE))

source(here::here("app", "studentsResults", "R", "funciones_graficos.R"))

# Creamos las rutas con las salidas de los gráficos y de los datos procesados
dir.create("salidas/datos",    recursive = TRUE, showWarnings = FALSE)

# ---- 1. CARGA Y EXPLORACIÓN INICIAL -----------------------------------------

# Cargamos los datos crudos
ruta_data <- here::here("data","dataset fase 3.txt")
datos_raw <- read_csv(ruta_data, locale = locale(encoding = "ISO-8859-1"),
                      show_col_types = FALSE) %>%
  clean_names()          # nombres en minúscula y sin espacios/tildes

config_columnas <- c(
  id_estudiante = "id_estudiante",
  institucion   = "institucion",
  departamento  = "Departamento",
  asignatura    = "asignatura",
  calificacion  = "calificacion_final",   # escala 0-5 (si tu columna es calificacion_final, ponla aquí)
  asistencia    = "asistencias",     # % (0-100) o proporción (0-1)
  n_actividades = "numero_actividades"
)

# Cargamos los datos de las instituciones
localizacion_instituciones <- read_excel(here::here("data","instituciones educativas y ubicacion.xlsx"))

coordenadas_departamentos <- read_csv(here::here("data","departamentos_colombia.csv"),
                                      show_col_types = FALSE,
                                      locale = locale(encoding = "UTF-8"))

# Antes de realizar la unión de los datos, verificamos la calidad de la llave en común
cat("\n==== Revisamos errores de escritura ====\n")
print(count(datos_raw, institucion, sort = TRUE))

datos_raw <- datos_raw %>% 
  mutate(institucion = str_replace_all(institucion, c(
    "Inst B" = "Institución B",
    "Institucion E" = "Institución E"
  ))) %>%
  mutate(institucion = if_else(institucion == "A", "Institución A", institucion)) %>% 
  mutate(institucion = str_squish(institucion)) # Elimina espacios dobles y espacios al inicio/final


cat("\n==== Vemos cómo quedó la varaible luego del cambio ====\n")
print(count(datos_raw, institucion, sort = TRUE))

# Unimos los datos de las instituciones con los datos de los estudiantes
datos_localidad <- left_join(
  datos_raw, 
  localizacion_instituciones, 
  by = c("institucion" = "Institución educativa")
)

datos_localidad <- datos_localidad %>%
  mutate(Departamento = if_else(Departamento == "Bogotá", "Bogotá D.C.", Departamento))

datos_unidos <- left_join(
  datos_localidad,
  coordenadas_departamentos,
  by = c("Departamento" = "departamento")
)

# Identificamos posibles instituciones sin departamento
instituciones_sin_depto <- datos_unidos %>% 
  filter(is.na(Departamento)) %>% 
  select(institucion, Departamento)

cat("\n==== Instituciones sin departamento asignado ====\n")
print(count(instituciones_sin_depto))


# Realizamos una descripción inicial de los datos:

cat("\n==== DIMENSIONES ====\n"); print(dim(datos_unidos))
cat("\n==== ESTRUCTURA ====\n");  glimpse(datos_unidos)
cat("\n==== PRIMERAS FILAS ====\n"); print(head(datos_unidos, 10))
cat("\n==== RESUMEN SKIMR ====\n"); print(skim(datos_unidos))


# ---- 2. CALIDAD DE DATOS ----------------------------------------------------
cat("\n==== VALORES FALTANTES POR VARIABLE ====\n")
faltantes <- datos_unidos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(pct_na = round(100 * n_na / nrow(datos_unidos), 2)) %>%
  arrange(desc(n_na))
print(faltantes)

p_na <- gg_miss_var(datos_unidos, show_pct = TRUE) +
  labs(title = "Porcentaje de valores faltantes por variable")
guardar(p_na, "00_faltantes.png")

cat("\n==== FILAS DUPLICADAS ====\n")
print(sum(duplicated(datos_unidos)))

cat("\n==== VALORES FUERA DE RANGO ====\n")
fuera_rango <- datos_unidos %>%
  summarise(
    calif_fuera  = sum(calificacion_final < 0 | calificacion_final > 5, na.rm = TRUE),
    asist_fuera  = sum(asistencias  < 0 | asistencias  > 100, na.rm = TRUE)
  )
print(fuera_rango)

cat("\n==== CATEGORÍAS (revisar errores de escritura) ====\n")
print(count(datos_unidos, asignatura,  sort = TRUE))


# ---- 3. LIMPIEZA Y VARIABLES CLAVE ------------------------------------------

# Austamos los nombres de las asignaturas para que queden iguales
datos_unidos <- datos_unidos %>% 
  mutate(asignatura = str_replace_all(asignatura, c(
    "Matematicas" = "Matemáticas",
    "Lenguage" = "Lenguaje"
  )))%>%
  mutate(asignatura = if_else(asignatura == "Histori", "Historia", asignatura))

# Cambiamos los datos faltantes de las calificaciones finales por 0
datos_unidos <- datos_unidos %>% mutate(calificacion_final = replace_na(calificacion_final, 0))

datos_limpios <- datos_unidos %>%
  rename(all_of(config_columnas))%>%
  distinct() %>%                                         # quita duplicados exactos
  mutate(
    across(c(institucion, departamento, asignatura), ~ str_squish(str_to_title(.))),
    # Si la asistencia viene en 0-1, pasarla a porcentaje:
    asistencia = if_else(asistencia <= 1, asistencia * 100, asistencia)
  ) %>%
  filter(
    # Omitimos las asignaturas e instituciones sin registros
    !is.na(asignatura), !is.na(institucion),
    # Así como las calificaciones y asistencias por fuera del rango esperado
    between(calificacion, 0, 5),
    between(asistencia, 0, 100)
  )%>%
  # Creamos una serie de variables derivadas para futuros análisis
  mutate(
    nivel_desempeno = cut(
      calificacion,
      breaks = c(-Inf, 2.99, 3.99, 4.59, Inf),
      labels = c("Bajo", "Básico", "Alto", "Superior")
    ),
    categoria_asistencia = cut(
      asistencia,
      breaks = c(-Inf, 70, 85, 95, Inf),
      labels = c("Crítica (<70%)", "Baja (70-85%)", "Media (85-95%)", "Alta (>95%)")
    ),
    aprueba = calificacion >= 3.0 & asistencia > 80
  )

# Imputación simple de numero_actividades (mediana por asignatura) si hay NA
datos_limpios <- datos_limpios %>%
  group_by(asignatura) %>%
  mutate(n_actividades = if_else(is.na(n_actividades),
                                 median(n_actividades, na.rm = TRUE),
                                 n_actividades)) %>%
  ungroup()

# Variables clave
vars_clave <- c("id_estudiante", "institucion", "departamento",
                "asignatura", "calificacion", "asistencia",
                "n_actividades", "nivel_desempeno", "categoria_asistencia", "aprueba",
                "latitud", "longitud")
datos_clave <- datos_limpios %>% select(all_of(vars_clave))

cat("\nRegistros antes:", nrow(datos_unidos), "| después de limpiar:", nrow(datos_clave), "\n")


# ---- 4. ESTADÍSTICA DESCRIPTIVA ---------------------------------------------
cat("\n==== summary() CALIFICACIONES ====\n")
print(summary(datos_clave$calificacion))
cat("\n==== summary() ASISTENCIA ====\n")
print(summary(datos_clave$asistencia))
cat("\n==== summary() NÚMERO DE ACTIVIDADES ====\n")
print(summary(datos_clave$n_actividades))

cat("\n==== summary() GENERAL ====\n")
print(summary(datos_clave %>% select(calificacion, asistencia, n_actividades)))

cat("\n==== DESV. ESTÁNDAR Y COEF. DE VARIACIÓN ====\n")
print(datos_clave %>% summarise(
  sd_calif = sd(calificacion), cv_calif = sd(calificacion) / mean(calificacion),
  sd_asist = sd(asistencia),   cv_asist = sd(asistencia) / mean(asistencia)
))

resumen_asignatura <- datos_clave %>%
  group_by(asignatura) %>%
  summarise(n = n(), media_calif = mean(calificacion), mediana_calif = median(calificacion),
            media_asist = mean(asistencia), pct_aprueba = mean(aprueba) * 100,
            .groups = "drop") %>%
  arrange(desc(media_calif))

resumen_institucion <- datos_clave %>%
  group_by(institucion, departamento) %>%
  summarise(n = n(), media_calif = mean(calificacion), media_asist = mean(asistencia),
            media_act = mean(n_actividades), pct_aprueba = mean(aprueba) * 100,
            .groups = "drop") %>%
  arrange(desc(media_calif))

resumen_departamento <- datos_clave %>%
  group_by(departamento) %>%
  summarise(n_inst = n(), media_calif = mean(calificacion),
            media_asist = mean(asistencia), pct_aprueba = mean(aprueba) * 100,
            .groups = "drop") %>%
  arrange(desc(media_calif))

cat("\n==== POR ASIGNATURA ====\n");  print(resumen_asignatura)
cat("\n==== POR INSTITUCIÓN ====\n"); print(resumen_institucion)
cat("\n==== POR DEPARTAMENTO ====\n"); print(resumen_departamento)

cat("\n==== CORRELACIONES (Pearson y Spearman) ====\n")
mat_cor <- datos_clave %>% select(calificacion, asistencia, n_actividades)
print(round(cor(mat_cor, use = "complete.obs"), 3))
print(round(cor(mat_cor, use = "complete.obs", method = "spearman"), 3))
png("salidas/graficos/01_correlaciones.png", width = 1600, height = 1400, res = 250)
corrplot(cor(mat_cor, use = "complete.obs"), method = "color", addCoef.col = "black",
         tl.col = "black", title = "Matriz de correlación", mar = c(0, 0, 2, 0))
dev.off()

# ---- 5. EXPORTAR DATOS PARA EL DASHBOARD ------------------------------------
write_csv(datos_clave,         here::here("app", "studentsResults", "salidas", "datos", "datos_limpios.csv"))
write_csv(resumen_institucion, "salidas/datos/resumen_institucion.csv")
write_csv(resumen_asignatura,  "salidas/datos/resumen_asignatura.csv")
write_csv(resumen_departamento,   "salidas/datos/resumen_departamento.csv")
saveRDS(datos_clave, "salidas/datos/datos_clave.rds")

cargar_datos <- function(ruta) {
  if (file.exists(ruta)) {
    raw <- readr::read_csv(ruta, show_col_types = FALSE,
                           locale = readr::locale(encoding = "UTF-8")) |>
      janitor::clean_names()
    es_demo <- FALSE
  } else {
    message("No se encontró '", ruta, "'. Usando datos de demostración.")
    raw <- generar_datos_demo()
    es_demo <- TRUE
  }
  out <- raw
  attr(out, "es_demo") <- es_demo
  out
}

grafico_barras(datos_clave)
