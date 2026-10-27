# =============================================================================
# Herramienta 3 · Aplicación de cálculo de indicadores con AVP
# Plan Regional de Monitoreo de Especies Prioritarias del PET · MigraMar
#
# Misma lógica que el código de R (versión sin GAMM), presentada como una
# aplicación donde el usuario solo sube su archivo, rellena espacios y elige
# opciones.
#
# Cómo abrirla: abra este archivo en RStudio y dé clic en "Run App".
# Coloque el Catálogo de vocabulario ("H4_catalogo-vocabulario.xlsx") en la
# misma carpeta para que especie, indicador y área se elijan de una lista.
# =============================================================================

# ---- Paquetes ----------------------------------------------------------------
paquetes <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readxl")
faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(faltantes) > 0) install.packages(faltantes)
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(ggplot2); library(dplyr); library(tidyr)
})
library(grid)

options(shiny.maxRequestSize = 50 * 1024^2)   # archivos de hasta 50 MB

# ---- Catálogo de vocabulario (Herramienta 4) ---------------------------------
archivo_catalogo <- "H4_catalogo-vocabulario.xlsx"

# Carpeta donde está este app.R (por si R está trabajando en otra carpeta)
carpeta_app <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())

# Busca el catálogo en la carpeta de trabajo y en la carpeta de la aplicación.
# Acepta el nombre exacto o cualquier .xlsx cuyo nombre contenga "catalogo" (ej. catalogo-campos-proveedores.xlsx).
buscar_catalogo <- function() {
  carpetas <- unique(c(getwd(), carpeta_app))
  candidatos <- c(file.path(carpetas, archivo_catalogo),
                  unlist(lapply(carpetas, list.files, pattern = "^[^~].*catalogo.*\\.xlsx$",
                                ignore.case = TRUE, full.names = TRUE)))
  candidatos <- unique(candidatos[file.exists(candidatos)])
  if (length(candidatos)) candidatos[1] else NULL
}

# Lee el catálogo. Devuelve los datos, o NULL y el motivo del error para mostrarlo al usuario.
leer_catalogo <- function(ruta) {
  if (is.null(ruta) || !file.exists(ruta))
    return(list(datos = NULL, ruta = ruta,
                error = paste0("No se encontró el archivo '", archivo_catalogo, "' en: ",
                               paste(unique(c(getwd(), carpeta_app)), collapse = " | "))))
  leer_desde <- function(r) {
    leer <- function(hoja) suppressMessages(readxl::read_excel(r, sheet = hoja))
    esp <- leer("especie_id")
    ind <- leer("indicador_id") %>% tidyr::fill(especie_en)
    are <- leer("area_id")
    list(especies = esp, indicadores = ind, areas = are)
  }
  intento <- tryCatch(leer_desde(ruta), error = function(e) e)
  # En Windows, una ruta con tildes (ej. "Lucía") puede impedir la lectura: se reintenta con la ruta corta
  if (inherits(intento, "error") && .Platform$OS.type == "windows")
    intento <- tryCatch(leer_desde(utils::shortPathName(ruta)), error = function(e) intento)
  if (inherits(intento, "error"))
    return(list(datos = NULL, ruta = ruta,
                error = paste0("Se encontró el catálogo (", ruta, ") pero no se pudo leer: ",
                               conditionMessage(intento),
                               ". Revise que no esté abierto en Excel y que tenga las hojas especie_id, indicador_id y area_id.")))
  list(datos = intento, ruta = ruta, error = NULL)
}

colores_avp <- c("Crítico" = "#8B0000", "Alerta" = "#FF8C00", "Estable" = "#FFD700",
                 "Bueno" = "#90EE90", "Muy Bueno" = "#006400")
niveles_avp <- names(colores_avp)
ESCRIBIR <- "__escribir__"   # opción "escribir el dato directamente"
`%||%` <- function(a, b) if (is.null(a)) b else a

f2 <- function(x, d = 2) formatC(round(x, d), format = "f", digits = d)

# =============================================================================
# CÁLCULO (Secciones II a IV del código de R)
# Devuelve una lista con los resultados y los mensajes para el usuario.
# =============================================================================
calcular_indicador <- function(df_crudo, cfg) {
  msj <- list()
  aviso <- function(tipo, texto) msj[[length(msj) + 1]] <<- list(tipo = tipo, texto = texto)
  fallar <- function(texto) { aviso("error", texto); return(list(ok = FALSE, mensajes = msj)) }

  # II.1 Verificar que el archivo tenga información
  if (nrow(df_crudo) == 0 || ncol(df_crudo) == 0)
    return(fallar("El archivo no contiene datos válidos. Revise que cargó el archivo correcto."))

  # II.2 Homologación de nombres de columnas
  df_crudo$Anio  <- suppressWarnings(as.integer(df_crudo[[cfg$col_anio]]))
  df_crudo$Valor <- suppressWarnings(as.numeric(df_crudo[[cfg$col_valor]]))
  if (all(is.na(df_crudo$Anio)))  return(fallar(paste0("La columna de año ('", cfg$col_anio, "') no contiene años válidos.")))
  if (all(is.na(df_crudo$Valor))) return(fallar(paste0("La columna del valor ('", cfg$col_valor, "') no contiene números. Revise que los decimales usen punto.")))

  # II.3 Metadatos del proveedor: columna del archivo o dato escrito directamente
  obtener_meta <- function(col, valor_directo) {
    if (!is.null(col) && col != ESCRIBIR && col %in% names(df_crudo)) {
      as.character(unique(df_crudo[[col]])[1])
    } else if (!is.null(valor_directo) && nzchar(trimws(valor_directo))) {
      trimws(valor_directo)
    } else "No especificado"
  }
  meta_dueno       <- obtener_meta(cfg$col_dueno, cfg$dato_dueno)
  meta_contacto    <- obtener_meta(cfg$col_contacto, cfg$dato_contacto)
  meta_institucion <- obtener_meta(cfg$col_institucion, cfg$dato_institucion)
  meta_web         <- if (nzchar(trimws(cfg$web))) trimws(cfg$web) else NA_character_

  # II.4 Estandarización por esfuerzo de muestreo
  if (cfg$tipo_esfuerzo == "simple") {
    df_crudo$Esfuerzo <- suppressWarnings(as.numeric(df_crudo[[cfg$esf1]]))
    df_crudo$Valor <- df_crudo$Valor / df_crudo$Esfuerzo
    aviso("ok", paste0("Valor dividido entre el esfuerzo de la columna '", cfg$esf1, "'."))
  } else if (cfg$tipo_esfuerzo == "compuesto") {
    df_crudo$Esfuerzo <- suppressWarnings(as.numeric(df_crudo[[cfg$esf1]]) * as.numeric(df_crudo[[cfg$esf2]]))
    df_crudo$Valor <- df_crudo$Valor / df_crudo$Esfuerzo
    aviso("ok", paste0("Valor dividido entre el esfuerzo compuesto '", cfg$esf1, "' × '", cfg$esf2, "'."))
  } else {
    aviso("ok", "Indicador ya estandarizado (sin división por esfuerzo).")
  }

  # II.5 Eliminar filas vacías o incompletas
  n_antes <- nrow(df_crudo)
  df_crudo <- df_crudo[!is.na(df_crudo$Anio) & !is.na(df_crudo$Valor) & is.finite(df_crudo$Valor), ]
  if (n_antes > nrow(df_crudo))
    aviso("aviso", paste(n_antes - nrow(df_crudo), "fila(s) sin año o sin valor válido se excluyeron del cálculo."))

  # II.6 Años con datos
  anios_unicos <- sort(unique(df_crudo$Anio))
  aviso("ok", paste0("Años con datos: ", min(anios_unicos), " a ", max(anios_unicos),
                     " (", length(anios_unicos), " años)."))

  # III.A Validación de la línea base (ampliación automática 5 -> 10 años)
  anio_inicio_base <- cfg$base_inicio; anio_final_base <- cfg$base_fin
  df_linea_base <- df_crudo %>% filter(Anio >= anio_inicio_base & Anio <= anio_final_base)
  if (nrow(df_linea_base) == 0)
    return(fallar(paste0("No hay datos en la línea base definida (", anio_inicio_base, "-", anio_final_base, ").")))
  anios_linea_base   <- n_distinct(df_linea_base$Anio)
  total_eventos_base <- nrow(df_linea_base)

  if (anios_linea_base < 5) {
    return(fallar(paste0("La línea base tiene datos en solo ", anios_linea_base,
                         " año(s). Se requieren al menos 5 años para calcular el AVP.")))
  } else if (total_eventos_base < 15 && cfg$tipo_metrica != "valor_directo") {
    anio_final_original <- anio_final_base
    anio_final_base <- min(anio_inicio_base + 9, max(df_crudo$Anio))
    df_linea_base <- df_crudo %>% filter(Anio >= anio_inicio_base & Anio <= anio_final_base)
    total_eventos_base <- nrow(df_linea_base)
    aviso("aviso", paste0("Reajuste automático de la línea base: el periodo ", anio_inicio_base, "-",
                          anio_final_original, " tenía menos de 15 eventos. Nueva línea base: ",
                          anio_inicio_base, "-", anio_final_base, "."))
  } else {
    aviso("ok", paste0("Línea base válida (", anio_inicio_base, "-", anio_final_base, "): ",
                       total_eventos_base, " eventos acumulados."))
  }
  eventos_por_anio <- df_linea_base %>% count(Anio, name = "n_eventos")
  bajos <- eventos_por_anio$Anio[eventos_por_anio$n_eventos < 10]
  if (length(bajos) > 0)
    aviso("aviso", paste0("Años de la línea base con pocos eventos (< 10): ", paste(bajos, collapse = ", "),
                          ". Es solo informativo."))

  # III.B Estadísticas descriptivas de la línea base (referencia)
  v <- df_linea_base$Valor
  estadisticas <- data.frame(Mínimo = min(v), Máximo = max(v), Media = mean(v), Mediana = median(v),
                             `Desv. est.` = sd(v), Varianza = var(v), check.names = FALSE)

  # III.C Percentiles del AVP
  perc <- quantile(v, probs = c(0.10, 0.25, 0.50, 0.75), na.rm = TRUE)
  P10 <- perc[[1]]; P25 <- perc[[2]]; P50 <- perc[[3]]; P75 <- perc[[4]]

  # III.D Valor anual y categoría AVP
  # Por año: réplicas, mediana, rango intercuartílico (RIC), promedio y desviación estándar (DE).
  # El valor anual es el estadístico elegido en el paso 3.
  indicador_anual <- df_crudo %>% group_by(Anio) %>%
    summarise(n_eventos = n(),
              Mediana   = median(Valor, na.rm = TRUE),
              RIC       = IQR(Valor, na.rm = TRUE),   # P75 - P25 de las réplicas del año
              Promedio  = mean(Valor, na.rm = TRUE),
              DE        = sd(Valor, na.rm = TRUE),    # NA si el año tiene una sola réplica
              Primero   = dplyr::first(Valor),
              .groups   = "drop") %>%
    mutate(Valor_Anual = case_when(cfg$tipo_metrica == "promedio"      ~ Promedio,
                                   cfg$tipo_metrica == "valor_directo" ~ Primero,
                                   TRUE                                ~ Mediana))

  # Dispersión que acompaña al valor anual:
  # mediana -> RIC · promedio -> DE · valor directo -> no aplica (NA)
  # Los códigos de 'dispersion_type' vienen del Catálogo de vocabulario (hoja dispersion_id).
  if (cfg$tipo_metrica == "mediana") {
    indicador_anual$Dispersion <- indicador_anual$RIC;  dispersion_type <- "interquartile_range"
  } else if (cfg$tipo_metrica == "promedio") {
    indicador_anual$Dispersion <- indicador_anual$DE;   dispersion_type <- "standard_deviation"
  } else {
    indicador_anual$Dispersion <- NA_real_;             dispersion_type <- "not_applicable"
  }
  indicador_anual$Categoria <- if (cfg$tipo_interpretacion == "A") {
    case_when(indicador_anual$Valor_Anual <= P10 ~ "Crítico", indicador_anual$Valor_Anual <= P25 ~ "Alerta",
              indicador_anual$Valor_Anual <= P50 ~ "Estable", indicador_anual$Valor_Anual <= P75 ~ "Bueno",
              TRUE ~ "Muy Bueno")
  } else {
    case_when(indicador_anual$Valor_Anual > P75 ~ "Crítico", indicador_anual$Valor_Anual > P50 ~ "Alerta",
              indicador_anual$Valor_Anual > P25 ~ "Estable", indicador_anual$Valor_Anual > P10 ~ "Bueno",
              TRUE ~ "Muy Bueno")
  }
  indicador_anual$Categoria <- factor(indicador_anual$Categoria, levels = niveles_avp)

  # IV. Tabla final con el formato de la hoja datos_calculados (Herramienta 2)
  # Cada celda de año: observado;dispersión;predicho (predicho = NA sin modelo), 3 decimales
  f3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))
  valores_anchos <- indicador_anual %>%
    mutate(Celda = paste(f3(Valor_Anual), f3(Dispersion), "NA", sep = ";")) %>%
    select(Anio, Celda) %>%
    pivot_wider(names_from = Anio, values_from = Celda)
  tabla_final <- data.frame(
    species = cfg$meta_species, indicator = cfg$meta_indicator, area = cfg$meta_area,
    metric_type = "observed_AVP", dispersion_type = dispersion_type,
    baseline_years = paste0(anio_inicio_base, "-", anio_final_base),
    P10 = round(P10, 3), P25 = round(P25, 3), P50 = round(P50, 3), P75 = round(P75, 3),
    check.names = FALSE) %>%
    bind_cols(valores_anchos) %>%
    mutate(data_owner = meta_dueno, provider_contact = meta_contacto,
           provider_institution = meta_institucion, additional_contact_info = meta_web)

  r <- list(ok = TRUE, mensajes = msj, anual = indicador_anual, estadisticas = estadisticas,
       P10 = P10, P25 = P25, P50 = P50, P75 = P75,
       base_inicio = anio_inicio_base, base_fin = anio_final_base, n_base = total_eventos_base,
       tabla_final = tabla_final,
       meta_dueno = meta_dueno, meta_contacto = meta_contacto,
       meta_institucion = meta_institucion, meta_web = meta_web)
  # Columna 'comments' de la Plantilla de datos calculados: texto "Especificaciones y fuente del dato"
  r$tabla_final$comments <- texto_fuente(r, cfg)
  r
}

# ---- Gráficos ----------------------------------------------------------------
grafico_avp <- function(r, cfg) {
  ggplot(r$anual, aes(x = Anio, y = Valor_Anual)) +
    geom_hline(yintercept = r$P50, color = "#00B0FF", linewidth = 0.8) +
    geom_hline(yintercept = c(r$P25, r$P75), linetype = "dashed", color = "#90A4AE", linewidth = 0.6) +
    geom_line(color = "#002447", linewidth = 0.8) +
    geom_point(aes(color = Categoria), size = 3.5) +
    scale_color_manual(values = colores_avp, drop = FALSE) +
    scale_x_continuous(breaks = r$anual$Anio) +
    labs(subtitle = "Línea azul = P50 (mediana de la línea base); líneas grises punteadas = P25 y P75",
         x = NULL, y = paste0(cfg$tipo_metrica_txt, " anual (", cfg$unidad, ")"), color = "Categoría AVP") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",
          panel.grid.minor = element_blank())
}

grafico_marine_manager <- function(r, cfg) {
  ggplot(r$anual, aes(x = Anio, y = Valor_Anual)) +
    annotate("rect", xmin = r$base_inicio - 0.5, xmax = r$base_fin + 0.5,
             ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6) +
    annotate("text", x = (r$base_inicio + r$base_fin) / 2, y = Inf, vjust = 1.5,
             label = "Línea base", size = 3, color = "grey40") +
    geom_line(color = "grey70", linewidth = 0.5) +
    geom_point(aes(color = Categoria), size = 3) +
    scale_color_manual(values = colores_avp, drop = FALSE, name = NULL) +
    scale_x_continuous(breaks = r$anual$Anio) +
    labs(x = NULL, y = cfg$unidad) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# ---- Textos en oraciones -----------------------------------------------------
# Qué representa cada valor anual y su dispersión (por si la plataforma no muestra 'dispersion_type')
texto_dispersion <- function(cfg) {
  switch(cfg$tipo_metrica,
    mediana  = "Cada valor anual es la mediana de las réplicas del año, acompañada del rango intercuartílico como medida de dispersión.",
    promedio = "Cada valor anual es el promedio de las réplicas del año, acompañado de la desviación estándar como medida de dispersión.",
    "Cada valor anual es un valor único por año, por lo que no se reporta una medida de dispersión.")
}
texto_fuente <- function(r, cfg) {
  paste0("Este indicador se calcula con datos tomados en ", cfg$sitios, " (", cfg$area_txt, "), durante ",
         cfg$estacion, ", entre ", min(r$anual$Anio), " y ", max(r$anual$Anio), ". ",
         texto_dispersion(cfg), " ",
         "Fuente del dato: ", r$meta_institucion,
         if (!is.na(r$meta_web)) paste0(" (", r$meta_web, ")") else "", ". ",
         "Punto de contacto: ", r$meta_dueno, " (", r$meta_contacto, ").")
}
texto_avp <- function(r, cfg) {
  paste0("La línea base (", r$base_inicio, "-", r$base_fin, ") define los umbrales del ",
         "Ámbito de Variación Permisible: P10 = ", f2(r$P10), ", P25 = ", f2(r$P25),
         ", P50 = ", f2(r$P50), " y P75 = ", f2(r$P75), " ", cfg$unidad, ".")
}

# ---- Reporte de revisión en PDF (Sección VI del código de R) -------------------
crear_reporte_pdf <- function(archivo, r, cfg) {
  y_pos <- 0.95
  escribir <- function(txt, tam = 10, negrita = FALSE, color = "#1f2d33", sangria = 0.07,
                       ancho = round(1080 / tam)) {
    for (l in unlist(lapply(txt, strwrap, width = ancho))) {
      grid.text(l, x = sangria, y = y_pos, just = c("left", "top"),
                gp = gpar(fontsize = tam, fontface = if (negrita) "bold" else "plain", col = color))
      y_pos <<- y_pos - (tam * 1.45) / (11 * 72)
    }
    y_pos <<- y_pos - 0.008
  }
  titulo <- function(txt) { y_pos <<- y_pos - 0.012; escribir(txt, 12.5, TRUE, "#0b4f6c") }

  dispositivo <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  dispositivo(archivo, width = 8.5, height = 11, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)

  # Página 1: así aparecerá en Marine Manager
  grid.newpage(); y_pos <- 0.95
  escribir("Reporte de revisión previa a publicación", 17, TRUE, "#0b4f6c")
  escribir(paste0("Indicador ", cfg$codigo, " · ", cfg$area_txt, " · Generado el ",
                  format(Sys.Date(), "%d/%m/%Y")), 9.5, color = "#4a6572")
  escribir("BORRADOR PARA REVISIÓN DEL PROVEEDOR — NO PUBLICADO", 9, TRUE, "#c0392b")
  titulo("Así aparecerá su indicador en Marine Manager")
  escribir(paste0(cfg$especie_txt, " · ", cfg$indicador_txt), 10, TRUE)
  print(grafico_marine_manager(r, cfg), vp = viewport(x = 0.5, y = y_pos, width = 0.88, height = 0.33, just = "top"))
  y_pos <- y_pos - 0.345
  titulo("Especificaciones y fuente del dato")
  escribir(texto_fuente(r, cfg))

  # ---- Bloque "Solo para su referencia": ficha -> AVP y estadísticos -> valores por año ----
  # Si un apartado no cabe, continúa en una página nueva dentro del mismo recuadro gris.
  alto_linea <- function(tam) (tam * 1.45) / (11 * 72)
  nueva_pagina_referencia <- function(continuacion = TRUE) {
    grid.newpage(); y_pos <<- 0.95
    grid.rect(x = 0.5, y = 0.965, width = 0.90, height = 0.93, just = "top",
              gp = gpar(fill = "#f3f5f6", col = "#c9d1d6"))
    if (continuacion) {
      escribir("Solo para su referencia (continuación) — esta información NO aparecerá en Marine Manager", 11, TRUE, "#4a6572")
    } else {
      escribir("Solo para su referencia — esta información NO aparecerá en Marine Manager", 11, TRUE, "#4a6572")
      escribir("Le compartimos estos resultados para que pueda revisar el cálculo de su indicador.", 9, color = "#4a6572")
    }
  }
  asegurar_espacio <- function(necesario) if (y_pos - necesario < 0.035) nueva_pagina_referencia()


  # 1. Ficha del indicador
  ficha <- c(
    paste0("Especie: ", cfg$especie_txt),
    paste0("Indicador: ", cfg$codigo, " (", cfg$indicador_txt, ")"),
    paste0("Área de estudio: ", cfg$area_txt),
    paste0("Sitio(s) de muestreo: ", cfg$sitios),
    paste0("Estación o época de muestreo: ", cfg$estacion),
    paste0("Periodo con datos: ", min(r$anual$Anio), "-", max(r$anual$Anio), " (", nrow(r$anual), " años)"),
    paste0("Línea base: ", r$base_inicio, "-", r$base_fin),
    paste0("Estadístico anual: ", cfg$tipo_metrica_txt, " (", cfg$unidad, ") · Interpretación: ", cfg$tipo_interpretacion),
    paste0("Propietario del dato: ", r$meta_institucion, if (!is.na(r$meta_web)) paste0(" (", r$meta_web, ")") else ""),
    paste0("Punto de contacto: ", r$meta_dueno, " (", r$meta_contacto, ")"))
  # El bloque empieza en la página 1 solo si cabe la ficha completa; si no, empieza en una página nueva
  espacio_ficha <- 0.02 + 0.06 + 0.05 + length(ficha) * alto_linea(9.5)
  if (y_pos - espacio_ficha < 0.035) {
    nueva_pagina_referencia(continuacion = FALSE)
  } else {
    y_pos <- y_pos - 0.02
    grid.rect(x = 0.5, y = y_pos + 0.008, width = 0.90, height = y_pos - 0.03, just = "top",
              gp = gpar(fill = "#f3f5f6", col = "#c9d1d6"))
    escribir("Solo para su referencia — esta información NO aparecerá en Marine Manager", 11, TRUE, "#4a6572")
    escribir("Le compartimos estos resultados para que pueda revisar el cálculo de su indicador.", 9, color = "#4a6572")
  }
  titulo("Ficha del indicador")
  escribir(ficha, 9.5)

  # 2. Ámbito de Variación Permisible y estadísticos descriptivos de la línea base
  asegurar_espacio(0.19)
  titulo("Ámbito de Variación Permisible")
  escribir(texto_avp(r, cfg))
  escribir("Estadísticos descriptivos de la línea base", 10, TRUE)
  est <- c(`Réplicas (n)` = as.character(r$n_base),
           setNames(f2(unlist(r$estadisticas[1, ]), 3), names(r$estadisticas)))
  cols_est <- 0.07 + (seq_along(est) - 1) * 0.125
  for (j in seq_along(est)) {
    grid.text(names(est)[j], x = cols_est[j], y = y_pos, just = c("left", "top"),
              gp = gpar(fontsize = 9, fontface = "bold"))
    grid.text(est[[j]], x = cols_est[j], y = y_pos - 0.02, just = c("left", "top"), gp = gpar(fontsize = 9.5))
  }
  y_pos <- y_pos - 0.055

  # 3. Valores por año
  asegurar_espacio(0.05 + min(nrow(r$anual) + 1, 12) * 0.019)
  titulo("Valores por año")
  cols_x <- c(0.07, 0.14, 0.21, 0.33, 0.48, 0.60, 0.71, 0.83)
  encab  <- c("Año", "n", "Valor anual", "Categoría AVP", "Mediana", "RIC", "Promedio", "DE")
  alto <- min(0.019, (y_pos - 0.06) / (nrow(r$anual) + 1))
  for (j in seq_along(encab)) grid.text(encab[j], x = cols_x[j], y = y_pos, just = c("left", "top"),
                                        gp = gpar(fontsize = 9, fontface = "bold"))
  for (i in seq_len(nrow(r$anual))) {
    a <- r$anual[i, ]
    fila_i <- c(a$Anio, a$n_eventos, f2(a$Valor_Anual, 3), as.character(a$Categoria),
                f2(a$Mediana, 3), f2(a$RIC, 3), f2(a$Promedio, 3), f2(a$DE, 3))
    for (j in seq_along(fila_i)) grid.text(fila_i[j], x = cols_x[j], y = y_pos - i * alto,
                                           just = c("left", "top"), gp = gpar(fontsize = 9))
  }
  y_pos <- y_pos - (nrow(r$anual) + 1) * alto - 0.01
  grid.text("RIC = rango intercuartílico (P75 - P25 de las réplicas del año) · DE = desviación estándar",
            x = 0.07, y = y_pos, just = c("left", "top"), gp = gpar(fontsize = 8, col = "#4a6572"))
}

# =============================================================================
# INTERFAZ
# =============================================================================
ayuda <- function(...) tags$p(class = "text-muted small mb-2", ...)

ui <- page_navbar(
  title = "Cálculo de indicadores · MigraMar",
  id = "pasos",
  theme = bs_theme(version = 5, primary = "#0b4f6c",
                   base_font = font_collection("Segoe UI", "system-ui", "Roboto", "Arial", "sans-serif")),
  header = tags$style(HTML("
    .paso-intro { max-width: 900px; }
    .msj-ok { color: #2e7d5b; } .msj-aviso { color: #a35d00; } .msj-error { color: #b3261e; font-weight: 600; }
    .tabla-chica table { font-size: .9rem; }
  ")),

  # ---- Paso 1 -------------------------------------------------------------
  nav_panel("1. Datos",
    div(class = "paso-intro",
      h4("Paso 1 · Suba su archivo de datos"),
      p("Use un archivo .csv con una fila por réplica, filtrado para un solo indicador, una especie y un área.",
        "Sus columnas pueden tener cualquier nombre: en el paso 2 indicará cuál es cuál."),
      layout_columns(col_widths = c(6, 6),
        fileInput("archivo", "Archivo de datos (.csv)", accept = ".csv",
                  buttonLabel = "Buscar…", placeholder = "Ningún archivo seleccionado"),
        radioButtons("encoding", "Codificación del archivo",
                     c("Latin1 (CSV normal de Excel)" = "Latin1", "UTF-8 (CSV UTF-8)" = "UTF-8"), inline = FALSE)
      ),
      ayuda("Si ve letras raras en tildes o ñ en la vista previa, cambie la codificación."),
      uiOutput("resumen_archivo"),
      div(class = "tabla-chica", tableOutput("vista_previa"))
    )
  ),

  # ---- Paso 2 -------------------------------------------------------------
  nav_panel("2. Columnas",
    div(class = "paso-intro",
      h4("Paso 2 · Indique qué columna corresponde a cada dato"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Datos del monitoreo"),
          selectInput("col_anio", "Año", choices = NULL),
          selectInput("col_valor", "Valor medido del indicador (por réplica)", choices = NULL),
          radioButtons("tipo_esfuerzo", "Esfuerzo de muestreo",
                       c("El valor ya está estandarizado (sin esfuerzo)" = "ninguno",
                         "Una variable de esfuerzo (ej. horas de grabación)" = "simple",
                         "Dos variables de esfuerzo (ej. ancho × largo del transecto)" = "compuesto")),
          conditionalPanel("input.tipo_esfuerzo != 'ninguno'",
            selectInput("esf1", "Columna de esfuerzo", choices = NULL)),
          conditionalPanel("input.tipo_esfuerzo == 'compuesto'",
            selectInput("esf2", "Segunda columna de esfuerzo", choices = NULL))
        ),
        card(card_header("Datos del proveedor (para darle crédito)"),
          ayuda("Elija la columna de su archivo o seleccione “Escribir el dato” y escríbalo."),
          tags$h6("1. Propietario del dato"),
          selectInput("col_institucion", "Institución, organización o proyecto propietario del dato", choices = NULL),
          conditionalPanel("input.col_institucion == '__escribir__'", textInput("dato_institucion", NULL, placeholder = "Ej. Universidad XYZ")),
          textInput("web", "Página web (opcional)", placeholder = "Ej. www.miproyecto.org"),
          tags$h6(class = "mt-3", "2. Punto de contacto"),
          selectInput("col_dueno", "Nombre del punto de contacto", choices = NULL),
          conditionalPanel("input.col_dueno == '__escribir__'", textInput("dato_dueno", NULL, placeholder = "Ej. Juana Pérez")),
          selectInput("col_contacto", "Correo del punto de contacto", choices = NULL),
          conditionalPanel("input.col_contacto == '__escribir__'", textInput("dato_contacto", NULL, placeholder = "Ej. juana.perez@correo.org"))
        )
      )
    )
  ),

  # ---- Paso 3 -------------------------------------------------------------
  nav_panel("3. Indicador",
    div(class = "paso-intro",
      h4("Paso 3 · Configure su indicador"),
      p("Use la Referencia metodológica (Herramienta 1, Parte 2) para elegir el estadístico, la interpretación y la línea base."),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Especie, indicador y área (Catálogo de vocabulario)"),
          uiOutput("ui_catalogo")
        ),
        card(card_header("Parámetros metodológicos"),
          selectInput("tipo_metrica", "Estadístico anual",
                      c("Mediana" = "mediana", "Promedio" = "promedio", "Valor directo" = "valor_directo")),
          radioButtons("tipo_interpretacion", "Tipo de interpretación",
                       c("A · valores altos = condición favorable" = "A",
                         "B · valores altos = condición desfavorable" = "B")),
          layout_columns(col_widths = c(6, 6),
            numericInput("base_inicio", "Línea base: año inicial", value = NA, step = 1),
            numericInput("base_fin", "Línea base: año final", value = NA, step = 1)),
          ayuda("Normalmente 5 años. Si no hay suficientes eventos, se amplía sola a 10 años."),
          textInput("unidad", "Unidad del valor anual", placeholder = "Ej. MaxN/hr")
        )
      ),
      card(card_header("Especificaciones del muestreo (aparecerán en Marine Manager)"),
        layout_columns(col_widths = c(6, 6),
          textInput("sitios", "Sitio(s) de muestreo", placeholder = "Ej. los sitios Manuelita, Dirty Rock y Alcyone"),
          textInput("estacion", "Estación o época de muestreo", placeholder = "Ej. la época seca (marzo a mayo)")),
        ayuda("Escríbalos como completarían la frase: “Este indicador se calcula con datos tomados en ___, durante ___”.")
      )
    )
  ),

  # ---- Paso 4 -------------------------------------------------------------
  nav_panel("4. Resultados",
    div(class = "paso-intro",
      h4("Paso 4 · Calcule y descargue sus resultados"),
      actionButton("calcular", "Calcular indicador", class = "btn-primary btn-lg mb-3"),
      uiOutput("mensajes"),
      uiOutput("bloque_resultados")
    )
  ),
  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "Herramienta 3 · Plan Regional de Monitoreo PET"))
)

# =============================================================================
# SERVIDOR
# =============================================================================
server <- function(input, output, session) {

  info_catalogo <- leer_catalogo(buscar_catalogo())
  catalogo_rv   <- reactiveVal(info_catalogo$datos)
  error_catalogo <- reactiveVal(info_catalogo$error)

  # Alternativa: el usuario carga el catálogo a mano si no se encontró o no se pudo leer
  observeEvent(input$catalogo_subido, {
    info <- leer_catalogo(input$catalogo_subido$datapath)
    catalogo_rv(info$datos); error_catalogo(info$error)
  })

  # ---- Paso 1: lectura del archivo ----
  datos <- reactive({
    req(input$archivo)
    tryCatch(read.csv(input$archivo$datapath, fileEncoding = input$encoding, check.names = FALSE),
             error = function(e) NULL)
  })

  output$resumen_archivo <- renderUI({
    req(input$archivo)
    d <- datos()
    if (is.null(d)) return(div(class = "msj-error", "No se pudo leer el archivo. Revise que sea un .csv o cambie la codificación."))
    div(class = "msj-ok mb-2", sprintf("✓ Archivo cargado: %d filas y %d columnas. Vista previa de las primeras filas:", nrow(d), ncol(d)))
  })
  output$vista_previa <- renderTable(head(datos(), 8), striped = TRUE, spacing = "xs")

  # Rellenar listas de columnas al cargar el archivo (adivina nombres comunes)
  observeEvent(datos(), {
    cols <- names(datos())
    adivinar <- function(patron, default = cols[1]) {
      m <- grep(patron, cols, ignore.case = TRUE, value = TRUE); if (length(m)) m[1] else default
    }
    updateSelectInput(session, "col_anio",  choices = cols, selected = adivinar("^(a[nñ]o|anio|year)$"))
    updateSelectInput(session, "col_valor", choices = cols, selected = adivinar("valor|maxn|value"))
    updateSelectInput(session, "esf1", choices = cols, selected = adivinar("esfuerzo|tiempo|hora|effort"))
    updateSelectInput(session, "esf2", choices = cols)
    opc <- c("— Escribir el dato —" = ESCRIBIR, cols)
    updateSelectInput(session, "col_dueno", choices = opc, selected = adivinar("due|owner|investig", ESCRIBIR))
    updateSelectInput(session, "col_institucion", choices = opc, selected = adivinar("instit", ESCRIBIR))
    updateSelectInput(session, "col_contacto", choices = opc, selected = adivinar("contact|mail|correo", ESCRIBIR))
  })

  # Sugerir línea base: primeros 5 años de la serie
  observeEvent(input$col_anio, {
    req(datos(), input$col_anio %in% names(datos()))
    a <- suppressWarnings(as.integer(datos()[[input$col_anio]]))
    if (any(!is.na(a))) {
      updateNumericInput(session, "base_inicio", value = min(a, na.rm = TRUE))
      updateNumericInput(session, "base_fin", value = min(a, na.rm = TRUE) + 4)
    }
  })

  # ---- Paso 3: catálogo ----
  output$ui_catalogo <- renderUI({
    if (is.null(catalogo_rv())) {
      tagList(
        div(class = "msj-aviso small mb-2",
            tags$b("No se pudo usar el Catálogo de vocabulario."), tags$br(), error_catalogo()),
        fileInput("catalogo_subido", "Cargue aquí el catálogo (.xlsx)", accept = ".xlsx",
                  buttonLabel = "Buscar…", placeholder = "H4_catalogo-vocabulario.xlsx"),
        ayuda("O escriba los valores exactos del catálogo:"),
        textInput("meta_species", "Especie (hoja especie_id, columna especie_id)"),
        textInput("meta_indicator", "Indicador (hoja indicador_id, columna id_cmar_gfw)"),
        textInput("codigo", "Código corto del indicador (ej. SLE-01)"),
        textInput("meta_area", "Área de estudio (hoja area_id, columna area_id)")
      )
    } else {
      esp <- catalogo_rv()$especies
      tagList(
        selectInput("meta_species", "Especie",
                    setNames(esp$especie_id, paste0(esp$label_es, " (", esp$label_scientific, ")"))),
        selectInput("codigo_indicador", "Indicador", choices = NULL),
        selectizeInput("meta_area", "Área de estudio",
                       setNames(catalogo_rv()$areas$area_id, catalogo_rv()$areas$label_es),
                       options = list(placeholder = "Escriba para buscar…")),
        ayuda("¿Su área no aparece? Comuníquese con el enlace técnico para agregarla al catálogo.")
      )
    }
  })

  observeEvent(input$meta_species, {
    req(catalogo_rv())
    esp_en <- catalogo_rv()$especies$label_en[catalogo_rv()$especies$especie_id == input$meta_species]
    ind <- catalogo_rv()$indicadores %>% filter(tolower(especie_en) == tolower(esp_en))
    if (nrow(ind) == 0) ind <- catalogo_rv()$indicadores
    # IMPORTANTE: el valor de cada opción es el código corto (ej. SLE-05), que es único en el catálogo.
    # El id_cmar_gfw NO sirve como identificador porque se repite entre especies
    # (ej. "number_of_individuals_sighted_per_dive" es MOL-01, CMA-05, SLE-05 y RTY-03).
    updateSelectInput(session, "codigo_indicador",
                      choices = setNames(ind$codigo_corto, paste0(ind$codigo_corto, " · ", ind$indicador_es)))
  })

  # ---- Configuración reunida ----
  configuracion <- reactive({
    lbl <- function(tabla, id_col, id, col) {
      if (is.null(catalogo_rv())) return(id)
      x <- catalogo_rv()[[tabla]][[col]][catalogo_rv()[[tabla]][[id_col]] == id]; if (length(x)) x[1] else id
    }
    # CÓDIGO DEL INDICADOR (define el nombre de los archivos): se toma directo de la opción elegida.
    # Con catálogo: el id_cmar_gfw y el nombre del indicador se buscan por el código corto (único).
    codigo         <- if (is.null(catalogo_rv())) input$codigo else input$codigo_indicador
    meta_indicator <- if (is.null(catalogo_rv())) input$meta_indicator else lbl("indicadores", "codigo_corto", codigo, "id_cmar_gfw")
    list(
      col_anio = input$col_anio, col_valor = input$col_valor,
      tipo_esfuerzo = input$tipo_esfuerzo, esf1 = input$esf1, esf2 = input$esf2,
      col_dueno = input$col_dueno, dato_dueno = input$dato_dueno,
      col_institucion = input$col_institucion, dato_institucion = input$dato_institucion,
      col_contacto = input$col_contacto, dato_contacto = input$dato_contacto, web = input$web,
      meta_species = input$meta_species, meta_indicator = meta_indicator, meta_area = input$meta_area,
      codigo = codigo,
      especie_txt = lbl("especies", "especie_id", input$meta_species, "label_es"),
      indicador_txt = if (is.null(catalogo_rv())) meta_indicator else lbl("indicadores", "codigo_corto", codigo, "indicador_es"),
      area_txt = lbl("areas", "area_id", input$meta_area, "label_es"),
      tipo_metrica = input$tipo_metrica,
      tipo_metrica_txt = c(mediana = "Mediana", promedio = "Promedio", valor_directo = "Valor directo")[[input$tipo_metrica]],
      tipo_interpretacion = input$tipo_interpretacion,
      base_inicio = input$base_inicio, base_fin = input$base_fin,
      unidad = input$unidad, sitios = input$sitios, estacion = input$estacion
    )
  })

  # ---- Paso 4: cálculo ----
  resultado <- eventReactive(input$calcular, {
    d <- datos(); cfg <- configuracion()
    faltan <- c(
      if (is.null(d)) "su archivo de datos (paso 1)",
      if (!nzchar(cfg$meta_species %||% "") || !nzchar(cfg$meta_indicator %||% "") || !nzchar(cfg$meta_area %||% "")) "especie, indicador y área (paso 3)",
      if (!nzchar(cfg$codigo %||% "")) "el código corto del indicador (paso 3)",
      if (is.na(cfg$base_inicio) || is.na(cfg$base_fin)) "los años de la línea base (paso 3)",
      if (!nzchar(trimws(cfg$unidad))) "la unidad del valor anual (paso 3)",
      if (!nzchar(trimws(cfg$sitios)) || !nzchar(trimws(cfg$estacion))) "los sitios y la época de muestreo (paso 3)")
    if (length(faltan)) return(list(ok = FALSE, mensajes = list(list(tipo = "error",
                          texto = paste0("Falta completar: ", paste(faltan, collapse = "; "), ".")))))
    tryCatch(calcular_indicador(d, cfg),
             error = function(e) list(ok = FALSE, mensajes = list(list(tipo = "error",
                                     texto = paste("Ocurrió un error en el cálculo:", conditionMessage(e))))))
  })

  output$mensajes <- renderUI({
    r <- resultado()
    icono <- c(ok = "✓ ", aviso = "⚠ ", error = "✗ ")
    tagList(lapply(r$mensajes, function(m) div(class = paste0("msj-", m$tipo), paste0(icono[[m$tipo]], m$texto))), tags$hr())
  })

  # NOMBRE DE LOS ARCHIVOS DESCARGADOS: [código corto]_[área]_calculado_[fecha]
  nombre_base <- reactive({
    cfg <- configuracion()
    paste0(cfg$codigo, "_", cfg$meta_area, "_calculado_", Sys.Date())
  })

  output$bloque_resultados <- renderUI({
    r <- resultado(); req(isTRUE(r$ok))
    tagList(
      layout_columns(col_widths = c(3, 3, 3, 3),
        value_box("P10", f2(r$P10, 3)), value_box("P25", f2(r$P25, 3)),
        value_box("P50 (mediana)", f2(r$P50, 3)), value_box("P75", f2(r$P75, 3))),
      card(card_header(paste0("Indicador anual con evaluación AVP · línea base ", r$base_inicio, "-", r$base_fin)),
           plotOutput("grafico", height = "420px")),
      card(card_header("Valores por año (RIC = rango intercuartílico · DE = desviación estándar)"),
           div(class = "tabla-chica", tableOutput("tabla_anual"))),
      layout_columns(col_widths = c(12),
        card(card_header("Así aparecerá en Marine Manager (texto)"),
             p(texto_fuente(r, configuracion())),
             tags$hr(),
             tags$p(class = "text-muted small", "Solo para su referencia (no se publica):"),
             p(texto_avp(r, configuracion())),
             tags$p(class = "text-muted small mt-3", "Estadísticas descriptivas de la línea base (referencia):"),
             div(class = "tabla-chica", tableOutput("tabla_estadisticas")))
      ),
      card(card_header("Descargas"),
        p("El archivo .csv es el que se entrega para Marine Manager. El PDF es el reporte de revisión previa a publicación."),
        div(downloadButton("descargar_csv", "Descargar resultado (.csv)", class = "btn-primary me-2"),
            downloadButton("descargar_pdf", "Descargar reporte de revisión (.pdf)")))
    )
  })

  output$grafico <- renderPlot({ r <- resultado(); req(isTRUE(r$ok)); grafico_avp(r, configuracion()) })
  output$tabla_anual <- renderTable({
    r <- resultado(); req(isTRUE(r$ok))
    a <- r$anual
    data.frame(Año = as.character(a$Anio), `Réplicas (n)` = a$n_eventos, `Valor anual` = f2(a$Valor_Anual, 3),
               Categoría = as.character(a$Categoria), Mediana = f2(a$Mediana, 3),
               `RIC` = f2(a$RIC, 3), Promedio = f2(a$Promedio, 3), `DE` = f2(a$DE, 3), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$tabla_estadisticas <- renderTable({ r <- resultado(); req(isTRUE(r$ok)); r$estadisticas },
                                           digits = 3, spacing = "xs")

  output$descargar_csv <- downloadHandler(
    filename = function() paste0(nombre_base(), ".csv"),
    content  = function(file) {
      # UTF-8 con marca BOM: así Excel muestra bien las tildes, la ñ y otros caracteres especiales
      con <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(con))
      writeChar("\ufeff", con, eos = NULL)
      write.csv(resultado()$tabla_final, con, row.names = FALSE, na = "")
    })
  output$descargar_pdf <- downloadHandler(
    filename = function() paste0(nombre_base(), "_reporte-revision.pdf"),
    content  = function(file) crear_reporte_pdf(file, resultado(), configuracion()))
}

shinyApp(ui, server)
