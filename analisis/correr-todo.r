

# ============================================================
# TRABAJO 2 - EQUIPO ST5NEW
# Archivo: correr-todo.R
# Ejecucion de lectura y analisis de escala
# ============================================================


# ------------------------------------------------------------
# 1. CARGAR FUNCIONES
# ------------------------------------------------------------

source("R/00-lectura.R")
source("R/01-transformacion.R")


# ------------------------------------------------------------
# 2. LEER LAS TRES SERIES
# ------------------------------------------------------------

serie1 <- leer_serie(
  ruta = "datos/ST5NEW-serie-1.csv",
  nombre = "Anomalia de temperatura del agua cruda",
  unidad = "Grados Celsius",
  frecuencia = "semanal",
  T_esperado = 208
)

serie2 <- leer_serie(
  ruta = "datos/ST5NEW-serie-2.csv",
  nombre = "Nivel medio anual de la laguna",
  unidad = "Metros sobre el nivel del mar",
  frecuencia = "anual",
  T_esperado = 88
)

serie3 <- leer_serie(
  ruta = "datos/ST5NEW-serie-3.csv",
  nombre = "Reclamos por facturacion",
  unidad = "Reclamos",
  frecuencia = "mensual",
  T_esperado = 144
)


# ------------------------------------------------------------
# 3. CONSOLIDAR FICHAS DESCRIPTIVAS
# ------------------------------------------------------------

fichas <- rbind(
  serie1$ficha,
  serie2$ficha,
  serie3$ficha
)

cat("\n========================================\n")
cat("FICHAS DESCRIPTIVAS\n")
cat("========================================\n")

print(fichas)


# ------------------------------------------------------------
# 4. ORGANIZAR LAS SERIES
# ------------------------------------------------------------

series_datos <- list(
  serie1 = serie1$datos,
  serie2 = serie2$datos,
  serie3 = serie3$datos
)

nombres <- c(
  "Anomalia de temperatura del agua cruda",
  "Nivel medio anual de la laguna",
  "Reclamos por facturacion"
)

unidades <- c(
  "Grados Celsius",
  "Metros sobre el nivel del mar",
  "Reclamos"
)


# ------------------------------------------------------------
# 5. CREAR CARPETA DE FIGURAS
# ------------------------------------------------------------

if (!dir.exists("figs")) {
  dir.create("figs")
}


# ------------------------------------------------------------
# 6. GRAFICOS TEMPORALES
# ------------------------------------------------------------

cat("\n========================================\n")
cat("GENERACION DE GRAFICOS TEMPORALES\n")
cat("========================================\n")

for (i in seq_along(series_datos)) {
  
  grafico_serie(
    datos = series_datos[[i]],
    nombre = nombres[i],
    unidad = unidades[i],
    archivo = paste0(
      "figs/serie", i, "_temporal.png"
    )
  )
  
}

cat("Graficos temporales generados.\n")


# ------------------------------------------------------------
# 7. ANALISIS DE ESCALA POR BLOQUES
# ------------------------------------------------------------

resultados_escala <- vector(
  "list",
  length(series_datos)
)

names(resultados_escala) <- names(series_datos)

for (i in seq_along(series_datos)) {
  
  resultados_escala[[i]] <- bloques_escala(
    z = series_datos[[i]]$valor,
    n_bloques = 8
  )
  
}


# ------------------------------------------------------------
# 8. GRAFICOS DE DISPERSION
# ------------------------------------------------------------

for (i in seq_along(series_datos)) {
  
  grafico_escala(
    resultado = resultados_escala[[i]],
    nombre = nombres[i],
    archivo = paste0(
      "figs/serie", i, "_escala.png"
    )
  )
  
}


# ------------------------------------------------------------
# 9. TABLAS DE LOS BLOQUES
# ------------------------------------------------------------

cat("\n========================================\n")
cat("ESTADISTICAS POR BLOQUES\n")
cat("========================================\n")

for (i in seq_along(resultados_escala)) {
  
  cat("\n", nombres[i], "\n")
  
  print(resultados_escala[[i]]$tabla)
  
}


# ------------------------------------------------------------
# 10. RESUMEN DE LA RELACION ESCALA-VARIANZA
# ------------------------------------------------------------

resumen_escala <- data.frame(
  
  serie = names(resultados_escala),
  
  log_log_aplicable = sapply(
    resultados_escala,
    function(x) x$log_log_aplicable
  ),
  
  pendiente = sapply(
    resultados_escala,
    function(x) x$pendiente
  ),
  
  lambda_aproximado = sapply(
    resultados_escala,
    function(x) x$lambda_aproximado
  ),
  
  R2 = sapply(
    resultados_escala,
    function(x) x$r2
  )
  
)

cat("\n========================================\n")
cat("RESUMEN DE ESCALA Y VARIANZA\n")
cat("========================================\n")

print(resumen_escala)


# ------------------------------------------------------------
# 11. FINALIZACION
# ------------------------------------------------------------


cat("\n========================================\n")
cat("LECTURA Y ANALISIS DE ESCALA COMPLETADOS\n")
cat("Equipo ST5NEW\n")
cat("========================================\n")
