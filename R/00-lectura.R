
# ============================================================
# Trabajo 2 - Equipo ST5NEW
# Archivo: 00-lectura.R
# Lectura y verificación de las tres series
# ============================================================

leer_serie <- function(ruta, nombre, unidad, frecuencia,
                       T_esperado) {
  
  # 1. Verificar existencia del archivo
  if (!file.exists(ruta)) {
    stop("No se encuentra el archivo: ", ruta)
  }
  
  # 2. Leer los datos originales
  datos <- read.csv(
    ruta,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # 3. Verificar columnas
  if (!all(c("fecha", "valor") %in% names(datos))) {
    stop("El archivo debe contener las columnas fecha y valor.")
  }
  
  datos <- datos[, c("fecha", "valor")]
  
  # 4. Convertir y verificar tipos
  datos$fecha <- as.Date(datos$fecha)
  datos$valor <- as.numeric(datos$valor)
  
  if (anyNA(datos$fecha) || anyNA(datos$valor)) {
    stop("Hay fechas inválidas o valores faltantes.")
  }
  
  # Verificar valores numéricos finitos
  if (any(!is.finite(datos$valor))) {
    stop("La serie contiene valores no finitos.")
  }
  
  # 5. Verificar número de observaciones
  if (nrow(datos) != T_esperado) {
    stop("El número de observaciones no coincide con el esperado.")
  }
  
  # 6. Verificar orden cronológico y fechas únicas
  if (anyDuplicated(datos$fecha) > 0 ||
      is.unsorted(datos$fecha)) {
    stop("Las fechas deben ser únicas y estar ordenadas.")
  }
  
  # 7. Verificar frecuencia semanal
  if (frecuencia == "semanal") {
    if (any(diff(datos$fecha) != 7)) {
      stop("La serie no tiene frecuencia semanal regular.")
    }
  }
  
  # 8. Verificar frecuencia anual
  if (frecuencia == "anual") {
    anios <- as.integer(format(datos$fecha, "%Y"))
    
    if (any(diff(anios) != 1) ||
        any(format(datos$fecha, "%m-%d") !=
            format(datos$fecha[1], "%m-%d"))) {
      stop("La serie no tiene frecuencia anual regular.")
    }
  }
  
  # 9. Verificar frecuencia mensual
  if (frecuencia == "mensual") {
    meses <- as.integer(format(datos$fecha, "%Y")) * 12 +
      as.integer(format(datos$fecha, "%m"))
    
    if (any(diff(meses) != 1) ||
        any(as.integer(format(datos$fecha, "%d")) != 1)) {
      stop("La serie no tiene frecuencia mensual regular.")
    }
  }
  
  # 10. Construir ficha descriptiva
  ficha <- data.frame(
    nombre = nombre,
    unidad = unidad,
    frecuencia = frecuencia,
    fecha_inicial = min(datos$fecha),
    fecha_final = max(datos$fecha),
    T = nrow(datos),
    minimo = min(datos$valor),
    maximo = max(datos$valor),
    media = mean(datos$valor),
    varianza = var(datos$valor)
  )
  
  # 11. Devolver datos y ficha
  return(list(
    datos = datos,
    ficha = ficha
  ))
}
