
# ============================================================
# TRABAJO 2 - EQUIPO ST5NEW
# Archivo: 02-integracion.R
# Seccion 4: Orden de integracion
#
# Funciones:
#   diferencias_serie()
#   resumen_diferencias()
#   seleccionar_rezagos_adf()
#   adf_propio()
#   verificar_adf_urca()
#   adf_secuencial()
#
# Implementacion de ADF en R base.
# Contraste: urca::ur.df()
# ============================================================


# ------------------------------------------------------------
# 1. CONSTRUIR NIVELES Y DIFERENCIAS
# ------------------------------------------------------------

diferencias_serie <- function(z) {
  
  if (!is.numeric(z) || !is.vector(z)) {
    stop("z debe ser un vector numerico.")
  }
  
  if (anyNA(z) || any(!is.finite(z))) {
    stop("La serie contiene valores faltantes o no finitos.")
  }
  
  if (length(z) < 4) {
    stop("La serie debe tener al menos cuatro observaciones.")
  }
  
  list(
    d0 = as.numeric(z),
    d1 = as.numeric(diff(z, differences = 1)),
    d2 = as.numeric(diff(z, differences = 2))
  )
}


# ------------------------------------------------------------
# 2. VARIANZA, AUTOCORRELACION Y ACF
# ------------------------------------------------------------

resumen_diferencias <- function(z, lag_max = NULL) {
  
  diferencias <- diferencias_serie(z)
  
  if (is.null(lag_max)) {
    lag_max <- min(24, floor(length(z) / 4))
  }
  
  if (length(lag_max) != 1 ||
      !is.numeric(lag_max) ||
      lag_max < 1 ||
      lag_max != as.integer(lag_max)) {
    stop("lag_max debe ser un entero positivo.")
  }
  
  series <- diferencias
  
  resumen <- do.call(
    rbind,
    lapply(names(series), function(nombre) {
      
      x <- series[[nombre]]
      
      acf_1 <- as.numeric(
        stats::acf(
          x,
          lag.max = 1,
          plot = FALSE,
          demean = TRUE
        )$acf[2]
      )
      
      data.frame(
        serie = nombre,
        T = length(x),
        varianza = stats::var(x),
        r1 = acf_1
      )
    })
  )
  
  acf_lista <- lapply(names(series), function(nombre) {
    
    x <- series[[nombre]]
    
    maximo <- min(lag_max, length(x) - 1)
    
    resultado <- stats::acf(
      x,
      lag.max = maximo,
      plot = FALSE,
      demean = TRUE
    )
    
    data.frame(
      serie = nombre,
      rezago = as.numeric(resultado$lag),
      acf = as.numeric(resultado$acf)
    )
  })
  
  names(acf_lista) <- names(series)
  
  list(
    resumen = resumen,
    acf = acf_lista,
    diferencias = series
  )
}


# ------------------------------------------------------------
# 3. CONSTRUIR LA REGRESION ADF
# ------------------------------------------------------------

construir_regresion_adf <- function(y,
                                    tipo = c("none", "drift", "trend"),
                                    rezagos = 0,
                                    muestra_comun = FALSE,
                                    max_rezagos = rezagos) {
  
  tipo <- match.arg(tipo)
  
  if (!is.numeric(y) || !is.vector(y)) {
    stop("y debe ser un vector numerico.")
  }
  
  if (anyNA(y) || any(!is.finite(y))) {
    stop("y contiene valores faltantes o no finitos.")
  }
  
  n <- length(y)
  
  if (length(rezagos) != 1 ||
      rezagos < 0 ||
      rezagos != as.integer(rezagos)) {
    stop("rezagos debe ser un entero no negativo.")
  }
  
  if (muestra_comun) {
    inicio <- max_rezagos + 2
  } else {
    inicio <- rezagos + 2
  }
  
  if (inicio > n) {
    stop("No hay suficientes observaciones para esta regresion.")
  }
  
  dy <- diff(y)
  
  t <- seq.int(inicio, n)
  
  datos <- data.frame(
    dy = dy[t - 1],
    nivel_rezagado = y[t - 1],
    tendencia = t
  )
  
  if (tipo == "drift" || tipo == "trend") {
    datos$constante <- 1
  }
  
  if (tipo == "trend") {
    datos$tendencia <- t
  }
  
  if (rezagos > 0) {
    
    for (j in seq_len(rezagos)) {
      datos[[paste0("dif_rezagada_", j)]] <- dy[t - j - 1]
    }
  }
  
  regresores <- character(0)
  
  if (tipo == "drift" || tipo == "trend") {
    regresores <- c(regresores, "constante")
  }
  
  if (tipo == "trend") {
    regresores <- c(regresores, "tendencia")
  }
  
  regresores <- c(regresores, "nivel_rezagado")
  
  if (rezagos > 0) {
    regresores <- c(
      regresores,
      paste0("dif_rezagada_", seq_len(rezagos))
    )
  }
  
  formula_adf <- stats::reformulate(
    regresores,
    response = "dy",
    intercept = FALSE
  )
  
  ajuste <- stats::lm(
    formula_adf,
    data = datos
  )
  
  list(
    modelo = ajuste,
    datos = datos,
    formula = formula_adf,
    tipo = tipo,
    rezagos = rezagos,
    n = nrow(datos)
  )
}


# ------------------------------------------------------------
# 4. SELECCION PROPIA DE REZAGOS ADF
#    Criterio AIC o BIC.
#    Todos los candidatos usan la misma muestra.
# ------------------------------------------------------------

seleccionar_rezagos_adf <- function(y,
                                    tipo = c("none", "drift", "trend"),
                                    max_rezagos = 8,
                                    criterio = c("BIC", "AIC")) {
  
  tipo <- match.arg(tipo)
  criterio <- match.arg(criterio)
  
  if (length(max_rezagos) != 1 ||
      max_rezagos < 0 ||
      max_rezagos != as.integer(max_rezagos)) {
    stop("max_rezagos debe ser un entero no negativo.")
  }
  
  if (max_rezagos + 5 >= length(y)) {
    stop("max_rezagos es demasiado grande para el tamano de la serie.")
  }
  
  resultados <- vector(
    "list",
    length = max_rezagos + 1
  )
  
  for (k in 0:max_rezagos) {
    
    ajuste <- construir_regresion_adf(
      y = y,
      tipo = tipo,
      rezagos = k,
      muestra_comun = TRUE,
      max_rezagos = max_rezagos
    )
    
    rss <- sum(stats::residuals(ajuste$modelo)^2)
    n <- ajuste$n
    parametros <- length(stats::coef(ajuste$modelo))
    
    if (rss <= 0 || !is.finite(rss)) {
      stop("No fue posible calcular el criterio de informacion.")
    }
    
    aic <- n * log(rss / n) + 2 * parametros
    bic <- n * log(rss / n) + log(n) * parametros
    
    resultados[[k + 1]] <- data.frame(
      rezagos = k,
      n = n,
      parametros = parametros,
      RSS = rss,
      AIC = aic,
      BIC = bic
    )
  }
  
  tabla <- do.call(rbind, resultados)
  
  columna <- if (criterio == "AIC") "AIC" else "BIC"
  
  elegido <- tabla$rezagos[
    which.min(tabla[[columna]])
  ]
  
  list(
    tabla = tabla,
    rezagos = elegido,
    criterio = criterio,
    tipo = tipo
  )
}


# ------------------------------------------------------------
# 5. PRUEBA ADF PROPIA
#    Regresion aumentada de Dickey y Fuller.
# ------------------------------------------------------------

adf_propio <- function(y,
                       tipo = c("none", "drift", "trend"),
                       rezagos = 0) {
  
  tipo <- match.arg(tipo)
  
  ajuste <- construir_regresion_adf(
    y = y,
    tipo = tipo,
    rezagos = rezagos,
    muestra_comun = FALSE
  )
  
  modelo <- ajuste$modelo
  
  coeficientes <- summary(modelo)$coefficients
  
  fila <- which(
    rownames(coeficientes) == "nivel_rezagado"
  )
  
  if (length(fila) != 1) {
    stop("No se pudo identificar el coeficiente del nivel rezagado.")
  }
  
  estadistico <- coeficientes[fila, "t value"]
  
  list(
    tipo = tipo,
    rezagos = rezagos,
    n = ajuste$n,
    modelo = modelo,
    coeficientes = coeficientes,
    estadistico = as.numeric(estadistico),
    p_value = NA_real_,
    hipotesis_nula = "El coeficiente del nivel rezagado es cero.",
    hipotesis_alternativa = "El coeficiente del nivel rezagado es negativo.",
    advertencia = paste(
      "El estadistico ADF no sigue la distribucion t usual.",
      "La decision requiere valores criticos ADF de la",
      "especificacion determinista correspondiente."
    )
  )
}


# ------------------------------------------------------------
# 6. VERIFICACION CONTRA URCA::UR.DF
#    Mismos datos, especificacion y numero de rezagos.
# ------------------------------------------------------------

verificar_adf_urca <- function(y,
                               tipo = c("none", "drift", "trend"),
                               rezagos = 0,
                               tolerancia = 1e-6) {
  
  tipo <- match.arg(tipo)
  
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop(
      "Se requiere instalar el paquete urca para la verificacion: ",
      "install.packages('urca')"
    )
  }
  
  propio <- adf_propio(
    y = y,
    tipo = tipo,
    rezagos = rezagos
  )
  
  paquete <- urca::ur.df(
    y = y,
    type = tipo,
    lags = rezagos,
    selectlags = "Fixed"
  )
  
  estadistico_paquete <- as.numeric(
    paquete@teststat[1, 1]
  )
  
  valores_criticos <- paquete@cval
  
  diferencia <- abs(
    propio$estadistico - estadistico_paquete
  )
  
  comparacion <- data.frame(
    implementacion = c("R base propia", "urca::ur.df"),
    estadistico = c(
      propio$estadistico,
      estadistico_paquete
    )
  )
  
  list(
    comparacion = comparacion,
    diferencia_absoluta = diferencia,
    coincide = diferencia < tolerancia,
    valores_criticos = valores_criticos,
    objeto_urca = paquete,
    especificacion = tipo,
    rezagos = rezagos
  )
}


# ------------------------------------------------------------
# 7. ANALISIS ADF SECUENCIAL
#
# Se evalua la serie transformada, su primera diferencia
# y su segunda diferencia.
#
# El tipo determinista se debe justificar con el grafico.
# No se decide automaticamente el orden de integracion.
# ------------------------------------------------------------

adf_secuencial <- function(y,
                           tipos = c("drift", "drift", "drift"),
                           max_rezagos = 8,
                           criterio = "BIC") {
  
  if (length(tipos) != 3) {
    stop(
      "Debe indicar tres especificaciones: niveles, primera y segunda diferencia."
    )
  }
  
  tipos <- vapply(
    tipos,
    match.arg,
    choices = c("none", "drift", "trend")
  )
  
  diferencias <- diferencias_serie(y)
  
  series <- list(
    niveles = diferencias$d0,
    primera_diferencia = diferencias$d1,
    segunda_diferencia = diferencias$d2
  )
  
  resultados <- vector("list", 3)
  names(resultados) <- names(series)
  
  for (i in seq_along(series)) {
    
    x <- series[[i]]
    
    seleccion <- seleccionar_rezagos_adf(
      y = x,
      tipo = tipos[i],
      max_rezagos = min(
        max_rezagos,
        max(0, floor(length(x) / 5))
      ),
      criterio = criterio
    )
    
    prueba <- adf_propio(
      y = x,
      tipo = tipos[i],
      rezagos = seleccion$rezagos
    )
    
    resultados[[i]] <- list(
      serie = names(series)[i],
      tipo = tipos[i],
      seleccion = seleccion,
      prueba = prueba
    )
  }
  
  resultados
}


# ------------------------------------------------------------
# 8. VERIFICACION DE LA IMPLEMENTACION PROPIA
# ------------------------------------------------------------

verificar_integracion <- function(y,
                                  tipo = "drift",
                                  max_rezagos = 8,
                                  criterio = "BIC") {
  
  seleccion <- seleccionar_rezagos_adf(
    y = y,
    tipo = tipo,
    max_rezagos = max_rezagos,
    criterio = criterio
  )
  
  contraste <- verificar_adf_urca(
    y = y,
    tipo = tipo,
    rezagos = seleccion$rezagos
  )
  
  list(
    seleccion_rezagos = seleccion,
    contraste_urca = contraste
  )
}


# ============================================================
# FIN DEL ARCHIVO 02-integracion.R
# ============================================================

