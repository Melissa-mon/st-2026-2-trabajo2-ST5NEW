# ============================================================
# 04-modelos.R
# Estimacion y comparacion de modelos ARIMA
# Equipo ST5NEW
# ============================================================


# ------------------------------------------------------------
# Estimar un modelo ARIMA
# ------------------------------------------------------------

estimar_arima <- function(z, p = 0, d = 1, q = 0,
                          drift = FALSE) {
  
  z <- as.numeric(z)
  
  if (anyNA(z) || any(!is.finite(z))) {
    stop("La serie contiene valores faltantes o no finitos.")
  }
  
  if (p < 0 || d < 0 || q < 0) {
    stop("Los ordenes ARIMA deben ser no negativos.")
  }
  
  if (drift && d != 1) {
    stop("La deriva se implementa para modelos con d = 1.")
  }
  
  if (drift) {
    
    tendencia <- seq_along(z)
    
    modelo <- stats::arima(
      z,
      order = c(p, d, q),
      xreg = tendencia,
      method = "ML"
    )
    
  } else {
    
    modelo <- stats::arima(
      z,
      order = c(p, d, q),
      method = "ML"
    )
  }
  
  modelo
}


# ------------------------------------------------------------
# Extraer criterios de informacion
# ------------------------------------------------------------

criterios_arima <- function(modelo) {
  
  list(
    AIC = stats::AIC(modelo),
    BIC = stats::BIC(modelo)
  )
}


# ------------------------------------------------------------
# Comparar modelos ARIMA candidatos
# ------------------------------------------------------------

comparar_arima <- function(z, candidatos, d = 1,
                           incluir_drift = TRUE) {
  
  combinaciones <- candidatos
  
  if (incluir_drift && d == 1) {
    
    combinaciones <- merge(
      candidatos,
      data.frame(drift = c(FALSE, TRUE)),
      by = NULL
    )
    
  } else {
    
    combinaciones$drift <- FALSE
  }
  
  resultados <- vector("list", nrow(combinaciones))
  
  for (i in seq_len(nrow(combinaciones))) {
    
    p <- combinaciones$p[i]
    q <- combinaciones$q[i]
    drift <- combinaciones$drift[i]
    
    modelo <- estimar_arima(
      z,
      p = p,
      d = d,
      q = q,
      drift = drift
    )
    
    resultados[[i]] <- data.frame(
      p = p,
      d = d,
      q = q,
      drift = drift,
      AIC = stats::AIC(modelo),
      BIC = stats::BIC(modelo)
    )
  }
  
  tabla <- do.call(rbind, resultados)
  
  tabla <- tabla[order(tabla$BIC), ]
  
  rownames(tabla) <- NULL
  
  tabla
}


# ------------------------------------------------------------
# Generar candidatos ARMA de orden bajo
# ------------------------------------------------------------

generar_candidatos <- function(max_p = 3, max_q = 3) {
  
  expand.grid(
    p = 0:max_p,
    q = 0:max_q
  )
}


# ------------------------------------------------------------
# Obtener residuos de un modelo
# ------------------------------------------------------------

residuos_arima <- function(modelo) {
  
  residuos <- as.numeric(stats::residuals(modelo))
  
  residuos
}


# ------------------------------------------------------------
# Ajustar y comparar candidatos
# ------------------------------------------------------------

ajustar_candidatos <- function(z, d = 1,
                               max_p = 3,
                               max_q = 3,
                               incluir_drift = TRUE) {
  
  candidatos <- generar_candidatos(
    max_p = max_p,
    max_q = max_q
  )
  
  comparar_arima(
    z,
    candidatos = candidatos,
    d = d,
    incluir_drift = incluir_drift
  )
}