# ============================================================
# 03-identificacion.R
# Identificacion de modelos ARMA mediante ACF, PACF y EACF
# Equipo ST5NEW
# ============================================================


# ------------------------------------------------------------
# ACF muestral
# ------------------------------------------------------------

acf_muestral <- function(z, lag_max = 20) {
  
  z <- as.numeric(z)
  
  if (anyNA(z) || any(!is.finite(z))) {
    stop("La serie contiene valores faltantes o no finitos.")
  }
  
  if (length(z) <= lag_max) {
    stop("La serie debe tener mas observaciones que lag_max.")
  }
  
  resultado <- stats::acf(z, lag.max = lag_max, plot = FALSE)
  
  list(
    valores = as.numeric(resultado$acf)[-1],
    rezagos = as.numeric(resultado$lag)[-1],
    limite = 1.96 / sqrt(length(z)),
    n = length(z)
  )
}


# ------------------------------------------------------------
# PACF muestral
# ------------------------------------------------------------

pacf_muestral <- function(z, lag_max = 20) {
  
  z <- as.numeric(z)
  
  if (anyNA(z) || any(!is.finite(z))) {
    stop("La serie contiene valores faltantes o no finitos.")
  }
  
  resultado <- stats::pacf(z, lag.max = lag_max, plot = FALSE)
  
  list(
    valores = as.numeric(resultado$acf),
    rezagos = as.numeric(resultado$lag),
    limite = 1.96 / sqrt(length(z)),
    n = length(z)
  )
}


# ------------------------------------------------------------
# Graficos ACF
# ------------------------------------------------------------

graficar_acf <- function(z, lag_max = 20,
                         titulo = "ACF") {
  
  z <- as.numeric(z)
  
  stats::acf(
    z,
    lag.max = lag_max,
    main = titulo
  )
}


# ------------------------------------------------------------
# Graficos PACF
# ------------------------------------------------------------

graficar_pacf <- function(z, lag_max = 20,
                          titulo = "PACF") {
  
  z <- as.numeric(z)
  
  stats::pacf(
    z,
    lag.max = lag_max,
    main = titulo
  )
}


# ------------------------------------------------------------
# EACF
# Exploracion mediante paquete TSA
# ------------------------------------------------------------

eacf_muestral <- function(z) {
  
  z <- as.numeric(z)
  
  if (anyNA(z) || any(!is.finite(z))) {
    stop("La serie contiene valores faltantes o no finitos.")
  }
  
  if (!requireNamespace("TSA", quietly = TRUE)) {
    stop("Se requiere instalar el paquete TSA.")
  }
  
  TSA::eacf(z)
}


# ------------------------------------------------------------
# Resumen numerico de ACF y PACF
# ------------------------------------------------------------

resumen_identificacion <- function(z, lag_max = 20) {
  
  resultado_acf <- acf_muestral(z, lag_max)
  resultado_pacf <- pacf_muestral(z, lag_max)
  
  tabla_acf <- data.frame(
    rezago = resultado_acf$rezagos,
    ACF = resultado_acf$valores,
    significativo =
      abs(resultado_acf$valores) > resultado_acf$limite
  )
  
  tabla_pacf <- data.frame(
    rezago = resultado_pacf$rezagos,
    PACF = resultado_pacf$valores,
    significativo =
      abs(resultado_pacf$valores) > resultado_pacf$limite
  )
  
  list(
    acf = tabla_acf,
    pacf = tabla_pacf,
    limite = resultado_acf$limite,
    n = resultado_acf$n
  )
}


# ------------------------------------------------------------
# Identificacion general
# ------------------------------------------------------------

identificar_modelos <- function(z, lag_max = 20) {
  
  resumen <- resumen_identificacion(z, lag_max)
  
  list(
    resumen = resumen,
    eacf = if (requireNamespace("TSA", quietly = TRUE))
      eacf_muestral(z) else NULL
  )
}