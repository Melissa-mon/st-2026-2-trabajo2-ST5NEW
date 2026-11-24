
# ============================================================
# TRABAJO 2 - EQUIPO ST5NEW
# Archivo: 01-transformacion.R
# ESCALA Y ESTABILIZACION DE LA VARIANZA
# ============================================================


# ------------------------------------------------------------
# 1. TRANSFORMACION DE BOX-COX
# ------------------------------------------------------------

boxcox_tr <- function(z, lambda) {
  
  if (any(!is.finite(z)) || any(z <= 0)) {
    stop("Box-Cox requiere valores positivos y finitos.")
  }
  
  if (length(lambda) != 1 || !is.finite(lambda)) {
    stop("Lambda debe ser un numero finito.")
  }
  
  if (abs(lambda) < 1e-8) {
    return(log(z))
  }
  
  return((z^lambda - 1) / lambda)
}


# ------------------------------------------------------------
# 2. TRANSFORMACION INVERSA DE BOX-COX
# ------------------------------------------------------------

boxcox_inv <- function(w, lambda) {
  
  if (length(lambda) != 1 || !is.finite(lambda)) {
    stop("Lambda debe ser un numero finito.")
  }
  
  if (abs(lambda) < 1e-8) {
    return(exp(w))
  }
  
  base <- 1 + lambda * w
  
  if (any(!is.finite(base)) || any(base <= 0)) {
    stop("La transformacion inversa no esta definida.")
  }
  
  return(base^(1 / lambda))
}


# ------------------------------------------------------------
# 3. GRAFICO TEMPORAL DE LA SERIE
# ------------------------------------------------------------

grafico_serie <- function(datos, nombre, unidad, archivo) {
  
  if (!dir.exists("figs")) {
    dir.create("figs")
  }
  
  grDevices::png(
    filename = archivo,
    width = 1600,
    height = 900,
    res = 140
  )
  
  on.exit(grDevices::dev.off(), add = TRUE)
  
  graphics::plot(
    datos$fecha,
    datos$valor,
    type = "l",
    main = nombre,
    xlab = "Fecha",
    ylab = paste("Valor (", unidad, ")"),
    xaxt = "n",
    lwd = 1.5
  )
  
  fechas <- pretty(as.numeric(datos$fecha), n = 8)
  
  graphics::axis(
    side = 1,
    at = fechas,
    labels = format(
      as.Date(fechas, origin = "1970-01-01"),
      "%Y-%m"
    ),
    las = 2,
    cex.axis = 0.8
  )
  
  graphics::mtext(
    paste("Numero de observaciones:", nrow(datos)),
    side = 3,
    line = 0.3,
    adj = 1
  )
}


# ------------------------------------------------------------
# 4. ESTADISTICAS POR BLOQUES IGUALES
# ------------------------------------------------------------

bloques_escala <- function(z, n_bloques = 8) {
  
  z <- as.numeric(z)
  
  if (any(!is.finite(z))) {
    stop("La serie contiene valores no finitos.")
  }
  
  if (n_bloques < 2 ||
      length(z) %% n_bloques != 0) {
    stop("Se requieren bloques consecutivos de igual tamano.")
  }
  
  tamano <- length(z) / n_bloques
  
  medias <- numeric(n_bloques)
  sd_diferencias <- numeric(n_bloques)
  
  for (i in seq_len(n_bloques)) {
    
    inicio <- (i - 1) * tamano + 1
    final <- i * tamano
    
    bloque <- z[inicio:final]
    
    medias[i] <- mean(bloque)
    
    # Desviacion estandar de la primera diferencia
    sd_diferencias[i] <- stats::sd(diff(bloque))
  }
  
  if (any(!is.finite(sd_diferencias)) ||
      any(sd_diferencias <= 0)) {
    stop("Hay desviaciones estandar no validas.")
  }
  
  # Comprobar si todos los niveles son positivos
  log_log_aplicable <- all(medias > 0)
  
  if (log_log_aplicable) {
    
    # Modelo log-log
    modelo <- stats::lm(
      log(sd_diferencias) ~ log(medias)
    )
    
    pendiente <- unname(coef(modelo)[2])
    
    lambda_aproximado <- 1 - pendiente
    
  } else {
    
    # Modelo descriptivo alternativo para niveles negativos
    # No se interpreta su pendiente como lambda.
    
    modelo <- stats::lm(
      log(sd_diferencias) ~ medias
    )
    
    pendiente <- unname(coef(modelo)[2])
    
    lambda_aproximado <- NA_real_
  }
  
  tabla <- data.frame(
    bloque = seq_len(n_bloques),
    media = medias,
    sd_primera_diferencia = sd_diferencias
  )
  
  return(list(
    tabla = tabla,
    modelo = modelo,
    pendiente = pendiente,
    lambda_aproximado = lambda_aproximado,
    log_log_aplicable = log_log_aplicable,
    r2 = summary(modelo)$r.squared
  ))
}


# ------------------------------------------------------------
# 5. GRAFICO DE DISPERSION POR BLOQUES
# ------------------------------------------------------------

grafico_escala <- function(resultado, nombre, archivo) {
  
  tabla <- resultado$tabla
  
  grDevices::png(
    filename = archivo,
    width = 1400,
    height = 900,
    res = 140
  )
  
  on.exit(grDevices::dev.off(), add = TRUE)
  
  if (resultado$log_log_aplicable) {
    
    # Grafico log-log convencional
    
    graphics::plot(
      tabla$media,
      tabla$sd_primera_diferencia,
      log = "xy",
      pch = 19,
      xlab = "Nivel medio por bloque",
      ylab = "Desviacion estandar de la primera diferencia",
      main = paste("Relacion escala-varianza:", nombre)
    )
    
    x_ajuste <- exp(seq(
      log(min(tabla$media)),
      log(max(tabla$media)),
      length.out = 100
    ))
    
    y_ajuste <- exp(coef(resultado$modelo)[1]) *
      x_ajuste^coef(resultado$modelo)[2]
    
    texto_lambda <- paste(
      "Lambda aproximado:",
      round(resultado$lambda_aproximado, 4)
    )
    
  } else {
    
    # Grafico alternativo para niveles negativos o nulos.
    # Se conserva la escala original del nivel.
    
    graphics::plot(
      tabla$media,
      tabla$sd_primera_diferencia,
      log = "y",
      pch = 19,
      xlab = "Nivel medio por bloque (escala original)",
      ylab = "Desviacion estandar de la primera diferencia",
      main = paste("Relacion escala-varianza:", nombre)
    )
    
    x_ajuste <- seq(
      min(tabla$media),
      max(tabla$media),
      length.out = 100
    )
    
    y_ajuste <- exp(
      coef(resultado$modelo)[1] +
        coef(resultado$modelo)[2] * x_ajuste
    )
    
    texto_lambda <- "Lambda no estimable mediante pendiente log-log"
  }
  
  graphics::lines(
    x_ajuste,
    y_ajuste,
    lwd = 2,
    lty = 2
  )
  
  graphics::legend(
    "topleft",
    legend = c(
      paste("Pendiente:", round(resultado$pendiente, 4)),
      texto_lambda,
      paste("R2:", round(resultado$r2, 4))
    ),
    bty = "n"
  )
}


# ------------------------------------------------------------
# 6. PERFIL DE LOG-VEROSIMILITUD DE BOX-COX
# ------------------------------------------------------------

perfil_lambda <- function(
    z,
    nombre,
    orden,
    tipo_determinista,
    lambda_grid = seq(-2, 2, by = 0.01)) {
  
  z <- as.numeric(z)
  
  if (any(!is.finite(z)) || any(z <= 0)) {
    stop("Box-Cox no se aplica a valores no positivos.")
  }
  
  if (length(orden) != 3 || orden[2] != 0) {
    stop("El perfil requiere un modelo ARMA sin diferencias.")
  }
  
  tipo_determinista <- match.arg(
    tipo_determinista,
    c("ninguna", "constante", "constante_tendencia")
  )
  
  if (length(lambda_grid) < 3 ||
      any(!is.finite(lambda_grid)) ||
      is.unsorted(lambda_grid, strictly = TRUE)) {
    stop("La grilla de lambda debe ser creciente y valida.")
  }
  
  n <- length(z)
  tiempo <- seq_len(n)
  
  # Construir componente determinista
  
  if (tipo_determinista == "ninguna") {
    
    xreg <- NULL
    
  } else if (tipo_determinista == "constante") {
    
    xreg <- matrix(
      1,
      nrow = n,
      ncol = 1,
      dimnames = list(NULL, "constante")
    )
    
  } else {
    
    xreg <- cbind(
      constante = rep(1, n),
      tendencia = tiempo
    )
  }
  
  log_verosimilitud <- rep(
    NA_real_,
    length(lambda_grid)
  )
  
  for (i in seq_along(lambda_grid)) {
    
    lambda <- lambda_grid[i]
    
    z_transformada <- boxcox_tr(z, lambda)
    
    ajuste <- tryCatch(
      suppressWarnings(
        stats::arima(
          z_transformada,
          order = orden,
          xreg = xreg,
          include.mean = FALSE,
          method = "ML"
        )
      ),
      error = function(e) NULL
    )
    
    if (!is.null(ajuste)) {
      
      loglik <- as.numeric(
        stats::logLik(ajuste)
      )
      
      jacobiano <- (lambda - 1) * sum(log(z))
      
      log_verosimilitud[i] <- loglik + jacobiano
    }
  }
  
  if (all(is.na(log_verosimilitud))) {
    stop("No fue posible estimar el perfil de lambda.")
  }
  
  # Lambda que maximiza el perfil
  
  posicion_maximo <- which.max(log_verosimilitud)
  
  lambda_hat <- lambda_grid[posicion_maximo]
  
  maximo <- log_verosimilitud[posicion_maximo]
  
  # Intervalo de confianza del 95 %
  
  limite <- maximo -
    stats::qchisq(0.95, df = 1) / 2
  
  aceptados <- which(
    log_verosimilitud >= limite
  )
  
  izquierda <- max(
    aceptados[aceptados <= posicion_maximo]
  )
  
  derecha <- min(
    aceptados[aceptados >= posicion_maximo]
  )
  
  ic_inferior <- lambda_grid[izquierda]
  ic_superior <- lambda_grid[derecha]
  
  intervalo_abierto <- (
    izquierda == 1 ||
      derecha == length(lambda_grid)
  )
  
  tabla_perfil <- data.frame(
    lambda = lambda_grid,
    log_verosimilitud = log_verosimilitud
  )
  
  # Generar grafico
  
  if (!dir.exists("figs")) {
    dir.create("figs")
  }
  
  nombre_archivo <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(nombre)
  )
  
  archivo <- paste0(
    "figs/perfil_lambda_",
    nombre_archivo,
    ".png"
  )
  
  grDevices::png(
    filename = archivo,
    width = 1400,
    height = 900,
    res = 140
  )
  
  on.exit(grDevices::dev.off(), add = TRUE)
  
  graphics::plot(
    lambda_grid,
    log_verosimilitud,
    type = "l",
    lwd = 2,
    xlab = expression(lambda),
    ylab = "Log-verosimilitud con jacobiano",
    main = paste("Perfil Box-Cox:", nombre)
  )
  
  graphics::abline(
    h = limite,
    lty = 2,
    col = "red"
  )
  
  graphics::abline(
    v = lambda_hat,
    lty = 2,
    col = "blue"
  )
  
  return(list(
    lambda_hat = lambda_hat,
    intervalo_95 = c(
      inferior = ic_inferior,
      superior = ic_superior
    ),
    intervalo_abierto = intervalo_abierto,
    log_verosimilitud_maxima = maximo,
    limite_intervalo = limite,
    tabla = tabla_perfil,
    modelo = paste(
      "ARMA",
      paste(orden[c(1, 3)], collapse = ","),
      "con componente determinista:",
      tipo_determinista
    )
  ))
}


# ============================================================
# FIN DEL ARCHIVO 01-TRANSFORMACION.R
# ============================================================
