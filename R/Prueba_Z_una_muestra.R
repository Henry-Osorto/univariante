#' Prueba.Z.para.una.muestra

#'@export

Prueba.Z.una.muestra <- function(x, Mu, S, n, alpha) {

  Z <- (x - Mu)/(S/sqrt(n))

  data <- data.frame(`Media muestral` = x,
                     `Media Poblacinal` = Mu,
                     `Desviación Estándar` = S,
                     n = n,
                     `Nivel de significancia` = alpha)


  Cola.Inferior <- data.frame(H1 = 'x < M',
                              H0 = 'x >= M',
                              Z = Z,
                              `Valor Crítico Inferior` = qnorm(alpha),
                              `Valor Crítico Superior` = NA,
                              `P-valor` = pnorm(Z),
                              Conclusión = ifelse(pnorm(Z)<alpha, 'Rechazar Ho', 'No rechazar Ho'))

    Dos.Colas <- data.frame(H1 = 'x ! M',
                            H0 = 'x = M',Z = Z,
                            `Valor Crítico Inferior` = qnorm(alpha/2),
                            `Valor Crítico Superior` = qnorm(1- alpha/2),
                            `P-valor` = 2*(pnorm(Z)),
                            Conclusión = ifelse(2*(pnorm(Z))<alpha/2, 'Rechazar Ho', 'No rechazar Ho'))

  Cola.Superior <- data.frame(H1 = 'x > M',
                              H0 = 'x <= M',
                              Z = Z,
                              `Valor Crítico Inferior` = NA,
                              `Valor Crítico Superior` = qnorm(1-alpha),
                              `P-valor` = 1-pnorm(Z),
                              Conclusión = ifelse(1-pnorm(Z)<alpha, 'Rechazar Ho', 'No rechazar Ho'))


  df <- rbind(Cola.Inferior, Dos.Colas, Cola.Superior)

  df <- data.frame(df)

  names(df) <- c('H1', 'H0', 'Z Calculado', 'Valor Crítico Inferior',
                 'Valor Crítico Superior', 'P_valor', 'Conclusión' )

  df[,3:6] <- lapply(df[,3:6], round, 4)

  return(df)
}

