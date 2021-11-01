library(dplyr)
library(pwr)
alcohol<-c('0','1-9','10-44','45 o mas')
cancer_oral <- c(43,89,109,242)
controles <- c(108,141,91,107)
incidencia_A <- data.frame(alcohol,cancer_oral,controles)
tabaco<-c('0','1-9','20-39','40 o mas')
cancer_oral <- c(26,66,248,143)
controles <- c(85,97,197,68)
incidencia_T <- data.frame(alcohol,cancer_oral,controles)

#PREGUNTA 1
#H0: p = 0.6
#HA: p!= 0.6
#Supuesto: normalidad para la proporción
casos <- incidencia_A[3,2]
controles <- incidencia_A[3,3]
valor_nulo <- 0.6
suma <- casos+controles
p_exito <- casos/suma
alfa=0.01
error_est <- sqrt((p_exito * (1 - p_exito)) / suma)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
print(inferior)
print(superior)
# Prueba de hipótesis.
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo)) / suma)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa unilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)
#Resultado: Con los valores obtenidos, 
#se determina que no es posible rechazar la hipótesis nula, dado que
# p>alfa, por lo que los datos respaldan la estimacion estipulada.

#PREGUNTA 2
#Supuesto 1: Por cada rango de edad, este se considera una poblacion
#Supuesto 2: Normalidad para la proporción
#pa: proporción de 10-44 de alcohol por día.
#pa2: proporción de 45-50 de alcohol por día.
#H0: pa-pa2=0
#HA: pa-pa2!=0
casos2 <- incidencia_A[4,2]
controles2 <- incidencia_A[4,3]
valor_nulo2 <- 0
suma2 <- casos2+controles2
p_exito2 <- casos2/suma2
resta <- p_exito-p_exito2
error1 <- (p_exito2 * (1 - p_exito2)) / suma2
error2 <- (p_exito* (1 - p_exito)) / suma
error_est <- sqrt(error1 + error2)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- resta - Z_critico * error_est
superior <- resta + Z_critico * error_est
p_agrupada <- (p_exito2 + p_exito) / (suma2 + suma) 
error1 <- (p_agrupada * (1 - p_agrupada)) / suma2
error2 <- (p_agrupada * (1 - p_agrupada)) / suma
error_est_hip <- sqrt(error1 + error2)
Z2 <- (resta - valor_nulo) / error_est_hip 
p2 <- 2 * pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa unilateral\n")
cat("Z =", Z2, "\n")
cat("p =", p2)
#Resultado: Con los valores obtenidos, 
#se determina que no es posible rechazar la hipótesis nula, dado que
# p>alfa, por lo que los datos respaldan la estimacion estipulada
#con un 99% de confianza.

#PREGUNTA 3
#Supuesto: Por cada rango de edad, este se considera una poblacion 
# y de igual cantidad.
resultado <- pwr.2p.test(h=0.20,n=NULL, sig.level=0.01, power=0.85, 
                         alternative="two.sided")
print(ceiling(resultado$n))
#la cantidad de personas por rango de edad debe ser 653.
