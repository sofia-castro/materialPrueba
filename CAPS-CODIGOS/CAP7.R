#Metodo de Wald para una proporcion.
# Fijar valores conocidos
n<-150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7
# Construcción del intervalo de confianza.
error_est <- sqrt((p_exito * (1 - p_exito)) / n)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
cat("Intervalo de confianza = [", inferior, ", ", superior, "]\n", sep = "")

# Prueba de hipótesis.
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo)) / n)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa unilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

#Metodo de Wald para dos proporciones p1-p2
# Fijar valores conocidos
n_hombres <- 48
n_mujeres <- 42
exitos_hombres <- 26
exitos_mujeres <- 20
alfa <- 0.05
valor_nulo <- 0
# Calcular probabilidades de éxito.
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres
# Estimar la diferencia.
diferencia <- p_hombres - p_mujeres
# Construcción del intervalo de confianza.
error_hombres <- (p_hombres * (1 - p_hombres)) / n_hombres
error_mujeres <- (p_mujeres * (1 - p_mujeres)) / n_mujeres
error_est <- sqrt(error_hombres + error_mujeres)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat("Intervalo de confianza = [", inferior, ", ", superior, "]\n", sep = "")
# Prueba de hipótesis.
p_agrupada <- (exitos_hombres + exitos_mujeres) / (n_hombres + n_mujeres) 
error_hombres <- (p_agrupada * (1 - p_agrupada)) / n_hombres
error_mujeres <- (p_agrupada * (1 - p_agrupada)) / n_mujeres
error_est_hip <- sqrt(error_hombres + error_mujeres)
Z <- (diferencia - valor_nulo) / error_est_hip 
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

#Metodo de wilson para una proporcion
# Fijar valores conocidos
n<-150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7
# Calcular cantidad de éxitos.
exitos <- p_exito * n

# Prueba de Wilson en R.
prueba <- prop.test(exitos, n = n, p = valor_nulo,
                        alternative = "greater", conf.level = 1 - alfa)
print(prueba)

#Metodo de wilson para dos proporciones
# Fijar valores conocidos (hombres, mujeres)
n <- c(c(48, 42))
exitos <- c(26, 20)
alfa <- 0.05
valor_nulo <- 0.0
# Prueba de Wilson en R.
prueba <- prop.test(exitos, n = n, alternative = "two.sided",
                       conf.level = 1 - alfa)
print(prueba)

##CALCULO DE PODER:FUNCIONES
#power.prop.test(n, p1, p2, sig.level, power, alternative)
#alternative: tipo de hipótesis alternativa (“one.sided” si es unilateral, “two.sided” si es bilateral)
#pwr.p.test(h, n, sig.level, power, alternative):
#para pruebas con una única proporción

#pwr.2p.test(h, n, sig.level, power, alternative):
#para pruebas con dos proporciones donde ambas muestras son de igual tamaño

#pwr.2p2n.test(h, n1, n2, sig.level, power, alternative): 
#para pruebas con dos proporciones y muestras de diferente tamaño

#h: tamaño de efecto
#alternative: tipo de hipótesis alternativa (“two.sided”, “less” o “greater”)







