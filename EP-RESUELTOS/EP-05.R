#SALA 1
#P-1:
desv <- 10
n <- 25
#H0: u  = 170
#HA: u != 170
mu_0 <- 170
inferior <- 169
superior <- 171
err_est <- desv/ sqrt(n)
alfa_izq <- pnorm(inferior, mean = mu_0, sd = err_est, lower.tail = TRUE)
alfa_der <- pnorm(superior, mean = mu_0, sd = err_est, lower.tail = FALSE)
alfa <- alfa_izq + alfa_der
cat("La probabilidad de cometer un error tipo I es alfa =", alfa, "\n\n")
#P-2:
mu_verdadera <- 172
# Calcular la probablidad de esta región (beta)
beta_superior <- pnorm(superior, mean = mu_verdadera, sd = err_est,
                       lower.tail = TRUE)

beta_inferior <- pnorm(inferior, mean = mu_verdadera, sd = err_est,
                       lower.tail = TRUE)

beta <- beta_superior - beta_inferior
cat("La probabilidad de cometer un error tipo II es beta =", beta, "\n\n")
#P-3:
inferior <- 162
superior <- 178
datos <- seq(162, 178, 1)
medio <- mean(datos)
poder_inf <- pnorm(inferior, mean = media, sd = error_estandar,
                   lower.tail = TRUE)
poder_sup <- pnorm(superior, mean = media, sd = error_estandar,
                   lower.tail = FALSE)
poder <- poder_inf + poder_sup

#P-4:
#¿Cuántas barras deberían revisarse para conseguir 
#un poder estadístico de 0,80 y un nivel de significación de 0,05?
poder <- 0.80
alfa <- 0.05
diferencia <- mu_0-mu_verdadera
tam <- power.t.test(delta = diferencia, sd = desv, sig.level = alfa,
                    power = poder, type = "one.sample",
                    alternative = "two.sided")

tamano_t2 <- ceiling(tam[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t2, "\n\n")

#P-5:
#¿Y si quisiera ser bien exigente y bajar la probabilidad
#de cometer un error de tipo 1 a un 1% solamente?
alfa <- 0.01
tam <-power.t.test(delta = diferencia, sd = desv, sig.level = alfa,
                      power = poder, type = "one.sample",
                      alternative = "two.sided")
tamano_t1 <- ceiling(tam[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t1, "\n\n")