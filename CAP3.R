 library(discreteRV) 
 library(ggpubr)
 # Crear una variable discreta para representar el dado
 # adulterado de la tabla 3.1.
 resultados <- 1:6
 probabilidades = c(0.25, 0.125, 0.125, 0.125, 0.125, 0.25)
 X <- RV(outcomes = resultados, probs = probabilidades)
 # Calcular el valor esperado.
 esperado <- E(X)
 cat("Valor esperado:", esperado , "\n")
 # Calcular la varianza.
 varianza <- V(X)
 cat("Varianza:", varianza , "\n")
 # Calcular la desviación estándar.
 desviacion <- SD(X)
 cat("Desviación estándar:", desviacion , "\n")
 
 # Crear vector con los resultados de 5 lanzamientos del dado.
 lanzar_5 <- SofIID(X, n=5)
 # Crear vector con los resultados de 10 lanzamientos del dado.
 lanzar_10 <- SofIID(X, n=10)
 # Crear vector con los resultados de 20 lanzamientos del dado.
 lanzar_20 <- SofIID(X, n=20)
 # Graficar los resultados.
 par(mfrow=c(1, 3))
 plot(lanzar_5,
      main="Lanzamiento de 5 dados", xlab="Suma de resultados", ylab="Probabilidad")
 plot(lanzar_10,
      main="Lanzamiento de 10 dados", xlab="Suma de resultados", ylab="Probabilidad")
 plot(lanzar_20,
      main="Lanzamiento de 20 dados", xlab="Suma de resultados", ylab="Probabilidad")
 
  # Crear una variable aleatoria para un dado balanceado, y calcular su valor
  # esperado, varianza y desviación estándar.
  Y <- RV(outcomes = resultados, probs = 1/6)
  esperado_y <- E(Y)
   varianza_y <- V(Y)
   desviacion_y <- SD(Y)
   cat("E(Y):", esperado_y, "\n")
   cat("V(Y):", varianza_y, "\n")
   cat("SD(Y):", desviacion_y, "\n\n")
  # Crear una combinación lineal de variables aleatorias, y calcular su valor
  # esperado, varianza y desviación estándar.
   Z<-0.5*X+0.5*Y
   esperado_z <- E(Z)
   varianza_z <- V(Z)
   desviacion_z <- SD(Z)
   cat("E(Z):", esperado_z, "\n")
   cat("V(Z):", varianza_z, "\n")
   cat("SD(Z):", desviacion_z)
   
   # Generar valores para una distribución normal con media 0 y
    # desviación estándar 1.
    media <- 0
    desv_est <- 1
    x <- seq(-15, 35, 0.01)
    y <- dnorm(x, mean = media, sd = desv_est)
    normal_1 <- data.frame(x, y)
   
    # Repetir el proceso para una distribución normal con media 10
    # y desviación estándar 6.
    media <- 10
    desv_est <- 6
    x <- seq(-15, 35, 0.01)
    y <- dnorm(x, mean = media, sd = desv_est)
    normal_2 <- data.frame(x, y)
    # Graficar ambas distribuciones.
    g <- ggplot(normal_1, aes(x, y)) + geom_line(color = "blue")
    g <- g + geom_line(data = normal_2, color = "red")
    g <- g + theme_pubr()
    print(g)
    # Cargar datos.
  datos <- mtcars
 # Gráfico Q-Q para la variable Rendimiento.
 g<- ggqqplot(datos ,
            x = "Rendimiento",
            color = "red")
 print(g)
 
 