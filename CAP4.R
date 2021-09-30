 library(ggpubr) 
 library(ggplot2)
 # Establecer la semilla para generar números aleatorios.
 set.seed(9437)
 # Generar aleatoriamente una población de tamaño 1500
 # (en este caso, con una distribución cercana a la normal).
 poblacion <- rnorm(n = 1500, mean = 4.32, sd = 0.98)
  # Calcular la media de la población.
  media_poblacion <- mean(poblacion)
  cat("Media de la población:", media_poblacion, "\n")
  # Tomar una muestra de tamaño 1250.
  tamano_muestra <- 1250
 muestra <- sample(poblacion , tamano_muestra)
  # Calcular
  # 1, 2, 3,
  n <- seq(along = muestra)
  media <- cumsum(muestra) / n
 #las medias acumuladas
  # Crear una matriz de datos con los tamaños y las medias muestrales.
  datos <- data.frame(n, media)
  # Graficar las medias muestrales.
  g <- ggline(data = datos ,
        x= "n",
        y= "media",
        plot_type = "l",
        color = "blue",
        main = "Media móvil",
        xlab = "Tamaño de la muestra", ylab = "Media muestral")
# Añadir
g <- g +geom_hline(aes(yintercept = media_poblacion), color = "red", linetype = 2)
print(g)



 # Establecer la semilla para generar números aleatorios.
 set.seed(94)
 # Generar aleatoriamente una población de tamaño 1500
 # (en este caso, con una distribución cercana a la normal).
 poblacion <- rnorm(n = 1500, mean = 4.32, sd = 0.98)
 # Calcular la media de la población.
 media_poblacion <- mean(poblacion)
 cat("Media de la población:", media_poblacion, "\n")
 # Tomar 1000 muestras de tamaño 100. Quedan almacenadas
 # como una matriz donde cada columna es una muestra.
  tamano_muestra <- 100
  repeticiones <- 1000
 
  muestras <- replicate(repeticiones ,
                          sample(poblacion ,
                                 tamano_muestra))
  # Calcular medias muestrales y almacenar los resultados
  # en forma de data frame.
  medias <- colMeans(muestras)
  medias <- as.data.frame(medias)
  g2<-gghistogram(data = medias , x = "medias",
                  bins = 20,
                  title = "Distribución de la media muestral", xlab = "Media",
                  ylab = "Frecuencia",
                  color = "blue",
                  fill = "blue",
                  alpha = 0.2)
  g2 <- g2+ geom_vline(aes(xintercept = media_poblacion),color = "red", linetype = 1)
  print(g2)

  
  set.seed(872)
  media_poblacion_antiguo <- 530 
  media_muestra_nuevo <- 527.9 
  desv_est <- 48
  n <- 1600
  error_est <- desv_est / sqrt(n)
  x <- seq(media_poblacion_antiguo - 5.2 * error_est, media_poblacion_antiguo 
           + 5.2 * error_est, 0.01)
  y <- dnorm(x, mean = media_poblacion_antiguo, sd = error_est)
  datos <- data.frame(x, y)
  # Graficar la muestra.
  g <- ggplot(data = datos, aes(x))
  g <- g + stat_function(fun = dnorm,
                         args = list(mean = media_poblacion_antiguo ,
                                     sd = error_est), colour = "steelblue", size = 1)
  g <- g + ylab("")
  g <- g + scale_y_continuous(breaks = NULL)
  g <- g + scale_x_continuous(name = "Tiempo de procesamiento [ms]") 
  g <- g + theme_pubr()
  # Colorear el área igual o menor que la media observada.
  g <- g + geom_area(data = subset(datos,
                                   x < media_muestra_nuevo),
                     aes(y = y),
                     colour = "steelblue", fill = "steelblue", alpha = 0.5)
  # Agregar una línea vertical para el valor nulo.
  g <- g + geom_vline(aes(xintercept = media_poblacion_antiguo), color = "red", linetype = 1)
  print(g)
  # Calcular el valor Z para la muestra.
  Z <- (media_muestra_nuevo - media_poblacion_antiguo) / error_est
  # Calcular el valor p.
  p_1 <- pnorm(Z, lower.tail = TRUE)
  cat("Valor p: ", p_1, "\n")
  # También se puede calcular el valor p directamente a partir de la
  # distribución muestral definida por el valor nulo y el error
  # estándar.
  p_2 <- pnorm(media_muestra_nuevo, mean = media_poblacion_antiguo, sd = est_err)
  cat("Valor p: ", p_2)
  
  
  
  # Generar una muestra donde la media cumpla con la hipótesis nula.
  set.seed(208)
  media_poblacion_antiguo <- 530 
  media_muestra_nuevo <- 527.9 
  desv_est <- 48
  n <- 1600
  error_est <- desv_est / sqrt(n)
  x <- seq(media_poblacion_antiguo - 5.2 * error_est, media_poblacion_antiguo 
           + 5.2 * error_est, 0.01)
  y <- dnorm(x,
             mean = media_poblacion_antiguo ,
             sd = error_est) 
  dataframe <- data.frame(x, y)
  # Graficar la muestra.
  g <- ggplot(data = dataframe, aes(x))
  g <- g + stat_function(fun = dnorm,
                         args = list(mean = media_poblacion_antiguo ,
                                     sd = error_est), colour = "steelblue", size = 1)
  g <- g + ylab("")
  g <- g + scale_y_continuous(breaks = NULL)
  g <- g + scale_x_continuous(name = "Tiempo de procesamiento [ms]")
  g <- g + theme_pubr()
  # Colorear el área igual o menor que la media observada.
  g <- g + geom_area(data = subset(dataframe,
                                   x < media_muestra_nuevo),
                     aes(y = y),
                     colour = "steelblue", fill = "steelblue", alpha = 0.5)
  # Calcular el área bajo la cola inferior.
  area_inferior <- pnorm(media_muestra_nuevo,
                         mean  = media_poblacion_antiguo , sd=desv_est)
  
  corte_x <- qnorm(1 - area_inferior ,
          mean = media_poblacion_antiguo , sd = desv_est)
  g <- g + geom_area(data = subset(dataframe,
                                 x > corte_x),
                   aes(y = y),
                   colour = "steelblue", fill = "steelblue",alpha = 0.5)
  g <- g + geom_vline(aes(xintercept = media_poblacion_antiguo),
                     color = "red", linetype = 1)
  print(g)
  Z <- (media_muestra_nuevo - media_poblacion_antiguo) / error_est
  p <- 2 * pnorm(Z, lower.tail = TRUE)
  cat("Valor p: ", p)
  
  
  
  
  
  
  
  
  