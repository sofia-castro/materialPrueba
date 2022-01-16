#CAPÍTULO 12. REMUESTREO
#BOOTSTRAPPING
#construcción de un intervalo de confianza para 
#la media poblacional mediante bootstrapping.
library(boot)
library(bootES)
# Crear muestra inicial , mostrar su histograma y calcular la media .
muestra <- c(79 , 75, 84, 75, 94, 82, 76, 90, 79, 88)
datos <- data.frame(muestra)
# Establecer cantidad de remuestreos y nivel de significaci ón.
B = 2000
alfa <- 0.01
cat(" Paquete boot \n")
# Construir distribuci ón bootstrap usando el paquete boot .
media <- function( valores , i) {
mean(valores [i])
}
set.seed(432)
distribucion_b <- boot( muestra , statistic = media , R = B)
print(distribucion_b)
#Graficar distribuci ón bootstrap .
print(plot(distribucion_b))
# Construir intervalos de confianza .
intervalo_t <- boot.ci( distribucion_b, conf = 1 - alfa , type = "norm")
cat("\n\ nIntervalo de confianza usando distribuci ón t:\n")
print(intervalo_t)
intervalo_per <- boot.ci( distribucion_b, conf = 1 - alfa , type = "perc")
cat("\n\ nIntervalo de confianza usando percentiles :\n")
print(intervalo_per)
intervalo_bca <- boot.ci(distribucion_b, conf = 1 - alfa , type = "bca")
cat("\n\ nIntervalo de confianza BCa :\n")
print(intervalo_bca)
# Construir distribuci ón bootstrap usando el paquete bootES .
set.seed(432)
distribucion_bootstrapES <- bootES( muestra , R = B, ci. type = "bca",
                                    ci.conf = 1 - alfa , plot = TRUE )
print(distribucion_bootstrapES)

#inferencia sobre la media de una muestra con bootstrapping.
library(boot)
set.seed (432)
# Crear muestra inicial , mostrar su histograma y calcular la media .
muestra <- c(79 , 75, 84, 75, 94, 82, 76, 90, 79, 88)
valor_observado <- mean(muestra)
datos <- data.frame(muestra)
# Construir distribuci ón bootstrap .
B <- 2000
media <- function( valores , i) {
    mean( valores [i])
}
distribucion_b <- boot( muestra , statistic = media , R = B)
# Desplazar la distribuci ón bootstrap para que se centre en
# el valor nulo .
valor_nulo <- 75
desplazamiento <- mean( distribucion_b[["t"]]) - valor_nulo
distribucion_nula <- distribucion_b[["t"]] - desplazamiento
# Determinar el valor p.
p <- (sum(distribucion_nula > valor_observado ) + 1) / (B + 1)
cat(" Valor p:", p)

#Bootstrapping para dos muestras independientes
#bootstraping para la diferencia de medias.
library(simpleboot)
library(boot)
library(ggpubr)
set.seed(432)

# Ingresar datos originales
hombres <- c(1.3 , 1.5 , 1.6 , 1.7 , 1.7 , 1.9 , 2.3 , 2.4 , 2.6 , 2.6 , 2.7 ,
               9 2.8 , 3.2 , 3.7 , 4.1 , 4.4 , 4.5 , 4.8 , 5.2 , 5.2 , 5.3 , 5.5 ,
               10 5.5 , 5.6 , 5.6 , 5.7 , 5.7)
mujeres <- c(3.5 , 3.6 , 3.8 , 4.3 , 4.5 , 4.5 , 4.9 , 5.1 , 5.3 , 5.3 , 5.5 ,
                13 5.8 , 6.0 , 6.3 , 6.3 , 6.4 , 6.4 , 6.6 , 6.7)
n_hombres <- length(hombres)
n_mujeres <- length(mujeres)
sexo <- c(rep("Hombre", n_hombres ), rep("Mujer", n_mujeres))
nota <- c( hombres , mujeres )
datos <- data.frame(nota , sexo )
# Comprobar normalidad de las muestras .
print( shapiro.test(hombres))
print( shapiro.test(mujeres))
# Calcular la diferencia observada entre las medias muestrales .
media_hombres <- mean(hombres)
media_mujeres <- mean(mujeres)
diferencia_observada <- media_hombres - media_mujeres
cat (" diferencia observada :", medi _hombres - media_mujeres , "\n\n")
# Establecer el nivel de significaci ón.
alfa <- 0.05
# Crear la distribuci ón bootstrap .
B <- 9999
distribucion_bootstrap <- two.boot( hombres , mujeres , FUN = mean , R = B)
# Examinar la distribuci ón bootstrap .
valores <- data.frame(distribucion_bootstrap$t)
colnames(valores) <- "valores"
histograma <- gghistogram( valores , x = "valores", color = "red",
                               fill = "red", bins = 100 ,
                               xlab = " Diferencia de medias",
                              ylab = " Frecuencia", add = "mean") 
print(histograma)
qq <- ggqqplot( valores , x = "valores", color = "red")
print(qq)
cat(" Distribuci ón bootstrap :\n")
cat("\ tMedia :", mean( valores$valores ), "\n")
cat("\ tDesviaci ón est á ndar :", sd( valores$valores), "\n\n")
# Construir el intervalo de confianza .
intervalo_bca <- boot.ci( distribucion_bootstrap , conf = 1 - alfa ,
                               type = "bca")
print(intervalo_bca)
#bootstraping para inferir acerca de la diferencia de medias.
library(simpleboot)
library(boot)
library(ggpubr)
set.seed(432)
# Ingresar datos originales
hombres <- c(1.3 , 1.5 , 1.6 , 1.7 , 1.7 , 1.9 , 2.3 , 2.4 , 2.6 , 2.6 , 2.7 ,
               9 2.8 , 3.2 , 3.7 , 4.1 , 4.4 , 4.5 , 4.8 , 5.2 , 5.2 , 5.3 , 5.5 ,
               10 5.5 , 5.6 , 5.6 , 5.7 , 5.7)
mujeres <- c(3.5 , 3.6 , 3.8 , 4.3 , 4.5 , 4.5 , 4.9 , 5.1 , 5.3 , 5.3 , 5.5 ,
                13 5.8 , 6.0 , 6.3 , 6.3 , 6.4 , 6.4 , 6.6 , 6.7)
n_ hombres <- length( hombres )
n_ mujeres <- length( mujeres )
sexo <- c(rep("Hombre", n_hombres ), rep("Mujer", n_mujeres ))
nota <- c( hombres , mujeres )
datos <- data.frame(nota , sexo )
# Calcular la diferencia observada entre las medias muestrales .
media_hombres <- mean( hombres )
media_mujeres <- mean( mujeres )
valor_observado <- media_hombres - media_mujeres
# Crear la distribuci ón bootstrap .
B <- 9999
valor_nulo <- 1.5
distribucion_bootstrap <- two.boot( hombres , mujeres , FUN = mean , R = B)
desplazamiento <- mean( distribucion_bootstra [["t"]]) - valor_nulo
distribucion_nula <- distribucion_bootstrap[["t"]] - desplazamiento
# Determinar el valor p.
p <- (sum( abs(distribucion_nula ) > abs( valor_observado )) + 1) / (B + 1)
cat (" Valor p:", p)
#Bootstrapping para dos muestras pareadas
