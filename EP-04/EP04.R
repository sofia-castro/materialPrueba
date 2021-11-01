#INTEGRANTES: Sofia Castro     -> 20.055.286-5
#             Sebastian Astete -> 18.562.196-0
#             Bastian Loyola   -> 20.552.001-5
#PREGUNTA 1
#H0 (hipótesis nula) =  El tiempo de activaión promedio para los los sistemas 
#asperos de prevención no sobrepasa los 25 segundos
#µ0 = 25
#HA (hipótesis alternativa) = El tiempo de activaión  promedio para los los sistemas 
#asperos de prevención sobrepasa los 25 segundos
#µA > 25
library(dplyr)
library(TeachingDemos)
library(ggpubr)
muestra_bomberos <- c(27,41,22,27,23,35,30,33,24,27,28,23,24)
muestra_bomberos<- data.frame(muestra_bomberos)
significancia_bomberos= 0.01
shapiro_bomberos <- shapiro.test(muestra_bomberos)
print(shapiro_bomberos)
#Dado que el valor de p es mayor al nivel de significancia estipulado y 
#que el valor de W es mayor al valor critico (p), no se rechaza la hipótesis nula.
n<-length(muestra_bomberos)
bomberos_nulo <-25
z <-(media_bomberos-bomberos_nulo)/desE_bomberos
p_bomberos <- 2 * pnorm(z, lower.tail = FALSE)
pruebaZ_bomberos = z.test(media_bomberos,bomberos_nulo,alternative = "greater",
                          desE_bomberos,1-significancia_bomberos )
print(pruebaZ_bomberos)
#Dado que el resultado de la media en la prueba z es mayor a 28, 
#se rechaza la hipótesis nula y se acepta la hipótesis alternativa.

#PREGUNTA 2: 
#Este ejercicio posee muestras pareadas, dado que tienen correspondencia en una observacion.
#Ademas, la eleccion de los elementos de la muestra es al azar.
#up= promedio de posdestete
#ul= promedio de lactancia
#H0 (hipótesis nula) = La diferencia entre el contenido total de minerales en los huesos 
#del cuerpo durante el posdestete y la etapa de lactancia es igual 30.
#µp-ul = 30
#HA (hipótesis alternativa) = La diferencia entre el contenido total de minerales en los huesos
#del cuerpo durante el posdestete excede el de la etapa de lactancia por más de 30
#up-ul > 30
lactancia <- c(2875,1893,1978,2599,1974,2671,2164,2225,2591,1678)
posdestete <- c(2895,2006,2126,2885,1942,2626,2164,2184,2627,1750)
resta_P2<- posdestete-lactancia
shapiro_P2<- shapiro.test(resta_P2)
print(shapiro_P2)
# La prueba de Shapiro nos indica que no se debe rechazar la hipótesis nula, 
#pues el valor de p es mayor al nivel de significancia estipulado
significancia_P2 <-0.01
prueba_t<-t.test(resta_P2 ,
                 alternative = "greater",
                 mu = 30, conf.level = 1 - significancia_P2)
print(prueba_t)
#El valor de t se acerca a 0 y el valor de p es mayor al nivel de significancia, 
#por lo que no se rechaza la hipotesis nula, sino que se puede concluir con un 99% 
#de confianza que  el contenido total de minerales en los huesos 
#del cuerpo durante el posdestete no excede el de la etapa de lactancia por más de 30.
  
  
  