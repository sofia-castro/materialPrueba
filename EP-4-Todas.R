#SALA 1
nivel_con<-0.01
#PREGUNTA 1
muestra_1<-c(1.38, 1.28, 1.09 ,1.07 ,0.96, 1.28, 0.91 ,1.49 ,1.11,
             0.66 ,1.14, 1.13, 0.91 ,0.94, 1.30 ,0.87 ,0.73, 0.92 ,1.00,
             1.05, 1.12, 1.10 ,0.95 ,1.29, 0.86, 0.96 ,0.94 ,1.45 ,1.12 ,1.06,
             0.71, 0.88 ,0.96 ,1.14 ,1.03, 0.89, 0.81 ,1.04 ,1.15 ,0.75, 1.12, 1.01, 1.11,
             0.64, 1.25, 0.68 ,1.44 ,1.28, 1.21)
tamano_1<- 49
#u0: 1.0
#Hipotesis nula: el ALD medio es superior o igual a 1.0 pixel
#u>=1.0
#hipotesis Alternativa: el ALD medio es inferior a 1.0 pixel
#u<1.0
normal_1<- shapiro.test(muestra_1)
print(normal_1)
#La prueba de shapiro permite decidir que no se debe rechazar la hipotesis nula y que distribuye
# de manera normal.
#se implementa la prueba t para una muestra.
prueba_1<- t.test(muestra_1 ,
       alternative = "less",
       mu = 1.0, conf.level = 1 - nivel_con)
print(prueba_1)
#los resultados de la prueba t no permite rechazar la hipotesis alternativa, dado que la media 
#de la muestra es de 1.044286 con un 99% de confianza.
#PRREGUNTA 2
#H0: La diferencia entre el contenido total de minerales en los huesos del cuerpo 
#durante el posdestete y el de la etapa de lactancia es igual o mayor a 60
#up-ul>=60
#HA: La diferencia entre el contenido total de minerales en los huesos del cuerpo 
#durante el posdestete y el de la etapa de lactancia es menor a 60
#up-ul<60
lactancia_1 <- c(1928,2549,2825,1924,1628,2175,2114,2621,1843,2541)
posdestete_1 <- c(2126,2885,2895,1942,1750,2184,2164,2626,2006,2627)
diferencia_1<- posdestete_1-lactancia_1
normal_2<- shapiro.test(diferencia_1)
print(normal_2)
#dado que P>nivel de confianza, se asume que posee una distribucion normal y 
#que no se puede rechazar la hipotesis nula.
#Se realiza la prueba t para muestras pareadas.
prueba_2  <- t.test(diferencia_1 ,
         alternative = "less",
         mu = 60, conf.level = 1 -0.01)
print(prueba_2)
#por lo que no se rechaza la hipotesis nula
#PREGUNTA 3
#h0: son iguales
#ha:son distintos
library(dplyr)
datos_3<-chickwts
linaza_1<-datos_3 %>% filter(feed == "linseed")
habas_1<-datos_3 %>% filter(feed == "horsebean")
linaza_1<-linaza_1[['weight']]
habas_1<-habas_1[['weight']]
habas_1<-append(habas_1,0)
habas_1<-append(habas_1,0)
diferencia_2<- linaza_1-habas_1
normal_3<- shapiro.test(diferencia_2)
print(normal_3)
prueba_3 <- t.test(diferencia_2 ,
                    alternative = "two.sided",
                    mu = 0, conf.level = 1 -0.01)
print(prueba_3)