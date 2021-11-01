#H0: se demoran 120 minutos en realizar la prueba de calculo I.
#Ha: se demoran distinto de 120 minutos en realizar la prueba de calculo I.
n <- 16
tiempos <- c(140.6, 133.3, 142.4, 86.4, 129.9, 110.8, 133.2, 
           129.1, 142.5, 150.2, 141.6, 111.0, 127.2, 137.9, 
           131.9, 121.9)
library(dplyr)
library ( ggpubr )
alfa <- 0.01
mu1 <- 120
normalidad <- shapiro.test(tiempos)
# dado que alfa es menor que p, se cumple la distribucion normalidad y 
# no hay evidencia suficiente para rechazar la hipotesis nula, por lo que 
# se procede a implementar la prueba t.
prueba_t1 <- t.test(tiempos, conf.level = 1-alfa, alternative = "two.sided", mu=mu1)
# dado que el alfa es menor al valor p entregado por la prueba t, finalmente no se
# puede rechazar la hipotesis nula y se acepta esta.

catalizador1 <- c(62.9, 67.2, 67.4, 67.4, 67.2, 64.6, 69.6, 65.7, 68.2, 72.0)
catalizador2 <- c(66.8, 69.3, 69.6, 67.3, 68.8, 68.4, 68.6, 70.3, 69.6, 71.7)

datos2 <- read.csv("/Users/macbookair/Desktop/IME/materialPrueba/EP-02_Datos_Casen_2017.csv",sep=",")

xprovincia <- datos2 %>% count(provincia)
hombres_RM<- datos2 %>% filter( sexo == "Hombre")
library(modeest)
tendenciasH <- hombres_RM %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                        moda=mfv(ytot),mediana=median(ytot))
mujeres_RM<- datos2 %>% filter( sexo == "Mujer")
tendenciasM <- mujeres_RM %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                         moda=mfv(ytot),mediana=median(ytot))

provincia<- group_by(datos2,provincia) %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                                             moda=mfv(ytot),mediana=median(ytot))

edad_mujeres<-group_by(mujeres_RM,edad) %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                                        moda=mfv(ytot),mediana=median(ytot))
edad_hombres<-group_by(hombres_RM,edad) %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                                       moda=mfv(ytot),mediana=median(ytot))
estadoCivil_M <- group_by(mujeres_RM,ecivil) %>%  summarise(media= mean(ytot),desv_estandar=sd(ytot),
                                                          moda=mfv(ytot),mediana=median(ytot))

datos3 <- c(1.38, 1.28, 1.09, 1.07, 0.96, 1.28, 0.91, 1.49, 1.11, 0.66, 1.14 ,1.13 ,0.91, 0.94, 1.30,
            0.87, 0.73, 0.92, 1.00, 1.05 ,1.12, 1.10, 0.95, 1.29, 0.86 ,0.96 ,0.94 ,1.45, 1.12, 1.06,
            0.71, 0.88, 0.96, 1.14, 1.03, 0.89, 0.81, 1.04 ,1.15, 0.75, 1.12 ,1.01, 1.11 ,0.64 ,1.25,
            0.68, 1.44, 1.28, 1.21)
#H0: la media de la poblacion equivale a 1.
#Ha: la media de la poblacion es distinto a 1.
n1 <- 49
m0 <- 1
alfa<- 0.01
library(dplyr)
library ( ggpubr )
normalidad <- shapiro.test(datos3)
#se distribuye de forma nomrla, por lo que no se puede rechazar la hipotesis nula.









