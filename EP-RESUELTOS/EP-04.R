library(dplyr)
library(datasets)
library(TeachingDemos)
library(ggpubr)
#SALA 1
#P1
#H0: uo>=1.0
#HA: u0<1.0
n11 <-49
uNulo11<- 1.0
alfa11 <- 0.5

datos11 <- c(1.38, 1.28 ,1.09 ,1.07 ,0.96 ,1.28 ,0.91 ,1.49 ,1.11 ,0.66 ,1.14 ,1.13 ,0.91 ,0.94, 1.30,
            0.87, 0.73 ,0.92 ,1.00 ,1.05 ,1.12 ,1.10 ,0.95 ,1.29 ,0.86 ,0.96 ,0.94 ,1.45 ,1.12, 1.06,
            0.71, 0.88 ,0.96 ,1.14 ,1.03 ,0.89 ,0.81 ,1.04 ,1.15 ,0.75 ,1.12 ,1.01 ,1.11 ,0.64, 1.25,
            0.68, 1.44 ,1.28 ,1.21)
normal11<- shapiro.test(datos11)
#P>alfa, por lo que se considera una distribucion normal en la muestra
#prueba normal
z11<- z.test(   datos11,
                y = NULL,
                alternative = "two.sided",
                mu = uNulo11,
                sd= sd(datos11),
                conf.level = 0.95
              )
# Dado que p<alfa si concluye que es un error rechazar la hipotesis alternativa 
# en favor de la hipotesis nula.
cat("\n\n")
cat("Z test implementada en biblioteca TeachingDemos\n")
cat("-----------------------------------------------\n")
print(z11)

#P2
#Ha: up-ua = 60
#H0: up-ua >  60
uNulo12 <- 60
datos12L <-c(1928, 2549, 2825, 1924 ,1628 ,2175 ,2114 ,2621 ,1843 ,2541)
datos12P <-c(2126, 2885, 2895, 1942, 1750, 2184, 2164, 2626, 2006, 2627)
t12 <- t.test(datos12L,  datos12P,
       alternative ="greater",
       mu = uNulo12, paired = TRUE, 
       conf.level =1-alfa11)
cat("\n\n")
cat("T test de Student para dos muestras correlacionadas en R\n")
cat("--------------------------------------------------------\n")
print(t12)
#P3: SALA 1- SALA 2 -
#H0: u  = 0
#HA: u != 0
uNulo13<- 0
datos13<- chickwts
linaza13 <-chickwts %>% filter(feed =="linseed")

habas13 <-chickwts %>% filter(feed =="horsebean")
p13L <- ggqqplot(
 linaza13,
  x = "weight",
  color = "feed"
)
p13H <- ggqqplot(
  habas13,
  x = "weight",
  color = "feed"
)
show(p13L)
show(p13H)
linaza13 <- select(linaza13,"weight")
habas13 <- select(habas13,"weight")

normal13L<-shapiro.test(linaza13[,1])
normal13H<-shapiro.test(habas13[,1])
print(normal13L)
print(normal13H)
#Ambas muestras se asemejan a una distribucion normal en sus elementos.
t13 <- t.test(linaza13,  habas13,
              alternative ="two.sided",
              mu = uNulo13, paired = FALSE, 
              conf.level =1-alfa11)
print(t13)
# Dado que p<alfa si concluye que es un error rechazar la hipotesis alternativa 
# en favor de la hipotesis nula.

#SALA 2
#P1
#H0: u=3
#HA: u<3
uNulo21 <-3
datos21 <- c(3.10, 5.09, 2.97, 1.59, 4.60, 3.32, 0.55, 1.45, 0.14, 4.47,
             0.80, 3.50, 5.02, 4.67, 5.22, 2.69, 3.98, 3.17, 3.03, 2.21,
             2.69, 4.47, 3.31, 1.17, 2.76, 1.17, 1.57, 2.62, 1.66, 2.05)
n21<-length(datos21)
z21<- t.test(datos21,mu=uNulo21,alternative = "two.sided",paired = FALSE,conf.level = 1-alfa11)
print(z21)
#No se puede rechazar la Hipotesis nula, dado que p>alfa, 
#sin embargo este se encuentra al limite, por lo que se recomienda aumentar el tamano
#de la muestra para obtener una valor representativo.

#P2
#H0: u=25
#HA: u>25
uNulo22 <-25
alfa2 <- 0.01
lactancia21 <- c(1928,2549,2825,1924,1628,2175,2114,2621,1843,2541)
posdestete22 <- c(2126,2885,2895,1942,1750,2184,2164,2626,2006,2627)
normalL21 <- shapiro.test(lactancia21)
normalP22 <- shapiro.test(posdestete22)
print(normalL21)
print(normalP22)
t22 <-t.test(posdestete22,lactancia21,paired = TRUE, alternative = "greater",
             mu=uNulo22, conf.level = 1-alfa2)
print(t22)
#se rechaza la hipotesis nula.


#SALA 3


#SALA 4


#SALA 5


#SALA 6


#SALA 7


#SALA 8


#SALA 9


#SALA 10


