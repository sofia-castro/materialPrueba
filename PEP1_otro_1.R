library(dplyr)
library(pwr)
#PREGUNTA 1
set.seed(127)
info <- read.csv('/Users/MacbookAir/Downloads/IME/PEP1/IME-2020-2-PE1-datos.csv')
info <- data.frame(info)
sinAnemia <- info %>% filter(anaemia==0)
conAnemia <- info %>% filter(anaemia==1)
nivelSignificancia <- 0.05
n <- 25
sinAnemia_25 <- sample(sinAnemia,n,replace=TRUE)
conAnemia_25 <- sample(conAnemia,n,replace=TRUE)
#Hipotesis nula: La diferencia entre el promedio de los pacientes con anemia y los pacientes
#sin anemia es 0.
#uca-usa=0
#Hipotesis alternativa: La diferencia entre el promedio de los pacientes con anemia y los pacientes
#sin anemia es distinta a 0.
#uca-usa=!0
#Comprobar distribucion normal:
normal_SA <- shapiro.test(sinAnemia_25[['creatinine_phosphokinase']])
normal_CA <- shapiro.test(conAnemia_25[['creatinine_phosphokinase']])
print(normal_CA)
print(normal_SA)
diferencia_medias <- media_CA-media_SA
prueba_T<- t.test(x=conAnemia_25,y=sinAnemia_25,
                  alternative = "two.sided",mu = 0,
                  conf.level = 1-nivelSignificancia)
print(prueba_T)
#por lo tanto se rechaza la hipotesis nula, dado que la diferencia entre las medias 
#de la muestra con anemia y sin anemia es desigual. Conluyendo, si existe una 
#diferencia significativa en los niveles de creatina quinasa entre los pacientes que
#poseen anemia y los que no presentan anemia, siendo la afirmacion realizada 
#por el estudio es incorrecto segun los resultados obtenidos.
poder_estadistico <-pwr.t.test( n = n,
                                d=0,
                                sig.level = nivelSignificancia,
                                power = NULL ,
                                type = "two.sample",
                                alternative = "two.sided")

print(poder_estadistico)
#PREGUNTA 2
set.seed(439)
n2<-60
nivelSignificancia2 <- 0.01
muertos <- info %>% filter(DEATH_EVENT==1)
muestra <-  sample(muertos,n2,replace=TRUE)
#hipotesis nula: Dos tercios de los pacientes fallecidos padecian de hipertension.
#p=2/3
#hipotesis nula: Dos tercios de los pacientes fallecidos no padecian de hipertension.
#p=!2/3
p0=2/3
cantidad_exito <-count(muestra,high_blood_pressure==1)
cantidad_exito <- cantidad_exito[2,]

p_exito<- cantidad_exito/n2
p_exito <- p_exito[,2]
np<- n2*p_exito
nq<- (1-p_exito)*n2

# Construcción del intervalo de confianza.
error_est <- sqrt((p_exito * (1 - p_exito)) / n2)
Z_critico <- qnorm(nivelSignificancia2 / 2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
cat("Intervalo de confianza = [", inferior, ", ", superior, "]\n", sep = "")
# Prueba de hipótesis.
error_est_hip <- sqrt((p0 * (1 - p0)) / n2)
Z <- (p_exito - p0) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa unilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

