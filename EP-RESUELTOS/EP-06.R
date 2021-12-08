library(dplyr)
library(pwr)
#PREGUNTA 1
#Como se desea evaluar si la incidencia de cáncer oral en la población general que bebe
#regularmente entre 10 y 44 ml de alcohol era de 50%, se establecen las siguientes hipotesis:
#H0: p0=0.5. 
#HA: p0!=0.5.
n<- 483
p_nulo <- 0.5
casos <- 109
p_exito <- casos/n
q <- 1-p_exito
alfa <- 0.05
#Se desea implementar la prueba de Wald para una proporción, por lo que se verifican 
#si cumple con las condiciones:
#Dado que es un estudio realizado por cientificos se asume que las muestras fueron escogidas al azar.
#Se tiene que p_exito*n>=10 y que n*q>= 10
#A continuación se implementa la prueba mencionada
error <- sqrt(( p_nulo * (1 - p_nulo )) / n)
Z <- (p_exito - p_nulo ) / error 
p <- pnorm (Z, lower.tail = FALSE )
cat ("Hipó tesis alternativa unilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)
#Como p>alfa, no se puede rechazar hipotesis nula en favor de la alternativa. Por lo tanto, 
#se concluye que la incidencia de cáncer oral en la población general que bebe
#regularmente entre 10 y 44 ml de alcohol era de 50% con un 95% de confianza.
#EN R
ptest1 <- prop.test(
  x = casos,
  n = n,
  p = p_nulo,
  alternative = "two.sided",
  conf.level = 1 - alfa,
  correct = FALSE
)
cat("\n")
cat("Usando la función prop.test():\n")
cat("------------------------------\n")
print(ptest1)
#PREGUNTA 2
#da lo mismo beber de 10 a 44 ml de alcohol diariamente que hacerlo con 45 o más ml?
#Como se solicita ver la diferencia entre dos proporciones, se decide implementar el 
#Método de Wald para dos proporciones
#condiciones:
#1. Siguen una distr. normal

#2. Son muestras independientes
#H0: p1-p2=0
#HA: p1-p2!=0
p_nulo <- 0
p1_exito <- casos/n
p2_exito <- 242/n
alfa <- 0.05
n1<-casos
n2 <- 242
diferencia <- p1_exito - p2_exito
p <- ( p1_exito + p2_exito ) / (n1+ n2 )
error_p1 <- (p * (1 - p )) / n1
error_p2 <- (p * (1 - p )) / n2
error <- sqrt ( error_p1 + error_p2 )
Z <- ( diferencia - p_nulo ) / error
p <- 2 * pnorm (Z, lower.tail = FALSE )
cat ("Hipó tesis alternativa bilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)
#El resultado indica que p>alfa, por lo que no se puede rechazar la hipotesis nula, determinando que 
#da lo mismo beber de 10 a 44 ml de alcohol diariamente que hacerlo con 45 o más ml.
#PREGUNTA 3
diferencia <- 0.15
poder <- 0.9
alfa <- 0.05
n_aprox <- power.prop.test(p1 = p1_exito, p2 = p2_exito, 
                           sig.level = alfa, power = poder,
                           alternative = "two.sided")
print(n_aprox)
