library(dplyr)
#PREGUNTA 1
#H0: ambas muestras presentan la misma proporcion
#HA: ambas muestras presentan distinta proporcion
p <- 0.05
total <- c(15,16)
alivio <- c(9,2)
tabla <- as.table(rbind(total,alivio))
dimnames(tabla) <- list( grupo = c("Total ","Alivio "),
                         lenguajes = c("Tratamiento","Control"))
# Hacer prueba chi - cuadrado de homogeneidad .
prueba <- chisq.test(tabla)
print(prueba)
#Dado los resultados no se puede rechazar la h0, por lo que se puede inferir
#que ambas muestran poseen las misma proporciones.

#PREGUNTA 2
#H0: las variables son independientes.
#HA: las variables están relacionadas.
antes <- c(25,25)
despues <- c(5,9)
tabla2 <- as.table(rbind(antes,despues))
dimnames(tabla2) <- list( Intervencion = c("Antes","Despues"),
                         Fumadores = c("si","no"))

prueba21 <- chisq.test ( tabla2 )
cat ("\ nLa prueba internamente calcula los valores esperados :\n")
esperados <- round ( prueba21[["expected"]], 3)
print ( esperados )
cat ("\ nResultado de la prueba :\n")
print ( prueba21 )

#PREGUNTA 3
prop1 <- c(205,26,25,19)
censo <- c(0.72*275,275*0.07,0.12*275,0.09*275 )
tabla3 <- as.table(rbind(prop1,censo))
dimnames(tabla3) <- list( Actual = c("Actual","Censo"),
                         Razas = c("Blancos","Negros","Latinos","OTRAS"))
prueba31 <- chisq.test ( tabla3 )
cat ("\ nLa prueba internamente calcula los valores esperados :\n")
esperados <- round ( prueba31[["expected"]], 3)
print ( esperados )
cat ("\ nResultado de la prueba :\n")
print ( prueba31 )

#PREGUNTA 4
# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las expectativas
# de los chilenos de ir al mundial de Catar, a la luz de los resultados de la selección nacional de fútbol en la
# jornada clasificatoria triple de octubre 2021, que requiera utilizar una prueba Q de Cochran. Identifique las
# variables involucradas y las hipótesis a contrastar

#Dado que Chile posee 16 puntos, requiere de 16 puntos más para poder clasificar al mundial, 
#por lo que se decide encuestar a los chilenos con respecto a los resultados de los partidos contra
# Bolivia, Paraguay y Argentina, donde 0 representa perder el parte y 1 ganar el partido.
#También se da por perdido el partido contra brasil, así obligando a ganar los tres partidos mencionados.

#  Persona     Bolivia      Paraguay   Argentina
#     1           1             1         0
#     2           1             0         0
#     .           .             .         .
#     .           .             .         .
#     .           .             .         .
#   nk>=24        1             1         0

#H0: Chile clasifica 
#Ha: Chile no clasifica 

