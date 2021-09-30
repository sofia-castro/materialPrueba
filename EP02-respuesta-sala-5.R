data <-read.csv("Desktop/IME/materialPrueba/EP-02_Datos_Casen_2017.csv", sep = ",")
library("modeest")
library("dplyr")
data <-data.frame(data,stringsAsFactors = TRUE)
medidasEstadisticas <- group_by(data, provincia) %>%
  summarise(mean(ytot), median(ytot), mfv(ytot),
            sd(ytot), IQR(ytot), var(ytot))
library(ggpubr)
library(ggplot2)
gModa <- ggbarplot(medidasEstadisticas , y ="mfv(ytot)" ,x = "provincia",fill = "provincia",ylab="Moda",
             title = "Moda segun provincia",palette = "pastel")

gPromedio <- ggbarplot(medidasEstadisticas , y ="mean(ytot)" ,x = "provincia",fill = "provincia",ylab="Promedio",
                title = "Promedio segun provincia", palette = "pastel")
gDE <- ggbarplot(medidasEstadisticas , y ="sd(ytot)" ,x = "provincia",fill = "provincia",ylab="Desviacion Estandar",
                       title = "Desviacion Estandar segun provincia", palette = "pastel")

gMediana<- ggbarplot(medidasEstadisticas , y ="median(ytot)" ,x = "provincia",fill = "provincia",ylab="Mediana",
                 title = "Mediana segun provincia", palette = "pastel")

gVar <- ggbarplot(medidasEstadisticas , y ="var(ytot)" ,x = "provincia",fill = "provincia",ylab="Varianza",
                       title = "Varianza segun provincia", palette = "pastel")

gIQR <- ggbarplot(medidasEstadisticas , y ="IQR(ytot)" ,x = "provincia",fill = "provincia",ylab="IQR",
                       title = "IQR segun provincia", palette = "pastel")
 
print(gModa)  
print(gPromedio)  
print(gMediana)  
print(gDE)
print(gVar)
print(gIQR)

#RESPUESTAS
#¿Son similares los ingresos registrados en las diferentes provincias de la RM?
# Dado que los promedios y las desviaciones estandar de  
#cada provincia son distintas, se concluye que no son similares (tabla medidasEstadisticas).
#Discutir y consensuar qué medidas estadísticas (media, mediana, moda, etc.)
# y qué forma gráfica ayudaría a responder la pregunta asignadas.
# la medida estadistica que permite visualizar las diferencias es el promedio junto con la desviacion estandar,
# pues la primera nos permite ver a que valor tiende y la segunda el nivel de dispersion existente.
# El histograma es la grafica que ayuda a responder, dado que nos permite visualizar las diferencias 
# entre las provincia segun la medida.