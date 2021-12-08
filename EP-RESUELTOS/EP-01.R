library(dplyr)
library(tidyr)
datos <-read.csv("Desktop/IME/EjercicioP/ejercicioPractico.csv",sep = ",",stringsAsFactors = FALSE)
datos <- data.frame(datos)
aricayparinacota <- datos %>% filter(Region=="Arica y Parinacota")

aricayparinacotaFechas <- select(aricayparinacota, "X2020.07.01":"X2020.12.31")
aricayparinacotaFechas<- aricayparinacotaFechas %>% pivot_longer(cols = starts_with("X"),
                                 names_to = "Fecha",
                                 values_to = "Contagiados",
                                 values_drop_na = TRUE)
maxAyP <- max(aricayparinacotaFechas[["Contagiados"]])
Dia <- aricayparinacotaFechas %>% filter(Contagiados == maxAyP)

periodo <-select(datos, "X2020.07.01":"X2020.12.31")
totalmes07<-select(periodo, starts_with("X2020.07"))
max07<-sum(totalmes07[17,])

totalmes08<-select(periodo, starts_with("X2020.08"))
max08<-sum(totalmes08[17,])

totalmes09<-select(periodo, starts_with("X2020.09"))
max09<-sum(totalmes09[17,])

totalmes10<-select(periodo, starts_with("X2020.10"))
max10<-sum(totalmes10[17,])

totalmes11<-select(periodo, starts_with("X2020.11"))
max11<-sum(totalmes11[17,])

totalmes12<-select(periodo, starts_with("X2020.12"))
max12<-sum(totalmes12[17,])

