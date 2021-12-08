library(dplyr)
library(modeest)
datos <- read.csv(file = "/Users/macbookair/Desktop/IME/materialPrueba/EP-RESUELTOS/EP-02_Datos_Casen_2017.csv")
#SALA 1:¿Se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM?

RM <- datos %>% filter(region=="Región Metropolitana de Santiago")
xProvincia <- RM %>% count(provincia)

#SALA 2:¿Cómo diría que es el ingreso de los hombres de la 
#RM (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)?

hombres <- RM %>% filter(sexo=="Hombre")
ytothombres <- hombres %>% summarise(Media = mean(ytot), Mediana = median(ytot),
                                         Desviacion_Estandar = sd(ytot), Moda = mfv(ytot))

#SALA 3: ¿Tienen hombres y mujeres ingresos similares en la RM?

mvsh<- group_by(RM,sexo) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

#SALA 4: ¿Se distribuye de igual manera la situación ocupacional de los hombre
#s que viven en áreas rurales y quienes viven en áreas urbanas de la RM?

zonaH <-group_by(hombres,zona) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

#SALA 5: ¿Son similares los ingresos registrados en las diferentes provincias de la RM?
ingProvincia<- group_by(RM,provincia) %>% summarise(Media = mean(ytot), Mediana = median(ytot), 
                                                    Desv_Estandar= sd(ytot))
#SALA 6: ¿Van los ingresos de las mujeres de la RM incrementándose con la edad?
mujeres <-  RM %>% filter(sexo=="Mujer")
edadM <-group_by(mujeres,edad) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

#SALA 7: ¿Tiene relación el ingreso de las mujeres de la RM
#con la riqueza del municipio donde habita?

ingMunicioioM <-group_by(mujeres,provincia) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

#SALA 8: ¿Cómo diría que es el ingreso de las mujeres de la RM
#(simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)?

ytotMujeres <- mujeres %>% summarise(Media = mean(ytot), Mediana = median(ytot),
                                Desviacion_Estandar = sd(ytot), Moda = mfv(ytot))

#SALA 9: El nivel de ingreso de las mujeres de la RM ¿varía con el estado civil?

ingEstCivilM <-group_by(mujeres,ecivil) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

#SALA 10: Los ingresos de los habitantes de la RM, 
#¿van incrementándose con la edad de forma similar en hombres y mujeres?
edadM <-group_by(mujeres,edad) %>% summarise(Media = mean(ytot), Mediana = median(ytot))
edadH <-group_by(hombres,edad) %>% summarise(Media = mean(ytot), Mediana = median(ytot))

