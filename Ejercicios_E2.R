
data <- read.csv("Desktop/IME/materialPrueba/EP-02_Datos_Casen_2017.csv", sep=",")

#Pregunta 1:¿Se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM?
data <-data.frame(data,stringsAsFactors = TRUE)
cantidadProvincia <- group_by(data, provincia) %>%
  summarise(count = n())
porcentajeProvincia= prop.table(cantidadProvincia[2])*100
cantidadProvincia$count <-NULL
cantidadProvincia$porcentaje <-porcentajeProvincia
cantidadProvincia <- data.frame(cantidadProvincia)
#calcular la dispersion 
medidasEstadisticas <- var(cantidadProvincia[2])

#Pregunta 2: ¿Cómo diría que es el ingreso de los hombres 
#             de la RM (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)
library("modeest")
library("dplyr")
RM<- group_by(data, region="Región Metropolitana de Santiago") 
sexoM<- group_by(RM, sexo="Hombre") 
sexoME<- sexoM %>%  summarise(mean(ytot), median(ytot), mfv(ytot),
                                            sd(ytot), IQR(ytot), var(ytot))

#Pregunta 3: ¿Tienen hombres y mujeres ingresos similares en la RM?
sexoFME <- group_by(data, sexo) %>%  summarise(mean(ytot), median(ytot), mfv(ytot),
                                            sd(ytot), IQR(ytot), var(ytot))

#Pregunta 4: ¿Se distribuye de igual manera la situación ocupacional de los
#             hombres que viven en áreas rurales y quienes viven en áreas urbanas de la RM?
library(ggpubr)
areas <- group_by(sexoM, zona)
areasR <-areas  %>% filter(zona=="Rural")
areasU <-areas  %>% filter(zona=="Urbano")
areasR <- table(areasR[["ch1"]],areasR[["zona"]])
areasU <- table(areasU[["ch1"]],areasU[["zona"]])
areas <- table(areas[["ch1"]],areas[["zona"]])
areasU<-as.data.frame(areasU)
areasR<-as.data.frame(areasR)
print(areasR)
print(areasU)
gU <- ggbarplot(areasU , y ="Freq" ,x = "Var1",fill = "Var1",ylab="Frecuencia",
                   title = "situacion ocupacional en zona Urbana",palette = "pastel" )
gR <- ggbarplot(areasR , y ="Freq" ,x = "Var1",fill = "Var1",ylab="Frecuencia",
                title = "situacion ocupacional en zona Rural",palette = "pastel" )
g <- ggarrange(gU ,gR,common.legend = TRUE)
print(g)

# Pregunta 6: ¿Van los ingresos de las mujeres de la RM incrementándose con la edad?
library(ggpubr)
sexoF <- group_by(RM, sexo="Mujer") 
sexoF <- sexoF %>% arrange(sexoF$edad)
gsexoF <- ggscatter(sexoF,
                    x = "edad",
                    y = "ytot",
                    title = "Ingreso vs Edad",
                    xlab = "Edad [años]",
                    ylab = "Ingreso [clp]",palette = "pastel")
fSexoF <- data %>% filter(sexo == "Mujer", region == "Región Metropolitana de Santiago")
print(gsexoF)

# Pregunta 7:¿Tiene relación el ingreso de las mujeres de la RM 
# con la riqueza del municipio donde habita?
library(ggpubr)
gsexoF7 <- ggscatter(sexoF,
                    x = "ing.comuna",
                    y = "ytot",
                    title = "Ingreso vs ingreso de comuna",
                    xlab = "Ingreso comuna [clp]",
                    ylab = "Ingreso x mujer [clp]",palette = "pastel")
print(gsexoF7)
# Pregunta 8: ¿Cómo diría que es el ingreso de las 
#mujeres de la RM (simétrico/asimétrico, concentrado/disperso,
#unimodal/multimodal, etc.)?
library("modeest")
library("dplyr")
pre8 <- group_by(fSexoF,sexo) %>%
  summarise(count = n(), mean(ytot), 
            median(ytot),sd(ytot), 
            IQR(ytot), mean(ytot),mfv(ytot)) 

#Pregunta 9: El nivel de ingreso de las mujeres de la RM
# ¿varía con el estado civil?
pre9 <- group_by(fSexoF,ecivil) %>%
  summarise( mean(ytot), 
             median(ytot),sd(ytot), 
             IQR(ytot), mean(ytot),mfv(ytot))


#Pregunta 10: Los ingresos de los habitantes de la RM, 
#van incrementandose con la edad de forma similar en hombres y mujeres
library("dplyr")
library(ggpubr)
library(ggplot2)
sexoM2<-data %>% filter(sexo == "Hombre",
                        region == "Región Metropolitana de Santiago")
sexoF2<-data %>% filter(sexo == "Mujer", 
                        region == "Región Metropolitana de Santiago")
sexoF2 <- sexoF2 %>% arrange(sexoF2$edad)
sexoM2 <- sexoM2 %>% arrange(sexoM2$edad)
gF2 <- ggbarplot(sexoF2 , y ="ytot" ,x = "edad",fill = "edad",ylab="ingreso",
                title = "Ingresos segun edad en mujeres",palette = "pastel" )
gM2 <- ggbarplot(sexoM2 , y ="ytot" ,x = "edad",fill = "edad",ylab="ingreso",
                 title = "Ingresos segun edad en hombres",palette = "pastel" )
gFM2 <- ggarrange(gF2 ,gM2,common.legend = TRUE)
print(gFM2)



