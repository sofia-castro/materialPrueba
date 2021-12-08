#PEP 1: 2020
#PREGUNTA 1
#Dado que el estudio propone que no hay diferencias en los niveles de creatina quinasa 
#entre los pacientes que no padecen de anemia y los que si sufren de esta. Se propene realizar 
#un analisis referencial, para comprobar la veracidad de el estudio.
#
library(dplyr)
set.seed(127)
#Se procese a leer el archivo con los datos proporcionados.
datos <- read.csv(file= "/Users/macbookair/Downloads/IME/PEP1/Pep1-IME-Grupo-9/Scripts/IME-2020-2-PE1-datos.csv")
#Se genera un filtro de los datos para obtener a la personas que poseen anemia.
pconAnemia <- datos %>% filter(anaemia=="1")
#Se genera un filtro de los datos para obtener a la personas que no poseen anemia.
psinAnemia <- datos %>% filter(anaemia=="0")
#Se genera una muestra (tamano 25) aleatoria de las personas con anemia
cAnCreati <- pconAnemia %>% select(creatinine_phosphokinase)
cAnCreati <- sample(cAnCreati[,1],size=25)
#Se genera una muestra (tamano 25) aleatoria de las personas sin anemia
sAnCreati <- psinAnemia %>% select(creatinine_phosphokinase)
sAnCreati <- sample(sAnCreati[,1],size=25)

#Se propone evaluar el promedio de los niveles de la creatina quinasa mediante 
#la inferencia de hipotesis, utilizando la diferencia de estas entre las muestras obtenidas,
#donde la nula representa la igualdad de los promedios
#y alternativa sugiere que existe una diferencia entres lo niveles de la creatina quinasa.
#H0: La diferencia entre los niveles de creatina quinasa entre las personas que poseen anemia
#y no poseen anemia es 0 -> u=0
#HA: La diferencia entre los niveles de creatina quinasa entre las personas que poseen anemia
#y no poseen anemia es distinta a 0 -> u =! 0
#Se define el nivel de significacion
alfa <- 0.05
#se define u nula
u_nula=0
#Se procede a evaluar si las observaciones  poseen una distribucion normal
test1normalidadCAC <- shapiro.test(cAnCreati)
print(test1normalidadCAC)
test2normalidadSAC <- shapiro.test(sAnCreati)
print(test2normalidadSAC)
#Ambas muestras poseen un p>alfa, por lo que se confirma la distribucion normal en ambas.
#Como el tamano de la muestra es menor a 30, no se puede implementar la prueba z, sin embargo
#se implementa una prueba t.test para muestras independientes, cumpliendo los requisitos 
#necesarios para esta.
test_t <- t.test(x = cAnCreati, y = sAnCreati,
                 alternative = "two.sided",
                 mu = u_nula, paired = FALSE,
                 conf.level = 1-alfa)
print(test_t)

#Como resultado se obtiene que p es menor a alfa, por lo que se obtiene suficiente evidencia
#para rechazar la hipotesis nula en favor de la hipotesis alternativa, determinando que existe 
#diferencia en  los niveles de creatina quinasa para pacientes con y sin anemia, contradiciendo
#los resutados obtenidos por el estudio dado en el enunciado.

#PREGUNTA 2:
#Como se trata un problema de proporciones, y las observaciones son independientes entre si,
#se procede a implementar el metodo de Wald
#H0: Se tiene probabilidad de  p=2/3
#Ha: Se tiene probabilidad de  p !=2/3
set.seed(439)
alfa <- 0.01
n<-60
hipert <- datos %>% select(high_blood_pressure)
mhipert <-  sample(hipert[,1],size=n)
mhipert <- data.frame(mhipert)
p_nulo <-2/3
n_p <- mhipert %>% filter(mhipert=="1")
n_p <- nrow(n_p)
p_exito <-  n_p/n
p_fallo <- 1-p_exito
error_est_hip <- sqrt (( p_nulo * p_fallo) / n)
Z <- (p_exito - p_nulo ) / error_est_hip 
p <- pnorm (Z, lower.tail = FALSE )
cat ("Hipótesis alternativa unilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)
#Dado que p>alfa no se puede rechazar la hipotesis nula en favor de la alternativa, resultando
# en concordar con el estudio dado.

#PREGUNTA 3
#Un experimento es verificar las personas vacunadas han presentado sintomasde ansiedad durante 
#el primer mes de cuarentena y el 4 mes.
#              1 mes de cuarentena      4 meses de cuarentena
#persona 1              SI                         SI
#persona 2              NO                         SI
#persona 3              NO                         SI
#persona 4              NO                         NO
#.
#.
#persona n >25          SI                         NO
#El estudio permitiría usar una prueba de McNemar, ya que se está comparando una
#variable dicotómica (tiene dansiedad o no tiene ansiedad) en un antes y un
#después de una situación, en este caso, a un  mes de cuarentena (o al
#comienzo de las cuarentenas) y a los 4 meses de cuarentena.

#Para que sea factible aplicar un test de McNemar, la cantidad de observaciones
#que pasan de SI a NO o de NO a SI en la tabla (es decir, en el contexto del
#problema, que pasan de no tener depresión a tener ansiedad, y viceversa) debe
#ser mayor 25.

#H0: La cantidad de personas que presentan ansiedad en estas comunas no presenta
#   variación luego de los 3 meses de pandemia.
#HA: La cantidad de personas que presentan ansiedad en estas comunas presenta
#    una variación luego de los 3 meses de pandemia.


#PREGUNTA 4
#caracteristicas distintas:
#1. la media de la creatina quinasa de un persona que no fuma es mayor que la media 
#de una persona que si fuma.

#2. Los valores extremos en la creatina quinasa de las personas no fumadoras 
#es reducida en comparacion a los fumadores, se tiene aproximadamente 550-350 y 600-150 
#respectivamente. Observando asi una mayor dispersion en los valores de las personas fumadoras
#que la no fumadoras.

#3. Los bigotes o el rango IQR se considera mayor en los no fumadores que en los fumadores,
#permitiendo asi un limite amplio para los valores de los no fumadores.



