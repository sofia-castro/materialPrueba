library(dplyr)
nombres <- c("Chocolates","Pizza","Tiramisu","Pasta","Lentejas")
precios <- c(2000,10000,7000,9000,5000)
satisfaccion <- c("Mediana","Alta","Alta","Alta","Baja")
cantidadComprada <- c(2,1,1,1,0)
precioFinal <-c(4000,10000,7000,9000,0)
data <- data.frame(nombres,precios,satisfaccion,cantidadComprada,precioFinal,stringsAsFactors = TRUE)
print(data["cantidadComprada"])

#filtrar valores:
baja <- data %>% filter(satisfaccion == "Baja")

#crear nueva columna
nuevaColumna <- data.frame(nombres="Arroz",
                    precios = 900, satisfaccion = "Mediana", cantidadComprada = 3,
                    precioFinal = 2700)

data <- rbind(data ,nuevaColumna)

#Calcular promedio de cantidad comprada y el promedio de precios.
media1 <- mean(data[["cantidadComprada"]])
cat("Cantidad Comprada:",media1,"\n")
media2 <- mean(data[["precios"]])
cat("Precios:",media2,"\n")

#Calcular la mediana de todo
mediana1 <- median(data[["cantidadComprada"]])
cat("Cantidad Comprada:",mediana1,"\n")
mediana2 <- median(data[["precios"]])
cat("Precios:",mediana2,"\n")
mediana3 <- median(data[["precioFinal"]])
cat("Precio final:",mediana3,"\n")

#calcular la varianza de todo
cat("VARIANZA \n")
var1 <- var(data[["cantidadComprada"]])
cat("Cantidad Comprada:",var1,"\n")
var2 <- var(data[["precios"]])
cat("Precios:",var2,"\n")
var3 <- var(data[["precioFinal"]])
cat("Precio final:",var3,"\n")

#Desviacion estandar para todo
cat("Desviacion estandar \n")
desE1 <- sd(data[["cantidadComprada"]])
cat("Cantidad Comprada:",desE1,"\n")
desE2 <- sd(data[["precios"]])
cat("Precios:",desE2,"\n")
desE3 <- sd(data[["precioFinal"]])
cat("Precio final:",desE3,"\n")

#Moda para todo
library("modeest")
cat("MODA \n")
moda1 <- mfv(data[["cantidadComprada"]])
cat("Cantidad Comprada:",moda1,"\n")
moda2 <- mfv(data[["precios"]])
cat("Precios:",moda2,"\n")
moda3 <- mfv(data[["precioFinal"]])
cat("Precio final:",moda3,"\n")
moda4 <- mfv(data[["arroz"]])
cat("Precio final:",moda4,"\n")


#Calcular el rango para todo
cat("RANGO\n")
rango1 <- range(data[["cantidadComprada"]])
cat("Cantidad Comprada:",rango1,"\n")
rango2 <- range(data[["precios"]])
cat("Precios:",rango2,"\n")
rango3 <- range(data[["precioFinal"]])
cat("Precio final:",rango3,"\n")

