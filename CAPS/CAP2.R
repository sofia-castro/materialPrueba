library(dplyr)
datos <- mtcars
datos <- datos %>% rename(Rendimiento = mpg, Cilindrada = cyl, Desplazamiento = disp, Potencia = hp,
                            Eje = drat, Peso = wt, Cuarto_milla = qsec, Motor = vs ,
                            Transmision = am , Cambios = gear , Carburadores = carb)
#calcular la media para la variable rendimiento
media <- mean(datos[["Rendimiento"]])
cat("Rendimiento medio:", media, "\n\n")

# Calcular la media para la tercera y quinta columnas
# (variables Desplazamiento y Eje).
cat("Medias\n")
print(sapply(datos[c(3, 5)], mean))
cat("\n")

# Calcular la media para las columnas 3 a 6
cat("Medias\n")
print(sapply(datos[3:6], mean))
cat("\n")

#Calcular la media para la variable Rendimiento omitiendo valores faltantes.
print(mean(datos[["Rendimiento"]], na.rm = TRUE))

#Calcular la desviacion estandar
cat("desviacion estandar\n")
print(sd(datos[["Rendimiento"]]))
cat("\n")

#Calcular la varianza
cat("varianza\n")
print(var(datos[["Rendimiento"]]))
cat("\n")

#calcular la moda 
library("modeest")
cat("Moda\n")
print(mfv(datos[["Rendimiento"]]))
cat("\n")

#Calcular el rango
cat("Rango\n")
print(range(datos[["Rendimiento"]]))
cat("\n")

# Cálculo de percentiles para la variable Rendimiento.
cat("Cuartiles:\n")
print(quantile(datos[["Rendimiento"]]))
cat("\n")

cat("Quintiles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.2)))
cat("\n")

cat("Deciles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.1)))
cat("\n")

cat("Percentiles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.01)))
cat("\n")

cat("IQR:\n")
print(IQR(datos[["Rendimiento"]]))
cat("\n")

library(dplyr)
# Cálculo de varias medidas para la variable Potencia.
medidas_potencia <- datos %>% summarise(Media = mean(Potencia), Mediana = median(Potencia),
                                        Varianza = var(Potencia), IQR = IQR(Potencia))
print(medidas_potencia)
cat("\n")
# Cálculo de la media y la desviación estándar para las variables Peso y # Cuarto_milla.
medidas_varias <- datos %>% summarise(Media_P = mean(Peso),
                                      Media_C = median(Cuarto_milla), SD_P = sd(Peso),
                                      SD_C = sd(Cuarto_milla))
print(medidas_varias)
cat("\n")

#Crear tabla de contingencia para la variable gear.
contingencia <- table(datos[["Cambios"]])
cat("Tabla de contingencia generada con table():\n")
print(contingencia)
cat("\n")
#Otra forma de crear la misma tabla.
contingencia <- xtabs(~ Cambios, data = datos)
cat("Tabla de contingencia generada con xtabs():\n")
print(contingencia)
cat("\n")

#Calcular totales por fila y mostrarlos por separado.
totales <- marginSums(contingencia)
cat("Totales por fila:\n")
print(totales)
cat("\n")

#Calcular totales por fila y agregarlos a la tabla.
con_totales <- addmargins(contingencia , 1)
cat("Tabla de contingencia con totales por fila:\n")
print(con_totales)
cat("\n")

# Convertir a tabla de proporciones
proporciones <- prop.table(contingencia)
proporciones <- addmargins(proporciones , 1)
cat("Tabla de contingencia con proporciones:\n")
print(proporciones)
cat("\n")

# Convertir a tabla de porcentajes con 2 decimales.
porcentajes <- round(prop.table(contingencia), 4) * 100 
porcentajes <- addmargins(porcentajes)
cat("Tabla de contingencia con porcentajes:\n")
print(porcentajes)
cat("\n")

# Crear tabla de contingencia para las variables Transmision y gear.
contingencia <- table(datos[["Transmision"]], datos[["Cambios"]])
cat("Tabla de contingencia generada con table():\n")
print(contingencia)
cat("\n")

# Otra forma de crear la misma tabla.
contingencia <- xtabs(~ Transmision + Cambios, data = datos)
cat("Tabla de contingencia generada con xtabs():\n")
print(contingencia)
cat("\n")

# Proporciones con totales por fila.
proporciones_fila <- prop.table(contingencia , margin=1)
proporciones_fila <- addmargins(proporciones_fila, margin=2)
cat("Tabla de contingencia con proporciones totales por fila:\n")
print(proporciones_fila)
cat("\n")

# Proporciones con totales por columna.
proporciones_columna <- prop.table(contingencia , margin=2)
proporciones_columna <- addmargins(proporciones_columna, margin=1)
cat("Tabla de contingencia con proporciones totales por columna:\n")
print(proporciones_columna)
cat("\n")

# Proporciones con totales.
proporciones <- prop.table(contingencia)
proporciones <- addmargins(proporciones)
cat("Tabla de contingencia con proporciones totales:\n")
print(proporciones)
cat("\n")

# Convertir la variable Cambios en categórica.
datos[["Cambios"]] <- factor(datos[["Cambios"]])

# Crear tabla de contingencia para las variables Transmision,
# Cambios y Motor.
contingencia <- ftable(datos[["Transmision"]], datos[["Cambios"]],
                        datos[["Motor"]])

cat("Tabla de contingencia generada con ftable():\n")
print(contingencia)
cat("\n")
# Otra forma de crear la misma tabla.
contingencia <-xtabs(~ Cambios + Transmision + Motor, data = datos)
cat("Tabla de contingencia generada con xtabs():\n")
print(contingencia)
cat("\n")

library(dplyr)
resumen <- group_by(datos, Cambios) %>% summarise(count = n(), mean(Rendimiento), 
                                                  median(Rendimiento),sd(Rendimiento), 
                                                  IQR(Rendimiento), mean(Potencia)) 
print(resumen)

library(ggpubr)
# Histograma para la variable Rendimiento.
g1 <- gghistogram(datos,
                  x = "Rendimiento",
                  bins = 10, add = "mean",xlab = "Rendimiento [Millas/galón]", ylab = "Frecuencia",
                  color = "blue",
                  fill = "blue")
print(g1)
library(ggpubr)
# Histograma para la variable Potencia.
g2 <- gghistogram(datos,
                  x = "Potencia",
                  bins = 10,
                  add = "mean",
                  xlab = "Potencia [hp]", ylab = "Frecuencia", color = "red",
                  fill = "yellow")
print(g2)

library(ggpubr)
# Cargar datos.
g3 <- ggboxplot(datos[["Potencia"]],color = "red",fill = "pink",ylab = "Potencia [hp]")
g3 <- g3 + rremove("x.ticks")
g3 <- g3 + rremove("x.text")
g3 <- g3 + rremove("x.title")
print(g3)

library(ggpubr)
# Cargar datos.
# Crear la tabla de frecuencias para la variable Cambios y convertirla a
# data frame.
contingencia <- as.data.frame(xtabs(~ Cambios, data = datos))
# Crear el gráico de barras.
g4 <- ggbarplot(contingencia , x = "Cambios",
            y = "Freq",
            fill = c("brown", "purple", "orange"),
            title = "Cantidad de cambios de los automóviles", xlab = "Cantidad de cambios",
            ylab = "Frecuencia")
 print(g4)
 
 library(ggpubr)
 #Crear gráfico de torta.
 contingencia <- as.data.frame(xtabs(~ Cambios, data = datos))
 g5 <- ggpie(contingencia , x = "Freq", label = "Cambios",
         fill = c("red", "yellow", "green"), title = "Cantidad de cambios de los automoviles", lab.pos = "in")
 print(g5)  
 
 library(ggpubr)
 g6 <-ggscatter(datos ,
             x = "Rendimiento",
             y = "Peso",
             color = "red",
             title = "Rendimiento v/s peso",
             xlab = "Rendimiento [millas/galón]", ylab = "Peso [1000 lb]")
 print(g6)
 
 library(ggpubr)
 g7 <- ggscatter(datos,
                   x = "Peso",
                    y = "Cuarto_milla",
                    color = "blue",
                    title = "Independientes",
                    xlab = "Peso [1000 lb]",
                    ylab = "Tiempo para recorrer un cuarto de milla [s]")
 
 # Gráfico para variables con asociación positiva.
 g8 <- ggscatter(datos,
                     x = "Peso",
                     y = "Potencia",
                     color = "orange",
                     title = "Asociación positiva",
                     xlab = "Peso [1000 lb]",
                     ylab = "Potencia [hp]")
 # Gráfico para variables con asociación negativa.
 g9 <- ggscatter(datos,
                     x = "Peso",
                     y = "Rendimiento",
                     color = "black",
                     title = "Asociación negativa",
                     xlab = "Peso [1000 lb]",
                     ylab = "Rendimiento [millas/galón]")

 # Crear figura con tres gráficos.
 g10 <- ggarrange(g7 ,g8 ,g9, ncol = 3, nrow = 1, common.legend = TRUE)
 
print(g10)
 
library(ggpubr)
# Crear tabla de contingencia para las variables Motor y Cambios,
# y guardarla como data frame.
tabla <- xtabs(~ Motor + Cambios, data = datos)
contingencia <- as.data.frame(tabla)
# Crear tabla de proporciones por columnas y guardarla como
 # data frame.
 proporciones <- as.data.frame(prop.table(tabla , margin = 2))
 # Crear gráfico de barras segmentadas.
 g11 <- ggplot(contingencia, aes(fill = Motor, y = Freq, x = Cambios))
 g11 <- g11 + geom_bar(position = "stack", stat = "identity")
 g11 <- g11 + labs(y = "Frecuencia") + ggtitle("Barras apiladas")
 g11 <- g11 + theme_pubr()

 # Crear gráfico de barras agrupadas.
  g12 <- ggplot(contingencia, aes(fill = Motor, y = Freq, x = Cambios))
 g12 <- g12 + geom_bar(position = "dodge", stat = "identity")
 g12 <- g12 + labs(y = "Frecuencia") + ggtitle("Barras agrupadas")
 g12 <- g12 + theme_pubr()

 # # Crear gráfico de barras segmentadas estandarizado.
 g13 <- ggplot(contingencia, aes(fill = Motor, y = Freq, x = Cambios))
 g13 <- g13 + geom_bar(position = "fill", stat = "identity")
 g13 <- g13 + labs(y = "Frecuencia") + ggtitle("Barras estandarizadas")
 g13 <- g13 + theme_pubr()

 # Crear una figura que contenga los tres gráficos.
 g14 <- ggarrange(g11, g12, g13, nrow = 1, common.legend = TRUE)

 # Agregar un título común en negrita y con fuente de 24 puntos.
 titulo <- text_grob("Tipo de motor por cantidad de Cambios",
                        face = "bold", size = 24)
 g14 <- annotate_figure(g14, top = titulo)
 print(g14)
 
 library(ggpubr)
 # Crear gráfico de mosaico. y guardarla como data frame.
  tabla1 <- xtabs(~ Cambios + Motor, data = datos)
  contingencia <- as.data.frame(tabla1)
  g15 <- ggplot(data = contingencia)
  g15 <- g15 + geom_mosaic(aes(weight = Freq, x = product(Cambios), fill = Motor)) 
  g15 <- g15 + labs(y = "Motor", x = "Cambios",
                    title = "Tipo de motor por cantidad de cambios")
  g15 <- g15 + scale_fill_manual(values=c("orange", "purple"))
  print(g15)
  
  library(ggpubr)
  g16 <- ggboxplot(datos, x = "Cambios", y = "Rendimiento",
                 palette = c("light blue", "pink", "yellow"), fill = "Cambios",
                 title = "Rendimiento por cantidad de cambios", xlab = "Cambios",
                 ylab = "Rendimiento [millas/galón]")
  print(g16)
  
  library(ggpubr)
  g17 <- ggstripchart(datos, x = "Cambios",
                    y = "Rendimiento",
                    palette = c("blue", "red", "dark green"),
                    color = "Cambios",
                    title = "Rendimiento por cantidad de cambios",
                    xlab = "Cambios",
                    ylab = "Rendimiento [millas/galón]")
  print(g17)