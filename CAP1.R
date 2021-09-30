library(dplyr)
library(tidyr)
library(magrittr)
####################################
nombre <- c("Alan Brito Delgado","Zacarías Labarca del Río","Elsa Payo Maduro")
fecha_nacimiento <- as.Date(c("2008-1-25", "2006-10-4", "2008-3-27"))
prueba_1 <- c(5.5, 3.4, 4.5)
prueba_2 <- c(3.2, 4.7, 4.1)
prueba_3 <- c(4.8, 4.3, 5.1)
dataframe <- data.frame(nombre,fecha_nacimiento,prueba_1,prueba_2,prueba_3,stringsAsFactors = FALSE)
write.csv2(dataframe , "Desktop/IME/Ejemplo.csv", row.names = FALSE)

#datos <- read.csv2(Desktop/IME/Ejemplo.csv", stringsAsFactors = FALSE)

# Eliminar del data frame la columna fecha_nacimiento.
dataframe$fecha_nacimiento <- NULL
# Agregar al data frame la columna edad.
dataframe$edad <- c(23, 25, 23)
# Crear una nueva observación.
nueva <- data.frame(nombre="Elba Calao del Río",
                    prueba_1 = 6.4, prueba_2 = 2.3, prueba_3 = 4.6,
                    edad = 24)

# Agregar la nueva observación
dataframe <- rbind(dataframe ,nueva)
# Eliminar las primeras 3 observaciones del data frame.
#dataframe <- dataframe[-c(1:3),]

####################################
datos <- iris
versicolor <- datos %>% filter(Species == "versicolor")
largas <- datos %>% filter(Species == "versicolor" & Sepal.Length >= 6)
petalos <- datos %>% select(Species, starts_with("Petal"))
anchos <- datos %>% select(ends_with("Width"), Species)
petalos <- petalos %>% mutate(Species, Petal.Width, Petal.Ratio = Petal.Length / Petal.Width)
petalos <- petalos %>% arrange(desc(Petal.Ratio))
petalos <- petalos %>% arrange(Petal.Length)

####################################
Instancia <- 1:6
Quicksort <- c(23.2, 22.6, 23.4, 23.3, 21.8, 23.9)
Bubblesort <- c(31.6, 29.3, 30.7, 30.8, 29.8, 30.3)
Radixsort <- c(30.1, 28.4, 28.7, 28.3, 29.9, 29.1)
Mergesort <- c(25.0, 25.7, 25.7, 23.7, 25.5, 24.7)
datos2 <- data.frame(Instancia , Quicksort , Bubblesort , Radixsort , Mergesort)
#Mostrar las primeras filas de la matriz de datos.
cat("Datos originales\n")
print(head(datos2))
cat("\n")
#Convertir la matriz de datos a formato largo.
datos_largos <- datos2 %>% pivot_longer(c("Quicksort", "Bubblesort", "Radixsort", "Mergesort"), names_to = "Algoritmo",
    values_to = "Tiempo")
# Mostrar las primeras filas de la matriz de datos largos.
cat("Datos largos\n")
print(head(datos_largos))
cat("\n")
# Convertir la matriz de datos largos a formato ancho.
datos_anchos <- datos_largos %>% pivot_wider(names_from = "Algoritmo", values_from = "Tiempo")
# Mostrar las primeras filas de la matriz de datos largos.
cat("Datos anchos\n")
print(head(datos_anchos))
cat("\n")

####################################
library(dplyr)
library(magrittr)
datos3 <- mtcars
datos3 <- datos3 %>% rename(Rendimiento = mpg, Cilindrada = cyl, Desplazamiento = disp, Potencia = hp,
                          Eje = drat, Peso = wt, Cuarto_milla = qsec, Motor = vs ,
                          Transmision = am , Cambios = gear , Carburadores = carb)
cat("1\n")
print(datos)
cat("\n")
datos3[["Motor"]] <- factor(datos[["Motor"]], levels = c(0, 1),labels = c("V", "Recto")) 
datos3[["Transmision"]] <- factor(datos3[["Transmision"]], levels = c(0, 1), labels = c("Automático", "Manual"))
datos3[["Cilindrada"]] <- factor(datos3[["Cilindrada"]], levels = c(4, 6, 8),
                                labels = c("4 cilindros", "6 cilindros", "8 cilindros"),
                                ordered = TRUE)
datos3[["Cambios"]] <- factor(datos3[["Cambios"]], levels = c(3, 4, 5),
                             labels = c("3 cambios", "4 cambios", "5 cambios"),
                             ordered = TRUE) 
