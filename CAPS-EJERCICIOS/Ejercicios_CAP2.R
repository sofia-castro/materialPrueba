data <- chickwts
#¿Qué son los cuartiles y cómo se pueden obtener para los pesos de los pollitos reportados en la columna weight?
cuartiles <-quantile(data[["weight"]])
cat("Cuartiles preso:\n",cuartiles)

#¿Cómo obtener un histograma de los pesos de los pollitos?
library(ggpubr)
library(ggplot2)
g1 <- gghistogram(data,
                  x = "weight",
                  bins = 10, add = "mean",xlab = "peso", ylab = "Frecuencia",
                  color = "blue",
                  fill = "blue")
print(g1)
#¿Cómo se obtiene un gráfico de cajas para comparar los pesos de los pollitos por tipo de alimento
#suministrado?
library(ggpubr)
library(ggplot2)
g16 <- ggboxplot(data, x = "feed", y = "weight", fill = "feed",
                 title = "Peso por tipo de alimento", xlab = "Alimento",
                 ylab = "Peso")
print(g16)
data<-data.frame(as.Date.default(sub('X', '', colnames(datos[2,2:571]))), sep = ",",stringsAsFactors = FALSE)