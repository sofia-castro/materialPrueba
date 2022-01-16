#ANOVA PARA MUESTRAS CORRELACIONADAS:
library(tidyverse)
library(ggpubr)
library(ez)
library(nlme)
library(emmeans)

# Crear el data frame .
instancia <- factor(1:6)
Quicksort <- c(23.2 , 22.6 , 23.4 , 23.3 , 21.8 , 23.9)
Bubblesort <- c(31.6 , 29.3 , 30.7 , 30.8 , 29.8 , 30.3)
Radixsort <- c(30.1 , 28.4 , 28.7 , 28.3 , 29.9 , 29.1)
Mergesort <- c(25.0 , 25.7 , 25.7 , 23.7 , 25.5 , 24.7)
datos <- data.frame( instancia , Quicksort , Bubblesort , Radixsort , Mergesort )
# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer(c("Quicksort", "Bubblesort", "Radixsort",
                                  "Mergesort"),
                                    names_to = "algoritmo", values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Comprobción de normalidad .
g <- ggqqplot(datos , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
# Procedimiento ANOVA con aov.
cat(" Procedimiento ANOVA usando aov\n\n")
anova <- aov( tiempo~algoritmo + Error( instancia /( algoritmo)),
                    data = datos )
print( summary(anova ))
# Procedimiento ANOVA con ezANOVA ().
cat("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba2 <- ezANOVA( data = datos , dv = tiempo , within = algoritmo ,
                         wid = instancia , return_aov = TRUE )
print(summary(prueba2$aov))
cat("\n\ nPero ezANOVA entrega más informaci ón.\n")
cat("El resultado de la prueba de esfericidad de Mauchly :\n\n")
print(prueba2[[" Mauchly ’s Test for Sphericity "]])
cat("\n\nY factores de correcci ón para cuando no se cumple la\n")
cat(" condición de esfericidad :\n\n")
print(prueba2$"Sphericity Corrections")
# Grá fico del tama ño del efecto .
g2 <- ezPlot( data = datos , dv = tiempo , wid = instancia , within = algoritmo ,
               y_lab = "Tiempo promedio de ejecuci ón [ms]", x = algoritmo )
print(g2)
#PRUEBAS POST HOC


# Nivel de significación.
alfa <- 0.01

# Procedimiento post -hoc de Bonferroni .
bonferroni <- pairwise.t.test( datos[["tiempo"]], datos[["algoritmo"]],
                                      p.adj = "bonferroni", paired = TRUE )
cat(" Correcci ón de Bonferroni \n")
print( bonferroni )
# Procedimiento post -hoc de Holm .
holm <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                               p.adj = "holm", paired = TRUE )
cat("\n\ nCorrecci ón de Holm \n")
print( holm )
# Procedimiento post -hoc HSD de Tukey .
mixto <- lme( tiempo ~ algoritmo , data = datos , random = ~1|instancia )
medias <- emmeans(mixto , "algoritmo")
tukey <- pairs(medias , adjust = "tukey")
cat("\n\ nPrueba HSD de Tukey \n\n")
print(tukey)
# Procedimiento post -hoc de Scheff é
cat("\n\ nComparaci ón de Scheff é\n")
scheffe <- pairs(medias , adjust = "scheffe")
print(scheffe)
  