#ANOVA DE UNA VIA PARA MUESTRAS INDEPENDIENTES:
library(tidyverse)
library(ggpubr)
library(DescTools)
library(ez)
# Crear el data frame en formato ancho .
A <- c(23 , 19, 25, 23, 20)
B <- c(26 , 24, 28, 23, 29)
C <- c(19 , 24, 20, 21, 17)
datos <- data.frame(A, B, C)
# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer(c("A", "B", "C"),
                                names_to = "algoritmo",
                                values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["instancia"]] <- factor(1: nrow(datos))

# Comprobcion de normalidad .
g <- ggqqplot(datos ,
              x = "tiempo",
              y = "algoritmo",
              color = "algoritmo")

g <- g + facet_wrap(~ algoritmo )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.titles")
print(g)
# Procedimiento ANOVA con aov ().
cat ("Procedimiento ANOVA usando aov\n\n")
anova <- aov( tiempo~algoritmo , data = datos )
print(summary(anova))

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba2 <- ezANOVA(  data = datos ,
                     dv = tiempo ,
                     between = algoritmo ,
                     wid = instancia ,
                     return _aov = TRUE )

print(prueba2)

# Grá fico del tama ño del efecto .
g2 <- ezPlot( data = datos ,
              dv = tiempo ,
              wid = instancia ,
              between = algoritmo ,
              y_lab = " Tiempo promedio de ejecuci ón [ms]",
              x = algoritmo)
print(g2)

#PRUEBA DE ANALISIS DE POST-HOC
library(tidyverse )
# Crear el data frame en formato ancho .
A <- c(23 , 19, 25, 23, 20)
B <- c(26 , 24, 28, 23, 29)
C <- c(19 , 24, 20, 21, 17)
datos <- data.frame(A, B, C)
# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer(c("A", "B", "C"),
                                names_to = "algoritmo",
                                values_to = "tiempo")

datos [["algoritmo"]] <- factor(datos[["algoritmo"]])
datos [["instancia"]] <- factor(1:nrow(datos))

# Establecer nivel de significaci ón (el mismo usado en ANOVA ).
alfa <- 0.025
# Procedimiento post -hoc de Bonferroni .
cat(" Procedimiento post -hoc de Bonferroni \n\n")

bonferroni <- pairwise.t.test( datos[["tiempo"]],
                               datos[["algoritmo"]],
                               p.adj = "bonferroni",
                               pool.sd = TRUE ,
                               paired = FALSE ,
                               conf.level = 1 - alfa )
print ( bonferroni )

# Procedimiento post -hoc de Holm .
cat ("\n\ nProcedimiento post - hoc de Holm \n\n")
holm <- pairwise.t.test( datos[["tiempo"]],
                         datos[["algoritmo"]],
                         p.adj = "holm",
                         pool.sd = TRUE ,
                         paired = FALSE ,
                         conf.level = 1 - alfa )

print(holm)
# Prueba HSD de Tukey .
post_hoc <- TukeyHSD(anova ,
                    "algoritmo",
                     ordered = TRUE ,
                     conf.level = 1 - alfa )
print( post_hoc)
# Crear matriz de contrastes .
contrastes <- matrix(c(1, -1, 0,
                           1, 0, -1,
                            0, 1, -1,
                            1, -0.5, -0.5,
                            -0.5, 1, -0.5,
                            -0.5, -0.5 , 1) ,
                        nrow =6,
                          byrow = TRUE)
# Trasponer matriz de contrastes ( para que cada contraste sea una columna ).
contrastes <- t(contrastes)
# Hacer prueba de Scheff é.
scheffe <- ScheffeTest(x = anova ,
                       which = "algoritmo",
                       contrasts = contrastes ,
                       conf.level = 1 - alfa )
print(scheffe)

