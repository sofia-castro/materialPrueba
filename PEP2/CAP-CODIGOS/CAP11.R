#CAPÍTULO 11. INFERENCIA NO PARAMÉTRICA CON MEDIANAS
#PRUEBAS PARA UNA O DOS MUESTRAS
#Prueba de Mann-Whitney
# Ingresar los datos .
a <- c(2.7 , 6.6 , 1.6 , 5.1 , 3.7 , 6.1 , 5.0 , 1.4 , 1.8 , 1.5 , 3.0 , 5.3)
b <- c(5.0 , 1.4 , 5.6 , 4.6 , 6.7 , 2.7 , 1.3 , 6.3 , 3.7 , 1.3 , 6.8)
# Establecer nivel de significaci ón.
alfa <- 0.05
# Hacer la prueba de Mann - Whitney .
prueba <- wilcox.test(a, b, alternative = "two.sided", conf.level = 1-alfa )
print(prueba)

#Prueba de rangos con signo de Wilcoxon
# Ingresar los datos .
a <- c(2.9 , 6.1 , 6.7 , 4.7 , 6.4 , 5.7 , 2.7 , 6.9 , 1.7 , 6.4)
b <- c(6.0 , 2.8 , 1.3 , 4.7 , 3.1 , 1.8 , 2.9 , 4.0 , 2.3 , 1.6)
# Establecer nivel de significaci ón.
alfa <- 0.05
# Hacer la prueba de rangos con signo de Wilcoxon .
prueba <- wilcox.test(a, b, alternative = " greater ", paired = TRUE ,
                      conf.level = 1 - alfa )
print(prueba)

#PRUEBAS PARA MÁS DE DOS MUESTRAS
# Construir la mariz de datos .
A <- c(24 , 23, 26, 21, 24, 24, 25, 22, 23, 22, 23, 23)
B <- c(17 , 15, 18, 20, 19, 21, 20, 18, 19)
C <- c(10 , 11, 14, 11, 15, 12, 12, 10, 9, 13, 12, 12, 10, 10)
D <- c(18 , 16, 18, 15, 16, 15, 18, 16)
Tiempo <- c(A, B, C, D)
Algoritmo <- c(rep("A", length (A)),
               rep ("B", length (B)),
               rep ("C", length (C)),
               rep ("D", length (D)))

Algoritmo <- factor(Algoritmo)
datos <- data.frame(Tiempo,Algoritmo)
# Establecer nivel de significación
alfa <- 0.01
# Hacer la prueba de Kruskal - Wallis .
prueba <- kruskal.test( Tiempo ~ Algoritmo , data = datos )
print( prueba )
# Efectuar procedimiento post -hoc de Holm si se encuentran diferencias
# significativas .
if( prueba$p.value < alfa ) {
post_hoc <- pairwise.wilcox.test(datos$Tiempo ,
                                 datos$Algoritmo ,
                                 p.adjust.method = "holm",
                                 paired = FALSE )
print(post_hoc)
}
#Prueba de Friedman
# Construir la mariz de datos .
A <- c(21 , 10, 7, 21, 24, 27, 17)
B <- c(6, 21, 18, 7, 24, 13, 13)
C <- c(13 , 25, 18, 20, 24, 8, 29)
Puntuacion <- c(A, B, C)
Interfaz <- c( rep("A", length (A)),
               rep("B", length (B)),
               rep("C", length (C)))
Sujeto <- rep(1:7,3)
Interfaz <- factor( Interfaz )
datos <- data.frame(Sujeto,Puntuacion,Interfaz )
 # Establecer nivel de significaci ón
alfa <- 0.05

# Hacer la prueba de Friedman .
prueba <- friedman.test( Puntuacion ~ Interfaz | Sujeto , data = datos )
print(prueba)
# Efectuar procedimiento post -hoc de Holm si se encuentran diferencias
# significativas .
if( prueba $p.value < alfa ) {
post_hoc <- pairwise.wilcox.test( datos$Puntuacion ,
                                  datos$Interfaz ,
                                  p.adjust.method = "holm",
                                  paired = TRUE )
print(post_hoc)
}
