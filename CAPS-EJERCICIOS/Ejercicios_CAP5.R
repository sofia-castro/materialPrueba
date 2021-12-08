#EJERCICIO 2
Ej2<- c(140.6, 133.3, 142.4, 86.4, 129.9, 110.8, 133.2, 129.1, 142.5, 150.2,
        141.6, 111.0, 127.2, 137.9, 131.9, 121.9)
valor_nulo<-120
normalidad_1 <- shapiro.test(Ej2)
print(normalidad_1)
alfa<-0.05
prueba_t<-t.test(Ej2,
                 alternative = "two.sided",
                 mu = valor_nulo, conf.level = 1 - alfa)
print(prueba_t)
#se rechaza hipotesis nula y se acepta la hipotesis alternativa.

#EJERCICIO 3
catalizador_1<-c(62.9, 67.2, 67.4, 67.4, 67.2, 64.6, 69.6, 65.7, 68.2, 72.0)
catalizador_2<-c(66.8, 69.3, 69.6, 67.3, 68.8, 68.4, 68.6, 70.3, 69.6, 71.7)
diferencia <- catalizador_1-catalizador_2
normalidad_2 <- shapiro.test(diferencia)
print(normalidad_2)
prueba_2 <-t.test(x=catalizador_1,y=catalizador_2 ,paired=TRUE,
         alternative = "two.sided",
         mu = 0, conf.level = 1 - alfa)
print(prueba_2)
#EJERCICIO 4
radolmes_1<- c( 105.6, 100.1, 90.9, 105.0, 91.2, 99.6, 96.9, 107.7, 96.5, 103.3, 91.3, 92.4)
radolmes_1<-append(radolmes_1,0)
radolmes_1<-append(radolmes_1,0)
radolmes_2<- c( 98.9, 94.3, 95.9, 107.7, 102.0, 94.2, 100.6, 98.5, 99.1, 101.3, 94.4, 103.6,
                95.3, 106.7)
prueba_3 <-t.test(x=radolmes_1,y=radolmes_2 ,paired=TRUE,
                  alternative = "two.sided",
                  mu = 0, conf.level = 1 - alfa)
print(prueba_3)
