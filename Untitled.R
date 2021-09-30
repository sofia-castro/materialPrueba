poblacion <- read.csv(file="Desktop/IME/materialPrueba/EP-02_Datos_Casen_2017.csv",sep = ",")
# basename <- "Casen 2017.csv"
# file <- file.path(dir, basename)
# poblacion <- read.csv(file = file)
tamano <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80] tamano.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado ) set.seed(133)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)