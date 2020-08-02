rm(list = ls())

#Bootstrap de algo Poisson
n <- 20         #Obtengo una muestra de tamaño 20
muestra       <- rpois(n, 1/5)
alpha.val     <- 0.05 #Intervalos al 95
nsim          <- 1000
media.muestra <- mean(muestra)
media.boots   <- rep(NA, nsim)
for (i in 1:nsim){
  remuestreo       <- sample(muestra, n, replace = TRUE)
  media.boots[i]   <- mean(remuestreo)
}

lado.sup <- quantile(media.boots - media.muestra, alpha.val/2) #delta1
lado.inf <- quantile(media.boots - media.muestra, 1 - alpha.val/2) #delta2

#Intervalos:
c(media.muestra - lado.inf, media.muestra - lado.sup)

#Bootstrap para una población finita (mediana)
#Suponer existe un único estrato y es este:
poblacion <- c(rgamma(100, 1, 1), rexp(100), rpois(100, 2))
N         <- length(poblacion)

#Verdadera mediana
mediana.real <- median(poblacion)

#Obtener una muestra de tamaño n = 25. Y calcular IC con Bootstrap.
n <- 25
muestra <- sample(poblacion, n, replace = FALSE)
mediana.muestral <- median(muestra)
mediana.boot <- rep(NA, nsim)

#Bootstrap
k.val <- floor(N/n)
Uf    <- rep(muestra, k.val)

for (i in 1:nsim){
  Uc              <- sample(muestra, N - n*k.val, replace = TRUE)
  Uboot           <- c(Uf, Uc)#población sintética
  remuestreo      <- sample(Uboot, n, replace = FALSE)
  mediana.boot[i] <- median(remuestreo)
}

lado.sup <- quantile(mediana.boot - mediana.muestral, alpha.val/2)
lado.inf <- quantile(mediana.boot - mediana.muestral, 1 - alpha.val/2)

c(mediana.muestral - lado.inf, mediana.muestral - lado.sup)

#Muestreo por clisters
escuelas <- paste0("Prepatoria ", 1:30)
datos.poblacion <- data.frame(
  Escuela = sample(escuelas, 1000, replace = TRUE),
  Promedio = runif(1000, 6, 10)
)

n <- 5 #Muestra de tamaño 5
escuelas.seleccionadas <- sample(escuelas, n)

muestra <- datos.poblacion %>% filter(Escuela %in% escuelas.seleccionadas) 


