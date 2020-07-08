#REPLICAR EL TEOREMA DEL LÍMITE CENTRAL 
rm(list = ls())
library(ggplot2)

#Ejemplo 1: Poisson
#------------------------------------------
nsim     <- 10000 #Número de simulaciones
media    <- 5.2   #lambda
varianza <- media #lambda
Z        <- rep(NA, 1000) #Vector donde guardo las aprox normales
for (i in 1:1000){
  Xi       <- rpois(nsim, lambda = media) #(X1, X2, ..., Xn)
  mediaXi  <- sum(Xi)/nsim                #Media muestral
  Z[i]     <- (mediaXi - media)*sqrt(nsim/varianza) #calculo la aproximada normal
}
x <- seq(-2.5, 2.5, length.out = 200)


ggplot() +
  geom_histogram(aes(x = Z, y = ..density..), fill = "red") +
  geom_line(aes(x = x, y = dnorm(x)))

#Ejemplo 2: Exponenciales
#------------------------------------------
nsim     <- 10000
mu       <- 1/5
sigma2   <- (1/5)^2
Z        <- rep(NA, 1000)
for (i in 1:1000){
  Xi      <- rexp(nsim, rate = 5)
  mediaXi <- mean(Xi)
  Z[i]    <- mediaXi
}

#REGRESAMOS 8:10
x <- seq(min(Z), max(Z), length.out = 100)
ggplot() +
  geom_histogram(aes(x = Z, y = ..density..), fill = "red",
                 bins = 100) +
  geom_line(aes(x = x, y = dnorm(x, mu, sqrt(sigma2/nsim))))

#Cálculo de intervalos de confianza
N             <- 1000
base.completa <- data.frame(Ingreso = rexp(N, 1/10))
total.pob     <- sum(base.completa$Ingreso) 
n             <- 100
muestra       <- sample(base.completa$Ingreso, n, replace = FALSE)
tgorro        <- N/n*sum(muestra)
var.gorro     <- N^2*(1 - n/N)/n*var(muestra)
var.buena     <- N^2*(1 - n/N)/n*var(base.completa$Ingreso)

#Intervalo de confianza al 90%
alpha.val     <- 0.1
Zalpha        <- qnorm(1 - alpha.val/2)
Lintervalo    <- tgorro - Zalpha*sqrt(var.gorro)
Uintervalo    <- tgorro + Zalpha*sqrt(var.gorro)
print(paste0("[", Lintervalo,",", Uintervalo,"]"))






