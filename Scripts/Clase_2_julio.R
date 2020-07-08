rm(list = ls())
set.seed(356781)
library(ggplot2)

nsim <- 10000
N    <- 2000
n    <- 100

base.completa <- data.frame(x = rnorm(N)) #universo
total         <- sum(base.completa$x)
muestra       <- sample(base.completa$x, n)
estimador.t   <- N*mean(muestra)
total.muestra <- rep(NA, nsim)
for (i in 1:nsim){
  muestra          <- sample(base.completa$x, n)
  total.muestra[i] <- N*mean(muestra)
}
mean(total.muestra)

ggplot() +
  geom_histogram(aes(x = total.muestra, y = ..density..), fill = "purple",
                 color = "black", bins = 20) +
  geom_vline(aes(xintercept = total), linetype = "dashed",
             color = "white") + 
  theme_classic()

f <- n/N
varianza.teorica  <- N^2*(1 - f)/n*var(base.completa$x)
var(total.muestra)
varianza.estimada <-  N^2*(1 - f)/n*var(muestra)

varianza <- rep(NA, nsim)
for (i in 1:nsim){
  muestra     <- sample(base.completa$x, n)
  varianza[i] <- N^2*(1 - f)/n*var(muestra)
}
mean(varianza)
varianza.teorica

ggplot() +
  geom_histogram(aes(x = varianza, y = ..density..), fill = "red",
                 color = "black", bins = 20) +
  geom_vline(aes(xintercept = varianza.teorica), linetype = "dashed",
             color = "white", size = 2) + 
  theme_classic()
