rm(list =ls())
library(ggplot2)

#Decidí el tamaño de la población
N     <- 1000

#La estoy simulando
censo <- data.frame(x = rbeta(N, 1.1, 1.2))

nsim <- 100 #Número de simulaciones
n    <- 100 #Tamaño de la muestra

total.muestra  <- rep(NA, nsim)
intervalo.alto <- rep(NA, nsim)
intervalo.bajo <- rep(NA, nsim)

#Intervalo al 90% entonces alpha = 0.1
Zalpha          <- qnorm(1 - 0.1/2)
total.poblacion <- sum(censo$x)  
var.total       <- N^2*(1 - n/N)/n*var(censo$x) #var(tgorro)
  
#Recordar el intervalo: tgorro + Z(1-alpha/2)*sqrt(varianza)
for (i in 1:nsim){
  muestra            <- sample(censo$x, n, replace = FALSE)
  total.muestra[i]   <- N/n*sum(muestra) #tgorro #OJO!! AQUÍ HABÍA UN ERROR
  intervalo.alto[i]  <- total.muestra[i] + Zalpha*sqrt(var.total) #U(S)
  intervalo.bajo[i]  <- total.muestra[i] - Zalpha*sqrt(var.total) #L(S)
}

intervalos.simulados <- data.frame(
  Simulacion     = 1:nsim,
  Intervalo.Bajo = intervalo.bajo, 
  Intervalo.Alto = intervalo.alto,
  Total          = total.muestra
)

ggplot(intervalos.simulados) +
  geom_point(aes(x = Simulacion, y = Total),
             color = "firebrick") +
  geom_errorbar(aes(x = Simulacion, ymin = Intervalo.Bajo, 
                    ymax = Intervalo.Alto)) +
  geom_hline(aes(yintercept = total.poblacion), 
             color = "blue")

#EJERCICIO RESUMEN: 
paquetes <- available.packages()
nrow(paquetes)

