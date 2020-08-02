rm(list = ls())
n      <- 15
lambda <- 0.1
Z      <- qnorm(0.975)
nsim   <- 100 
N      <- 1000000 #1 millon (6 ceros)
pop    <- sample(c(0,1), N, replace = TRUE, prob = c(1 - lambda, lambda)) 

#Crear una base para guardar los intervalos de la muestra
datos           <- data.frame(matrix(NA, ncol = 3, nrow = nsim))
colnames(datos) <- c("IC_Bajo","Media","IC_Alto")
datos$Sim       <- 1:nsim

for (i in 1:nsim){
  muestra <- sample(pop, n)
  datos$Media[i]    <- mean(muestra)
  varianza          <- (1 - n/N)/n*var(muestra)
  datos$IC_Bajo[i]  <- datos$Media[i] - Z*sqrt(varianza) #aquí iba [i]
  datos$IC_Alto[i]  <- datos$Media[i] + Z*sqrt(varianza)
}

ggplot(datos, aes(x = Sim)) +
  geom_errorbar(aes(ymin = IC_Bajo, ymax = IC_Alto)) +
  geom_point(aes(y = Media), color = "tomato3") +
  geom_hline(aes(yintercept = mean(pop)), color = "blue", size = 1) +
  theme_bw() 

library(binom) 
binom.confint(0, n, conf.level = 0.95, method = "exact")

#Ej Poisson
n         <- 20
muestra   <- rpois(n, 1/5)
ybarra    <- sum(muestra) 
alpha.val <- 0.05 

func.opt.1 <- function(lambda){ppois(ybarra, n*lambda) - alpha.val/2}
lambda.1   <- uniroot(func.opt.1, lower = 0, upper = 10, tol = 1.e-10)$root

func.opt.2 <- function(lambda){
  1 - (ppois(ybarra, n*lambda) - dpois(ybarra, n*lambda)) - alpha.val/2
  }
lambda.2   <- uniroot(func.opt.2, lower = 0, upper = 10, tol = 1.e-10)$root

c(lambda.2, lambda.1)


