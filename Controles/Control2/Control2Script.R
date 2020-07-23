rm(list = ls())
library(readr)
setwd("C:/Users/pablo/Desktop/GithubRepos/Aplicada1/Controles/Control2")
base.datos <- read_rds("178863.rds")


base.datos$veces_muestra <- 0
x <- seq(1:10)
proba <- .01
for (i in 1:nrow(base.datos)){
  
  Bi <- sample(c(0,1), 1, replace = FALSE, prob = c(1 - proba, proba))
  base.datos$veces_muestra[i] <- Bi
}