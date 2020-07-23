#Nota: Esta es la tarea de un compañero y compartió el código

rm(list = ls())

library(gtools)
library(tidyverse)
library(dplyr)

set.seed(646)
muestra <- sample(c(0, 1), 13, replace = TRUE, prob = c(0.2, 0.8))
y_t <- sum(muestra)

comb <- function(n, x) {
  factorial(n) / factorial(n - x) / factorial(x)
} # @CCC en https://stackoverflow.com/a/17775464

estim <- function(p) {
  n <- 13
  y <- y_t
  s <- 0
  alpha <- 0.1
  for (i in 0:y) {
    prob <- comb(n, i) * p^i * (1 - p)^(n - i)
    s <- s + prob
  }
  return(s - alpha / 2)
}

estim_2 <- function(p) {
  n <- 13
  y <- y_t
  s <- 0
  alpha <- 0.1
  for (i in 0:y) {
    prob <- comb(n, i) * p^i * (1 - p)^(n - i)
    s <- s + prob
  }
  return(1 - s - alpha / 2)
}

maxim <- uniroot(estim, lower = 0, upper = 1)$root
minim <- uniroot(estim_2, lower = 0, upper = 1)$root

c(minim, maxim)
