#Tarea:
N <- c(3500,2000,2000)
Ntot <- sum(N)
n <- c(500,200,300)
p <- c(.13,.45,.50)
pgorro <- c(0,0,0)

for (i in 1:3) {
  pgorro[i] <- (N[i]/Ntot)*p[i]
  
}

estim_p <- sum(pgorro)

#intervalo de confianza:-------------------------
#usamos tcl y queremos un intervalo con alpha = .05
p_ast <- qnorm(.975)
sh <- c(0,0,0)

for (i in 1:3) {
  sh[i] = ( n[i] / (n[i] - 1) ) * p[i] * (1 - p[i])
}

term <-c(0,0,0)
for (i in 1:3) {
  term[i] = N[i]^2 *(1-n[i]/N[i])* (sh[i]/n[i])
  
}

SE <- (1/Ntot)*sqrt(sum(term))
ME <- p_ast * SE

CI <- c(estim_p + ME, estim_p - ME)

