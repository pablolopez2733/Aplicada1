rm(list = ls())
n <- 50
r <- 42

func.optim <- function(N){
  suma <- 0
  for (i in 0:(r-1)){
    suma <- suma + N/(N - i)
  }
  return(suma - n)
}

uniroot(func.optim, lower = 42, upper = 500)
#La raíz es 136.13 ¿el óptimo es 136 ó 137?

#Checo cada uno:
func.optim(136) #Éste es el más cercano a 0
func.optim(137)

#El íptimo es 136
