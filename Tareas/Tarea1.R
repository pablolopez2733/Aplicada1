#1. función que devuelve el k-esimo momento de un vector de observaciones
kesimo.momento <- function(x, k)
  {
  sigma_x <- sd(x)
  n_x <- length(x)
  xbarra <- mean(x)
  suma <- sum((x)^k)
  
  momentok <- 1/(sigma_x^k*n_x)*suma
  return(momentok)
  }

#2



#3 funcion que devuelve el j-ésimo estadístico de orden
jesimo.dato <- function(x,j)
{
  x_orden <- sort(x,decreasing = FALSE)
  jesimo <- x_orden[j]
  return(jesimo)
}

