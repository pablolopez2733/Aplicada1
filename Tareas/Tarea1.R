#1. funci�n que devuelve el k-esimo momento de un vector de observaciones
kesimo.momento <- function(x, k)
  {
  sigma_x <- sd(x)
  n_x <- length(x)
  xbarra <- mean(x)
  suma <- sum((x)^k)
  
  momentok <- 1/(n_x)*suma
  return(momentok)
  }

#2 Funci�n que devuelve la media de los datos entre el cuantil 1 y 1-alpha
media.truncada <- function(x,alpha)
{
  vr<- c()
  q1<-quantile(x,alpha/2)
  q2<-quantile(x,(1-alpha/2))
  #iterar por todo el vector y crear un nuevo vector con los vectores que cumplan
  for (v in x)
  {
    if(q1<=v & v<=q2 )
    {
      vr <- c(vr,v)
    }
  }
  n_alpha <- length(vr)
  mtrunc <- (1/n_alpha)*(sum(vr))
  return(mtrunc)
}


#Prueba:
mean(c(1.7,1.8,1.67,1.5,1.87,1.93),trim = 0.2)
media.truncada(c(1.7,1.8,1.67,1.5,1.87,1.93),0.2)

#3 funcion que devuelve el j-�simo estad�stico de orden
jesimo.dato <- function(x,j)
{
  x_orden <- sort(x,decreasing = FALSE)
  jesimo <- x_orden[j]
  return(jesimo)
}

