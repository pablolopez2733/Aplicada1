#TAREA 17-JUN-2020

#1. Construye una función que tome de input dos variables: 
#x un vector y k un entero de tal manera que calcule el 
#k-ésimo momento centRal de los datos

kesimo.momento<-function(x,k){
  sigma_x <- sd(x)
  n<-length(x)
  xbar<-mean(x)
  suma<-sum((x-xbar)^k)
  momento <- 1/(n*(sigma_x)^k)*suma
  return(momento)
}

# 2. Sin usar la opción de trim ni trimmed.mean crea una función que calcule la media de los datos que están entre el 
# cuantil α/2 y el cuantil 1−α/2 (0 ≤ α ≤ 1. A esta media se le conoce como media truncada al nivel α×100%. 

media.truncada<-function(x,alpha){
  cuartil_inicial<-quantile(x,alpha/2)
  cuartil_final<-quantile(x,(1-alpha/2))
  x_alpha<- x[x>= cuartil_inicial | x <= cuartil_final]
  n_alpha<-length(x_alpha)
  suma <- sum(x_alpha)
  media_truncada<-1/n_alpha * suma
  return(media_truncada)
}

#Una función llamada jesimo.dato de dos argumentos que dado un vector de datos ~
#x me devuelva el j-ésimo dato ordenado (es decir el x(j)). 
#NOTA No confundir con devolver el xj que es la j-ésima entrada. 
#Como sugerencia usar arrange, order ó sort.

jesimo.dato<-function(x,j){
  x_ordenado <- sort(x, decreasing= FALSE)
  return(x_ordenado[j])
}

