#####################
# CODIGO PROYECTO 2 #
#####################

rm(list= ls())

# 0. Importamos Librerias
library(cowplot)
library(dplyr) 
library(glue)
library(ggplot2)
library(ggmap)
library(ggcorrplot)
library(jsonlite)
library(kableExtra)
library(knitr)
library(ks)
library(leaflet)
library(lubridate)
library(mapsapi)
library(moments) 
library(rtweet)
library(rjson)
library(RCurl)
library(stringr)
library(tidytext)
library(tidyverse) 
library(tmaptools)

#Parte 1: Importamos los tweets y los convertimos en una base de datos. 
#SegÃºn el link https://github.com/ropensci/rtweet/issues/356 existe un error en la configuraciÃ³n 
#de rTweet cuando usamos files de tipo json, luego entonces utilizamos el fix proporcionado en el 
#siguiente link: https://gist.github.com/JBGruber/dee4c44e7d38d537426f57ba1e4f84ab

#El fix consiste en la función recover_stream, la cual se construye en las siguientes líneas:

#' Recovers Twitter damaged stream data (JSON file) into parsed data frame.
#'
#' @param path Character, name of JSON file with data collected by
#'   \code{\link{stream_tweets}}.
#' @param dir Character, name of a directory where intermediate files are
#'   stored.
#' @param verbose Logical, should progress be displayed?
#'
#' @family stream tweets
recover_stream <- function(path, dir = NULL, verbose = TRUE) {
  
  # read file and split to tweets
  lines <- readChar(path, file.info(path)$size, useBytes = TRUE)
  tweets <- stringi::stri_split_fixed(lines, "\n{")[[1]]
  tweets[-1] <- paste0("{", tweets[-1])
  tweets <- tweets[!(tweets == "" | tweets == "{")]
  
  # remove misbehaving characters
  tweets <- gsub("\r", "", tweets, fixed = TRUE)
  tweets <- gsub("\n", "", tweets, fixed = TRUE)
  
  # write tweets to disk and try to read them in individually
  if (is.null(dir)) {
    dir <- paste0(tempdir(), "/tweets/")
    dir.create(dir, showWarnings = FALSE)
  }
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Processing tweets [:bar] :percent, :eta remaining",
      total = length(tweets), clear = FALSE
    )
    pb$tick(0)
  }
  
  tweets_l <- lapply(tweets, function(t) {
    pb$tick()
    id <- unlist(stringi::stri_extract_first_regex(t, "(?<=id\":)\\d+(?=,)"))[1]
    f <- paste0(dir, id, ".json")
    writeLines(t, f, useBytes = TRUE)
    out <- tryCatch(rtweet::parse_stream(f),
                    error = function(e) {})
    if ("tbl_df" %in% class(out)) {
      return(out)
    } else {
      return(id)
    }
  })
  
  # test which ones failed
  test <- vapply(tweets_l, is.character, FUN.VALUE = logical(1L))
  bad_files <- unlist(tweets_l[test])
  
  # Let user decide what to do
  if (length(bad_files) > 0) {
    message("There were ", length(bad_files),
            " tweets with problems. Should they be copied to your working directory?")
    sel <- menu(c("no", "yes", "copy a list with status_ids"))
    if (sel == 2) {
      dir.create(paste0(getwd(), "/broken_tweets/"), showWarnings = FALSE)
      file.copy(
        from = paste0(dir, bad_files, ".json"),
        to = paste0(getwd(), "/broken_tweets/", bad_files, ".json")
      )
    } else if (sel == 3) {
      writeLines(bad_files, "broken_tweets.txt")
    }
  }
  
  # clean up
  unlink(dir, recursive = TRUE)
  
  # return good tweets
  return(dplyr::bind_rows(tweets_l[!test]))
}

# 1. Minería de Datos
# Después de emplear el fix, comenzamos con la minería de datos/tweets
# Notemos que tomaremos 2 muestras, pues esto nos servirá para realizar captura y recaptura
#Mineria de Datos
file.name1 <- "data_tw.json"
file.name2 <- "data_tw2.json"
time.ellapsed<- 30

#MUESTRA 1
stream_tweets(      "Trump",
                    parse=FALSE,
                    file_name=file.name1,
                    language = "en", 
                    timeout = time.ellapsed
)

data_1<-recover_stream(file.name1)
data_1<- as.data.frame(data_1)

#MUESTRA 2
stream_tweets(      "Trump",
                    parse=FALSE,
                    file_name=file.name2,
                    language = "en", 
                    timeout = time.ellapsed
)

data_2<-recover_stream(file.name2)
data_2 <- as.data.frame(data_2)

#Para ambos data.frames nos quedamos con las variables de relevancia
data1<- data_1 %>% select(user_id, status_id, screen_name, text,verified)
data2<-data_2 %>% select(user_id, status_id, screen_name, text, verified)

#Para realizar captura y recaptura juntaremos las dos bases de datos y buscaremos los textos
# únicos para obtener nuestra "k"
r<-rbind(data1,data2) 
r.len <- r%>%count()
unicos <- r %>% distinct(text)%>%tally()%>%as.numeric() 
repetidos<- r.len - unicos

#Obtenemos nuestra N
n1 <- nrow(data1)
n2 <- nrow(data2)
m <- as.numeric(repetidos)
N <- (n1 * n2)/m #Utilizando la formula de captura y recaptura
N <- as.numeric(N) *100 #Notamos que es el 1% de la muestra

# 2. Buscamos encontrar el número de cuentas verificadas que hablan acerca de Trump

verified <- data_1 %>% select(user_id,verified)
trus <- length (verified$verified[verified$verified==TRUE])

cols <- sapply(verified, is.logical)
verified[,cols] <- lapply(verified[,cols], as.numeric)

x_s <- mean(verified$verified)
t_hat <- N*(1/n1)*(sum(verified$verified))*100
alpha <- .01
z <- qnorm(1-alpha/2)
term <- rep(0,n1)
#estimar sx
for (i in 1:n1) {
  
  term[i]=(verified$verified[i]-x_s)^2
}

s_x <- ((1/(n1-1))*(sum(term))*100)

V_hat <- N^2 * ((1-n1/N)/n1 )* s_x

#Intervalo de confianza al 99% del total de cuentas verificadas que hablan de Trump

IC_ver <- c(t_hat-z*sqrt(V_hat),t_hat + z*sqrt(V_hat))
ciLow <- round(IC_ver[1])
ciHigh <- round(IC_ver[2]) 
paste0("Se estiman ", round(t_hat) ," cuentas verificadas tweeteando sobre Trump con intervalo de confianza [",
       ciLow, ",", ciHigh,"] del 99%")

#Realizamos una gráfica 
df <- data.frame(cuentas=c("Verificadas", "No Verificadas"),
                 cantidad_tweets=c(trus,n1-trus))
p<-ggplot(data=df, aes(x=cuentas, y=cantidad_tweets)) +
  geom_bar(stat="identity",fill="lightblue")
p

# 3. Estimar la diferencia promedio entre seguidos y seguidores de los usarios que hablan acerca de Trump
#(i.e. Followers - Following).
muestra.usuarios  <- users_data(data_1)

#Usamos una base de datos más sencilla que nos sirva
user_data <- data.frame("user_id" = muestra.usuarios$user_id,
                        "Followers" = muestra.usuarios$followers_count,
                        "Following" = muestra.usuarios$friends_count,
                        "Diferencia" = muestra.usuarios$followers_count-muestra.usuarios$friends_count)

#Notemos que la diferencia en datos puede ser muy grande, de ahí que para que nuestro
#análisis sea mejor, tennemos que eliminar los "outliers"
Q          <- quantile(user_data$Diferencia, probs=c(.25, .75), na.rm = FALSE)
iqr        <- IQR(user_data$Diferencia)
eliminated <- subset(user_data, user_data$Diferencia > (Q[1] - 1.5*iqr) & user_data$Diferencia < (Q[2]+1.5*iqr))
#Sea Theta el promedio de la cantidad Followers-Following de todos los nodos en
#el universo de twitter. Entonces el estimador de máxima verosimilitud, insesgado
#y consistente del parámetro Theta está dado por el promedio de las observaciones
#en nuestra muestra.
x.barra <- mean(eliminated$Diferencia)


#Partimos del estimador puntual que previamente hemos definido para crear un 
#intervalo de confianza al nivel 99% para el parámetro Theta.
a            <- qnorm(1-alpha/2)
desv.muestra <- sd(eliminated$Diferencia)


##Realizamos un esquema para visualizar bien los datos
n <- length(eliminated$Diferencia)
vec <- seq(1:n)
ggplot(eliminated, aes(x = vec, y = Diferencia))+
  geom_bar(stat = 'identity', fill = 'blue4')+
  geom_hline(yintercept = x.barra, color = 'firebrick')

#Ahora tenemos los elementos suficientes para construir nuestro intervalo de
#confianza al nivel 99%
limite.inferior <- x.barra - a*desv.muestra
limite.superior <- x.barra + a*desv.muestra

#3. Queremos estimar el tamaño promedio de un post
# En primera instancia queremos medir la longitud de los tweets dada la muestra
len<-nchar(data1$text, type = "chars", allowNA = FALSE, keepNA = NA)
data1$len<-len
len.bar<- mean(data1$len) 
n<-length(data1$len)
s<-sd(len)
      
#Notemos que la varianza de la población es desconocida, tenemos que utilizar el siguiente intervalo de confianza
talpha<-qt(1-alpha/2,n-1)
ic <- round(c(len.bar - talpha*s/sqrt(n), len.bar + talpha*s/sqrt(n)), 2) 

#5. Realizamos un Sentiment Analysis en busca de encontrar los sentimientos de la población acerca de Trumo
dropwords <- c("trump") # La palabra "Trump" tiene una conotación en el lexicon

sentiment<-data1 %>% 
  unnest_tokens(word, text, token = 'words') %>% 
  left_join(get_sentiments("bing") %>% filter(!word %in% dropwords)) %>% # Obtiene las palabras de sentimiento
  mutate(sentiment = if_else(sentiment == 'positive', 1, -1, missing = 0)) %>% # Asigna valores a las palabras de sentimiento 
  group_by(user_id) %>% #Agrupamos las palabras por clave
  summarise(sentiment = sum(sentiment))

#Agrupamos por tipo de sentimiento del tweet

n.positivo<-subset(sentiment,sentiment>0)%>%count()
n.positivo$class<-c("Positivos")
n.neutral<-subset(sentiment,sentiment==0)%>%count()
n.neutral$class<-c("Neutrales")
n.negativo<-subset(sentiment,sentiment<0)%>%count()
n.negativo$class<-c("Negativos")

n<- as.numeric(n.positivo$n + n.neutral$n + n.negativo$n)

count.sentiment<-rbind(n.positivo,n.neutral,n.negativo)

#Realizamos una tabla
kable(count.sentiment, booktabs = T) %>% kable_styling(latex_options = "striped")

ggplot(count.sentiment)+
  geom_col(aes(x = class, y = n), color='skyblue', fill='steelblue') + 
  theme_bw() + 
  labs(
    title = "Conteo de Sentimientos de Tweets de Trump",
    x = "Tipo de Sentimiento",
    y = "Conteo"
  ) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Notemos que tenemos un caso de estratificado, de ahí que utilizamos la N anteriormente obtenida de captura y recaptura
#En general sabemos que n_h = n/N *N_h
#Luego entonces, nosotros buscamos N_H = n_h/n *N
#Total poblacional de tweets positivos de Trump

N.positivo <- as.numeric(n.positivo$n)/n * N
N.negativo <- as.numeric(n.negativo$n)/n * N
N.neutral  <- as.numeric(n.neutral$n)/n * N
#--------------------------------------------------------------------------------------------------------------------
#Con lo que obtenemos el número de tweets con una característica: hablar bien, ser neutrales o hablar mal de Trump

#Obtenemos una muestra aleatoria de 5,000 followers de Trump
muestra_aux       <- get_followers("realDonaldTrump", n = 5000)
muestra_aux1      <- lookup_users(muestra_aux$user_id)

#Aplicamos la función users_data para obtener la información de su ubicación
muestra_followers <- users_data(muestra_aux1)

#Eliminamos de nuestra base de datos aquellas entradas que no tienen especificada
#una ubicación
muestra_followers <- muestra_followers%>%filter(location != "")

#Obtenemos el tamaño real de nuestra muestra
n_muestra         <- length(muestra_followers$user_id)

#Ahora nos quedamos con los usuarios que tweetean desde Estados Unidos
muestra_followers <- muestra_followers%>%filter(str_detect(location, "USA")|
                                                  str_detect(location, "United States")|
                                                  str_detect(location, ", AL")|
                                                  str_detect(location, ", AK")|
                                                  str_detect(location, ", AZ")|
                                                  str_detect(location, ", AR")|
                                                  str_detect(location, ", CA")|
                                                  str_detect(location, ", CO")|
                                                  str_detect(location, ", CT")|
                                                  str_detect(location, ", DE")|
                                                  str_detect(location, ", FL")|
                                                  str_detect(location, ", GA")|
                                                  str_detect(location, ", HI")|
                                                  str_detect(location, ", ID")|
                                                  str_detect(location, ", IL")|
                                                  str_detect(location, ", IN")|
                                                  str_detect(location, ", IA")|
                                                  str_detect(location, ", KS")|
                                                  str_detect(location, ", KY")|
                                                  str_detect(location, ", LA")|
                                                  str_detect(location, ", ME")|
                                                  str_detect(location, ", MD")|
                                                  str_detect(location, ", MA")|
                                                  str_detect(location, ", MI")|
                                                  str_detect(location, ", MN")|
                                                  str_detect(location, ", MS")|
                                                  str_detect(location, ", MO")|
                                                  str_detect(location, ", MT")|
                                                  str_detect(location, ", NE")|
                                                  str_detect(location, ", NV")|
                                                  str_detect(location, ", NH")|
                                                  str_detect(location, ", NJ")|
                                                  str_detect(location, ", NM")|
                                                  str_detect(location, ", NY")|
                                                  str_detect(location, ", NC")|
                                                  str_detect(location, ", ND")|
                                                  str_detect(location, ", OH")|
                                                  str_detect(location, ", OK")|
                                                  str_detect(location, ", OR")|
                                                  str_detect(location, ", PA")|
                                                  str_detect(location, ", RI")|
                                                  str_detect(location, ", SC")|
                                                  str_detect(location, ", SD")|
                                                  str_detect(location, ", TN")|
                                                  str_detect(location, ", TX")|
                                                  str_detect(location, ", UT")|
                                                  str_detect(location, ", VT")|
                                                  str_detect(location, ", VA")|
                                                  str_detect(location, ", WA")|
                                                  str_detect(location, ", WV")|
                                                  str_detect(location, ", WI")|
                                                  str_detect(location, ", WY")|
                                                  str_detect(location, ", DC")|
                                                  str_detect(location, "Alabama")|
                                                  str_detect(location, "Alaska")|
                                                  str_detect(location, "Arizona")|
                                                  str_detect(location, "Arkansas")|
                                                  str_detect(location, "California")|
                                                  str_detect(location, "Colorado")|
                                                  str_detect(location, "Connecticut")|
                                                  str_detect(location, "Delaware")|
                                                  str_detect(location, "Florida")|
                                                  str_detect(location, "Georgia")|
                                                  str_detect(location, "Hawaii")|
                                                  str_detect(location, "Idaho")|
                                                  str_detect(location, "Illinois")|
                                                  str_detect(location, "Indiana")|
                                                  str_detect(location, "Iowa")|
                                                  str_detect(location, "Kansas")|
                                                  str_detect(location, "Kentucky")|
                                                  str_detect(location, "Louisiana")|
                                                  str_detect(location, "Maine")|
                                                  str_detect(location, "Maryland")|
                                                  str_detect(location, "Massachusetts")|
                                                  str_detect(location, "Michigan")|
                                                  str_detect(location, "Minnesota")|
                                                  str_detect(location, "Mississippi")|
                                                  str_detect(location, "Missouri")|
                                                  str_detect(location, "Montana")|
                                                  str_detect(location, "Nebraska")|
                                                  str_detect(location, "Nevada")|
                                                  str_detect(location, "New Hampshire")|
                                                  str_detect(location, "New Jersey")|
                                                  str_detect(location, "New Mexico")|
                                                  str_detect(location, "New York")|
                                                  str_detect(location, "North Carolina")|
                                                  str_detect(location, "North Dakota")|
                                                  str_detect(location, "Ohio")|
                                                  str_detect(location, "Oklahoma")|
                                                  str_detect(location, "Oregon")|
                                                  str_detect(location, "Pennsylvania")|
                                                  str_detect(location, "Rhode Island")|
                                                  str_detect(location, "South Carolina")|
                                                  str_detect(location, "South Dakota")|
                                                  str_detect(location, "Tennessee")|
                                                  str_detect(location, "Texas")|
                                                  str_detect(location, "Utah")|
                                                  str_detect(location, "Vermont")|
                                                  str_detect(location, "Virginia")|
                                                  str_detect(location, "Washington")|
                                                  str_detect(location, "West Virginia")|
                                                  str_detect(location, "Wisconsin")|
                                                  str_detect(location, "Wyoming")
)

#Obtenemos la cantidad de usuarios de nuestra muestra que no tweetean desde Estados Unidos
n_extranjeros <- n_muestra-length(muestra_followers$user_id)


#Estimamos la proporción de de tweets extranjeros
estim         <- n_extranjeros/n_muestra

#A partir de esta estimación puntual de la proporción de followers extranjeros de Trump
#crearemos un intervalo de confianza a nivel 99%
N                 <- 83650000 #Numero total de followers de Trump 
alpha             <- 0.01
Z                 <- qnorm(1 - alpha/2)
varianza.gorro    <- estim*(1-estim)*(N - n_muestra)/n_muestra/(N-1)
limite.inferior   <- estim - Z*sqrt(varianza.gorro)
limite.superior   <- estim + Z*sqrt(varianza.gorro)

