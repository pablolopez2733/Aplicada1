#Prueba captura recaptura
#####################
# CODIGO PROYECTO 2 #
#####################

rm(list= ls())

#Importamos librerias
library(tidyverse) 
library(cowplot)
library(kableExtra)
library(knitr) 
library(lubridate)
library(dplyr) 
library(moments) 
library(ggcorrplot) 
library(rtweet)
library(rjson)
library(mapsapi)
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(leaflet)
library(tidytext)
library(glue)
library(stringr)
library(ggplot2)

#Parte 1: Importamos los tweets y los convertimos en una base de datos. 
#Según el link https://github.com/ropensci/rtweet/issues/356 existe un error en la configuración 
#de rTweet cuando usamos files de tipo json, luego entonces utilizamos el fix proporcionado en el 
#siguiente link: https://gist.github.com/JBGruber/dee4c44e7d38d537426f57ba1e4f84ab

#El fix consiste en la función recover_stream, la cual se construye en las siguientes l�neas:

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


#Mineria de Datos
file.name1 <- "data_tw.json"
file.name2 <- "data_tw2.json"
time.ellapsed<- 600

stream_tweets(      "Trump",
                    parse=FALSE,
                    file_name=file.name1,
                    language = "en", 
                    timeout = time.ellapsed
)

data1<-recover_stream(file.name1)
data1<- as.data.frame(data1)

stream_tweets(      "Trump",
                    parse=FALSE,
                    file_name=file.name2,
                    language = "en", 
                    timeout = time.ellapsed
)

data2<-recover_stream(file.name2)
data2 <- as.data.frame(data2)



#Nos quedamos con variables de relevancia                   
data1<- data1 %>% select(user_id, status_id, screen_name, text)
data2<-data2 %>% select(user_id, status_id, screen_name, text)
r<- rbind(data1,data2)
r.len <- r%>%count()
unicos <- r %>% distinct(text)%>%tally()%>%as.numeric() 
repetidos<- r.len - unicos


# Estimamos total de cuentas verificadas que se involucran en trump-------------
followers <- data1 %>% select(user_id,followers_count)
verified <- data1 %>% select(user_id,verified)
trus <- length (verified$verified[verified$verified==TRUE])

cols <- sapply(verified, is.logical)
verified[,cols] <- lapply(verified[,cols], as.numeric)

#estimamos N:
n1 <- nrow(data1)
n2 <- nrow(data2)
m <- as.numeric(repetidos)
N <- (n1 * n2)/m
N <- as.numeric(N)
x_s <- mean(verified$verified)
t_hat <- N*(1/n1)*(sum(verified$verified))
alpha <- .01
z <- qnorm(1-alpha/2)
term <- rep(0,n1)
#estimar sx
for (i in 1:n1) {
  
  term[i]=(verified$verified[i]-x_s)^2
}

s_x <- 1/(n1-1)*(sum(term))

V_hat <- N^2 * ((1-n1/N)/n1 )* s_x
#Intervalo de confianza al 99% del total de cuentas verificadas que hablan de Trump

IC_ver <- c(t_hat-z*sqrt(V_hat),t_hat + z*sqrt(V_hat))
ciLow <- round(IC_ver[1])
ciHigh <- round(IC_ver[2])
paste0("Se estiman ", round(t_hat) ," cuentas verificadas tweeteando sobre Trump con intervalo de confianza [",
       ciLow, ",", ciHigh,"] del 99%")
#Graficas:
# Basic barplot
df <- data.frame(cuentas=c("Verificadas", "No Verificadas"),
                 cantidad_tweets=c(trus,n1-trus))
p<-ggplot(data=df, aes(x=cuentas, y=cantidad_tweets)) +
  geom_bar(stat="identity",fill="lightblue")
p