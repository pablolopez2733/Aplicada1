rm(list= ls())

#Importamos librerias

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
time.ellapsed<- 600

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



#-------------------------------------------------------------------------


saveRDS(data_1,file = "data_1.rds")
saveRDS(data_2,file = "data_2.rds")
#-------------------------------------------------------------------------
muestra_aux       <- get_followers("realDonaldTrump", n = 5000)
muestra_aux1      <- lookup_users(muestra_aux$user_id)
muestra_followers <- users_data(muestra_aux1)
saveRDS(muestra_followers,file = "muestraTrump.rds")
