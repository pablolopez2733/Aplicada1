library(readr)
library(survey)
ensanut <- read_csv("~/Dropbox/ITAM_CLASES/Aplicada1/Archivos_2020/ENSANUT_1.csv")

#pEGAR ID DE vivienda (cluster) con ID de persona
ensanut <- ensanut %>% mutate(id = paste0(VIV_SEL, "_", NUMREN))
ensanut <- ensanut %>% mutate(Diabetes = ifelse(P3_1 == 1, 1, 0))

diseño  <- svydesign(id = ~id, strata = ~EST_DIS, weights = ~F_20MAS,
                     PSU = UPM, data = ensanut, nest = TRUE) 
media <- svymean(~Diabetes, diseño)
confint(media)
mean(ensanut$Diabetes)
