library(tidyverse) 
library(cowplot)
library(kableExtra)
library(knitr) 
library(lubridate)
library(dplyr) 
library(moments) 
library(readr)
library(rgdal)
library(broom)
library(scales)
library(lemon)
library(ggplot2)
library(survey)



tpv2 <- read.csv("C:/Users/ferna/OneDrive/Escritorio/TPerVic2.csv")



tpv2<-tpv2[ tpv2$CVE_ENT %in% c(9),]   #Elección de Ciudad de Mexico
tpv2$ENT <- (tpv2$NOM_MUN) #Calibramos por Municipio



tpv2<-  tpv2 %>% mutate(sufrio.delitos = if_else(AP6_2== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(robo.coche = if_else(AP6_4_01== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(robo.casa = if_else(AP6_4_04== 1 , 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(secuestro = if_else(AP6_9== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(desaparicion = if_else(AP6_15_1== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(homicidio = if_else(AP6_19== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(asalto = if_else(AP7_3_05== 1, 1, 0, missing = 0))
tpv2<-  tpv2 %>% mutate(menor.edad = if_else(EDAD<18, 1, 0, missing = 0))



design = svydesign(id=~UPM_DIS,strata=~EST_DIS, weights=~tpv2$FAC_ELE, data=tpv2)



#Si quisieramos hacer un análisis del total de delitos, tomamos los totales para esto
total.edo            <- svyby(~sufrio.delitos, by=tpv2$ENT, design=design, svytotal)
total.edo.coche      <- svyby(~robo.coche, by=tpv2$ENT, design=design, svytotal)
total.edo.casa       <- svyby(~robo.casa, by=tpv2$ENT, design=design, svytotal)
total.edo.secuestro  <- svyby(~secuestro, by=tpv2$ENT, design=design, svytotal)
total.edo.desp       <- svyby(~desaparicion, by=tpv2$ENT, design=design, svytotal)
total.edo.homicidio  <- svyby(~homicidio, by=tpv2$ENT, design=design, svytotal)
total.edo.asalto     <- svyby(~asalto, by=tpv2$ENT, design=design, svytotal)



#Intervalos de Confianza
total.edo$intervalo<-confint(total.edo)
aux.int <- as.data.frame(total.edo$intervalo)
total.edo<-cbind(total.edo, aux.int)
total.edo$intervalo<-NULL



total.edo.coche$intervalo<-confint(total.edo.coche)
aux.int <- as.data.frame(total.edo.coche$intervalo)
total.edo<-cbind(total.edo.coche, aux.int)
total.edo$intervalo<-NULL



total.edo.casa$intervalo<-confint(total.edo.casa)
aux.int <- as.data.frame(total.edo.casa$intervalo)
total.edo.casa<-cbind(total.edo.casa, aux.int)
total.edo.casa$intervalo<-NULL



total.edo.secuestro$intervalo<-confint(total.edo.secuestro)
aux.int <- as.data.frame(total.edo.secuestro$intervalo)
total.edo.secuestro<-cbind(total.edo.secuestro, aux.int)
total.edo.secuestro$intervalo<-NULL



total.edo.homicidio$intervalo<-confint(total.edo.homicidio)
aux.int <- as.data.frame(total.edo.homicidio$intervalo)
total.edo.homicidio<-cbind(total.edo.homicidio, aux.int)
total.edo.homicidio$intervalo<-NULL



total.edo.desp$intervalo<-confint(total.edo.desp)
aux.int <- as.data.frame(total.edo.desp$intervalo)
total.edo.desp<-cbind(total.edo.desp, aux.int)
total.edo.desp$intervalo<-NULL



total.edo.asalto$intervalo<-confint(total.edo.asalto)
aux.int <- as.data.frame(total.edo.asalto$intervalo)
total.edo.asalto<-cbind(total.edo.asalto, aux.int)
total.edo.asalto$intervalo<-NULL





#Genero Variable Auxiliar por tipo de Delito
total.edo$tipo<-"Total de Delitos"
total.edo.coche$tipo<- "Robo Coche"
total.edo.casa$tipo<- "Robo Casa"
total.edo.secuestro$tipo<- "Secuestro"
total.edo.desp$tipo<- "Desaparicion"
total.edo.homicidio$tipo<- "Homicidio"
total.edo.asalto$tipo<- "Asalto"



#Genero Variable Tamaño Poblacional 
total.edo$poblacion <- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.coche$poblacion<- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.casa$poblacion<- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.secuestro$poblacion<-c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.desp$poblacion<- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.homicidio$poblacion<- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)
total.edo.asalto$poblacion<- c(749982,400161,417416,608479,199224,532553,1164477,390348,1827868,243886,364439,137927,361593,677104,427263,415933)



#Renombro mi variable para poder tener todos en un data.frame
colnames(total.edo)[2]<-"Conteo"
colnames(total.edo.coche)[2]<-"Conteo"
colnames(total.edo.casa)[2]<-"Conteo"
colnames(total.edo.secuestro)[2]<-"Conteo"
colnames(total.edo.desp)[2]<-"Conteo"
colnames(total.edo.homicidio)[2]<-"Conteo"
colnames(total.edo.asalto)[2]<-"Conteo"



#Realizamos un mapa de calor por el tipo de delito y la delegación
aux_heatmap <-rbind(total.edo.coche,total.edo.casa,total.edo.secuestro,total.edo.homicidio)



color_palette <- colorRampPalette(c("#ffeda0","#feb24c","#f03b20"))



ggplot(aux_heatmap) +  
  geom_tile(aes(tipo, 
                by, 
                fill= Conteo)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(
    y="Delegacion", 
    x="Tipo de Delito", 
    title="Criminalidad por Delegacion y Tipo de Delito CDMX") +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=10,face="bold")) +
  scale_fill_gradient(
    name = "Conteo", 
    low = "#ffeda0",
    high = "#f03b20",
    space = "Lab",
    guide = "colourbar"
  )



#Población Total 
#Proporcion de Delitos de la Poblacion 
total.edo$proporcion<- total.edo$Conteo/total.edo$poblacion
total.edo.coche$proporcion<- total.edo.coche$Conteo/total.edo.coche$poblacion
total.edo.casa$proporcion <- total.edo.casa$Conteo/total.edo.casa$poblacion
total.edo.asalto$proporcion<- total.edo.asalto$Conteo/total.edo.asalto$poblacion
total.edo.homicidio$proporcion<- total.edo.homicidio$Conteo/total.edo.homicidio$poblacion
total.edo.secuestro$proporcion<- total.edo.secuestro$Conteo/total.edo.secuestro$poblacion
total.edo.desp$proporcion<- total.edo.desp$Conteo/total.edo.desp$poblacion



aux_total<- rbind(total.edo, total.edo.coche, total.edo.casa,total.edo.desp,total.edo.homicidio,total.edo.secuestro, total.edo.asalto)



#knitr::kable(aux_total, format="markdown")



#Plot
ggplot(aux_total)+ 
  labs(x = "Alcaldía",
       y = " Tipo",
       title="Densidad Poblacional Delito") + 
  geom_jitter(aes(by, tipo, size=proporcion)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text=element_text(size=7)) 



##### Aqui vamos a hacer un analisis de las proporciones por tipo de delito



ratio_asaltos<-svyratio(~asalto,~sufrio.delitos,design)
ci_asaltos<-confint(ratio_asaltos)
ratio_asaltos<-as.numeric(ratio_asaltos)



ratio_homicidios<-svyratio(~homicidio,~sufrio.delitos,design)
ci_homicidio<-confint(ratio_homicidios)
ratio_homicidios<-as.numeric(ratio_homicidios)



ratio_coche<-svyratio(~robo.coche,~sufrio.delitos,design)
ci_coche<-confint(ratio_coche)
ratio_coche<-as.numeric(ratio_coche)



ratio_casa<-svyratio(~robo.casa, ~sufrio.delitos,design)
ci_casa<-confint(ratio_casa)
ratio_casa<-as.numeric(ratio_casa)



ratio_secuestro<-svyratio(~secuestro, ~sufrio.delitos, design)
ci_secuestro<-confint(ratio_secuestro)
ratio_secuestro<-as.numeric(ratio_secuestro)

ratio_desp<-svyratio(~desaparicion, ~sufrio.delitos, design)
ci_desp<-confint(ratio_desp)
ratio_desp<-as.numeric(ratio_desp)



prop_names<-c("Proporción Asalto", "Proporción Homicidios", "Proporción Robo Coches", "Proporción Robo Casa", "Proporción Secuestro", "Proporción Desaparición")
intervalos<-as.data.frame(rbind(ci_asaltos,ci_homicidio,ci_coche,ci_casa,ci_secuestro, ci_desp))



aux_ratio<-cbind(prop_names, intervalos)
#knitr::kable(aux_ratio, format="markdown")
