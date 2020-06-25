#Nos deshacemos de valores missing en variables de interés

linea.mujeres.filtered<-filter(linea.mujeres, !is.na(TEMATICA_1), !is.na(ORIGEN), !is.na(SERVICIO), !is.na(ESCOLARIDAD), !is.na(ESTADO_CIVIL))

#Realizamos un Count Plot entre el Estado Civil y la ocupación
ggplot(linea.mujeres.filtered)+ 
  geom_count(aes(x =ESTADO_CIVIL , y = OCUPACION), color = "darkblue", show.legend=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(subtitle="Estado Civil vs Ocupación", 
       y="Ocupación", 
       x="Estado Civil", 
       title="Gráfica de Conteo") +
  theme(axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"))  


#Realizamos un Bubble Plot
ggplot(linea.mujeres.filtered) +
  geom_bar(aes(SERVICIO, fill = ORIGEN), width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text=element_text(size=7)) +
  labs(
    title = "Servicio Brindado en la Llamada a Linea Mujeres",
    subtitle = "Por Origen de la Llamada",
    x = "Servicio brindado",
    y = "n"
  )

tematica.escolaridad <- linea.mujeres.filtered %>%group_by(TEMATICA_1, ESCOLARIDAD, ESTADO_CIVIL) %>% count()

ggplot(tematica.escolaridad)+ 
  labs(x = "Estado Civil",
       y = " Tematica de la llamada",
       title="Temática de la Lllamada por Estado Civil y Escolaridad") + 
  geom_jitter(aes(ESTADO_CIVIL, TEMATICA_1, size=n, color = ESCOLARIDAD)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text=element_text(size=7)) + 
  facet_grid(cols = vars(ESCOLARIDAD))