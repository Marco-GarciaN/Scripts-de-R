library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes) 
library(lubridate)
library(tools)
library(haven)

fecha_act <- as.Date("01 Abril 2023", format = "%d %B %Y")
Mes_act <- paste0(format(fecha_act, "%Y"),format(fecha_act, "%m"))

fecha_ant <- fecha_act - months(1)
Mes_ant <- paste0(format(fecha_ant, "%Y"),format(fecha_ant, "%m"))

ant <- read_sas(paste("Y:\\Jorge\\Calificacion_CNBV\\Nueva_Metodología\\Resultados\\",
                      Mes_ant,"\\Comercial\\reserva_com_",Mes_ant,".sas7bdat", sep="")) %>%
  filter(Cartera_cont>0 & TipoP=="PFAE") %>%
  select(dossier, Cartera_cont, ETAPA, Reserva_total, TipoP) %>%
  rename(Cartera_cont_ant = Cartera_cont,
         ETAPA_ant = ETAPA,
         Reserva_ant = Reserva_total,
         TipoP_ant = TipoP)

act <- read_sas(paste("Y:\\Jorge\\Calificacion_CNBV\\Nueva_Metodología\\Resultados\\",
                      Mes_act,"\\Comercial\\reserva_com_",Mes_act,".sas7bdat", sep="")) %>%
  filter(Cartera_cont>0 & TipoP=="PFAE") %>%
  select(dossier, Cartera_cont, ETAPA, Reserva_total, TipoP)


base <- full_join(act, ant, by="dossier") %>%
  mutate(ANT = case_when(!is.na(ETAPA) & is.na(ETAPA_ant) ~ 0,
                         is.na(ETAPA) & !is.na(ETAPA_ant) ~ 1,
                         !is.na(ETAPA) & !is.na(ETAPA_ant) ~ 1)) %>%
  mutate(ACT = case_when(!is.na(ETAPA) & is.na(ETAPA_ant) ~ 1,
                         is.na(ETAPA) & !is.na(ETAPA_ant) ~ 0,
                         !is.na(ETAPA) & !is.na(ETAPA_ant) ~ 1))

base$ETAPA[is.na(base$ETAPA)] <- 0
base$Reserva_total[is.na(base$Reserva_total)] <- 0
base$Cartera_cont[is.na(base$Cartera_cont)] <- 0

base$ETAPA_ant[is.na(base$ETAPA_ant)] <- 0
base$Cartera_cont_ant[is.na(base$Cartera_cont_ant)] <- 0
base$Reserva_ant[is.na(base$Reserva_ant)] <- 0

base <- base %>%
  mutate(Categoria = case_when(ACT==1 & ANT==0 ~ 1,
                               ACT==0 & ANT==1 & ETAPA_ant==1 ~ 2,
                               ACT==0 & ANT==1 & ETAPA_ant==2 ~ 3,
                               ACT==0 & ANT==1 & ETAPA_ant==3 ~ 4,
                               ACT==1 & ANT==1 & ETAPA==1 & ETAPA_ant==3 ~ 5,
                               ACT==1 & ANT==1 & ETAPA==1 & ETAPA_ant==2 ~ 6,
                               ACT==1 & ANT==1 & ETAPA==1 & ETAPA_ant==1 ~ 7,
                               ACT==1 & ANT==1 & ETAPA==2 & ETAPA_ant==3 ~ 8,
                               ACT==1 & ANT==1 & ETAPA==2 & ETAPA_ant==2 ~ 9,
                               ACT==1 & ANT==1 & ETAPA==2 & ETAPA_ant==1 ~ 10,
                               ACT==1 & ANT==1 & ETAPA==3 & ETAPA_ant==1 ~ 11,
                               ACT==1 & ANT==1 & ETAPA==3 & ETAPA_ant==2 ~ 12,
                               ACT==1 & ANT==1 & ETAPA==3 & ETAPA_ant==3 ~ 13),
         TipoP_fin = case_when(is.na(TipoP) ~ TipoP_ant,
                               !is.na(TipoP) ~ TipoP))

Walk_PFAE_out <- base %>%
  group_by(Categoria) %>%
  summarise(Frecuencia = n(),
            Suma_OS_ant = sum(Cartera_cont_ant),
            Suma_OS_act = sum(Cartera_cont),
            Suma_Rva_ant = sum(Reserva_ant),
            Suma_Rva_act = sum(Reserva_total)) %>%
  mutate(delta_rva = Suma_Rva_act - Suma_Rva_ant,
         delta_OS = Suma_OS_act - Suma_OS_ant,
         fecha = Mes_act)

Walk_PFAE_out <- Walk_PFAE_out %>%
  mutate(Sub_clase = case_when(Categoria==1 ~ "Nuevos",
                               Categoria==2 ~ "",
                               Categoria==3 ~ "",
                               Categoria==4 ~ "",
                               Categoria==5 ~ "De Etapa 3 a Etapa 1",
                               Categoria==6 ~ "De Etapa 2 a Etapa 1",
                               Categoria==7 ~ "Se mantiene en Etapa 1",
                               Categoria==8 ~ "De Etapa 3 a Etapa 2",
                               Categoria==9 ~ "Se mantiene en Etapa 2",
                               Categoria==10 ~ "De Etapa 1 a Etapa 2",
                               Categoria==11 ~ "De Etapa 1 a Etapa 3",
                               Categoria==12 ~ "De Etapa 2 a Etapa 3",
                               Categoria==13 ~ "Se mantiene en Etapa 3"))

Walk_PFAE_out <- Walk_PFAE_out %>%
  mutate(clase = case_when(Categoria==1 ~ 1,
                           Categoria==2 ~ NA,
                           Categoria==3 ~ NA,
                           Categoria==4 ~ NA,
                           Categoria==5 ~ 1,
                           Categoria==6 ~ 1,
                           Categoria==7 ~ 1,
                           Categoria==8 ~ 2,
                           Categoria==9 ~ 2,
                           Categoria==10 ~ 2,
                           Categoria==11 ~ 3,
                           Categoria==12 ~ 3,
                           Categoria==13 ~ 3))

Cartera_PFAE_E1 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==1,c("Suma_OS_act", "Sub_clase")]) %>%
  arrange(Suma_OS_act) %>% rename(Etapa_1 = Suma_OS_act) %>% na.omit()
Cartera_PFAE_E1$Numero <- formatC(Cartera_PFAE_E1$Etapa_1,format= "f",digits= 2, big.mark = ",")

Cartera_PFAE_E2 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==2,c("Suma_OS_act", "Sub_clase")]) %>%
  arrange(Suma_OS_act) %>% rename(Etapa_2 = Suma_OS_act) %>% na.omit()
Cartera_PFAE_E2$Numero <- formatC(Cartera_PFAE_E2$Etapa_2,format= "f",digits= 2, big.mark = ",")

Cartera_PFAE_E3 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==3,c("Suma_OS_act", "Sub_clase")]) %>%
  arrange(Suma_OS_act) %>% rename(Etapa_3 = Suma_OS_act) %>% na.omit()
Cartera_PFAE_E3$Numero <- formatC(Cartera_PFAE_E3$Etapa_3,format= "f",digits= 2, big.mark = ",")


Reserva_PFAE_E1 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==1,c("Suma_Rva_act", "Sub_clase")]) %>%
  arrange(Suma_Rva_act) %>% rename(Etapa_1 = Suma_Rva_act) %>% na.omit()
Reserva_PFAE_E1$Numero <- formatC(Reserva_PFAE_E1$Etapa_1,format= "f",digits= 2, big.mark = ",")

Reserva_PFAE_E2 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==2,c("Suma_Rva_act", "Sub_clase")]) %>%
  arrange(Suma_Rva_act) %>% rename(Etapa_2 = Suma_Rva_act) %>% na.omit()
Reserva_PFAE_E2$Numero <- formatC(Reserva_PFAE_E2$Etapa_2,format= "f",digits= 2, big.mark = ",")

Reserva_PFAE_E3 <- data.frame(Walk_PFAE_out[Walk_PFAE_out$clase==3,c("Suma_Rva_act", "Sub_clase")]) %>%
  arrange(Suma_Rva_act) %>% rename(Etapa_3 = Suma_Rva_act) %>% na.omit()
Reserva_PFAE_E3$Numero <- formatC(Reserva_PFAE_E3$Etapa_3,format= "f",digits= 2, big.mark = ",")

rm(ant, act, base)

##################################################################################

PFAE1 <- ggplot(Cartera_PFAE_E1, aes(x = Sub_clase, y = Etapa_1)) +
  geom_bar(stat = "identity", color = "green3",lwd = 0.9, 
           fill = "limegreen", alpha=0.7, width = 0.40)+
  scale_x_discrete(limits = Cartera_PFAE_E1$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1 , hjust=-0.1,
            colour = "limegreen", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.6 , hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Cartera_PFAE_E1$Etapa_1)*1.9))+
  coord_flip()+
  labs(title = "Etapa 1",
       subtitle = formatC(sum(Cartera_PFAE_E1$Etapa_1),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "limegreen"))

PFAE2 <- ggplot(Cartera_PFAE_E2, aes(x = Sub_clase, y = Etapa_2)) +
  geom_bar(stat = "identity", color = "orange2",lwd = 0.9, 
           fill = "orange", alpha=0.7, width = 0.3)+
  scale_x_discrete(limits = Cartera_PFAE_E2$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1, hjust=-0.1,
            colour = "darkorange2", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.4, hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Cartera_PFAE_E2$Etapa_2)*1.9))+
  coord_flip()+
  labs(title = "Etapa 2",
       subtitle = formatC(sum(Cartera_PFAE_E2$Etapa_2),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "darkorange2"))

PFAE3 <- ggplot(Cartera_PFAE_E3, aes(x = Sub_clase, y = Etapa_3)) +
  geom_bar(stat = "identity", color = "red2",lwd = 0.9, 
           fill = "red", alpha=0.7, width = 0.25)+
  scale_x_discrete(limits = Cartera_PFAE_E3$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1 , hjust=-0.1,
            colour = "red3", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.4 , hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Cartera_PFAE_E3$Etapa_3)*1.9))+
  coord_flip()+
  labs(title = "Etapa 3",
       subtitle = formatC(sum(Cartera_PFAE_E3$Etapa_3),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "red2"))

annotate_figure(ggarrange(PFAE1, PFAE2, PFAE3, ncol = 3, nrow = 1), 
                top = text_grob(paste0("Cartera PFAE ",
                                       toTitleCase(as.character(month(fecha_ant,label=TRUE,abbr=TRUE))),"-",
                                       toTitleCase(as.character(month(fecha_act,label=TRUE,abbr=TRUE))), 
                                       " ", year(fecha_act)),
                                color = "black", face = "bold", size = 22))

###################################################################################

RvaPFAE1 <- ggplot(Reserva_PFAE_E1, aes(x = Sub_clase, y = Etapa_1)) +
  geom_bar(stat = "identity", color = "green3",lwd = 0.9, 
           fill = "limegreen", alpha=0.7, width = 0.40)+
  scale_x_discrete(limits = Reserva_PFAE_E1$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1 , hjust=-0.1,
            colour = "limegreen", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.6 , hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Reserva_PFAE_E1$Etapa_1)*1.9))+
  coord_flip()+
  labs(title = "Etapa 1",
       subtitle = formatC(sum(Reserva_PFAE_E1$Etapa_1),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "limegreen"))

RvaPFAE2 <- ggplot(Reserva_PFAE_E2, aes(x = Sub_clase, y = Etapa_2)) +
  geom_bar(stat = "identity", color = "orange2",lwd = 0.9, 
           fill = "orange", alpha=0.7, width = 0.3)+
  scale_x_discrete(limits = Reserva_PFAE_E2$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1, hjust=-0.1,
            colour = "darkorange2", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.4, hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Reserva_PFAE_E2$Etapa_2)*1.9))+
  coord_flip()+
  labs(title = "Etapa 2",
       subtitle = formatC(sum(Reserva_PFAE_E2$Etapa_2),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "darkorange2"))

RvaPFAE3 <- ggplot(Reserva_PFAE_E3, aes(x = Sub_clase, y = Etapa_3)) +
  geom_bar(stat = "identity", color = "red2",lwd = 0.9, 
           fill = "red", alpha=0.7, width = 0.25)+
  scale_x_discrete(limits = Reserva_PFAE_E3$Sub_clase)+
  geom_text(aes(label = Numero), vjust = 1.1 , hjust=-0.1,
            colour = "red3", size=4)+
  geom_text(aes(label = Sub_clase), vjust = -0.4 , hjust=-0.1,
            colour = "black", size=3.5)+
  ylim(c(0, max(Reserva_PFAE_E3$Etapa_3)*1.9))+
  coord_flip()+
  labs(title = "Etapa 3",
       subtitle = formatC(sum(Reserva_PFAE_E3$Etapa_3),
                          format= "f",digits= 2, big.mark = ","),
       x = "", y = "") + theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "black",size = 13),
        plot.subtitle = element_text(color = "red2"))

annotate_figure(ggarrange(RvaPFAE1, RvaPFAE2, RvaPFAE3, ncol = 3, nrow = 1), 
                top = text_grob(paste0("Reserva PFAE ",
                                       toTitleCase(as.character(month(fecha_ant,label=TRUE,abbr=TRUE))),"-",
                                       toTitleCase(as.character(month(fecha_act,label=TRUE,abbr=TRUE))), 
                                       " ", year(fecha_act)),      
                                color = "black", face = "bold", size = 22))
