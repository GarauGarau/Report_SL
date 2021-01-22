library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
library(glmnet)
library(readxl)
library(ISLR)
require(boot)
library(gridExtra)
library(grid)

df_tot <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Script/script suicidi/serie_storica.xlsx")

#confronto per genere

ggplot(data=df_tot, aes(x = genere, y = tasso_suicidi, fill = genere)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Suicidi per genere",
       x = "genere", 
       y = "suicidi") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 2, 5), minor_breaks = F)+
  labs(caption = ("Fonte: Nostra elaborazione su ISTAT"))


  df_tot %>%
  group_by(anno) %>%
  ggplot(aes(x = anno, y = tasso_suicidi, col = factor(genere))) + 
  facet_grid(genere ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trend nel tempo, per genere", 
       x = "Anni", 
       y = "Suicidi per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1990, 2018, 5), minor_breaks = F)+
    labs(caption = ("Fonte: Nostra elaborazione su ISTAT"))

#confronto genere e tasso di disoccupazione

dis_tot<-df_tot %>%
  group_by(anno) %>%
  ggplot(aes(x = anno, y = tasso_dis, col = factor(genere))) + 
  facet_grid(genere ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trend nel tempo disoccupazione", 
       x = "anni", 
       y = "tasso disoccupazione", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1990, 2018, 5), minor_breaks = F)

suic_tot<-df_tot %>%
  group_by(anno) %>%
  ggplot(aes(x = anno, y = tasso_suicidi, col = factor(genere))) + 
  facet_grid(genere ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trend nel tempo tasso di suicidi", 
       x = "Year", 
       y = "tasso suicidi", 
       color = "anni") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1990, 2018, 5), minor_breaks = F)


grid.arrange(suic_tot, dis_tot, ncol = 2)


#andamento nazionale generale

serie_globale_suicidi <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Script/serie_globale_suicidi.xlsx", 
col_types = c("numeric", "numeric", "numeric"))

dis<-ggplot(data=serie_globale_suicidi, aes(x = anno, y = tasso_dis)) + 
  geom_line(aes(), size = 1) +
  geom_point() + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Trend tasso disoccupazione nazionale", 
       x = "Anno", 
       y = "Disoccupazione", 
       color = "Sex") + 
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(1990, 2018, 2), minor_breaks = F)

suic<-ggplot(data=serie_globale_suicidi, aes(x = anno, y = tasso_suicidi)) + 
  geom_line(aes(), size = 1) +
  geom_point() + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Trend suicidi nazionale", 
       x = "Anno", 
       y = "Suicidi", 
       color = "Sex") + 
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(1990, 2018, 2), minor_breaks = F)

grid.arrange(dis, suic, nrow = 2)

#grafico per provincia

provincie_generale <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Script/script suicidi/provincie generale.xlsx")

oggetto <- ggplot(data=subset(provincie_generale, suicidio>=0.95),  aes(x = reorder(provincia, + suicidio), y= suicidio ))+geom_bar(stat = "identity")+ coord_flip()+
  labs(
    x = "Denominazione provincia",
    y = "Tasso di suicidio",
    fill = "",
    color = "",
    title = "20 province con tasso suicidio più elevato",
    subtitle = paste0("Italia\nData: 2017 "),
    caption = paste0("Fonte: ISTAT"))+
  theme_bw() +
  theme(legend.position = "right")+
  labs(caption = ("Fonte: Nostra elaborazione su ISTAT"))

oggetto

#a questa figura accostare la mappa delle provincie con piu' suicidi

#SARDEGNA

sardegna <- read_excel("C:/Users/LENOVO/OneDrive - Università di Cagliari/Report_SL/Script/script suicidi/sardegna.xlsx")

library(ggpubr)
scatter<-ggplot(sardegna, aes(x = disoccupazione, y = suicidi)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 1.25) +
  stat_regline_equation(label.y = 1.2)+
  labs(x = "tasso disoccupazione", y = "suicidi") +
  ggtitle("Suicidi VS tasso disoccupazione")

suicidi_s<-ggplot(data=sardegna, aes(x = anno, y = suicidi)) + 
  geom_line(stat = "identity") +
  geom_point() +
  labs(title = "Suicidi in Sardegna",
       x = "anni", 
       y = "suicidi") +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1993, 2018, 2), minor_breaks = F)

dis_s<-ggplot(data=sardegna, aes(x = anno, y = disoccupazione)) + 
  geom_line(stat = "identity") +
  geom_point() +
  labs(title = "Disoccupazione in Sardegna",
       x = "anni", 
       y = "tasso di disoccupazione") +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1993, 2018, 2), minor_breaks = F)

library(ggpubr)
grid.arrange(suicidi_s, dis_s, scatter, nrow = 3)
