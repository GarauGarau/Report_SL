library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
library(glmnet)
library(readxl)
library(ISLR)
require(boot)


url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"

df_covid <- read_csv(file = url)

# %>% takes the output of one statement and makes it the input of the next statement.
#When describing it, you can think of it as a "THEN"

df_cases_acum <- df_covid %>%
  
  mutate(data = as.Date(data)) %>%
  
  group_by(data) %>%
  
  summarise(
    
# creates a new data frame. It will have one (or more) rows for each combination of grouping variables; 
#the input. It will contain one column for each grouping variable and one column for each of the summary
#statistics that you have specified.
    
    total_cases = sum(totale_casi, na.rm = TRUE), # for total cases on dates, exludin missing values
    
  ) %>%
  
  ungroup() %>% #removing previs gouping
  
  mutate(
    
    cases = total_cases - lag(total_cases), # for diary cases on dates
    
    #cumsum returns a vector whose elements are the cumulative sums
    
    cases_acum = cumsum(cases), # for cumulative cases
    
    #rollmean generic functions for computing rolling means,
    
    avg_cases = rollmean(cases, 7, align = "right", fill = NA) # for smooth diary cases
    
  )


italy_covid_plot <- ggplot(data = df_cases_acum, aes(x = data)) +
  
  geom_bar(aes(y = cases, fill = "Casi giornalieri"), stat = "identity", alpha = 0.4) +
  
  geom_line(aes(y = avg_cases, color = "media mobile settimanale"), size = 1.5) +
  
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
  
  labs(
    
    x = "Tempo",
    
    y = "Casi",
    
    fill = "",
    
    color = "",
    
    title = "Evoluzione giornaliera casi COVID-19\n(media mobile settimanale)",
    
    subtitle = paste0("Italia\nData: ", Sys.Date()),
    
    caption = paste0("Fonte: Nostra elaborazione su dati Protezione Civile")
    
  ) +
  
  theme_bw() +
  
  theme(
    
    legend.position = "bottom"
  )

italy_covid_plot

###################

dati_covid <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Dati/Dati pronti/dati_covid.xlsx")


#Grafico a barre 20 provincie con più casi

media <- mean(dati_covid$tot_casi)

oggetto <- ggplot(data=subset(dati_covid, tot_casi>media), aes(x = reorder(Provincia, +tot_casi), y= tot_casi , fill=Regione))+geom_bar(stat = "identity")+ coord_flip()+
  labs(
    x = "Denominazione provincia",
    y = "Totale casi",
    fill = "",
    color = "",
    title = "Province con casi sopra la media nazionale",
    subtitle = paste0("Italia\nData: ", Sys.Date()),
    caption = paste0("Fonte: Nostra elaborazione su dati Protezione Civile"))+
  theme_bw() +
  theme(legend.position = "right")+
  geom_hline(yintercept = media, linetype = 2, color = "grey35", size = 1)

oggetto

#ho rimosso i missing dai permessi di soggiorno

sum(is.na(dati_covid$perm_soggiorno))

dati_covid1=na.omit(dati_covid)

dim(dati_covid1)

sum(is.na(dati_covid1$perm_soggiorno))

#visualizzo i dati

summary(dati_covid1[3:13])

pairs(dati_covid1[,3:13], pch = 20,  lower.panel = NULL)

#matrice di correlazione

library(ggcorrplot)

corr <- round(cor(dati_covid1[3:13]), 1)
ggcorrplot(corr, hc.order = TRUE,
           outline.col = "white",
           lab = TRUE, #aggiunge numero con correlazione
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))+
  labs(caption = ("Fonte: Nostra elaborazione su dati Protezione Civile e ISTAT"))

#scatter plot delle variabili piui' ineteressanti
library(ggpubr)

A<-ggplot(dati_covid, aes(x = pendolari_K, y = tot_casi)) +
geom_point(show.legend = FALSE) +
geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 150000) +
  stat_regline_equation(label.y = 130000)+
labs(x = "pendolari_K", y = "Totale casi") +
ggtitle("Totale casi VS pendolari")


B<-ggplot(dati_covid, aes(x = densità, y = tot_casi))+
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 150000) +
  stat_regline_equation(label.y = 130000)+
  labs(x = "Densità", y = "Totale casi") +
  ggtitle("Totale casi VS Densità")

C<-ggplot(dati_covid, aes(x = perm_soggiorno, y = tot_casi)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 150000) +
  stat_regline_equation(label.y = 130000)+
  labs(x = "permessi di soggiorno", y = "Totale casi") +
  ggtitle("Totale casi VS permessi di soggiorno")

##plot riassuntivo dei 4 casi più interessanti

library(ggpubr)

ggarrange(A, B, C,
        labels = c("A", "B", "C"),
        ncol = 2, nrow = 2)