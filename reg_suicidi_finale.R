library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
library(glmnet)
library(readxl)
library(ISLR)
require(boot)

suicidi_fine <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Script/script suicidi/suicidi_last.xlsx")

sum(is.na(suicidi_fine))

suicidi_fine1=na.omit(suicidi_fine)

dim(suicidi_fine1)

sum(is.na(suicidi_fine1))

#visualizzo i dati

summary(suicidi_fine1[3:8])

pairs(suicidi_fine1[,3:8], pch = 20,  lower.panel = NULL)

#matrice di correlazione

library(ggcorrplot)

#data(df_suic_pr)
corr <- round(cor(suicidi_fine1[3:8]), 1)
ggcorrplot(corr, hc.order = TRUE,
           outline.col = "white",
           lab = TRUE, #aggiunge numero con correlazione
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))+
  labs(caption = ("Fonte: Nostra elaborazione su dati Protezione Civile e ISTAT"))

#variabili più interessanti

library(ggpubr)

A<-ggplot(suicidi_fine1, aes(x = alcol, y = suicidi)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 2.3) +
  stat_regline_equation(label.y = 2)+
  labs(x = "Morti da alchol", y = "suicidi") +
  ggtitle("Suicidi VS morti da alchol")

B<-ggplot(suicidi_fine1, aes(x = tumori, y = suicidi))+
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 2.3) +
  stat_regline_equation(label.y = 2)+
  labs(x = "morti tumori", y = "suicidi") +
  ggtitle("Suicidi VS mortalità da tumore")

C<-ggplot(suicidi_fine1, aes(x = psichici, y = suicidi)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 2.3) +
  stat_regline_equation(label.y = 2)+
  labs(x = "morti malattie psichiche", y = "suicidi") +
  ggtitle("Suicidi VS morti per malattie psichiche")

D<-ggplot(suicidi_fine1, aes(x = disoccupazione, y = suicidi)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method='lm', formula= y~x)+
  stat_cor(label.y = 2.3) +
  stat_regline_equation(label.y = 2)+
  labs(x = "tasso disoccupazione", y = "suicidi") +
  ggtitle("Suicidi VS tasso disoccupazione")

##plot riassuntivo dei 4 casi più interessanti

ggarrange( B, C, D, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

#scelta delle variabili

library(leaps)

regfit.bwd=regsubsets(suicidi~., data=suicidi_fine1[3:8] ,nvmax=10,method="backward")
summary(regfit.bwd)
regfit.bwd=summary(regfit.bwd)
names(regfit.bwd)

regfit.bwd$bic
regfit.bwd$adjr2

plot(regfit.bwd$adjr2)
plot(regfit.bwd$cp)
plot(regfit.bwd$bic)

max(regfit.bwd$adjr2)
min(regfit.bwd$bic)

#regressione + BOOTSTRAP

boot.fn=function(data,index)                      
  
coef(lm(suicidi~disoccupazione+psichici+tumori, data=suicidi_fine1[3:8], subset=index))            

boot.fn(suicidi_fine1, 1:436)       

set.seed(1)

boot.fn(suicidi_fine1c, sample(436,436, replace=T)) 

boot(suicidi_fine1, boot.fn,1000)

#SARDEGNA + BOOTSTRAP

sardegna <- read_excel("C:/Users/LENOVO/OneDrive - Università di Cagliari/Report_SL/Script/script suicidi/sardegna.xlsx")

boot.fn=function(data,index)                      
  
coef(lm(suicidi~disoccupazione, data=sardegna, subset=index))            

boot.fn(sardegna, 1:26)       

set.seed(1)

boot.fn(sardegna, sample(26,26, replace=T)) 

bla<-boot(sardegna, boot.fn,1000)

summary(bla)