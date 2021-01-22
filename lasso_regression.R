#applicazione lasso regresison

library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
library(glmnet)
library(readxl)
library(ISLR)
require(boot)


dati_covid <- read_excel("C:/Users/Alessio/OneDrive - Università di Cagliari/Report_SL/Dati/Dati pronti/dati_covid.xlsx")

#ho rimosso i missing dai permessi di soggiorno

sum(is.na(dati_covid))

dati_covid1=na.omit(dati_covid)

dim(dati_covid1)

data_lasso <- dati_covid1[3:13]
View(data_lasso)
# A causa della numerosità delle covariate (e poichè ci aspettiamoc che la variabile sia 
#associata solo ad una parte dei coefficienti) si è deciso di utilizzare dei metodi di shrinkage.
#Nello specifico abbiamo scelto la lasso regression al posto della ridge regression in modo che
#i coefficienti non significativi fossero effettivamente condotti a 0.

x = model.matrix(tot_casi~., data = data_lasso)
y = data_lasso$tot_casi

lasso.mod=glmnet(x,y,alpha=1)
plot(lasso.mod,xvar="lambda",label=TRUE)
lasso.mod=cv.glmnet(x,y)

#how to choose best lambda with cv
#10 fold per capire qual'e' il modello migliore in modo data_driven. Si specifica come non abbiamo
#usato altre misure di goodness of fit (Cp, R2adj e Bic) poiche' presuppongono che si conosca il numero
#il numero di predittori del modello.

set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)

lam_migliore=cv.out$lambda.min
lam_migliore

# refit our lasso regression model on the full data set, using the value of lambda chosen by cross-validation, and examine the coefficient estimates
out=glmnet(x,y,alpha=1)
lasso.coef=predict(out,type="coefficients",s=lam_migliore)[1:11,]
lasso.coef

lasso.coef[lasso.coef!=0]
