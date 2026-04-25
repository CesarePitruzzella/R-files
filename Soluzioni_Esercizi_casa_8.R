# ------------------------------------------------------------------
# ESERCIZI A CASA - REGRESSIONE LOGISTICA
# ------------------------------------------------------------------

library(MASS)
library(rgl)
library(GGally)
library(PRROC)

#' ------------------------------------------------------------------
#' ESERCIZIO 1
#' Il file hurricanes.csv contiene i dati raccolti da James B. Elsner e 
#' e i colleghi, lavorando alla classificazione degli uragani del Nord
#' America. Nel file sono contenuti due tipi di uragani: uragani tropicali
#' (Type = 0) e non-tropicali (Type = 1). Le altre informazioni riguardano
#' la localizzazione del punto di formazione e delle zone in cui l'uragano
#' si e' spostato.
#'
#' Quanti uragani per classe ci sono nel dataset? Si tratta di un problema
#' di classificazione bilanciato?
#' 
#' 1) Costruite un modello di regressione logistica semplice per predire
#'    il tipo di uragano sulla base della latitudine di origine (FirstLat).
#' 2) Valutate il modello fittato sull'intero dataset in termini di performance
#'    di classificazione, disegnandone la ROC curve. 
#' 3) Scegliete una soglia per il vostro classificatore sulla base dei principali
#'    indici di performance di classificazione.
#'    (suggerimento: testate diverse soglie, calcolate la tabella di misclassificazione
#'    per ciascuna soglia e da lì calcolate gli indici visti in classe. Quale 
#'    soglia è più ragionevole?)
#' 4) Come cambia la situazione aggiungendo l'informazione sulla latitudine 
#'    dove l'uragano si è estinto? (variabile LastLat)
#' ------------------------------------------------------------------

hurricanes_data = read.csv("hurricanes.csv")
head(hurricanes_data)

table(hurricanes_data$Type) # abbastanza bilanciato

## 1) Definiamo il modello

mod.h <- glm(Type ~ FirstLat, family=binomial(link=logit), data= hurricanes_data)   
summary(mod.h)

## facciamo previsione
pred = predict(mod.h, hurricanes_data, type='response')

## 2) Disegnamo la ROC curve

PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0=as.numeric(paste(hurricanes_data$Type)), curve=TRUE)
x11()
plot(PRROC_obj)  

## 3) Scegliamo la soglia per la classificazione

p = seq(0.2,0.9, by = 0.05)
sens = spec = acc = aper = rep(0,length(p))
for ( i in 1:length(p))
{
  pred_class=ifelse(pred>p[i],1,0) 
  misclass_table=as.matrix(table(pred_class, hurricanes_data$Type))
  sens[i] = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
  spec[i] = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
  acc[i]  = (misclass_table[1,1] + misclass_table[2,2])/dim(hurricanes_data)[1]
  aper[i] = 1 -acc[i]
}

x11()
plot(p, sens, col = "red", type = "l", ylim = c(0,1))
lines(p, spec, col = "green", type = "l")
lines(p, acc, col = "salmon", type = "l" )
lines(p, aper, col = "purple", type = "l")
legend("topleft", fill = c("red","green","salmon", "purple"), legend = c("sensitivity","specificity","accuracy","aper"))

## 4) Modello multivariato

mod.h <- glm(Type ~ FirstLat + LastLat, family=binomial(link=logit), data= hurricanes_data)   
summary(mod.h)  ## tutto è in linea con il film drama, povero Jack

## facciamo previsione
pred = predict(mod.h, hurricanes_data, type='response')

PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0=as.numeric(paste(hurricanes_data$Type)), curve=TRUE)
x11()
plot(PRROC_obj)  # area sotto la curva leggermente più bassa

p = seq(0.2,0.9, by = 0.05)
sens = spec = acc = aper = rep(0,length(p))
for ( i in 1:length(p))
{
  pred_class=ifelse(pred>p[i],1,0) 
  misclass_table=as.matrix(table(pred_class, hurricanes_data$Type))
  sens[i] = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
  spec[i] = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
  acc[i]  = (misclass_table[1,1] + misclass_table[2,2])/dim(hurricanes_data)[1]
  aper[i] = 1 -acc[i]
}

x11()
plot(p, sens, col = "red", type = "l", ylim = c(0,1))
lines(p, spec, col = "green", type = "l")
lines(p, acc, col = "salmon", type = "l" )
lines(p, aper, col = "purple", type = "l")
legend("topleft", fill = c("red","green","salmon", "purple"), legend = c("sensitivity","specificity","accuracy","aper"))

#' --------------------------------------------------------------------------
#' ESERCIZIO 2
#' Il dataset myopia contiene 618 soggetti che presentano miopia (myopic 2:Yes, 1:No)
#' 
#' 1) Costruite un modello di regressione logistica per predire la probabilità di miopia
#'    dei soggetti sulla base delle diottrie (variabile spheq - "Spherical Equivalent
#'    Refraction") e delle ore spese a studiare settimanalmente fuori dalla scuola
#'    (studyhr).
#' 2) Di quanto aumentano gli odds di essere miopi per ogni ora in piu' di studio
#'    settimanale?
#' 3) Suddividendo il dataset in training e test set (70% e 30% del dataset rispettivamente)
#'    costruite la ROC curve del modello, definite una soglia e valutate sul test set
#'    il classificatore ottenuto.
#' ------------------------------------------------------------------------------

#install.packages("aplore3")
library(aplore3)
str(myopia)

#install.packages("caret")
library(caret)

## 1) 
myopia_ds = myopia

myopia.glm <- glm(myopic ~ spheq + studyhr, family=binomial(link=logit), data= myopia_ds)   
summary(myopia.glm) 

## 2) 
exp(1*coef(myopia.glm)[3])   # l'odds ratio aumenta di 0.83 per ogni ora in più a settimana spesa studiando


## 3)

# definiamo il dataset in train (70%) e test (30%)
n_train = round( 0.7*dim(myopia_ds)[1] )
train = sample(dim(myopia_ds)[1], n_train)

myopia_ds_train = myopia_ds[ train, ]
myopia_ds_test = myopia_ds[ -train, ]

table(myopia_ds_train$myopic) # unbalanced

# fittiamo il modello su training
myopia.glm.TRAIN <- glm(myopic ~ spheq + studyhr, family=binomial(link=logit), data= myopia_ds_train)
summary(myopia.glm.TRAIN)  

pred = predict(myopia.glm.TRAIN, myopia_ds_train, type='response')

# disegnamo la curva roc 
PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0=as.numeric(myopia_ds_train$myopic)-1, curve=TRUE)
x11()
plot(PRROC_obj)  # scelgo soglia a 0.18


# valutiamo classificatore ottenuto su test set 
pred = predict(myopia.glm.TRAIN, myopia_ds_test, type='response')

pred_class=ifelse(pred>0.18,1,0) # se la p > soglia -> myopic = 1, altrimenti 0
misclass_table=as.matrix(table(pred_class, as.numeric(myopia_ds_test$myopic)-1))
sens = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
spec = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
acc = (misclass_table[1,1] + misclass_table[2,2])/dim(myopia_ds_test)[1]
aper = 1-acc

sens
spec
acc
aper

PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0=as.numeric(myopia_ds_test$myopic)-1, curve=TRUE)
x11()
plot(PRROC_obj)  # soglia a 0.18 su test set è confermata essere buona


# proviamo a vedere se effettivamente il plot di sensitivity,specificity,accuracy,aper
# ci dà un risultato coerente alla nostra soglia scelta
p = seq(0.05,0.3, by = 0.05)
sens = spec = acc = aper = rep(0,length(p))
for ( i in 1:length(p))
{
  pred_class=ifelse(pred>p[i],1,0) # se la p > soglia -> myopic = 1, altrimenti 0
  misclass_table=as.matrix(table(pred_class, as.numeric(myopia_ds_test$myopic)-1))
  sens[i] = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
  spec[i] = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
  acc[i]  = (misclass_table[1,1] + misclass_table[2,2])/dim(myopia_ds_test)[1]
  aper[i] = 1 -acc[i]
}

x11()
plot(p, sens, col = "red", type = "l", ylim = c(0,1))
lines(p, spec, col = "green", type = "l")
lines(p, acc, col = "salmon", type = "l" )
lines(p, aper, col = "purple", type = "l")
legend("topleft", fill = c("red","green","salmon", "purple"), legend = c("sensitivity","specificity","accuracy","aper"))

