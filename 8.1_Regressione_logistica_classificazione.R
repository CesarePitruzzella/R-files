## CLASSIFICAZIONE E REGRESSIONE LOGISTICA -----------------------------------------

# Si noti che la regressione logistica NON è un algoritmo di classificazione,
# infatti si limita a stimare delle probabilità.
# Tuttavia, tali stime possono essere utilizzate ai fini della classificazione.

# Utilizziamo il dataset del Titanic: 
library(MASS)
library(car)
library(rgl)
library(GGally)
library(titanic)
library(PRROC)

rm (list = ls())
graphics.off()
cat("\014")

train = titanic_train
str(train)

train2 = train[,c("Survived","Pclass","Sex","Age")]

### Aggiusto i factors
train2$Pclass = factor(train2$Pclass)
train2$Sex = factor(train2$Sex)
##eliminiamo gli NA
train2 = na.omit(train2)
str(train2)


## Regressione Logistica: a nostra risposta dicotomica sarà la sopravvivenza 
## (1 = sopravvissuto, positivo) 
## o la morte (0 = morto, negativo). Vogliamo legare tale risposta alle variabili sesso, 
## età e classe di prenotazione, e mettiamo l’effetto delle categoriche solo sull’intercetta del modello 
## per semplicità
mod = glm(Survived ~ ., family=binomial(link=logit), data= train2)   
summary(mod)
exp(coef(mod)["Pclass2"] - coef(mod)["Pclass3"])
anova(mod,test = "Chisq") # Si direbbe che tutte le variabili siano significative
# NOTA: in questo caso 1 vuol dire sopravvissuto, che sarà il nostro "positivo"
## valori fittati di probabilità:
pred = mod$fitted.values

## Ma dalle probabilità stimate, come passiamo a classificare Surv = 1 o Surv = 0?
## La cosa più intuitiva è fissare una soglia, e 
## se la p > soglia -> Surv = 1; 
## se la p < soglia -> Surv = 0

## Possiamo fissare una soglia, arbitrariamente, per esempio 0.5:

pred_class=ifelse(pred>0.5,1,0) # se la p > soglia classifico come sopravvissuto,
                                # altrimenti no

# Valutazione della bontà della classificazione sulla base
# della confusion matrix del training set:
## Misclassification table
table(pred_class, train2$Survived) # la prima sulle righe, la seconda sulle colonne
misclass_table=as.matrix(table(pred = pred_class, actual = train2$Survived))
misclass_table

## Come valuto la bontà del modello? Attraverso gli indici calcolati sulla Mislassification table: 
## sensitivity, specificity, accuracy

# sensitivity = TP /(TP + FN) = TPR
# specificity = TN /(TN + FP) = TNR
# False Positive Rate (FPR) = 1 - specificity
# accuracy    = (TP + TN)/tot 
# aper = 1 - accuracy = misclassification rate (accuracy e aper da usare con cautela. Implicitamente 
# richiedono che i dati siano bilanciati, ovvero le proporzioni nei dati osservati rispettano quelle reali)
sens = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
spec = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
fpr = 1 - spec
acc  = (misclass_table[1,1] + misclass_table[2,2])/dim(train2)[1]
aper = 1 - acc

sens
spec
fpr
acc
aper

# Vediamo come variano queste quantità all'aumento della soglia per la classificazione:
p = seq(0.2,0.9, by = 0.05)
sens = spec = acc = aper = rep(0,length(p))
for ( i in 1:length(p))
{
  pred_class=ifelse(pred>p[i],1,0) 
  misclass_table=as.matrix(table(pred_class, train2$Survived))
  sens[i] = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
  spec[i] = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
  acc[i]  = (misclass_table[1,1] + misclass_table[2,2])/dim(train2)[1]
  aper[i] = 1 -acc[i]
}

x11()
plot(p, sens, col = "red", type = "l", ylim = c(0,1))
lines(p, spec, col = "green", type = "l")
lines(p, acc, col = "salmon", type = "l" )
lines(p, aper, col = "purple", type = "l")
legend("topleft", fill = c("red","green","salmon", "purple"), legend = c("sensitivity","specificity","accuracy","aper"))

# Un possibile criterio è scegliere il punto di intersezione, se non abbiamo preferenze
# [oppure (se aper è affidabile), la soglia che lo minimizza]

## la curva ROC è una soluzione più generale:
x11()
PRROC_obj = roc.curve(scores.class0 = pred, weights.class0=train2$Survived, curve=TRUE)
plot(PRROC_obj)  ## qual è il migliore p0 per classificare? Tipicamente si
                 ## scelgono i punti più vicini all'angolo in alto a sinistra,
                 ## ma questo comunque dipende fortemente dal problema reale

#La quantità AUC, area under the curve è una misura della bontà generale del classificatore basato 
#sulle probabilità: AUC=0.5 corrisponde ad un classificatore inutile (più propriamente, un classificatore 
#che assegna le etichette a caso) mentre AUC=1 corrisponde ad un classificatore perfetto 
#(o almeno, perfetto sul training set). Valori più vicini a 1 corrispondono quindi a modelli migliori.

