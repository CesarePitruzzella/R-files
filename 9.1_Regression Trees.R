#' ########################################################
#' ############# Alberi di Regressione  ###################
#' ########################################################
#' 

library(tree)
library(ISLR)
library(MASS)
library(GGally)

rm(list=ls())
graphics.off()
cat("\014")

##### Regression Tree sul dataset dei valori delle case di Boston --------------------------------------
help(Boston)
str(Boston)
View(Boston)
data(Boston)

#' crim: tasso di criminalità pro capite
#'       (i.e. numero di omicidi per numero di residenti)
#' medv: valore mediano del costo delle abitazioni in migliaia di dollari
#' zn: percentuale di terreni residenziali destinati a lotti superiori
#'     a 25000 piedi quadrati
#' nox: concentrazione di ossidi di azoto (parti per 10 milione).
#' rm: numero medio di stanze per abitazione
#' age: proporzione di abitazioni costruite prima del 1940
#' dis: media ponderata delle distanze dai cinque centri occupazionali di Boston 
#' lstat: percentuale di popolazione in condizioni di basso reddito

#' Guardiamo ad esempio le variabili: medv, crim, zn, rm, dis, lstat
x11()
ggpairs(Boston[,c(14,1,2,6,8,13)])

set.seed(02091991) #settiamo il seed per rendere le analisi ripetibili 
train = sample(1:nrow(Boston), nrow(Boston)/2) # dividiamo il dataset in 2: training e test
Boston_train = Boston[train,]
Boston_test = Boston[-train,]
help(tree) #(package tree)

# I metodi di classificazione e regressione basati su alberi suddividono 
# (stratificano e segmentano) dello spazio dei predittori in un insieme di regioni semplici, 
# dove si effettuano previsioni costanti (la media o la moda dei dati di training in ciascuna regione).
# INPUT:
# - formula: come lm
# - data: dataframe da cui prendere i dati 
# - subset: eventuale specifica degli indici del training set
# - na.action: cosa fare con gli na
# - ... Altri parametri come
#       mincut=5, numero minimo di osservazioni nei nodi terminali
#       mindev: minima devianza nei nodi
# OUTPUT:
# - where: assegnamento dei nodi
# ...

tree.boston = tree(medv~.,data = Boston_train)  ## oppure, come sempre, medv ~ cov1 + cov2 + ...
summary(tree.boston) # informazioni generali
#' qui ci racconta come è costruito l'albero, ma è molto meglio visualizzarlo..

x11()
plot(tree.boston)
text(tree.boston,pretty=0)
# Interpretazione immediata:
# lstat è la più impattante, seguita dal numero di camere
# Più basso lo status, più basso il valore, maggiore il numero di camere maggiore il valore


# Predizione sull'albero, senza pruning
# Il comando predict funziona come al solito, 
# si passa il modello e il data frame su cui si vogliono fare le predizioni

yhat = predict(tree.boston,newdata= Boston_test)
# Visualizzo le vere risposte e le predizioni:
plot(Boston_test$medv, yhat)
abline(0,1)   ## Idealmente è tutto vicino alla bisettrice

## domanda, come mai la yhat è fatta a salti? Ce lo aspettavamo?

## Valutare l'MSE sul test set
mean((yhat-Boston_test$medv)^2) # test set MSE

## Pruning e cross-validazione
# funzione prune.tree
# INPUT:
# - un albero
# - k: parametro di costo sulla complessita' dell'albero
# - best: numero di nodi che si vogliono ottenere
# - method: metodo per determinare il costo
# ...
# OUTPUT:
# - albero potato dal pruning
# - size: numero di nodi terminali
# - deviance: devianza totale della sequenza di alberi ottenuta
# ...

# Vogliamo trovare il migliore albero con 7 nodi terminali
prune.boston = prune.tree(tree.boston, best=7)
summary(prune.boston)

# vediamo
x11()
plot(prune.boston)
text(prune.boston,pretty=0)

## Ma come facciamo a scegliere il "migliore"? Cross validazione!
#' cv.tree:
#' INPUT:
#' - Albero
#' - funzione di pruning
#' - K = Numero di Folds di validazione. Il default è 10.
#' OUTPUT:
#' - Informazioni sulla cross validazione

#' REMARK sulla funzione di pruning:
#' Come default, viene invocata prune.tree nel caso di alberi di regressione e 
#' prune.misclass nel caso di alberi di classificazione

cv.boston = cv.tree(tree.boston, FUN=prune.tree)
names(cv.boston)
cv.boston$size
cv.boston$dev
cv.boston$k
cv.boston$method

# Cambiamento della devianza (RSS in questo caso) al cambiare del numero di nodi terminali
x11()
plot(cv.boston$size,cv.boston$dev,type='b')  ## qual e' la size migliore?

#Presa la decisione, potiamo l'albero
prune.boston = prune.tree(tree.boston, best=3)
summary(prune.boston)

x11()
plot(prune.boston)
text(prune.boston,pretty=0)

# Visualizziamo dunque la variabili piu' importanti

x11()
ggpairs(Boston[,c(14,6,13)])

# Predizione sul dataset di test
yhat = predict(prune.boston,newdata=Boston[-train,])
boston.test = Boston_test$medv

x11()
plot(boston.test,yhat)
abline(0,1)

mean((yhat-boston.test)^2) # test set MSE


#### Esempio aggiuntivo su Dati simulati: p grande, selezione di variabili con alberi, ridge e lasso.

rm(list=ls())

library(mvtnorm)
set.seed(1000)
# 100 osservazioni, p predittori, quindi p grande (confrontabile con n)
n = 100
p = 50

covariates = array(0, dim=c(n,p))  ## p = 50 variabili, n=100 osservazioni per ogni variabile

# La prima variabile è la risposta. Seconda e terza saranno correllate con la prima, le altre no.
matrix(data=c(1,0.7,0.7,0.7,1,0,0.7,0,1),nrow = 3,ncol = 3) 
matrix  ## cor=0.7 per le 2 variabili con la risposta

## genero le tre variabili
temp = rmvnorm(n, mean = rep(0, 3), sigma = matrix(data=c(1,0.7,0.7,0.7,1,0,0.7,0,1),nrow = 3,ncol = 3)) 
cor(temp[,1],temp[,2])
cor(temp[,1],temp[,3])
#  La prima è la risposta
resp = temp[,1]
# La seconda e la terza sono le covariate correlate
covariates[,1] = temp[,2]
covariates[,2] = temp[,3]

# Genero le altre 48 variabili, scorrelate dalla risposta
for(i in 3:p){
  covariates[,i] = rnorm(n)
}

# Plot
x11()
par(mfrow=c(3,3))
for(i in 1:9){
  plot(covariates[,i],resp, pch=19)   ## le prime due infatti solo correlate, dopo no
}

correlations = NULL
for(i in 1:p){
  correlations = c(correlations,cor(resp,covariates[,i]))
}
x11()
plot(1:p,correlations, pch=19)

data = data.frame(cbind(resp,covariates))  

names(data)
for(i in 1:p){
  names(data)[i+1] = paste('V',i,sep='')  
}
names(data)

# Albero
tree.complete = tree(resp ~., data)
summary(tree.complete)

x11()
plot(tree.complete)
text(tree.complete,pretty=0)
# Le più impattanti sono V1 e V2

# Facciamo cross-validazione

cv = cv.tree(tree.complete)
names(cv)

x11()
plot(cv$size,cv$dev,type="b")

prune = prune.tree(tree.complete,best=7)
summary(prune)

x11()
plot(prune)
text(prune,pretty=0)
#Come ci aspettavamo, vengono selezionate solo 2 variabili. 
# In generale, gli alberi sono un buon modo per fare selezione preliminare

# Ridge e Lasso (Riguardate i lab corrispondenti per i comandi)
library(glmnet)
x = model.matrix(resp~.,data)[,-1] 
y = data$resp 

# griglia di lambda
grid = 10^seq(5,-3,length=100)

# Ridge
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)

## Vediamo i coefficienti
x11()
plot(ridge.mod,xvar='lambda',label=TRUE)

cv.out = cv.glmnet(x,y,alpha=0,lambda=grid) # default: 10-fold cross validation
x11()
plot(cv.out)

# Selezioniamo il lambda migliore
bestlam = cv.out$lambda.min
bestlam
log(bestlam)

x11()
plot(ridge.mod,xvar='lambda',label=TRUE)
abline(v=log(bestlam))

predict(ridge.mod,s=bestlam,type="coefficients")

# Lasso
lasso.mod = glmnet(x,y,alpha=1,lambda=grid)
x11()
plot(lasso.mod,xvar='lambda',label=TRUE)

cv.out = cv.glmnet(x,y,alpha=1,lambda=grid) 
x11()
plot(cv.out)

#### Stavolta prendiamo il più regolarizzato
bestlam = cv.out$lambda.1se  
bestlam
log(bestlam)

x11()
plot(lasso.mod,xvar='lambda',label=TRUE)
abline(v=log(bestlam))

predict(lasso.mod,s=bestlam,type="coefficients")

# Lasso coglie quasi perfettamente nel segno
graphics.off()
