#' ######################################################################
#' #################### Bagging e Random Forests  #######################
#' ########## Sul dataset del costo delle abitazioni di Boston ##########
#' ########## facciamo un confronto fra Albero di regressione, ##########
#' ###################### Bagging e Random Forest #######################
#' ######################################################################
#' 
#' 

#install.packages("randomForest")
library(randomForest)
library(MASS)
library(tree)

rm(list = ls())
graphics.off()
cat("\014")

## Confronto fra Albero di regressione, Bagging e Random Forests ---------------
#' Consideriamo di nuovo il dataset sul costo delle abitazioni di Boston
data(Boston)
set.seed(2)
train = sample(1:nrow(Boston), nrow(Boston)*2/3)
Boston_train = Boston[train,]
Boston_test =  Boston[-train,]

#### Albero di regressione ####
tree.boston = tree(medv~.,data=Boston, subset=train)
summary(tree.boston)

x11()
plot(tree.boston)
text(tree.boston,pretty=0)

# Plot logico
tree.boston
# Riporta tutte le condizioni di splitting, Con * si indicano i nodi terminali

graphics.off()

#### Bagging ####
help(randomForest)
# randomForest:
# INPUT:
# - formula: come lm
# - data: dataframe contenente le variabili nella formula
# - subset: specifica gli indici del training set
# - na.action: cosa fare con na? Di default vengono eliminati
# - xtest, ytest: testset
# - ntree: Numero di alberi
# - mtry: numero di variabili da utilizzare per albero (bagging => mtry = p)
# - nodesize: dimensione minima dei nodi (numero minimo di osservazioni per nodo)
# - maxnodes: Numero massimo di nodi terminali
# ...
# OUTPUT:
# - call, type: informazioni sugli input del modello
# - mse: vettore di mse (calcolato con OOB)
# - confusion: matrice di confusione (per la classificazione)
# - test: Se c'? un test set, le informazioni rilevanti
#...

# Il bagging è una random forest, in cui si tengono sempre tutte le variabili ad ogni albero

dim(Boston)
# mtry = 13 => faccio bagging

bag.boston = randomForest(medv~. , data=Boston_train, mtry=13, ntree = 500, importance=TRUE)
bag.boston

bag.boston$mse[500]
# Nota:
# bag.boston$mse è un vettore che contiene l’MSE stimato tramite OOB
# (Out-Of-Bag) dopo la costruzione progressiva degli alberi.
# In particolare:
# - bag.boston$mse[50]  = MSE OOB dopo i primi 50 alberi
# - bag.boston$mse[200] = MSE OOB dopo 200 alberi
# - bag.boston$mse[500] = MSE OOB finale dopo tutti i 500 alberi (ntree = 500)
#
# Come viene calcolato, ad esempio, bag.boston$mse[50]:
# per ogni osservazione si considerano solo gli alberi (tra i primi 50)
# in cui essa non è stata inclusa nel campione bootstrap (OOB),
# si fa la media delle predizioni di questi alberi e si confronta con il valore reale.
# L’MSE è poi la media dei quadrati degli errori su tutte le osservazioni.
#
# Quindi il vettore descrive come varia l’errore al crescere del numero di alberi.

# Predizione sul test set
yhat.bag = predict(bag.boston, newdata=Boston_test)
# Stime sul test set
x11()
plot(yhat.bag, Boston_test$medv)
abline(0,1)

# MSE di test
mean((yhat.bag-Boston_test$medv)^2) 

# PARAMETER SELECTION: 
# numero di alberi (ntree) (non è un parametro sensibile di overfitting)

# Proviamo con 1000 alberi
bag.boston = randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=1000)
bag.boston

yhat.bag = predict(bag.boston,newdata=Boston_test)
mean((yhat.bag-Boston_test$medv)^2)
# L'errore è praticamente identico

#' Vediamo cosa accade al variare del numero di alberi
#' Visualizziamo l'andamento dell'errore OOB in funzione del numero di alberi
x11()
plot(bag.boston)   ## errore OOB, non c'è overfitting se ntree cresce

# L'MSE è praticamente costante da un certo punto in poi.


####  Random Forests ####
# In termini di comandi è esattamente identico, solo va ridotto il parametro mtry

# proviamo con 6 variabili alla volta
set.seed(10)
rf.boston = randomForest(medv~.,data=Boston_train,mtry=6,
                         importance=TRUE,ntree = 1000)
## default mtry = p/3 per la regressione, sqrt(p) per la classificazione
rf.boston

# Predizione sul test set
yhat.rf = predict(rf.boston, newdata=Boston_test)

# Mean Square Prediction Error
mean((yhat.rf-Boston_test$medv)^2)

x11()
plot(rf.boston) 

#' importance(): La funzione prende una random forest e valuta l'importanza delle variabili, 
#' in termini di purezza totale dei nodi terminali 
#' (per purezza intendiamo la RSS per la regressione e l'indice di Gini per la classificazione)
#' e di diminuzione in errore OOB (MSE per regressione e error rate per classificazione).
#' 
#' In particolare:
#' %IncMse: indica di quanto aumenterebbe l'MSE in OOB (Out Of Bag) se la variabile in questione 
#' venisse rimossa dal modello. Un valore maggiore di %IncMSE indica che la variabile è più importante 
#' nel determinare la qualità del modello
#' 
#' %IncNodePurity: indica di quanto aumenta la purezza dei nodi quando si usa quella variabile
#' per fare lo splitting, in media su tutti gli alberi. In pratica, si riferisce a quanto 
#' la variabile aiuta a separare meglio i dati (riducendo la somma dei quadrati degli errori, RSS). 
#' Un valore maggiore di IncNodePurity indica che la variabile è molto utile per creare separazioni 
#' nei nodi durante la costruzione degli alberi.

importance(rf.boston)

# Possiamo plottare l'importanza delle variabili
x11()
varImpPlot(rf.boston)

# Selezione dei parametri
# numero di alberi (ntree, non sensibile di overfitting), numero di variabili (mtry, sensibile di overfitting)

## Troviamo l'errore OOB e di test in corrispondenza di diversi valori di mtry
oob.err = rep(0,13)
test.err = rep(0,13)
for(mtry in 1:13){
  # Fittiamo il modello con mtry dato
  fit = randomForest(medv~.,data=Boston_train,mtry=mtry,ntree=1000)
  # salvo l'ultimo OOB stimato
  oob.err[mtry] = fit$mse[1000]
  ## Provo a fare il confronto con l'insieme di test
  pred = predict(fit, Boston[-train,])
  test.err[mtry] = mean((Boston_test$medv - pred)^2)
  # vediamo a che punto siamo
  cat(mtry," ")  
}
oob.err
test.err

x11()
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c('red','blue'),type='b',ylab="Mean Squared Error")
legend('topright',legend=c("Test","OOB"),pch=19,col=c('red','blue'))

# mtry=5 (potrebbe cambiare)
fit.best = randomForest(medv~.,data=Boston,subset=train,mtry=5,ntree=1000)
# predizione e mse
mean((Boston[-train,'medv']-predict(fit.best,Boston[-train,]))^2)


## Nota: Grazie alla possibilità di fare OOB già nella costruzione della RF, 
## si possono tenere tutti i 500 dati come training e analizzare l'OOB error 
## come stime dell'errore del test, quale è il motivo?.
## Dividendo 500 osservazioni tra training e test i risultati potrebbero essere 
## molto sensibili a variazioni (date proprio dalla suddivisione).

oob.err = rep(0,13)
for(mtry in 1:13){
  # Fittiamo il modello con mtry dato
  fit = randomForest(medv~.,data=Boston,mtry=mtry,ntree=1000)
  # salvo l'ultimo OOB stimato
  oob.err[mtry] = fit$mse[1000]
  # vediamo a che punto siamo
  cat(mtry," ")  
}
oob.err

x11()
matplot(1:13, oob.err,pch=19, type = "b")
