library(tree)
library(ISLR)
library(MASS)
library(GGally)

rm(list=ls())
graphics.off()
cat("\014")


### ESERCIZIO 1 - classification tree ###
### 1) Il file 'income_status.csv' e' un dataset 
###    che contiene informazioni su 155 persone, di cui sappiamo l'eta', lo stato lavorativo (group), 
###    la provenienza da una famiglia ricca (rich_parents), e lo stipendio.
###    Dalle variabili 'group' e 'rich_parents' abbiamo creato per voi una categorica a 4 livelli 
###    (chiamata 'target') che riassuma le combinazioni possibili
###    delle due categoriche (tra quelle esistenti tra i dati). 
###     Visualizzate il risultato colorando diversamente i 4 gruppi.


data1 = read.csv('income_status.csv')
str(data1)
data1$group = factor(data1$group)
data1$rich_parents = factor(data1$rich_parents)
data1$target = factor(data1$target)
data1$X = NULL
str(data1)

idxs.w = which(data1$group=="working")
idxs.yes = which(data1$rich_parents=="yes")
idxs.wyes = intersect(idxs.w, idxs.yes) #Intersezione vuota, questo gruppo non c'è

idxs.ret = which(data1$group=="retired")
idxs.retyes = intersect(idxs.ret, idxs.yes) #Intersezione vuota

table(data1$target)

x11()
plot(data1[,1:2], main='dataset 1', xlab='age', ylab='income', pch=19)
points(data1[data1$target=='no-inschool',1:2], col='red', pch=19)
points(data1[data1$target=='no-retired',1:2], col='green', pch=19)
points(data1[data1$target=='yes-inschool',1:2], col='blue', pch=19)
points(data1[data1$target=='no-working',1:2], col='pink', pch=19)
legend("topleft", legend=c('no-inschool', 'no-retired', 
                           'yes-inschool', 'no-working'), fill=c('red','green','blue','pink'))

### 2) Dividete ora il dataset in train (70%) e test (30%)
###    usate le variabili age e income per predire
### la variabile target utilizzando un albero di classificazione (sui dati di training),
### che abbia età e reddito come predittori

set.seed(1000)
train = sample(1:nrow(data1), round(0.7*nrow(data1)))  ## 70% di righe per il training

data1_train = data1[train,]
data1_test = data1[-train,]
# Albero di classificazione
library(tree)
tree.1 = tree(target~ age + income, data1_train)
summary(tree.1)

x11()
plot(tree.1)
text(tree.1,pretty=0)

### 3) Scegliete ora il miglior numero di nodi terminali tramite crossvalidazione,
###    fate pruning e plottate il nuovo albero ottenuto.
###    Facoltativo ma interessante: usando l'albero, dividete lo scatterplot dei dati in
###    rettangoli (potete usare il comando abline)

# Cross-validazione
cv.1 = cv.tree(tree.1,FUN=prune.misclass)

# grafico
x11()
plot(cv.1$size,cv.1$dev,type="b")

# Si direbbe che 4 sia il giusto numero di nodi terminali
prune.1 = prune.misclass(tree.1,best=4)

x11()
plot(prune.1)
text(prune.1,pretty=0)

### vediamo
x11()
plot(data1_train[,1:2], main='dataset 1', xlab='age', ylab='income', pch=19)
points(data1_train[data1_train$target=='no-inschool',1:2], col='red', pch=19)
points(data1_train[data1_train$target=='no-retired',1:2], col='green', pch=19)
points(data1_train[data1_train$target=='yes-inschool',1:2], col='blue', pch=19)
points(data1_train[data1_train$target=='no-working',1:2], col='pink', pch=19)
legend("topleft", legend=c('no-inschool', 'no-retired', 
                           'yes-inschool', 'no-working'), fill=c('red','green','blue','pink'))

abline(v = 21.315)
abline(h = 30127.5)
abline(h = 23471)

### 4) Fate ora predizione sul test set, 
###  calcolate la misclassification table e tutti gli indici di performance visti a lezione

# Predizione:
tree.pred = predict(prune.1,data1_test,type="class")

table(tree.pred,data1_test$target)

misclass_table=as.matrix(table(tree.pred,data1_test$target))

# accuracy    = (TP + TN)/tot
# aper = 1 - accuracy  

acc  = (misclass_table[1,1] + misclass_table[2,2] + misclass_table[3,3] + misclass_table[4,4] )/sum(misclass_table)
aper = 1 - acc

acc
aper 

# sul train

# Predizione:
tree.pred = predict(prune.1,data1_train,type="class")

table(tree.pred,data1_train$target)

misclass_table=as.matrix(table(tree.pred,data1_train$target))

# accuracy    = (TP + TN)/tot
# aper = 1 - accuracy  

acc  = (misclass_table[1,1] + misclass_table[2,2] + misclass_table[3,3] + misclass_table[4,4] )/sum(misclass_table)
aper = 1 - acc
acc
aper

### ESERCIZIO 2 - regression tree ###
### Per questo esercizio torneremo a sfruttare il dataset Hitters, gia' visto in occasione del lab sulla collinearit?,
### relativo al salario di giocatori di baseball https://rdrr.io/cran/ISLR/man/Hitters.html
rm(list = ls())
graphics.off()
cat("\014")

library(ISLR)
data2 = na.omit(Hitters)

### 1) Dividete il dataset in train (70%) e test(30%)
str(data2)

set.seed(1000)
train = sample(1:nrow(data2), round(0.7*nrow(data2)))  ## 70% di righe per il training

data2_train = data2[train, ]
data2_test = data2[-train, ]

### 2) Addestrate un albero di regressione sul train set per la variabile Salary e visualizzatelo
tree.2 = tree(Salary~., data2_train)
summary(tree.2)

x11()
plot(tree.2)
text(tree.2,pretty=0)

### 3) Effettuate a questo punto la predizione sul test set e valutate MSE
yhat = predict(tree.2,newdata=data2_test)

## Valutare l'MSE sul test set
mean((yhat-data2_test$Salary)^2) # test set MSE


### 4) Potate l'albero tramite cross-validazione
cv.2 = cv.tree(tree.2,FUN=prune.tree)

# Cambiamento della devianza al cambiare del numero di nodi terminali

x11()
plot(cv.2$size,cv.2$dev,type='b')  ## qual ? la size migliore?

#Presa la decisione, potiamo l'albero
prune.2= prune.tree(tree.2,best=3)

x11()
plot(prune.2)
text(prune.2,pretty=0)

### 5) Effettuate una nuova predizione sul test set e valutate l'MSE.

# Predizione sul dataset di test
yhat = predict(prune.2,newdata=data2_test)

mean((yhat-data2_test$Salary)^2) # test set MSE


#### ESERCIZIO 3 - Classification tree, Bagging e Random Forest #####
### Per questo esercizio torneremo a sfruttare il dataset sul Titanic, gia' visto in occasione del 
### lab sulla regressione logistica.
### Questa volta proveremo a classificare la dicotomica "Survived", usando i metodi basati su alberi.
###
### Abbiamo gia' preparato per voi il dataset con le variabili di interesse per questo esercizio:

library(titanic)

rm (list = ls())
graphics.off()
train = titanic_train

train2 = train[,c("Survived","Pclass","Sex","Age", "Fare", "Embarked")]

train2 = na.omit(train2)

train2$Pclass = factor(train2$Pclass)
train2$Embarked = factor(train2$Embarked)


### 1) Applicate un albero di classificazione per predire la classe "Survived" utilizzando le altre variabili disponibili nel dataset.
###    Dividendo il dataset in training e test set (70/30%), valutate la performance di classificazione, e identificate le variabili rilevanti per
###    la predizione della classe di interesse.

train2$Survived = factor(train2$Survived)
set.seed(1000)
train = sample(1:nrow(train2), round(0.7*nrow(train2)))  ## 70% di righe per il training

data_train = train2[train, ]
data_test = train2[-train, ]

data_train$Sex = factor(data_train$Sex)
data_train$Embarked = factor(data_train$Embarked)
data_test$Sex = factor(data_test$Sex)
data_test$Embarked = factor(data_test$Embarked)

tree.1 = tree(Survived ~ Pclass + Sex + Age + Fare + Embarked, data_train)
summary(tree.1) # variabili più importanti: "Sex" "Pclass" "Age" "Fare" 

x11()
plot(tree.1)
text(tree.1,pretty=0)

# vediamo se sia il caso di fare pruning

# Cross-validazione
cv.1 = cv.tree(tree.1,FUN=prune.misclass)

# grafico
x11()
plot(cv.1$size,cv.1$dev,type="b")

# Si direbbe che 3 sia il giusto numero di nodi terminali
prune.1 = prune.misclass(tree.1,best=3)

x11()
plot(prune.1)
text(prune.1,pretty=0)

tree.pred = predict(prune.1,data_test,type="class")

table(tree.pred, data_test$Survived )

misclass_table=as.matrix(table(tree.pred,data_test$Survived))

# accuracy    = (TP + TN)/tot
# aper = 1 - accuracy  

acc  = (misclass_table[1,1] + misclass_table[2,2])/sum(misclass_table)
aper = 1 - acc

acc
aper 


### 2) Ripetete la stessa analisi applicando l'algoritmo di Bagging, con 1000 alberi.
###    Fittate l'algoritmo sul dataset di training e stimate la performance sul test set. 
###    Identificate le variabili più rilevanti secondo il criterio dell'
###    aumento dell'errore di classificazione. 

dim(data_train)
# mtry = 5

library(randomForest)
bag1 = randomForest(Survived~. , data=data_train, mtry=5, ntree = 1000, importance=TRUE)
bag1

# importanza delle variabili
importance(bag1)
# Possiamo plottare l'importanza delle variabili
x11()
varImpPlot(bag1)


# Predizione sul test set
pred.bag1 = predict(bag1,newdata=data_test, type='class')

table(pred.bag1, data_test$Survived )
misclass_table=as.matrix(table(pred.bag1,data_test$Survived))

# accuracy    = (TP + TN)/tot
# aper = 1 - accuracy  

acc  = (misclass_table[1,1] + misclass_table[2,2])/sum(misclass_table)
aper = 1 - acc

acc
aper 

### 3) Ripetete le analisi applicando una Random Forest. Fittate l'algoritmo sullo stesso training
###    e test, dopo aver selezionato il numero ottimo di variabili da utilizzare ad ogni split
###    sulla base dell'errore OOB. Valutate la performance dell'algoritmo così definito sul test set,
###    e identificate Le variabili più rilevanti. 

dim(data_train)
# mtry = 5

# per scegliere il numero ottimo di variabili
library(randomForest)
oob = rep(0,4)
zeros = rep(0,4)
ones = rep(0,4)
test.err = rep(0,4)
for(i in 1:4) {#i=1
  bag2 = randomForest(Survived~ ., data=data_train, mtry=i, ntree = 1000, importance=TRUE)
  #' dall'help: $err.rate
  #' L'err.rate è una matrice restituita dall'algoritmo (per la sola classificazione),
  #' che ha tante righe quanti sono gli alberi della random forest e 3 colonne.
  #' Ogni colonna corrisponde a un tipo di errore.
  #' - La prima colonna corrisponde all'errore oob (quello che ci interessa qui). Come sempre,
  #'   l'i-esima riga contiene l'errore oob considerando i primi i aberi della foresta
  #' - La seconda colonna contiene l'errore di classificazione degli 0 --> false positive rate
  #' - La terza colonna contiene l'errore di classificazione degli 1 --> false negative rate.
  #' A noi interessa solo la prima colonna, ma per completezza vi facciamo vedere anche come si
  #' comportano gli altri due errori.
  oob[i] = bag2$err.rate[,1][1000]
  zeros[i] = bag2$err.rate[,2][1000]
  ones[i] = bag2$err.rate[,3][1000]
}

x11()
matplot(1:i,cbind(oob, zeros, ones),pch=19,col=c('red','blue', 'green'),type='b',ylab="OOB")
legend('topright',legend=c("OOB","0", "1"),pch=19,col=c('red','blue', 'green'))


# scegliamo mtry=2
bag2 = randomForest(Survived~. , data=data_train, mtry=2, ntree = 1000, importance=TRUE)

# importanza delle variabili
importance(bag2)
# Possiamo plottare l'importanza delle variabili
x11()
varImpPlot(bag2)


# Predizione sul test set
pred.bag2 = predict(bag2,newdata=data_test, type='class')

table(pred.bag2, data_test$Survived )

misclass_table=as.matrix(table(pred.bag2,data_test$Survived))

# accuracy    = (TP + TN)/tot
# aper = 1 - accuracy  

acc  = (misclass_table[1,1] + misclass_table[2,2])/sum(misclass_table)
aper = 1 - acc

acc
aper 

