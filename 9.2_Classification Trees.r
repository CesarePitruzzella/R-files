#' ########################################################
#' ############# Alberi di Classificazione  ###############
#' ########################################################
#' 

library(tree)
library(ISLR)
library(MASS)
library(car)

rm(list = ls())
graphics.off()
cat("\014")

#####  Carseats dataset ####
#' Carichiamo il Carseats dataset, che contiene informazioni sulle vendite di
#' seggiolini da auto per bambini in 400 diversi negozi.

Carseats = Carseats 
str(Carseats)

?Carseats

# Classificazione per Sales, Alto (> 8), Basso (< 8)
x11()
hist(Carseats$Sales)   
abline(v=8,lwd=3,col='red')

# Costruiamo la variabile categorica corrispondente
Carseats$High = ifelse(Carseats$Sales<=8,"No","Yes")
Carseats$High = as.factor(Carseats$High)
table(Carseats$High)

# Fittiamo un albero con tutte le altre variabili su un training set
set.seed(02091991)
train = sample(1:nrow(Carseats), 200)
Carseats_train = Carseats[train,]
Carseats_test = Carseats[-train,]
#' 200 righe per training --> 400 - 200 = 200 righe anche per il test

tree.carseats = tree(High~.-Sales, Carseats_train)
summary(tree.carseats)

#' Sembra che la variabile piu' impattante sia il budget che l'azienda alloca
#' per l'advertisement di quella sede

x11()
plot(tree.carseats)
text(tree.carseats,pretty=0)

# Vediamo gli errori sull'insieme di test e su quello di training
tree.pred_test = predict(tree.carseats,Carseats_test,type="class")
tree.pred_training = predict(tree.carseats, Carseats_train,type="class")

# Misclassification table sul test set
tab = table(tree.pred_test, actual = Carseats_test$High)
tab
# Accuratezza:
(tab[1,1]+tab[2,2])/200 
# Errore di misclassificazione:
(tab[1,2]+tab[2,1])/200

## Sul train
# Tabella di misclassificazione:
tab = table(tree.pred_training, actual = Carseats_train$High)
# Accuratezza:
(tab[1,1]+tab[2,2])/200 
# Errore di misclassificazione:
(tab[1,2]+tab[2,1])/200
#' Grande differenza! L'errore sul train è ingannevole!
#' Siamo in presenza di un chiaro esempio di overfitting

# Pruning e cross-validazione
#' NB: nel caso degli alberi di classificazione bisogna specificare
#' la funzione prune.misclass (e non prune.tree, che va bene per gli alberi
#' di regressione)
cv.carseats = cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats

# Vediamo
x11()
plot(cv.carseats$size, cv.carseats$dev, type="b")

# Vediamo l'albero selezionato
prune.carseats = prune.misclass(tree.carseats, best=9)
summary(prune.carseats)

x11()
plot(prune.carseats)
text(prune.carseats,pretty=0)

# prediction su test
tree.pred = predict(prune.carseats, Carseats_test, type="class")
tree.pred.p = predict(prune.carseats,Carseats_test)

tab = table(tree.pred, actual = Carseats_test$High)
tab
# Accuratezza
(tab[1,1]+tab[2,2])/(tab[1,1]+tab[2,2]+tab[2,1]+tab[1,2])
# abbiamo perso pochissimo in termini di performance
# sul test set, ma ridotto di molto la complessità!

# prediction su training
tree.pred_training = predict(prune.carseats, Carseats_train,type="class")

tab = table(tree.pred_training, actual = Carseats_train$High)
tab
# Accuratezza:
(tab[1,1]+tab[2,2])/200 


graphics.off()


### Esempio con risposta categorica a 3 livelli
##### Classification Trees: Iris dataset #####
# Classifichiamo la specie a partire da lunghezza e larghezza del petalo 
rm(list = ls())
head(iris)
names(iris)

species.name = iris$Species
iris2 = iris[,3:4] ## Solo informazioni sul petalo

i1 = which(species.name=='setosa')
i2 = which(species.name=='versicolor')
i3 = which(species.name=='virginica')

n1 = length(i1)
n2 = length(i2)
n3 = length(i3)
n = n1+n2+n3


# Visualizziamo
x11()
plot(iris2, main='Iris Petal', xlab='Petal.Length', ylab='Petal.Width', pch=19)
points(iris2[i1,], col='red', pch=19)
points(iris2[i2,], col='green', pch=19)
points(iris2[i3,], col='blue', pch=19)
legend("topleft", legend=levels(species.name), fill=c('red','green','blue'))


# creiamo il dataframe con tutti i dati utili
iris.df = data.frame(iris2,species.name)

# Training e Test
set.seed(1000)
train = sample(1:nrow(iris.df), 100)  ## 100 righe a caso per il training
iris.test = iris.df[-train,]

# Albero di classificazione sulle specie
tree.iris = tree(species.name ~., iris.df[train,])
summary(tree.iris)

#Vediamo i risultati
x11()
plot(tree.iris)
text(tree.iris,pretty=0)

# Cross-validation

cv.iris = cv.tree(tree.iris,FUN=prune.misclass)
names(cv.iris)
cv.iris

# Plot of the misclassification error as a function of size
x11()
plot(cv.iris$size,cv.iris$dev,type="b")

# Si direbbe che 3 ? il giusto numero di nodi terminali
prune.iris = prune.misclass(tree.iris,best=3)

x11()
plot(prune.iris)
text(prune.iris,pretty=0)

# Struttura logica
prune.iris
summary(prune.iris)

#Vediamo le regioni di decisione
x11()
plot(iris2, main='Iris Petal', xlab='Petal.Length', ylab='Petal.Width', pch=19)
points(iris2[i1,], col='red', pch=19)
points(iris2[i2,], col='green', pch=19)
points(iris2[i3,], col='blue', pch=19)
legend("topleft", legend=levels(species.name), fill=c('red','green','blue'))

abline(v=2.45)
abline(h=1.75)

# Predizione:
tree.pred = predict(prune.iris,iris.test,type="class")

table(tree.pred, actual = iris.test$species.name)
errort = (tree.pred != iris.test$species.name)
errort

AERt   = sum(errort)/length(iris.test$species.name)
AERt
