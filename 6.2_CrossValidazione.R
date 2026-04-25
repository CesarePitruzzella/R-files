#' #################################################################
#' ####################### CROSS-VALIDAZIONE #######################
#' #################################################################
#' 
rm(list = ls())
graphics.off()
cat("\014")

## VALIDAZIONE E CONFRONTO DI MODELLI DI REGRESSIONE ALTERNATIVI ------------------
#' Consideriamo il Cars dataset: velocità delle macchine e distanza di arresto
#' Vogliamo spiegare la distanza di arresto in funzione della velocità
#' 
#' 
#' Lo scopo di questo esercizio è di utilizzare l'approccio validation set per trovare 
#' l'ordine del polinomio migliore da includere nel modello lineare per Velocita' e
#' Stopping Distance 
library(boot)

data=read.csv('cars.txt', sep = " ")
str(data)

# Visualizziamo i nostri dati con un semplice scatterplot
x11()
plot(data$speed,data$dist, xlab = 'Speed', ylab = 'Stopping distance',lwd=2, las = 1)

## Proviamo a valutare vari modelli polinomiali usando l'approccio di validazione
## Costruiamo un insieme di validazione che verra' usato
## per testare le performance dei modelli

set.seed(4) # per rendere riproducibili le procedure casuali
# NOTA sul seed: ogni volta che il vostro computer genera 'casualmente' dei valori, 
# in realtà sta seguendo un algoritmo che non è veramente casuale, 
# ma parte da un determinato valore (il seed) per generare valori casuali 
# seguendo una determinata distribuzione detta "pseudo-random".
# Se volete ottenere gli stessi valori (e quindi avere tutti lo stesso output di questo 
# script), potete impostare il seed con "set.seed()", come fatto nelle righe qui sopra.


# selezioniamo 25 indici random (25 osservazioni casuali) dal dataset
train = sample(50,25) # indici
train

# Dividiamo il dataframe per comodità
data_train = data[train,]
data_test = data[-train,]

x11()
plot(data_test$speed,data_test$dist, xlab = 'Speed', ylab = 'Stopping distance', lwd=2, las = 1, col='red', pch=19) # in rosso il test set
points(data_train$speed,data_train$dist, lwd=2, las = 1, col='blue', pch=19) # In blu il training set
legend('topleft',legend=c('training set','test set'),
       col=c('blue','red'),lty=1,lwd=2)


# Step 1: Fittiamo il modello lineare sul training
lm.fit = lm(dist ~ speed, data = data_train)
coef = lm.fit$coef

x11()
plot(data_train$speed,data_train$dist, xlab = 'Speed', ylab = 'Stopping distance',
     lwd=2, las = 1, col='red', pch=19)
points(data_test$speed,data_test$dist, lwd=2, las = 1, col='blue', pch=19)
abline(a=coef[1],b=coef[2],col='grey',lwd=2)
legend('topleft',legend=c('training set','test set'),
       col=c('blue','red'),lty=1,lwd=2)

# Step 2: Calcoliamo il MSE sul test set
mean( (data_test$dist - predict(lm.fit,data_test) )^2 )

# MSE sul training set?
mean( (lm.fit$residuals )^2 )

## Osservazione:
## In generale ci aspettiamo che l'errore sul test set sia maggiore che sul modello globale, 
## ma con una sola estrazione, potrebbe capitare anche l'opposto


# Ora ripetiamo step 1 e 2 cambiando il grado del polinomio:
# proviamo a variarlo tra 1 e 8 (+ la costante per l'intercetta)

# Costruiamo il dataset con le colonne con la velocità elevata a potenza
for(p in 2:8){
  data_test = cbind(data_test, data_test$speed^p) # adding new column to dataframe increasing the degree p each column
  data_train = cbind(data_train, data_train$speed^p) # adding new column to dataframe increasing the degree p each column
  data = cbind(data, data$speed^p) # adding new column to dataframe increasing the degree p each column
  }
names(data_test) = c("speed","dist",paste0("speed",2:8))
names(data_train) = names(data) = names(data_test)

# Osserviamo i nostri dati
str(data_test)
str(data_train)

# Le righe qui di seguito ci servono a generare dei punti su cui valutare le funzioni polinomiali che
# stiamo utilizzando. Ci servono in pratica per disegnare la "linea" che rappresenta il nostro modello
data.plot = NULL
x.plot = seq(5, 40, length = 210) # x axis for plot, 210 points
for(p in 1:8)
  data.plot = cbind(data.plot, x.plot^p) # evaluating x^p for plots
data.plot = data.frame(data.plot)
names(data.plot) = c("speed",paste0('speed', 2:8))


# Ora apriamo un plot con 8 finestre per tutti i nostri modelli
x11(width=16, height=8)
par(mfrow=c(2,4))

# per ogni grado K=1,2,...,8 del polinomio considerato fittiamo un modello che ha la forma
# Y = beta.0 + beta.1 * X + ... + beta.K * X^K
# e ci salviamo il valore del MSE
MSE_train = NULL
MSE_test = NULL
for(p in 1:8){ 
  fit = lm(dist ~ . , data = data_train[,1:(p+1)]) 
  # dist ~ . vuol dire 
  # "usa tutte le variabili nel dataframe che ti passo, le categoriche solo sull'intercetta" 
  plot(data_test$speed,data_test$dist, xlab = 'Speed', ylab = 'Stopping distance', main = paste0("grado ",p),lwd=2, las = 1, col='red', pch=19) # in rosso il test set
  points(data_train$speed,data_train$dist, lwd=2, las = 1, col='blue', pch=19)
  lines(x.plot, predict(fit, data.plot))
  
  MSE_test = c(MSE_test, mean((data_test$dist - predict(fit,data_test))^2) )
  MSE_train = c(MSE_train, mean(fit$residuals^2))
  }
legend('topleft',legend=c('training set','test set'),
       col=c('blue','red'),lty=1,lwd=2)

x11()
par(mfrow=c(1,2))
plot(1:8, MSE_train,pch=16, type='b', xlab='Degree of Polynomial', 
     ylab='Mean Squared Error', ylim=c(1,10), main = 'Training MSE')
plot(1:8, MSE_test,pch=16, type='b', xlab='Degree of Polynomial', 
     ylab='Mean Squared Error', ylim=c(1,10), main = 'Test MSE')

#l’MSE sull’insieme di Training ha un andamento sempre decrescente con la complessità 
#del modello. L’errore di test, invece, presenta una decisa riduzione in corrispondenza 
#del modello “giusto”, che in questo caso è evidentemente il modello quadratico, 
#per poi rimanere praticamente costante e infine aumentare sensibilmente in presenza 
#di modelli eccessivamente complessi: siamo arrivati nella zona dell’overfitting

#' Osservazione: questi risultati dipendono dalla scelta (casuale) dell'insieme di test.
#' L'approccio con un insieme di test ha due principali controindicazioni:
#' 
#' (i)  una porzione di dati consistente (nel nostro caso il $50\%$) non viene utilizzata
#'      nel training, con conseguente aumento della varianza degli stimatori,
#' (ii) l'insieme di test è selezionato in maniera casuale, e potrebbe non essere
#'      rappresentativo della popolazione reale, in quanto particolarmente "fortunato"
#'      o "sfortunato".


graphics.off()

## CROSS-VALIDAZIONE ----------------------------------------------------------------------------
## Questa di seguito e' la funzione che ci servira' per automatizzare la cross validazione 
## nei nostri modelli lineari possiamo utilizzarla sia per Leave-one-Out CV, che per la K-fold CV.

help(cv.glm)
# Calcola l'errore di predizione stimato via K-fold cross-validation 
# per modelli lineari (e modelli lineari generalizzati)
# INPUTS:
# - data: matrice contenente i dati
# - glmfit: modello fittato con glm (i.e. output of glm)
# - cost: funzione di costo su cui cross-validare (default is average squared error)
# - K: numero dei "folds" (ATTENZIONE: il default e' n, ovvero il numero delle osservazioni. 
#     Ma questo significa fare Leave-one-Out!)
# OUTPUT:
# - delta: il primo componente è la stima di cross-validazione grezza dell'errore di predizione

set.seed(1)
# Per esempio, questo fitta il modello lineare seguente:
# Y = beta.0 + beta.1*X + eps
glm.fit = glm(dist ~ speed, data = data)
summary(glm.fit)
# oppure
lm.fit = lm(dist ~ speed, data = data)
summary(lm.fit) 
# E' esattamente lo stesso modello!
# Tuttavia per fare crossvalidazione dobbiamo usare glm.

## Cross-validazione
cv.err = cv.glm(data,glm.fit) # NB: non stiamo specificando nulla su K, quindi K=n!
cv.err$delta[1]  # L'errore in crossvalidazione

## Se voglio K-fold, per esempio con 10 folds
cv.err = cv.glm(data,glm.fit,K = 10)
cv.err$delta[1] 

##### Cross-Validazione Leave-One-Out per i diversi modelli polinomiali
# LOOCV = N-Fold Cross-Validation, 
# dove N e' il numero di osservazioni

set.seed(1)

cv.error = rep(0,8) 
mse = rep(0,8)

for (p in 1:8){
  glm.fit = glm(dist ~ . , data = data[,1:(p+1)])
  cv.error[p] = cv.glm(data[,1:(p+1)],glm.fit)$delta[1]
  mse[p] = mean(glm.fit$residuals^2)
}

cv.error # Average squared errors

x11()
plot(1:8,cv.error,type='b',xlab='Degree of Polynomial',ylim = c(min(mse),10)) # Stima dell'errore di CV
lines(1:8, mse, type = "b", col = "green") # MSE 

######## Cross-Validazione 10-Fold per i diversi modelli polinomiali

cv.error.10 = rep(0,8)
for (p in 1:8){
  glm.fit = glm(dist ~ . , data = data[,1:(p+1)])
  cv.error.10[p] = cv.glm(data[,1:(p+1)],glm.fit, K = 10)$delta[1]
}

x11()
plot(1:8,cv.error.10,type='b',xlab='Degree of Polynomial',ylim = c(min(mse),10)) # Stima dell'errore di CV
lines(1:8, mse, type = "b", col = "green") 

graphics.off()


## Osservazioni conclusive:
## cross validazione fa esattamente ciò che speriamo: produce stime di errore 
## che ricalcano quelle dell’approccio con test set, proteggendoci dall’overfitting, 
## senza dover sacrificare consistenti porzioni di dataset per avere dei test set.

