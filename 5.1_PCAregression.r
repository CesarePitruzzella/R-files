#' #################################################################
####### Analisi delle Componenti Principali e Regressione ##########
#' #################################################################

rm(list=ls())
library(ISLR)  
# la libreria che contiene tutti i dataset del libro di Hastie e Tibshirani

# MBA DATASET: Dati sulla Major League di baseball delle stagioni 1986 e 1987.
Hitters = na.omit(Hitters) # eliminiamo le osservazioni con dati mancanti

head(Hitters)
dim(Hitters)
names(Hitters)

help(Hitters)

x11()
hitters.data = Hitters[ ,c(1:10)]
pairs(hitters.data) 
# Collinearita' tra i predittori

# Modello: Hitters$Salary= beta0 + beta1*Hitters$AtBat + beta2*Hitters$Hits + ... + beta10*Hitters$CHmRun + eps
result = lm(Hitters$Salary ~ Hitters$AtBat + Hitters$Hits + Hitters$HmRun + Hitters$Runs + Hitters$RBI + Hitters$Walks + Hitters$Years + Hitters$CAtBat + Hitters$CHits + Hitters$CHmRun)
summary(result)

# Proviamo a ridurre il modello, togliendo una variabile alla volta 
# partendo sempre da quella con il p-value piu' alto.
s = summary(result)
sort(s$coefficients[ ,4], decreasing = T)

# Eliminiamo Hitters$Runs
result = lm(Hitters$Salary ~ Hitters$AtBat + Hitters$Hits + Hitters$HmRun + Hitters$RBI + Hitters$Walks + Hitters$Years + Hitters$CAtBat + Hitters$CHits + Hitters$CHmRun)
summary(result)

# Eliminiamo Hitters$HmRun
result = lm(Hitters$Salary ~ Hitters$AtBat + Hitters$Hits + Hitters$RBI + Hitters$Walks + Hitters$Years + Hitters$CAtBat + Hitters$CHits + Hitters$CHmRun)
summary(result)

# Eliminiamo Hitters$RBI
result = lm(Hitters$Salary ~ Hitters$AtBat + Hitters$Hits + Hitters$Walks + Hitters$Years + Hitters$CAtBat + Hitters$CHits + Hitters$CHmRun)
summary(result)

# Eliminiamo Hitters$Years
result = lm(Hitters$Salary ~ Hitters$AtBat + Hitters$Hits + Hitters$Walks + Hitters$CAtBat + Hitters$CHits + Hitters$CHmRun)
summary(result)

x11()
par(mfrow = c(2,2))
plot(result)

shapiro.test(result$residuals)

# La collinearità rende instabili le stime dei coefficienti di regressione, aumentando la loro varianza 
# e riducendo la precisione del modello. Inoltre, complica l'interpretazione dei risultati e può portare 
# a scelte errate nelle variabili da includere nel modello.

# Un'altra soluzione e' quella di svolgere una PCA sui predittori. 
# Questo risolve il problema della collinearita', e contemporaneamente riduce la dimensionalita'
# del modello senza eliminare necessariamente l'informazione portata da alcune delle variabili
# (ogni componente principale e' data da una media pesata di tutti i predittori)

# verifichiamo intanto se serva standardizzare le variabili
x11()
boxplot(hitters.data) # sembra proprio di si

pca = princomp(scale(hitters.data), scores=T)

x11()
plot(cumsum(pca$sd^2)/sum(pca$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1), lwd=2)
abline(h=1, col='red', lwd=1.5)
abline(h=0.8, lty=2, col='blue', lwd=1.5)
box()
axis(2, at=0:10/10, labels=0:10/10)
axis(1, at=1:ncol(hitters.data), labels=1:ncol(hitters.data), las=2)

# osserviamo i loadings delle prime due componenti principali
x11()
par(mar = c(1,4,0,2), mfrow = c(2,1))
for(i in 1:2) barplot(pca$loadings[,i], ylim = c(-1, 1),
                      names.arg = rownames(pca$loadings),  # nomi delle variabili
                      las = 2)
# - PC1 : media di tutti i regressori
# - PC2 : contrasto tra performance del 1986 e quelle di tutta la carriera sportiva

x11()
par(mfrow=c(2,1))
boxplot(scale(hitters.data))
boxplot(pca$scores)

# Estraiamo le prime due componenti principali
sp1.pc = pca$scores[ ,1]
sp2.pc = pca$scores[ ,2]

# Utilizziamole per costruire un modello: Hitters$Salary = beta0 + beta1*PC1 + beta2*PC2 + eps
result = lm(Hitters$Salary ~ sp1.pc + sp2.pc)
summary(result)

# Il modello non ha un R^2 adjusted maggiore di quello precedente, ma siamo sicuri che non 
# esista piu' il problema della collinearita' tra i predittori. Inoltre, rispetto al precedente, 
# con soli due termini stiamo "riassumendo" l'informazione di tutti i predittori originali.

# diagnostica
x11()
par(mfrow = c(2,2))
plot(result)

shapiro.test(result$residuals)

dev.off()
