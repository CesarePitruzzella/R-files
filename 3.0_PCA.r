rm(list = ls())
graphics.off()
cat("\014")

library(rgl)
library(mvtnorm)
#install.packages("pracma")
library(pracma)
#library(GGally)

#### Interpretazione geometrica della PCA --------------------------------------------
#' Cominciamo richiamando l'interpretazione geometrica della PCA.
#' Per farlo, simulo 100 dati da una popolazione normale trivariata con un dato
#' vettore media mu e una data matrice di covarianza sig.
#' NB: Non è importante che voi sappiate simulare dei dati. I dati che generiamo
#' servono soltanto per aiutarci in questa interpretazione geometrica.

#### CODICE PER GENERARE DATI TRIVARIATI CON UNA CERTA MEDIA E MATRICE COVARIANZA
set.seed(1)
nobs = 100
sig = rbind(c(9, 1, 1), c(1, 4, 1), c(1, 1, 1))
mu = c(0, 2, 3)
X = rmvnorm(nobs,mu,sig)
X
#### FINE CODICE PER GENERARE I DATI
#' I dati che abbiamo generato adesso sono contenuti nella matrice X

# Media (per colonna, cioè per ogni variabile)
M = colMeans(X) 
M

# Matrice di covarianza campionarie
S = cov(X)
S # eigen(S)

# Matrice di correlazione campionaria
R = cor(X)
R

# Visualizziamo i dati che abbiamo generato
open3d()                    
points3d(X, asp=1, size=4)  
axes3d()

# Calcoliamo le componenti principali:
PC = princomp(X)
PC

PC$loadings
PC$scores

#' Guardiamo alle componenti come approssimazioni successive.
#' La "componente 0" è la media, cioè il punto dello spazio che
#' minimizza la somma dei quadrati delle distanze

open3d()
points3d(X, asp=1, size=4)
axes3d()
points3d(t(M), col='red', size=6)
for(i in 1:nobs){
  lines3d(rbind(X[i,], M))
}

#' Prima componente principale: retta che minimizza la somma dei quadrati delle
#' distanze dei punti
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC1 = NULL
for(i in 1:nobs){
  PC1 = rbind(PC1, PC$loadings[,1]*PC$scores[i,1] + M)
}
points3d(PC1, col='red', size=6)

for(i in 1:nobs){
  lines3d(rbind(X[i,], PC1[i,]),col='blue')
}
lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]),
        col='forestgreen',lwd=2)

# Prima + Seconda componente principale: piano che minimizza la somma delle distanze dei punti
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC12 = NULL
for(i in 1:nobs){
  PC12 = rbind(PC12, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + M)
}
points3d(PC12, col='red', size=6)

for(i in 1:nobs){
  lines3d(rbind(X[i,], PC12[i,]),col='blue')
}
normal = cross(PC$loadings[,1],PC$loadings[,2])
planes3d(a = normal,d = - normal %*% M, col = "green")

#### ESEMPIO 1: PCA dei flussi turistici del 2015 a Milano -------------------------
rm(list = ls())

turisti = read.table("tourists.txt")
str(turisti)
table(turisti$Region.of.origin)
# il dataset contiene il numero di turisti a Milano nel 2015 
# per ogni mese e per ogni regione italiana di provenienza dei turisti
# (eccetto la Lombardia)
turisti$Month = factor(turisti$Month, levels = c("Jan", "Feb", "Mar", "Apr",
                                                 "May", "Jun", "Jul", "Aug",
                                                 "Sept", "Oct", "Nov", "Dec"))
turisti$Region.of.origin = factor(turisti$Region.of.origin)
# Isolo le variabili numeriche
turisti_obs = turisti[ ,-c(1,2)]
turisti_labels = turisti[ ,c(1,2)]

# Scatterplot delle variabili numeriche
#library(GGally)
#x11()
#ggpairs(turisti_obs)
x11()
pairs(turisti_obs)

## Vediamo la matrice di covarianza
cov(turisti_obs)
## C'è forte disomogeneità, questo potrebbe influire sulla PCA

## Vediamo la struttura di correlazione empirica
cor(turisti_obs)
## La struttura di correlazione sembra abbastanza forte
## Procedere con la PCA ha senso, specie per ridurre la dimensionalità

x11()
boxplot(turisti_obs, col = "gold")

## Eseguiamo la PCA tramite comando princomp
PC_tour = princomp(turisti_obs)

## Vediamo la varianza spiegata
var_props = (PC_tour$sdev^2) / sum(PC_tour$sdev^2)
var_props

## La varianza spiegata cumulata
cumsum(var_props)

## Possiamo avere un'idea immediata con summary
summary(PC_tour)

## Estraiamo i loadings
load.tour = PC_tour$loadings
load.tour


## Costruiamo a mano qualche plot per interpretare i risultati dei loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(7,1))
for(i in 1:7){
  barplot(load.tour[,i], ylim = c(-1, 1))
}
dev.off()
# Interpretazione:
# Prima PC: media pesata di 3, 4 stelle e residences
# Seconda PC: contrasto tra 3 e 4 stelle
# Terza PC: residences

## Visualizzaimo lo Scree Plot per visualizzare il contributo di ogni componente alla
## variabilità totale
x11()
plot(cumsum(PC_tour$sd^2)/sum(PC_tour$sd^2), type='b', axes=F, xlab='numero di componenti', 
     ylab='contributo alla varianza totale', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.95, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(turisti),labels=1:ncol(turisti),las=2)
dev.off()

# Riguardiamo un attimo il boxplot:
boxplot(turisti_obs, col = "gold")

# Notiamo che le prime PC sono fortemente influenzate dalle variabili con varianza più alta

#### ESEMPIO 1 - PCA dei flussi turistici con var STANDARDIZZATE --------------------------------------
# Rifacciamo l'analisi sulle variabili standardizzate (comando scale())
turisti_obs_scaled = data.frame(scale(turisti_obs))
cov(turisti_obs_scaled)
cor(turisti_obs_scaled)

# x11()
# ggpairs(turisti_obs_scaled)
x11()
pairs(turisti_obs_scaled)

PC_tour_scaled = princomp(turisti_obs_scaled)
summary(PC_tour_scaled)

load.tour_scaled = PC_tour_scaled$loadings
load.tour_scaled

# Visualizziamo i loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(7,1))
for(i in 1:7){
  barplot(load.tour_scaled[,i], ylim = c(-1, 1))
}
dev.off()
# Interpretazione
# Prima PC: alto/basso flusso di turisti
# Seconda PC: contrasto lusso/economico
# Terza PC: bed and breakfast

## Scree Plot
x11()
plot(cumsum(PC_tour_scaled$sd^2)/sum(PC_tour_scaled$sd^2), type='b', axes=F, xlab='numero di componenti', 
     ylab='contributo alla varianza totale', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.95, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(turisti),labels=1:ncol(turisti),las=2)
dev.off()

#### Visualizzazione più avanzata ------------------------------------------------
# install.packages("ggfortify")
# install.packages("ggplot2")
library(ggfortify)

new_turisti = data.frame(PC_tour_scaled$scores)
str(new_turisti)

x11()
autoplot(PC_tour_scaled) # visualizziamo gli scores sul piano ottenuto da PC1 e PC2 

# Costruiamo il BIPLOT
x11()
autoplot(PC_tour_scaled, loadings = TRUE,  loadings.label = TRUE) 

# Possiamo decidere quale componente rappresentare (x=2, y=3)
x11()
autoplot(PC_tour_scaled, x = 2, y = 3, loadings = TRUE,  loadings.label = TRUE)

# Analizziamo gli scores più in dettaglio
Scores = data.frame(PC_tour_scaled$scores)
names(Scores) = paste0("Comp",1:8)
Scores = cbind(Scores,turisti_labels)

#Vediamo la variabilità degli scores
x11()
boxplot(Scores[,1:8], col = "gold")

#Uniamo all'analisi le variabili categoriche

x11()
boxplot(Scores$Comp1 ~ Scores$Month) 

x11()
boxplot(Scores$Comp2 ~ Scores$Month)

x11()
boxplot(Scores$Comp1 ~ Scores$Region.of.origin)

x11()
boxplot(Scores$Comp2 ~ Scores$Region.of.origin) ## e così via...
