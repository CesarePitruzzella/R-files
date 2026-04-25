library(GGally)
library(ggfortify)
rm(list = ls())
graphics.off()
cat("\014")

#### ESEMPIO 2: PCA sul dataset food --------------------------------------------
food = read.csv('Food.txt', sep = " ")

str(food)

#Isoliamo le variabili numeriche
Countries = food$Country
food = food[,2:9]

# x11()
# ggpairs(food)
x11()
pairs(food)

cov(food)

#Standardizziamo
food.sd = scale(food)

#Calcoliamo le componenti principali
pc.food = princomp(food.sd)
summary(pc.food)

### Vediamo il grafico di varianza spiegata
x11()
plot(cumsum(pc.food$sdev^2)/sum(pc.food$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(food.sd),labels=1:ncol(food.sd),las=2) 
### notare la differenza col caso dei turisti

### Cerchiamo di interpretare i loadings delle prime 3 PC
load.food = pc.food$loadings

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3){
  barplot(load.food[,i], ylim = c(-1, 1))
}
dev.off()
## Per meglio visualizzare l'interpretazione dei loadings,
## può aiutare mettere una soglia sull'intensità dei loading da plottare
soglia = 0.3
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3){
  barplot(ifelse(abs(load.food[,i]) < soglia, 0, load.food[,i]) , ylim = c(-1, 1));abline(h=0)
}

## Interpretazione:
## PC1 : contrasto proteine VS legumi-cereali
## PC2 : Presenza o meno di frutta e pesce nella dieta (Ricordate: RISPETTO ALLA MEDIA)
## PC3 : contrasto latte + pesce / frutta + maiale

## Analisi degli score
scores.food = data.frame(pc.food$scores)
scores.food$Countries = Countries

## Vediamo i boxplots rispetto alle variabili originali standardizzate
x11()
par(mfrow = c(2,1))
boxplot(food.sd)
boxplot(scores.food[,1:8], col = "gold") # Attenzione alla seconda componente! (Outliers)

## Facciamo un po' di visualizzazione sugli scores
x11()
autoplot(pc.food,loadings = TRUE, loadings.label = TRUE) 

# Vediamo seconda e terza componente
x11()
autoplot(pc.food,loadings = TRUE, loadings.label = TRUE, x = 2, y = 3) 

## In questo caso, possiamo rappresentare i paesi direttamente sui biplot:
x11()
autoplot(pc.food,loadings = TRUE, loadings.label = TRUE, size = 0.5) + geom_text(label = scores.food$Countries)

x11()
autoplot(pc.food,loadings = TRUE, loadings.label = TRUE, x = 2, y = 3, size = 0.5) + geom_text(label = scores.food$Countries)

dev.off()

rm(list = ls())
graphics.off()

#############################################################################
############### ESEMPIO 3: PCA sui dati delle corse olimpiche ###############
#############################################################################
### PCA sui dati delle corse olimpiche: effetto della varianza
runrec = read.csv(file = "record_mod.txt", sep = " ")

str(runrec)

# Isolo le numeriche
Countries = runrec$Country
runrec$Country = NULL
# rendiamo omogenee le unità di misura (ore/minuti)
runrec[,4:7] = runrec[,4:7]*60

# Esplorazione
# x11()
# ggpairs(runrec)
x11()
pairs(runrec)

# Boxplots
x11()
boxplot(runrec) 
## Grande disomogeneità nella variabilità, che cresce non linearmente con la distanza

## Vediamo direttamente la matrice di covarianza
cov(runrec)

## E la matrice di correlazione
cor(runrec)


## Vediamo i loadings
pc_run = princomp(runrec)
summary(pc_run) 
pc_run$loadings ## La prima componente è molto guidata dalle Marathon

## Varianza spiegata
var_props = (pc_run$sdev^2) / sum(pc_run$sdev^2)
var_props ## La prima componente spiega più del 99.5 % della varianza

## Varianza spiegata cumulata
cumsum(var_props)

# Visualizziamo i loadings
load.run = pc_run$loadings
x11()
par(mar = c(3,4,0,2), mfrow = c(7,1)) # The default is c(5.1, 4.1, 4.1, 2.1)
for(i in 1:7){
  barplot(load.run[,i], ylim = c(-1,1))
}
## Struttura interessante, la disomogeneità ha una fortissima influenza


## Guardiamo lo screeplot
x11()
plot(cumsum(pc_run$sd^2)/sum(pc_run$sd^2), type='b', axes=F, xlab='numero di componenti', 
     ylab='contributo alla varianza totale', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.95, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(runrec),labels=1:ncol(runrec),las=2)


## Vediamo i biplot
x11()
autoplot(pc_run, loadings = T, loadings.label = T)

x11()
autoplot(pc_run, loadings = T, loadings.label = T, x = 2, y = 3)

#############################################################
############### ESEMPIO 3: var STANDARDIZZATE ###############
#############################################################
pc_run = princomp(scale(runrec))
summary(pc_run)
pc_run$loadings

load.run = pc_run$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3){
  barplot(load.run[,i], ylim = c(-1, 1)) ## Interpetazione MOLTO più significativa!
}

## Scree Plot
x11()
plot(cumsum(pc_run$sd^2)/sum(pc_run$sd^2), type='b', axes=F, xlab='numero di componenti', 
     ylab='contributo alla varianza totale', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.95, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(runrec),labels=1:ncol(runrec),las=2)


## Biplots
x11()
autoplot(pc_run, loadings = T, loadings.label = T, size = 0.5) +geom_text(label = Countries)

x11()
autoplot(pc_run, loadings = T, loadings.label = T, x = 2, y = 3, size = 0.5) + geom_text(label = Countries)

