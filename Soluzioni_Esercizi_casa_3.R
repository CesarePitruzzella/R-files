
rm(list = ls())
graphics.off()
cat('\014')

library(rgl)
library(mvtnorm)
library(pracma)

############ ESERCIZIO 1 #################

#' Il file Product_ratings.txt contiene dati su una serie di prodotti nel catalogo di un noto marketplace online.
#' Per ogni mese e ogni prodotto e' elencato il totale delle review che hanno ottenuto da 5 stelle a 1 stella ('Stars_5', 'Stars_4', etc.)
#' e tutte le volte che il prodotto e' stato acquistato ma non valutato ('not_rated').

prod_data <- read.csv('Product_ratings.txt', header=T, sep = '\t')
head(prod_data)

# 1) Eseguite l'analisi delle componenti principali sul dataset Product_ratings, utilizzando solo le variabili numeriche.
#    Controllate prima di tutto se sia necessario standardizzare il dataset.

# investighiamo le variabili nel dataset
x11()
pairs(prod_data[,-c(1,2)]) # C'e' forte correlazione

x11()
boxplot(prod_data[,-c(1,2)], col = "gold")

# Siccome la variabilità è molto diversa tra le varie componenti, scegliamo di scalare i dati
data2pc <- data.frame(scale(prod_data[,-c(1,2)])) # prendiamo solo le numeriche
pc <- princomp(data2pc, scores = TRUE)
summary(pc)

# 2) Provate ad interpretare il significato dei loadings, quando possibile (in particolare, le prime due componenti
#    che significato possono avere?)
load.prod <- pc$loadings

# plottiamo le prime due componenti principali
x11()
par(mar = c(1,4,0,2), mfrow = c(2,1))
for(i in 1:2) barplot(load.prod[,i], ylim = c(-1, 1))
dev.off()

# 3) Generate il biplot della vostra PCA, considerando lo spazio generato dalle prime due componenti principali.

x11()
biplot(pc)
dev.off()

# 4) Proponete una riduzione dimensionale del dataset, motivando la vostra scelta.
cumsum(pc$sdev^2)/sum(pc$sdev^2)

x11()
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data2pc),labels=1:ncol(data2pc),las=2)
dev.off()
#' Notiamo la presenza di un gomito in corrispondenza della seconda componente principale.
#' La prima e la seconda componente insieme spiegano più del 97% della variabilità dei nostri dati.
#' Utilizzando un criterio di soglia al 95%, siamo indotti a scegliere due componenti.
#' Prima di confermare la scelta, controlliamo come si comportano i boxplot degli scores:

scores = data.frame(pc$scores)
x11()
boxplot(scores, col = "gold")

#' In questo caso, la presenza di outliers (in ciascuno dei boxplot) non maschera una
#' eventuale alta variabilità delle componenti successive alla seconda. Confermiamo la
#' scelta di tenere le prime due componenti.

######## ESERCIZIO 2 ####################

# Il dataset mtcars (gia' disponibile in R) consiste in 32 modelli di macchine.
# Per ogni macchina, avete 11 informazioni, espresse in diverse unita' di misura:

# carichiamo il dataset, trattenendo solo le informazioni numeriche.
cars_data = mtcars[,c(1:7,10,11)]
head(cars_data)       

# 1) valutate se standardizzare il dataset 'cars_data' al fine di svolgere l'analisi delle componenti principali. 
#    Per quale motivo potrebbe rivelarsi fondamentale in questo caso?

x11()
pairs(cars_data) # alcune variabili sono correlate, altre no

x11()
boxplot(cars_data, col = "gold")

# la variabilita' e' molto piu' grande per alcune delle variabili nel dataset, questo puo' essere
# dovuto al fatto che abbiano diverse unita' di misura.
# Procediamo l'analisi quindi solo dopo aver standardizzato.

# 2) Eseguite l'analisi delle componenti principali e proponete una riduzione dimensionale dei dati.

cars_scaled <- scale(cars_data) # prendiamo solo le numeriche
pc.cars <- princomp(cars_scaled, scores = TRUE)
summary(pc.cars)

### Vediamo il grafico di varianza spiegata per provare a proporre una riduzione dimensionale
x11()
plot(cumsum(pc.cars$sdev^2)/sum(pc.cars$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(cars_scaled),labels=1:ncol(cars_scaled),las=2) 

#' Notiamo un gomito in corrispondenza della seconda componente principale. Questo ci
#' suggerisce di tenere solo le prime due componenti.

# 3) Generate il biplot dei dati e provate ad interpretare i loadings delle prime due componenti principali. 
#    Come potreste raggruppare qualitativamente i modelli di macchine, una volta proiettati nello spazio
#    generato dalle prime due componenti principali?

load.cars <- pc.cars$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(2,1))
for(i in 1:2) barplot(load.cars[,i], ylim = c(-1, 1))

x11()
biplot(pc.cars)

# suggerimento: guardate al significato delle diverse variabili e provate a riconoscere se
# una distinzione tra macchine sportive, city cars e SUV.
