rm(list = ls())
graphics.off()
cat("\014")

#################### Clustering: K-MEANS #######################
# install.packages("cluster")
library(rgl)
library(cluster)
library(ggfortify)


##### ESEMPIO 1: Nidificazione -------------------------------------------------
# Leggiamo i dati relativi alla posizione della nidificazione di alcuni uccelli,
# conosciamo solo le posizioni (non quante e quali specie nidificano)
# e vorremmo fare delle supposizioni
nidi = read.csv("Nidificazione.txt")
head(nidi)
x11()
plot(nidi, xlab='X', ylab='Y', pch = 20)

#______________________________________________________________________
# RICHIAMO TEORICO SUL FUNZIONAMENTO DEL KMEANS
# Algoritmo K-means per clusterizzare le osservazioni
# Proviamo a farlo "a mano", per vedere cosa succede
k = 2
n = length(nidi$X)
cluster = sample(1:2, n, replace=TRUE)  # campioniamo a caso i cluster iniziali
iter.max = 3 # tre iterazioni dell'algoritmo

colplot = c('royalblue','red') # per dare colore a cluster 1 e 2 rispettivamente
colpoints = c('blue4','red4')

# Plot per capire meglio il funzionamento del k-means
x11()
par(mfrow = c(iter.max,3))
for(i in 1:iter.max) 
{
  plot(nidi, col = colplot[cluster], pch=19, main= paste('Clusters at iteration', i-1),
       xlab='X', ylab='Y')
  
  C = NULL
  for(l in 1:k)
    C = rbind(C, colMeans(nidi[cluster == l,])) 
  # per ognuno dei k cluster, calcolo la media per X e Y e ottengo il centroide

  plot(nidi, col = 'grey', pch=19, main= paste('Centroids - iteration', i-1),
       xlab='X', ylab='Y')
  points(C, col = colpoints, pch = 4, cex = 2, lwd = 2)

  QC = rbind(C, nidi) 
  # prendo le distanze tra gli n punti e i k centroidi
  # quindi: calcolo la matrice delle distanze as.matrix(dist(QC, method = 'euclidean'))
  # e poi prendo le distanza tra prime n righe escludendo i k centroidi (k+1):(k+n)) ed i centroidi (1:k)
  # ottenendo quindi Dist di dimensione [n x k]
  Dist = as.matrix(dist(QC, method = 'euclidean'))[(k+1):(k+n),1:k] 
  for(j in 1:n)
    cluster[j] = which.min(Dist[j,]) 
  # ad ogni osservazione attribuisco il cluster con distanza minima dal centroide
  plot(nidi, col = colplot[cluster],pch=19, main= paste('Updated clusters - iteration', i),
       xlab='X', ylab='Y')
  points(C, col = colpoints, pch = 4, cex = 2, lwd = 2)
}
# colplot[cluster]
graphics.off() 

#FINE RICHIAMO TEORICO
#______________________________________________________________________

# La funzione 'kmeans' automatizza tutto
help(kmeans)
# INPUT:
# - nidi: dati da raggruppare (primo parametro da passare)
# - centri: numero di cluster o vettore di centroidi iniziali
# - iter.max: numero Massimo di iterazioni da eseguire.
# - distanza: distanza da utilizzare
# - nstart: specifica quante volte ripetiamo kmeans per diverse inizializzazioni (aiuta a trovare un minimo globale)
# ...
# OUTPUT:
# - cluster: vettore di etichette assegnate alla nostra osservazione
# - centri: centri dei cluster
# - totss: somma totale dei quadrati
# - withinss: somma dei quadrati all'interno del cluster (vettore: un componente per cluster)
# - tot.withinss: somma totale dei quadrati all'interno del cluster (cioè, sum(withinss))
# - betweeness: somma dei quadrati between-cluster (totss-tot.withinss)
# - size: dimensione di ogni cluster
# ...


# k = 2
km.out2 = kmeans(nidi, centers = 2)

km.out2$cluster
km.out2$centers
km.out2$totss
km.out2$withinss
km.out2$tot.withinss # ovvero sum(km.out$withinss)
km.out2$betweenss

#Plottiamo i cluster
x11()
plot(nidi, col=colplot[km.out2$cluster],
     main="K-Means Clustering Results with K=2",
     pch=20, cex=2, xlab='X', ylab='Y')
# ricordiamo che colplot è semplicemente un vettore di due colori
#colplot[km.out$cluster]

# Se è più chiaro, notate che in modo equivalente si possono settare i colori dei punti
# appartenenti ai due cluster con
x11()
plot(nidi, col=km.out2$cluster, main="K-Means Clustering Results with K=2",
     pch=20, cex=2, xlab='X', ylab='Y')

# Proviamo ora con
# k = 3
km.out3 = kmeans(nidi,centers=3)
km.out3$cluster
km.out3$centers
km.out3$totss
km.out3$withinss
km.out3$tot.withinss # ovvero sum(km.out$withinss)
km.out3$betweenss

#Plottiamo i cluster
x11()
plot(nidi, col=(km.out3$cluster), main="K-Means Clustering Results with K=3", 
     pch=20, cex=2, xlab='X', ylab='Y')

# vediamo ora come cambiano le cose se aggiungiamo nstart 
# (che specifica quante volte ripetiamo kmeans per diverse inizializzazioni)
km.out3 = kmeans(nidi,3) 
km.out3$tot.withinss # inizializzazione random, cambia ogni volta
km.out3_multi = kmeans(nidi,3,nstart=20) # R seleziona il clustering con minor tot.withinss

km.out3_multi$size

km.out3_multi$tot.withinss # tipicamente inferiore al caso singola istanza
# infatti, noi vorremmo scegliere la configurazione che minimizzi 
# la somma dei quadrati intra-cluster totale

#_________________________________________________________________
#'              Selezione di K 

## Vari criteri:

#1) Grafico della within sum of squares  (tot.withinss)
tot.withinss = NULL
for(i in 1:10){
  tot.withinss = c(tot.withinss,kmeans(nidi,i,nstart=20)$tot.withinss) 
}

x11()
plot(1:10,tot.withinss,type='b', xlab='K', ylab="Total WSS")   ## ragionevole
# ci aspettiamo andamento monotono decrescente, troviamo un gomito (elbow)?
# è rilevante il guadagno tra k=1 e k=2, mentre l'aggiunta di altri cluster
# oltre al secondo produce guadagni sempre minori, sempre più marginali

#2) Grafico della silhouette per k = 3 e k = 2
# Si ricorda che:
# Silhouette di circa 1 significa che i cluster sono molto densi e ben separati
# Silhouette di circa 0 significa che i cluster si sovrappongono 
# Silhouette minore di 0 significa che i cluster potrebbero essere sbagliati/non corretti

x11()
silhouette_3cluster = silhouette(km.out3_multi$cluster , dist(nidi) )
plot(silhouette_3cluster, col = 1:3, main = 'Silhouette plot per k=3')
abline(v = mean(silhouette_3cluster[,3]))

x11()
km.out2_multi = kmeans(nidi,centers = 2, nstart = 20)
silhouette_2cluster = silhouette(km.out2_multi$cluster , dist(nidi) )
plot(silhouette_2cluster, col = 1:2,  main = 'Silhouette plot per k=2')
abline(v = mean(silhouette_2cluster[,3]))
# l'indice di silhouette indica che la soluzione con K=2
# produce cluster di qualità migliore

#### ESEMPIO 2: Dati simulati -------------------------------------------------------
#_________________________________________________________________
# Cerchiamo di capire meglio i due criteri (tot.withinss e silhouette)
# visualizzando due diverse configurazioni: 
#          A - gruppi ben separati
#          B - gruppi quasi sovrapposti

### Codice per simulare i dati
n = 100
set.seed(1)
data = matrix(rnorm(n*2), ncol=2)
data[1:(n/2),1] = data[1:(n/2),1]+5
data[1:(n/2),2] = data[1:(n/2),2]-5
### Fine del codice per simulare i dati

## A - Cosa accade quando i gruppi sono ben separati?
x11()
plot(data,pch=20,cex=2, xlab='X', ylab='Y')

# Vediamo cosa accade alla tot.withinness
tot.withinss = NULL
for(i in 1:10){
  tot.withinss = c(tot.withinss,kmeans(data,i,nstart=20)$tot.withinss)
}
x11()
plot(1:10,tot.withinss,type='b', xlab='K', ylab="Total WSS") 
# gomito molto evidente: k=2 sembra essere la migliore soluzione

# Vediamo la silhouette
x11()
par(mfrow = c(2,2))
for ( i in 2:5)
{ 
  output.sim = kmeans(data, centers = i, nstart = 20)
  silhouette.sim = silhouette(output.sim$cluster , dist(data) )
  plot(silhouette.sim, col = 1:i, main=paste("Silhouette per k =",i,sep=" "))
  abline(v = mean(silhouette.sim[,3]))
}
graphics.off()
# K=2 sembra essere la scelta migliore anche per la silhouette

## B - Cosa accade con gruppi quasi sovrapposti?
### Codice per simulare i dati
set.seed(1)
n=100
data = matrix(rnorm(n*2), ncol=2)
data[1:(n/2),1] = data[1:(n/2),1]+0.5
data[1:(n/2),2] = data[1:(n/2),2]-0.5
### Fine del codice per simulare i dati

x11()
plot(data,pch=20,cex=2, xlab='X', ylab='Y')

tot.withinss <- NULL
for(i in 1:10){
  tot.withinss <- c(tot.withinss,kmeans(data,i,nstart=20)$tot.withinss)
}
x11()
plot(1:10,tot.withinss,type='b', xlab='K', ylab="Total WSS")  
## qui è difficile identificare un k minore del massimo che funzioni

# Si noti che, a volte, si preferisce guardare al grafico della tot.withinss normalizzandolo rispetto 
# alla total sum of squares ovvero la variabilità totale. 
# Si ottiene una curva del tutto equivalente ai fini interpretativi, 
# che ha però il pregio di assumere valori tra 0 e 1: 
# la si può interpretare come frazione della variabilità totale dovuta alla variabilità intra-cluster
ratio = NULL
for(i in 1:10){
  kmeans_out = kmeans(data,i,nstart=20)
  ratio = c(ratio,kmeans_out$tot.withinss/kmeans_out$totss)
}
x11()
plot(1:10,ratio,type='b', xlab='K', ylab = "tot.withinss/totss")
# l'andamento della curva è esattamente identico: 
# ogni suo punto è semplicemente stato diviso per una costante, la variabilità totale. 
# Ricordiamo infatti che, mentre sia betweenss che tot.withinss sono proprietà della clusterizzazione, 
# la loro somma, cioè la variabilità totale, dipende solo dai dati.
# Questa curva può essere interpretata come
# frazione della variabilità totale dovuta alla variabilità intra-cluster

x11()
par(mfrow = c(2,2))
for ( i in 2:5)
{ 
  output.sim = kmeans(data, centers = i, nstart = 20)
  silhouette.sim = silhouette(output.sim$cluster , dist(data) )
  plot(silhouette.sim, col = 1:i, main=paste("Silhouette per k =",i,sep=" "))
  abline(v = mean(silhouette.sim[,3]))
}
# Come ci aspettavamo, non abbiamo indicazioni precise

graphics.off()

##### ESEMPIO 3: Dati sui terremoti ----------------------------------------------
# Clustering del set di dati sui terremoti tramite kmeans
# Il set di dati fornisce le posizioni di 1000 eventi sismici di MB > 4.0.
# Gli eventi si sono verificati in un cubo vicino alle Fiji dal 1964.
str(quakes)
head(quakes)
# Vogliamo raggruppare i terremoti in base a profondità e posizione
Q <- cbind(quakes[,1:2], depth = -quakes[,3]/100)
# cambiamo unità di misura di depth per comodità
head(Q)

# Scatterplot 3d
plot3d(Q, size=3, col='orange', aspect = F)

## Applichiamo il kmeans
## Calcoliamo ora la "total within sum of squares" e la "between sum of squares"
w=NULL
b=NULL
for(k in 1:10){ 
  result.k = kmeans(Q, k, nstart = 20)
  w = c(w, result.k$tot.withinss)
  b = c(b, result.k$betweenss)
}

# notare che la variabilità totale è sempre identica !!!
# Quindi, tot.withinss e betweenss hanno somma costante
x11()
par(mfrow=c(1,2))
plot(1:10, b, type='b', xlab='clusters', ylab='BSS', main='Choice of k',col='orange')
plot(1:10, w, type='b', xlab='clusters', ylab='Total WSS', main='Choice of k',col='purple')

# oppure
x11()
par(mfrow=c(1,2))
plot(1:10, b/(w+b),type='b', xlab='clusters', ylab='BSS/TSS', main='Choice of k', ylim=c(0,1),col='orange')
plot(1:10, w/(w+b),type='b', xlab='clusters', ylab='TotWSS/TSS', main='Choice of k', ylim=c(0,1),col='purple')
# che sono esattamente gli stessi plot di prima, ma normalizzati

## Proviamo con K = 2
result.k <- kmeans(Q, centers=2)

x11()
plot(Q, col = result.k$cluster) # scatterplot

plot3d(Q, size=3, col=result.k$cluster, aspect = F)  #3d plot
points3d(result.k$centers,  col = c("blue", "red"), size=10)


## Proviamo con K = 3
result.k <- kmeans(Q, 3, nstart = 20)

x11()
plot(Q, col = result.k$cluster) # scatterplot

plot3d(Q, size=3, col=result.k$cluster, aspect = F) # 3d plot
points3d(result.k$centers,  col = c("blue", "red", "forestgreen"), size=10)


# Vediamo la silhouette nei due casi:
x11()
par(mfrow = c(1,2))
for ( i in 2:3)
{
  output.quakes = kmeans(Q, i, nstart = 20)
  silhouette.quakes = silhouette(output.quakes$cluster , dist(Q) )
  plot(silhouette.quakes, col = 1:i, main=paste("Silhouette per k =",i,sep=" "))
  abline(v = mean(silhouette.quakes[,3]))
}
graphics.off()
