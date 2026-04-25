##### Homeworks ####

### ESERCIZIO 1 ###
# Venerdì 17 ottobre 2008, nei cieli della Black Fortune si è verificato uno scontro tra
# due satelliti artificiali. Un centinaio di detriti sono stati trovati sulla
# terra (file satellite.txt). A causa dell'attrito con l'atmosfera, non è stato possibile
# definire l'origine di nessuno dei detriti. Per chiarire l' 
# incidente, l'US Air Force chiede di stimare la posizione relativa dei 
# due punti di impatto con il suolo.
# a) Scegliere il K appropriato per l'analisi kmeans
# b) Eseguire il kmeans corrispondente, riportando la numerosità dei due cluster e i centri.

dati = read.table(file='satellite.txt', header=T)

head(dati)
plot(dati)  ## intravedo 2 clusters, forse 3
cov(dati)  ## stessa variabilità

# a) selezione del best k
b = w = NULL
for(k in 1:10){ # loop on k
  result.k = kmeans(dati, k)
  w = c(w, result.k$tot.withinss)  
  b = c(b, result.k$betweenss)
}

x11()
par(mfrow=c(1,2))
plot(1:10, b/(w+b),type='b', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1),col='orange')
plot(1:10, w/(w+b),type='b', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1),col='purple')


# scelgo k= 2

# b) kmeans con k=2 

km.dati = kmeans(dati, centers=2, nstart=20)
km.dati

# I centri sono:
# Cluster 1: 70.43939 lat e 60.77273 long; size = 66
# Cluster 2: 51.31818 lat e 40.22727 long; size = 44

## plot dei clusters

## sono 2 quindi definisco 2 colori
plot(dati, col=km.dati$cluster+1)  # cambiando n in km.dati$cluster+n avete diversi colori possibili


rm(list=ls())
graphics.off()



### ESERCIZIO 2 ###
# Il Diagramma Veritatis è un'opera misteriosa attribuita a Galileo. Alcuni
# semiologi credono che alcune pagine del libro nascondano un messaggio in codice;
# credono anche che queste pagine siano caratterizzate da un'anomala
# numerosità di alcune lettere dell'alfabeto. Il file veritatis.txt
# elenca, per 132 pagine del libro, le frequenze assolute delle cinque
# vocali dell'alfabeto latino.
# Utilizzando un algoritmo di clustering agglomerativo (average linkage), 
# identifica due cluster e segnala le pagine sospette; 
# Riportare la numerosità e le medie dei cluster.

dati = read.table(file='veritatis.txt', header=T)
head(dati)
library(GGally)
ggpairs(dati)

# Distanza Euclidea
dx <- dist(dati)

# Linkage average
hcx<- hclust(dx, method='average')
plot(hcx, labels=F, cex=0.5, hang=-0.1, xlab='', sub='x')
rect.hclust(hcx, k=2) # comando per evidenziare i gruppi nel plot

cl2 = cutree(hcx,k=2)
table(cl2)
# le numerosità dei gruppi sono 124 e 8

# come calcoliamo le medie dei gruppi?
colMeans(dati[cl2==1,])  # medie nel gruppo 1
colMeans(dati[cl2==2,])  # medie nel gruppo 2

## plot dei due clusters identificati
ggpairs(dati, aes(col=as.factor(cl2)))

rm(list=ls())
graphics.off()



### ESERCIZIO 3 ####
# Il dataset Sparrows.txt riporta Lunghezza (cm) Larghezza (cm) del petto di
# 50 passeri. Il biologo che ha raccolto le misure mira a 
# dimostrare che i passeri possono essere divisi in due gruppi distinti
# in termini di lunghezza e larghezza del petto. Aiutalo a dimostrare la sua teoria:
# con un algoritmo di clustering gerarchico agglomerativo con single linkage
# valuta se è ragionevole raggruppare i dati in  due gruppi. 
# Riportare la numerosità e le medie dei cluster.

dati=read.table(file='Sparrows.txt', header=T)
head(dati)
ggpairs(dati)  # ad occhio si vedono 2 gruppi ben distinti

# Distanza Euclidea
dx <- dist(dati)

# Linkage single
hcx<- hclust(dx, method='single')
plot(hcx, labels=F, cex=0.5, hang=-0.1, xlab='', sub='x')
rect.hclust(hcx, k=2)

cl2 = cutree(hcx,k=2)
table(cl2)
# le numerosità dei gruppi sono 20 e 30

# come calcoliamo le medie dei gruppi?
colMeans(dati[cl2==1,])  # medie nel gruppo 1
colMeans(dati[cl2==2,])  # medie nel gruppo 2

## plot dei due clusters identificati
ggpairs(dati, aes(col=as.factor(cl2)))  # perfetti

