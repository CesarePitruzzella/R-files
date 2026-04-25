rm(list = ls())
graphics.off()
cat("\014")

#################### Clustering gerarchico #######################
#### ESEMPIO 1: localizzazione alberi di pioppo ------------------------------------
pioppi = read.csv("Pioppi.txt")
str(pioppi)
x11()
plot(pioppi,pch=19,xlab='X',ylab='Y')

# Clustering gerarchico, distanza euclidea
#' #### Il comando "dist" #####
#'  Il comando dist calcola una matrice metrica, in forma vettoriale per evitare
#'  ridondanze, seguendo una distanza specificata dall'utente.
#'  Il default è la distanza euclidea
metric = dist(pioppi, method="euclidean") 
# sarebbe come fare metric = dist(pioppi, method="euclidean")
?dist
help(dist)

head(metric)
# R non rappresenta la matrice metrica in forma matriciale, 
# per evitare di ripetere elementi che sono uguali per definizione, 
# ma questo non è un problema, poichè non utilizzeremo direttamente tale oggetto, 
# passandolo invece agli algoritmi di clustering gerarchico.

# Se però voglio visualizzare la matrice 
# (non è strettamente necessario)
metric_matrix = as.matrix(metric)
metric_matrix[1:7,1:7]

##### Il comando "hclust" ####
# Clustering gerarchico a partire da una matrice metrica: comando hclust
# Occorre anche passare il metodo di linkage desiderato: 
?hclust
# - Complete Linkage: distanza tra cluster è distanza tra i punti più lontani
hc.complete = hclust(metric, method="complete")

# - Average Linkage: distanza tra cluster è distanza tra i centroidi
hc.average = hclust(metric, method="average")

# - Single Linkage: distanza tra cluster è distanza tra i punti più vicini
hc.single = hclust(metric, method="single")

#?hclust

# Riportiamo i dendrogrammi
x11()
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# in questo caso, tutti i tipi di linkage offrono quasi la stessa prospettiva,
# con ampie zone di stabilità del dendrogramma corrispondenti a una suddivisione in
# 4 cluster. Ovviamente, questo non sempre è vero.

##### Il comando "cutree" ####
# Taglio del dendrogramma, comando cutree

# È possibile ottenere una clusterizzazione “tagliando” il dendrogramma 
# ad una certa altezza (corrispondente ad una granularità, 
# una risoluzione con cui si vogliono aggregare i dati) o, più semplicemente, 
# tagliandolo in modo da ottenere un determinato numero di cluster.

# INPUT:
# - tree: tree type, output di hclust()
# - k: numero di clusters
# - h: Altezza di taglio dei clusters 
# (si fornisce h oppure k)
# OUTPUT:
# vettore coi cluster

# Provo con K = 4 clusters:
clusters_complete = cutree(hc.complete, 4)
clusters_average = cutree(hc.average, 4)
clusters_single = cutree(hc.single, 4)

clusters_complete

x11()
par(mfrow = c(1,3))
plot(pioppi, col = clusters_complete)
plot(pioppi, col = clusters_average)
plot(pioppi, col = clusters_single)
# suddivisione identica!

# Cosa succede con K = 2?
clusters_complete = cutree(hc.complete, 2)
clusters_average = cutree(hc.average, 2)
clusters_single = cutree(hc.single, 2)
x11()
par(mfrow = c(1,3))
plot(pioppi, col = clusters_complete)
plot(pioppi, col = clusters_average)
plot(pioppi, col = clusters_single)
# il single linkage produce un risultato diverso!

# Altre distanze (oltre all'euclidea): Manhattan distance
Manhattan_metric = dist(pioppi, method = 'manhattan')
hc.manh.complete = hclust(Manhattan_metric, method = "complete") # provo con complete linkage
x11()
par(mfrow=c(1,2))
plot(hc.complete,main="Complete Linkage with Euclidean Distance", xlab="", sub="")
plot(hc.manh.complete, main="Complete Linkage with Manhattan Distance", xlab="", sub="")

# I risultati sono molto simili, ma in generale la scelta della metrica 
# può influire in maniera molto significativa sui risultati.

graphics.off()

#### ESEMPIO 2: Chaining effect in dati simulati ------------------------------------
# Esempio in cui emergono le problematiche di alcuni tipi di linkage

### Carichiamo i file di esempio
Esempio = read.csv("Esempio_chaining_effect.txt")
x11()
plot(Esempio)

# Confronto con distanza euclidea, tre tipi di linkage:
metric = dist(Esempio, method = 'euclidean')
x.single = hclust(metric, method='single')
x.average = hclust(metric, method='average')
x.complete = hclust(metric, method='complete')
x11()
par(mfrow=c(1,3))
plot(x.single, main = 'Single linkage'  , hang=-0.1, xlab='', labels=F, sub='')
plot(x.average, main = 'Average linkage' , hang=-0.1, xlab='', labels=F, sub='')
plot(x.complete, main = 'Complete linkage', hang=-0.1, xlab='', labels=F, sub='')
# - average e complete linkage presentano ampie aree di stabilità in corrispondenza di 2 cluster, 
# - single linkage non fornisce alcuna indicazione significativa
dev.off()

#Tagliamo a 2 clusters
cluster.single = cutree(x.single, k = 2)
cluster.average = cutree(x.average, k = 2)
cluster.complete = cutree(x.complete, k = 2)

x11()
par(mfrow=c(1,3))
plot(Esempio, xlab='Var 1', ylab='Var 2', main = 'Single linkage', col=ifelse(cluster.single==1,'red','blue'), pch=16, asp=1)
plot(Esempio, xlab='Var 1', ylab='Var 2', main = 'Average linkage', col=ifelse(cluster.average==1,'red','blue'), pch=16, asp=1)
plot(Esempio, xlab='Var 1', ylab='Var 2', main = 'Complete linkage', col=ifelse(cluster.complete==1,'red','blue'), pch=16, asp=1)

graphics.off()

# Complete e Single Linkage sono metodi locali, che riducono 
# la distanza tra gruppi ad una distanza tra due osservazioni 
# (le più vicine in Single e le più lontane in Complete). 

# Questo può causare cluster indesiderati. 

# Poichè il criterio di fusione è strettamente locale, 
# una "catena di punti" può essere estesa per lunghe distanze 
# senza tener conto della forma complessiva del cluster emergente. 
# Questo effetto è chiamato chaining.


