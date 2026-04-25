rm(list = ls())
graphics.off()
cat("\014")

#### ESEMPIO: Cluster Ellissoidali -----------------------------------------------
dati = read.csv("Esempio_ellissoidali.txt")
str(dati)
metric = dist(dati)
x11()
plot(dati)

##### Vediamo come si comporta il kmeans -----------------------------------------
km_2 = kmeans(dati, centers = 2, nstart = 20)
plot(dati, col = km_2$cluster)
# grafico mostra una patologia tipica del k-means 
# in presenza di cluster ellissoidali vicini e con eccentricità pronunciata

silhouette_km2 = silhouette(km_2$cluster , metric )
x11()
plot(silhouette_km2, col = 1:2, main="Silhouette plot per k=2")
abline(v = mean(silhouette_km2[,3]))
# in casi come questo, ma con più di 2 dimensioni, 
# visualizzare graficamente risulta complesso.
# Il grafico della silhouette può essere di aiuto, nei casi in cui mostri un
# buon numero di punti con silhouette bassa o negativa

##### Vediamo come si comporta il clustering gerarchico ------------------------
#Linkage completo
hc.complete = hclust(metric, method="complete")
# Linkage average
hc.average = hclust(metric, method="average")
# Single Linkage
hc.single = hclust(metric, method="single")

# Riportiamo i dendrogrammi
x11()
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9, label = F)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9, label = F)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9, label = F)

# k=2 clusters:
clusters_complete = cutree(hc.complete, 2)
clusters_average = cutree(hc.average, 2)
clusters_single = cutree(hc.single, 2)
x11()
par(mfrow = c(1,3))
plot(dati, col = clusters_complete, main="Complete")
plot(dati, col = clusters_average, main="Average")
plot(dati, col = clusters_single, main="Single")

# I cluster ellissoidali tendono ad essere meglio identificati dal single linkage.

