###  KNN su dataset Iris _---------------------------------------------------------------------

# Nella classificazione k-NN, viene stimata la probabilità che un punto nello spazio delle covariate 
# sia associato ad una classe (un’etichetta), in base alla proporzione di elementi delle k osservazioni 
# più vicine che appartengono a quella classe. 
# Di conseguenza, un punto è classificato in base a un voto di pluralità dei suoi vicini. 
# Se k = 1, l’oggetto viene semplicemente assegnato alla classe di quel singolo vicino più prossimo.

library(class)

rm (list = ls())
graphics.off()
cat("\014")

k = 3 #arbitrario
data = iris #classifichiamo sulla base delle caratteristiche del sepalo

x11()
plot(data[,1:2], main='Iris Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
points(data[data$Species == "setosa",1:2], col=2, pch=20)
points(data[data$Species == "versicolor",1:2], col=3, pch=20)
points(data[data$Species == "virginica",1:2], col=4, pch=20)
legend("topleft", legend=levels(data$Species), fill=c(2,3,4))

x  = seq(min(iris[,1]), max(iris[,1]), length=200)
y  = seq(min(iris[,2]), max(iris[,2]), length=200)

# classificazione
iris.knn = knn(train = data[,1:2], test = data[,1:2], cl = data$Species, k = k, prob = T)

?knn
# cosa richiene knn() in input?
# - un training set di dati, contenente solo le covariate.
# - un insieme di nuove osservazioni delle covariate da classificare. 
#   (Potrebbe essere un test set se conosciamo le vere etichette o potremmo semplicemente volerle classificare)
# - la classe (il vettore delle vere etichette osservate nel training set)
# - k, il numero di “vicini” da considerare

iris.knn[1:20] #risultato della classificazione
attributes(iris.knn)$prob[1:20] #proporzione dei vicini appartenenti alla classe predetta (confidenza del voto)
attributes(iris.knn)$prob
attr = attributes(iris.knn)
View(attr)


# La matrice si visualizza come sappiamo
table(pred = iris.knn, true = iris$Species) # esercizio: calcolare l'APER


xy = expand.grid(Sepal.Length=x, Sepal.Width=y)
# se voglio fare la classificazione su altri punti:
iris.knn = knn(train = data[,1:2], test = xy, cl = data$Species, k = k, prob = T)
iris.knn[1:20] #risultato della classificazione
attributes(iris.knn)$prob[1:20] #risultati del "voto"

z  = as.numeric(iris.knn)

iris.knn[1:20]
z[1:20]

#Vediamo la frontiera di classificazione
x11()
plot(data[,1:2], main='Iris Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20, asp = 1)
points(data[data$Species == "setosa",1:2], col="red4", pch=20)
points(data[data$Species == "versicolor",1:2], col="springgreen4", pch=20)
points(data[data$Species == "virginica",1:2], col= "navyblue", pch=20)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T, lwd = 1.5)
colvec = rep(2,nrow(xy))
colvec[iris.knn == "versicolor"] = 3
colvec[iris.knn == "virginica"] = 4
points(xy[,1],xy[,2],col = colvec, pch = '.')
legend(x = 3.5, y = 4.2,               # coordinate precise nel plot
       legend = levels(data$Species),  # nomi classi
       fill = c("red4", "springgreen4", "navyblue"), 
       cex = 0.8,                       # riduce il testo
       pt.cex = 0.6,                    # riduce la dimensione dei simboli
       bty = "o")                       # tipo di box ("o" = linee, "n" = nessun box)


### Cosa accade variando K?
x11()
par(mfrow = c(3,4))
for ( k in 1:12)
{
  plot(data[,1:2], main= paste0("k = ",k), xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
  points(data[data$Species == "setosa",1:2], col=2, pch=20)
  points(data[data$Species == "versicolor",1:2], col=3, pch=20)
  points(data[data$Species == "virginica",1:2], col=4, pch=20)
  iris.knn = knn(train = data[,1:2], test = xy, cl = data$Species, k = k, prob = T)
  z  = as.numeric(iris.knn)
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

x11()
par(mfrow = c(3,4))
for ( k in 1:12)
{ 
  iris.knn = knn(train = data[,1:2], test = xy, cl = data$Species, k = k, prob = T)
  z  = as.numeric(iris.knn)
  plot(data[,1:2], main=paste0('K = ',k), xlab='Sepal.Length', ylab='Sepal.Width', pch=20, asp = 1)
  points(data[data$Species == "setosa",1:2], col="red4", pch=20)
  points(data[data$Species == "versicolor",1:2], col="springgreen4", pch=20)
  points(data[data$Species == "virginica",1:2], col= "navyblue", pch=20)
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T, lwd = 1.5)
  colvec = rep(2,nrow(xy))
  colvec[iris.knn == "versicolor"] = 3
  colvec[iris.knn == "virginica"] = 4
  points(xy[,1],xy[,2],col = colvec, pch = '.')
}

## Potremmo scegliere K con i soliti metodi (test set, cross validazione e così via)

## NOTA: 
# Si noti che all’aumentare di k le frontiere di classificazione tendono a diventare più regolari, (meno frastagliate), 
# poichè l’impatto delle etichette delle singole osservazioni viene progressivamente ridotto.
# Al limite in cui k=n, una sola classe, quella maggioritaria, viene proposta per ogni punto da classificare

# riassumendo
# Se k si riduce il bias diminuisce ma la varianza aumenta: k troppo piccoli producono seri rischi di overfitting
# Se k aumenta il bias aumenta e la varianza diminuisce. k troppo grandi tipicamente producono classificatori di modeste prestazioni.
