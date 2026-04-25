##### Esercizio su Ridge and Lasso Regression ####

##### Cars Dataset #####
library(glmnet)
library(ggfortify)
library(ISLR)
library(pls)
library(car)
data = Auto

# Il dataset contiene i consumi in Miglia per Gallone (mpg) in funzione delle caratteristiche dell'auto 
head(data)
dim(data)
names(data)

# Scatterplot of data
x11()
pairs(data, pch=19)

## 1) fittare un modello lineare con metodo OLS che abbia come variabile risposta 'mpg' e come predittori tutte le altre 
## variabili tranne 'name'. Valutate la presenza di collinearita' tra i predittori.

# Model: mpg ~ cars' 
x <- model.matrix(mpg~.-name, data)[,-1] # predictor matrix
y <- data$mpg # response

# Least squares
fit.LS <- lm(mpg~.-name,data)
summary(fit.LS)

vif(fit.LS)

## 2) Costruite un modello con la stessa variabile target e gli stessi predittori, applicando la
##    penalizzazione di Ridge. Scegliete il parametro lambda adeguato tramite cross-validazione.

# Costruisco il vettore dei candidati lambda
lambda.grid <- 10^seq(5,-3,length=100)

# Ridge Regression
ridge.mod <- glmnet(x,y,alpha=0,lambda=lambda.grid)

x11()
plot(ridge.mod,xvar='lambda',label=TRUE,col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

# Select the best lambda via CV
set.seed(1)
cv.out <- cv.glmnet(x,y,alpha=0,nfolds=3,lambda=lambda.grid) 
x11()
plot(cv.out)
# Il CV-MSE della Ridge regression è sempre più basso che quella della OLS

bestlam <- cv.out$lambda.min
bestlam
log(bestlam)
# Ricorda che lambda.min è il lambda che corrisponde al valore minimo di CV-MSE


coef.ridge <- predict(ridge.mod, s=bestlam, type='coefficients')
coef.ridge

x11()
plot(ridge.mod,xvar='lambda',label=TRUE,col =  rainbow(dim(x)[2]))
abline(v=log(bestlam),col='orange')
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

## 3) Ripetete la stessa procedura questa volta applicando la penalizzazione Lasso.
##    Quali predittori rimangono nel modello?

lasso.mod <- glmnet(x,y,alpha=1,lambda=lambda.grid)

x11()
plot(lasso.mod,xvar='lambda',label=TRUE,col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

# Selezioniamo il lambda migliore con una CV
cv.out <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda=lambda.grid) 
x11()
plot(cv.out)

# In questo caso, prendiamo come lambda migliore quello a una 1 s.e. dal lambda.min
bestlam2 <- cv.out$lambda.1se
bestlam2
log(bestlam2)

# now we can inspect the resulting model
coef.lasso <- predict(lasso.mod, s=bestlam2, type='coefficients')
coef.lasso

## 4) Comparate i coefficienti stimati da OLS, Ridge e Lasso
x11()
plot(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20, xlim=c(-1,3), xlab='', ylab=expression(beta),
     axes=F)
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)
abline(h=0, col='grey41', lty=1)
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)
box()
axis(2)
axis(1, at=c(0,1,2), labels = c('OLS', 'Ridge', 'Lasso'))

graphics.off()

