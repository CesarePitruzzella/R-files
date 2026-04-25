#' ###########################################################################
#' ##################### Collinearita', PCA Regression, ######################
#' ############## Introduzione allo shrinkage con Ridge e Lasso ##############
#' ############ e confronto con i minimi quadrati ordinari (OLS) #############
#' ###########################################################################

rm(list = ls())
graphics.off()
cat("\014")
library(glmnet)
library(ggfortify)
library(ISLR)
library(pls)
library(car)
library(boot)

### Introduzione al problema della collinearità --------------------------------------
# Problemi legati alla collinearità:
# - I coefficienti di regressione possono assumere valori molto grandi e cambiare drasticamente con piccole variazioni nei dati.
#   La presenza di collinearità porta a coefficienti con elevata varianza, cioè soggetti a grandi fluttuazioni.
#   Questo si traduce in intervalli di confidenza molto larghi, rendendo difficile stabilire se una variabile è significativa.
# - Se due variabili sono collineari, non è chiaro quale abbia effettivamente un impatto sulla variabile di risposta.
# - Potenziamento dell'Overfitting
# - I test statistici (come il t-test per verificare se un coefficiente è significativamente diverso da zero) possono essere distorti.
#   Anche se una variabile è realmente importante, il suo p-value potrebbe essere alto a causa della collinearità.

#' Analizziamo un dataset reale, relativo al salario di giocatori di baseball
#' https://rdrr.io/cran/ISLR/man/Hitters.html
data = na.omit(Hitters)
str(data)
#' 20 variabili numeriche, di cui 3 variabili categoriche. Costruiamo un modello lineare
#' per Salary, considerando l'effetto di tutte le variabili (le categoriche solo sull'intercetta)

model_ols = lm(Salary ~. , data = data)
summary(model_ols)
#' Sembrerebbe ci sia un buon numero di predittori non significativi.
#' Abbiamo un problema di scelta dei regressori, tipico nei casi di modelli di regressione 
#' con più di un paio di predittori. Come già ampiamente anticipato, si tratta di un problema 
#' abbastanza delicato: idealmente, vorremmo includere nel modello solo quei predittori 
#' che spiegano una parte significativa della varianza della risposta, tuttavia non possiamo 
#' fare alcuna selezione basandoci esclusivamente sui p-value. 
#' Questo problema è legato alla collinearità: valutiamo il VIF

## Variance Inflation Factor : valutare la collinearità
vif(model_ols)
# alcune variabili hanno valori di VIF altissimi (>5/10)! CAtBat, CHits, CHmRun, CRuns, CRBI

round( cor(data[,c(1:13,16:18)]), 2 )
#' calcolo la tabella di correlazione per le numeriche (approssimando alla seconda cifra decimale).
#' Ritrovo correlazioni molto alte in corrispondenza delle variabili con VIF alto.
#' C'e' un evidente problema di collinearita'.

#' Cosa succede se tolgo predittori non significativi? Togliamo CHmRun, che fra l'altro è una
#' delle covariate con VIF molto alto
model_ols_1 = lm(Salary ~. - CHmRun  , data = data )
summary(model_ols_1) 
#' CRBI è diventato significativo!(p-value del t-test passato da 0.245 a 0.003)
#' Ecco perche' non si possono togliere piu' predittori alla volta 
#' basandosi solo sui p-values! E' uno degli effetti della collinearita'.

# Cos'è il VIF?
model_v = lm(AtBat ~. -Salary, data = data )
summary(model_v)
vif_AtBat <- 1 / (1 - summary(model_v)$r.squared)

#' Per risolvere il problema della multicollinearita' ci sono diverse strade 
#' che possono essere percorse:
#' 
#' 1) Aggiunta di nuove osservazioni, nella speranza che ciò produca una riduzione 
#'    dei Variance Inflation Factors. Generalmente si tratta di una soluzione inapplicabile;
#'    
#' 2) Esclusione dal modello, una per volta, delle variabili correlate ovvero di quelle per le quali 
#'    la stima del VIF è elevata (scelta arbitraria);
#'    
#' 3) Uso della Principal Component Regression (PCR): si estraggono le componenti principali 
#'    dai regressori originali (queste nuove variabili sono per definizione tra loro ortogonali) 
#'    e si fa regredire la variabile risposta su queste;
#'    
#' 4) Uso di Ridge o Lasso regression.


### PCA Regression -------------------------------------------------------------------
#' Proviamo a seguire la strada della PCA regression. 
#' Possiamo fare PCA solo sulle variabili numeriche, quindi per semplicita' in
#' questo esempio escluderemo le categoriche. E' comunque possibile includerle 
#' nell'analisi, con un po' di sforzo, ma questo e' al momento oltre il nostro scopo.

# Dividiamo numeriche e categoriche:
data_cat = data[,c(14,15,20)]
data_num = data[,-c(14,15,20)]

#' Controlliamo di nuovo che tipo di modello risulterebbe includendo tutte le 
#' variabili numeriche
model_ols_num = lm(Salary ~. , data = data_num)
summary(model_ols_num)
vif(model_ols_num)
#' Il problema di collinearità tra i predittori, ovviamente, non si è risolto semplicemente
#' eliminando i predittori categorici dal modello

# Verifichiamo intanto se serva standardizzare le variabili prima di eseguire la PCA
x11()
boxplot(data_num[,-17]) # sembra proprio di si
## [ , -17] perchè sto togliendo Salary

regressori = scale(data_num[,-17])
cov(regressori)

# Facciamo la PCA
pca_baseball = princomp(regressori, scores=T)
summary(pca_baseball)

# NOTA: se la collinearità è forte, le ultime componenti principali avranno variabilità bassa.

x11()
plot(cumsum(pca_baseball$sd^2)/sum(pca_baseball$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(regressori),labels=1:ncol(regressori),las=2)

# osserviamo i loadings delle prime tre componenti principali 
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(pca_baseball$loadings[,i], ylim = c(-1, 1))
colnames(data_num)
# Visualizziamo i boxplot della variabilità:
x11()
par(mfrow=c(2,1))
boxplot(scale(data_num[,-17]))
boxplot(pca_baseball$scores) 

## Fermiamoci alle prime cinque componenti (Scelta arbitraria al momento... )
k = 5
data_for_regression = data.frame(cbind(Salary = data_num$Salary,pca_baseball$scores[,1:k]))
# fittiamo il nostro modello lineare con le prime 5 componenti
pca_model = lm(Salary ~., data = data_for_regression)
summary(pca_model)
vif(pca_model) # siamo sorpresi?
# VIF pari a 1 rende minima la varianza dei beta; 
# questo è il caso in cui tutti i predittori sono scorrelati

# Quali sono i coefficienti rispetto alle variabili originali? 
# Prendiamo le prime K=5 componenti 
load = pca_baseball$loadings[,1:k]
load

beta_original = load %*% c(coef(pca_model)[2:(k + 1)]) #partiamo da 2 perchè la prima e' l'intercetta!
beta_original
beta_original_AtBat <- sum(load[1,] %*% c(coef(pca_model)[2:(k + 1)]))
#' Stiamo estraendo i loadings delle prime k=5 componenti della PCA
#' (una matrice che avrà tante righe quante sono le osservazioni k=5 colonne) 
#' e la stiamo moltiplicando matricialmente (in R, il comando %∗% indica il prodotto matriciale) 
#' per il vettore dei coefficienti [β1,β2,β3,β4,β5] del nostro modello. 


#' Per ora abbiamo scelto K=5, ma appunto si trattava di una scelta arbitraria.
#' Come scegliere il numero di componenti? O fissando una soglia di varianza spiegata,
#' o con i metodi di selezioni step-wise visti nello scorso lab, o, se ci interessa
#' ottenere un modello che minimizzi il MSE, possiamo crossvalidare!

#' Automatizzazione della cross-validazione per Principal Component Regression con
#' il comando pcr (pacchetto pls), che ci consente di fittare il modello e crossvalidare
#' automaticamente
pcr_model = pcr(Salary ~., data = data_num, scale = T, validation = "CV")
summary(pcr_model)

#' Abbiamo i risultati di stima dei coefficienti, rispetto alle variabili originali,
#' per un qualunque numero di componenti.
#' Se ne prendiamo 5, riotteniamo i coefficienti che abbiamo stimato a mano
pcr_model$coefficients[,1,5]
beta_original

pcr_model$coefficients[,1,4]
#' Risultati identici a quello che abbiamo ottenuto a mano, ma adesso abbiamo a disposizione
#' molte piu' informazioni (e.g., possiamo analizzare qualunque modello dei 16 disponibili.)

#' Osserviamo cosa cambia al variare del numero di componenti inclusi nel modello
x11()
validationplot(pcr_model,val.type = "RMSEP",estimate = "CV", main = "root mean square error of prediction, CV") # RMSEP = Radice quadrata dell'MSE
axis(side=1, at=0:16,labels = 0:16,cex.axis=0.7) 

## Notare la differenza con le stime sul training:
x11()
validationplot(pcr_model,val.type = "RMSEP",estimate = "train", main = "root mean square error of prediction, training set")
axis(side=1, at=0:16, labels = 0:16,cex.axis=0.7) ### Potete visualizzare praticamente qualunque tipo di informazione

## Possiamo visualizzare tutto insieme
x11()
validationplot(pcr_model,val.type = "RMSEP",estimate = "all", legendpos = "topright")
axis(side=1, at=0:16, labels = 0:16,cex.axis=0.7) 

## direi che ci siamo fatti un'idea di quali componenti scegliere. 
## Possiamo fare predizione, scegliendo al momento quale modello usare
predict(pcr_model, newdata = data[3:6,], ncomp = 4) 
## Ho usato dati osservati per comodita'. Faccio le predizioni con 4 componenti


### Ridge e Lasso ---------------------------------------------------------------
#' Adesso cambiamo strada e usiamo Ridge e Lasso regression (pacchetto glmnet)

#### Esempio 1 -----------------------------------------------------------------
#### Torniamo al modello con tutte le variabili.
x = model.matrix(Salary~. , data = data)[,-1]  ## prendo tutte le covariate (tolgo intercetta)

## Definisco una griglia di lambda per cui voglio fittare il modello (lungo 101 e DECRESCENTE)
lambdas = c(10^seq(10,-2,length.out = 100) ,0) # a meno di approssimazioni numeriche, 
                                               # per lambda = 0 ritrovo i minimi quadrati


## Il pacchetto piu' consolidato per ridge e lasso e' glmnet. 
## Si noti che in automatico standardizza le covariate in input (standardize=TRUE di default, si veda ?glmnet)
help(glmnet)
model_ridge = glmnet(x,data$Salary,lambda = lambdas,alpha = 0, thresh = 1e-10)
#alpha = 0 e' NECESSARIO perche' venga fatta ridge regression

## coefficienti per ogni lambda
coef(model_ridge)   ## num regressori x 101

## coefficienti per il cinquantesimo lambda
coef(model_ridge)[,50]

##coefficienti per il centesimo lambda
coef(model_ridge)[,100] ## nota: i lambda sono decrescenti, quindi il centesimo sara' un modello 
                        ## con una regolarizzazione meno forte del 50-esimo.
                        ## Infatti i parametri stimati hanno valori in modulo più elevati per il centesimo lambda.

## coefficienti per lambda = 0 (il 101-esimo modello, con nessuna regolarizzazione)
coef(model_ridge)[,101]
summary(model_ols) #### Praticamente tutto identico (o quasi...)

### Visualizzazione del fitting per ogni lambda
x11()
autoplot(model_ridge,label = T, xvar = "lambda")

# alternativa per chi non riesce a installare il pacchetto ggfortify
x11()
plot(model_ridge,xvar='lambda',label=TRUE,col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

# ricordiamo ciò che abbiamo visto a lezione: 
# più velocemente vado a zero, meno le covariate sono "significative"/"importanti".
# Tuttavia nella Ridge regression i coefficienti non andranno mai a zero, per costruzione.
# Notiamo anche che per lambda = 0 abbiamo al stima OLS.

x11()
autoplot(model_ridge, label = T, xvar = "dev") ## pseudo R^2 sull'ascissa


# alternativa per chi non riesce a installare il pacchetto ggfortify
x11()
plot(model_ridge,xvar='dev',label=TRUE,col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

#### come scegliere lambda? cross validazione!

cv.model_ridge = cv.glmnet(x,data$Salary,lambda = lambdas,alpha = 0,nfolds = 10)
# si noti che il default di nfolds = 10 (valore minimo permesso =3).
# Si richiama che nello scorso laboratorio avevamo visto cv.glm, dove il default era K=n
# (quindi leave-one-out, altamente sconsigliato per dataset grandi).

x11()
autoplot(cv.model_ridge)

bestlam = cv.model_ridge$lambda.min    ## prima linea tratteggiata 
bestlam                                ## (lambda in corrispondenza del minor MSE in CV)

reglam = cv.model_ridge$lambda.1se    ## seconda linea tratteggiata (il piu' grande lambda, 
reglam                                ## cioe' il modello piu' regolarizzato, 
                                      ## che abbia MSE CV entro una dev standard dal minimo MSE)


# selezioniamo il modello corrispondente a quel lambda
indbest = which(cv.model_ridge$lambda == bestlam)
indreg = which(cv.model_ridge$lambda == reglam)
c(indbest, indreg)

# Stima dei coefficienti
coef(model_ridge)[,c(indbest,indreg)]
predict(model_ridge, s=c(bestlam, reglam), type='coefficient')

# Stima degli errori
cv.model_ridge$cvm[c(indbest,indreg)]

cv.model_ridge$cvm[101] ### praticamente uguale al seguente
cv.glm(data, glm(Salary~., data = data), K = 10)$delta[1]

## Cosa scegliamo tra il migliore e il piu' regolarizzato? 
## Il regolarizzato ha varianze delle stime minori, pagando in MSE

## In questo caso ridge produce un piccolo miglioramento rispetto ai minimi quadrati ordinari
## Tuttavia, non abbiamo fatto nessun tipo di selezione di variabili (tutti i coefficienti sono ancora non nulli). 
## Usiamo lasso (l'output di lm ci suggerisce che un po' di selezione sarebbe opportuna)

model_lasso = glmnet(x,data$Salary,lambda = lambdas, alpha = 1, thresh = 1e-10) 
#alpha = 1 e' NECESSARIO perche' venga fatta lasso regression
#ricordiamo che i lambdas sono decrescenti
length(lambdas)
## coefficienti per ogni lambda
coef(model_lasso)

## coefficienti per il cinquantesimo lambda
coef(model_lasso)[,50]

##coefficienti per il centesimo lambda
coef(model_lasso)[,100]                      ## nota: i lambda sono decrescenti

## coefficienti per lambda = 0
coef(model_lasso)[,101]
summary(model_ols) #### Praticamente tutto identico

x11()
autoplot(model_lasso,xvar = "lambda",xlim = c(-6,6), size = 0.5)
autoplot(model_lasso,xvar = "dev")

graphics.off()

#### come scegliere lambda? cross validazione!
cv.model_lasso = cv.glmnet(x,data$Salary,lambda = lambdas,alpha = 1,nfolds = 10,thresh = 1e-10)

x11()
autoplot(cv.model_lasso)

bestlam = cv.model_lasso$lambda.min
bestlam

reglam = cv.model_lasso$lambda.1se
reglam

#Selezioniamo i modelli corrispondenti a questi lambda
indbest = which(cv.model_lasso$lambda == bestlam)
indreg = which(cv.model_lasso$lambda == reglam)
c(indbest, indreg)

# Stima dei coefficienti

coef(model_lasso)[,c(indbest,indreg)] # Notiamo che questa volta ci sono alcuni coefficienti nulli!
                                      # Ammesso dalla formulazione matematica della lasso regression

summary(model_ols)
cv.model_lasso$cvm[c(indbest,indreg,101)] ##confronto MSE migliore, regolarizzato, originale

### In generale, se il numero di regressori e' davvero alto, 
### lasso diventa un strada preferibile alla stepwise selection

predict(model_lasso, s = bestlam, newx = x[1:5, ])



#### Esempio 2: Swiss dataset --------------------------------------------------

#' Misura standardizzata della fertilità e indicatori socioeconomici
#' per ciascuna delle 47 province francofone della Svizzera intorno al 1888.

#[,1]	Fertility	        Ig, 'common standardized fertility measure'
#[,2]	Agriculture	      % of males involved in agriculture as occupation
#[,3]	Examination	      % draftees receiving highest mark on army examination
#[,4]	Education 	      % education beyond primary school for draftees.
#[,5]	Catholic	        % 'catholic' (as opposed to 'protestant').
#[,6]	Infant.Mortality  live births who live less than 1 year.All variables but 'Fertility' give proportions of the population.

# Tutte le variabili tranne 'Fertility' sono riferite a percentuali della popolazione.

data("swiss")

help(swiss)
names(swiss)
dim(swiss)
summary(swiss)


#### Ridge Regression ____________________________________________________________

# costruiamo la matrice dei predittori (escludendo l'intercetta, tutte le colonne tranne la prima)
x = model.matrix(Fertility~. , data=swiss)[,-1]
# vettore risposta (variabile target)
y = swiss$Fertility

#ridge.mod_1 <- glmnet(x,y,alpha=0) # (alpha = 0 per Ridge)
#summary(ridge.mod_1)

# settiamo una sequenza di lambda
lambda.grid <- 10^seq(5,-1,length=100)
lambda.grid

ridge.mod <- glmnet(x,y,alpha=0,lambda=lambda.grid)

x11()
autoplot(ridge.mod,xvar='lambda')

# Possiamo estrarre i coefficienti del modello corrispondenti al 50-esimo lambda
coef(ridge.mod)[,50]

#' Possiamo usare il comando 'predict' per trovare i coefficienti stimati per un nuovo
#' valore di lambda, che non era stato inserito nella griglia iniziale. Il nuovo valore
#' di lambda va specificato nel parametro 's' del comando 'predict'.


predict(ridge.mod,s=100,type="coefficients")

## Ora cross validiamo per la scelta del miglior lambda
## Cross-validation

set.seed(02041991)

cv.out <- cv.glmnet(x,y,alpha=0)

# Risultati che restituisce cv.glmnet
names(cv.out)
cv.out$lambda # i lambda utilizzati per fittare il modello
cv.out$cvm    # errore medio di cross-validazione per ogni lambda (MECV)
cv.out$lambda.min # valore di lambda che restituisce il minimo MECV
cv.out$lambda.1se # il più grande valore di lambda tale per cui il MECV è a 1 deviazione standard dal minimo

x11()
plot(cv.out)

# Il MSE aumenta con l'aumentare di lambda

# Salviamo il lambda che minimizza l'errore di cross-validazione
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

# Proviamo con un intervallo più grande di lambda
lambda.grid= 10^seq(10,-3,length=100)
cv.out <- cv.glmnet(x,y,alpha=0,lambda=lambda.grid)
x11()
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

abline(v=log(bestlam), lty=1,col='orange')

# Dopo aver selezionato il lambda ottimo con la cross-validazione, possiamo
# rifittare il modello ridge di regressione
coef.ridge <- predict(cv.out,type="coefficients",s=bestlam)
coef.ridge


#### Lasso Regression _____________________________________________________________

# default: alpha=1 setta la Lasso come metodo di regolarizzazione
fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha = 1)
names(fit.lasso)

x11()
autoplot(fit.lasso,xvar='lambda',label=TRUE)
#legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

# Selezioniamo il lambda ottimo con CV
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid, alpha = 1)

bestlam2 <- cv.lasso$lambda.min
bestlam2
log(bestlam2)

x11()
plot(cv.lasso)
abline(v=log(bestlam2), lty=1,col='orange')

# Estraiamo i coefficienti corrispondenti al lambda ottimo

# iniziamo con lambda.min (bestlam2)
coef.lasso <- predict(fit.lasso, s=bestlam2, type = 'coefficients')
coef.lasso 
#' Notiamo che non c'è nessun coefficiente che è messo a 0.
#' 
#' Ma se invece che il lambda.min selezioniamo il lambda.1se (bestlam3):
bestlam3 <- cv.lasso$lambda.1se
bestlam3
log(bestlam3)
coef.lasso <- predict(fit.lasso, s=bestlam3, type = 'coefficients')
coef.lasso 
# Agricoltura va a zero

# Confronto con le stime OLS dei coefficienti con i coefficienti stimati da Ridge e Lasso
x11()
plot(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20, xlim=c(-1,3), xlab='', ylab=expression(beta),
     axes=F)
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)
abline(h=0, col='grey41', lty=1)
box()
axis(2)
axis(1, at=c(0,1,2), labels = c('OLS', 'Ridge', 'Lasso'))

graphics.off()
