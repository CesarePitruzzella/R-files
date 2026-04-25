#' ##############################################################################
#' #################### REGRESSIONE LINEARE SEMPLICE E MULTIPLA #################
#' ################################## Concetti base #############################
#' ##############################################################################

rm(list = ls())
graphics.off()
cat("\014")

library(GGally)
library(MASS)
library(car)
library(rgl)

## dati di reddito da Introduction to Statistical Learning
data = read.csv(file = "Income2.csv", header = T)
str(data)
data$X = NULL
str(data)
x11()
ggpairs(data)

### REGRESSIONE LINEARE SEMPLICE -----------------------------------------------
## Impostiamo un modello di regressione semplice tra livello di istruzione e reddito
## y_i = beta0 + beta1*x_i + eps_i  ,  eps_i iid N(0,sigma^2)

# y_i: variabile dipendente (reddito dell’individuo i)
# beta0, beta1: parametri del modello (intercetta e effetto dell’istruzione sul reddito)
# x_i: variabile esplicativa (livello di istruzione) 
# eps_i sono i residui (errori): rappresentano la parte di y non spiegata da x

#' RICHIAMO TEORICO:
#' Vedremo subito come ricavare automaticamente il modello di regressione con il
#' comando lm. Ricordiamoci però che tutto potrebbe essere fatto a mano, seguendo passo
#' passo le formule che avete visto durante le lezioni teoriche.
#' Se voleste ricavare a mano le stime dei coefficienti beta0 e beta1 della regressione semplice,
#' i comandi da eseguire sarebbero:

## x = data$Education
## y = data$Income
## n = length(x)
## p = 1
## beta1 =  cor(x,y) * sd(y)/sd(x)
## beta0 = mean(y) - beta1*mean(x)

#' COMPITO PER CASA: riguardare gli appunti sulla stima di beta0 e beta1 e controllare
#' che tutto torna con questa implementazione!

#' Tutto questo ha solo fini didattici. 
#' Nella pratica possiamo ottenere tutto con un singolo comando:
first_model = lm(Income ~ Education, data = data)
first_model$coefficients
str(first_model)
beta0 = first_model$coefficients[1]
beta1 = first_model$coefficients[2]

## Tracciamo la retta di regressione
x = data$Education
y = data$Income
x11()
plot(x, y, xlab = "Education", ylab = "Income", main = "Very simple regression")
abline(beta0, beta1, col = "blue")
# NOTA: abline prende in input intercetta (beta0) e coefficiente angolare (beta1)
help(abline)

# Calcoliamo i valori fittati
y_hat_by_hand = beta0 + beta1*data$Education
y_hat = first_model$fitted.values
y_hat_by_hand
y_hat

# Ed i residui
eps_hat = y - y_hat

# Gradi di libertà del modello:
p = 1
n = dim(data)[1]
dof = n - p - 1
dof
# Minori sono i gradi di libertà, meno dati rimangono per stimare la variabilità residua, 
# il che può influenzare l'affidabilità delle stime.
# Se n è piccolo e p è grande, i gradi di libertà si riducono, e il modello rischia di essere poco robusto.

# Stimiamo la varianza residuale
sigma_hat = sum(eps_hat^2)/dof
sigma_hat
sqrt(sigma_hat)

# Calcolo di R^2 (0 < R^2 < 1)
R2 = 1 - sum(eps_hat^2)/ sum((y - mean(y))^2)
R2 # Ottimo adattamento, come si vedeva dal grafico
# R2 è una misura di quanto il modello spiega della variabilità totale nei dati. 
# Indica la proporzione di varianza nei dati che è spiegata dalla variabile indipendente 
# (in questo caso Education)

R2_adj = 1 - (n-1)/(n-p-1) * sum(eps_hat^2)/ sum((y - mean(y))^2)
R2_adj
# R2_adj aggiustato tiene conto del numero di variabili nel modello e penalizza il modello per 
# l'aggiunta di variabili che non contribuiscono significativamente a spiegare la variabilità nei dati

# Con il comado summary, possiamo direttamente ricavare tutte queste informazioni
summary(first_model)
# Notare che tutto coincide con quanto abbiamo calcolato a mano

## Per ottenere tutti i possibili output che ci servono dal modello lineare costruito:
names(first_model)
first_model$coefficients  ### valori stimati dei beta
first_model$residuals     ### residui : epsilon_hat
first_model$rank          ### p + 1 
first_model$fitted.values ### valori fittati: y_hat
first_model$df.residual   ### gradi di liberta' del modello: n - p - 1 
first_model$model         ### Il data frame dei dati

vcov(first_model)         ## Matrice di covarianza dei beta
rstandard(first_model)    ## Residui standardizzati 
## Prendete confidenza con lm!


#### Diagnostica ------------------------------------------------------------------
# Verifichiamo le assunzioni del modello per poter fare inferenza
# (1) Normalità dei residui
shapiro.test(eps_hat)
## p-value alto -> sembra che le cose vadano bene

# (2) Omoschedasticità dei residui
#' Verifichiamo l'omoschedasticità con un plot dei residui standardizzati sul predittore
x11()
plot(data$Education, scale(eps_hat))
abline(h = 0) 
# Si direbbe nulla di patologico: non si identifica nessun trend particolare
# e i residui definiscono "una nuvola" intorno allo zero
# Se i dati sono omoschedastici, ci aspettiamo di vedere i residui distribuiti casualmente attorno alla linea 
# y=0. Non dovrebbero esserci tendenze visibili o nuvole di punti che si concentrano in alcune aree.

## Ulteriori grafici utili per la diagnostica sui residui:
x11()
par(mfrow = c(2,2))
plot(first_model)

# Grafico 1: Residui vs Fitted (valori predetti) --> utile per verifica omoschedasticità dei dati

# Grafico 2: Normal Q-Q (grafico quantile-quantile) dei residui --> utile per verificare normalità
# dei residui

# Grafico 3: Scale-Location (o grafico dei residui standardizzati)
# È simile al primo grafico (Residui vs Fitted), ma i residui sono standardizzati, 
# quindi il grafico permette di valutare meglio la dispersione dei residui. 
# E' utile per verificare l'omoschedasticità.

# Grafico 4: Cook's Distance

# Asse X (Leverage): # Il valore di leverage per il punto indica quanto quel punto è distante dal 
# centro della distribuzione delle variabili indipendenti. Un valore alto indica forte impatto sul modello.

# Asse Y (Standardized Residuals): Mostra quanto un punto si discosta dai valori stimati. 
# Residui elevati segnalano possibili outlier, ovvero osservazioni in cui il valore della variabile 
# dipendente Y è molto distante rispetto ai valori previsti dal modello.

# Linee di riferimento: Punti oltre queste soglie sono altamente influenti e potrebbero distorcere la regressione.
# Queste linee sono date dalla Cook’s Distance.

# Punto influente: Ha sia alto leverage che un residuo elevato, alterando significativamente il modello. 
# Identificabile dalla Cook’s Distance, se supera la soglia di riferimento.

# SOGLIE PRATICHE

# Residui standardizzati
# Un residuo standardizzato con valore assoluto maggiore di 2 (oppure 3 per un criterio più conservativo) indica un outlier nella variabile risposta.
#
# rstandard(first_model)

# Leverage (hi)
# Punti con leverage hi > 2(p+1)/n (oppure 3(p+1)/n come soglia conservativa) indicano punti ad alta leva.
#leverage = hatvalues(first_model)
#
#n = nobs(first_model)                 # numero osservazioni
#p = length(first_model$coefficients) - 1  # numero regressori (esclusa intercetta)
#soglia = 2 * (p + 1) / n
#soglia_cons = 3 * (p + 1) / n
#print(leverage)
#cat("Soglia leverage (2*(p+1)/n):", soglia, "\n")
#cat("Soglia conservativa (3*(p+1)/n):", soglia_cons, "\n")


# Cook’s Distance (Di)
# Punti con Cook's distance D_i > 4/n (oppure 1 come soglia conservativa) indicano punti influenti.
# 
#cooks_d = cooks.distance(first_model)
#n = nobs(first_model)
#soglia = 4 / n
#soglia_cons = 1
#print(cooks_d)
#cat("Soglia Cook's Distance (4/n):", soglia, "\n")
#cat("Soglia conservativa (1):", soglia_cons, "\n")


#### Inferenza sui risultati di regressione --------------------------------------
## (1) Intervalli di confidenza per i beta
alpha = 0.05
confint(first_model, level = 1 - alpha)

## Possiamo correggere per la nostra dimensionalità (p+1)
confint(first_model, level = 1 - alpha/2)  # in generale: 1 - alpha/(p+1)


## (2) Intervalli di confidenza e predizione per una nuova osservazione x0
x_0 = 16.5
predict(first_model, data.frame(Education = x_0), interval = "confidence", level = 1 - alpha) ## intervallo per la media
# Serve a stimare la media attesa della variabile dipendente per x_0 con un certo grado di confidenza.
# Ha un intervallo più stretto rispetto all'intervallo di predizione, perché tiene conto solo 
# dell'incertezza nella stima della media e non della variabilità futura.

predict(first_model, data.frame(Education = x_0), interval = "prediction", level = 1- alpha) ## intervallo di predizione
# Serve a prevedere un singolo valore futuro della variabile dipendente per x_0 tenendo conto della 
# variabilità residua del modello.
# Ha un intervallo più largo rispetto all'intervallo di confidenza, perché include sia 
# l'incertezza del modello che la variabilità intrinseca nei dati futuri.

## Riportiamo graficamente gli intervalli puntuali di predizione e confidenza
# (point-wise intervals!)
X0   = data.frame(Education = seq(10, 22, length=100)) 
# creiamo 100 "nuovi" punti e interpoliamo
Conf = predict(first_model, X0, interval='confidence')
Pred = predict(first_model, X0, interval='prediction')

x11()
plot(data$Education, data$Income, pch=19, xlab = 'Education', ylab = 'Income')
lines(X0[,1], Conf[,'fit'], lwd=1.5)
lines(X0[,1], Conf[,'lwr'], lty=2, col='red', lwd=3)
lines(X0[,1], Conf[,'upr'], lty=2, col='red', lwd=3)

lines(X0[,1], Pred[,'lwr'], lty=3, col='deepskyblue', lwd=3)
lines(X0[,1], Pred[,'upr'], lty=3, col='deepskyblue', lwd=3)

legend('topleft', legend = c('Fit', 'Confidence Interval', 'Prediction Interval'), lwd=2,
        lty = c(1,2,3) , col = c('black', 'red', 'deepskyblue') )

graphics.off()

## Per esempi grafici dedicati alla regressione semplice potete consultare, 
#  ad esempio, https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/


### REGRESSIONE LINEARE MULTIPLA -----------------------------------------------

# Adesso facciamo regressione lineare multipla mettendo anche un altro regressore: l'anzianita'
# Quindi il modello e' y_i = beta_0 + beta_1*Education_i + beta_2*Seniority_i + eps_i

range(data$Seniority)

## Noi intanto visualizziamo i dati nel nuovo spazio
open3d()
plot3d(data$Education,data$Seniority,data$Income,col = "red") ## Si puo' pensare che un piano di regressione possa adattarsi

second_model = lm(Income ~ Education + Seniority, data = data)
summary(second_model) 
## Nota: R-squared aumenta SEMPRE quando si aggiungono predittori

# Plottiamo il piano di regressione
# Equazione del piano di regressione:
# y - beta0 - beta1*x1 - beta2*x2 = 0
open3d()
plot3d(data$Education, data$Seniority, data$Income, size=5) 
beta = second_model$coefficients
planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1], col = "green", alpha = 0.4)
y_hat = second_model$fitted.values
coord_fitted = as.matrix(cbind(data[ ,1:2], y_hat))
points3d(coord_fitted, col='forestgreen', size=2)
for ( i in 1:n) lines3d(rbind(as.numeric(data[i,1:3]),coord_fitted[i,]), col = "red")

# Controlliamo le assunzioni:
# 1) Normalità dei residui 
shapiro.test(second_model$residuals) 
## Occhio alla normalita' : p-value > 0.05 di poco

# 2) Omoschedasticità dei residui
#' La controlliamo graficamente, insieme alla presenza di eventuali non linearità
#' o forti effetti di leva
x11()
par(mfrow = c(2,2))
plot(second_model)
# ok omoschedasticità, no forti effetti di leva

plot(second_model, which = 5)


#' Bisogna SEMPRE fare diagnostica sui residui e sulla retta di regressione, 
#' non limitarsi a guardare SOLO R2.

### Vediamo un esempio:
### QUARTETTO DI ANSCOMBE ------------------------------------------------------
data = anscombe
names(data)
cov(data)
dev.off()
# Le x hanno tutte la stessa media e varianza, le y anche, 
# i coefficienti di correlazione sono identici. Eppure...

# DATASET 1
x1 = data$x1
y1 = data$y1
lm1 = lm(y1 ~ x1)

summary(lm1)

x11(width=14, height=7)
par(mfcol = c(2,4))
plot(x1, y1, main='Dataset 1')
abline(lm1)

# Tutto regolare: la retta descrive bene i dati
# I residui sono casuali attorno a zero
# Modello lineare appropriato


plot(x1, residuals(lm1))
abline(h = 0)

# Nessun pattern nei residui


# DATASET 2
x2 = data$x2
y2 = data$y2
lm2 = lm(y2 ~ x2)

summary(lm2)

plot(x2, y2, main='Dataset 2')
abline(lm2)
# Relazione non lineare (curva a U)
# La retta non cattura bene il trend
# R² ingannevole: residui mostrano pattern evidente

plot(x2, residuals(lm2))
abline(h = 0)
# R^2 identico, identica retta di regressione, identica sigma_hat 
# ma la situazione vi sembra identica?
# Pattern a U nei residui -> modello lineare non adeguato


# DATASET 3
x3 = data$x3
y3 = data$y3
lm3 = lm(y3 ~ x3)

summary(lm3)

plot(x3, y3, main='Dataset 3')
abline(lm3)
# Presenza di un outlier verticale
# La retta è influenzata da un singolo punto
# Residui grandi per quel punto

plot(x3, residuals(lm3))
abline(h = 0)
# Residui per la maggior parte piccoli, ma un punto spicca


# DATASET 4
x4 = data$x4
y4 = data$y4
lm4 = lm(y4 ~ x4)

summary(lm4)

plot(x4, y4, main='Dataset 4')
abline(lm4)
# Presenza di un outlier in x (alto leverage)
# Punto lontano in x influenza fortemente la retta
# Guardare solo R² sarebbe fuorviante

plot(x4, residuals(lm4))
abline(h = 0)

### Controlliamo:
x11()
par(mfrow = c(2,2))
plot(lm3)

graphics.off()

### TRASFORMAZIONE DELLE VARIABILI NEI MODELLI LINEARI --------------------------
# Ricorda: Modello lineare significa lineare NEI PARAMETRI da stimare (i beta), non nei predittori (le x):
# y = beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x1*x2 + beta_4*atan(x1*x2) e' un modello lineare perfettamente valido.
# A volte puo' essere utile tentare di trasformare i dati prima di procedere alla regressione.

#### Esempio semplice ---------------------------------------------------------

#### Nel modello sul reddito visto in precedenza, 
#### c'e' qualche interazione tra Education e Seniority?
#### aggiungo un termine di interazione al mio modello
#### y = beta_0 + beta_1*Education + beta_2*Seniority + beta_3*Education*Seniority

data = read.csv("Income2.csv")
third_model = lm(Income ~ Education + Seniority + Education*Seniority, data = data)

summary(third_model)
## Osservazioni? Abbiamo ottenuto un miglioramento?
summary(second_model) 

### Proviamo a vedere cosa accade nello spazio
model = function(x1,x2){return(third_model$coefficients[1] + third_model$coefficients[2] * x1 + third_model$coefficients[3] * x2 + third_model$coefficients[4]*x1*x2 )}
open3d()
plot3d(model, xlim = range(data$Education), ylim = range(data$Seniority), alpha=0.6)
points3d(data$Education, data$Seniority, data$Income, size=5) 

## Si puo' pensare che un piano di regressione possa adattarsi
y_hat = third_model$fitted.values
coord_fitted = as.matrix(cbind(data[,2:3], y_hat))
for ( i in 1:n) lines3d(rbind(as.numeric(data[i,2:4]),coord_fitted[i,]), col = "red")
# notate che ha una curvatura, dovuta al termine di interazione

## Diagnostica
x11()
par(mfrow = c(2,2))
plot(third_model)

shapiro.test(third_model$residuals)

#' In questo caso, aggiungere termini non lineari non migliora il modello, 
#' e si direbbe non ci siano interazioni significative tra Education e Seniority
#' -> in questo caso è preferibile il modello più semplice senza interazioni


#### Esempio 2: Modello lineare multiplo con trasformazione delle variabili ------------
#' Regressione Multipla con il Dataset 'cars'####
#' Modello lineare multiplo con trasformazione delle variabili (termine quadratico)

# Cars dataset: i dati sono relativi alla velocita' delle macchine [km/h] 
# e la distanza necessaria a frenare [m] (i dati sono stati registrati nel 1920s).

data = read.table('cars.txt')
names(data)

speed = data$speed
dist = data$dist

# scatterplot
x11()
plot(data$speed, data$dist, xlab = 'Speed (km/h)', ylab = 'Stopping distance (m)', lwd=2, las=1)

### Proviamo un primo modello: Regressione lineare semplice
### dist = beta_0 + beta_1 * speed + Eps
result = lm(dist ~ speed, data=data)
summary(result)

coef = result$coef
plot(speed, dist, xlab = 'Speed (km/h)', ylab = 'Stopping distance (m)', lwd=2, las=1)
abline(result, lwd=2, col='red') 
# NOTA : prima avevamo dato i coefficienti ad abline, ora diamo result: è lo stesso

# diagnostica dei residui
x11()
par(mfrow=c(2,2))
plot(result)
# Dai residui vediamo un andamento che assomiglia ad una parabola. 
# L'omoschedasticita' non e' rispettata.
# Forse la regressione lineare semplice con la variabile originale  
# non basta per catturare questa variabilita' nei residui.

# controlliamo l'assunzione di normalita' dei residui
shapiro.test(residuals(result)) 
# p-value alto ma non altissimo


### Impostiamo un nuovo modello, aggiungendo un termine al quadrato:
### dist = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
speed2 = speed^2
result2 = lm(dist ~ speed + speed2, data=data) 
# notate la sintassi per aggiungere il termine al quadrato
# potrei anche scrivere speed*speed, aggiungendo il termine di interazione tra speed e sè stessa
summary(result2)

coef = coefficients(result2)

x11()
plot(speed, dist, xlab = 'Speed (km/h)', ylab = 'Stopping distance (m)', lwd=2, las=1)
x = seq(min(speed), max(speed), length=100)
lines(x, coef[1] + coef[2]*x + coef[3]*x^2, lwd=2, col='red')

# NB: ora la nostra retta ha una "curvatura" 
#     ma il modello è ancora un modello lineare!!

x11()
par(mfrow=c(2,2))
plot(result2)

shapiro.test(residuals(result2))
# adesso anche la diagnostica ci da' risultati migliori

# Riduciamo il modello togliendo il termine non significativo (speed)
# Notate che quando si tratta di effetti primari (come in questo caso) 
# e poi termini di ordine superiore della stessa variabile x
# (es: x^2, x^3, etc...) si tende a trattenere nel modello 
# anche gli effetti di ordine inferiore non significativi, se quelli
# di ordine maggiore lo sono.

result2 = lm(dist ~ I(speed^2), data=data)

# equivalentemente, rinominando la variabile:
speed2 = speed^2
result2 = lm(dist ~ speed2, data=data)
summary(result2)

coef = coefficients(result2)

# intervals on beta
confint(result2, level= 1 - 0.05)  # alpha = 0.05 -> 95% CI 

x11()
plot(speed,dist, xlab = 'Speed (km/h)', ylab = 'Stopping distance (m)', lwd=2, las=1)
x = seq(min(speed), max(speed), length=100)
lines(x, coef[1]+coef[2]*x^2, lwd=2, col='red')

x11()
par(mfrow=c(2,2))
plot(result2)

shapiro.test(residuals(result2))

# Proviamo ad utilizzare il nostro modello per predire nuove osservazioni 
# e generare degli intervalli di confidenza e predizione

# Step 1: generiamo 20 nuove osservazioni (dentro il range di valori osservati)
X.new = data.frame(speed2 = seq(min(speed2), max(speed2), len=20))

# Step 2: costruiamo gli intervalli
IC = predict(result2, X.new, interval="confidence", level=0.95)
IP = predict(result2, X.new, interval="prediction", level=0.95)

IC
IP

# disegnamo gli intervalli nello spazio del nostro ultimo modello (con speed^2 e dist)

x11()
plot(speed2, dist, main='Scatterplot Speed^2 vs. Dist', lwd=2,
     xlab='Speed2', ylab='dist')

matplot(X.new, IC, add=T, type='l', col=c('black','red','red'), lwd=2, lty=2)

matplot(X.new, IP, add=T, type='l', col=c('black','blue','blue'), lwd=2, lty=2)

legend('topleft', legend=c('Regression line','Confidence intervals','Prediction intervals'),
       col=c('black','red','blue'), lwd=2, lty=c(1,2,2), cex=1.2)


### Domanda: cosa succede se cambiamo unita' di misura?
### ESERCIZIO: Confrontare i risultati ottenuti con:
distance.ft = dist/0.3       # m -> feet
speed.mph = speed/1.6        # km per hour -> miles/hour 
### Confrontare:
### - Coefficienti stimati
### - Intervalli di confidenza dei coefficienti
### - Fitted values (y_hat)
### - Residui



#### Esempio 3: Ulteriore esempio di trasformazione delle variabili -----------------------------------------------

## A volte trasformare le variabili diventa un passo fondamentale!
rm(list = ls())
graphics.off()
data = Animals ## Peso dell'animale e massa del cervello
x11()
plot(data)
## Si puo' ipotizzare una relazione lineare tra le due masse? 
## Guardando allo scatterplot sembra improbabile. Proviamoci comunque

first = lm(brain ~ body, data = data)
summary(first) 
## Situazione pessima. 
#  Notare quanto la stima del coefficiente di body (beta1) sia vicina a zero
x11()
par(mfrow = c(2,2))
plot(first) # notare il plot della distanza di Cook

x11()
plot(data)
abline(first) 
## Il modello non funziona : dobbiamo pensare a qualcosa di diverso

## Mi ricordo che la trasformazione logaritmica "allarga" dati schiacciati 
## molto vicini a 0 e "restringe" valori molto lontani sulle code.
## Potrebbe fare al caso nostro:

second = lm(brain ~ log(body), data = data)
summary(second)
x11()
par(mfrow = c(2,2))
plot(second)

x11()
plot(log(data$body), data$brain)
abline(second) 
## Sicuramente meglio, ma proviamo a passare ai logaritmi anche sulle y
## Se trasformo anche y, mi ricordo che la stima che ottengo poi va SEMPRE ri-trasformata!

third = lm(log(brain) ~ log(body), data=data)
summary(third) 
## Grande miglioramento!
x11()
par(mfrow = c(2,2))
plot(third) 
## I dinosauri a quanto pare disturbano parecchio


x11()
plot(log(data$body),log(data$brain))
abline(third) 
## Riuscite a immaginare quali sono quei tre punti che 
## tirano in basso la nostra retta? Riguardate la diagnostica


x11()
ggplot(data, aes(x = log(body), y = log(brain))) + 
  geom_abline(intercept = third$coefficients[1], slope=third$coefficients[2]) + 
  geom_text(aes(label = row.names(data)))


### Togliamo i dinosauri
data$names = row.names(data)
data = data[data$names != "Triceratops", ]
data = data[data$names != "Dipliodocus", ]
data = data[data$names != "Brachiosaurus", ]

fourth = lm(log(brain) ~ log(body), data=data)
summary(fourth) ### Ora andiamo MOLTO BENE!

x11()
par(mfrow = c(2,2))
plot(fourth)

shapiro.test(fourth$residuals)

x11()
plot(log(data$body), log(data$brain))
abline(fourth, lwd=2, col="red") 

x11()
ggplot(data,aes(x = log(body), y = log(brain))) + geom_abline(intercept = fourth$coefficients[1],slope = fourth$coefficients[2]) + geom_text(aes(label = row.names(data)))


### Conclusioni: 
### (0) Un modello lineare naive tra massa corporea e cerebrale degli animali non ha alcun senso
### (1) La diagnostica ci ha condotto a scoprire che cercare di fare un modello unico 
###     per la relazione tra massa corporea e cerebrale
###     di mammiferi e dinosauri insieme non e' una grande idea
### (2) Abbiamo scoperto che nel caso dei mammiferi un buon modello e' 
###     log(y) = 2.15 + 0.752 * log(x)
###     ovvero y = exp(2.15) * x^0.752
### (3) Se continuassimo a farci guidare dalla diagnostica dell'ultimo modello, 
###     sembrerebbe che il cervello dei primati (umani, scimpanze', machachi)
###     segua un comportamento che "stona" un po' rispetto agli altri mammiferi. 
###     Vi suona familiare?
###
### Nota: ci sono stati, in effetti, casi in cui la regressione lineare aiuta 
###       le scienze applicate nel compiere nuove scoperte e formulare nuove ipotesi!
