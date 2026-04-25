#' ############################################################################
#' #########                  Regressione Logistica                   #########
#' ############################################################################

# Y_i ~ B(p_i)
# logit(p_i) = beta0 + betai x_i    i=1,...,n
# dove logit(p_i) = log(p_i / (1-p_i)) per 0<p_i<1
# da cui si ottiene hatp_i = exp( hatbeta0 + hatbeta1 x_i ) / (1 + exp( hatbeta0 + hatbeta1 x_i ))

### Importazione dataset ###
rm(list = ls() )
graphics.off()
cat("\014")

HF = read.csv("HF_data.csv")
str(HF)
HF$sex = as.factor(HF$sex) #1 = Uomo

# Eliminiamo la colonna X che non ci serve
HF$X <- NULL

# Scopo dello studio: spiegare la mortalità globale (DEATH_ind) alla prima ospedalizzazione
# in funzione dell'eta (age) in anni dei pazienti, 
# del sesso (sex) e del numero di comorbidità e procedure (n_com e n_pro). 

# Variabile dipendente: DEATH_ind (dicotomica 0-1) -> 1 = decesso, 0 = sopravvivenza
# Variabili indipendenti: age (continua), sex (dicotomica), n_com e n_pro (numeriche)   


# Nel seguito, se la variabile risposta è uguale a 1, parleremo di esito POSITIVO
# se la variabile risposta è uguale a 0, parleremo di esito NEGATIVO

table(HF$DEATH_ind)

# guardiamo le proporzioni dei casi (DEATH_ind = 1) nei diversi gruppi
# rispetto al genere
table(sex = HF$sex, death = HF$DEATH_ind) 

# rispetto al numero di comorbidita'
table(com = HF$n_com, death = HF$DEATH_ind)

# rispetto al numero di procedure
table(pro = HF$n_pro, death = HF$DEATH_ind)

# verifichiamo come sono distribuite le eta' tra uomini e donne
x11()
boxplot(HF$age ~ HF$sex)   # 1=uomo
# Gli uomini sono in media piu' giovani

## Potremmo avere un'idea globale col solito ggpairs
library(GGally)
x11()
ggpairs(HF, aes(col = sex))


### REGRESSIONE LOGISTICA SEMPLICE ----------------------------------------------------
## Adesso ci concentriamo solo sull'effetto della variabile età
## Prima di produrre un modello, vogliamo un'idea qualitativa di come varia la probabilità
## di morte al variare dell'età. Tuttavia, il grafico 
x11()
plot(HF$age,HF$DEATH_ind)
## non è particolarmente significativo

## Dividiamo in classi di età, diciamo 10 classi (arbitrario)
range(HF$age)
intervalli = seq(17,103, length.out = 11)
classes = cut(HF$age, breaks = intervalli)
classes
## Calcoliamo la proporzione di morti in ogni classe
prop = tapply(HF$DEATH_ind, classes, mean)
prop
## Facciamo un grafico per visualizzarle, assegnamo ogni proporzione al punto medio del suo intervallo:
x11()
par(cex = 1.5)
plot((intervalli[1:10] + intervalli[2:11]  )/2, prop, pch = 18)
abline(h = 0)
abline(h = 1)

# Tipica forma sigmoidale. Molto qualitativamente, la regressione logistica adatta una curva ai punti mostrati
# in precedenza, senza ovviamente passare per suddivisioni arbitrarie della
# probabilità di morte che aumentano all’aumentare della classe di età.

# Modello, regressione logistica semplice, solo con l'età come predittore #
help(glm)

# Y_i ~ B(p_i)
# logit(p_i) = beta0 + betai x_i    i=1,...,n
# dove logit(p_i) = log(p_i / (1-p_i)) per 0<p_i<1
# da cui si ottiene hatp_i = exp( hatbeta0 + hatbeta1 x_i ) / (1 + exp( hatbeta0 + hatbeta1 x_i ))
mod = glm(DEATH_ind ~ age, family=binomial(link=logit), data = HF)   # Sintassi per regressione logistica
summary(mod) 
# Modello stimato  logit(p) = log(p/(1-p))= -7.79599 + 0.10665 x age
# Il summary si interpreta come nel caso della regressione lineare
# Si faccia attenzione al fatto che i beta vengono stimati sul logit,
# non direttamente sulla probabilità
# notare z test e notare devianza (la quale viene minimizzata)

mod$coefficients # coefficienti del modello visti nel summary
#In questo caso, l’età, com’era facilmente intuibile dal grafico precedente, incide positivamente, 
#e in maniera statisticamente significativa, sulla probabilità di morte. 

## Valori stimati per il logit (l'equivalente degli y fittati nella regressione
## lineare, ma su scala logit). Stampiamo solo i primi 20, ovviamente sono
## tanti quanti le osservazioni (11611)

mod$linear.predictors[1:20]

## Possiamo ottenere le probabilità:
mod$fitted.values[1:20]

## Che ovviamente sono una funzione dei logit: 
## se t = logit(p) allora p = exp(t)/(1 + exp(t))
exp(mod$linear.predictors[1:20])/(1 + exp(mod$linear.predictors[1:20]))

## con queste probabilità possiamo tracciare la curva sigmoidale stimata:
x11()
par(cex = 1.5)
punti_x = (intervalli[1:10] + intervalli[2:11]  )/2
plot(punti_x, prop, pch = 18)
abline(h = 0)
abline(h = 1)
lines(sort(HF$age),sort(mod$fitted.values),col = "red")
## Si noti che la curva fittata non è proprio passante per i punti,
## che avevamo individuato in maniera piuttosto arbitraria e qualitativa


## Possiamo fare predizione nello stesso spirito del comando lm:
newdata = data.frame(age=52)
predict(mod, newdata = newdata, type = "response") # notate type='response': così troviamo la probabilità

## Possiamo anche fare inferenza sui parametri, perchè abbiamo anche una stima
## della varianza dei beta, che se abbiamo tante osservazioni possiamo considerare
## normalmente distribuiti:

vcov(mod) ## Matrice di covarianza stimata dei beta dall'algoritmo MLE
## I valori di varianza vengono usati per gli intervalli di confidenza:
alpha = 0.05
confint.default(mod, level = 1 - alpha) # affidabili in caso di osservazioni numerose


## Usando la matrice di covarianza dei beta, è anche possibile produrre automaticamente
## intervalli di previsione per la risposta (ovvero la probabilità)
## ma non ci addentriamo in questo aspetto. Passiamo all'interpretazione 
## basata sugli ODDS RATIOS


#--- Interpretazione dei Coefficienti (OR) ---#

# Uno dei motivi per cui la tecnica di regressione logistica è largamente diffusa specialmente 
# in ambito clinico,
# è che i coefficienti del modello hanno una naturale interpretazione in termini di odds ratio 
# (nel seguito OR).

# Si consideri un predittore x dicotomico a livelli 0 e 1. 
# Si definisce ODDS che y = 1 fra gli individui con x = 0 la quantità Pr(y=1|x=0)/(1-Pr(y=1|x=0)). 
# Analogamente per i soggetti con x = 1, l'ODDS che y = 1 è Pr(y=1|x=1)/(1-Pr(y=1|x=1)). 
# L'OR è definito come il rapporto degli odds per x = 1 e x = 0.

# Dato che:  Pr(y=1|X=1) = exp(beta0 + beta1X)/(1+exp(beta0 + beta1X))
#            Pr(y=1|X=0) = exp(beta0)/(1+exp(beta0))
#   => OR = exp(beta1)

# Predittore X continuo.
# Si definisce ODDS che Y = 1 per un individuo con X = x come la quantità:
# Pr(Y=1|X=x) / (1 - Pr(Y=1|X=x)).

# Se il modello di regressione logistica è dato da:
# Pr(Y=1|X) = exp(beta0 + beta1 * X) / (1 + exp(beta0 + beta1 * X))

# allora gli ODDS per X = x e X = x + 1 sono:
#   ODDS(Y=1 | X=x)   = exp(beta0 + beta1 * x)
#   ODDS(Y=1 | X=x+1) = exp(beta0 + beta1 * (x+1))

# L'OR per un incremento unitario di X è dato da:
# OR = ODDS(Y=1 | X=x+1) / ODDS(Y=1 | X=x) = exp(1*beta1)

# Se l'incremento di X è pari a k unità, allora:
# OR_k = exp(k * beta1)

summary(mod)
coef(mod)[2]

exp(coef(mod)[2])

# Quindi l'OR per un incremento di 10 anni d'età è  
exp(10*coef(mod)[2])   # circa 3

# per ogni incremento di 10 anni d'età, il rischio (non la probabilità!) di morte aumenta di 3 volte circa.

# Oss: il modello sottointende che il logit sia lineare nella variabile età, ossia che l'OR fra persone di 20 
#      contro 30 anni sia lo stesso che fra individui di 40 contro 50 anni. 
##_________________________________________________________________________________________

### REGRESSIONE LOGISTICA MULTIPLA --------------------------------------------------------
# Adesso facciamo la stessa cosa ma con tutte le variabili del modello.
# le variabili categoriche sono interpretate in maniera identica
str(HF)
mod = glm(DEATH_ind ~ . , family=binomial(link=logit), data = HF)   
summary(mod)                                   

#' Tutte le variabili sembrano significative.
#' Se volessimo approfondire l'indagine sulla significatività o non significatività
#' dei predittori, come al solito dovremmo procede in modo stepwise.
#' Il comando anova esegue una sorta di inclusione delle variabili in direzione forward
#' (i.e. dal modello base a quello che contiene tutte le variabili, aggiungendo un predittore
#' alla volta), e valuta via via la significatività del predittore aggiunto in
#' termini di riduzione della funzione di verosimiglianza (Deviance).
anova(mod, test="Chisq")
#' Tutti i predittori risultano significativi: l'aggiunta di ciascuna variabile
#' riduce significativamente la devianza del modello.

mod$coefficients
#1) L’età incide positivamente sulla probabilità di morte
#2) I pazienti di sesso maschile hanno una probabilità di morte più elevata, in particolare l’intercetta 
#   del modello per gli uomini (in scala logit, non dimenticarlo mai) è maggiore di 0.3438 rispetto   
#   all’intercetta del modello per le donne
#3) Un elevato numero di comorbidità aumenta la probabilità di morte, 
#4) mentre il numero di procedure ospedaliere subite tende a ridurlo


#Interpretazione gli ODDS Ratios dei vari predittori

# quant'è l'ODD Ratio tra sesso maschile e femminile? E per una comorbidità in più?

# Per il sesso maschile:
exp(coef(mod)["sex1"]) # odds ratio tra uomini e donne

# Per le comorbidità
exp(coef(mod)["n_com"]) #odds ratio per una comorbidità in più

# odds ratio per un aumento di età di 10 anni
exp(coef(mod)["age"]*10)


#Esprimendo il rischio in termini di odds ratio, troviamo dunque che il rischio di morte aumenta di 
#circa 3 volte per un aumento di 10 anni di età, del 40% per gli uomini rispetto alle donne e del 
#17% per una comorbidità in più.


mod$linear.predictors[1:20] # valori stimati dei logit

# Valori stimati per la probabilità di decesso 
mod$fitted.values[1:20]

# Intervalli di confidenza per i beta
confint.default(mod, level = 1-alpha)

# Proviamo a fare qualche previsione.
newdata = data.frame(age=95,sex=as.factor(1),n_com=2,n_pro=2)
predict(mod, newdata = newdata,type = "response")
