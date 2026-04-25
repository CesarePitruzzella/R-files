#' #################################################################
#' ############## SELEZIONE STEPWISE DELLE VARIABILI ################
#' #################################################################
#' 
#' Cominciamo a introdurre il tema della selezione dei predittori (variable selection)
#' nei modelli di regressione lineari. Questo è un argomento che ritroveremo anche
#' più avanti nel corso (Lasso regression), di cui per ora vediamo i metodi più classici
#' ed intuitivi:
#' 1. Forward selection
#' 2. Backward selection
#' 3. Mixed selection
#'
#' Vediamo questi tre approcci alla variable selection, applicati ad un caso pratico
#' di regressione della profondità dei terremoti rispetto alla loro posizione geografica.
#' 
#'

#### In Terremoti.csv sono contenuti dei rilevamenti riguardanti 960 eventi sismici:
rm(list = ls())
graphics.off()
cat("\014")
library(MASS)
library(car)
library(rgl)
library(tidyverse)

terremoti = read.csv("Terremoti.csv")
str(terremoti)

## Vogliamo predirre la profondità in funzione di latitudine e longitudine, 
## eventualmente usando anche la zona sismica
data = data.frame(lat = terremoti$lat,
                  long = terremoti$long,
                  depth = terremoti$depth,
                  zone = terremoti$zone)
data$zone = factor(data$zone)
str(data)

## Visualizziamo i dati, colorandoli in base alla zona sismica: 
open3d()
points3d(x=data$lat, y=data$long, z= data$depth, col = data$zone, size=4, aspect = T)
box3d()
axes3d()

#' La divisione per zona geografica sembra, almeno a occhio, abbastanza significativa.
#' Questo vuol dire che ha senso introdurre la variabile categorica zona nel modello.
## Comunque, proviamo a ignorarla e facciamo un modello lineare molto semplice
first = lm(depth ~ lat + long, data = data)
summary(first)
x11()
par(mfrow = c(2,2))
plot(first) ## R2 pessimo, pessima distribuzione e comportamento dei residui. 

## Proviamo a visualizzare cosa sta accadendo:
open3d()
points3d(x=data$lat, y=data$long, z= data$depth, col = data$zone, size=4, aspect = T)
box3d()
axes3d()
beta = first$coefficients
planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1],col = "green",alpha = 0.4)
points3d(data$lat,data$long,first$fitted.values, size = 2, col = "blue") ## Possiamo fare decisamente di meglio.

### Proviamo a includere la zona sismica nel modello
second = lm(depth ~ lat + long + zone + zone:lat + zone:long, data = data)
summary(second)
x11()
par(mfrow = c(2,2))
plot(second)
shapiro.test(second$residuals)

# In questo caso la zona 1 è la baseline, e quindi corrisponde a un valore 0 della
# variabile dummy "zone2" definita da R, mentre la zona 2 corrisponde al valore 1
# di "zone2"

## Vediamo i due piani che abbiamo generato
open3d()
points3d(x=data$lat, y=data$long, z= data$depth, col = as.numeric(data$zone) +1, size=4, aspect = T)
box3d()
axes3d()
beta = second$coefficients
planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1],col = "red",alpha = 0.4)
planes3d(a = c(-beta[2] - beta[5],- beta[3] - beta[6],1), d = -beta[1] - beta[4],col = "green",alpha = 0.4)
points3d(data$lat,data$long,second$fitted.values, size = 2, col = "blue") 

#' Domanda: si puo' pensare che i nostri dati siano ben descritti da 
#' un modello polinomiale?
#' 
#' Iniziamo con un modello molto "ricco" di variabili, in cui sono inclusi tutti i
#' contributi quadratici delle variabili continue e tutti i possibili termini di
#' interazione fra le variabili continue (anche quelle trasformate) e la variabile
#' categorica:
fourth = lm(depth ~ lat + long + I(lat^2) + I(long^2) + lat:long + 
              zone + zone:lat + zone:long + zone:I(lat^2) + zone:I(long^2) + 
              zone : I(lat*long) , data = data)
summary(fourth)
# Si noti che nel modello iniziale uno dei coefficienti associati al
# termine zone:I(lat*long) non è stimabile (singolarità nella matrice
# del modello).
shapiro.test(fourth$residuals)
x11()
par(mfrow = c(2,2))
plot(fourth)


## Plottiamo le due superfici che abbiamo fittato
open3d()
points3d(x=data$lat, y=data$long, z= data$depth, col = as.numeric(data$zone), size=4, aspect = T,zlim = range(data$depth))
lat_vector = seq(min(data$lat), max(data$lat),length.out = 100)
long_vector = seq(min(data$long), max(data$long),length.out = 100)
surface3d(lat_vector, long_vector, 
          matrix(predict(fourth, expand.grid(lat = lat_vector, long = long_vector, zone = factor(1))),100,100),
          alpha = 0.5, col='red', zlim = range(data$depth))
surface3d(lat_vector, long_vector, 
          matrix(predict(fourth, expand.grid(lat = lat_vector, long = long_vector, zone = factor(2))),100,100),
          alpha = 0.5, col='green',zlim = range(data$depth))
points3d(x=data$lat, y=data$long, z= fourth$fitted.values, col = "blue", size=2, aspect = T)
box3d()
axes3d()
#' Considerazioni:
#' 1. Il modello lineare si adatta bene anche per fittare due paraboloidi
#' 2. Attenzione: non si faccia inferenza/previsione in punti dello spazio in cui non ci sono osservazioni
#' 3. Summary complicati all'aumentare dei predittori: come trovare il giusto compromesso?


## Il processo di selezione e riduzione del modello puo' essere 
## reso automatico con alcuni algoritmi!

## Utilizziamo stepAIC che procede con la stepwise selection nelle sue tre forme, 
## a seconda dell'opzione `direction` specificata, e restituisce il modello che, 
## in termini di AIC, non poteva essere ulteriormente migliorato 
## (la regola è quella di preferire i modelli con l'AIC più basso).

## Nota bene: Tra gli algoritmi di scelta delle variabili abbiamo:
#- Backward elimination (`"backward"`): elimina predittori in sequenza dal modello iniziale. 
   #Produce una sequenza di modelli di complessita' decrescente fino ad ottenere
   #il modello ottimo. 
#- Forward selection (`"forward"`): aggiunge predittori in sequenza, usando quelli 
   #disponibili nell'argomento `data` di `lm`. Produce una sequenza di modelli di complessità 
   #crescente fino a raggiungere il modello ottimo.
#- Stepwise regression (`"both"`): una ricerca forward-backward che, ad ogni step, decide 
   #se includere o escludere un predittore. A differenza delle modalità precedenti, 
   #un predittore che era stato incluso/escluso in uno step precedente, può essere 
   #escluso/incluso in uno step successivo.


### Stepwise forward selection -----------------------------------------------------------

# definisco il modello minimo con solo l'intercetta,
# passando però tutto il dataset alla funzione
# questo permetterà a stepAIC di sapere dove prendere le variabili da
# aggiungere fino a raggiungere l'upper bound 
minimal_model <- lm(depth ~ 1, data = data)

forward = stepAIC(minimal_model,direction="forward",
                  scope=list(lower=minimal_model,upper=fourth))
forward$anova
summary(forward)
x11()
par(mfrow = c(2,2))
plot(forward)

# Il metodo forward parte dal modello minimo (solo intercetta) e aggiunge
# progressivamente i termini presenti nello scope fino a quando l'AIC
# non può essere ulteriormente migliorato.
#
# Il modello finale include quasi tutti i termini del modello `fourth`,
# ad eccezione dell'interazione tra latitudine e longitudine:
# - lat:long
#
# Il modello risultante contiene quindi 10 predittori (oltre all'intercetta).

### Stepwise backward selection ----------------------------------------------------
backward = stepAIC(fourth, details = T, direction = "backward")
backward$anova
summary(backward)
x11()
par(mfrow = c(2,2))
plot(backward)

shapiro.test(backward$residuals)

# Il metodo backward parte dal modello completo `fourth` e rimuove
# progressivamente i termini che non migliorano il valore di AIC.
# In particolare elimina:
# - lat:long
# - zone:I(lat*long)
# - I(lat^2):zone
#
# Il modello finale contiene quindi 8 predittori (oltre all'intercetta).

### Stepwise mixed selection -----------------------------------------------------
both = stepAIC(fourth, details = T, direction = "both")
both$anova
summary(both)
x11()
par(mfrow = c(2,2))
plot(both)
# Il metodo stepwise ("both") parte dal modello completo e, ad ogni step,
# valuta sia l'eliminazione sia l'aggiunta di termini, scegliendo la
# modifica che produce la maggiore riduzione dell'AIC.
#
# Nel nostro caso il procedimento porta allo stesso modello finale
# ottenuto con la backward elimination. I termini rimossi sono:
# - lat:long
# - zone:I(lat*long)
# - I(lat^2):zone
#
# Il modello finale contiene quindi 8 predittori (oltre all'intercetta).


# In conclusione, backward elimination e stepwise (both) portano allo
# stesso modello finale, mentre la forward selection produce un modello
# più complesso.

## Nota: i metodi di selezione stepwise presentano qualche criticita' 
## e vanno usati con cautela
