## Pacchetti da installare: 
## se non ancora installati, decommentare le seguenti righe ed eseguire solo la prima volta
#install.packages("GGally")
# install.packages("gplots")
# install.packages("tidyverse")

library(tidyverse)
library(GGally)

graphics.off()
rm(list = ls())
cat("\014")

### ESPLORAZIONE GRAFICA PER DATI MULTIVARIATI ---------------------------------
dati = read.csv("appendiceA.txt", sep = ";")
str(dati)
dati$X = dati$Paziente = NULL # Liberiamoci delle colonne inutili
dati$Sesso = factor(dati$Sesso) # Conversione a variabile categorica
str(dati)


#' Solo guardando i grafici, è già possibile farsi un'idea della struttura
#' di correlazione di un dataset. 

#### Funzione di base: plot() 
x11()
plot(dati)
# già notiamo correlazione positiva tra peso e pressione nei pazienti

#### La funzione ggpairs di GGally è più flessibile.
ggpairs(dati, aes(alpha = 0.4, color = Sesso)) # Tante informazioni in pochissimo spazio!)
# Diagonale:
# - Grafici di densità per le variabili numeriche, separati per genere (colori diversi per maschi e femmine).
# - Un grafico a barre (barplot) per la variabile categorica (Sesso), che mostra la distribuzione dei maschi e delle femmine.

# - Triangolo inferiore 
# Il triangolo inferiore contiene:
# - Grafici a dispersione, che mostrano la relazione tra le variabili.

# Triangolo superiore (Correlazioni)
#' Nella parte superiore della tabella a incrocio, è riportato il risultato di un test di
#' correlazione di Pearson fatto fra le variabili relative ad ogni incrocio.
#' Il test è fatto sia per la popolazione completa (valore in nero), che per le sottopopolazioni
#' relative a ciascun genere.
#' Il valore numerico rappresenta la stima della correlazione, gli asterischi riportano
#' invece il livello di significatività del test di correlazione
#' [RECAP stat 1: *** vuol dire MOLTO significativo].
cor.test(dati$Peso, dati$Colesterolo, method = "pearson")
cor.test(dati$Peso[dati$Sesso=='F'], dati$Colesterolo[dati$Sesso=='F'], method = "pearson")
cor.test(dati$Peso[dati$Sesso=='M'], dati$Colesterolo[dati$Sesso=='M'], method = "pearson")

# Colonna più a destra
# La colonna più a destra presenta boxplot, che confrontano la distribuzione delle variabili 
# numeriche tra i due generi.

# Riga più in basso
# La riga più in basso presenta istogrammi, che confrontano la distribuzione delle variabili 
# numeriche tra i due generi.


#### Calcolo della matrice di covarianza
cov(dati[ ,-4])
# RICORDA: con dati[,-4] sto prendendo tutte le righe di dati e tutte le colonne meno la quarta

#### Calcolo della matrice di correlazione
cor(dati[,-4])

#### Eventualmente è anche possibile plottare un grafico 3D
# install.packages("rgl")
library(rgl)
colors = as.numeric(dati$Sesso)
colors
plot3d(dati$Peso, dati$Colesterolo, dati$Pressione, col = colors)
#colors <- ifelse(dati$Sesso == "F", "deeppink", "blue")
#plot3d(dati$Peso, dati$Colesterolo, dati$Pressione, col = colors)
?plot3d

### VERIFICA DI NORMALITA' -----------------------------------------------------
#' Ci troviamo spesso nella necessità di verificare se un campione 
#' può essere assunto (anche solo approssimativamente) gaussiano.
#' E' possibile affermare che i dati sono normali? Come possiamo testarlo?

## Usiamo prima i dati sul colesterolo
col = dati$Colesterolo
mu_hat = mean(col)
sd_hat = sd(col)


#### STRUMENTO 1 (grafico) -----------------
#' Visualizziamo l'istogramma e sovrapponiamo la densità normale (con stessa media e sd)
t = seq(min(col), max(col), length.out = 1000)
f = dnorm(t, mean = mu_hat, sd = sd_hat) # densità normale a cui do in input t, media e sd
x11()
hist(col, breaks = 15, col = "gold", probability = T)
lines(t, f, col = "red", type = "l", lwd = 2)

#### STRUMENTO 2 (grafico) -----------------
#' Un'altra possibilità è usare i qq-plot
#' Confrontiamo i quantili empirici ottenuti dai dati con i quantili teorici della normale corrispondente.
x11()
qqnorm(col)
qqline(col, col = "red") ## In caso di normalità perfetta, i dati giacciono sulla retta

#### STRUMENTO 3 (test) -----------------
## Metodo quantitativo, test di Shapiro ( per numerosità < 5000)
## H0 : X~N   vs H1: dati non gaussiani
## per p-value bassi, rifiutiamo l'ipotesi di normalità dei dati
## per p-value alti, H0 non può essere rifiutata: i dati provengono da una distribuzione distribuita normalmente
shapiro.test(col)
## Se non c'è evidenza per rifiutare H0, si può lavorare sotto l'ipotesi di gaussianità

# Se proviamo a fare lo stesso con i dati sul peso:
# a.) Strumento 1
tpeso = seq(min(dati$Peso), max(dati$Peso), length.out = 1000)
fpeso = dnorm(tpeso, mean = mean(dati$Peso), sd = sd(dati$Peso))
x11()
hist(dati$Peso, breaks = 15, col = "gold", probability = T)
lines(tpeso, fpeso, col = "red", type = "l", lwd = 2)

# b.) Strumento 2
x11()
qqnorm(dati$Peso)
qqline(dati$Peso, col = "red")
## Pessimo adattamento alla linea

# c.) Strumento 3
shapiro.test(dati$Peso) ## ma n = 312... quindi?

## Qualunque risultato sarà asintotico e basato sul TCL.
## Dal TCL, mean(dati$Peso) è approssimativamente normale se n è abbastanza grande (come nel nostro caso).
## Ricordate, è la media che tende a una normale, i dati continuano (ovviamente) 
## a seguire la loro ignota distribuzione.

### TEST di IPOTESI sulla MEDIA di una SINGOLA POPOLAZIONE ---------------------

#' Rinfreschiamoci la memoria su alcuni test di base. 
#' Concentriamoci sul colesterolo e procediamo passo per passo.
#' Ricordiamoci che abbiamo appena accettato che i dati sul colesterolo siano
#' gaussiani. Dalla gaussianità dei dati deriva la garanzia che la media campionaria
#' sia uno stimatore gaussiano.

#### CASO 1: pop distribuita normalmente: t-test ---------------------------------
#' Si supponga di voler aiutare un medico a dimostrare che i pazienti hanno 
#' in media un livello di colesterolo inferiore a 200

## TEST UNILATERO A SINISTRA
## H0: mu >=200 vs H1: mu<200
mu_0 = 200
n = length(dati$Colesterolo)
media_campionaria = mean(dati$Colesterolo)
dev_std_campionaria = sd(dati$Colesterolo)
t = (media_campionaria - mu_0)/sqrt(dev_std_campionaria^2/n)  #Statistica test
pval = pt(t, df = n-1)
#Calcolo il pvalue
pval # il medico ha probabilmente ragione nell'affermare che "la media del colesterolo è < 200"

### Si ricorda che:
### - p-value ALTO: l'evidenza empirica non è sufficientemente contraria all'ipotesi nulla H0,
###                 che quindi non può essere rifiutata

### - p-value BASSO: l'evidenza empirica è fortemente contraria all'ipotesi H0 che quindi va rifiutata

## TEST UNILATERO A DESTRA
## H0: mu<=200 vs H1: mu>200
pval = 1 - pt(t, df=n-1)
pval

## TEST BILATERO
## H0: mu=200 vs H1: mu!=200
pval = 2*( 1 - pt(abs(t), df = n-1)) 
pval

# Oppure possiamo utilizzare la funzione t.test, prestando attenzione alla sintassi
t.test(dati$Colesterolo,mu = 200,alternative = "less") # UNILATERO A SINISTRA
t.test(dati$Colesterolo,mu = 200,alternative = "greater") # UNILATERO A DESTRA
t.test(dati$Colesterolo,mu = 200,alternative = "two.sided") # BILATERO

##### INTERVALLO DI CONFIDENZA PER LA MEDIA del colesterolo
alpha = 0.1
quantile = qt(1-alpha/2, df = n-1) # qui utilizzo i quantili (qt anzichè pt)
c(media_campionaria - quantile/sqrt(n)*dev_std_campionaria, 
  media_campionaria + quantile/sqrt(n)*dev_std_campionaria)



#### CASO 2: pop non normale ma osservazioni numerose: z-test --------------------

## Proviamo con il peso, che non è distribuito normalmente. 
## Un medico sostiene che i pazienti pesino in media 157 libbre,
## sottoponiamo l'affermazione a verifica d'ipotesi con uno z-test asintotico

## Ho sostituito pnorm al posto di pt

## TEST BILATERO
## H0: mu=157 vs H1: mu!=157
mu_0 = 157
n = length(dati$Peso)
media_campionaria = mean(dati$Peso)
dev_std_campionaria = sd(dati$Peso)
t = (media_campionaria - mu_0)*sqrt(n)/dev_std_campionaria #Statistica test
pval = 2*(1-pnorm(abs(t)))                                 #Calcolo il pvalue del test bilatero
pval

## Anche in questo caso è possibile formulare TEST UNILATERO a DESTRA e SINISTRA
## ricalcando i casi visti in precedenza, con l'accorgimento di sostituire pnorm al posto di pt

## Anche gli intervalli di confidenza di costruiscono in maniera analoga:
## utilizzando qnorm

##### INTERVALLO DI CONFIDENZA PER LA MEDIA del peso
alpha = 0.01
quantile = qnorm(1-alpha/2) 
n = length(dati$Peso)
c(media_campionaria - quantile/sqrt(n)*dev_std_campionaria, 
  media_campionaria + quantile/sqrt(n)*dev_std_campionaria)

## RIEPILOGO:
## 1) verifico ipotesi di lavoro
## 2) formulo correttamente ipotesi nulla e alternativa
## 3) calcolo p-value con le formule corrispondenti


### TEST di CONFRONTO tra MEDIE di popolazioni indipendenti -------------------
#' Adesso ci occupiamo di confrontare il livello di colesterolo nei pazienti
#' maschi e femmine.
#' In particolare, vogliamo accertarci che ci sia evidenza per affermare che 
#' le donne hanno in media un livello di colesterolo diverso da quello degli uomini.
#' 
#' Formalmente, ciò che vogliamo testare è: 
#' H0: muF=muM vs H1: muF!=muM
#' 
#' NOTA che questo test può essere formulato in un caso più generale, in cui ci
#' si chiede se la differenza fra le medie sia pari ad una certa quantità delta0:
#' H0: muF-muM=delta0 vs H1: muF-muM != delta0
#' 
#' Nel nostro caso, in cui ci chiediamo se le medie siano uguali o diverse, delta0=0.

colesterolo_uomini = dati$Colesterolo[dati$Sesso == "M"]
colesterolo_donne = dati$Colesterolo[dati$Sesso == "F"]

### Vediamo prima la normalità IN OGNI SOTTOPOPOLAZIONE
shapiro.test(colesterolo_uomini)
shapiro.test(colesterolo_donne)
#### Possiamo stare abbastanza tranquilli

#### CASO 1 : Caso a varianze note -----------------------------------------------

# date le nostre due popolazioni normali
# supponiamo che le varianze siano NOTE e indichiamole con var_M e var_F
var_M = 150
var_F = 110
delta0 = 0
differenza_medie = mean(colesterolo_uomini) - mean(colesterolo_donne)
n_uomini = length(colesterolo_uomini)
n_donne = length(colesterolo_donne)
z = (differenza_medie - delta0) / sqrt(var_M/n_uomini + var_F/n_donne)
pvalue = 2*(1-pnorm(abs(z)))
pvalue


#### CASO 2. Caso a varianze non note --------------------------------------------
#' Le VARIANZE nelle due sottopopolazioni sono UGUALI? O DIVERSE?
#' Esistono dei testi approssimati per l'uguaglianza tra varianze di due campioni, 
#' come ad esempio il test di Fisher.
#' NB: Non vi è richiesto di conoscere i dettagli
#' sul test di Fischer. E' sufficiente che, leggendo il p-value, voi sappiate
#' trarre la conclusione corretta sulle varianze delle due popolazioni.
var.test(colesterolo_uomini, colesterolo_donne)
# pvalue=7.7%, morale: interpretare i risultati con cautela 

#' Stiamo testando:
#' H0: muF=muM vs H1: muF!=muM
#' Quindi di nuovo possiamo specificare
delta0=0

### Scorciatoia: Usare la funzione t.test
t.test(colesterolo_uomini,colesterolo_donne, mu=delta0, var.equal = TRUE)
#nota: esiste anche var.equal=F per il caso in cui le varianze siano diverse

### Oppure facciamo artigianalmente con la procedura che conosciamo
delta0=0
X = colesterolo_donne
Y = colesterolo_uomini
nX = length(X)
nY = length(Y)
s2x = var(X)
s2Y = var(Y)
s2p = 1/(nX + nY - 2)  * ((nX-1)*s2x + (nY-1)*s2Y)
sp = sqrt(s2p)
t = (mean(X) - mean(Y) - delta0) / (sp * sqrt (1/nX + 1/nY))
pval = 2*(1 - pt(t, df = nX + nY - 2)) # Risultati coerenti
pval

## Ripetere le stesse analisi sulla variabile Peso, 
## DOPO aver deciso se condurre un t-test o uno z-test


