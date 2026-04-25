########## Analisi descrittiva e grafica delle variabili numeriche ##########
setwd("C:/Users/user/Desktop/EseBDA-2425/Laboratori/Lab1")
rm(list = ls())


## PRINCIPALI INDICI PER VARIABILI NUMERICHE -----------------------------------

## Vogliamo concentrarci sul peso dei nostri pinguini

## carichiamo i dati
dati = read.csv("penguins.txt") 

## Ricordiamo che dati$peso è un vettore (di numeri in questo caso)
## Fermiamoci un attimo per imparare cosa sono i vettori in R, come trattarli, e quali operazioni possono
## essere effettuate su di essi.
  
## Creare  vettori di numeri
v1 = c(1, 2.4, 5.6, 7)
v1
v1[3]
v2 = c(2,3,4,0)

## Ci sono molti altri metodi per creare vettori. Uno molto utile è il seguente
v3 = seq(1,10, by = 0.1) # Numeri equispaziati di 0.1, da 1 a 10
v3

v3.bis = seq(1, 10, length.out=90)
v3.bis

v6
## Principali operazioni
2 * v1 # usuale prodotto per un numero

v1/2   # usuale divisione per un numero

v1 - 1 # A tutti gli elementi viene sottratto 1

v1 + 4 # A tutti gli elementi viene sommato 4

v1 + v2 # Somma componente per componente

v1 * v2 # Prodotto COMPONENTE PER COMPONENTE, non prodotto tra vettori che conoscete in algebra

sum(v1) # Somma di tutti gli elementi di v1

v1[2]   # Secondo elemento di v1

v1[c(2,4)] # Secondo e quarto elemento di v1

v1[1:3]  # Elementi di v1 dal primo al terzo

length(v1) # numero degli elementi di v1

cos(v1) #coseno effettuato componente per componente

exp(v1) # esponenziale effettuato componente per componente


# Nota: Stiamo solo stampando a video i risultati delle operazioni. Non abbiamo creato nuovi oggetti,
# come potete vedere dall'environment, né modificato v1 o v2
# Per creare o modificare oggetti, bisogna SEMPRE effettuare un "assegnamento", ovvero usare il
# simbolo =

v1 = 2*v1 # Adesso ho modificato v1

v3 = v1*v2 # Adesso ho creato un nuovo vettore, v3, prodotto di v1 e v2.


# Nota: queste semplici operazioni tra vettori saranno sufficienti ai fini del corso, ed è bene che le
# teniate a mente. Si fa presente che molte altre operazioni sono disponibili. 
# Rimandiamo gli interessati a questi ed altri dettagli al file Algebra.r


## Torniamo adesso all'analisi del peso dei nostri pinguini
  
# Possiamo applicare a qualunque vettore numerico le funzioni statistiche di base.
# Facciamolo sui dati di peso
# Indici di posizione
min(dati$peso)
max(dati$peso)
range(dati$peso)
mean(dati$peso) # la media
median(dati$peso) # mediana
quantile(dati$peso)  
quantile(dati$peso, 0.10)
dati$peso[order(dati$peso)] # mostra i dati ordinati in ordine crescente
quantile(dati$peso, c(0.25, 0.75)) # quartili a 0.25 e 0.75
# visualizzazione globale
summary(dati$peso)
dati$specie = factor(dati$specie)
# dati$sesso = factor(dati$sesso)
summary(dati)
# Nota: ovviamente, R applica le formule che tutti conosciamo. Ad esempio possiamo ottenere la media:

n = length(dati$peso)
media = sum(dati$peso)/n
media

# Indici di dispersione
var(dati$peso) # varianza campionaria, calcolata come noto:

1/(n - 1) * sum((dati$peso - media)^2)

### Possiamo anche ottenere direttamente la deviazione standard campionaria come
sd(dati$peso)

IQR(dati$peso) # Range interquartile

## RAPPRESENTAZIONE GRAFICA DELLE VARIABILI NUMERICHE ---------------------------
## Adesso concentriamoci sulla rappresentazione grafica delle variabili numeriche.
## Il commando per aprire un device grafico è x11() (su windows e linux). Per Mac,
## sostituire con quartz()

x11()



# ISTOGRAMMA: un istogramma conta la frequenza dei dati in ogni intervallo numerico prefissato

## COSTRUIRE UN ISTOGRAMMA
# Per costruire un istogramma si effettuano le seguenti operazioni:
# - si divide il range dei dati in classi (intervalli)
# - si calcolano, per ogni classe, frequenze assolute, frequenze relative
#   e densità [densità = (frequenza relativa)/(ampiezza classe)]
# - in corrispondenza di ogni classe si disegnano dei rettangoli di area
#   pari alla frequenza relativa della classe considerata (ovvero di
#   altezza pari alla densità)

# NB: spesso un istogramma viene rappresentato con l'altezza dei rettangoli
# uguale alle frequenza assoluta. In questo caso, però, l'istogramma
# è coerente solo se le classi hanno tutte la stessa ampiezza.

## ora guardiamo la variabile peso
x11()
hist(dati$peso) # in ordinata ci sono le frequenze assolute
hist(dati$peso, prob=TRUE) # in ordinata ci sono le frequenze relative
abline(v= median(dati$peso), col = 'red')
abline(v= mean(dati$peso), col = 'green')
abline(v= quantile(dati$peso, 0.25), col = 'blue')

# posso giocare con il numero di classi: non esiste un numero di classi
# 'giusto', la scelta sta alla sensibilità dello statistico
hist( dati$peso, prob = TRUE, breaks = 25, col = 'orange',
      main = 'Istogramma del peso', xlab = 'peso', ylab = 'Densita' )


## COSTRUIRE UN BOXPLOT

# Il boxplot è uno strumento grafico molto utile per identificare
# eventuali asimmetrie della distribuzione e/o la presenza di eventuali
# valori estremi (outlier).

# Per costruire un boxplot (verticale) si effettuano le seguenti operazioni:
# - si costruisce un rettangolo con basi inferiore e superiore uguali,
#   rispettivamente, al primo e al terzo quartile e che quindi conterrà
#   il 50% centrale delle osservazioni
# - all'interno del rettangolo di traccia una linea in corrispondenza
#   della mediana
# - si considera il limite superiore uguale a Q3 + 1.5*IQR e si traccia
#   un baffo che collega la base superiore del rettangolo all'osservazione
#   più alta contenuta all'interno del limite superiore
# - si considera il limite inferiore uguale a Q1 - 1.5*IQR e si traccia
#   un baffo che collega la base inferiore del rettangolo all'osservazione
#   più bassa contenuta all'interno del limite superiore.
# - eventuali valori maggiori del limite superiore o minori di quello
#   inferiore vengono segnati singolarmente con un cerchio sul grafico
#   e vengono chiamati outlier (superiori o inferiori).

# Con R il boxplot si ottiene tramite il comando "boxplot"
x11()
boxplot(dati$peso, main = 'Boxplot del peso' , col = "forestgreen")

# per disattivare e chiudere tutti i device grafici e reimpostare il default:
graphics.off()

# Più plot nello stesso device
x11()



par(mfrow = c(1,2)) # sara' un device 1x2, quindi c'e' spazio per 2 grafici. 
for( i in 1:2){
  hist(dati[,i], main = names(dati)[i], xlab =  names(dati)[i], prob = T) # Per riportare le frequenze metto prob = T. Lascio che i bin vengano costruiti autonomamente
}
dev.off()


### Se voglio tutto alla stessa scala specifico ylim.
x11()
par(mfrow = c(1,2))  
for( i in 1:2){
  hist(dati[,i], main = names(dati)[i], xlab =  names(dati)[i], prob = T, col = i, ylim = c(0,0.10)) 
}
dev.off()

### Nota: ai fini del corso, è fondamentale sapere cosa sia un istogramma e cosa sia un boxplot,
### e saperli rappresentare nelle loro forme di base
### Tuttavia, in nessun caso sarà richiesto di produrre grafici complessi.
### Ricordiamo inoltre che tutto il codice dei laboratori sarà sempre a vostra disposizione

### Si fa presente, tuttavia, che essere in grado di produrre e manipolare grafici è un'ottima competenza,
### anche se al di là degli scopi del corso.
### Rimandiamo gli interessati al file tidyverse.r, fuori programma,
### che dà un assaggio delle potenzialità delle librerie
### grafiche e di manipolazione dati di R.


## Passiamo adesso alle variabili categoriche.

## VARIABILI CATEGORICHE -----------------------------------------------------
# La colonna sesso del data frame è un esempio di realizzazione
# di variabile categorica (dicotomica in questo caso)
# Ricordiamoci di convertirla a factor

str(dati)
dati$sesso = factor(dati$sesso)
dati$specie = factor(dati$specie)

f_ass = table(dati$sesso) # Otteniamo le frequenze assolute
f_ass

# calcoliamo le frequenze relative:
f_rel = f_ass / length(dati$sesso)
f_rel
# metodo alternativo
f_rel2 = prop.table(f_ass)
f_rel2

# Barplot:
# costruiamo un grafico a barre con le frequenze assolute e relative
x11()
par(mfrow = c(2,1))
barplot(f_ass, col=c('pink','lightblue'), ylab='F. ass')
barplot(f_rel, col=c('pink','lightblue'), ylab='F. rel')


x11()
pie(f_ass, col = c('pink','lightblue')) ## Grafico a torta


# Dati categorici a più classi
# Usiamo dei dati artificiali, supponiamo di prendere 120 persone a caso in una classe e chiediamo 
# a ognuno la provincia di provenienza
prov_data = factor(c(rep("Milano",60),rep("Lecco",20),rep("Bergamo",18),rep("Brescia",22)))
prov_data
str(prov_data)
# Mescoliamo
prov_data = sample(prov_data)
# Si tratta di dati categorici
# Individuare i valori
factor(prov_data)
# Calcolare le numerosita'
num = table(prov_data)
num
# Calcolare le frequenze
table(prov_data)/length(prov_data)
# Diagramma a barre
x11()
barplot(table(prov_data)/length(prov_data), col = "gold")
dev.off()
# Diagramma a torta
x11()
pie(table(prov_data))
dev.off()

#calcolo della moda
max(num)
num[num == 60]
num[num == max(num)]


## ANALISI DIFFERENZIATA PER CLASSI ---------------------------------------------
## Capita spesso di voler differenziare l'analisi di una variabile numerica, in base ad una categorica.
## Ad esempio, vogliamo analizzare il peso dei pinguini maschi e delle femmine in maniera separata.
## Vengono in nostro aiuto i comandi R per manipolare vettori

## Peso dei soli pinguini maschi:


tf = dati$sesso == "M"
tf
peso_maschi = dati$peso[dati$sesso == "M"]
peso_femmine = dati$peso[dati$sesso == "F"]

## Rispettive medie
mean(peso_maschi)
mean(peso_femmine)


## Posso anche creare direttamente due data frame e analizzarli separatamente
pinguini_maschi = dati[dati$sesso == "M",]
pinguini_femmine = dati[dati$sesso == "F",]
## Notare la notazione matriciale: nei comandi precedenti, il dataframe "dati" è visto come una matrice,
## ovvero un oggetto con doppia indicizzazione, e noi abbiamo richiesto, nel primo comando ad esempio,
## di selezionare tutte le colonne della matrice (ovvero le variabili), ma solo le righe corrispondenti
## a osservazioni con sesso maschile. Useremo spesso questa indicizzazione per estrarre i dati che ci 
## interessano.
mean(pinguini_maschi$peso) ## e così via...

## e così via... abbiamo alcune scorciatoie che R ci mette a disposizione. Se ad esempio voglio
## fare boxplot del peso divisi per specie:

x11()
boxplot(dati$peso~dati$specie, col = "gold")
#boxplot(dati$peso~dati$sesso, col = c("pink", "lightblue"))

## Ma le vedremo quando ci capiterà di utilizzarle.

# Nota: combinando le funzioni di manipolazione base di R, con un po' di pazienza, e' possibile manipolare un data frame senza limiti. Analogamente,
# con le funzioni di plotting base di R si può realizzare praticamente qualunque cosa.
# Tuttavia, tantissime funzioni molto avanzate riguardo queste due grandi aree dell'analisi descrittiva 
# sono gia' presenti in un sistema di pacchetti software avanzato, il tidyverse, che introduciamo, solo per gli interessati,
# nello scriptvtidyverse.r. 
