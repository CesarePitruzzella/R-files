##########################################
########### ESERCIZIO 1 ##################
##########################################

# Se necessario, impostare la directory di lavoro in "Esercitazione 1"

# 0. Ripulire l'environment, e caricare i dati dei pazienti presenti nel file "appendiceA.txt", 
#    usando opportunamente la funzione read.csv. Eliminare la colonne del dataframe che a vostro avviso sono inutili.
# 1. Stampare a video i livelli di colesterolo dei pazienti
# 2. Calcolare indici di posizione e dispersione della variabile colesterolo e rappresentarla graficamente attraverso 
#    boxplot e istogrammi
# 3. Costruire una tabella con le frequenze relative di maschi e femmine
# 4. Dividere i dati di colesterolo tra pazienti maschi e femmine, calcolando gli indici di posizione e 
#    dispersione per ogni sesso.
# 5. Confrontare gli istogrammi del colesterolo per i maschi e per le
#    femmine mettendoli fianco a fianco in un unico grafico.
# 6. Cosa possiamo affermare sul rapporto tra sesso del paziente
#    e livello di colesterolo?
# 7. Ripetere i punti da 1 a 6 considerando solo i dati dei primi 100 pazienti del data frame

# SOLUZIONE

# 0.
rm(list=ls())
pazienti = read.csv(file = 'appendiceA.txt', sep=';')
names(pazienti)
pazienti$X = NULL
pazienti$Paziente = NULL

# 1.
pazienti$Colesterolo
# alternativamente, creando una nuova variabile:
colesterolo = pazienti$Colesterolo
colesterolo 


# 2.
# indici di posizione
mean(colesterolo)
median(colesterolo)
# indici di dispersione
var(colesterolo)
sd(colesterolo)
IQR(colesterolo)
# overview generale della variabile
summary(colesterolo)
# boxplot
x11()
boxplot(colesterolo, col='pink') 
# Note: qui vediamo qualche outlier sotto il limite inferiore Q1 - 1.5*IQR
# proviamo a disegnarlo sul grafico (opzionale)
lim_inf = quantile(colesterolo, 0.25) - 1.5*IQR(colesterolo)
abline(h=lim_inf, col='red')
# istogramma
x11()
par(mfrow=c(2,2)) # qui creo una interfaccia grafica con due righe e due colonne, che contenga tutte e
                  # quattro le alternative di istogramma che propongo qua sotto
hist(colesterolo, col='deeppink') # istogramma con frequenze assolute sulle ordinate
hist(colesterolo, prob=TRUE, col='orange') #istogramma con densità sulle ordinate
hist(colesterolo, prob=TRUE, breaks = 30) #istogramma con più classi
hist(colesterolo, prob=TRUE, breaks = 5) #istogramma con meno classi (poco leggibile)


# 3.
pazienti$Sesso = factor(pazienti$Sesso)
freq_ass = table(pazienti$Sesso)  # frequenze assolute
freq_rel = freq_ass/length(pazienti$Sesso)
freq_rel
# oppure:
freq_rel2 = prop.table(freq_ass) 
freq_rel2


# 4.
# ____MASCHI____
maschi = pazienti[pazienti$Sesso=='M',]
# indici di posizione
mean(maschi$Colesterolo)
median(maschi$Colesterolo)
# indici di dispersione
var(maschi$Colesterolo)
sd(maschi$Colesterolo)
IQR(maschi$Colesterolo)
# overview generale della variabile
summary(maschi$Colesterolo)
# ____FEMMINE____
femmine = pazienti[pazienti$Sesso=='F',]
# indici di posizione
mean(femmine$Colesterolo)
median(femmine$Colesterolo)
# indici di dispersione
var(femmine$Colesterolo)
sd(femmine$Colesterolo)
IQR(femmine$Colesterolo)
# overview generale della variabile
summary(femmine$Colesterolo)


# 5.
#' Nelle analisi comparative, è spesso molto rilevante imporre che i grafici messi a confronto abbiano
#' gli stessi assi. Questo permette di individuare meglio le differenze che intercorrono fra popolazioni.
#' Costruiamo quindi gli istogrammi, ma imponendo che gli assi su cui sono definiti sia uguali.
#' Questo si ottiene specificando:
#' 1. I parametri xlim e ylim nel comando hist (NB: questo vale per qualsiasi altro comando
#'   per fare plot)
#' 2. I breaks, cioè gli estremi degli intervalli in cui è suddivisa l'asse delle ascisse

limiti.x = range(pazienti$Colesterolo) # imponiamo limiti dell'asse delle ascisse come gli estremi del range
                                       # del colesterolo, a prescindere dal sesso
limiti.y = c(0,0.04)
#' ATTENZIONE: il limite superiore di questo range va cambiato in modo che includa
#'             completamente le colonne degli istogrammi. Se facendo runnare
#'             i comandi qui sotto vi accorgete che vengono plottate delle colonne
#'             troppo basse, abbassate il valore 0.04.
#'             Se invece le colonne sono "tagliate", alzate il valore 0.04.


x11()
par(mfrow=c(1,2))
hist( femmine$Colesterolo, prob = TRUE,
      main = 'Istogramma del colesterolo delle femmine', xlab = 'Colesterolo',
      ylab = 'Densita', col = 'pink', xlim = limiti.x, 
      breaks = seq( min( pazienti$Colesterolo ), max( pazienti$Colesterolo ), length = 10 ),
      ylim=limiti.y)
abline(v=median(femmine$Colesterolo), col = 'red')
abline(v=mean(femmine$Colesterolo), col = 'green')

hist( maschi$Colesterolo, prob = TRUE,
      main = 'Istogramma del colesterolo dei maschi', xlab = 'Colesterolo',
      ylab = 'Densita', col = 'lightblue', xlim = limiti.x, 
      breaks = seq( min( pazienti$Colesterolo ), max( pazienti$Colesterolo ), length = 10 ),
      ylim=limiti.y)
abline(v=median(maschi$Colesterolo), col = 'red')
abline(v=mean(maschi$Colesterolo), col = 'green')

# 6. Cosa possiamo affermare sul rapporto tra sesso del paziente
#    e livello di colesterolo?

#' Commento: Guardando gli indici calcolati al punto 4., possiamo dire che media e mediana di
#'           colesterolo nelle femmine sono più alte rispetto ai maschi.
#'           Il colesterolo dei maschi presenta varianza e deviazione standard più alte.
          

#Graficamente, possiamo confrontare i boxplot:
x11()
boxplot(pazienti$Colesterolo ~ pazienti$Sesso, col = c('pink','lightblue'),
        ylab = 'Colesterolo', main = 'Boxplot del Colesterolo F/M')

#' Commento: Le femmine hanno valori di colesterolo più alti e presentano minore dispersione.
#'           I maschi presentano valore medio e mediano inferiori rispetto alle femmine,
#'           ma i valori sono più dispersi. Presentano infatti alcuni outliers, sia superiori che
#'           inferiori, e dei baffi più lunghi rispetto al boxplot delle femmine.


# 7. 
new_pazienti = pazienti[1:100, ] # considero nuovo dataset con solo le prime 100 righe (i primi 100 pazienti)
                                 # e tutte le colonne originali: stessi comandi con new_pazienti al posto di pazienti

dev.off()   # chiudo tutti i grafici



##########################################
#########  ESERCIZIO 2  ###################
##########################################
## Consideriamo adesso i dati contenuti 
## nel file "studenti.txt", che si trova nella cartella Data della cartella Lab1

# 0) importare in R il dataset "studenti.txt" e salvarlo in un dataframe
# 1) Calcolare gli indici di posizione per la variabile "Taglio":
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9
# 2) Calcolare gli indici di dispersione visti per la variabile "Taglio":
#    varianza della popolazione, deviazione standard, range
#    e range interquartile
# 3) Costruire un istogramma che illustri le frequenze relative 
#    della variabile "Taglio".
#    Che considerazioni si possono trarre dall'istogramma?
# 4) Costruire un boxplot con le osservazioni della variabile "Taglio".
#    Che considerazioni si possono trarre dal boxplot?
# 5) Calcolare gli indici di posizione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    media campionaria, massimo, minimo, mediana, primo e terzo quartile
#    e il quantile di ordine 0.9
# 6) Calcolare gli indici di dispersione per la variabile "Taglio"
#    nei due sottocampioni individuati dal genere:
#    varianza della popolazione, deviazione standard, 
#    range e range interquartile
# 7) Costruire gli istogrammi che illustrino le frequenze relative
#    della variabile "Taglio" nei due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due istogrammi?
# 8) Costruire i boxplot con le osservazioni della variabile "Taglio"
#    per i due sottocampioni individuati dal genere.
#    Che considerazioni si possono fare confrontando i due boxplot?
# 9) Calcolare le frequenze assolute e relative della variabile "Bevanda", e farne un diagramma a barre
#    e uno a torta.
# 10) Ripetere il punto 9), dividendo stavolta la variabile "Bevanda" in base al genere

# SOLUZIONE

# 0.
rm(list=ls())
studenti = read.csv(file = 'Data/studenti.txt', sep=' ')


# 1. 
taglio = studenti$Taglio
summary(taglio)         # min, max, Q1, Q3, media e mediana
quantile(taglio, 0.9)   # quantile 0.9


# 2.
var(taglio)
sd(taglio)
range(taglio)
IQR(taglio)


# 3.
x11()
hist(taglio, prob=TRUE, col='gold')
# la distribuzione non è simmetrica, in particolare ha una coda destra poichè
# la maggior parte delle osservazioni è concentrata su valori più bassi, 
# mentre si hanno poche osservazioni verso valori più alti


# 4.
x11()
boxplot(taglio, col='gold')
# vediamo che i valori più alti sono da considerarsi outliers,
# sono infatti sopra al limite superiore dato da Q3 + 1.5*IQR
# la maggior parte dei dati è concentrata su valori più bassi
# per questo la distribuzione risulta essere marcatamente asimmetrica
lim_sup = quantile(taglio, 0.75) + 1.5*IQR(taglio)
lim_sup
abline(h=lim_sup, col='red')


# 5.
studenti$Sesso = factor(studenti$Sesso)
# ____MASCHI____
maschi = studenti[studenti$Sesso=='M',]
summary(maschi$Taglio)
quantile(maschi$Taglio, 0.9)
# ____FEMMINE____
femmine = studenti[studenti$Sesso=='F',]
summary(femmine$Taglio)
quantile(femmine$Taglio, 0.9)


# 6.
# ____MASCHI____
var(maschi$Taglio)
sd(maschi$Taglio)
range(maschi$Taglio)
IQR(maschi$Taglio)
# ____FEMMINE____
var(femmine$Taglio)
sd(femmine$Taglio)
range(femmine$Taglio)
IQR(femmine$Taglio)


# 7. 
limiti.x = range(studenti$Taglio) # imponiamo limiti dell'asse delle ascisse come gli estremi del range
# del colesterolo, a prescindere dal sesso
limiti.y = c(0,0.05)
#' ATTENZIONE: il limite superiore di questo range va cambiato in modo che includa
#'             completamente le colonne degli istogrammi. Se facendo runnare
#'             i comandi qui sotto vi accorgete che vengono plottate delle colonne
#'             troppo basse, abbassate il valore 0.04.
#'             Se invece le colonne sono "tagliate", alzate il valore 0.04.


x11()
par(mfrow=c(1,2))
hist( femmine$Taglio, prob = TRUE,
      main = 'Femmine', xlab = 'Taglio',
      ylab = 'Densita', col = 'pink', xlim = limiti.x, 
      breaks = seq( min( studenti$Taglio ), max( studenti$Taglio ), length = 10 ),
      ylim=limiti.y)
abline(v=median(femmine$Taglio), col = 'red')
abline(v=mean(femmine$Taglio), col = 'green')

hist( maschi$Taglio, prob = TRUE,
      main = 'Maschi', xlab = 'Taglio',
      ylab = 'Densita', col = 'lightblue', xlim = limiti.x, 
      breaks = seq( min( studenti$Taglio ), max( studenti$Taglio ), length = 10 ),
      ylim=limiti.y)
abline(v=median(maschi$Taglio), col = 'red')
abline(v=mean(maschi$Taglio), col = 'green')

graphics.off()

# 8.
x11()
boxplot(studenti$Taglio ~ studenti$Sesso, col=c('pink', 'lightblue'))
# commento: le femmine presentano molti più outliers e la mediana della variabile 
#           Taglio risulta molto più bassa nei maschi che nelle femmine.
#           In generale la variabile Taglio è distribuita su valori maggiori per le femmine


# 9.
f_ass = table(studenti$Bevanda)
f_ass 
f_rel = prop.table(f_ass)
f_rel
# oppure
f_rel = f_ass/length(studenti$Bevanda)
f_rel
# diagramma a barre
x11()
par(mfrow=c(1,2))    # una riga due colonne: grafici affiancati
barplot(f_ass, main='Frequenza Assoluta Bevande', col=c('yellow', 'orange', 'red'))
barplot(f_rel, main='Frequenza Relativa Bevande', col=heat.colors(3))
# diagramma a torta
x11()
pie(f_ass, main='Frequenza Assoluta Bevande', col=c('yellow', 'orange', 'red'))


# 10.
# ____MASCHI____
bevanda.maschi = studenti$Bevanda[studenti$Sesso=='M']
f_ass.maschi = table(bevanda.maschi)
f_ass.maschi 
f_rel.maschi = prop.table(f_ass.maschi)
f_rel.maschi
# diagramma a barre
x11()
par(mfrow=c(1,2))
barplot(f_ass.maschi, main='Frequenza Assoluta Bevande', col=c('lightblue', 'deepskyblue', 'blue'))
barplot(f_rel.maschi, main='Frequenza Relativa Bevande')

# ____FEMMINE____
bevanda.femmine = studenti$Bevanda[studenti$Sesso=='F']
f_ass.femmine = table(bevanda.femmine)
f_ass.femmine 
f_rel.femmine = prop.table(f_ass.femmine)
f_rel.femmine
# diagramma a barre
x11()
par(mfrow=c(1,2))
barplot(f_ass.femmine, main='Frequenza Assoluta Bevande', col=c('lightpink', 'pink', 'deeppink'))
barplot(f_rel.femmine, main='Frequenza Relativa Bevande')
# diagramma a torta : affianco i due grafici per confrontarli 
x11()
par(mfrow=c(1,2))
pie(f_ass.femmine, main='Frequenza Bevande Femmine')
pie(f_ass.maschi, main='Frequenza Bevande Maschi')
