################################
##### Approccio Ad Rstudio #####
################################

## Introduzione e approccio ai dati ## -----------------------------------------------------

## A meno di personalizzazioni, avete davanti a voi 4 finestre:

# In alto a sinistra, visualizzate gli Script, ovvero semplicissimi file di testo in cui
# scrivere i comandi che successivamente volete eseguire.
# Per creare un nuovo script File -> Nuovo script

# In alto a destra visualizzate l'Environment, ovvero tutti gli oggetti che avete finora creato
# Al momento, l'environment è vuoto

# In basso a sinistra, avete la Console, in cui i comandi vengono eseguiti, uno per volta.
# Vi consigliamo di non scrivere MAI i comandi direttamente sulla console: scriveteli prima su uno script
# per poi eseguirli

# In basso a destra, abbiamo la possibilità di navigare attraverso tutti i file del nostro computer.
# Non useremo molto questa finestra.

# Iniziamo la nostra sessione. E' buona norma iniziare sempre con il comando 

rm(list = ls()) ## premete ctrl+invio per eseguire il comando

# Il comando ripulisce l'Environment da ogni oggetto precedentemente creato.
# Come ogni linguaggio di programmazione che si rispetti, R è in primo luogo una calcolatrice 
# potente e versatile

x = 2.1              ## Ho creato una variabile numerica, l'ho chiamata x, e le ho assegnato il valore 2.1
y = 
x*exp(x)*atan(x) ## Ho creato y facendo dei conti su x
y                    ## Stampo y a video

rm(list = ls())      ## Ripulisco tutto 
rm(y)

## Come vedete, ogni commento è preceduto da #. Questo simbolo indica alla console che tutto ciò che
## segue deve essere ignorato. 
## (è SEMPRE opportuno commentare i propri script come mostrato a laboratorio)


## In R è possibile fare conti con numeri, funzioni, vettori, matrici e anche oggetti più complessi.
## Ci capiterà di utilizzare queste operazioni, che vedremo passo passo.
## Tuttavia, il principale scopo del corso è Analizzare Dati, che generalmente
## si trovano in forma di tabelle sul nostro computer. E' su questo che ci focalizzeremo adesso.

## Iniziamo la nostra prima analisi. Questi i passi fondamentali:

## Dire a R in quale cartella desideriamo lavorare. E' un passaggio FONDAMENTALE, che va fatto OGNI
## volta che iniziate a lavorare con R. Questo perchè R vede SOLO i file che sono contenuti nella cartella
## di lavoro:

## Stampare a video la cartella di lavoro attuale:
getwd()

## Visualizzare i File disponibili (e quindi gli unici visibili a R)

dir()

## Cambiare la directory di lavoro (da interfaccia Rstudio)
# 'Session' -> 'Set working directory' -> 'choose directory'
# Impostiamo "Lab1" come directory di lavoro.

# Adesso siamo pronti a caricare il nostro primo dataset
# Il comando che utilizzeremo (ce ne sono decine, ognuno specializzato su diversi tipi di file)
# è read.csv.

# read.csv si limita a leggere una tabella da file di testo, assumendo che i campi siano separati da virgole.
# Questo è proprio il caso del file "penguin.txt"
# Scriveremo dunque semplicemente

dati_pinguini = read.csv("penguins.txt")
head(dati_pinguini)

## Supponiamo di voler ripetere l'operazione per la tabella "studenti.txt" che si trova nella cartella "Data".
## Ci accorgiamo di due problemi:

# Il file non è visibile a R, che vede solo la cartella Data:

dir()

# Il file non è separato da virgole, bensì da spazi.

# Per quanto riguarda il primo problema, se infatti scriviamo

dati_studenti = read.csv("studenti.txt") # viene stampato un messaggio di errore

## Devo dire a R dove cercarlo, A PARTIRE DALLA DIRECTORY DI LAVORO

dati_studenti = read.csv("Data/studenti.txt") ## Il comando funziona
head(dati_studenti)

## Però qualcosa è andato storto! Non abbiamo ottenuto il risultato che speravamo,
## perchè nel file originale i dati non sono separati da virgola, bensì da spazi.
## per risolvere il problema, occorre utilizzare l'argomento sep della funzione read.csv:

dati_studenti = read.csv("Data/studenti.txt", sep = " ")
head(dati_studenti) ## Adesso tutto funziona perfettamente
dati_studenti

## dati_pinguini e dati_studenti sono oggetti speciali: dataframe, ovvero un insieme (una lista) di vettori,
## in questo caso 4 vettori, della stessa lunghezza, che possono essere di tipi diversi
## Per avere accesso ai dati, si procede scrivendo "Nome del data frame"$"Nome della variabile"

## Ad esempio voglio stampare a video il peso dei pinguini

dati_pinguini$peso
dati_pinguini$peso
dim(dati_pinguini)

## Notare che non possiamo accedere a "peso" senza specificare che si trova nel dataframe "dati_pinguini"

peso ## errore!

## Se proprio voglio creare una nuova variabile che contiene il peso dei pinguini:

peso = dati_pinguini$peso 

peso ## adesso la variabile esiste!

## Ci accorgiamo che la colonna X è inutile, per eliminarla facciamo così
dati_pinguini$X

dati_pinguini$X = NULL ## Notare come il dataset si trasforma
head(dati_pinguini)
## Per ottenere la struttura generale di un dataframe, ovvero dimensioni e tipi delle variabili,
## scriviamo str("nome del dataframe")

str(dati_pinguini)
str(x)

## Vediamo che la colonna peso è una variabile numerica, mentre specie e sesso sono stringhe ("chr")
## E' buona norma fare sapere a R che desideriamo che le variabili stringhe vengano trattate
## come variabili categoriche:

dati_pinguini$specie = factor(dati_pinguini$specie)
dati_pinguini$sesso = factor(dati_pinguini$sesso)

str(dati_pinguini) ## Adesso il dataframe è composto solo da variabili numeriche o categoriche


## RIEPILOGO ##

## Nel corso dei prossimi laboratori, e in sede d'esame, sarà assolutamente fondamentale:


#  Essere in grado di cambiare la directory di lavoro all'inizio della sessione

#  Essere in grado di importare un data frame con il comando read.csv e i suoi argomenti

#  Visualizzare la struttura del data frame con il comando str, nonchè modificare
#  le variabili categoriche con il comando factor

#  Ricordarsi che, se desiderate accedere a una colonna del data frame, va usata la sintassi
#  "Nome del dataframe"$"Nome della colonna"

#  Parte degli homework di questo laboratorio si concentrano su questo, esercitatevi e prendete confidenza
#  con questi comandi