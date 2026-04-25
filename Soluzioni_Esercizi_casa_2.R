
#####################
# Durante l'estate australe tre specie di pinguini nidificano lungo la costa
# Antartica: Chinstrap, Adelie and Gentoo.
# Alcuni biologi hanno misurato il peso di 90 adulti:
# 15 maschi e 15 femmine per ognuna delle tre specie (file penguins.txt). 

peng = read.table(file='penguins.txt', sep=',', header=T)
str(peng)

# Come sempre, facciamo alcune manipolazioni iniziali per preparare il dataset
peng$specie = factor(peng$specie)
peng$sesso = factor(peng$sesso)
peng$X = NULL
str(peng)

# (1) Visualizzare i dati di peso mettendoli in relazione ai fattori Sesso e Specie
library(GGally)
library(tidyverse)
library(ggplot2)

ggpairs(peng, aes(alpha = 0.4, color = specie), columns = 1:2) # guardiamo solo specie come categorica
ggpairs(peng, aes(alpha = 0.4, color = sesso), columns = c(1,3)) # guardiamo solo sesso come categorica
ggpairs(peng, aes(alpha = 0.4, color = specie), columns = 1:3) # visualizziamo entrambe ma coloriamo per specie 


# (2) In base all'esplorazione grafica, quale fattore (Sesso o specie) scegliereste per
# un modello Anova?

# Scegliamo specie, perchè vediamo più differenze su peso tra specie che tra generi diversi


# (3) Dopo aver effettuato i dovuti controlli sulle ipotesi sottostanti il modello,
# implementate l'Anova sul peso scegliendo Specie come fattore

# controlliamo la normalità dei dati in ogni gruppo
tapply(peng$peso, peng$specie, shapiro.test)
# pvalue alti in tutti e tre i gruppi--> accetto normalità

# controlliamo che le varianze nei gruppi siano uguali 

bartlett.test(peng$peso ~ peng$specie)
# il pvalue è appena sotto 0.05, siamo al limite nell'accettare l'uguaglianza delle varianze,
# procediamo con cautela.

# classi bilanciate?
table(peng$specie) # si



# (4) Commentate adeguatamente i risultati, e fate seguire al modello opportuni
# intervalli di confidenza per  differenze delle medie nei gruppi
Anova_peng = aov(peso ~ specie, data = peng)
summary(Anova_peng)
# Il test rifiuta decisamente H_0. C'è evidenza per affermare che esistono almeno
# due gruppi con media diversa.

# La stima della comune varianza 
S = sum(Anova_peng$residuals ^ 2) / Anova_peng$df.residual # df=n-g
S

# Abbiamo rifiutato H0 ma non sappiamo quale gruppo è diverso.
# Facciamo gli intervalli di confidenza delle medie e li confrontiamo
# per capire per quale specie la media del peso è significativamente diversa
# dagli altri gruppi

alpha=0.05

source("Confronti_multipli_da_anova.R")
# Questa funzione fornisce gli intervalli di confidenza cercati, i p-value relativi al 
# t-test di uguaglianza tra le medie ed un oggetto grafico per visualizzare i risultati

intervalli_anova = Confronti_multipli_da_anova(Anova_peng, alpha = alpha)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

### Proviamo a vedere come cambiano i risultati se correggiamo con Bonferroni ###
intervalli_anova = Confronti_multipli_da_anova(Anova_peng, alpha = alpha,bonferroni = TRUE)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

# In entrambi i casi (sia con la correzione per Bonferroni che senza), 
# la differenza fra le medie di ciascuna coppia di specie è significativamente
# diversa da zero. C'è evidenza per concludere che le medie delle tre specie
# siano tutte diverse.

# (5) Concentratevi adesso sul fattore Sesso. Impostare un opportuno test di ipotesi
# che verifichi se c'è sufficiente evidenza per affermare che i pinguini maschi
# pesino più delle femmine. Ricordate di verificare le ipotesi modellistiche prima
# di scegliere il test.

# Rivediamolo graficamente
ggpairs(peng, aes(alpha = 0.4, color = sesso), columns = c(1,3)) # guardiamo solo sesso come categorica
tapply(peng$peso, peng$sesso, mean) 
# ad occhio le medie sembrano molto simili

# Impostiamo il test
# H0: mu_m <= mu_f   vs   H1: mu_m > mu_f

# controlliamo che siano due popolazioni normali
femmine = peng[peng$sesso=='F',]
maschi = peng[peng$sesso=='M',]

shapiro.test(femmine$peso)
shapiro.test(maschi$peso)
#' Entrambi i pvalue sono molto bassi. In particolare, non c'è evidenza a livello
#' 0.01 per affermare che le due popolazioni (peso dei maschi e peso delle femmine)
#' siano normali.
#' 

#' Siccome la numerosità del campione è sufficientemente elevata (n>30), possiamo
#' testare comunque l'uguaglianza fra le medie utilizzando uno z-test.
#' Prima, però, bisogna fare un test per l'uguaglianza delle varianze.
#' ATTENZIONE!
help(var.test)
#' Come vedete dall'help, il test di Fisher per l'uguaglianza delle varianze lavora
#' sotto l'ipotesi di normalità delle popolazioni. Nel nostro caso questa ipotesi
#' non è verificata, ma anche qui la numerosità delle sottopopolazioni viene in nostro
#' soccorso e ci consente di considerare come affidabili i risultati del test.

# Vediamo quindi se le varianze possono essere assunte uguali:
# (NB: I comandi qui sotto sono tre alternative valide per fare il test)
var.test(peng$peso ~ peng$sesso) # si, le varianze sono uguali (e NON note)
var.test(maschi$peso,femmine$peso)
var.test(femmine$peso, maschi$peso)

# Construisco manualmente lo z-test
X = femmine$peso
Y = maschi$peso
nX = length(X)
nY = length(Y)
s2X = var(X)
s2Y = var(Y)
s2p = 1/(nX + nY - 2)  * ((nX-1)*s2X + (nY-1)*s2Y)
sp = sqrt(s2p)
t = (mean(X) - mean(Y)) / sp / sqrt (1/nX + 1/nY)
t
z = (mean(X)- mean(Y))/sqrt(s2X/nX + s2Y/nY)
z
pval = (pnorm(t))
pval

t_alt = (mean(Y) - mean(X)) / sp / sqrt (1/nX + 1/nY)
t_alt

1 - pnorm(t_alt)

#' Il pvalue è molto elevato. Non c'è evidenza per affermare che il peso
#' dei pinguini maschi sia superiore al peso dei pinguini femmina.

