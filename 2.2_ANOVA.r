########################### ANOVA ###################################
#' Vediamo come affrontare in R il problema dell'analisi di varianza univariato
#' (one way ANOVA).
#' A livello modellistico, abbiamo g popolazioni (normali o numerose) 
#' con media mu_j e varianza sigma^2_j, j = 1,... g.
#' Osserviamo per ogni j = 1,...g un campione casuale X_ji, i = 1, ... n_j.
#' Sulla base del campione osservato, dobbiamo valutare il test
#' H_0 : mu_j = mu per ogni j  VS H_1 : esiste almeno una coppia di medie diverse

rm(list = ls())
graphics.off()
cat("\014")

# Cominciamo con un semplice e famoso dataset, costituito da osservazioni sul peso dei polli
dati_polli = chickwts
str(dati_polli)

# Ciò che vogliamo capire è:
# il nutrimento dato ai polli ha influenza sul loro peso medio?

# Esploriamo i dati
x11()
boxplot(dati_polli$weight~dati_polli$feed)

# Nota: specie in presenza di pochi dati, è bene accertarsi che le classi siano ragionevolmente bilanciate. 
table(dati_polli$feed)
# ok, le classi sono bilanciate

#' Vediamo subito che procedere con l'ANOVA potrebbe aver senso
#' Siccome abbiamo POCHI DATI, occorrono le IPOTESI AGGIUNTIVE:
#' (1) Tutte le distribuzioni sono normali
#' (2) Tutte le varianze sono uguali

#' (1) Tutte le distribuzioni sono normali?
#' Verifichiamolo con il test di shapiro
tapply(dati_polli$weight, dati_polli$feed, shapiro.test)
#' la funzione tapply suddivide i valori di una variabile quantitativa 
#' per i livelli di una variabile qualitativa e, per ogni livello, 
#' applica una particolare funzione (ad esempio: mean, max, shapiro.test,...) sui dati


#' (2) Tutte le varianze sono uguali?
#' Verifichiamolo con il Bartlett's test.
#' Il test di Bartlett è un test di omogeneità delle varianze.
#' NB: Non vi è richiesto di conoscere le caratteristiche del test di Bartlett,
#' ma soltanto di saper interpretare il p-value.
#' Il Bartlett test produce un pvalue basso nel caso in cui ci sia evidenza per affermare che 
#' almeno uno dei gruppi abbia varianza diversa dagli altri
bartlett.test(dati_polli$weight ~ dati_polli$feed)
# Anche qui sembra che le cose vadano bene 


#' Le ipotesi sono verificate, possiamo procedere con l'ANOVA
#' Ricordiamo la parametrizzazione mu_j = mu + tau_j , con tau_1 = 0
#' Quindi il modello è X_ji = mu + tau_j + epsilon_ij, epsilon_ij ~ N(0, sigma^2)
#' H0: tau_j = 0 per ogni j VS H1: esiste j tale che tau_j!=0

Anova_polli = aov(weight ~ feed, data = dati_polli)
summary(Anova_polli)
# Il test rifiuta decisamente H_0 (p-value basso).
# c'è evidenza per affermare che almeno uno dei gruppi abbia media diversa dagli altri
# prima riga Df = g-1 
# seconda riga Df = n-g 
# dove g=gruppi, n=osservazioni

# Vediamo se effettivamente R utilizza il vincolo tau_1 = 0
tapply(dati_polli$weight, dati_polli$feed, mean)
Anova_polli$coefficients
# Il primo coefficiente rappresenta la media del primo gruppo

# Gli altri coefficienti rappresentano gli effetti tau_j, ovvero la differenza tra la 
# media del gruppo j e la media del primo gruppo.

# La stima della varianza comune si ricava facilmente dalla somma delle varianze nei gruppi
S = sum(Anova_polli$residuals ^ 2) / Anova_polli$df.residual
S

####### INTERVALLI ----------------------------------------------------------------
# Abbiamo rifiutato H0 ma non sappiamo quale gruppo è diverso.
# Facciamo gli intervalli di confidenza delle medie e li confrontiamo
# per capire quale cibo è effettivamente meglio degli altri

alpha = 0.05
# Possiamo costruire intervalli di confidenza multipli per tutte le differenze, 
# e calcolare i rispettivi pvalues

# Artigianalmente usiamo la stima di S ottenuta dall'ANOVA
# (se interessati potete consultare il codice della funzione)
source("Confronti_multipli_da_anova.R")
# Questa funzione fornisce gli intervalli di confidenza cercati, i p-value relativi al 
# t-test di uguaglianza tra le medie ed un oggetto grafico per visualizzare i risultati

intervalli_anova = Confronti_multipli_da_anova(Anova_polli, alpha = alpha)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

### Correggiamo con Bonferroni ###
# Se voglio che il livello di confidenza globale sia (1-alpha), 
# ed ho g gruppi, gli intervalli di confidenza
# dovranno avere livello 1-alpha/g, quindi gli intervalli saranno più grandi

intervalli_anova = Confronti_multipli_da_anova(Anova_polli, alpha = alpha,bonferroni = TRUE)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

# In questo caso gli intervalli sono ovviamente più larghi e i pvalues più alti: 
# infatti, il livello di ogni intervallo è adesso 0.05/15, 
# mentre i corrispondenti pvalue sono invece stati moltiplicati per 15.
