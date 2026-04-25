#' ##################################################################
#' ####### Inclusione delle Variabili Categoriche nei modelli #######
#' ##################### di regressione lineari #####################
#' ##################################################################

####### Modello di regressione lineare semplice con un predittore categorico ############
rm(list = ls())
graphics.off()
cat("\014")

# Importiamo il dataset, siamo interessati
# all' Average score come funzione di anni di servizio
# e sesso
work = read.csv('work.txt', sep=" ")
work$Sex = as.factor(work$Sex)
str(work)
x11()
plot(work$Years_Service,work$Average_Score, main='Scatterplot di Y vs X', lwd=2, 
     xlab='Years of Service', ylab='Average Score', col = ifelse(work$Sex == "Male", "blue","pink"))

# vogliamo includere anche la variabile categorica Sesso in un modello lineare che
# metta in relazione Average Score con Years_Service.

## Come modellare le variabili categoriche nel modello?
## Matematicamente possiamo scrivere, se le variabili si dividono in K gruppi: 

## y_ig = beta_0g + beta_1g * x1_ig + beta_2g * x2_ig + eps_ig, con Var(eps_ig) = sigma^2, ogni i, g = 1,...K

## Come modellare tutto in un unico modello, stimando sigma^2 usando tutta l'informazione disponibile?
## Prendiamo il primo gruppo come riferimento, così che per il primo gruppo valga il modello

## y_i1 = beta_01 + beta_11 * x1_i1 + beta_21 * x2_i1 + eps_i1. 

## Poi per un generico gruppo g diverso dal primo, definiamo la variabile dummy d_jg = 1 se la j-esima osservazione è nel gruppo g, d_jg = 0 altrimenti.
## Scriviamo quindi il modello generale per un'osservazione j qualsiasi:

## y_j = beta_01 + beta_11 * x1_j + beta_21 * x2_j + beta_02 * d_j2 + beta_12 * x1_j  * d_j2  + beta_21 * x2_j * d_j2 + .... + beta_0K * d_jK + beta_1K * x1_j  * d_jK  + beta_2K * x2_j * d_jK

## Così, in un unico modello, otteniamo in realtà K modelli lineari diversi: 
## Per il primo gruppo, il modello è proprio 

## y_i1 = beta_01 + beta_11 * x1_i1 + beta_21 * x2_i1 + eps_i1

## Per ogni altro gruppo avremo

## y_ig = (beta_01 + beta_0g)  + (beta_11 + beta_1g) * x1_ig + (beta_21 + beta_2g) * x2_ig + eps_ig


## Nota: se ci sono K gruppi, abbiamo quindi bisogno di K-1 variabili dummy!

## In questo caso abbiamo due gruppi, quindi abbiamo bisogno di un nuovo vettore che abbia 0 in corrispondenza del primo gruppo e 1 in corrispondenza del secondo

# Costruiamo una variabile "dummy", ovvero una variabile binaria (solo valori 0 o 1) che
# codifica l'appartenenza ad una categoria.
# La variabile avrà infatti valore 1 se l'osservazione è della categoria maschi, 0 altrimenti (femmine).

work$dummy = ifelse(work$Sex == "Male",1,0) #definisco la dummy e la inserisco nel dataset

# fittiamo il modello
model = lm(Average_Score ~ dummy + Years_Service + I(dummy*Years_Service), data = work)
summary(model)


# Sembra che l'interazione tra Years e la variabile relativa al sesso non sia significativa (cambia l'intercetta
# ma non la pendenza).
# Costruiamo quindi un modello ridotto, da cui escludiamo l'interazione
model = lm(Average_Score ~ dummy + Years_Service, data = work)
summary(model)

# interpretazione del modello:
# modello per le donne:         Y = 7.035 + 0.097 X + eps
# modello per gli uomini:       Y = 7.035 - 2.59 + 0.097 X = 4.44 + 0.097 X + eps

coefficienti = model$coefficients
beta_0_donne = coefficienti[1]
beta_1_donne = beta_1_uomini =  coefficienti[3]
beta_0_uomini = coefficienti[1] + coefficienti[2]

x11()
plot(work$Years_Service, work$Average_Score, main='Scatterplot di Y vs X', lwd=2, 
     xlab='Years of Service', ylab='Average Score', col = ifelse(work$Sex == "Male", "blue","pink"))
abline(beta_0_donne,beta_1_donne,lwd=2, col = "pink")
abline(beta_0_uomini, beta_1_uomini, lwd = 2, col = "blue")
# diagnostica dei residui
x11()
par(mfrow=c(2,2))
plot(model)

shapiro.test(model$residuals)

graphics.off()

## In R non è necessario definire le dummy come abbiamo fatto! è più semplice e pulito
## procedere così:

model = lm(Average_Score ~ Sex + Years_Service + Sex:Years_Service, data = work) #notare la sintassi
summary(model) # il gruppo 1 è scelto arbitrariamente, ma il summary è chiaro, ed in questo caso
               # è identico
# diagnostica dei residui
x11()
par(mfrow=c(2,2))
plot(model)
shapiro.test(model$residuals)

####### Modello di regressione lineare multipla con piu' predittori categorici ############

# Ripartiamo dal nostro esempio precedente sul dataset 'Work'.
# Questa volta vogliamo considerare sia il sesso che il livello di educazione. Passiamo direttamente
# all'implementazione in R senza definire le dummy a mano

work = read.csv('work.txt', sep=" ")
work$Sex = as.factor(work$Sex)
work$EduLevel = as.factor(work$EduLevel)
str(work)

## Assemblo i colori per fare i grafici:
colors = rep(NA, 20)
colors[work$Sex == "Female" & work$EduLevel == "Master"] = "orange"
colors[work$Sex == "Female" & work$EduLevel == "Bachelor"] = "indianred"
colors[work$Sex == "Male" & work$EduLevel == "Bachelor"] = "forestgreen"
colors[work$Sex == "Male" & work$EduLevel == "Master"] = "lightgreen"
x11()
plot(work$Years_Service,work$Average_Score, main='Scatterplot di Y vs X', lwd=2, 
     xlab='Years of Service', ylab='Average Score', col = colors, pch = 19, cex = 1.5)
legend('bottomright', legend=c('Female+Master','Female+Bachelor','Male+Master','Male+Bachelor'),
       col=c('orange','indianred','lightgreen','forestgreen'), lwd=2, cex=0.85)


# Voglio fittare una retta di regressione per ogni sottogruppo identificato dalle due categoriche.
# La sintassi R è identica al caso precedente: 

model = lm(Average_Score ~ Sex + EduLevel + Years_Service + Sex:Years_Service + EduLevel:Years_Service, data = work )
summary(model) # Come si interpreta il summary? (vedi anche file riassuntivo)

#Eliminiamo (senza troppa attenzione) gli effetti che sembrano non significativi e disegnamo le rette di regressione
model = lm(Average_Score ~ Sex + EduLevel + Years_Service, data = work )
summary(model)

### diagnostica
x11()
par(mfrow = c(2,2))
plot(model)

shapiro.test(model$residuals)

#Devo calcolare tutti i coefficienti
coefficienti = model$coefficients
beta_1 = coefficienti[4] # Adesso è uguale per tutti
beta_0_donne_bachelor = coefficienti[1]
beta_0_donne_master = coefficienti[1] + coefficienti[3]
beta_0_uomini_bachelor = coefficienti[1] + coefficienti[2]
beta_0_uomini_master = coefficienti[1] + coefficienti[2] + coefficienti[3]


## Disegno le rette
x11()
plot(work$Years_Service,work$Average_Score, main='Scatterplot di Y vs X', lwd=2, 
     xlab='Years of Service', ylab='Average Score', col = colors, pch = 19, cex = 1.5)
legend('bottomright', legend=c('Female+Master','Female+Bachelor','Male+Master','Male+Bachelor'),
       col=c('orange','indianred','lightgreen','forestgreen'), lwd=2, cex=0.85)
abline(beta_0_donne_bachelor, beta_1, lwd = 2, col = "indianred")
abline(beta_0_donne_master, beta_1, lwd = 2, col = "orange")
abline(beta_0_uomini_bachelor, beta_1, lwd = 2, col = "forestgreen")
abline(beta_0_uomini_master, beta_1, lwd=2, col="lightgreen")


## Voglio un intervallo di confidenza e uno di predizione per la risposta in un uomo con master
## con 25 anni di servizio:

alpha = 0.05
newdata = data.frame(Years_Service = 25, Sex = "Male", EduLevel = "Master")
predict(model,newdata,level = 1 - alpha,interval =   "confidence")
predict(model,newdata,level = 1 - alpha,interval = "prediction")

############ Variabile categorica a più livelli: dataset Iris ######################
## Vediamo che le cose sono assolutamente identiche, semplicemente bisogna fare valutazioni
## e calcolo dei coefficienti per ogni gruppo:
fiori = iris
fiori$Species = as.factor(fiori$Species)
str(fiori)
library(GGally)
x11()
pairs(fiori, aes(col = Species))
ggpairs(fiori, aes(col = Species))

#Desideriamo formulare un modello che metta in relazione la lunghezza del Sepalo 
#con la specie del fiore, la lunghezza del Petalo e la larghezza del Petalo. 
#Vogliamo inizialmente permettere sia intercette che coefficienti davanti ai predittori
#diversi da specie a specie:

model = lm(Sepal.Length ~ Species + Petal.Length + Species:Petal.Length + 
             Petal.Width + Species:Petal.Width, data = fiori)
summary(model) # come interpretiamo? come prima, ma stavolta su tre livelli diversi!

#Intercetta per iris 
#per specie Setosa pari a 4.2475, 
#per specie versicolor pari a 4.2475 - 1.867, 
#per specie virginica pari a 4.2475 - 3.1959. 
#Tutte le differenze sono significative. 
#Concludiamo che, ad esempio, a parità di lunghezza e larghezza del petalo, 
#gli iris Setosa hanno in media una lunghezza del sepalo maggiore delle versicolor e delle virginica. 

#Coefficiente davanti alla lunghezza del petalo (Petal.Length)
#per specie Setosa pari a 0.399, 
#per specie versicolor pari a 0.399 + 0.5352, 
#per specie virginica pari a 0.399 + 0.5956. Occorre cautela sulla significatività.

#E cosi via per il coefficiente davanti alla larghezza del petalo (Petal.Width)
#per specie Setosa pari a 0.7121
#per specie versicolor pari a 0.7121-1.0321 = -0.32
#per specie virginica pari a 0.7121-0.7051 = 0.007


####### DIGRESSIONE: Regressione lineare con variabili categoriche e ANOVA ----------------------
#' Supponiamo di non avere regressori, ma di osservare solo una variabile categorica. In questo caso
#' gli unici termini sono le intercette:
#' 
#' y_i = beta_0 + beta_02 * d_i2 + beta_03 * d_i3 + ... beta_0K * d_ik + eps_i, eps_i ~ N(0, sigma^2). 
#' 
#' Se guardiamo il modello ANOVA, che modella
#' 
#' y_ig = mu + tau_g + eps_i,  eps_i ~ N(0, sigma^2), g = 1,...K 
#' 
#' scopriamo che e' esattamente lo stesso modello, con le seguenti relazioni tra parametri:
#' mu + tau_1 = beta_0 (media del primo gruppo)
#' mu + tau_g = beta_0 + beta_0g, g = 2,...K 
#' In altre parole, un modello ANOVA e' un particolare modello di regressione lineare in cui c'e'
#' un solo regressore, e questo e' categorico.

## Esempio con il dataset iris:
# ANOVA
anova_iris = aov(Sepal.Length ~ Species, data = iris)
summary(anova_iris)
# LM
model = lm(Sepal.Length ~ Species, data = iris)
summary(model)
# Confrontiamo i coefficienti
anova_iris$coefficients
model$coefficients


## Esempio con il dataset dei polli:
polli = chickwts
# ANOVA
Anova_polli = aov(weight ~ feed, data = polli)
summary(Anova_polli)
# LM
lm_polli = lm(weight~feed,data = polli)
summary(lm_polli)
# Confrontiamo i coefficienti
Anova_polli$coefficients
lm_polli$coefficients

# Possiamo anche confrontare gli intervalli di confidenza dei coefficienti
alpha = 0.05
confint(lm_polli, level =  1 - alpha/6)
confint(Anova_polli, level =  1 - alpha/6)

#__________________________________________________________________________________________

#' Concludiamo osservando che, in molti casi precedenti, ci troviamo nella situazione 
#' di dover decidere se un predittore, sia esso numerico, categorico, o frutto dell'interazione
#' tra un numerico e un categorico, sia significativo o meno, cosa che possiamo giudicare 
#' immediatamente guardando ai pvalues del summary. Tuttavia, tali pvalues non sono di alcuna 
#' utilità nello stabilire se due predittori possano contemporaneamente essere eliminati.
#' In altre parole, il fatto che in un summary due righe corrispondano a predittori non significativi
#' non significa che siamo autorizzati ad eliminarli entrambi: non è mai possibile "cancellare"
#' due o più righe del summary contemporaneamente solo basandosi sui pvalues.
#' 
#' E' anche per questo motivo che sono necessari metodi di selezione dei predittori più sofisticati
#' di un semplice giudizio basato sui pvalues, come ad esempio i metodi di stepwise selection, 
#' di cross-validazione, o basati su regressione lasso. 
#' I metodi stepwise e la cross validazione sono oggetto dei prossimi script.  
