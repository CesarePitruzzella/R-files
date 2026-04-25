#######################
##### Esercizio 3 #####
#######################
##### Problema 4 del 1/7/2009, Statistica Applicata
#####-------------------------
#' L'Index Librorum Prohibitorum (edizione del 1948), elenca circa 10000 opere 
#' considerate eretiche dalla Chiesa cattolica. Il file 'index.txt'
#' mostra, per gli anni che vanno dal 1300 al 1899, il numero di opere
#' aggiunte annualmente all'Indice. La maggior parte degli storici crede che la media del
#' numero di opere aggiunte ogni anno sia diminuita linearmente nel tempo durante questo
#' periodo (modello A). Recentemente, il Prof. Langdon ha proposto una teoria secondo la quale
#' la tendenza lineare è cambiata "momentaneamente" (in modo discontinuo) 
#' durante il periodo di egemonia francese (1768, trattato di Versailles, 1815, 
#' Battaglia di Waterloo), durante il quale un crollo delle opere annualmente 
#' aggiunte all'indice si è verificato (Modello B). Definendo come μ(t) il numero medio 
#' di opere aggiunte all'Indice nell'anno t, e formalizzando i due modelli 
#' come segue:
#' Modello A: μ(t) = alfa + beta * t;
#' Modello B: μ(t) = alpha1 + beta1 * t per 1768 <= t <= 1815
#'            μ(t) = alfa2 + beta2 * t per t <= 1767 o t> = 1816;
#' Rispondere alle seguenti domande:
#' a) stimare i parametri di entrambi i modelli usando il metodo dei minimi quadrati,
#'    dopo avere enunciato le opportune ipotesi.
#' b) c'è evidenza statistica di un diverso trend lineare nel periodo di
#'    egemonia francese? Scegliere tra modello A e B.
#' c) usando il modello (b), fornire 2 intervalli di confidenza al 90%,  per la media
#'    del numero di opere incluse nell'indice nell'anno 1800;


index = read.table("index.txt")
str(index)

# Punto a)
#' Ipotesi (qui si chiedeva solo di enunciarle):
#' - normalità dei residui
#' - omoschedasticità dei residui
#' - quando dividerò le osservazioni nei due gruppi, in base al tempo dell'osservazione,
#'   lavorerò sotto l'ipotesi aggiuntiva che anche l'ipotesi che la varianza dei residui
#'   nei due gruppi sia uguale
modelloA = lm(Numero ~ Anno, data = index)
summary(modelloA)

alpha = coef(modelloA)[1] # uguale a modelloA$coefficients[1]
beta = coef(modelloA)[2]
alpha
beta

sigma2_hatA = sum(modelloA$residuals^2)/modelloA$df.residual
sigma2_hatA

## Costruisco la dummy per dividere in gruppi
index$dummy = as.integer(index$Anno >= 1768 & index$Anno <=1815)
#' (index$Anno >= 1768 & index$Anno <=1815) crea un vettore lungo come le nostre osservazioni,
#' che vale TRUE se l'osservazione è relativa all'intervallo temporale [1768;1815], e FALSE altrimenti.
#' as.integer(index$Anno >= 1768 & index$Anno <=1815) trasforma tutti i TRUE in 1 e i FALSE in 0.

## Costruisco il modello B
modelloB = lm(Numero ~ Anno + dummy + dummy*Anno, data = index)
summary(modelloB)

alpha1 = coef(modelloB)[1] + coef(modelloB)[3]
beta1 = coef(modelloB)[2] + coef(modelloB)[4]

alpha2 = coef(modelloB)[1]
beta2 = coef(modelloB)[2]

alpha1
beta1
alpha2
beta2

# Punto (b)
summary(modelloB)
#' Il pvalue su Anno:dummy è basso, c'è evidenza per affermare che c'è un diverso trend lineare
#' (beta1 è diverso da beta2, facendo riferimento alla notazione che il testo dell'esercizio
#' usa per definire i due modelli.)

summary(modelloA) 
#' Il modello B porta ad un consistente aumento di R^2, e ha tutti i parametri significativi

shapiro.test(modelloB$residuals)
x11()
par(mfrow = c(2,2))
plot(modelloB)

#' Ci sono molti più dati in corrispondenza di valori alti di fitted values, ma questo
#' non ha un impatto negativo sull'omoschedasticità. Non si può dire, guardando i due plot a
#' sinistra, che la varianza dei residui non sia costante lungo l'asse delle ascisse. E' ragionevole
#' quindi assumere che i residui siano omoschedastici.
#' Inoltre, sia il QQ plot in alto a dx che lo shapiro test non ci portano a rifiutare la normalità
#' dei residui.

# Punto (c)
#' 1800 appartiene all'intervallo [1768;1815]

new.data = data.frame(Anno = 1800, dummy = 1)
predict(modelloB,newdata = new.data, interval = "confidence",level = 0.9)
predict(modelloB,newdata = new.data ,interval = "prediction",level = 0.9)
