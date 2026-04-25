#######################
##### Esercizio 1 #####
#######################
##### Problema 4, 6/2/2007, Statistica Applicata
#####-------------------------
#' Il file Pb4.txt riporta il numero Y (espresso in migliaia di unità)
#' di veicoli immatricolati annualmente in tre paesi dell'Unione europea
#' (Francia, Germania e Italia) durante un periodo di riferimento di 10 anni.
#' Modelli economici recenti descrivono il comportamento di questa variabile secondo
#' il modello:
#' Y | (X = x, G = g) = beta0_g + beta1_g * x^2 + eps
#' con eps ~ N (0, sigma ^ 2), x = 1, 2,. . . , 10 (anni) e
#' g = Francia, Germania, Italia (EU country).
#' (a) Stimare i 7 parametri del modello (SETTE: 3 intercette (beta0_g), 3 pendenze (beta1_g) e sigma^2)
#' (b) utilizzando test statistici appropriati, dichiarare se si ritiene necessario
#'     includere nel modello:
#'     1. la variabile x^2;
#'     2. la variable G;
#'     3. l'effetto della variabile G sul coefficiente che moltiplica il
#'        regressore x^2;
#'     4. l'effetto della variabile G sull'intercetta
#' (c) Una volta identificato il "miglior modello", costruire tre intervalli di previsione
#'     per il numero di veicoli registrati nei tre paesi durante l'undicesimo anno,
#'     in modo che le tre nuove osservazioni cadranno simultaneamente all'interno 
#'     dei rispettivi intervalli con il 95% di probabilità.

rm (list = ls())
graphics.off()
cat("\014")
data = read.table("Pb4.txt")
str(data)

# Per prima cosa aggiusto il dataframe in modo da avere sia la categorica 
# sia il regressore descritto nel testo (x = 1:10 anni) come colonne.
# Ordino le Y fornite in "Pb4.txt" mettendo prima i valori della Francia (prima colonna),
# poi quelli della Germania (seconda colonna) e infine quelli dell'Italia (terza colonna).
# Procedo quindi come segue:
Y = c(data$Francia, data$Germania, data$Italia)
cat = c(rep("Francia",10), rep("Germania", 10), rep("Italia",10))
year = rep(1:10, 3)
# creo un nuovo data.frame con queste colonne per poi poterlo utilizzare in lm()
data = data.frame ( veicoli = Y, 
                    anno = year, 
                    paese = cat   )
# specifico che paese è da intendersi come variabile categorica
data$paese = factor(data$paese) 
# Ora posso utilizzare il nuovo dataframe per rispondere ai quesiti richiesti

# (a) Stimare i 7 parametri del modello (SETTE: 3 intercette (beta0_g), 3 pendenze (beta1_g) e sigma^2)
# costruisco il modello
first = lm( veicoli~ I(anno^2) + paese + paese:I(anno^2), data = data)
summary(first)

coef(first)
beta0.Francia = coef(first)[[1]]
beta1.Francia = coef(first)[[2]]

beta0.Germania = coef(first)[[1]] + coef(first)[[3]]
beta1.Germania = coef(first)[[2]] + coef(first)[[5]]

beta0.Italia = coef(first)[[1]] + coef(first)[[4]]
beta1.Italia = coef(first)[[2]] + coef(first)[[6]]

beta0.Francia
beta1.Francia
beta0.Germania
beta1.Germania
beta0.Italia
beta1.Italia

sigma2_hat = sum(first$residuals^2)/first$df.residual
sigma2_hat

#' Ricorda: I df.residual sono i gradi di libertà dei residui, cioè n-p-1.
#'          In questo caso, n=30 e p=5 (i due beta_0 corrispondenti alle dummies di Germania e Italia
#'          e i 3 beta_1. Il beta0 della baseline non lo contiamo perché è l'intercetta complessiva
#'          del modello).
first$df.residual
30 - 5 - 1
#' Tutto torna!

#' (b) utilizzando test statistici appropriati, dichiarare se si ritiene necessario
#'     includere nel modello:
#'     1. la variabile x^2;
#'     2. la variable G;
#'     3. l'effetto della variabile G sul coefficiente che moltiplica il
#'        regressore x^2;
#'     4. l'effetto della variabile G sull'intercetta

summary(first)

#' I risultati del summary sono da considerarsi affidabili solo se sono rispettate
#' le ipotesi di normalità e omoschedasticità.
#' Facciamo una diagnostica sui residui:
shapiro.test(first$residuals)
x11()
par(mfrow = c(2,2))
plot(first)
#' Il test di shapiro ci dice che i residui sono gaussiani (p-value alto, non c'è evidenza per rifiutare H0,
#' in H0 c'è che i dati sono gaussiani).
#' Guardando i due grafici a sinistra possiamo controllare l'eventuale presenza di eteroschedasticità.
#' I residui non si comportano in modo perfettamente omogeneo lungo l'asse delle ascisse, ma questo è anche
#' dovuto al fatto che in corrispondenza di valori alti di fitted values ci sono pochi dati.
#' Tutto sommato, possiamo dire con sufficiente sicurezza di non essere in presenza di una
#' deviazione patologica dall'omoschedasticità. Possiamo quindi considerare come affidabili i risultati
#' del summary.

#' 
#' 1 -> Si (pvalue basso)
#' 2 -> Si (ad esempio su anno^2 c'è un effetto significativo dei gruppi)
#' 3 -> Si (pvalue bassi)
#' 4 -> I pvalue sono alti entrambi (su paeseGermania e paeseItalia), tuttavia non abbiamo
#' gli strumenti matematici per fare inferenza su due parametri contemporaneamente:
#' potrebbe capitare che, tolto un parametro, l'altro diventi significativo.

#' Per controllare se ci sia un impatto del paese sull'intercetta, potremmo provare
#' ad accorpare i due paesi non di baseline (Italia e Germania), e vedere se la nuova variabile dummy
#' che si genera corrisponda ad un coefficiente significativo.
#' NB: la nuova variabile dummy è inclusa solo per l'intercetta! Dal summary vediamo che il 
#' raggruppamento dei paesi originale ha un impatto sulla pendenza della retta, quindi questa parte
#' non la vogliamo cambiare.

data['paese2'] = NA
data$paese2[data$paese == "Italia"] = "ItaliaGermania"
data$paese2[data$paese == "Germania"] = "ItaliaGermania"
data$paese2[data$paese == "Francia"] = "Francia"
second = lm( veicoli~ I(anno^2) + paese2 + paese:I(anno^2), data = data)
summary(second)
#' Vediamo comunque che non c'è un impatto dei paesi sull'intercetta.

#' (c) Una volta identificato il "miglior modello", costruire tre intervalli di previsione
#'     per il numero di veicoli registrati nei tre paesi durante l'undicesimo anno,
#'     in modo che le tre nuove osservazioni cadranno simultaneamente all'interno 
#'     dei rispettivi intervalli con il 95% di probabilità.
reduced = lm( veicoli~ I(anno^2) + paese:I(anno^2), data = data)
summary(reduced)
to_pred = data.frame( anno = c(11,11,11) , paese = c("Francia", "Germania", "Italia"))
to_pred$paese = factor (to_pred$paese)
alpha = 0.05
predict(object = reduced,newdata = to_pred,interval = "prediction",level = 1 - alpha/3)
#' Stiamo costruendo tre intervalli contemporaneamente! Bisogna includere la correzione di Bonferroni!
#' Per questo alpha viene diviso per 3.


#######################
##### Esercizio 2 #####
#######################
##### Problema 4 of 4/7/2007
#####-------------------------
# Nelle acciaierie Tenaris, è in fase di studio la relazione tra la lunghezza [m] 
# e la temperatura [°C] di alcune barre d'acciaio che verranno vendute alla Pirelli 
# (i dati sono contenuti nel file "tenaris.txt"). 
# Si ipotizza che la relazione sia del tipo:
# lunghezza = L0 + C * temperatura + D * temperatura^2 + eps
# dove:
# - lunghezza è la lunghezza della barra
# - temperatura è la temperatura della barra
# - L0 è la lunghezza della barra a 0 °C
# - C è il coefficiente di espansione termica lineare
# - D è il coefficiente di espansione termica quadratico
# - eps è un errore di misura a media nulla.
# Rispondi alle seguenti domande utilizzando argomentazioni statistiche appropriate:
# a) Stima i parametri L0, C, D e la varianza dell'errore eps.
# b) Sulla base dell'analisi dei residui, ritieni che ci siano le condizioni per effettuare 
#    inferenze sui coefficienti basandoti su un modello gaussiano?
#    (In caso affermativo, procedi al punto (c); in caso negativo, identifica il problema, 
#    risolvilo e torna al punto (a)).
# c) Ritieni che il modello spieghi la possibile dipendenza tra la temperatura e la lunghezza?
# d) Ritieni che sia possibile eliminare dal modello il termine quadratico?

ten <- read.csv('tenaris.txt')
str(ten)


# a) Stima i parametri L0, C, D e la varianza dell'errore eps.
fit <- lm(lunghezza ~ temperatura + I(temperatura^2), data = ten)
summary(fit)
# L0 = 2.72160
# C = 0.51342
# D = 0.04417

sigma2_hat = sum(fit$residuals^2) / fit$df.residual
sigma2_hat #1.124481 

# b) Sulla base dell'analisi dei residui, ritieni che ci siano le condizioni per effettuare 
#    inferenze sui coefficienti basandoti su un modello gaussiano?
#    (In caso affermativo, procedi al punto (c); in caso negativo, identifica il problema, 
#    risolvilo e torna al punto (a)).

shapiro.test(fit$residuals)
#P value molto basso -> rigetto H0 -> I residui non sono normali
x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()
#Osservo un outlier (riga 1) sia dal grafico dei residui sia dalla distanza di Cook

x11()
plot(ten$temperatura,ten$lunghezza)
points(ten[1,"temperatura"],ten[1,"lunghezza"],pch=19, col="red")
dev.off()

# Remove the outlier
ten1 <- ten[-1,]
fit <- lm(lunghezza ~ temperatura + I(temperatura^2), data=ten1)
summary(fit)

sigma2_hat = sum(fit$residuals^2) / fit$df.residual
sigma2_hat #0.6829185 

shapiro.test(fit$residuals)
x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()
#' Il test di shapiro ci dice che i residui sono gaussiani (p-value alto, non c'è evidenza per rifiutare H0).
#' Guardando i due grafici a sinistra possiamo coonfermare l'omoschedasticità dei residui.
#' Possiamo quindi considerare come affidabili i risultati del summary.

# c) Ritieni che il modello spieghi la possibile dipendenza tra la temperatura e la lunghezza?
x11()
plot(ten1$temperatura,ten1$lunghezza)
points(ten1$temperatura,fitted(fit),col='blue', pch=19, type="l")
dev.off()

#' Si, il modello sta spiegando l'associazione tra la temperatura e la lunghezza 
#' (lo possiamo vedere graficamente plottando la retta di regressione e guardando il summary).
summary(fit)$adj.r.squared #0.9277484 --> R^2 adj molto alto

# d) Ritieni che sia possibile eliminare dal modello il termine quadratico?

#' Sì, possiamo eliminare il termine quadratico dal modello, poiché non è 
#' statisticamente significativo (p-value 0.287). 
#' Un modello più semplice con solo il termine lineare potrebbe essere più 
#' interpretabile e altrettanto efficace nel descrivere la relazione tra temperatura e lunghezza.
fit <- lm(lunghezza ~ temperatura , data=ten1)
summary(fit)
#Adjusted R-squared:  0.9275 

shapiro.test(fit$residuals)
x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()
#Diagnostica dei residui ok.

x11()
plot(ten1$temperatura,ten1$lunghezza)
points(ten1$temperatura,fitted(fit),col='blue', pch=19, type="l")
dev.off()


#######################
##### Esercizio 3 #####
#######################
##### Problema 2 del 28/2/2007, Statistica Applicata
#####--------------------------------------
# Il dataset Pb2.txt mostra la temperatura media mensile (° C) registrata nel
# 2006 in tre località canadesi: Edmonton, Montreal e Resolute. 
# È comune in meteorologia assumere che le temperature medie mensili 
# fluttuino sinusoidalmente intorno a un valore medio annuale:
# Temp.g (t) = beta0.g + beta1.g * sin (2pi / 12 * t) +  beta2.g * cos (2pi / 12 * t) + eps
# con eps ~ N (0,2), t = 1, 2, 3,. . . , 12 (mese) e g = Edmonton, Resolute, Montreal (località).
# (a) Usando il metodo dei minimi quadrati si stimino i 10 parametri del modello
# (b) Verificare le ipotesi del modello.
# (c) Sfruttando la nota relazione trigonometrica
# sin(alfa-beta) = sin(alfa) * cos(beta) - cos(alfa) * sin(beta)
# e reinterpretando il modello della forma:
# Tempg (t) = mu.g + A.g * sin (2pi / 12 * (t-phi.g)) + eps
# riportare la relazione analitica tra i nuovi parametri
# (mu.g, A.g, phi.g) e i vecchi parametri (beta0.g, beta1.g, beta2.g).
# (d) Stimare i parametri della nuova formulazione, cioè:
# - I valori medi annuali (μ.g).
# - Le ampiezze di oscillazione (A.g).
# - Le fasi delle oscillazioni (phi.g).
# (e) Giustificare la possibilità di utilizzare un modello ridotto
# che assuma che le oscillazioni abbiano stessa ampiezza e fase, ma 
# medie annuali diverse nelle stazioni di Edmonton e Montreal.

rm(list = ls())
graphics.off()
cat("\014")

## Preparazione del dataset
data = read.table("Pb2.txt")
str(data)

# Assemblo il dataframe
mese = rep(1:12,3) # ripeti la sequenza di numeri da 1 a 12, per tre volte
names(data)
localita = c(rep("Edmonton",12),rep("Resolute",12), rep("Montreal",12))
#' Significato del comando:
#' rep("Edmonton",12) --> ripeti la stringa "Edmonton" per 12 volte
#' c(rep(), rep(), rep()) --> incolonnami i tre vettori che creo col comando rep

data_for_lm = data.frame(temp = c(data$Edmonton,data$Resolute,data$Montreal), localita, mese)
#' Ho creato un dataframe con tre colonne:
#' - temp --> contiene i valori di temperatura media mensile per ciascuna delle tre località, incolonnati
#'            uno dopo l'altro
#' - localita --> è il vettore che abbiamo creato prima, che in ogni riga contiene il nome della località
#'                a cui si riferisce il valore medio di temperatura in quella riga
#' - mese --> per ciascuna riga, mi indica il mese a cui si riferisce il valore medio di temperatura
#'            in quella riga

data_for_lm$localita = factor(data_for_lm$localita)
str(data_for_lm)

## (a) Usando il metodo dei minimi quadrati si stimino i 10 parametri del modello
## Prepariamo i regressori in modo da avere un summary più leggibile
data_for_lm$x1 = sin(pi*data_for_lm$mese/6)
data_for_lm$x2 = cos(pi*data_for_lm$mese/6)
first = lm (temp ~ x1 + x2 + localita + localita:x1 + localita:x2, data = data_for_lm )
summary(first)

coef(first)

beta0.ed = coef(first)[[1]]
beta0.mon = coef(first)[[1]] + coef(first)[[4]] 
beta0.Res = coef(first)[[1]] + coef(first)[[5]] 

beta1.ed = coef(first)[[2]]
beta1.mon = coef(first)[[2]] + coef(first)[[6]] 
beta1.Res = coef(first)[[2]] + coef(first)[[7]]

beta2.ed = coef(first)[[3]]
beta2.mon = coef(first)[[3]] + coef(first)[[8]] 
beta2.Res = coef(first)[[3]] + coef(first)[[9]]

beta0.ed
beta0.mon
beta0.Res

beta1.ed
beta1.mon
beta1.Res

beta2.ed
beta2.mon
beta2.Res

sigma2_hat = sum(first$residuals^2) / first$df.residual
sigma2_hat

#  (b) Verificare le ipotesi del modello.

#' Normalità dei residui
shapiro.test(first$residuals)
#' Il p-value è molto alto. Non c'è evidenza per rifiutare H0 (in cui è assunta la normalità dei residui).
#' Concludo che i residui sono normali.

par(mfrow=c(2,2))
plot(first)
#' Non sembra che i nostri residui siano pienamente omoschedastici,
#' e lo si vede dal grafico in basso a sx che mostra una decrescita della variabilità
#' dei residui standardizzati rispetto ai fitted values.
#' 
#' Andiamo a indagare meglio i motivi che portano all'eteroschedasticità in questo caso,
#' controllando il comportamento dei residui sia rispetto ai mesi (ricorda: x1 e 
#' x2 sono ottenute come trasformazioni sinusoidali dei mesi) che rispetto alla località.

x11()
par(mfrow=c(1,2))
plot(data_for_lm$mese,first$residuals, col=data_for_lm$localita, pch=16)
plot(data_for_lm$localita,first$residuals, col=data_for_lm$mese, pch=16)

#' Se guardiamo il comportamento dei residui rispetto ai mesi, si osserva come si abbiano tre
#' osservazioni per ogni mese (ognuna relativa alla sua località), e come i residui siano ben centrati
#' in 0 e presentino variabilità costante rispetto ai mesi. I residui sono omoschedastici rispetto ai mesi.
#' Notiamo che i residui presentano maggiore variabilità in corrispondenza della località
#' Resolute. Controlliamo con un test sulle varianze se ci sia evidenza per affermare che le varianze
#' dei residui siano diverse nelle tre località (detto meglio: che almeno una località abbia residui con
#' varianza diversa rispetto agli altri).

# Prima di fare il test, controlliamo che i residui siano normali anche dentro ai gruppi
tapply(first$residuals, data_for_lm$localita, shapiro.test)
# Lo sono, facciamo il test sull'omogeneità delle varianze
bartlett.test(first$residuals ~ data_for_lm$localita)
#' pvalue 0.037, basso ma non infimo.
#' Possiamo concludere, seppur con qualche riserva, che i residui siano omoschedastici.

# (c)
## sin(pi/6*(t-phi)) = sin(pi/6*t) * cos(pi/6*phi)  - cos(pi/6*t) * sin(pi/6*phi) = x1 * cos(pi/6*phi) - x2 *  sin(pi/6*phi)
## Quindi beta1.g = A.g *  cos(pi/6*phi.g) , beta2.g = - A.g *  sin(pi/6*phi.g) da cui
## phi.g = - 6/pi * atan(beta2.g/beta1.g)
## A.g = sqrt ( beta1.g ^ 2 + beta2.g^2)
## mu.g = beta0.g

# (d)
Coefs = cbind(c(beta0.ed, beta0.mon, beta0.Res), c(beta1.ed, beta1.mon, beta1.Res), c(beta2.ed, beta2.mon, beta2.Res) ) 
row.names(Coefs) = c("Edm", "Mon", "Res")
Coefs

NewCoefs = data.frame( mu = Coefs[,1], A = sqrt(Coefs[,2]^2 + Coefs[,3]^2), phi = -6/pi*atan(Coefs[,3]/Coefs[,2]))
row.names(Coefs) = c("Edm", "Mon", "Res")
NewCoefs

# (e) Giustificare la possibilità di utilizzare un modello ridotto
# che assuma che le oscillazioni abbiano stessa ampiezza e fase, ma 
# medie annuali diverse nelle stazioni di Edmonton e Montreal.

summary(first)

#' Guardando i p-values, ridurre il primo modello in tal senso sembra sensato, poiché
#' i p-values delle interazioni sia di Montreal che di Edmonton con x1 e x2 sono alti.
#' Siccome beta1.g e beta2.g e determinano l'ampiezza e la fase A.g e phi.g, se non c'è
#' evidenza per dire che beta1.Montreal sia diverso da beta1.Edmonton, e che beta2.Montreal
#' sia diverso da beta2.Edmonton, allora non c'è evidenza per dire che A.Montreal sia diversa
#' da A.Edmonton e che phi.Montreal sia diversa da phi.Edmonton.
#' 
#' Per essere proprio sicuri, prima di procedere alla riduzione bisognerebbe:
#' 1. Accorpare le stazioni di Edmonton e Montreal e controllare se l'interazione con x2 sia
#'    significativa. Se non lo è, rimuovere l'interazione.
#' 2. Controllare se l'interazione di x1 con le stazioni di Edmonton e Montreal accorpate sia
#'    significativa. Se no, allora utilizzare il modello ridotto è giustificato.
#' In questo caso non era richiesto un approfondimento di questo tipo, ma era sufficiente
#' guardare i p-value e trarre delle conclusioni su cosa suggerissero.
#' 

new.data = data_for_lm
new.data['localita2'] = NA
new.data$localita2[data_for_lm$localita == "Montreal"] = "EdmontonMontreal"
new.data$localita2[data_for_lm$localita == "Edmonton"] = "EdmontonMontreal"
new.data$localita2[data_for_lm$localita == "Resolute"] = "Resolute"
new.data$localita2 = as.factor(new.data$localita2)

second = lm (temp ~ x1 + x2 + localita + localita:x1 + localita2:x2, data = new.data )
summary(second)

third = lm (temp ~ x1 + x2 + localita + localita2:x1 + localita2:x2, data = new.data )
summary(third)

fourth = lm (temp ~ x1 + x2 + localita + localita2:x1, data = new.data )
summary(fourth)
