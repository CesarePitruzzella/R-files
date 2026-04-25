rm(list = ls())
cat("\014")
graphics.off()
getwd()

# Potete sostituire qui il percorso con quello in cui avete salvato la cartella 
# 'Esercitazione 2' (decompressa) per impostare la directory di lavoro
setwd("C:/Users/user/Desktop/EseBDA-2425/Esercitazioni/Esercitazione 2")

############# VARIABILI ALEATORIE E DISTRIBUZIONI ##############################
# Statistica e Teoria della Probabilità sono intimamente connesse. Pertanto, R fornisce metodi
# per far di conto con la probabilità. Supponiamo che X ~ F(params), ovvero che X abbia una 
# distribuzione F nota dipendente da (uno o più) parametri params. 
# La distribuzione è identificata da una qualsiasi tra le tre funzioni seguenti:

# (1) Funzione di ripartizione F(t) = P(X <= t)
# (2) Funzione Quantile, inversa (generalizzata) di F
# (3) Funzione Densità, ovvero f(t) = P(X = t) con X discreta, o f(t) = F'(t) se X continua

# E' bene avere chiaro il significato di (1) e (3) in ogni caso, e di (2) nel caso di X continua.

# R ci permette di valutare le tre funzioni elencate per una larga famiglia di distribuzioni note. 
# Le distribuzioni disponibili sono riassunte di seguito

#  DISRIBUZIONE                 RIPARTIZIONE  QUANTILE    DENSITA'    
#  Normal	                      pnorm	        qnorm	      dnorm	               
#  Student t	                  pt	          qt	        dt	                 


# COME UTILIZZARE LE FUNZIONI --------------------------------------------------

## a.) NORMALE -----------------------------------------------------------------
# Voglio fare dei conti con la normale di media 2 e varianza 6
mu = 2
sigma2 = 6
# Mi chiedo: quali sono i parametri che vuole R?
# Devo passare la varianza o la deviazione standard?

# Risposta: uso l'help
?pnorm
help(pnorm)

# Scopro che devo passare media e deviazione standard come parametri
sigma = sqrt(sigma2)

# Calcolo la densità in un vettore equispaziato su mu +- 4sigma, con 2000 elementi
t = seq(mu - 4*sigma, mu + 4*sigma, length.out = 2000) # vettore equispaziato
f = dnorm(t, mean = mu, sd = sigma)                    # densità

# Calcolo la funzione di ripartizione
P = pnorm(t, mean = mu, sd = sigma)

# Se voglio i quantili? 
# Mi ricordo che la funzione quantile ha per dominio l'intervallo [0,1], qualunque sia la distribuzione
q = seq(1e-6,1-1e-6,length.out = 1000) # Non metto 0 e 1 per ragioni di stabilità numerica
Q = qnorm(q, mean = mu, sd = sigma)

### Plotto tutto insieme (si noti type='l' per ottenere una linea)
x11()
par(mfrow = c(1,3))
plot(t,f, col = "red", type = "l", lwd = 4, main = "Densità")
abline(v = mu, col = "black", lwd = 2)
plot(t,P, col = "forestgreen", type = "l", lwd = 2, main = "Ripartizione")	
abline(h=c(0,1), col="black", lwd=1, lty=2)
plot(q,Q, col = "blue", type = "l", lwd = 2, main = "Quantile")
abline(v=c(0,1), col="black", lwd=1, lty=2)
graphics.off()


## b.) t di Student ------------------------------------------------------------
gradi = 8 # i gradi di libertà che voglio
t = seq(-6, 6, length.out = 2000)
f = dt(t, df = gradi) # densità

P = pt(t, df = gradi) # funzione di ripartizione

q = seq(1e-6,1-1e-6,length.out = 1000)
Q = qt(q, df = gradi) # quantili

# Mettiamo tutto nello stesso grafico
x11()
par (mfrow = c(1,3)) #Grafico ad una riga, 3 colonne
plot(t,f, col = "red", type = "l", lwd = 2, main = "Densità")
plot(t,P, col = "forestgreen", type = "l", lwd = 2, main = "Ripartizione")
abline(h=c(0,1), col="black", lwd=1, lty=2)
plot(q,Q, col = "blue", type = "l", lwd = 2, main = "Quantile")
abline(v=c(0,1), col="black", lwd=1, lty=2)


#' RIASSUNTO: per fare i conti bisogna
#' (1) Capire quale distribuzione sia utile ai nostri scopi
#' (2) Saper sfruttare R per ottenere valori particolari di:
#'     a) Funzione di ripartizione
#'     c) Funzione quantile
#'     c) Densità
#' 


tinytex::install_tinytex()
