## ANOVA su dataset Iris
## Studiamo un altro famoso dataset, contenente informazioni riguardo misurazioni effettuate su tre specie di Iris
## Facciamo tutto lo studio sulla variabile Sepal.width, per verificare se specie diverse hanno Sepal.width diversa
rm(list = ls())
graphics.off()
library(GGally)
library(tidyverse)
dati = iris
x11()
ggpairs(dati, aes(color = Species, alpha = 0.4))
dati = dati[,c(2,5)]
x11()
ggpairs(dati, aes(color = Species, alpha = 0.4), columnLabels = c())

### Otteniamo un rapido riassunto (provare a ottenerlo "artigianalmente")
indici_dati = dati %>% group_by(Species) %>% summarise(num = n(), media = mean(Sepal.Width),
                                                       var = var(Sepal.Width), 
                                                       s_test = shapiro.test(Sepal.Width)$p.value) 
indici_dati
### Situazione canonica, quasi perfetta: 
### classi perfettamente bilanciate, non c'è evidenza per affermare non normalità, numerosità accettabile

# Vediamo il test di Bartlett
bartlett.test(Sepal.Width ~ Species ,data = dati)


# Non c'è evidenza per affermare che le varianze non siano omogenee. Possiamo applicare l'Anova senza remore
Anova_fiori = aov(Sepal.Width ~ Species ,data = dati)
summary(Anova_fiori)

## C'è evidenza per affermare che almeno una coppia di specie ha media diversa
source("Confronti_multipli_da_anova.R")
intervals_from_anova = Confronti_multipli_da_anova(Anova_fiori,alpha = 0.05, bonferroni = TRUE)
intervals_from_anova$intervals

x11()
plot(intervals_from_anova$plot)

#### Proviamo adesso ad esplorare un dataset più complesso
#### https://www2.stat.duke.edu/courses/Fall15/sta112.01/post/hw/HW1.html contiene una spiegazione del dataset. 
#### Si tratta di un dataset contenente le caratteristiche di oltre 50000 diamanti
rm(list = ls())
dati = diamonds

### Possiamo procedere con un'esplorazione progressiva. Guardiamo prima solo prezzo e peso
x11()
ggpairs(dati,columns = c("carat","price"))

### prezzo, peso, colore 
x11()
ggpairs(dati, columns = c("carat","color","price"), aes(col = color, alpha = 0.3),upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "box_no_facet"))

#prezzo, peso, colore, taglio, brillantezza, depth
x11()
ggpairs(dati, columns = c("carat","color","price","cut","clarity","depth"),upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "box_no_facet"))

# Cerchiamo di ottenere più informazioni discriminando ancora per colore
x11()
ggpairs(dati, columns = c("carat","color","price","cut","clarity","depth"),mapping = aes(col = color, alpha = 0.7),upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "box_no_facet"))


# E così via... questa fase di esplorazione è di solito propedeutica ad altre analisi (ANOVA, PCA...)


##### VISUALIZZAZIONE 3D #######################################
# Diamo un'occhiata alle dimensioni dei diamanti ###############
# Eliminiamo i diamanti eccessivamente grandi o piccoli (Ma solo perchè i grafici diventano altrimenti illegibili)
thr_right = 0.995
thr_left = 0.005
q_x = quantile(dati$x,probs = c(thr_right,thr_left))
q_y = quantile(dati$y, probs = c(thr_right,thr_left))
q_z = quantile(dati$z, probs = c(thr_right,thr_left))
dati_filter = dati %>% filter(x >= q_x[2] & x <= q_x[1], y >= q_y[2] & y <= q_y[1], z >= q_z[2] & z <= q_z[1])
### Pacchetto visualizzazione 3D: install.packages("rgl")
library(rgl)
open3d()
plot3d(dati_filter$x,dati_filter$y,dati_filter$z)

### Possiamo aggiungere un colore sulla base di un factor. Vediamo se c'è un'interazione con i colori ( non ce lo aspettiamo)
### Mappo il factor in colori
unique_colors = as.numeric(unique(dati$color))
colors = as.numeric(dati$color)
open3d()
plot3d(dati_filter$x,dati_filter$y,dati_filter$z, col = colors)
legend3d("topright",legend = unique(dati$color), col = unique_colors, pch = 16,cex=1, inset=c(0.02))
### Vediamo che interazioni ci sono con il prezzo
#Ordino in maniera crescente col prezzo
dati_filter = dati_filter %>% arrange(price)
## Divido il prezzo in classi
price_classes = cut_number(dati_filter$price, n = 5)
unique_colors = as.numeric(unique(price_classes))
colors = as.numeric(price_classes)
## Plottiamo
open3d()
plot3d(dati_filter$x,dati_filter$y,dati_filter$z, col = colors)
legend3d("topright",legend = unique_colors, col = unique_colors, pch = 16,cex=1, inset=c(0.02))
### I plot in 3d possono fornire informazioni molto utili. Il pacchetto rgl contiene
### Moltissime funzioni per tracciare grafici 3d, qualcuna la vedremo insieme.
### Un compendio completo è disponibilie a https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html