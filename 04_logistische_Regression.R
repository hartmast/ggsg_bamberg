# Pakete installieren / laden
sapply(c("tidyverse", "lme4","visreg", "effects", "lattice", 
         "afex", "Hmisc", "psych"), 
       function(x) 
         if(!is.element(x, installed.packages())) 
           install.packages(x, dependencies = T))


library(tidyverse)
library(lme4)
library(visreg)
library(lmerTest)

# Daten einlesen
gen <- read_csv("mercurius_genitiv.csv")

# Fehltreffer ausschliessen
gen <- subset(gen, Fehltreffer!="j")

# Prae- und Poststellung sowie Eigenname als Faktor
gen$Stellung <- factor(gen$Stellung, levels = c("prae", "post"))
gen$Eigenname <- factor(gen$Eigenname)

# Daten visualisieren
plot(gen$Stellung, gen$wordcount)

# Spalten umbenennen
colnames(gen)
colnames(gen)[1:7] <- c("id2", "span1", "anno_default1",
                   "doc", "id2", "span2", "anno_default2")


# einfaches binomiales Modell
m <- glm(Stellung ~ wordcount,
         data = gen,
         family = binomial(link = "logit"))
summary(m)



# komplexeres Modell mit random effect
m1 <- glm(Stellung ~ wordcount + Eigenname + doc,
           data = gen,
           family = binomial)
summary(m1)


# Vergleich mit Nullmodellen
m0 <- glm(Stellung ~  1, data = gen, family = binomial)
anova(m1, m0, test = "Chisq")




m0.2 <- glmer(Stellung ~ Eigenname + (1 | doc),
              data = gen,
              family = binomial)

anova(m, m0.2)
anova(m0.1, m0.2)

afex::mixed(Stellung ~ wordcount + Eigenname + (1 | doc),
            data = gen, family = binomial, method = "LRT")


# Modelldiagnostik: Somers' Dxy
library(Hmisc)
somers2()


# Die vorhergesagten Werte (fitted values) lassen sich
# mit zwei Funktionen anzeigen: fitted() und predict().
# Bei generalisierten linearen Modellen besteht ein wichtiger
# Unterschied zwischen beiden Funktionen:
# predict() zeigt die Werte, *bevor* die Umkehrfunktion
# der Link-Funktion angewendet wird; fitted() zeigt
# die Werte, nachdem sie durch die Umkehrfunktion quasi wieder
# auf die urspruengliche "Skala" der abhaengigen Variable
# zuruckkonvertiert wurden.


# logit-Transform der Fitted-Werte
psych::logit(fitted(m))
qlogis(fitted(m)) # plogis ist "inverse logit", q gibt die q-Werte dazu
predict(m)

# zum Vergleich: logistische Transformation der predict-Werte
plogis(predict(m))
fitted(m)

# siehe auch:
plot(predict(m), fitted(m)) # Die typische Sigmoidform

# 



# alternative Modelle...


# oder als fixed effect?
m1a <- glm(Stellung ~ wordcount + Eigenname + doc,
         data = gen,
         family = binomial)
summary(m1a)

# oder mit interaktion?
m2 <- glmer(Stellung ~ wordcount * Eigenname + (1 | doc),
            data = gen,
            family = binomial)
summary(m2)

