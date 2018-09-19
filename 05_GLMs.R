# Einfuehrung generalisierte lineare Modelle
# GGSG-Statistikkurs
# September 2018


# Pakete installieren / laden
sapply(c("tidyverse",
         "beeswarm",
         "lme4",
         "lmerTest",
         "afex",
         "MuMIn"), 
       function(x) 
         if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(tidyverse)
library(beeswarm)
library(lme4)
library(lmerTest)
library(afex)
library(MuMIn)

# Daten einlesen
maed <- read_csv("maedchen.csv")

# Jahrzehnt-Spalte hinzufuegen
maed$Jahrzehnt <- 
  sapply(1:nrow(maed),
    function(i) as.numeric(paste0(gsub("(?<=[0-9][0-9][0-9]).*", "", maed$Date[i], perl = T), "0"))
)


# NAs auslassen
maed <- maed[!is.na(maed$dist),]

# Plot
with(maed, beeswarm(dist ~ anaph))
with(maed, boxplot(dist ~ anaph, add = T, col = rgb(0,.4,.8, alpha = .3)))


# nur aus Neugier: was passiert, wenn wir Outlier weglassen?
with(subset(maed, dist < 18), boxplot(dist ~ anaph))

# Anaphor. Referenz als Faktor
maed$anaph <- factor(maed$anaph)

# Modell
m1 <- glm(anaph ~ dist, data = maed, family = binomial)
m2 <- glmer(anaph ~ dist + (1 | Genre), data = maed, family = binomial)
summary(m1)
summary(m2)

# Koeffizienten
coef(m1)

# Fuer jedes Wort mehr, das zwischen "Maedchen" und
# dem Pronomen steht, erhoehen sich die log odds,
# dass "sie" kommt, um 0.08
# Um von log odds zu Odds zu kommen,
# koennen wir den Koeffizienten exponieren:

exp(coef(m1)[2])

# von odds zur Wahrscheinlichkeit:
exp(coef(m1)[2]) / (1 + exp(coef(m1)[2]))

# oder auch:
plogis(coef(m1)[2])


#######################
# Vorhersagen ansehen #
#######################

# Bereich der Werte, die wir uns vorhersagen lassen wollen
werte <- seq(min(maed$dist), 
         max(maed$dist), 
         1)
preds       <- data_frame(dist = werte)
predict(m1, preds)
preds$preds <- predict(m1, preds)

# Die Daten muessen noch logistisch transformiert werden:
preds$preds <- plogis(preds$preds)


# mit standard errors:
predict(m1, preds, se.fit = T)

# aus dem Standardfehler kann man 
# Konfidenzintervalle ableiten:
# CI = fit + / - 1.96 * Standardfehler
fit      <- predict(m1, preds, se.fit = T)$se.fit
se       <- predict(m1, preds, se.fit = T)$se.fit
ci_upper <- fit + 1.96 * se
ci_lower <- fit - 1.96 * se

# Das Ganze muss noch logistisch transformiert werden:
fit      <- plogis(fit)
ci_upper <- plogis(ci_upper)
ci_lower <- plogis(ci_lower)

# plot
plot(werte, fit, type = "l", ylim = c(0,1), 
     lwd = 2, col = "blue", 
     ylab = "Wahrscheinlichkeit \'sie\'", xlab = "Distanz")
lines(werte, ci_upper, lty = 3)
lines(werte, ci_lower, lty = 3)
polygon(x = c(werte, rev(werte)),
        y = c(ci_upper, rev(ci_lower)),
        col = rgb(0,.4,.6, alpha = .2), border = F)

