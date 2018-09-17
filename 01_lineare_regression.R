# Einfuehrung lineare Modelle
# GGSG-Statistikkurs
# September 2018


# Pakete installieren / laden
if(!is.element("tidyverse", installed.packages())) { install.packages("tidyverse") }
library(tidyverse)

# Daten einlesen
hw <- read_csv("howell.csv")

# nur Erwachsene
hw <- subset(hw, age >= 18)

# nach Spalte "height" sortieren
hw <- hw %>% arrange(height)

# Visualisierung: Scatterplot
with(hw, plot(weight, height))

# Visualisierung: Histogramm
hist(hw$height)
hist(hw$height, probability = T)
lines(density(hw$height), col = "red", lwd = 2)

# zurueck zum Scatterplot
with(hw, plot(weight, height))

# Modell anpassen
m01 <- lm(height ~ weight, data = hw)

# Regressionslinie zum Plot hinzufuegen
abline(m01, col = "red")

# Reisuden anzeigen
sapply(1:nrow(hw), 
       function(i)
         lines(x = rep(hw$weight[i], 2),
               y = c(hw$height[i], fitted(m01)[i]), 
               col = "blue")
)

# Zusammenfassung des Modells
summary(m01)


# Modell plotten
# zur Erinnerung: Grundformel ist y = a + b * x
# wobei a der Intercept ist, b die Slope
coef(m01) # Intercept + Slope
a <- coef(m01)[1]
b <- coef(m01)[2] 

# Regressionsgerade durch Punktwolke aus Originaldaten...
# x- und y-Limits werden angepasst, damit wir den Intercept (x = 0) sehen
with(hw, plot(weight, height, col = "lightgrey", xlim = c(-10, 65),
              ylim = c(110, 180)))
abline(a = a, b = b)
points(x = 0, y = a, pch = 15, cex = 3, col = "orange") # intercept
abline(v = 0, col = "orange", lty = 2)

# vorhergesagte + beobachtete Verteilung
predict(m01) %>% hist(probability = T, main = "vorhersagte Groesse",
                      ylim = c(0, 0.065))
predict(m01) %>% density %>% lines(col = "darkblue", lwd = 2)
hw$height %>% density %>% lines(col= "darkred", lwd = 2, lty = 2)


############################
## R^2 manuell errechnen: ##
############################

# R^2 = model sum of squares / total sum of squares

# model sum of squares (SSM): Differenz zwischen
# Mittelwert von Y und der Regressionslinie

# visualisiert:
with(hw, plot(weight, height, col = "grey"))
abline(m01, col = "grey20")
abline(h = mean(hw$height), lty = 2) # Mittelwert

sapply(1:nrow(hw), function(i)
  lines(x = rep(hw$weight[i], 2),
        c(mean(hw$height), fitted(m01)[i]), col = "purple")
)

title("Differenz zw. Mittelwert von Y und der Regressionslinie")


# model sum of squares
ssm <- sum((fitted(m01) - mean(hw$height))^2)



# total sum of squares (SST): Differenz zwischen
# beobachteten daten (observed) und dem
# Mittelwert von Y

# visualisiert:
with(hw, plot(weight, height, col = "grey"))
abline(m01, col = "grey20")
abline(h = mean(hw$height), lty = 2) # Mittelwert

sapply(1:nrow(hw), function(i)
  lines(x = rep(hw$weight[i], 2),
        c(mean(hw$height), hw$height[i]), col = "darkorange")
)

title("Differenz zw. Mittelwert von Y und beobachteten Daten")


sst <- sum((hw$height - mean(hw$height))^2)


# R^2
ssm / sst

# zum Vergleich:
summary(m01)$r.squared

# wegen Rundungsungenauigkeit sind allerdings beide nicht genau gleich:
(ssm / sst) == summary(m01)$r.squared


# SST ist uebrigens die Quadratfehlersumme (ssm)
# des Nullmodells:
m0 <- lm(height ~ 1, data = hw) # nur intercept
summary(m0)

# Nullmodell plotten
with(hw, plot(weight, height, col = "grey"))
abline(m0, col = "orange", lwd = 2) # Regressionsline = Mittelwert


##############################
## F-Wert manuell berechnen ##
##############################

# F = mean model sum of squares / mean residual sum of squares
# residual sum of squares: Differenz zwischen
# beobachteten Daten und Regressionslinie

# wie bereits oben visualisiert:
with(hw, plot(weight, height, col = "grey"))
abline(h = mean(hw$height), col = "blue", lty = 2)
sapply(1:nrow(hw), 
       function(i)
         lines(x = rep(hw$weight[i], 2),
               y = c(hw$height[i], fitted(m01)[i]), 
               col = "blue")
)
title("Differenz zwischen beobachteten Daten und der Regressionslinie")

# ein zufaellig herausgegriffener Punkt
# zur Visualisierung von erklaerter
# und unerklaerter Varianz:
with(hw, points(weight[351], height[351], col = "red", pch = 20))
abline(m01, col = "orange", lwd = 2)
lines(x = rep(hw$weight[351], 2),
      y = c(hw$height[351], mean(hw$height)), col = "red")


# residual sum of squares
ssr <- sum((hw$height - fitted(m01))^2)




# um *mean* model sum of squares und *mean* residual
# sum of squares zu bekommen, muessen wir durch die
# Freiheitsgrade teilen
# fuer SSM sind die Freiheitsgrade einfach die Anzahl der
# Variablen im Modell (hier: 1)
# fuer SSR ist es die Anzahl der Beobachtungen minus Anzahl
# der Parameter, die geschaetzt werden (hier: Intercept und weight,
# also zwei), siehe auch
summary(lm(m01)) # ganz unten: "1 and 350 DF"

# mean model sum of squares
msm <- ssm / 1

# mean residual sum of squares
msr <- ssr / 350

# F = msm / msr
msm / msr

# vergleiche:
summary(lm(m01))$fstatistic


###########################
## Annahmen ueberpruefen ##
###########################

# a) Normalverteilung der Residuen
qqnorm(resid(m01)) 
qqline(resid(m01))

# b) Homoskedastizität der Residuenvarienz
plot(resid(m01), fitted(m01))

# c) keine Kollinearitaet zwischen Praediktoren
# hier nur ein Praediktor, daher irrelevant

# d) keine uebermaessig einflussreichen Datenpunkte.
# Funktion dfbeta() gibt an, um wie viel der jeweilige Koeffizient
# angepasst werden muesste, wenn man den Datenpunkt weglaesst.
# Winter (2017): aendert sich das Vorzeichen des Koeffizienten,
# ist der Datenpunkt auf jeden Fall problematisch. 
# Ansonsten Faustregel: plusminus halber absoluter Wert 
# des Koeffizienten besorgniserregend
(coef(m01)[2] + dfbeta(m01)[,2]) %>% plot
abline(h = coef(m01)[2], col = "red", lty = 2)
coef(m01)[2] + coef(m01)[2] / 2 # deutlich oberhalb des hoechsten Punkts
coef(m01)[2] - coef(m01)[2] / 2 # deutlich unterhalb des niedrigsten Punkts

# aussderdem bietet influencePlot eine Reihe von Diagnostiken
# (aus dem Paket "car")
if(!is.element("car", installed.packages())) {install.packages("car")}
library(car)
influencePlot(m01)
plot(hw$height, hw$weight, col = "darkgrey")
points(hw[c(352, 350, 309, 6),]$height, hw[c(352, 350, 309, 6),]$weight, col="red", pch = 20)


# e) Unabhaengigkeit:
# hier gewaehrleistet, weil jeder Datenpunkt
# eine andere Person repraesentiert



###########################
# Lineare Transformation: #
#    Daten zentrieren     #
###########################

# nochmal: Regressionsgerade durch Punktwolke aus Originaldaten...
# x- und y-Limits werden angepasst, damit wir den 
# Intercept (x = 0) sehen
with(hw, plot(weight, height, col = "lightgrey", xlim = c(-10, 65),
              ylim = c(110, 180)))
abline(a = a, b = b)
points(x = 0, y = a, pch = 15, cex = 3, col = "orange") # intercept
abline(v = 0, col = "orange", lty = 2)
text(0, 120, expression(bold("Gewicht = 0 ?!?")), col = "orange")


# Beide Variablen zentrieren:
hw$weight_c <- hw$weight - mean(hw$weight)
hw$height_c <- hw$height - mean(hw$height)

# vergleichen:
plot(hw$weight, hw$height)
plot(hw$weight_c, hw$height)

# neues Modell:
m02 <- lm(height ~ weight_c, data = hw)
summary(m02)
summary(m01) # zum Vergleich

# Plot:
with(hw, plot(weight_c, height, col = "lightgrey"))
abline(m02, col = "orange", lwd = 2)
points(x = 0, y = coef(m02)[1], pch = 15, cex = 3, col = "orange")
abline(v = 0, lty = 2, col = "orange")
text(0.5, 152, 
     paste0("Mittleres Gewicht: ", round(mean(hw$weight), 2), " kg"),
     col = "orange", adj = 0)

# was sagt uns der Beta-Koeffizient?
coef(m02)
b <- coef(m02)[2]

# steigt "Gewicht" um eine Einheit, 
# dann steigt "Groesse" um ~0.9.
# vergleiche:
a <- coef(m02)[1] # Intercept


# Modell: a + b x
# x sei 1:
(a + b * 1)

# x sei 2:
x = 2
a + b * x
points(x = x, y = (a + b * x), pch = 20, col = "red", cex = 2)

# x sei 3:
x = 3
a + b * x
points(x = x, y = (a + b * x), pch = 20, col = "red", cex = 2)

# etc.!


# welcher Groessenzuwachs ist bei einem Gewichts-
# zuwachs von einer Einheit zu erwarten?
a          # Intercept: vorhergesagte Groesse,
           # wenn Gewicht den Mittelwert hat (~ 45 kg)
a + b      # vorhergesagte Groesse, wenn Gewicht
           # um 1 Einheit groesser ist
mean(hw$weight)


