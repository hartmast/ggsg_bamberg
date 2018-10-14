# Einfuehrung lineare Modelle
# GGSG-Statistikkurs
# September 2018


# Pakete installieren / laden
if(!is.element("tidyverse", installed.packages())) { install.packages("tidyverse") }
if(!is.element("car", installed.packages())) { install.packages("car") }
if(!is.element("visreg", installed.packages())) { install.packages("visreg") }
library(tidyverse)


# Tulpen: Beispiel aus McElreath (2015)
t <- read_csv("tulips.csv")

# Praediktiren zentrieren
t$water.c <- t$water - mean(t$water)
t$shade.c <- t$shade - mean(t$shade)

# Modell ohne Interaktion
m00 <- lm(blooms ~ water.c + shade.c, data = t)


# Modell mit Interaktion
m01 <- lm(blooms ~ water.c + shade.c + water.c : shade.c, data = t) # aequivalent zu m03 <- lm(blooms ~ water.c * shade.c, data = t)
m01b <- lm(blooms ~ water.c + shade.c + water.c:shade.c, data = t)
m02 <- lm(blooms ~ water.c*shade.c, data = t)

# vergleichen
summary(m00)
summary(m01)
summary(m02)

# Interaktion visualisieren
library(visreg)
visreg(m01, "shade.c", by = "water.c")
# zum Vergleich:
visreg(m00, "shade.c", by = "water.c") # gleiche Slope fuer alle!


# Multikollinearitaet?
car::vif(m01)

# zum Vergleich manuell generierter
# linkster Plot im Triptychon...
shade.c <- seq(-1, 1, 0.1)
water.c <- -1
plot((-1 * shade.c), type = "l", ylim=c(-10,400))
-1 * shade.c
plot(128.99 + 75.8 * water.c + -41.6 * shade.c, ylim = c(-10, 300))

water.c <- -1
plot(128.99 + 75.8 * water.c + -41.6 * shade.c + -52.85 * (water.c * shade.c), ylim = c(0,300))
