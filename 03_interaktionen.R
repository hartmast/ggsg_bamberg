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
m01 <- lm(blooms ~ water.c + shade.c + water.c : shade.c, data = t) # aequivalent zu lm(blooms ~ water * shade, data = t)

# vergleichen
summary(m00)
summary(m01)


# Interaktion visualisieren
library(visreg)
visreg(m01, "shade.c", by = "water.c")
# zum Vergleich:
visreg(m00, "shade.c", by = "water.c") # gleiche Slope fuer alle!


# Multikollinearitaet?
car::vif(m01)
