# Einfuehrung lineare Modelle
# GGSG-Statistikkurs
# September 2018


# Pakete installieren / laden
if(!is.element("tidyverse", installed.packages())) { install.packages("tidyverse") }
library(tidyverse)

# Daten einlesen
hw <- read_csv("howell.csv")

# nur Kinder
hw <- subset(hw, age < 18)

# plot
with(hw, plot(age, height))

# Modell
mm1 <- lm(height ~ age + gender, data = hw)

# Summary
summary(mm1)

# plot (1)
with(hw, plot(age, height, col = "darkgrey"))
abline(mm1) # was sagt uns die Warnung?
            # --> es gibt natuerlich mehr als 2 Koeffizienten...
            # d.h. es wurde mehr als 1 Linie angepasst!

# einzelne Koeffizienten
coef(mm1)
a        <- coef(mm1)[1]
b_age    <- coef(mm1)[2]
b_gender <- coef(mm1)[3] 


# scatterplot3d package
if(!is.element("scatterplot3d", installed.packages())) {
  install.packages("scatterplot3d")
}
library(scatterplot3d)

# 3D-Plot mit Regressionsplane
hw$gender2 <- ifelse(hw$gender=="male", 0, 1)
s3d <- scatterplot3d(x = as.numeric(as.factor(hw$gender))-1,
              y = hw$age, 
              z = hw$height, type = "h", highlight.3d = T,
              xlab = "gender", ylab = "height", zlab = "age",
              angle = 35)
s3d$plane3d(mm1, draw_polygon = T, draw_lines = T) 


# rotieren lassen:
for(i in 1:36) {
  s3d <- scatterplot3d(x = as.numeric(as.factor(hw$gender))-1,
                       y = hw$age, 
                       z = hw$height, type = "h", highlight.3d = T,
                       xlab = "gender", ylab = "height", zlab = "age",
                       angle = i*10)
  s3d$plane3d(mm1, draw_polygon = T, draw_lines = T)
  Sys.sleep(.5)
}



#######################
# Multikollinearitaet #
#######################

# Beispiel aus McElreath (2015)
# wir ergaenzen den !Kung-Datensatz um fiktive Beinlaengen...
set.seed(1990)
hw$leg_left <- runif(nrow(hw), 0.4, 0.5) * hw$height + rnorm(nrow(hw), 0, 0.02)
set.seed(1995)
hw$leg_right <-  runif(nrow(hw), 0.4, 0.5) * hw$height + rnorm(nrow(hw), 0, 0.02)


# Laenge eines Beins als Praediktor fuer Groesse
m_leg1 <- lm(height ~ leg_left, data = hw)
summary(m_leg1)


# Laenge beider Beine als Praediktoren
m_leg2 <- lm(height ~ leg_left + leg_right, data = hw)
summary(m_leg2)
car::vif(m_leg2)


# mit Interaktion
m_leg3 <- lm(height ~  leg_left * leg_right, data = hw)
summary(m_leg3)
car::vif(m_leg3)
