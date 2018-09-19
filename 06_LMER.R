# Einfuehrung generalisierte lineare Modelle
# GGSG-Statistikkurs
# September 2018


# Pakete installieren / laden
sapply(c("tidyverse",
         "beeswarm",
         "lme4",
         "lmerTest",
         "afex",
         "MuMIn",
         "languageR",
         "lattice"), 
       function(x) 
         if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(tidyverse)
library(beeswarm)
library(lme4)
library(lmerTest)
library(afex)
library(MuMIn)
library(lattice)
library(languageR)


#############################
# Lineare gemischte Modelle #
#############################

# Daten laden (aus Baayens languageR-Paket)
data("lexdec")

# Ausreisser ausschliessen & nur korrekte Antworten
lexdec2 = lexdec[lexdec$RT < 7, ]
lexdec3 = lexdec2[lexdec2$Correct == "correct", ]

# relevante Daten zentrieren
lexdec3$Frequency.c <- scale(lexdec3$Frequency, scale = F)
lexdec3$Trial.c <- scale(lexdec3$Trial, scale = F)


# Plot: jede/r Teilnehmer/in einzeln
xylowess.fnc(RT ~ Trial | Subject, data = lexdec3, ylab = "log RT")

# Modell: Reaktionszeit ~ Frequenz
m1 <- lme4::lmer(RT ~ Frequency.c + 
             (1 | Subject) + 
             (1 | Word), 
           data = lexdec3, 
           REML = F) # REML: restricted maximum likelihood
                     # REML = F wird empfohlen fuer Modelle,
                     # bei denen man fixed effects vergleichen moechte.
summary(m1)


# random intercepts fuer die einzelnen Woerter
dotplot(ranef(m1))[1]

# random intercepts fuer die einzelnen Subjects
dotplot(ranef(m1))[2]

# residuals-fitted plot
plot(m1)

# qqplot
qqnorm(resid(m1))
qqline(resid(m1), col = "red")




# random slopes
data("quasif") # SOA = stimulus onset asynchrony: the time between the presentation of a prime or distractor and the presentation of the target in chronometric experiments)

# Plot
xylowess.fnc(RT ~ SOA | Subject, data = quasif, ylab = "log RT")
xylowess.fnc(RT ~ SOA | Subject, data = quasif)

m2 <- lmer(RT ~ SOA + (1|Item) +
             (1 + SOA|Subject), data = quasif, REML = F)

m2.0 <- lmer(RT ~ (1|Item) +
                     (1 + SOA|Subject), data = quasif, REML = F)

# Modellvergleich
anova(m2, m2.0)
