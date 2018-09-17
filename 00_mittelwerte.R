library(tidyverse)

# zufaellige Daten generieren
set.seed(42)
x <- rnorm(n = 1000, mean = 5, sd = 20)

# Scatterplot
plot(x, col = "darkgrey")

# Mittelwert
abline(h = mean(x), lwd = 2, col = "red", lty = 5)

# Standardabweichung
arrows(x0 = 500, x1 = 500,
       y0 = mean(x) + sd(x),
       y1 = mean(x) - sd(x), code = 3)

# Standardfehler
se <- sd(x) / sqrt(length(x))


arrows(x0 = 700, x1 = 700,
       y0 = mean(x) - se,
       y1 = mean(x) + se,
       code = 3, length = .1, angle = 30)


set.seed(42)
y <-rnorm(n = 1000, mean = 30, sd = 30)


plot(y, col = "lightblue")
points(x, col = "orange")
arrows(x0 = 500, x1 = 500,
       y0 = mean(x) + sd(x),
       y1 = mean(x) - sd(x), code = 3, 
       col = "red", lwd = 5)
arrows(x0 = 700, x1 = 700,
       y0 = mean(y) + sd(y),
       y1 = mean(y) - sd(y), code = 3,
       col = "blue", lwd = 5)
abline(h = mean(x), col = "red", lwd = 5, lty = 2)
abline(h = mean(y), col = "blue", lwd = 5, lty = 2)



# Idee hinter dem Standardfehler illustriert

# unsere "Grundgesamtheit"
set.seed(2000)
gg <- rnorm(n = 1000000, mean = 50, sd = 10)

# samples
seeds <- c(1900:2000)
samples <- list()

# Liste mit Samples
for(i in 1:100) {
  set.seed(seeds[i])
  samples[[i]] <- sample(gg, 1000)
}

# wie unterscheiden sich die Samples?
boxplot(samples)

# Standardfehler: 
# Quadratwurzel aus Summe aus (sample mean - overall mean)^2
# (fuer jedes Sample) geteilt durch Zahl der Samples,
# also sqrt(sum(fuer jedes Sample: sample mean - overall mean)  /
# Anzahl der Samples)
(sapply(1:length(samples), 
       function(i) (mean(samples[[i]]) - 
                      mean(unlist(samples)))^2) / 100) %>%
  sum %>%
  sqrt

# zum Vergleich:
sd(samples[[1]]) / sqrt(length(samples[[1]]))
sd(samples[[5]]) / sqrt(length(samples[[1]]))
sd(samples[[10]]) / sqrt(length(samples[[1]]))
# etc.
