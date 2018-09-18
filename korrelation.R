# Daten einlesen
at <- read.csv("alkohol_tabak.csv", 
               fileEncoding = "UTF-8")

# Raenge
at$Rang_Alkohol <- rank(at$Alkohol)
at$Rang_Tabak <- rank(at$Tabak)

# Inversionen
at$Inversion <- NA
for(i in 1:nrow(at)) {
  l <- (i+1):nrow(at)
  at$Inversion[i] <- 
    length(which(at[l,]$Rang_Tabak < at[i,]$Rang_Tabak))
}

# Summe der Inversionen
s <- sum(at$Inversion)

tau <- 1 - ((2 * 28) / 55)
tau

# mit R:
cor.test(at$Alkohol, at$Tabak, method = "kendall")
cor.test(at$Alkohol, at$Tabak, method = "pearson")
cor.test(at$Alkohol, at$Tabak, method = "spearman")

