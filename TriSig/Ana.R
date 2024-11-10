# Étape 1 : Collecte des données
resultats_theoriques <- c(1, 2, 3, 4, 5) # Remplacez par vos données théoriques
resultats_experimentaux <- c(1.1, 1.9, 3.2, 3.8, 5.1) # Remplacez par vos données expérimentales

# Étape 2 : Préparation des données
data <- data.frame(
  Theorie = resultats_theoriques,
  Experiment = resultats_experimentaux
)

# Étape 3 : Visualisation
library(ggplot2)

ggplot(data, aes(x = Theorie)) +
  geom_line(aes(y = Theorie, color = "Théorique")) +
  geom_point(aes(y = Experiment, color = "Expérimental")) +
  labs(title = "Comparaison des résultats théoriques et expérimentaux",
       x = "Valeurs Théoriques",
       y = "Valeurs",
       color = "Légende") +
  theme_minimal()

# Étape 4 : Analyse statistique
correlation <- cor(data$Theorie, data$Experiment)
cat("Coefficient de corrélation :", correlation, "n")

# Test de Student pour comparer les moyennes
t_test_result <- t.test(data$Theorie, data$Experiment)
print(t_test_result)

# Étape 5 : Interprétation
# Vous pouvez interpréter les résultats du test t et du coefficient de corrélation ici.
