#qst 1: importation des données cac40.csv sous fomre de data frame en utilisant la fonction read.csv 
data = read.csv("./CAC40.csv", header = TRUE, sep = ",")
head(data)
#qst 2: 2.1 Vérification des types de variables des colonnes et les premières observations de chaque colonne en utilisant la fonction str
str(data)
#2.2 Recherche des valeurs manquantes en utilisant la fonction summary()
summary(data)
# Identification des colonnes avec un Volume = 0 ou les autres valeurs != 0 ce qui indique un probleme dans la valeur du volume 
anomalies = data[data$Volume == 0 & (data$Volume != data$High | data$Volume != data$Low | data$Volume != data$Close | data$Volume != data$Adj.Close) , ]
# Afficher les anomalies pour vérification
print(anomalies)
#Identifier les doublons au cas où certaines lignes seraient répétées en utilisant la fonction duplicated
duplicated_rows = data[duplicated(data), ]
print(duplicated_rows)
#Remplacement du valeur volume = 0 par la moyenne de volume != 0 d'autre jours 
# Filtrer les colonnes ou Volume != 0
volume_non_null = data[data$Volume != 0 , ]

# Calculer la moyenne de 'Volume'  ou Volume != 0
moy_non_null = mean(volume_non_null$Volume, na.rm = TRUE)
moy_non_null

#remplacement
data$Volume[data$Volume == 0] <- moy_non_null

# les données apres le mise a jour 
print(data)

# calcule des statistiques descriptives pour les prix d'ouverture 
# Calcule du min , max , medianne , moyenne , 1st and 3rd quartiles du prix d'overture en utilisant la fonction summary 
summary_open = summary(data$Open)
summary_open

#calcule d'ecart type du prix d'ouverture 
sd_open = sd(data$Open, na.rm = TRUE)
sd_open 

# Calcul de la variance pour les prix d'ouverture
variance_open = var(data$Open, na.rm = TRUE)
variance_open 

#calcule du mode du prix d'ouverture 
# Fonction pour calculer le mode
calculate_mode = function(x) {
  uniqx = unique(x)  # Valeurs uniques du prix d'ouverture
  uniqx[which.max(table(x))]  # la fonction table calculer la frequence de chaque valeur unique du uniqx 
}
# Calcul du mode pour les prix d'ouverture
mode_open = calculate_mode(data$Open)
mode_open

# calcule des statistiques descriptives pour les prix de fermeture
# Calcule du min , max , medianne , moyenne , 1st and 3rd quartiles du prix d'overture en utilisant la fonction summary
summary_close = summary(data$Close)
summary_close
#calcule d'ecart type du prix de fermeture 
sd_close = sd(data$Close, na.rm = TRUE)
sd_close
# Calcul de la variance pour les prix d'ouverture
variance_close = var(data$Close, na.rm = TRUE)
variance_close

#calcule du mode du prix de fermeture
# Calcul du mode pour les prix de fermeture
mode_close = calculate_mode(data$Close)
mode_close

#qst4 
# Histogramme pour les prix d'ouverture
hist(data$Open, main = "Distribution des prix d'ouverture", xlab = "Prix d'ouverture", col = "lightpink", border = "black", breaks = 20)

# Histogramme pour les prix de fermeture
hist(data$Close, main = "Distribution des prix de fermeture", xlab = "Prix de fermeture", col = "lightgreen", border = "black", breaks = 20)

#qst5
# Convertir la colonne Date en format Date
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Graphiques des prix d'ouverture

plot(data$Date, data$Open, type = "l", col = "blue", lwd = 2, 
     main = "Tendance temporelle des prix d'ouverture", 
     xlab = "Date", ylab = "Prix d'ouverture")

# Tendance des prix de fermeture

plot(data$Date, data$Close, type = "l", col = "red", lwd = 2, 
     main = "Tendance temporelle des prix de fermeture", 
     xlab = "Date", ylab = "Prix de fermeture")

## qst 6
# Boxplots comparatifs
boxplot(data$Open, data$Close, names = c("Ouverture", "Fermeture"), col = c("blue", "red"), main = "Comparaison des Prix d'Ouverture et de Fermeture")


# Test statistique de Wilcoxon
test_result <- wilcox.test(data$Open, data$Close, paired = TRUE)
test_result

# Test de normalité pour les différences
shapiro.test(data$Open - data$Close)

# Si les différences sont normalement distribuées, utiliser le test t apparié
t_test_result <- t.test(data$Open, data$Close, paired = TRUE)
t_test_result


## qst 7 
# Calcul de la volatilité
data$Volatility <- abs(data$Open - data$Close)

# Statistiques descriptives de la volatilité
# Moyenne de la volatilité
mean_volatility <- mean(data$Volatility, na.rm = TRUE)

# Écart-type de la volatilité
sd_volatility <- sd(data$Volatility, na.rm = TRUE)

# Valeurs minimum et maximum de la volatilité
min_volatility <- min(data$Volatility, na.rm = TRUE)
max_volatility <- max(data$Volatility, na.rm = TRUE)

# Afficher les résultats
mean_volatility
sd_volatility
min_volatility
max_volatility