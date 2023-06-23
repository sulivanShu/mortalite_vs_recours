#!/usr/bin/Rscript

library(insee) 
library(rvest)
library(weights)

# Évolution du taux de mortalité standardisé de 2020 par rapport à la moyenne des trois années précédantes.
EvoMortStd = get_idbank_list("DECES-MORTALITE") |> 
	subset(FREQ == "A" & INDICATEUR == "TAUX_MORTALITE_STANDARDISE" & grepl("^D", REF_AREA) & AGE == "65-") |> 
	# data.frame des idbank
	`[`(x = _, "idbank") |> 
	# convertir le data.frame en character
	unlist() |> 
	# sélectionner la période pertinente des tableaux
	get_insee_idbank(startPeriod = 2017, endPeriod = 2020) |> 
	# sélectionner les valeurs pertinentes
	`[`(x = _, "OBS_VALUE") |> 
	# convertir le data.frame en numeric
	unlist() |> 
	unname() |>
	# fonction pour calculer le taux de croissance de la dernière année par rapport à la moyenne des trois années précédentes
	(function(data = _) { sapply(seq(1, length(data), by = 4), function(start) { ( data[start] - mean(data[(start+1):(start+3)]) ) / mean(data[(start+1):(start+3)]) }) })() |>
	print()

# Nombre de personnes de plus de 60 ans par département fois 100
PondPop60 = get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |> 
	subset(grepl("^D", REF_AREA) & SEXE == "0" & grepl("00-$|60-$", AGE) ) |> 
	`[`(x = _, "idbank") |> 
	unlist() |> 
	get_insee_idbank(startPeriod = 2020, endPeriod = 2020) |> 
	(\(data) {data[order(data$REF_AREA), ] })() |> 
	`[`(x = _, "OBS_VALUE") |> 
	unlist() |> 
	unname() |>
	(\(data) { sapply(seq(1, length(data), by = 2), \(start) { (data[start]) * data[start+1] }) })() |>
	print()

# évolution du taux de patients
# période: 2017 à 2020
# type de taux: taux standardisés
# niveau géographique: département

# convertir les caractères de chiffres en nombres
clean_numeric = \(x) {
  x = gsub(",", ".", x) |> 
    gsub(" ", "", x=_) |>
    as.numeric() |>
    suppressWarnings()
}

html_page = read_html("https://www.scansante.fr/applications/taux-de-recours-tous-champs/submit?snatnav=&mbout=part1&champ=tous+champs&unite=patients&version=v2021&taux=stand&tgeo=dep")

EvoTxPat = html_page |>
	(\(data) {html_table(data)[[3]]})() |>
	(\(df) {
		 names(df) = paste(names(df), df[1, ], sep = " ")
		 df[, -1] = lapply(df[, -1], clean_numeric)
		 df[-c(1, tail(seq_len(nrow(df)), 3)), ]
})() |>
(\(df) {
	 sapply(1:nrow(df), \(i) {
			(df[i, 5] - rowMeans(df[i, 2:4])) / rowMeans(df[i, 2:4])
} ) |> 
unlist()
})() |> 
unname() |>
print()

# Corrélation entre l'évolution de la mortalité standardisée et l'évolution du taux de recours aux soins hospitaliers, par département, 2020 par rapport à la moyenne 2017-2019, pondérée la population des plus de 60 ans dans chaque département
wtd.cor(EvoMortStd, EvoTxPat, weight = PondPop60) |>
	print()

# Créer le fichier PNG
png("EvoMortStdVsEvoTxPat.png")
# Dessiner le nuage de points
plot(EvoTxPat, EvoMortStd, cex = PondPop60/10000000, main = "Δ Mortalité standardisée vs Δ recours aux soins, 2020", xlab = "Δ recours aux soins", ylab = "Δ mortalité standardisée")
# Ajuster la régression linéaire et ajouter la ligne de tendance:
lm(EvoMortStd ~ EvoTxPat, weights = PondPop60) |> abline(col = "red")
# Fermer le fichier PNG
dev.off()
