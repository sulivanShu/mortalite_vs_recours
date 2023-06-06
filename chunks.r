#!/usr/bin/Rscript
stop("

     Ce fichier contient des blocs de code utilisés pour mieux comprendre
     le langage R et la bibliothèque `insee`.\

     CE FICHIER NE DOIT PAS ÊTRE EXÉCUTÉ, C'EST UN FICHIER DE TEST.\

	")

# mkfifo $XDG_DATA_HOME/fifo 2>/dev/null ; chmod 600 $XDG_DATA_HOME/fifo && tail -f $XDG_DATA_HOME/fifo | R --no-echo --save --restore --file=- & echo 'options(error=dump.frames)' | cat > $XDG_DATA_HOME/fifo ; fg 2>/dev/null
# Insee: manque les départements  

# options(error=dump.frames)
# 
# system.time ({

library(insee) 
# Évolution du taux de mortalité standardisé de 2020 par rapport à la moyenne des trois années précédantes.
get_idbank_list("DECES-MORTALITE") |> 
	subset(FREQ == "A" & INDICATEUR == "TAUX_MORTALITE_STANDARDISE" & grepl("^D", REF_AREA) & AGE == "65-") |> 
	# data.frame des idbank
	`[`(x = _, "idbank") |> 
	# convertir le data.frame en numeric
	unlist() |> 
	# sélectionner la période pertinente des tableaux
	get_insee_idbank(startPeriod = 2017, endPeriod = 2020) |> 
	# sélectionner les valeurs pertinentes
	`[`(x = _, "OBS_VALUE") |> 
	# convertir le data.frame en numeric
	unlist() |> 
	# fonction pour calculer le taux de croissance de la dernière année par rapport à la moyenne des trois années précédentes
	(function(data = _) { sapply(seq(1, length(data), by = 4), function(start) { ( data[start] - mean(data[(start+1):(start+3)]) ) / mean(data[(start+1):(start+3)]) }) })() |>
	print()

# })

# Nombre de personnes de plus de 60 ans par département fois 100
get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |> 
	subset(grepl("^D", REF_AREA) & SEXE == "0" & grepl("00-$|60-$", AGE) ) |> 
	`[`(x = _, "idbank") |> 
	unlist() |> 
	get_insee_idbank(startPeriod = 2020, endPeriod = 2020) |> 
	(\(data) {data[order(data$REF_AREA), ] })() |> 
	`[`(x = _, "OBS_VALUE") |> 
	unlist() |> 
	(\(data) { sapply(seq(1, length(data), by = 2), \(start) { (data[start]) * data[start+1] }) })() |>
	print()

scan("AtihTauxPatients1000Std.tsv", what = numeric(), quiet = TRUE) |>
	(\(data) { sapply(seq(1, length(data), by = 4), function(start) { ( data[start] - mean(data[(start+1):(start+3)]) ) / mean(data[(start+1):(start+3)]) }) })() |>
	print()

help("scan")

	print()

"blabla"

print()

	(\(data) { sapply(seq(1, length(data), by = 4), function(start) { ( data[start] - mean(data[(start+1):(start+3)]) ) / mean(data[(start+1):(start+3)]) }) })()

get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |> 
	subset(grepl("^D", REF_AREA) & SEXE == "0" & grepl("00-$|60-$", AGE) ) |> 
	`[`(x = _, "idbank") |> 
	unlist() |> 
	get_insee_idbank(startPeriod = 2020, endPeriod = 2020) |> 
	colnames()

install.packages('pmeasyr', repos = 'https://guillaumepressiat.r-universe.dev')

install.packages('sjlabelled')
install.packages('sqldf')

available.packages()



library()


help("read_html" package="rvest")

	class()

colnames()

unique() |> print(n=200)

library(insee) 
# liste des départements. 
get_idbank_list("DECES-MORTALITE") |> 
	subset(FREQ == "A" & INDICATEUR == "TAUX_MORTALITE_STANDARDISE" & grepl("^D", REF_AREA) & AGE == "65-") |> 
	# data.frame des idbank
	`[`(x = _, "idbank") |> 
	# convertir le data.frame en numeric
	unlist() |> 
	# sélectionner la période pertinente des tableaux
	get_insee_idbank(startPeriod = 2020, endPeriod = 2020) |> 
	(\(data) data$REF_AREA)()


	colnames()

