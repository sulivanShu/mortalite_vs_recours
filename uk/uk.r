#!/usr/bin/Rscript
library(purrr)
library(dplyr)
library(readxl)
library(tibble)
library(weights)
# la librairie tibble n'est pas nécessaire, mais elle est utilisée pour mieux voir les dataframes
library(parallel)
# créer le cluster et appeler les bibliothèques
cl =
	detectCores() |>
	makeCluster()
clusterEvalQ(cl, {
		     library(readxl)
		     library(tibble)
})
setDefaultCluster(cl)
system.time({
	# Pour une exécution parallèle avec parLapply
	# Possibilité d'ajouter autant de taches parallèles que nécessaire, qui seront ensuite identifiées par des conditions if (task = ???) {}. Possibilité d'imbriquer des conditions. 
	tasks_list = c(
		       "2017_admi" = "2017_admi",
		       "2018_admi" = "2018_admi",
		       "2019_admi" = "2019_admi",
		       "2020_admi" = "2020_admi",
		       "2017_morta" = "2017_morta",
		       "2018_morta" = "2018_morta",
		       "2019_morta" = "2019_morta",
		       "2020_morta" = "2020_morta",
		       "nhs_geo_code" = "nhs_geo_code",
		       "clean_nhs2ons_code" = "clean_nhs2ons_code"
	)
	clusterEvalQ(cl, {
			     hosp_list = c(
					   "2017_admi" = "hosp-epis-stat-admi-prov-2017-18-tab.xlsx",
					   "2018_admi" = "hosp-epis-stat-admi-prov-2018-19-tab.xlsx",
					   "2019_admi" = "hosp-epis-stat-admi-prov-2019-20-tab.xlsx",
					   "2020_admi" = "hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx"
			     )
			     morta_list = c(
					    "2017_morta" = "deathsregisteredbyareaofusualresidence2017.xls",
					    "2018_morta" = "deathsregisteredbyareaofusualresidence2018.xls",
					    "2019_morta" = "deathsregisteredbyareaofusualresidence2019.xls",
					    "2020_morta" = "deathsregisteredbyareaofusualresidence2020.xlsx"
			     )
	})
	clean_hosp_codes_list =
		tasks_list |> 
		parLapply(cl = NULL, function(task) {
				  # Traitements en parallèle.
				  # Sur un ordinateur multicore, parLapply() est deux fois plus rapide que lapply()
				  # Traitement des fichiers xlsx, sur les statistiques de recours aux soins par fournisseurs de soins
				  if (task %in% c("2017_admi", "2018_admi", "2019_admi", "2020_admi"))
				  {
					  # Chargement des fichiers. Le numéro de chaque tâche dans le vecteur tasks_list (2017_admi:2020_admi) est aussi le numéro de chaque fichier dans le vecteur hosp_list. On peut donc réutiliser cet argument. 
					  hosp = hosp_list[task] |>
						  read_excel()
					  # Traitement
					  if (task == "2020_admi") {
						  hosp = hosp[,-1]
					  }
					  hosp[7,c(1,2,48)] =
						  c("Code", "Hospital.provider.name", "Zero.bed.day.cases.Emergency") |>
						  as.list() 
					  # Renommer des colonnes
					  colnames(hosp) =
						  hosp[7,] |>
						  gsub("NA", NA, x=_)
					  # Sélectionner les lignes pertinentes, qui sont différentes pour chaque fichier.
					  if (task == "2017_admi") {
						  hosp = hosp[16:499,]
					  } else if (task == "2018_admi") {
						  hosp = hosp[17:487,]
					  } else if (task == "2019_admi") {
						  hosp = hosp[19:502,]
					  } else if (task == "2020_admi") {
						  hosp = hosp[19:509,]
					  }
					  # Nettoyage du fichier 
					  # Dans le fichier, le caractère * signfie soit un entier entre 1 et 7, soit "Not available".
					  # Vecteur des colonnes pour lesquelles "*" signifie un entier entre 1 et 7.
					  stars_rows = c( "Finished consultant episodes", "Admissions", "Male", "Female", "Gender Unknown", "Emergency", "Waiting list", "Planned", "Other Admission Method", "Age 0", "Age 1-4", "Age 5-9", "Age 10-14", "Age 15", "Age 16", "Age 17", "Age 18", "Age 19", "Age 20-24", "Age 25-29", "Age 30-34", "Age 35-39", "Age 40-44", "Age 45-49", "Age 50-54", "Age 55-59", "Age 60-64", "Age 65-69", "Age 70-74", "Age 75-79", "Age 80-84", "Age 85-89", "Age 90+", "Day case", "FCE bed days", "Zero.bed.day.cases.Emergency", "Elective", "Other")
					  # Fonction pour remplacer le caractère "*" par une valeur numérique. J'ai choisi 3 pour avoir un entier entre 1 et 7. Possibilité d'être plus précis.
					  replace_star_by_3 =
						  \(x) {
							  gsub("\\*", "3", x) |>
								  as.integer() 
						  }
					  # Vecteur des colonnes pour lesquelles "*" signifie NA.
					  means_medians_rows = c("Mean time waited", "Median time waited", "Mean length of stay", "Median length of stay", "Mean age")
					  # Fonction pour remplacer le caratère "*" par la valeur "Not available".
					  replace_star_by_NA =
						  \(x) {
							  gsub("\\*", NA, x) |> as.numeric()
						  }
					  # Les codes se terminant par "-X" sont des codes de réserve provisoires. Il faut supprimer ce suffixe pour avoir le code normal. 
					  replace_X_code =
						  \(x) {
							  gsub("-X", "", x)
						  }
					  # Nettoyage
					  clean_hosp =
						  hosp[,hosp |> names() |> (\(x) !is.na(x))()] |>
						  (\(df) {
							   # Remplacer "*" par 3
							   df[,stars_rows] = lapply(df[,stars_rows], replace_star_by_3)
							   # Remplacer "*" par NA
							   df[,means_medians_rows] = lapply(df[,means_medians_rows], replace_star_by_NA)
							   # Supprimer le suffixe "-X" des codes
							   df[, "Code"] = lapply(df[, "Code"], replace_X_code)
							   df
			     })() |>
			  # Sélectionner les colonnes pertinentes
			  _[,c("Code", "Finished consultant episodes")] |>
			  (\(df) {
				   df[order(df$"Code"),]
			     })() 
			  return(clean_hosp)
				  }
				  # Mortalité
				  if (task %in% c("2017_morta", "2018_morta", "2019_morta", "2020_morta"))
				  {
					  morta = morta_list[task] |>
						  read_excel(sheet = "Table 1a") 
					  # Traitement
					  # ne pas oublier de convertir des colonnes de caractères en colonnes de nombres
					  if (task == "2020_morta") {
						  colnames(morta) =
							  morta[2,]
						  morta = morta[3:423,c("Area codes","Populations Number (thousands) of Persons all ages","Deaths of Persons all ages")]
					  }
					  else {
						  if (task == "2017_morta") {
							  morta = morta[11:512,c(1,3,6)]
						  }
						  if (task == "2018_morta") {
							  morta = morta[11:501,c(1,3,6)]
						  }
						  if (task == "2019_morta") {
							  morta = morta[10:495,c(1,3,6)]
						  }
						  morta = morta |>
							  na.omit()
						  colnames(morta) = c("Area codes","Populations Number (thousands) of Persons all ages","Deaths of Persons all ages")
					  }
					  morta$"Populations Number (thousands) of Persons all ages" = morta$"Populations Number (thousands) of Persons all ages" |>
						  as.numeric() * 1000
					  morta$"Deaths of Persons all ages" = morta$"Deaths of Persons all ages" |>
						  as.integer()
					  return(morta)
				  }
				  ## Correspondance `Code` -- `Geographic.Local.Authority.Code` (Code NHS de l'établissement -- code NHS géographique) 
				  if (task == "nhs_geo_code") {
					  nhs_geo_code =
						  read.csv("ODS_2023-06-20T170741.csv") |>
						  _[,c("Code","Geographic.Local.Authority.Code")] |>
						  # Ajout manuel des hôpitaux qui manquent
						  rbind(
							read.csv("genealogie.csv") |>
								_[,c("Code",
								     "Geographic.Local.Authority.Code"
								     )]
							) |>
			  (\(df) {
				   df[order(df$"Code"),]
							})() |>
			  as_tibble() 
		  return(nhs_geo_code)
				  }
				  if (task == "clean_nhs2ons_code") {
					  ## Correspondance `Geographic.Local.Authority.Code` et `ONS Geography` (NHS geographic code -- ONS geographic code)
					  nhs2ons_code =
						  read_excel("LA-Type-B-February-2020-4W5PA.xls", sheet = "LA - by type of care") 
					  colnames(nhs2ons_code) =
						  nhs2ons_code[12,] |>
						  gsub("Code","Geographic.Local.Authority.Code", x=_)
					  clean_nhs2ons_code =
						  nhs2ons_code[15:164,c("ONS Geography", "Geographic.Local.Authority.Code")] |>
						  (\(df) {
							   df[order(df$"Geographic.Local.Authority.Code"),]
							})() 
						  return(clean_nhs2ons_code)
				  }
				  # Fin de la fonction parLapply()
	}) |>
	    print()
})

# Solution sérielle
# 80-90ms sans print()
# plus rapide que la version parallèle
system.time({
	years_tasks_list_admi = c(
				  "2017_admi" = "2017_admi",
				  "2018_admi" = "2018_admi",
				  "2019_admi" = "2019_admi",
				  "2020_admi" = "2020_admi"
	) 
	evo.FCE.2017_2019.2020 =
		years_tasks_list_admi |>
		lapply(function(task) {
			       merge(clean_hosp_codes_list$"nhs_geo_code", clean_hosp_codes_list[[years_tasks_list_admi[task]]], by = "Code") |>
				       (\(df) {
						df[order(df$"Geographic.Local.Authority.Code"),]
							})() |>
		       # Fusion du résultat avec le fichier clean_nhs2ons_code. Permet d'associer le code géographique NHS au code géographique ONS.
		       merge(clean_hosp_codes_list$"clean_nhs2ons_code", by = "Geographic.Local.Authority.Code") |>
		       _[c("ONS Geography", "Finished consultant episodes")] |> 
		       # grouper par code géographique ONS et sommer les éléments de chaque groupe.
		       (\(df) {
				df = df |>
					mutate(
					       `ONS Geography` = case_when(
									   `ONS Geography` %in% c(
												  "E08000037", 
												  "E08000021", 
												  "E08000022", 
												  "E08000023", 
												  "E08000024"
												  ) ~ "E11000007",
							     `ONS Geography` %in% c(
										    "E08000001", 
										    "E08000002", 
										    "E08000003", 
										    "E08000004", 
										    "E08000005", 
										    "E08000006", 
										    "E08000007", 
										    "E08000008", 
										    "E08000009", 
										    "E08000010"
										    ) ~ "E11000001",
							     `ONS Geography` %in% c(
										    "E08000011",
										    "E08000012",
										    "E08000014",
										    "E08000013",
										    "E08000015"
										    ) ~ "E11000002",
							     `ONS Geography` %in% c(
										    "E08000016",
										    "E08000017",
										    "E08000018",
										    "E08000019"
										    ) ~ "E11000003",
							     `ONS Geography` %in% c(
										    "E08000032",
										    "E08000033",
										    "E08000034",
										    "E08000035",
										    "E08000036"
										    ) ~ "E11000006",
							     `ONS Geography` %in% c(
										    "E08000025",
										    "E08000026",
										    "E08000027",
										    "E08000028",
										    "E08000029",
										    "E08000030",
										    "E08000031"
										    ) ~ "E11000005",
							     `ONS Geography` %in% c(
										    "E09000007",
										    "E09000001",
										    "E09000012",
										    "E09000013",
										    "E09000014",
										    "E09000019",
										    "E09000020",
										    "E09000022",
										    "E09000023",
										    "E09000025",
										    "E09000028",
										    "E09000030",
										    "E09000032",
										    "E09000033"
										    ) ~ "E13000001",
							     `ONS Geography` %in% c(
										    "E09000002",
										    "E09000003",
										    "E09000004",
										    "E09000005",
										    "E09000006",
										    "E09000008",
										    "E09000009",
										    "E09000010",
										    "E09000011",
										    "E09000015",
										    "E09000016",
										    "E09000017",
										    "E09000018",
										    "E09000021",
										    "E09000024",
										    "E09000026",
										    "E09000027",
										    "E09000029",
										    "E09000031"
										    ) ~ "E13000002",
							     #			     # fusion des authorités unitaires dont les résultats sont extrêmes avec l'autorité locale la plus importante et la proche
							     #			     # !! il faudra corriger la pondération!
							     #			     # South Gloucestershire et North Somerset avec Bristol, City of
							     #			     `ONS Geography` %in% c(
							     #"E06000025",
							     #"E06000024"
							     #								  ) ~ "E06000023",
							     #			     # Stockton-on-Tees avec Middlesbrough, sous le nom de Stockton
							     #			     `ONS Geography` %in% c(
							     #"E06000002"
							     #								  ) ~ "E06000004",
							     #			     # Windsor and Maidenhead et Bracknell Forest avec outer london
							     #			     `ONS Geography` %in% c(
							     #						    "E06000036",
							     #						    "E06000040"
							     #								  ) ~ "E13000002",
							     TRUE ~ `ONS Geography`
					       )
					)
				df = aggregate(df$"Finished consultant episodes", by = df$"ONS Geography" |> list(), FUN = sum)
				names(df) = c("ONS.Geography", "Finished consultant episodes") 
				df
							})() 
		       # fin de lapply()
	}
		) |>
	    (\(ls_admi) {
		     ls_admi[[1]] |>
			     merge(ls_admi[[2]], by = "ONS.Geography", suffixes = c(".2017",".2018")) |>
			     merge(ls_admi[[3]], by = "ONS.Geography", suffixes = c(".2018",".2019")) |>
			     merge(ls_admi[[4]], by = "ONS.Geography", suffixes = c(".2019",".2020"))
		})() |>
	    (\(df) {
		     df[,"evo.FCE.2017-2019.2020"] = (df[, 5] - rowMeans(df[, 2:4])) / rowMeans(df[, 2:4])
		     df
		})() |>
	    (\(df) {
		     # df = df[grepl("E11", df$ONS.Geography), ]
		     # df = df[df[,"evo.FCE.2017-2019.2020"] >= -0.4 & df[,"evo.FCE.2017-2019.2020"] <= 0.1, ]
		     # df[order(df$"evo.FCE.2017-2019.2020"),]
		     df
		})() |>
	    _[c("ONS.Geography","evo.FCE.2017-2019.2020")] |>
	    print()
})

# E060000: ns 0
# E0600000: s 0
# E0600001: ns +
# E0600002: ns 0
# E0600003: ns 0
# E0600004: ns 0
# E0600005: ns pas assez de données
# E080000: ns 0
# E0800000: ns 0
# E0800001: ns +
# E0800002: ns -
# E0800003: ns 0
# E090000: ns 0
# E0900000: ns +
# E0900001: ns +
# E0900002: ns 0
# E0900003: ns -
# E100000: s -
# E1000000: s -
# E1000001: ns -
# E1000002: s -
# E1000003: s 0
# valeurs aberrantes! exemples: E06000040 6.24106464, E09000026 -0.89128306, E06000025 -0.99195423, etc. pourquoi nous n'avons pas les E07? (districts)
# !!!remember: renommer la liste clean_hosp_codes_list en all_data_list
years_tasks_list_morta = c(
			   "2017_morta" = "2017_morta",
			   "2018_morta" = "2018_morta",
			   "2019_morta" = "2019_morta",
			   "2020_morta" = "2020_morta"
) 
mortality = clean_hosp_codes_list[years_tasks_list_morta] |>
	map( ~ {
		    # donnerles noms exacts
		    .x["mortality"] = .x["Deaths of Persons all ages"] / .x["Populations Number (thousands) of Persons all ages"] 
		    .x[c("Area codes","mortality")]
}) |>
			   reduce(merge, by = "Area codes") |>
			   print()
		   # la mortalité est correcte, correspond aux fichiers xsl
		   names(mortality) = c("Area codes", "2017", "2018", "2019", "2020") 
		   mortality |>
			   as_tibble() |>
			   print()
		   evo_mortality = mortality |>
			   (\(df) {
				    df[,"evo.morta.2017-2019.2020"] = (df[, "2020"] - rowMeans(df[, c("2017","2018","2019")])) / rowMeans(df[, c("2017","2018","2019")])
				    df
}
		   )() |>
			   _[c("Area codes","evo.morta.2017-2019.2020")] |>
			   as_tibble() |>
			   print()
		   ponderation = clean_hosp_codes_list[["2020_morta"]] |>
			   _[c("Area codes","Populations Number (thousands) of Persons all ages")] |>
			   print()
		   corelation = evo_mortality |>
			   merge(evo.FCE.2017_2019.2020, by = 1) |>
			   merge(ponderation, by = 1) |>
			   as_tibble() |>
			   print()
		   wtd.cor(corelation[[2]], corelation[[3]], weight = corelation[[4]]) |>
			   print()
		   # Créer le fichier PNG
		   png("test.png", width = 1000, height = 1000)
		   # Dessiner le nuage de points
		   plot(
			x = corelation[[3]],
			y = corelation[[2]],
			cex = corelation[[4]]/500000,
			main = "Δ Mortalité vs Δ recours aux soins, 2020",
			xlab = "Δ recours aux soins",
			ylab = "Δ mortalité"
		   )
		   # Ajuster la régression linéaire et ajouter la ligne de tendance:
		   lm(corelation[[2]] ~ corelation[[3]], weights = corelation[[4]]) |>
			   abline(col = "red")
		   # Fermer le fichier PNG
		   dev.off()
