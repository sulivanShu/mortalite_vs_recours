#!/usr/bin/Rscript

library(readxl)
library(tibble)
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
		       "2017" = "2017",
		       "2018" = "2018",
		       "2019" = "2019",
		       "2020" = "2020",
		       "nhs_geo_code" = "nhs_geo_code",
		       "nhs2ons_code" = "nhs2ons_code"
	)
	clusterEvalQ(cl, {
			     hosp_list = c(
					   "2017" = "hosp-epis-stat-admi-prov-2017-18-tab.xlsx",
					   "2018" = "hosp-epis-stat-admi-prov-2018-19-tab.xlsx",
					   "2019" = "hosp-epis-stat-admi-prov-2019-20-tab.xlsx",
					   "2020" = "hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx"
			     )
	})
	clean_hosp_codes_list =
		tasks_list |> 
		parLapply(cl = NULL, function(task) {
				  # Traitements en parallèle.
				  # Sur un ordinateur multicore, parLapply() est deux fois plus rapide que lapply()
				  # Traitement des fichiers xlsx, sur les statistiques de recours aux soins par fournisseurs de soins
				  if (task %in% c("2017", "2018", "2019", "2020"))
				  {
					  # Chargement des fichiers. Le numéro de chaque tâche dans le vecteur tasks_list (2017:2020) est aussi le numéro de chaque fichier dans le vecteur hosp_list. On peut donc réutiliser cet argument. 
					  hosp = hosp_list[task] |>
						  read_excel() |>
						  print()
					  # Traitement
					  if (task == "2020") {
						  hosp = hosp[,-1]
					  }
					  hosp[7,c(1,2,48)] =
						  c("Code", "Hospital.provider.name", "Zero.bed.day.cases.Emergency") |>
						  as.list() |>
						  print()
					  # Renommer des colonnes
					  colnames(hosp) =
						  hosp[7,] |>
						  gsub("NA", NA, x=_) |>
						  print()
					  # Sélectionner les lignes pertinentes, qui sont différentes pour chaque fichier.
					  if (task == "2017") {
						  hosp = hosp[16:499,]
					  } else if (task == "2018") {
						  hosp = hosp[17:487,]
					  } else if (task == "2019") {
						  hosp = hosp[19:502,]
					  } else if (task == "2020") {
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
			     })() |>
			  print()
		  return(clean_hosp)
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
			  as_tibble() |>
			  print()
		  return(nhs_geo_code)
				  }
				  if (task == "nhs2ons_code") {
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
							})() |>
			  print()
		  return(clean_nhs2ons_code)
				  }
				  # Fin de la fonction parLapply()
	}) |>
	    print()
})

capabilities("fifo")

help("tempfile")

getOption("encoding")

help("fifo")

packageVersion("base")

tempfile(fileext = ".fifo")


zz <- tempfile() |>
	fifo("w+")
c("azerty") |>
	writeLines(zz)
readLines(zz) 
close(zz)

clean_hosp_codes_vector =
	clean_hosp_codes_list |>
	unlist() |>
	sort() |>
	unique() |>
	unname() |>
	print()



nhs_geo_code =
	read.csv("ODS_2023-06-20T170741.csv") |>
	_$"Code" |>
	print()

setdiff(c(1,2,3),c(1,2))

setdiff(clean_hosp_codes_vector, nhs_geo_code)


rbind(data.frame(
		 Code = c("8A718","8HE28"),
		 Geographic.Local.Authority.Code = c("713","702"),
		 Geographic.Local.Authority.Name = c("CITY OF WESTMINSTER", "LONDON BOROUGH OF CAMDEN")
		 )) 

help("%in%")

help("union")

setDefaultCluster(NULL)
stopCluster(cl)

# # Correspondances

# Les objets dont le nom commence par "clean_" sont des objets nettoyés, utilisables dans les calculs

# ## Correspondance `Code` -- `Finished consultant episodes` (Code NHS de l'établissement vs indicateur de recours au soin) 
# lire le fichier qui contient les indicateurs de recours aux soins hospitaliers
hosp =
	read_excel("hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx")

# Éditer une ligne qui servira ensuite de noms pour les colonnes. "Code" est le code NHS de l'établissement.
hosp[7,c(2,3,49)] =
	c("Code", "Hospital.provider.name", "Zero.bed.day.cases.Emergency") |>
	as.list() |>
	print()

# Renommer des colonnes
colnames(hosp) =
	hosp[7,] |>
	gsub("NA", NA, x=_) |>
	print()

# Nettoyage du fichier 
# Dans le fichier, le caractère * signfie soit un nombre entre 1 et 7, soit "Not available".
# Fonction pour remplacer le caractère "*" par une valeur numérique. J'ai choisi 3 pour avoir un entier entre 1 et 7. Possibilité d'être plus précis.
replace_star_by_3 =
	\(x) {
		gsub("\\*", "3", x) |> as.integer() 
	}
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
# Sélectionner les cellules utiles du tableau. Lignes 19 à 509, colonnes dont le nom n'est pas NA (colonnes non-vides)
clean_hosp =
	hosp[19:509, hosp |> names() |> (\(x) !is.na(x))()] |>
	(\(df) {
		 df[, -c(1,2,12,14,16)] = lapply(df[, -c(1,2,12,14,16)], replace_star_by_3)
		 df[, c(12,14,16)] = lapply(df[, c(12,14,16)], replace_star_by_NA)
		 df[, "Code"] = lapply(df[, "Code"], replace_X_code)
		 df
})() |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
_[c("Code","Finished consultant episodes")] |> 
print()

## Correspondance `Code` -- `Geographic.Local.Authority.Code` (Code NHS de l'établissement -- code NHS géographique) 
# Ajout manuel des hôpitaux qui manquent.
nhs_geo_code =
	read.csv("ODS_2023-06-20T170741.csv") |>
	rbind(
	      read.csv("genealogie.csv") |>
		      _[,c("Code",
			   "Geographic.Local.Authority.Code",
			   "Geographic.Local.Authority.Name")]
	      ) |>
(\(df) {
	 df[order(df$"Code"),]
	      })() |>
_$Geographic.Local.Authority.Code |>
unique() |>
print()

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
	      })() |>
_$Geographic.Local.Authority.Code |>
unique() |>
print()

help("$")

# ## Fusion des fichiers clean_hosp et nhs_geo_code selon `Code` (le code de l'établissement)

# Permet d'associer le code NHS de l'établissement à son code géographique NHS.

nhs_geo_code_clean_hosp =
	merge(nhs_geo_code, clean_hosp, by = "Code") |>
	(\(df) {
		 df[order(df$"Geographic.Local.Authority.Code"),]
	      }
	)() |>
# Fusion du résultat avec le fichier clean_nhs2ons_code. Permet d'associer le code géographique NHS au code géographique ONS.
merge(clean_nhs2ons_code, by = "Geographic.Local.Authority.Code") |>
_[c("ONS Geography", "Finished consultant episodes")] |> 
# grouper par code géographique ONS et sommer les éléments de chaque groupe.
(\(df) {
	 df = aggregate(df$`Finished consultant episodes`, by = df$`ONS Geography` |> list(), FUN = sum)
	 names(df) = c("ONS.Geography", "Finished consultant episodes") 
	 df
	})() |>
as_tibble() |>
print()
