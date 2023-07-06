#!/usr/bin/Rscript

library(readxl)
# La bibliothèque readxl n'est nécessaire que pour les noeuds du cluster, si tous les calculs sont faits dans le cluster.
library(tibble)
# la librairie tibble n'est pas nécessaire, mais elle est utilisée pour mieux voir les dataframes
library(parallel)

# options(mc.cores = detectCores()) 
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
		       "2020" = "2020"
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
 				  hosp = hosp_list[task] |>
 					  read_excel() |>
 					  print()
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
				  # Appliquer une fonction différente à chaque élément de tasks_list
				  if (task == "2017") {
					  # Code à exécuter pour l'élément "2017"
					  # ...
					  hosp = hosp[16:499,]
				  } else if (task == "2018") {
					  # Code à exécuter pour l'élément "2018"
					  # ...
					  hosp = hosp[17:487,]
				  } else if (task == "2019") {
					  # Code à exécuter pour l'élément "2019"
					  # ...
					  hosp = hosp[19:502,]
				  } else if (task == "2020") {
					  # Code à exécuter pour l'élément "2020"
					  # ...
					  hosp = hosp[19:509,]
				  }
				  # Nettoyage du fichier 
				  # Dans le fichier, le caractère * signfie soit un entier entre 1 et 7, soit "Not available".
				  # Vecteur des colonnes pour lesquelles "*" signifie un entier entre 1 et 7.
				  stars_rows = c( "Finished consultant episodes", "Admissions", "Male", "Female", "Gender Unknown", "Emergency", "Waiting list", "Planned", "Other Admission Method", "Age 0", "Age 1-4", "Age 5-9", "Age 10-14", "Age 15", "Age 16", "Age 17", "Age 18", "Age 19", "Age 20-24", "Age 25-29", "Age 30-34", "Age 35-39", "Age 40-44", "Age 45-49", "Age 50-54", "Age 55-59", "Age 60-64", "Age 65-69", "Age 70-74", "Age 75-79", "Age 80-84", "Age 85-89", "Age 90+", "Day case", "FCE bed days", "Zero.bed.day.cases.Emergency", "Elective", "Other")
				  # Fonction pour remplacer le caractère "*" par une valeur numérique. J'ai choisi 3 pour avoir un entier entre 1 et 7. Possibilité d'être plus précis.
				  replace_star_integer =
					  \(x) {
						  gsub("\\*", "3", x) |> as.integer() 
					  }
				  # Vecteur des colonnes pour lesquelles "*" signifie NA.
				  means_medians_rows = c("Mean time waited", "Median time waited", "Mean length of stay", "Median length of stay", "Mean age")
				  # Fonction pour remplacer le caratère "*" par la valeur "Not available".
				  replace_star_numeric =
					  \(x) {
						  gsub("\\*", NA, x) |> as.numeric()
					  }
				  # Vecteur des colonnes pour lesquelles "*" signifie un entier entre 1 et 7.
				  # Les codes se terminant par "-X" sont des codes de réserve provisoires. Il faut supprimer ce suffixe pour avoir le code normal. 
				  replace_X_code =
					  \(x) {
						  gsub("-X", "", x)
					  }
				  clean_hosp =
					  hosp[,hosp |> names() |> (\(x) !is.na(x))()] |>
					  (\(df) {
						   df[, "Code"] = lapply(df[, "Code"], replace_X_code)
						   df[,means_medians_rows] = lapply(df[,means_medians_rows], replace_star_numeric)
						   df[,stars_rows] = lapply(df[,stars_rows], replace_star_integer)
						   df
			     })()
				  clean_hosp[,c("Code", "Finished consultant episodes")] |>
					  (\(df) {
						   df[order(df$"Code"),]
			     })() |>
					  print()
	}) |>
	    lapply(function(df) subset(df, Code %in% c("8A718",	"8G301",	"8HE28",	"8J285",	"NT438",	"NYW20",	"R1E",	"R1J",	"RA3",	"RBA",	"RC1",	"RDD",	"RE9",	"RGQ",	"RJF",	"RLN",	"RNL",	"RQ6",	"RQ8",	"RR1",	"RXKTC",	"RY1"))) |>
	    print()
})

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


"source: wikipédia, principalement"

"8A718",	"713",	"CITY OF WESTMINSTER",	"Non-Nhs organisation - LONDON CLINIC"
"8G301",	"E07000227",	"Horsham",	"disparu des radars en 2019. 90 consultations en 2018. pas de fermeture enregistrée. Centre de psychothérapie."
"8HE28",	"702",	"LONDON BOROUGH OF CAMDEN",	"Non-Nhs organisation - HARLEY STREET ON 15"
"8J285",	"415",	"HEREFORDSHIRE COUNCIL",	"Non-Nhs organisation - SHAW HEALTHCARE,	"LEDBURY" LIMITED"
"NT438",	"E07000110",	"Maidstone",	"In April 2019, the firm closed the Somerfield Hospital in Maidstone. Fernbrae Hospital in Dundee is to close in May 2019."
"NYW20",	"E07000070",	"Chelmsford",	"This hospital has closed.,	"31/03/2019"" 
"R1E",	"E07000195",	"Newcastle-under-Lyme",	"It merged with the South Staffordshire and Shropshire Healthcare NHS Foundation Trust in 2018 forming a new organisation called Midlands Partnership NHS Foundation Trust.",	"413"
"R1J",	"E07000083",	"Tewkesbury",	"In September 2017 the trust announced plans to merge with 2gether NHS Foundation Trust, which also covers Herefordshire.[6] The merger was completed in October 2019 to form Gloucestershire Health and Care NHS Foundation Trust.",	"904"
"RA3",	"910",	"NORTH SOMERSET COUNCIL",	"WESTON AREA HEALTH NHS TRUST In January 2018, it was announced that the Weston trust was to merge with University Hospitals Bristol NHS Foundation Trust, which runs eight hospitals in the nearby city of Bristol.[12] On 1 April 2020, the merger was completed with the formation of the University Hospitals Bristol and Weston NHS Foundation Trust.",	"909"
"RBA",	"905",	"SOMERSET COUNCIL",	"In 2018, the trust announced plans to merge with Somerset Partnership NHS Foundation Trust, a mental health trust.[3] The merger completed in 2020.",	"905"
"RC1",	"625",	"BEDFORD BOROUGH COUNCIL",	"In September 2017 plans were announced to merge the Bedford Hospital NHS Trust with Luton and Dunstable University Hospital NHS Foundation Trust",	"611".
"RDD",	"E07000066",	"Basildon",	"A merger with Southend University Hospital NHS Foundation Trust and Mid Essex Hospital Services NHS Trust was proposed in January 2018.[3] On 31 July 2019 the Secretary of State for Health and Social Care endorsed the merger and a provisional date of 1 April 2020 was agreed. [in the Mid and South Essex NHS Foundation Trust]",	"621"
"RE9",	"109",	"SOUTH TYNESIDE COUNCIL",	"It merged with City Hospitals Sunderland NHS Foundation Trust to form South Tyneside and Sunderland NHS Foundation Trust in April 2019.",	"110"
"RGQ",	"E07000202",	"Ipswich",	"It is now managed by East Suffolk and North Essex NHS Foundation Trust which was formed on 1 July 2018 by the merging of Ipswich Hospital NHS Trust with Colchester Hospital University NHS Foundation Trust.",	"620"
"RJF",	"?",	"East Staffordshire",	"The trust merged with Derby Teaching Hospitals NHS Foundation Trust to form University Hospitals of Derby and Burton NHS Foundation Trust in July 2018.",	"507"
"RLN",	"110",	"SUNDERLAND CITY COUNCIL",	"In May 2018 it agreed to merge with South Tyneside NHS Foundation Trust to form South Tyneside and Sunderland NHS Foundation Trust.",	"110" 
"RNL",	"H6X4G",	"CUMBERLAND COUNCIL",	"in 2018 it proposed to merge with Cumbria Partnership NHS Foundation Trust.[2] The merger took place in October 2019. The new organisation is called North Cumbria Integrated Care NHS Foundation Trust.",	"H6X4G"
"RQ6",	"?",	"Liverpool",	"It merged with the Aintree University Hospitals NHS Foundation Trust to form the Liverpool University Hospitals NHS Foundation Trust on 1 October 2019.",	"316"
"RQ8",	"Chelmsford",	"A merger with Basildon and Thurrock University Hospitals NHS Foundation Trust and Southend University Hospital NHS Foundation Trust was proposed in January 2018. On 31 July 2019 the Secretary of State for Health and Social Care endorsed the merger and a provisional date of 1 April 2020 was agreed. ----- Southend University Hospital NHS Foundation Trust was an NHS foundation trust which ran Southend University Hospital. It merged with two other trusts to form Mid and South Essex NHS Foundation Trust on 1 April 2020.",	"621"
"RR1",	"Birmingham",	"In September 2016 HEFT announced plans to merge with the University Hospitals Birmingham NHS Foundation Trust.[2] The merger took place on 1 April 2018.",	"406"
"RXKTC",	"406",	"BIRMINGHAM CITY COUNCIL",	"hopital"
"RY1"	"316",	"LIVERPOOL CITY COUNCIL" "The Trust entered a dispersal process under NHS England instruction. South Sefton CCG awarded their contract to Mersey Care NHS Foundation Trust. Bridgewater Community Healthcare NHS Foundation Trust was to take over the trust's contract in the Liverpool City Council boundaries. This was abandoned in March 2017 by Liverpool Clinical Commissioning Group after concerns over the due diligence process overseen by NHS Improvement arose.",	"probablement 316"

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
replace_star_integer =
	\(x) {
		gsub("\\*", "3", x) |> as.integer() 
	}
# Fonction pour remplacer le caratère "*" par la valeur "Not available".
replace_star_numeric =
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
		 df[, -c(1,2,12,14,16)] = lapply(df[, -c(1,2,12,14,16)], replace_star_integer)
		 df[, c(12,14,16)] = lapply(df[, c(12,14,16)], replace_star_numeric)
		 df[, "Code"] = lapply(df[, "Code"], replace_X_code)
		 df
})() |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
_[c("Code","Finished consultant episodes")] |> 
print()

# ## Correspondance `Code` -- `Geographic.Local.Authority.Code` (Code NHS de l'établissement -- code NHS géographique) 

# # Ajout manuel des hôpitaux "London Clinic" et "Harley Street On 15" qui manquent.

nhs_geo_code =
	read.csv("ODS_2023-06-20T170741.csv") |>
	rbind(data.frame(
			 Code = c("8A718","8HE28"),
			 Geographic.Local.Authority.Code = c("713","702"),
			 Geographic.Local.Authority.Name = c("CITY OF WESTMINSTER", "LONDON BOROUGH OF CAMDEN")
			 )) |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
as_tibble() |>
print()

# ## Correspondance `Geographic.Local.Authority.Code` et `ONS Geography` (NHS geographic code -- ONS geographic code)

nhs2ons_code =
	read_excel("LA-Type-B-February-2020-4W5PA.xls", sheet = "LA - by type of care") |>
	print(n = 20)

colnames(nhs2ons_code) =
	nhs2ons_code[12,] |>
	gsub("Code","Geographic.Local.Authority.Code", x=_) |>
	print()

clean_nhs2ons_code =
	nhs2ons_code[15:164,c("ONS Geography", "Geographic.Local.Authority.Code")] |>
	(\(df) {
		 df[order(df$"Geographic.Local.Authority.Code"),]
})() |>
print()

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
