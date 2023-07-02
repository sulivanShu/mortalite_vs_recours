# ---
# title: "Corrélation entre la baisse du recours aux soins et la hausse de la mortalité en 2020, Royaume-Uni"
# author: ""
# date: "2023-07-02"
# output: html_document
# ---
# 
# # Corrélation entre la baisse du recours aux soins et la hausse de la mortalité en 2020, Royaume-Uni
# 
# ## Bibliothèques & API
# 
# Il n'existe pas de bibliothèque R similaire à `insee` pour le NHS, l'agence de santé britannique.
# 
# Il existe une API, mais son accès est soumise à autorisation.
# 
# https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics/users-uses-and-access-to-hospital-episode-statistics
# 
# ## Sources
# 
# ### Recours au soin
# 
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2020-21
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2019-20
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2018-19
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2017-18
# 
# Section:
# 
# Hospital Admitted Patient Care Activity, 2020-21: Provider-level analysis
# Hospital Admitted Patient Care Activity, 2019-20: Provider-level analysis
# Hospital Admitted Patient Care Activity, 2018-19: Provider-level analysis
# Hospital Admitted Patient Care Activity, 2017-18: Provider-level analysis
# 
# hosp-epis-stat-admi-hosp-prov-2015-16-tab.xlsx
# hosp-epis-stat-admi-hosp-prov-2016-17-tab.xlsx
# hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx
# hosp-epis-stat-admi-hosp-prov-2021-22-tab.xlsx
# hosp-epis-stat-admi-prov-2017-18-tab.xlsx
# hosp-epis-stat-admi-prov-2018-19-tab.xlsx
# hosp-epis-stat-admi-prov-2019-20-tab.xlsx
# 
# ces fichiers contiennent le nom de tous les hôpitaux d'Angleterre, leur identifiant, et des indicateurs de recours aux soins. L'indicateur utilisé est "Finished consultant episodes".
# 
# ### Code géographique NHS
# 
# https://odsdatapoint.digital.nhs.uk/userdefined
# DataPointSavedGridState.json
# 
# ODS_2023-06-20T170741.csv
# 
# Ce fichier indique les codes géographiques NHS et postaux des hôpitaux d'Angleterre, à l'exeption de "8A718 London Clinic" et "8HE28 Harley Street On 15" qui doivent être ajoutés manuellement.
# 
# selon:
# 
# https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=67432&showOpenChildredOnly=True
# 
# https://www.streetcheck.co.uk/postcode/w1g6bw
# 
# "8A718 London Clinic" a pour autorité administrative: Westminster (ONS: E09000033, NHS: 713)
# 
# selon:
# 
# https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=75403&showOpenChildredOnly=True
# 
# https://www.streetcheck.co.uk/postcode/nw12bu
# 
# "8HE28 Harley Street On 15" a pour autorité administrative: Camden (ONS: E09000007, NHS: 702)
# 
# # Code géographique ONS
# 
# https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/delayed-transfers-of-care-data-2019-20/
# 
# Total Delayed Days Local Authority 2019-20 February (revised 10.09.2020) (XLS, 138KB)
# 
# LA-Type-B-February-2020-4W5PA.xls
# 
# Ce fichier comprend la correspondance des codes géographiques NHS avec les codes géographiques ONS, ce qui permet de croiser les données de recours au soins du NHS avec les données de mortalité de l'ONS.
# 
# Cette correspondance peut également être effectuée à l'aide des codes postaux des hôpitaux, mis en correspondance avec les codes ONS des autorités administratives, à l'aide d'un autre fichier (non fourni)



#!/usr/bin/Rscript

library(readxl)
library(tibble)
# la librairie tibble n'est pas nécessaire, mais elle est utilisée pour mieux voir les dataframes

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
`[`(x = _, c("Code","Finished consultant episodes")) |> 
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
# fusion avec le fichier clean_nhs2ons_code. Permet d'associer le code géographique NHS au code géographique ONS.
	merge(clean_nhs2ons_code, by = "Geographic.Local.Authority.Code") |>
	`[`(x = _, c("ONS Geography", "Finished consultant episodes")) |> 
# grouper par code géographique ONS et sommer les éléments de chaque groupe.
	(\(df) {
		 df = aggregate(df$`Finished consultant episodes`, by = df$`ONS Geography` |> list(), FUN = sum)
		 names(df) = c("ONS.Geography", "Finished consultant episodes") 
		 df
	})() |>
	as_tibble() |>
	print()

help("[.data.frame")

help("data.frame")

help("subset")

data.frame(x = 1, y = 1:10) |>
	as_tibble() |>
	print()

??parallel

data.frame(col1 = c(1, 2, 3), col2 = c("a", "b", "c"), col3 = c(TRUE, FALSE, TRUE)) |>
	subset(select = c(1,2)) |>
	print()

	`[`(x = _, 



	subset(select = c(col1, col2))
