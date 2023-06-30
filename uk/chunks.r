#!/usr/bin/Rscript

library(readxl)
library(tibble)
# la librairie tibble n'est pas nécessaire, mais elle est utilisée pour mieux voir les dataframes

# lire le fichier qui contient les indicateurs de recours aux soins hospitaliers
# l'accès aux bases de données est soumise à autorisation: https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics/users-uses-and-access-to-hospital-episode-statistics
hosp = read_excel("hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx")

# éditer une ligne qui servira ensuite de noms pour les colonnes. "Code" est le code NHS de l'établissement.
hosp[7,c(2,3,49)] = c("Code", "Hospital.provider.name", "Zero.bed.day.cases.Emergency") |>
	as.list() |>
	print()

# renommer des colonnes
colnames(hosp) = hosp[7,] |>
	gsub("NA", NA, x=_) |>
	print()

# nettoyage du fichier. dans le fichier, le caractère * signfie soit un nombre entre 1 et 7, soit "Not available".
replace_star_integer = \(x) {
	gsub("\\*", "3", x) |> as.integer() # valeur réelle entre 1 et 7. J'ai choisi 3 pour avoir un entier. Possibilité d'être plus précis.
}
replace_star_numeric = \(x) {
	gsub("\\*", NA, x) |> as.numeric()
}
# les codes terminant par "-X" sont des codes de réserve provisoires. Il faut supprimer ce suffixe pour avoir le code normal. reste encore "8A718 London Clinic" et "8HE28 Harley Street On 15" à gérer. Apparemment, ce sont des "Non-Nhs organisations", c'est peut-être pour ça qu'ils n'apparaissent pas dans mon autre fichier. absents de https://odsdatapoint.digital.nhs.uk/userdefined . Présent à https://odsportal.digital.nhs.uk/ . 8HE28 : GREATER LONDON . 8A718 : GREATER LONDON .
replace_X_code = \(x) {
	gsub("-X", "", x)
}
# lignes 19 à 509, colonnes dont le nom n'est pas NA (colonnes vides)
clean_hosp = hosp[19:509, hosp |> names() |> (\(x) !is.na(x))()] |>
	(\(df) {
		 df[, -c(1,2,12,14,16)] = lapply(df[, -c(1,2,12,14,16)], replace_star_integer)
		 df[, c(12,14,16)] = lapply(df[, c(12,14,16)], replace_star_numeric)
		 df[, "Code"] = lapply(df[, "Code"], replace_X_code)
		 df
#		 df |>
#			 subset(`Finished consultant episodes` == "3") |>
#			 print()
		 # df[df$`Finished consultant episodes` %in% grep("3", df$`Finished consultant episodes`, value = TRUE), ]
})() |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
print()

clean_hosp |>
	subset(Code %in% badcode) |>
	`[`(x = _, c(1:3)) |> 
	print()

clean_hosp[,"Code"] |>
	print()

# "Code" est le code NHS de l'établissement, et "Geographic.Local.Authority.Code" son code géographique NHS (pas ONS).
nhs_geo_code = read.csv("ODS_2023-06-20T170741.csv") |>
# ajout des codes pour "London Clinic" et "Harley Street On 15", qui n'ont pas de code dans le document original.
# selon:
# https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=67432&showOpenChildredOnly=True
# https://www.streetcheck.co.uk/postcode/w1g6bw
# "8A718 London Clinic" a pour autorité administrative: Westminster (ONS: E09000033, NHS: 713)
# selon:
# https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=75403&showOpenChildredOnly=True
# https://www.streetcheck.co.uk/postcode/nw12bu
# "8HE28 Harley Street On 15" a pour autorité administrative: Camden (ONS: E09000007, NHS: 702)
	add_row(Code = "8A718",Geographic.Local.Authority.Code = "713",Geographic.Local.Authority.Name = "CITY OF WESTMINSTER") |>
	add_row(Code = "8HE28",Geographic.Local.Authority.Code = "702",Geographic.Local.Authority.Name = "LONDON BOROUGH OF CAMDEN") |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
	as_tibble() |>
	print()

# fichier de correspondance trouvé par hasard à : https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/LA-Type-B-February-2020-4W5PA.xls
# d'autres fichiers de correspondance plus anciens existent
nhs2ons_code = read_excel("LA-Type-B-February-2020-4W5PA.xls", sheet = "LA - by type of care") |>
	print(n = 20)

colnames(nhs2ons_code) = nhs2ons_code[12,] |>
	gsub("Code","Geographic.Local.Authority.Code", x=_) |>
	print()

clean_nhs2ons_code = nhs2ons_code[15:164,c("ONS Geography", "Geographic.Local.Authority.Code")] |>
(\(df) {
	 df[order(df$"Geographic.Local.Authority.Code"),]
})() |>
	print()

# fusion des fichiers nhs_geo_code et clean_hosp selon le code de l'établissement. permet d'associer le code NHS de l'établissement à son code géographique NHS.
nhs_geo_code_clean_hosp = merge(nhs_geo_code, clean_hosp, by = "Code") |>
	(\(df) {
		 df[order(df$"Geographic.Local.Authority.Code"),]
})() |>
# fusion avec le fichier clean_nhs2ons_code. Permet d'associer le code géographique NHS au code géographique ONS.
merge(clean_nhs2ons_code, by = "Geographic.Local.Authority.Code") |>
`[`(x = _, c("ONS Geography", "Hospital.provider.name", "Admissions")) |> 
# grouper par code géographique ONS et sommer les éléments de chaque groupe.
(\(df) {
	 df = aggregate(df$Admissions, by = df$`ONS Geography` |> list(), FUN = sum)
	 names(df) = c("ONS.Geography", "Admission") 
	 df
})() |>
as_tibble() |>
print()

help("assign")
