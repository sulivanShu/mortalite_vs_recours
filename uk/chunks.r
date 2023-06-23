#!/usr/bin/Rscript

library(readxl)
library(tibble)

hosp = read_excel("hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx")

hosp[7,c(2,3,49)] = c("Code", "Hospital.provider.name", "Zero.bed.day.cases.Emergency") |>
	as.list() |>
	print()

colnames(hosp) = hosp[7,] |>
	gsub("NA", NA, x=_) |>
	print()

replace_star_integer = \(x) {
	gsub("\\*", "3", x) |> as.integer() # valeur réelle entre 1 et 7. J'ai choisi 3 pour avoir un entier.
}
replace_star_numeric = \(x) {
	gsub("\\*", NA, x) |> as.numeric()
}
clean_hosp = hosp[19:509, names(hosp) |> (\(x) !is.na(x))() |> which()] |>
	(\(df) {
		 df[, -c(1,2,12,14,16)] = lapply(df[, -c(1,2,12,14,16)], replace_star_integer)
		 df[, c(12,14,16)] = lapply(df[, c(12,14,16)], replace_star_numeric)
		 df
})() |>
(\(df) {
	 df[order(df$"Code"),]
})() |>
print()

nhs_geo_code = read.csv("ODS_2023-06-20T170741.csv") |>
	as_tibble() |>
	print()

nhs2ons_code = read_excel("LA-Type-B-February-2020-4W5PA.xls", sheet = "LA - by type of care") |>
	print()

colnames(nhs2ons_code) = nhs2ons_code[12,] |>
	gsub("Code","Geographic.Local.Authority.Code", x=_) |>
	print()

clean_nhs2ons_code = nhs2ons_code[15:164,c("ONS Geography", "Geographic.Local.Authority.Code")] |>
(\(df) {
	 df[order(df$"Geographic.Local.Authority.Code"),]
})() |>
	print()

nhs_geo_code_clean_hosp = merge(nhs_geo_code, clean_hosp, by = "Code") |>
	(\(df) {
		 df[order(df$"Geographic.Local.Authority.Code"),]
})() |>
merge(clean_nhs2ons_code, by = "Geographic.Local.Authority.Code") |>
#`[`(x = _, c("ONS Geography", "Geographic.Local.Authority.Code", "Admissions")) |> 
`[`(x = _, "Geographic.Local.Authority.Code") |> 
unique() |>
as_tibble() |>
print()

#help(which)
