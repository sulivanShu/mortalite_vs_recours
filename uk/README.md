---
title: "Corrélation entre la baisse du recours aux soins et la hausse de la mortalité en 2020, Royaume-Uni"
author: ""
date: "2023-07-02"
output: html_document
---

# Corrélation entre la baisse du recours aux soins et la hausse de la mortalité en 2020, Royaume-Uni

## Bibliothèques & API

Il n'existe pas de bibliothèque R similaire à `insee` pour le NHS, l'agence de santé britannique.

Il existe une API, mais son accès est soumise à autorisation.

https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics/users-uses-and-access-to-hospital-episode-statistics

## Sources

### Recours au soin

https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity
https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2020-21
https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2019-20
https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2018-19
https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2017-18

Sections:

Hospital Admitted Patient Care Activity, 2020-21: Hospital providers
Hospital Admitted Patient Care Activity, 2019-20: Hospital providers
Hospital Admitted Patient Care Activity, 2018-19: Hospital providers
Hospital Admitted Patient Care Activity, 2017-18: Hospital providers

hosp-epis-stat-admi-hosp-prov-2015-16-tab.xlsx
hosp-epis-stat-admi-hosp-prov-2016-17-tab.xlsx
hosp-epis-stat-admi-hosp-prov-2020-21-tab.xlsx
hosp-epis-stat-admi-hosp-prov-2021-22-tab.xlsx
hosp-epis-stat-admi-prov-2017-18-tab.xlsx
hosp-epis-stat-admi-prov-2018-19-tab.xlsx
hosp-epis-stat-admi-prov-2019-20-tab.xlsx

pas d'erreur sur les noms, il s'agit bien de ces noms-ci.

ces fichiers contiennent le nom de tous les hôpitaux d'Angleterre, leur identifiant, et des indicateurs de recours aux soins. L'indicateur utilisé est "Finished consultant episodes".

### Code géographique NHS

https://odsdatapoint.digital.nhs.uk/userdefined
DataPointSavedGridState.json

ODS_2023-06-20T170741.csv

Ce fichier indique les codes géographiques NHS et postaux des hôpitaux d'Angleterre, à l'exeption de "8A718 London Clinic" et "8HE28 Harley Street On 15" qui doivent être ajoutés manuellement.

Selon:

https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=67432&showOpenChildredOnly=True

https://www.streetcheck.co.uk/postcode/w1g6bw

"8A718 London Clinic" a pour autorité administrative: Westminster (ONS: E09000033, NHS: 713)

Selon:

https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=75403&showOpenChildredOnly=True

https://www.streetcheck.co.uk/postcode/nw12bu

"8HE28 Harley Street On 15" a pour autorité administrative: Camden (ONS: E09000007, NHS: 702)

# Code géographique ONS

https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/delayed-transfers-of-care-data-2019-20/

Total Delayed Days Local Authority 2019-20 February (revised 10.09.2020) (XLS, 138KB)

LA-Type-B-February-2020-4W5PA.xls

Ce fichier comprend la correspondance des codes géographiques NHS avec les codes géographiques ONS, ce qui permet de croiser les données de recours au soins du NHS avec les données de mortalité de l'ONS.

Cette correspondance peut également être effectuée à l'aide des codes postaux des hôpitaux, mis en correspondance avec les codes ONS des autorités administratives, à l'aide d'un autre fichier (non fourni)




