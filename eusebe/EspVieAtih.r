#!/usr/bin/Rscript
# Auteur original: Eusèbe Rioché
# Publié sur github sous licence GPL3 avec l'autorisation de l'auteur

library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(foreign)

deps <- tibble(
  dep = c(
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22",
    "23", "24", "25", "26", "27", "28", "29", "2A", "2B", "30", "31",
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
    "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53",
    "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64",
    "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75",
    "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86",
    "87", "88", "89", "90", "91", "92", "93", "94", "95", "971", "972",
    "973", "974", "976"),
  nom = c(
    "FRANCE", "AIN", "AISNE", "ALLIER", "ALPES-DE-HAUTE-PROVENCE", "HAUTES-ALPES",
    "ALPES-MARITIMES", "ARDÈCHE", "ARDENNES", "ARIÈGE", "AUBE", "AUDE", "AVEYRON",
    "BOUCHES-DU-RHÔNE", "CALVADOS", "CANTAL", "CHARENTE", "CHARENTE-MARITIME", "CHER",
    "CORRÈZE", "CÔTE-D'OR", "CÔTES-D'ARMOR", "CREUSE", "DORDOGNE", "DOUBS", "DRÔME",
    "EURE", "EURE-ET-LOIR", "FINISTÈRE", "CORSE-DU-SUD", "HAUTE-CORSE", "GARD",
    "HAUTE-GARONNE", "GERS", "GIRONDE", "HÉRAULT", "ILLE-ET-VILAINE", "INDRE",
    "INDRE-ET-LOIRE", "ISÈRE", "JURA", "LANDES", "LOIR-ET-CHER", "LOIRE", "HAUTE-LOIRE",
    "LOIRE-ATLANTIQUE", "LOIRET", "LOT", "LOT-ET-GARONNE", "LOZÈRE", "MAINE-ET-LOIRE",
    "MANCHE", "MARNE", "HAUTE-MARNE", "MAYENNE", "MEURTHE-ET-MOSELLE", "MEUSE", "MORBIHAN",
    "MOSELLE", "NIÈVRE", "NORD", "OISE", "ORNE", "PAS-DE-CALAIS", "PUY-DE-DÔME",
    "PYRÉNÉES-ATLANTIQUES", "HAUTES-PYRÉNÉES", "PYRÉNÉES-ORIENTALES", "BAS-RHIN",
    "HAUT-RHIN", "RHÔNE", "HAUTE-SAÔNE", "SAÔNE-ET-LOIRE", "SARTHE", "SAVOIE", "HAUTE-SAVOIE",
    "PARIS", "SEINE-MARITIME", "SEINE-ET-MARNE", "YVELINES", "DEUX-SÈVRES", "SOMME",
    "TARN", "TARN-ET-GARONNE", "VAR", "VAUCLUSE", "VENDÉE", "VIENNE", "HAUTE-VIENNE",
    "VOSGES", "YONNE", "TERRITOIRE DE BELFORT", "ESSONNE", "HAUTS-DE-SEINE", "SEINE-SAINT-DENIS",
    "VAL-DE-MARNE", "VAL-D'OISE", "GUADELOUPE", "MARTINIQUE", "GUYANE", "LA RÉUNION", "MAYOTTE"
  )
)

recupInseePop2020 <- function(recharge = FALSE) {
  if (! dir.exists("Sources/Pop")) {
    if (! dir.exists("Sources")) {
      dir.create("Sources")
    }
    dir.create("Sources/Pop")
    recharge <- TRUE
  }
  if (recharge) {
    download.file(
      url = "https://www.insee.fr/fr/statistiques/fichier/6683035/ensemble.zip",
      destfile = "Sources/Pop/popDep2020.zip"
    )
    unzip("Sources/Pop/popDep2020.zip", exdir = "Sources/Pop")
  }
  read.table(
    file = "Sources/Pop/donnees_departements.csv",
    header = TRUE,
    sep = ";",
    quote = "\"",
    dec = ".",
    colClasses = rep("character", 9),
    stringsAsFactors = FALSE
  ) %>%
  tibble() %>%
  select(CODDEP, PTOT) %>%
  rename(dep = CODDEP, pop2020 = PTOT) %>%
  mutate(pop2020 = as.numeric(pop2020)) %>%
  arrange(dep)
}

popDeps <- recupInseePop2020()

recupInseeDcHospCovid2020 <- function(recharge = FALSE) {
  if (! dir.exists("Sources/Covid")) {
    if (! dir.exists("Sources")) {
      dir.create("Sources")
    }
    dir.create("Sources/Covid")
    recharge <- TRUE
  }
  if (recharge) {
    download.file(
      url = "https://www.data.gouv.fr/fr/datasets/r/5c4e1452-3850-4b59-b11c-3dd51d7fb8b5",
      destfile = "Sources/Covid/table-indicateurs-open-data-dep.csv"
    )
  }
  read.table(
    file = "Sources/Covid/table-indicateurs-open-data-dep.csv",
    header = TRUE,
    sep = ",",
    quote = "\"",
    dec = ".",
    colClasses = rep("character", 22),
    stringsAsFactors = FALSE
  ) %>%
  tibble() %>%
  select(dep, date, dchosp) %>%
  rename(dcHospC19 = dchosp) %>%
  mutate(
    date = as.Date(date),
    dcHospC19 = as.numeric(dcHospC19)
  ) %>%
  filter(year(date) == 2020) %>%
  group_by(dep) %>%
  summarize(dcHospC19 = sum(dcHospC19, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup() %>%
  inner_join(popDeps, by = "dep") %>%
  mutate(mHospC19 = 1000 * dcHospC19 / pop2020) %>%
  select(dep, pop2020, dcHospC19, mHospC19) %>%
  arrange(dep)
}

covidDeps <- recupInseeDcHospCovid2020()

recupInseePyrAges <- function(recharge = FALSE) {
  if (! dir.exists("Sources/PyrAges")) {
    if (! dir.exists("Sources")) {
      dir.create("Sources")
    }
    dir.create("Sources/PyrAges")
    recharge <- TRUE
  }
  codes <- deps$dep[deps$dep != "00"]
  if (recharge) {
    for (d in codes) {
      download.file(
        url = sprintf("https://www.insee.fr/fr/outil-interactif/6798992/data/Dep/%s/donnees_pyramide_act.csv", d),
        destfile = sprintf("Sources/PyrAges/%s.csv", d)
      )
    }
  }
  tibble(dep = codes) %>%
  reframe(
    .by = "dep",
    read.table(
      file = sprintf("Sources/PyrAges/%s.csv", dep),
      header = TRUE,
      sep = ";",
      quote = "\"",
      colClasses = c(rep("character", 3), "double"),
      stringsAsFactors = FALSE
    )
  ) %>%
  rename(annee = ANNEE, sexe = SEXE, age = AGE, nb = POP) %>%
  mutate(annee = as.integer(annee)) %>%
  group_by(dep, annee, age) %>%
  summarize(nb = sum(nb, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup() %>%
  pivot_wider(
    names_from = age,
    values_from = nb,
    values_fill = 0,
    names_prefix = "pa"
  ) %>%
  rowwise() %>%
  mutate(pa = sum(c_across(pa0:pa99), na.rm = TRUE)) %>%
  ungroup() %>%
  select(annee, dep, pa, pa0:pa99) %>%
  bind_rows(
    mutate(., dep = "00") %>%
    group_by(annee, dep) %>%
    summarize(across(pa:pa99, ~ sum(.x, na.rm = TRUE)), .groups = "drop_last")
  )
}

pyrAgesA <- recupInseePyrAges()

recupInseeDeces <- function(recharge = FALSE) {
  if (! dir.exists("Sources/Deces")) {
    if (! dir.exists("Sources")) {
      dir.create("Sources")
    }
    dir.create("Sources/Deces")
    recharge <- TRUE
  }
  if (recharge) {
    download.file(
      url = "https://www.insee.fr/fr/statistiques/fichier/4487988/2023-04-07_detail.zip",
      destfile = "Sources/Deces/2023-04-07_detail.zip"
    )
    unzip("Sources/Deces/2023-04-07_detail.zip", exdir = "Sources/Deces")
    download.file(
      url = "https://www.insee.fr/fr/statistiques/fichier/3606190/etatcivil2017_dec2017_dbase.zip",
      destfile = "Sources/Deces/etatcivil2017_dec2017_dbase.zip"
    )
    unzip("Sources/Deces/etatcivil2017_dec2017_dbase.zip", exdir = "Sources/Deces")
  }
  colsAge <- paste0("da", 0:120)
  bind_rows(
    tibble(fich = c("dec2017.dbf")) %>%
    rowwise() %>%
    reframe(read.dbf(file = paste0("Sources/Deces/", fich))) %>%
    transmute(
      dd = as.Date(ISOdate(adec, mdec, "15")),
      dn = as.Date(ISOdate(anais, "06", "15")),
      age = pmax(0, as.integer(floor(time_length(difftime(dd, dn), "years")))),
      ld = lieudecr,
      dep = depdec
    ),
    tibble(fich = c("DC_2018_det.csv", "DC_2019_det.csv", "DC_2020_det.csv", "DC_2021_det.csv")) %>%
    rowwise() %>%
    reframe(
      read.table(
        file = paste0("Sources/Deces/", fich),
        header = TRUE,
        sep = ";",
        quote = "\"",
        dec = ".",
        colClasses = rep("character", 11),
        stringsAsFactors = FALSE
      )
    ) %>%
    transmute(
      dd = as.Date(ISOdate(ADEC, MDEC, JDEC)),
      dn = as.Date(ISOdate(ANAIS, if_else(MNAIS %in% c("", "00"), "06", MNAIS), if_else(JNAIS %in% c("", "00"), "15", JNAIS))),
      age = pmax(0, as.integer(floor(time_length(difftime(dd, dn), "years")))),
      ld = case_when(
        LIEUDEC2 == "HopCli" ~ "EHC",
        LIEUDEC2 == "HosMar" ~ "HMR",
        LIEUDEC2 == "Logem" ~ "LOG",
        TRUE ~ "AUT"
      ),
      dep = DEPDEC
    )
  ) %>%
  group_by(dd, age, dep, ld) %>%
  summarize(dc = n(), .groups = "drop_last") %>%
  ungroup() %>%
  bind_rows(
    mutate(., ld = "TOT") %>%
    group_by(dd, age, dep, ld) %>%
    summarize(dc = sum(dc, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  ) %>%
  full_join(
    tibble(dd = seq.Date(from = ymd("2017-01-01"), ymd("2021-12-31"), by = "day")) %>%
    cross_join(tibble(age = 0:120)) %>%
    cross_join(tibble(dep = deps$dep[deps$dep != "00"])) %>%
    cross_join(tibble(ld = c("EHC", "HMR", "LOG", "TOT"))) %>%
    mutate(dc = 0),
    by = c("dd", "age", "dep", "ld")
  ) %>%
  mutate(dc = pmax(dc.x, dc.y, na.rm = TRUE)) %>%
  select(-dc.x, -dc.y) %>%
  pivot_wider(
    names_from = age,
    values_from = dc,
    values_fill = 0,
    names_prefix = "da"
  ) %>%
  mutate(da = rowSums(across(da0:da120), na.rm = TRUE)) %>%
  select(dd, dep, ld, da, paste0("da", 0:120)) %>%
  bind_rows(
    group_by(., dd, ld) %>%
    summarize(
      dep = "00",
      across(da:da120, ~ sum(.x, na.rm = TRUE)),
      .groups = "drop_last"
    ) %>%
    ungroup()
  )
}

pyrDeces <- recupInseeDeces()

pyrDecesA <-
  pyrDeces %>%
  mutate(annee = year(dd)) %>%
  group_by(annee, dep, ld) %>%
  summarize(
    across(da:da120, ~ sum(., na.rm = TRUE)), .groups = "drop_last") %>%
  ungroup()

calculePyrMort <- function(p = pyrAgesA, d = pyrDecesA) {
  res <-
    p %>%
    inner_join(d %>% filter(ld == "TOT"), by = c("annee", "dep"))
  for (acol in c("a", paste0("a", 0:99))) {
    pcol <- paste0("p", acol)
    dcol <- paste0("d", acol)
    res <- res %>% mutate(!!acol := .data[[dcol]] / .data[[pcol]])
  }
  for (acol in c("a", paste0("a", 100:119))) {
    res <- res %>% mutate(!!acol := a99)
  }
  res %>%
    mutate(a120 = 1) %>%
    select(annee, dep, a, a0:a120)
}

pyrMortA <- calculePyrMort()

recupAtihTauxPatients <- function(recharge = FALSE) {
# source: https://www.scansante.fr/applications/taux-de-recours-tous-champs
# période: 2017 à 2021
# type de taux: taux standardisés
# niveau géographique: département
# fichier "applications_taux-de-recours-tous-champs.xls" dans "Sources/Atih"
# retravaillé à la main pour obtenir "AtihTauxPatients1000Std.csv"
# car R n'arrive pas à lire correctement les fichiers Mi©®o$oft €x©€£
# et je n'ai pas envie de leur consacrer plus de temps que ça.
  as_tibble(
    read.table(
      file = "Sources/Atih/AtihTauxPatients1000Std.csv",
      header = TRUE,
      sep = ";",
      quote = "\"",
      dec = ",",
      colClasses = c("character", "integer", "double"),
      stringsAsFactors = FALSE
    )
  ) %>%
  rename(tp = tauxPatients) %>%
  select(annee, dep, tp) %>%
  arrange(annee, dep)
}

atih <- recupAtihTauxPatients()

calculeEspVie <- function(m) {
  n <- length(m)
  p <- numeric(n)
  p[1] <- 1
  for (i in 2:n) {
    p[i] <- p[i - 1] * (1 - m[i - 1])
  }
  for (i in 1:n) {
    p[i] <- p[i] * m[i] * (i - 1)
  }
  sum(p)
}

espVie <-
  pyrMortA %>%
  rowwise() %>%
  mutate(ev = calculeEspVie(unlist(c_across(a0:a120)))) %>%
  ungroup() %>%
  select(annee, dep, ev) %>%
  arrange(annee, dep)

prefixMean <- function(t) {
  l <- length(t)
  res <- numeric(l)
  res[1] <- NA
  if (l > 1) {
    for (i in 2:l) {
      res[i] <- mean(t[1:(i - 1)], na.rm = TRUE)
    }
  }
  res
}

diffPc <- function(col) {
  colInf <- prefixMean(col)
  100 * (col - colInf) / colInf
}

diffD <- function(col) {
  col - prefixMean(col)
}

abSoins <-
  pyrDeces %>%
  group_by(dd, dep) %>%
  summarize(
    annee = first(year(dd)),
    daEHC = max(ifelse(ld == "EHC", da, NA), na.rm = TRUE),
    daTOT = max(ifelse(ld == "TOT", da, NA), na.rm = TRUE),
    ab = daTOT - 2 * daEHC,
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  inner_join(
    pyrAgesA %>%
    select(annee, dep, pa),
    by = c("annee", "dep")
  ) %>%
  group_by(annee, dep) %>%
  summarize(
    pa = first(pa),
    ab = sum(ab, na.rm = TRUE) / pa,
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  select(annee, dep, ab, pa) %>%
  arrange(annee, dep)

estDepMetroFr <- function (dep) {
  dep %in% c("2A", "2B") | suppressWarnings(as.numeric(dep) %in% c(1:95))
}

dataCmp <-
  atih %>%
  inner_join(espVie, by = c("annee", "dep")) %>%
  inner_join(abSoins, by = c("annee", "dep")) %>%
  filter(2017 <= annee) %>%
  group_by(dep) %>%
  mutate(
    diffTpPc = diffPc(tp),
    diffEvPc = diffPc(ev),
    diffAbD = 100000 * diffD(ab),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  filter(2018 <= annee & estDepMetroFr(dep)) %>%
  inner_join(deps, by = "dep") %>%
  left_join(covidDeps %>% mutate(annee = 2020), by = c("dep", "annee")) %>%
  select(annee, dep, nom, tp, diffTpPc, ev, diffEvPc, ab, diffAbD, pop2020, mHospC19) %>%
  arrange(annee, dep)

# Corrélations de Pearson et de Spearman pondérées,
# empruntées à la librairie wCorr:
# https://github.com/American-Institutes-for-Research/wCorr

rangPondere <- function(x, p = rep(1, length(x))) {
  ord <- order(x)
  rord <- (1:length(x))[order(ord)]
  xp <- x[ord]
  pp <- p[ord]
  rg <- rep(NA, length(x))
  t1 <- 0
  i <- 1
  t2 <- 0
  n <- 0
  while(i < length(x)) {
    t2 <- t2 + pp[i]
    n <- n + 1
    if (xp[i + 1] != xp[i]) {
      rgi <- t1 + (n + 1) / (2 * n) * t2
      for(ii in 1:n) {
        rg[i - ii + 1] <- rgi
      }
      t1 <- t1 + t2
      t2 <- 0
      n <- 0
    }
    i <- i + 1
  }
  n <- n + 1
  t2 <- t2 + pp[i]
  rgi <- t1 + (n + 1) / (2 * n) * t2
  for(ii in 1:n) {
    rg[i - ii + 1] <- rgi
  }
  rg[rord]
}

correlationPonderee <- function(x, y, methode = c("pearson", "spearman"), p = rep(1, length(x))) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  p <- as.numeric(p)
  if (! is.vector(x)) stop(paste0("L'argument ",sQuote("x"), " doit être un vecteur."))
  if (! is.vector(y)) stop(paste0("L'argument ",sQuote("y"), " doit être un vecteur."))
  if (! is.vector(p)) stop(paste0("L'argument ",sQuote("p"), " doit être un vecteur."))
  if (length(x) != length(y)) stop(paste0("Les arguments ", sQuote("x"), " et ", sQuote("y"), " doivent avoir la même longueur."))
  if (length(x) != length(p)) stop(paste0("Les arguments ", sQuote("x"), ", ", sQuote("y"), ", et ", sQuote("p") ," doivent avoir la même longueur."))
  if (! methode %in% c("pearson", "spearman"))  stop(paste0("Impossible de trouver la méthode ", sQuote(methode), "."))

  if (methode == "spearman") {
    x <- rangPondere(x, p)
    y <- rangPondere(y, p)
  }
  xb <- sum(p * x) / sum(p)
  yb <- sum(p * y) / sum(p)
  num <- sum(p * (x - xb) * (y - yb))
  den <- sqrt(sum(p * (x - xb) ^ 2) * sum(p * (y - yb) ^ 2))
  num / den
}

correlations <- function(fich = "") {
  d <- dataCmp %>% filter(annee == 2020)
  cP0 <- correlationPonderee(x = d$diffEvPc, y = d$diffTpPc, methode = "pearson")
  cS0 <- correlationPonderee(x = d$diffEvPc, y = d$diffTpPc, methode = "spearman")
  cP1 <- correlationPonderee(x = d$mHospC19, y = d$diffTpPc, methode = "pearson")
  cS1 <- correlationPonderee(x = d$mHospC19, y = d$diffTpPc, methode = "spearman")
  cpP0 <- correlationPonderee(x = d$diffEvPc, y = d$diffTpPc, methode = "pearson", p = d$pop2020)
  cpS0 <- correlationPonderee(x = d$diffEvPc, y = d$diffTpPc, methode = "spearman", p = d$pop2020)
  cpP1 <- correlationPonderee(x = d$mHospC19, y = d$diffTpPc, methode = "pearson", p = d$pop2020)
  cpS1 <- correlationPonderee(x = d$mHospC19, y = d$diffTpPc, methode = "spearman", p = d$pop2020)
  file <- fich
  write("Corrélations sur l'année 2020:", file)
  write(paste0("- Pearson entre l'espérance de vie et le taux de patients: ", cP0), file, append = TRUE)
  write(paste0("- Spearman entre l'espérance de vie et le taux de patients: ", cS0), file, append = TRUE)
  write(paste0("- Pearson entre la mortalité «COVID» hospitalière et le taux de patients: ", cP1), file, append = TRUE)
  write(paste0("- Spearman entre la mortalité «COVID» hospitalière et le taux de patients: ", cS1), file, append = TRUE)
  write("", file, append = TRUE)
  write("Corrélations pondérées par la population sur l'année 2020:", file, append = TRUE)
  write(paste0("- Pearson entre l'espérance de vie et le taux de patients: ", cpP0), file, append = TRUE)
  write(paste0("- Spearman entre l'espérance de vie et le taux de patients: ", cpS0), file, append = TRUE)
  write(paste0("- Pearson entre la mortalité «COVID» hospitalière et le taux de patients: ", cpP1), file, append = TRUE)
  write(paste0("- Spearman entre la mortalité «COVID» hospitalière et le taux de patients: ", cpS1), file, append = TRUE)
}

graphEspVieAtih <- function(data = dataCmp, annee, xl, yl, fich, w, h) {
  if (! missing(fich)) {
    png(filename = fich, width = w, height = h)
  } else {
    x11()
  }

  ann <- annee
  data <- data %>% filter(annee == ann)
  plot(
    data$diffEvPc,
    data$diffTpPc,
    pch = 19,
    xlab = "variation de l'espérance de vie en %",
    ylab = "variation du taux de patients en %",
    xlim = xl,
    ylim = yl
  )
  titre <-  paste0("Comparaison des variations de l'espérance de vie et du taux de patients en ", ann)
  sousTitre <- paste0("variation par rapport à la moyenne de 2017-", ann - 1)
  mtext(side = 3, line = 2, adj = 0.5, cex = 1, font = 2, text = titre)
  mtext(side = 3, line = 1, adj = 0.5, cex = 1, font = 1, text = sousTitre)
  abline(v = 0)
  abline(h = 0)

  if (! missing(fich)) {
    dev.off()
  }
}

graphAbSoins <- function(data = dataCmp, annee, xl = c(), yl = c(), fich, w, h) {
  if (! missing(fich)) {
    png(filename = fich, width = w, height = h)
  } else {
    x11()
  }

  ann <- annee
  data <- data %>% filter(annee == ann)
  plot(
    data$diffAbD,
    data$diffTpPc,
    pch = 19,
    xlab = "variation des abandons de soins pour 100000",
    ylab = "variation du taux de patients en %",
    xlim = xl,
    ylim = yl
  )
  titre <-  paste0("Comparaison des abandons de soins et de la variation du taux de patients en ", ann)
  sousTitre <- paste0("variation par rapport à la moyenne de 2017-", ann - 1)
  mtext(side = 3, line = 2, adj = 0.5, cex = 1, font = 2, text = titre)
  mtext(side = 3, line = 1, adj = 0.5, cex = 1, font = 1, text = sousTitre)
  abline(v = 0)
  abline(h = 0)

  if (! missing(fich)) {
    dev.off()
  }
}

graphCovidAtih <- function(data = dataCmp, xl = c(), yl = c(), fich, w, h) {
  if (! missing(fich)) {
    png(filename = fich, width = w, height = h)
  } else {
    x11()
  }

  data <- data %>% filter(annee == 2020)
  plot(
    data$mHospC19,
#    data$tp,
    data$diffTpPc,
    pch = 19,
    xlab = "mortalité hospitalière «COVID-19» pour 1000",
    ylab = "variation du taux de patients en %",
    xlim = xl,
    ylim = yl
  )
  titre <- "Comparaison de la mortalité hospitalière «COVID-19» et de la variation du taux de patients en 2020"
  sousTitre <- "variation par rapport à la moyenne de 2017-2019"
  mtext(side = 3, line = 2, adj = 0.5, cex = 1, font = 2, text = titre)
  mtext(side = 3, line = 1, adj = 0.5, cex = 1, font = 1, text = sousTitre)
  abline(v = 0)
  abline(h = 0)

  if (! missing(fich)) {
    dev.off()
  }
}

# tests
if (FALSE) {

  graphEspVieAtih(
    annee = 2019,
    xl = c(-3, 3),
    yl = c(-20, 6)
  )

  graphEspVieAtih(
    annee = 2020,
    xl = c(-3, 3),
    yl = c(-20, 6)
  )

  graphEspVieAtih(
    annee = 2021,
    xl = c(-3, 3),
    yl = c(-20, 6)
  )

  graphAbSoins(
    annee = 2019,
    xl = c(-400, 400),
    yl = c(-20, 20)
  )

  graphAbSoins(
    annee = 2020,
    xl = c(-400, 400),
    yl = c(-20, 20)
  )

  graphAbSoins(
    annee = 2021,
    xl = c(-400, 400),
    yl = c(-20, 20)
  )

  graphCovidAtih(
    xl = c(0, 400),
    yl = c(-20, 0)
  )

  graphCovidAtih(
    xl = c(0, 450),
    yl = c(100, 250)
  )

}

genResultats <- function() {
  if (! dir.exists("Resultats")) {
    dir.create("Resultats")
  }

  graphEspVieAtih(
    annee = 2019,
    xl = c(-3, 3),
    yl = c(-20, 6),
    fich = "Resultats/EspVieAtih2019.png",
    w = 800,
    h = 800
  )

  graphEspVieAtih(
    annee = 2020,
    xl = c(-3, 3),
    yl = c(-20, 6),
    fich = "Resultats/EspVieAtih2020.png",
    w = 800,
    h = 800
  )

  graphEspVieAtih(
    annee = 2021,
    xl = c(-3, 3),
    yl = c(-20, 6),
    fich = "Resultats/EspVieAtih2021.png",
    w = 800,
    h = 800
  )

  graphAbSoins(
    annee = 2019,
    xl = c(-400, 400),
    yl = c(-20, 20),
    fich = "Resultats/AbSoinsAtih2019.png",
    w = 800,
    h = 800
  )

  graphAbSoins(
    annee = 2020,
    xl = c(-400, 400),
    yl = c(-20, 20),
    fich = "Resultats/AbSoinsAtih2020.png",
    w = 800,
    h = 800
  )

  graphAbSoins(
    annee = 2021,
    xl = c(-400, 400),
    yl = c(-20, 20),
    fich = "Resultats/AbSoinsAtih2021.png",
    w = 800,
    h = 800
  )

  graphCovidAtih(
    xl = c(0, 400),
    yl = c(-20, 0),
    fich = "Resultats/CovidAtih2020.png",
    w = 800,
    h = 800
  )

  write.table(
    dataCmp,
    file = "Resultats/EspVieAtihCmp.csv",
    sep = ";",
    dec = ",",
    row.names = FALSE,
    quote = FALSE,
    na = ""
  )

  correlations(fich = "Resultats/Correlations.txt")
}

genResultats()

