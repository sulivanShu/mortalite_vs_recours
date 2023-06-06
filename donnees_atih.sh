#/bin/sh
# Préparation et nettoyage du fichier AtihTauxPatients1000Std.csv pour les calculs
cat Sources/Atih/AtihTauxPatients1000Std.csv | grep -v '\("00"\)\|\("977"\)\|\("978"\)\|\(;2021;\)' | tail +2 | tr , . | sort -t \; -k1,1 -k2,2r  >AtihTauxPatients1000Std.csv
cat Sources/Atih/AtihTauxPatients1000Std.csv | grep -v '\("00"\)\|\("977"\)\|\("978"\)\|\(;2021;\)' | tail +2 | tr , . | tr \; \t | sort -k1,1 -k2,2r | cut -f 3 >AtihTauxPatients1000Std.tsv
