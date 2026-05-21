library(LibreSQE)
library(tidyverse)

# fichier_prog<-file.choose()
fichier_prog<- "C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB2026_version dev libreSQE.xlsx"

connexion <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

mar_id<-24
annee<-2026
frequence_bdc = "mensuelle"
strict_referentiel = TRUE
prefixe = "SQE2026"
