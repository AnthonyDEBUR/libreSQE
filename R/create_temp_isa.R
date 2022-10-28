# chargement referentiel SANDRE intervenants


library(tidyverse)
library(downloader) # pour télécharger gros fichiers en https dont paramètres sandre


# Intervenants

download(
  "https://api.sandre.eaufrance.fr/referentiels/v1/int.csv?outputSchema=SANDREv2&compress=true",
  chemin_out,
  mode = "wb",
  cacheOK = T,
  extra = options(timeout = 600)
)

Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

# choisir le fichier téléchargé pour le dézipper et le lire
# fichier <- file.choose()
intervenants <- read_delim(chemin_out, delim = ";", skip = 0)
intervenants <-
  intervenants[2:nrow(intervenants), ] # suppression de la 2ème ligne du fichier avec les descriptifs des noms de champs
file.remove(chemin_out)

colnames(intervenants) <-
  stringi::stri_trans_general(colnames(intervenants), "Latin-ASCII")


temp_isa<-intervenants
# renommage des colonnes de la bdd
names(temp_isa)<-c("isa_codesandre",
                       "isa_siret",
                       "isa_nom",
                       "isa_statut",
                       "isa_datecreation",
                       "isa_datemaj",
                       "drop_1",
                       "isa_mnemo",
                       "isa_bp",
                       "isa_ensembleimmobilier",
                       "isa_rue",
                       "isa_lieudit",
                       "isa_ville",
                       "isa_departementpays",
                       "drop_2",
                       "isa_codepostal",
                       "drop_3",
                       "drop_4")

temp_isa<-temp_isa%>%select(starts_with("isa_"))
