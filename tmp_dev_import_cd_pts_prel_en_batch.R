# Installer et charger la librairie xml2
library(xml2)
library(tidyverse)
library(progress)

staq.xml.gz <- tempfile()


# telechargement du referentiel fraction sandre
downloader::download(
  paste0(
    "https://api.sandre.eaufrance.fr/referentiels/v1/stq.xml?outputSchema=SANDREv3.1&compress=true&derniereDateDeMAJ=1965-09-19"
  ),
  staq.xml.gz,
  mode = "wb",
  cacheOK = T
)
# 2023-09-20 OK
# 2023-09-19 KO

# Staq en cause potentielle d'erreur
# 02061290
# 2023-09-19
# 2

# 02048975
# 2023-09-19

# 3
# 02061275
# 2023-09-19

# 4
# O2705023
# 2023-09-19

# 5
# 03169992
# 2023-09-19

# Charger le fichier XML
xml_file <- read_xml(staq.xml.gz)

# structure du xml
# xml_structure(xml_file)

# suppression du fichier tmp
unlink(staq.xml.gz)

# Sélectionner les enfants de la racine du document
root_children <- xml_children(xml_root(xml_file))

xml_content <- '<Root>'  # On doit ajouter une racine au contenu XML pour pouvoir le lire
xml_content <- paste(xml_content, paste(root_children, collapse=""), sep='')
xml_content <- paste(xml_content, '</Root>', sep = '')  # On ferme la balise racine
doc <- xml2::read_xml(xml_content)

# A_traiter<-readRDS("A_traiter.rds")
A_traiter<-xml_find_all(doc, ".//*[local-name()='StationMesureEauxSurface']")
saveRDS(A_traiter, "A_traiter.rds")

##### Extraction des données stations #####
# Extraire les informations nécessaires dans une liste en ignorant l'espace de noms
# data_list <- lapply(A_traiter, function(x) {
#   data_frame(
#     CdStationMesureEauxSurface = xml_text(xml_find_first(x, ".//*[local-name()='CdStationMesureEauxSurface']")),
#     DateMAJInfosStationMesureEauxSurface = xml_text(xml_find_all(x, ".//*[local-name()='DateMAJInfosStationMesureEauxSurface']"))
#   )
# })
#
# # Concaténer les résultats en un seul data frame
# data_staq <- do.call(rbind, data_list)
# saveRDS(data_staq, "data_staq.rds")

##### Extraction des données lieu de qualité #####


# Diviser A_traiter en listes de 1000 éléments chacune
batches <- split(A_traiter, ceiling(seq_along(A_traiter) / 300))

# Créez un objet progress_bar
pb <- progress_bar$new(total = length(batches))

# Initialiser une liste pour stocker les résultats
result_list <- list()
result_list <-readRDS("result_list.rds")

# Boucler à travers les lots
# for (i in length(result_list):length(batches)) {
#   pb$tick()
#   batch<-batches[[i]]
for (batch in batches) {
    pb$tick()

  # Extraire les informations nécessaires dans une liste en ignorant l'espace de noms
  data_list <- lapply(batch, function(x) {
    tibble(
      CdStationMesureEauxSurface = xml_text(xml_find_first(x, ".//*[local-name()='CdStationMesureEauxSurface']")),
      LbPointEauxSurf = xml_text(xml_find_all(x, ".//*[local-name()='LbPointEauxSurf']")),
      CdPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CdPointEauxSurf']") %>% xml_text(),
      CdSupport = purrr::map_chr(xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']"), function(point) {
        support <- xml_find_all(point, ".//*[local-name()='Support']/*[local-name()='CdSupport']")
        if (length(support) > 0) {
          return(xml_text(support))
        } else {
          return("0")
        }
      }),
      CoordXPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CoordXPointEauxSurf']")%>% xml_text(),
      CoordYPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CoordYPointEauxSurf']")%>% xml_text(),
      StPointPrel = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='StPointPrel']")%>% xml_text(),
      DateMAJInfosPointPrel = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='DateMAJInfosPointPrel']")%>% xml_text(),
      ComPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='ComPointEauxSurf']")%>% xml_text()
    )
  })


  # Concaténer les résultats en un seul data frame et stocker dans la liste
  result_list[[length(result_list) + 1]] <- bind_rows(data_list)
  saveRDS(result_list, "result_list.rds")
}

# Concaténer les résultats de chaque lot en un seul data frame final
data <- bind_rows(result_list)

saveRDS(data, "lieux_qual_1970_au_20240126.rds")

