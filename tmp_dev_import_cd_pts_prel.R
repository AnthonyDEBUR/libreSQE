# Installer et charger la librairie xml2
library(xml2)
library(tidyverse)
library(progress)

staq.xml.gz <- tempfile()


  # telechargement du referentiel fraction sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/stq.xml?outputSchema=SANDREv3.1&compress=true&derniereDateDeMAJ=2023-09-21"
    ),
    staq.xml.gz,
    mode = "wb",
    cacheOK = T
  )


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



##### Extraction des données stations #####
# Extraire les informations nécessaires dans une liste en ignorant l'espace de noms
# data_list <- lapply(xml_find_all(doc, ".//*[local-name()='StationMesureEauxSurface']"), function(x) {
#   pb$tick()
#   data_frame(
#     CdStationMesureEauxSurface = xml_text(xml_find_first(x, ".//*[local-name()='CdStationMesureEauxSurface']")),
#     DateMAJInfosStationMesureEauxSurface = xml_text(xml_find_all(x, ".//*[local-name()='DateMAJInfosStationMesureEauxSurface']"))
#   )
# })
#
# # Concaténer les résultats en un seul data frame
# data_staq <- do.call(rbind, data_list)


##### Extraction des données lieu de qualité #####

A_traiter<-xml_find_all(doc, ".//*[local-name()='StationMesureEauxSurface']")

# Créez un objet progress_bar
pb <- progress_bar$new(total = length(A_traiter))


# Extraire les informations nécessaires dans une liste en ignorant l'espace de noms
data_list <- lapply(A_traiter, function(x) {
  pb$tick()

  tibble(
    CdStationMesureEauxSurface = xml_text(xml_find_first(x, ".//*[local-name()='CdStationMesureEauxSurface']")),
    LbPointEauxSurf = xml_text(xml_find_all(x, ".//*[local-name()='LbPointEauxSurf']")),
    CdPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CdPointEauxSurf']") %>% xml_text(),
    CdSupport = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='Support']/*[local-name()='CdSupport']") %>%
      lapply(xml_text) %>%
      unlist(),
    CoordXPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CoordXPointEauxSurf']")%>% xml_text(),
    CoordYPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='CoordYPointEauxSurf']")%>% xml_text(),
    StPointPrel = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='StPointPrel']")%>% xml_text(),
    DateMAJInfosPointPrel = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='DateMAJInfosPointPrel']")%>% xml_text(),
    ComPointEauxSurf = xml_find_all(x, ".//*[local-name()='PointPrelEauxSurf']/*[local-name()='ComPointEauxSurf']")%>% xml_text()
    )
})

# Concaténer les résultats en un seul data frame
data <- bind_rows(data_list)


