#' func_charge_ref_sandre_dispocollecte
#'
#'
#' @description Charge le référentiel SANDRE des fractions analysées
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#'
func_charge_ref_sandre_dispocollecte <- function(date_maj = "1950-01-01")
{
  dispo_collecte.csv.gz <- tempfile()


  # telechargement du referentiel fraction sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/dc.csv?outputSchema=SANDREv4&compress=true&derniereDateDeMAJ=",
      date_maj
    ),
    dispo_collecte.csv.gz,
    mode = "wb",
    cacheOK = T
  )

  dispo_collecte <-
    readr::read_delim(dispo_collecte.csv.gz,
                      delim = ";",
                      show_col_types = FALSE)

  file.remove(dispo_collecte.csv.gz)

  if(nrow(dispo_collecte)>0)
  {colnames(dispo_collecte) <-
    stringi::stri_trans_general(colnames(dispo_collecte), "Latin-ASCII")

  # suppression de 1ère ligne qui décrit les colonnes
  dispo_collecte<-dispo_collecte[2:nrow(dispo_collecte),]

  dispo_collecte <-
    dispo_collecte %>%
    dplyr::select(CodeSandreRdd, NomRdd, StRdd) %>%
    dplyr::rename("rdd_cdrdd" = "CodeSandreRdd",
                  "rdd_nomrdd" = "NomRdd",
                  "rdd_statut" = "StRdd")}else{dispo_collecte<-NULL}


  return(dispo_collecte)

}
