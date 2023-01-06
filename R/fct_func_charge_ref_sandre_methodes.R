#' func_charge_ref_sandre_methodes
#'
#'
#'
#' @description Charge le référentiel SANDRE des méthodes d'analyse
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#'
#'@export
func_charge_ref_sandre_methodes <- function(date_maj = "1950-01-01")
{
  methodes.csv.gz <- tempfile()

  # telechargement du referentiel unites sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/met.csv?outputSchema=SANDREv4&compress=true&derniereDateDeMAJ=",
      date_maj
    ),
    methodes.csv.gz,
    mode = "wb",
    cacheOK = T
  )
  methodes <-
    readr::read_delim(
      methodes.csv.gz,
      delim = ";",
      skip = 1,
      show_col_types = FALSE
    )
  file.remove(methodes.csv.gz)

  if (nrow(methodes) > 0) {
    colnames(methodes) <-
      stringi::stri_trans_general(colnames(methodes), "Latin-ASCII")

    methodes <- methodes [,c(1,2,3)]
    names(methodes)<- c("met_code", "met_nom", "met_statut")
      # methodes %>% dplyr::rename(
      #   "met_code" = "Code de la méthode...1",
      #   "met_nom" = "Nom de la méthode",
      #   "met_statut" = "Statut de la méthode"
      # ) %>% dplyr::select(met_code, met_nom, met_statut)

    methodes$met_code<- methodes$met_code%>%as.character()

  } else{
    methodes <- NULL
  }

  return(methodes)


}
