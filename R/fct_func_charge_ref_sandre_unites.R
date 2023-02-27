#' func_charge_ref_sandre_unites
#'
#'
#' @description Charge le référentiel SANDRE des unités analysées
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#' @export
func_charge_ref_sandre_unites <- function(date_maj = "1950-01-01")
{
  uni_codesandreunite<-uni_symbole<-uni_lblsandreunite<-NULL

   unites.csv.gz <- tempfile()

  # telechargement du referentiel unites sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/urf.csv?compress=true&derniereDateDeMAJ=",
      date_maj
    ),
    unites.csv.gz,
    mode = "wb",
    cacheOK = T
  )
  unites_sandre <-
    readr::read_delim(
      unites.csv.gz,
      delim = ";",
      skip = 1,
      show_col_types = FALSE
    )
  file.remove(unites.csv.gz)

  if (nrow(unites_sandre) > 0) {
    colnames(unites_sandre) <-
      stringi::stri_trans_general(colnames(unites_sandre), "Latin-ASCII")

    unites_sandre <-
      unites_sandre %>% dplyr::rename(
        "uni_codesandreunite" = "Code de l'unite de reference",
        "uni_symbole" = "Symbole de l'unite de reference",
        "uni_lblsandreunite" = "Libelle de l'unite de reference"
      ) %>% dplyr::select(uni_codesandreunite, uni_symbole, uni_lblsandreunite)

    unites_sandre$uni_codesandreunite<-as.character(unites_sandre$uni_codesandreunite)

  } else{
    unites_sandre <- NULL
  }

  return(unites_sandre)


}
