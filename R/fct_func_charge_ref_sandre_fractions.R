#' func_charge_ref_sandre_fractions
#'
#' @description Charge le référentiel SANDRE des fractions analysées
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#' @export

func_charge_ref_sandre_fractions <- function(date_maj = "1950-01-01")
{
  CdFractionAnalysee<-LbFractionAnalysee<-NULL



  fraction.csv.gz <- tempfile()


  # telechargement du referentiel fraction sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/fan.csv?outputSchema=SANDREv4&compress=true&derniereDateDeMAJ=",
      date_maj
    ),
    fraction.csv.gz,
    mode = "wb",
    cacheOK = T
  )

  fractions_sandre <-
    readr::read_delim(fraction.csv.gz,
                      delim = ";",
                      show_col_types = FALSE)

  file.remove(fraction.csv.gz)

  if(nrow(fractions_sandre)>0)
  {colnames(fractions_sandre) <-
    stringi::stri_trans_general(colnames(fractions_sandre), "Latin-ASCII")

  fractions_sandre <-
    fractions_sandre %>%
    dplyr::select(CdFractionAnalysee, LbFractionAnalysee) %>%
    dplyr::rename("fra_codefraction" = "CdFractionAnalysee",
                  "fra_nomfraction" = "LbFractionAnalysee")}else{fractions_sandre<-NULL}


  return(fractions_sandre)

}
