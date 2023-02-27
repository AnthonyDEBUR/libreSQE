#' func_charge_ref_sandre_staq
#'
#' @description Charge le référentiel SANDRE des stations de mesures
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#' @export
func_charge_ref_sandre_staq <- function(date_maj = "1950-01-01")
{
  CdStationMesureEauxSurface<-LbStationMesureEauxSurface<-CoordXStationMesureEauxSurface<-NULL
  CoordYStationMesureEauxSurface<-NULL


  staq.csv.gz <- tempfile()

  # si date de dernière maj > 3 mois alors on recharge tout le référentiel (pour palier les défauts du SANDRE)
  if(Sys.Date()-as.Date(date_maj)>90){
  # telechargement du referentiel fraction sandre
  downloader::download(
    paste0(
      "https://api.sandre.eaufrance.fr/referentiels/v1/stq.csv?outputSchema=SANDREv4&compress=true"
    ),
    staq.csv.gz,
    mode = "wb",
    cacheOK = T
  )
  } else {
    # telechargement du referentiel fraction sandre
    downloader::download(
      paste0(
        "https://api.sandre.eaufrance.fr/referentiels/v1/stq.csv?outputSchema=SANDREv4&compress=true&derniereDateDeMAJ=",
        date_maj
      ),
      staq.csv.gz,
      mode = "wb",
      cacheOK = T
    )

}



  staq <-
    readr::read_delim(staq.csv.gz,
                      delim = ";",
                      show_col_types = FALSE)

  file.remove(staq.csv.gz)

  if(nrow(staq)>0)
  {
      colnames(staq) <-
    stringi::stri_trans_general(colnames(staq), "Latin-ASCII")

  # suppression de 1ère ligne qui décrit les colonnes
  staq<-staq[2:nrow(staq),]

  # on ne conserve que les stations du bassin AELB et les stations plan d'eau
  staq<-staq[substr(staq$CdStationMesureEauxSurface,1,2)=="04" | substr(staq$CdStationMesureEauxSurface,1,1)!="0" ,]

  staq <- staq %>%
    dplyr::select(CdStationMesureEauxSurface,
                  LbStationMesureEauxSurface,
                  CoordXStationMesureEauxSurface,
                  CoordYStationMesureEauxSurface) %>%
    dplyr::rename("stm_cdstationmesureauxsurface" = "CdStationMesureEauxSurface",
                  "stm_lbstationmesureeauxsurface" = "LbStationMesureEauxSurface",
                  "stm_x" = "CoordXStationMesureEauxSurface",
                  "stm_y" = "CoordYStationMesureEauxSurface"
    )

  staq$stm_x<-as.numeric(staq$stm_x)
  staq$stm_y<-as.numeric(staq$stm_y)

  }else{staq<-NULL}


  return(staq)

}

