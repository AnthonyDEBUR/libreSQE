#' func_charge_ref_sandre_intervenants
#'
#'
#' @description Charge le référentiel SANDRE des inetervenants
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#' @export
func_charge_ref_sandre_intervenants <-  function(date_maj = "1950-01-01")
{
  CdIntervenant<-NomIntervenant<-StIntervenant<-DateCreationIntervenant<-NULL
DateMajIntervenant<-MnIntervenant<-ImmoIntervenant<-BpIntervenant<-LieuIntervenant<-NULL
VilleIntervenant<-DepIntervenant<-CPIntervenant<-isa_datemaj<-RueIntervenant<-NULL


  interv.csv.gz <- tempfile()

  # si date de dernière maj > 3 mois alors on recherge tout el référentiel (pour palier les défauts du SANDRE)
  if(Sys.Date()-as.Date(date_maj)>90){
    # telechargement du referentiel fraction sandre
    download.file(
      paste0(
        "https://api.sandre.eaufrance.fr/referentiels/v1/int.csv?outputSchema=SANDREv2&compress=true"
      ),
      interv.csv.gz,
      mode = "wb",
      cacheOK = T
    )} else {
      download.file(
        paste0(
          "https://api.sandre.eaufrance.fr/referentiels/v1/int.csv?outputSchema=SANDREv2&compress=true&derniereDateDeMAJ=",
          date_maj),
        interv.csv.gz,
        mode = "wb",
        cacheOK = T
      )
    }

  intervenants_sandre <-
    readr::read_delim(interv.csv.gz,
                      delim = ";",
                      show_col_types = FALSE)

  file.remove(interv.csv.gz)

  if(nrow(intervenants_sandre)>1){

    # suppression de la 1ère ligne qui décrit les noms de colonnes
    intervenants_sandre<-intervenants_sandre[-1,]

    colnames(intervenants_sandre) <-
      stringi::stri_trans_general(colnames(intervenants_sandre), "Latin-ASCII")


    intervenants_sandre <-
      intervenants_sandre %>%
      dplyr::select(
        CdIntervenant,
        NomIntervenant,
        StIntervenant,
        DateCreationIntervenant,
        DateMajIntervenant,
        MnIntervenant,
        ImmoIntervenant,
        BpIntervenant,
        RueIntervenant,
        LieuIntervenant,
        VilleIntervenant,
        DepIntervenant,
        CPIntervenant
      )


    names(intervenants_sandre) <- c(
      "isa_codesandre",
      "isa_nom",
      "isa_statut",
      "isa_datecreation",
      "isa_datemaj",
      "isa_mnemo",
      "isa_ensembleimmobilier",
      "isa_bp",
      "isa_rue",
      "isa_lieudit",
      "isa_ville",
      "isa_departementpays",
      "isa_codepostal"
    )
    intervenants_sandre$isa_datecreation<-as.Date(intervenants_sandre$isa_datecreation)
    intervenants_sandre$isa_datemaj<-as.Date(intervenants_sandre$isa_datemaj)
    intervenants_sandre$isa_codepostal<-as.numeric(intervenants_sandre$isa_codepostal)

    intervenants_sandre<-intervenants_sandre%>%subset(as.Date(isa_datemaj)>=as.Date(date_maj))

  } else{
    intervenants_sandre <- NULL
  }

  return(intervenants_sandre)
}
