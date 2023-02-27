#' func_lit_le_fichier
#'
#' @description Fonction pour charger les onglets marchés à partir des fichiers EXCEL au format imposé
#'
#' @return Un dataframe contenant les onglets de programmation avec les formats de données
#' conformes qu dictionnaire de données
#'
#' @param onglet : fichier xlsx définissant le marché.
#' @param fichier_prog : fichier xlsx définissant le marché.
#' Le fichier contient les onglets programmes_types, BPU, cout_run_analytiques
#' Ces onglets ont a minima les colonnes prévues au dictionnaire de données
#'
#' fonction lecture du fichier
#' @export
func_lit_le_fichier<-function(fichier_prog, onglet){

# chargement du dico de données
dictionnaire_import_data <-
  system.file("dictionnaire_import_data.csv", package = "LibreSQE")
dictionnaire_import_data <-
  utils::read.csv2(dictionnaire_import_data, encoding = "UTF-8")



# fonction test si les noms de colonnes sont bien présents
test_noms_colonnes<-function(fichier_prog, onglet){
  col_a_importer<-dictionnaire_import_data[dictionnaire_import_data$onglet == onglet,]$colonne

  if (!(all(col_a_importer %in%
            names(get(onglet))))) {
    stop(
      {paste0(
        "Onglet ",onglet,": la (les) colonne(s) suivantes sont manquantes :",
        paste(col_a_importer[!col_a_importer %in%
                               names(get(onglet))], collapse = ";")
      )
      }
    )
  }
}

# fonction convertit les formats de colonnes qui ne sont pas du text
convert_format_colonnes<-function(onglet){
  colonnes_numeric<-dictionnaire_import_data[dictionnaire_import_data$onglet == onglet &
                                               dictionnaire_import_data$format=="numeric",]$colonne

  if(length(colonnes_numeric)>0){
    is_numeric<-names(get(onglet))%in%colonnes_numeric
    suppressWarnings(tmp<- sapply(colonnes_numeric, function(x) as.numeric(get(onglet)[[x]])))
    names(tmp)<-names(get(onglet))[names(get(onglet))%in%colonnes_numeric]
    assign(onglet, cbind(get(onglet)[,!is_numeric],tmp),
           envir = .GlobalEnv)
  }

  colonnes_date<-dictionnaire_import_data[dictionnaire_import_data$onglet == onglet &
                                            dictionnaire_import_data$format=="date",]$colonne

  if(length(colonnes_date)>0){
    is_date<-names(get(onglet))%in%colonnes_date
    tmp<- lapply(colonnes_date, function(x){as.Date(as.numeric(get(onglet)[[x]]), origin = "1899-12-30")})
    tmp<-as.data.frame(tmp)
    names(tmp)<-names(get(onglet))[names(get(onglet))%in%colonnes_date]
    assign(onglet, cbind(get(onglet)[,!is_date],tmp),
           envir = .GlobalEnv)

  }

}



  tryCatch(
    assign(onglet,
           readxl::read_excel(fichier_prog, onglet, col_types = "text"),
           envir = .GlobalEnv
    ))

  if (nrow(get(onglet)) == 0) {
    stop(paste0("Onglet ",onglet," ne contient pas de donn\u00e9es"))
  }

  tryCatch(test_noms_colonnes(fichier_prog, onglet))
  tryCatch(convert_format_colonnes(onglet))



}
