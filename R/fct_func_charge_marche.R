#' func_charge_marche
#'
#' @description Fonction pour charger les marchés à partir des fichiers EXCEL
#' au format imposé et le bancariser dans la bdd
#' Sont chargés BPU, cout_run_analytiques et programmes_types
#'
#' @return Un programme prévisionnel
#'
#' @param fichier_prog : fichier xlsx définissant le marché.
#' Le fichier contient les onglets programmes_types, BPU, cout_run_analytiques
#' Ces onglets ont a minima les colonnes prévues au dictionnaire de données
#' @param connexion : connexion à la BDD
#' @param mar_id : identifiant du marché concerné par la programmation dans la table marché
#'
#' func_charge_marche(fichier_prog, connexion, mar_id)
func_charge_marche <- function(fichier_prog, connexion, mar_id)
{

  # fichier_prog<-"C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB Est_Ouest 2022 - commande_3 derniers trimestres_ajout suivis Captages_version dev libreSQE.xlsx"
# mar_id=17
  # connexion <- DBI::dbConnect(RPostgres::Postgres(),
  #                             dbname="libresqe",
  #                             host="localhost",
  #                             port=5432,
  #                             user= "postgres",
  #                             password= "postgres")

  func_lit_le_fichier(fichier_prog=fichier_prog,
                      onglet="programmes_types")


  func_lit_le_fichier(fichier_prog=fichier_prog,"BPU")

  # Table run analytique
  func_lit_le_fichier(fichier_prog=fichier_prog, "cout_run_analytiques")

  cout_run_analytiques<-dplyr::rename(cout_run_analytiques,
                                      "run_nom"="run_analytique",
                                      "run_met_code"="code_methode_sandre",
                                      "pru_valeur"="prix_unitaire_ht",
                                      "pru_datedebut"="date_debut_validite_prix",
                                      "pru_datefin"="date_fin_validite_prix")

    t_prixunitairerunanalytique_prr <-
    cout_run_analytiques %>% dplyr::select(run_analytique,
                                           code_methode_sandre,
                                           prix_unitaire_ht,
                                           date_debut_validite_prix,
                                           date_fin_validite_prix)
  t_prixunitairerunanalytique_prr$prr_mar_id <- mar_id
  table_runanalytique <-
    cout_run_analytiques %>% dplyr::select(run_analytique, code_methode_sandre)
  names(table_runanalytique) <- c("run_nom", "run_met_code")


}
