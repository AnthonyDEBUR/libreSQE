#' func_maj_referentiels_sandre
#'
#' @description Fonction pour mettre à jour les référentiels SANDRE dans la base de données
#'
#' @param connexion : connexion à la bdd postgreSQL
#'
#' @return Met à jour la bdd postgreSQL associée à libre SQE en actualisant les tables liées au référentiel SANDRE.
#'
# func_maj_referentiels_sandre(connexion)
func_maj_referentiels_sandre<-function(connexion)
{

  # connexion <- DBI::dbConnect(RPostgres::Postgres(),
  #                             dbname="libresqe",
  #                             host="localhost",
  #                             port=5432,
  #                             user= "postgres",
  #                             password= "postgres")


  # on récupère la table avec la date des dernières mises à jour du référentiel
  dates_maj<-DBI::dbReadTable(connexion, DBI::Id(schema="sqe",
                                                 table="ts_suivi_maj_refer"))


  # Mise à jour table référentiel paramètres SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_parametre_par",]$ts_date-1

  table_sandre<-func_charge_referentiel_SANDRE_parametres(date_maj=date_maj_table)
if(!is.null(table_sandre))
 { func_update_table(table_sandre,
                    "par_cdparametre",
                    "tr_parametre_par",
                    "refer",
                    connexion)}


  # Mise à jour table référentiel fractions SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_fraction_fra",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_fractions(date_maj=date_maj_table)

  if(!is.null(table_sandre))
 { func_update_table(table_sandre,
                    "fra_codefraction",
                    "tr_fraction_fra",
                    "refer",
                    connexion)}



  # Mise à jour table référentiel intervenants SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_intervenantsandre_isa",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_intervenants(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
  func_update_table(table_sandre,
                    "isa_codesandre",
                    "tr_intervenantsandre_isa",
                    "refer",
                    connexion)}

  # Mise à jour table référentiel unités SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_uniteparametre_uni",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_intervenants(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
    func_update_table(table_sandre,
                      "uni_codesandreunite",
                      "tr_uniteparametre_uni",
                      "refer",
                      connexion)}


  # Mise à jour table référentiel méthodes SANDRE
  # date_maj_table<-dates_maj[dates_maj$ts_table=="tr_methode_met",]$ts_date-1
  #
  # table_sandre<-func_charge_ref_sandre_fractions(date_maj=date_maj_table)
  #
  # func_update_table(table_sandre,
  #                   "fra_codefraction",
  #                   "tr_methode_met",
  #                   "refer",
  #                   connexion)


}
