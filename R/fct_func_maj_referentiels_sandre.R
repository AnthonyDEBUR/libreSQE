#' func_maj_referentiels_sandre
#'
#' @description Fonction pour mettre à jour les référentiels SANDRE dans la base de données
#'
#' @param connexion : connexion à la bdd postgreSQL
#'
#' @return Met à jour la bdd postgreSQL associée à libre SQE en actualisant les tables liées au référentiel SANDRE.
#'
# connexion <- pool::dbPool(RPostgres::Postgres(),
#                           dbname="libresqe",
#                           host="localhost",
#                           port=5432,
#                           user= "postgres",
#                           password= "postgres")
# func_maj_referentiels_sandre(connexion)
func_maj_referentiels_sandre<-function(connexion)
{




  # on récupère la table avec la date des dernières mises à jour du référentiel
  # dates_maj<-DBI::dbReadTable(connexion, DBI::Id(schema="sqe",
  #                                                table="ts_suivi_maj_refer"))

  dates_maj<-pool::dbGetQuery(connexion, "SELECT * FROM sqe.ts_suivi_maj_refer;")

  # Mise à jour table référentiel paramètres SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_parametre_par",]$ts_date-1

  table_sandre<-func_charge_referentiel_SANDRE_parametres(date_maj=date_maj_table)
if(!is.null(table_sandre))
 {func_update_table(table_sandre,
                    "par_cdparametre",
                    "tr_parametre_par",
                    "refer",
                    connexion)
  Sys.sleep(3)}


  # Mise à jour table référentiel fractions SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_fraction_fra",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_fractions(date_maj=date_maj_table)

  if(!is.null(table_sandre))
 { func_update_table(table_sandre,
                    "fra_codefraction",
                    "tr_fraction_fra",
                    "refer",
                    connexion)
    Sys.sleep(3)}



  # Mise à jour table référentiel unités SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_uniteparametre_uni",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_unites(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
    func_update_table(table_sandre,
                      "uni_codesandreunite",
                      "tr_uniteparametre_uni",
                      "refer",
                      connexion)
    Sys.sleep(3)}


  # Mise à jour table référentiel méthodes SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_methode_met",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_methodes(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
  func_update_table(table_sandre,
                    "met_code",
                    "tr_methode_met",
                    "refer",
                    connexion)
          Sys.sleep(3)
        }


  # Mise à jour table dispositifs de collectes (réseaux de mesures) du SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_rdd_rdd",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_dispocollecte(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
  func_update_table(dataframe_a_enr=table_sandre,
                    cle="rdd_cdrdd",
                    table_destination="tr_rdd_rdd",
                    schema_destination="refer",
                    connexion=connexion)

    Sys.sleep(3)
    }

  # Mise à jour table stations de mesures du SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_stationmesure_stm",]$ts_date-1

  table_sandre<-func_charge_ref_sandre_staq(date_maj=date_maj_table)
  if(!is.null(table_sandre)){if(nrow(table_sandre)>0){
    table_sandre$stm_cdstationmesureinterne<-table_sandre$stm_cdstationmesureauxsurface

    func_update_table(table_sandre,
                      "stm_cdstationmesureauxsurface",
                        "tr_stationmesure_stm",
                      "refer",
                      connexion)

    Sys.sleep(3)
    }}



  # Mise à jour table référentiel intervenants SANDRE
  date_maj_table<-dates_maj[dates_maj$ts_table=="tr_intervenantsandre_isa",]$ts_date-1
   table_sandre<-func_charge_ref_sandre_intervenants(date_maj=date_maj_table)
  if(!is.null(table_sandre)){
    func_update_table(table_sandre,
                      "isa_codesandre",
                      "tr_intervenantsandre_isa",
                      "refer",
                      connexion)}




}
