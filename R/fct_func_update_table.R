#' func_update_table
#'
#' @description Fonction pour mettre à jour dans la base postgresql une table
#'
#' @return The return value, if any, from executing the function.
#'
#'@param dataframe_a_enr dataframe à enregistrer dans la base. Ses noms de colonnes doivent
#'correspondre aux noms de champs de la bdd
#'@param table_destination : nom postgreSQL de la table dans laquelle on souhaite enregistrer
#'@param cle : nom commun de la colonne de data.frame et de la cle primaire de la table de destination
#'@param schema_destination : nom postgreSQL du schéma sous lequel enregistrer
#'@param connexion : connexion vers la base postgreSQL
#'
#'
#'
#' Les noms de colonnes doivent correspondre aux noms de champs dans la base.
#'

#'
#' @return résultat de l'exécution de la requête.
#' @export

func_update_table <- function(dataframe_a_enr,
                              cle,
                              table_destination,
                              schema_destination,
                              connexion)
{
  if(!("character"%in%class(cle))){stop("le paramètre cle doit être de classe character")}
  if(!all(cle%in%names(dataframe_a_enr))){stop("la dataframe à enregistrer doit avoir parmi ses colonnes la cle de mise à jour.")}

  # cle<-"par_cdparametre"
  # dataframe_a_enr<-func_charge_referentiel_SANDRE_parametres(date_maj="2022-03-01")
  # schema_destination<-"refer"
  # table_destination<-"tr_parametre_par"
  # connexion <- DBI::dbConnect(RPostgres::Postgres(),
  #                             dbname="libresqe",
  #                             host="localhost",
  #                             port=5432,
  #                             user= "postgres",
  #                             password= "postgres")

  # chargement de la table à mettre à jour
  tbl_a_maj<-DBI::dbGetQuery(connexion, paste0("SELECT * FROM ",schema_destination,".",table_destination,";"))

  # identification des lignes à insérer
  lignes_a_inserer<-dataframe_a_enr[!(dataframe_a_enr[[cle]]%in%tbl_a_maj[[cle]]),]

  if(nrow(lignes_a_inserer)>0)
{  ##### création des nouvelles lignes dans la table #####
  table_isa_id <- DBI::Id(schema = "temp",
                          table = paste0(table_destination, "_temp"))
  # ecriture de la table
  conn<-  pool::poolCheckout(connexion)
  on.exit(pool::poolReturn(conn))
  pool::dbBegin(conn) # commence transaction SQL
  DBI::dbExecute(connexion,
                 paste0("DROP TABLE if exists temp.", table_destination, "_temp"))
  DBI::dbWriteTable(connexion, table_isa_id, lignes_a_inserer)
  tryCatch(
    {DBI::dbExecute(
      connexion,
      paste0(
        "INSERT INTO ",
        schema_destination,
        ".",
        table_destination,
        " (",
        paste(names(dataframe_a_enr), collapse = ","),
        ")
                    SELECT ",
        paste(names(dataframe_a_enr), collapse = ","),
        " FROM temp.",
        table_destination,
        "_temp;"
      )
    )
      DBI::dbCommit(connexion)
      },
    error = function(e) {
      print(e$message)
      pool::dbRollback(conn)
    }
  )}

  ##### mise à jour des lignes de la table déjà existantes #####
  lignes_a_inserer<-dataframe_a_enr[(dataframe_a_enr[[cle]]%in%tbl_a_maj[[cle]]),]

  if(nrow(lignes_a_inserer)>0){

    table_isa_id <- DBI::Id(schema = "temp",
                            table = paste0(table_destination, "_temp"))
    # ecriture de la table
    conn<-  pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn))
    pool::dbBegin(conn) # commence transaction SQL
    DBI::dbExecute(connexion,
                   paste0("DROP TABLE if exists temp.", table_destination, "_temp"))
    DBI::dbWriteTable(connexion, table_isa_id, lignes_a_inserer)
    tryCatch(
      {DBI::dbExecute(
        connexion,

        # UPDATE refer.tr_parametre_par
        # SET par_nomparametre = tt.par_nomparametre,
        # par_statutparametre = tt.par_statutparametre,
        # par_nomcourt = tt.par_nomcourt,
        # par_codecas = tt.par_codecas
        # FROM temp.tr_parametre_par_temp tt
        # WHERE tt.par_cdparametre = tr_parametre_par.par_cdparametre ;

        paste0(
          "UPDATE ",
          schema_destination,
          ".",
          table_destination,
          " SET ",
          paste(paste0(names(dataframe_a_enr)[names(dataframe_a_enr)!=cle],
                      " = tt.",
                      names(dataframe_a_enr)[names(dataframe_a_enr)!=cle]),
                       collapse = ","),
          " FROM temp.",
          table_destination,
          "_temp tt WHERE tt.", cle," = ",
           table_destination,".",cle,";"
        )
      )
        pool::dbCommit(conn)
      },
      error = function(e) {
        print(e$message)
        pool::dbRollback(conn)
      }
    )
 #   pool::poolReturn(conn)

  }




}
