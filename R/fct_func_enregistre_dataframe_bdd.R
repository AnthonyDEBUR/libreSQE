#' func_enregistre_dataframe_bdd
#'
#' @description Fonction pour ajouter un dataframe au contenu d'une table de la base libreSQE
#'
#' @param dataframe_a_enr Dataframe à enregistrer dans la base.
#' Les noms de colonnes doivent correspondre aux noms de champs dans la base.
#'
#' @param table_destination : nom postgreSQL de la table dans laquelle on souhaite enregistrer
#'
#' @param schema_destination : nom postgreSQL du schéma sous lequel enregistrer
#'
#' @param connexion : connexion vers la base postgreSQL
#'
#' @return résultat de l'exéctution de la requête.
#'
#' @noRd

func_enregistre_dataframe_bdd <- function(dataframe_a_enr,
                                          table_destination,
                                          schema_destination,
                                          connexion)
{
  table_isa_id <- DBI::Id(schema = "temp",
                          table = paste0(table_destination, "_temp"))
  # ecriture de la table
  DBI::dbBegin(connexion) # commence transaction SQL
  DBI::dbExecute(connexion,
                 paste0("DROP TABLE if exists temp.", table_destination, "_temp"))
  DBI::dbWriteTable(connexion, table_isa_id, dataframe_a_enr)
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
    DBI::dbCommit(connexion)}
    ,
    error = function(e) {
      print(e$message)
      DBI::dbRollback(connexion)
    }
  )
}
