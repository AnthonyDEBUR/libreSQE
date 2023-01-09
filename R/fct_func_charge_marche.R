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
#' @export
func_charge_marche <- function(fichier_prog, connexion, mar_id)
{

  # func_charge_marche(fichier_prog, connexion, mar_id)

# fichier_prog<-"C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB2023_version dev libreSQE.xlsx"
# mar_id=18
# connexion <- pool::dbPool(RPostgres::Postgres(),
#                             dbname="libresqe",
#                             host="localhost",
#                             port=5432,
#                             user= "postgres",
#                             password= "postgres")


  ##### ENREGISTREMENT INITIAL DE LA TABLE run analytique #####
  func_lit_le_fichier(fichier_prog=fichier_prog, "cout_run_analytiques")

  func_lit_le_fichier(fichier_prog=fichier_prog,
                    onglet="programmes_types")

  #on recupère les infos sur les run_analytiques contenues dans l'onglet programme type
  spec_run_ana <- programmes_types %>%
                  dplyr::select(run_analytique,
                         code_methode_sandre,
                         siret_prestataire_analyse) %>%
                  unique()

  # on ajoute correspondance entre SIRET du prestataire et son identifiant
  # récupération de la table des prestataires
  conn<-  pool::poolCheckout(connexion)
  on.exit(pool::poolReturn(conn))
  pool::dbBegin(conn) # commence transaction SQL

  prestataires<-pool::dbGetQuery(conn,
                                "SELECT * FROM refer.tr_prestataire_pre;"
  )

  spec_run_ana<-dplyr::left_join(spec_run_ana, prestataires, by=c("siret_prestataire_analyse"="pre_siret"))
  if(any(is.na(spec_run_ana$pre_id))){stop(paste0("Table programme type : le(s) numéros SIRET suivants ne correspondent pas à un prestataire défini dans libreSQE : ",
                                                  paste(spec_run_ana[is.na(spec_run_ana$pre_id),]$siret_prestataire_analyse%>%unique(), collapse = " - ")))}


  # on vérifie l'absence de doublons parmi mes noms de runs analytiques
  verif<-spec_run_ana%>%dplyr::group_by(run_analytique)%>%dplyr::count()
  if(any(verif$n>1)){stop(
    paste0("Les runs analytiques suivants correspondent à plusieurs codes méthodes ou siret de prestataire : ", paste(verif[verif$n>1,]$run_analytique, collapse = " - ")))}

  rm(verif)

  spec_run_ana <- spec_run_ana %>%
    dplyr::select(run_analytique,
                  code_methode_sandre,
                  pre_id)

  #on complète le tableau cout run analytique avec les informations sur la méthode et le prestataire
  cout_run_analytiques<-dplyr::left_join(cout_run_analytiques, spec_run_ana, by="run_analytique")

  if(any(is.na(cout_run_analytiques$code_methode_sandre))){stop(
    paste0("Méthode sandre non renseignée dans les programmes types pour les run : ", paste(cout_run_analytiques[is.na(cout_run_analytiques$code_methode_sandre),]$run_analytique, collapse = " - ")))}

  if(any(is.na(cout_run_analytiques$siret_prestataire_analyse))){stop(
    paste0("SIRET du prestataire non renseigné dans les programmes types pour les run : ", paste(cout_run_analytiques[is.na(cout_run_analytiques$siret_prestataire_analyse),]$run_analytique, collapse = " - ")))}

  #renommage des colonnes avant intégration dans la base
  cout_run_analytiques<-dplyr::rename(cout_run_analytiques,
                                      "run_nom"="run_analytique",
                                      "run_met_code"="code_methode_sandre",
                                      "run_prestataire"="pre_id",
                                      "pru_valeur"="prix_unitaire_ht",
                                      "pru_datedebut"="date_debut_validite_prix",
                                      "pru_datefin"="date_fin_validite_prix")
  cout_run_analytiques$prr_mar_id<-mar_id

   # ecriture dans table t_prixunitairerunanalytique_prr
  tryCatch(
    {
  conn<-  pool::poolCheckout(connexion)
  on.exit(pool::poolReturn(conn))
  pool::dbBegin(conn) # commence transaction SQL


  schema_destination<-"SQE"
  schema_destination2<-"SQE"
  table_destination<-"t_runanalytique_run"
  table_destination2<-"t_prixunitairerunanalytique_prr"
  dataframe_a_enr<-cout_run_analytiques%>%dplyr::select(run_nom,
                                                        run_met_code,
                                                        run_prestataire)

  variables_a_retourner<-c("run_id", "run_nom")

  pool::dbExecute(conn,
                 paste0("DROP TABLE if exists temp.", table_destination, "_temp"))

  table_isa_id <- DBI::Id(schema = "temp",
                          table = paste0(table_destination, "_temp"))

  pool::dbWriteTable(conn, table_isa_id, dataframe_a_enr)

        retour<-pool::dbGetQuery(
        conn,
        paste0(
          "INSERT INTO ",
          schema_destination,
          ".",
          table_destination,
          " (",
          paste(names(dataframe_a_enr), collapse = ","),
          ") SELECT ",
          paste(names(dataframe_a_enr), collapse = ","),
          " FROM temp.",
          table_destination,
          "_temp RETURNING ",
          paste(variables_a_retourner, collapse = ","),
          ";"
        )
      )

        cout_run_analytiques<-dplyr::full_join(cout_run_analytiques, retour, by="run_nom")
        cout_run_analytiques<-dplyr::rename(cout_run_analytiques,
                                            "prr_run_id"="run_id")

      dataframe_a_enr<-cout_run_analytiques%>%dplyr::select(prr_mar_id,
                                                            prr_run_id,
                                                            pru_datedebut,
                                                            pru_datefin,
                                                            pru_valeur)


      pool::dbCommit(conn)
      },
      error = function(e) {
        print(e$message)
        pool::dbRollback(conn)
      }
    )


  dataframe_a_enr<-cout_run_analytiques%>%dplyr::select(prr_mar_id,
                                                        prr_run_id,
                                                        pru_datedebut,
                                                        pru_datefin,
                                                        pru_valeur)

  tryCatch(
    {
 pool::dbBegin(conn) # commence transaction SQL

  DBI::dbExecute(connexion,
                 paste0("DROP TABLE if exists temp.", table_destination2, "_temp"))

  table_isa_id <- DBI::Id(schema = "temp",
                          table = paste0(table_destination2, "_temp"))

  DBI::dbWriteTable(connexion, table_isa_id, dataframe_a_enr)

  DBI::dbExecute(
    connexion,
    paste0(
      "INSERT INTO ",
      schema_destination2,
      ".",
      table_destination2,
      " (",
      paste(names(dataframe_a_enr), collapse = ","),
      ") SELECT ",
      paste(names(dataframe_a_enr), collapse = ","),
      " FROM temp.",
      table_destination2,
      "_temp;"
    )
  )


  pool::dbCommit(conn)
},
error = function(e) {
  print(e$message)
  pool::dbRollback(conn)
}
)


##### ENREGISTREMENT DE LA TABLE DES BPU #####

func_lit_le_fichier(fichier_prog=fichier_prog,"BPU")

BPU$prs_mar_id<-mar_id
BPU<-dplyr::left_join(BPU, prestataires, by=c("siret_prestataire"="pre_siret"))

if(any(is.na(BPU$pre_id))) {
  tmp <-
    BPU[is.na(BPU$pre_id), c("siret_prestataire", "nom_prestataire")] %>% unique()

   stop(
    paste0(
      "Onglet BPU : le(s) codes SIRET suivants ne correspondent pas à des intervenants connus dans la base de données : ",
      paste(tmp[, 1], " - ", tmp[, 2], collapse = "; ")
    )
  )
}


table_a_enregistrer<-data.frame(
                                prs_mar_id=mar_id,
                                prs_pre_id=BPU$pre_id,
                                prs_nomprestation=BPU$nom_complet_prestation,
                                prs_label_prestation=BPU$label_prestation,
                                prs_natureprestation=BPU$nature_prestation,
                                prm_unitedoeuvre=BPU$unite_facturation,
                                prs_idprestationdansbpu=BPU$id_prestation)

func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
                                          table_destination="t_prestation_prs",
                                          schema_destination="sqe",
                                          connexion=connexion)


# enregistrement des prix BPU

# on récupère les prs_id
t_prestation_prs<-pool::dbGetQuery(connexion, paste0("SELECT * FROM sqe.t_prestation_prs WHERE prs_mar_id=",mar_id,";"))

BPU<-dplyr::left_join(BPU, t_prestation_prs, by=c("prs_mar_id",
                                                   "pre_id"="prs_pre_id",
                                                   "nom_complet_prestation"="prs_nomprestation"))


table_a_enregistrer<-data.frame(
  pru_datedebut=BPU$date_debut_validite_prix,
  pru_datefin=BPU$date_fin_validite_prix,
  pru_valeur=BPU$prix_unitaire_HT,
  prp_prs_id=BPU$prs_id
)


func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
                              table_destination="t_prixunitaireprestation_prp",
                              schema_destination="sqe",
                              connexion=connexion)


##### ENREGISTREMENT INITIAL DE LA TABLE PROGRAMMES TYPES #####


# ajout de l'info run_id
  programmes_types<-dplyr::left_join(programmes_types, retour, by=c("run_analytique"="run_nom"))

# ajout de l'info prs_id
  programmes_types<-dplyr::left_join(programmes_types, t_prestation_prs, by=c("programme"="prs_label_prestation"))

# ajout de l'info pre_id
  programmes_types<-dplyr::left_join(programmes_types, prestataires, by=c("siret_prestataire_analyse"="pre_siret"))
  if(any(is.na(programmes_types$pre_id))) {
    tmp <-
      programmes_types[is.na(programmes_types$pre_id), c("siret_prestataire_analyse", "prestataire_analyse")] %>% unique()

    stop(
      paste0(
        "Onglet programmes types : le(s) codes SIRET suivants ne correspondent pas à des intervenants connus dans la base de données : ",
        paste(tmp[, 1], " - ", tmp[, 2], collapse = "; ")
      )
    )
  }


  table_a_enregistrer<-data.frame(
    ppt_prs_id=programmes_types$prs_id,
    ppt_mar_id=mar_id,
    ppt_run_id=programmes_types$run_id,
    ppt_par_cdparametre=programmes_types$code_parametre_sandre,
    ppt_fra_codefraction=programmes_types$code_fraction_sandre,
    ppt_nomparametre=programmes_types$nom_parametre_sandre,
    ppt_uni_codesandreunite=programmes_types$code_unite_sandre,
    ppt_met_codesandremethode=programmes_types$code_methode_sandre,
    ppt_pre_id=programmes_types$pre_id,
    ppt_analyseinsitu=ifelse(tolower(programmes_types$analyse_in_situ)=="oui", TRUE, FALSE),
    ppt_limitedetec=programmes_types$limite_detection_garantie,
    ppt_limitequantif=programmes_types$limite_quantification_garantie,
    ppt_incertitude=programmes_types$incertitude_garantie,
    ppt_accreditation=ifelse(tolower(programmes_types$accreditation)=="oui", TRUE, FALSE),
    ppt_commentaireparametre=programmes_types$commentaires_parametre)

  func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
                                table_destination="t_parametreprogrammetype_ppt",
                                schema_destination="sqe",
                                connexion=connexion)





}
