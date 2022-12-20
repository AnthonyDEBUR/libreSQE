#' func_charge_prog_annuelle
#'
#' @description Fonction pour charger les programmations annuelles à partir des fichiers EXCEL
#' au format imposé et le bancariser dans la bdd
#' sont chargés programme_annuel et calendrier
#'
#' @return Un programme prévisionnel
#'
#' @param fichier_prog : fichier xlsx définissant le marché.
#' Le fichier contient les onglets programme_annuel et calendrier
#' Ces onglets ont a minima les colonnes prévues au dictionnaire de données
#' @param connexion : connexion à la BDD
#' @param mar_id : identifiant du marché concerné par la programmation dans la table marché
#' @param annee : année civile de la programmation saisie
#'
#' func_charge_marche(fichier_prog, connexion, mar_id)
func_charge_prog_annuelle <- function(fichier_prog, connexion, mar_id, annee)
{
  #
  fichier_prog<-"C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB Est_Ouest 2022 - commande_3 derniers trimestres_ajout suivis Captages_version dev libreSQE.xlsx"
  mar_id=17
  connexion <- pool::dbPool(RPostgres::Postgres(),
                            dbname="libresqe",
                            host="localhost",
                            port=5432,
                            user= "postgres",
                            password= "postgres")

  annee<-2022


  ##### ENREGISTREMENT INITIAL DE LA TABLE run analytique #####
  func_lit_le_fichier(fichier_prog=fichier_prog, "calendrier")


mise_en_forme_calendrier<-function(calendrier, annee)
    {calendrier<-tidyr::pivot_longer(calendrier,
                                   cols=c(janvier,
                                          février,
                                          mars,
                                          avril,
                                          mai,
                                          juin,
                                          juillet,
                                          aout,
                                          septembre,
                                          octobre,
                                          novembre,
                                          décembre),
                                   names_to="mois",
                                   values_to="nb")

    calendrier<-calendrier%>%subset(nb>0)

    mois_numeric<-data.frame(mois=c("janvier",
                                          "février",
                                          "mars",
                                          "avril",
                                          "mai",
                                          "juin",
                                          "juillet",
                                          "aout",
                                          "septembre",
                                          "octobre",
                                          "novembre",
                                          "décembre"),
                             num_mois=seq(1,12,1)
                             )

    calendrier<-dplyr::left_join(calendrier, mois_numeric, by="mois")


    if(any(!calendrier$nb%in%seq(1,31,by=1))){stop("Le nombre de prélèvements mensuels doit être un entier entre 1 et 31")}

    ecart_2prel<-function(n){
      if(n==1){out<-1}else{out<-seq(1,floor(31/n)*n,by=floor(31/n))}
      return(out)
    }

    tmp<-lapply(calendrier$nb, ecart_2prel)

    for(i in 1:nrow(calendrier))
    {
      calendrier1<-data.frame(calendrier[i,], jour=tmp[[i]])

      if(i==1){calendrier_out<-calendrier1}else{calendrier_out<-rbind(calendrier_out,calendrier1)}
    }
    calendrier<-calendrier_out
    calendrier$cal_date<-as.Date(paste0(annee,"-",calendrier$num_mois, "-",calendrier$jour))
    calendrier<-calendrier%>%dplyr::select(-nb, -num_mois,-jour)

    calendrier$cal_refannee<-annee
    calendrier$cal_mar_id<-mar_id

    # faire join sur les id de prestation à partir du nom du programme
    # calendrier$cal_prs_id


    calendrier<-dplyr::rename(calendrier,
                              "cal_typestation"="type_station")

    return(calendrier)
    }

calendrier<-mise_en_forme_calendrier(calendrier, annee)





#
#
#   # ecriture dans table t_prixunitairerunanalytique_prr
#   conn<-  pool::poolCheckout(connexion)
#   on.exit(pool::poolReturn(conn))
#   pool::dbBegin(conn) # commence transaction SQL
#
#
#   schema_destination<-"SQE"
#   schema_destination2<-"SQE"
#   table_destination<-"t_runanalytique_run"
#   table_destination2<-"t_prixunitairerunanalytique_prr"
#   dataframe_a_enr<-cout_run_analytiques%>%dplyr::select(run_nom,
#                                                         run_met_code)
#
#   variables_a_retourner<-c("run_id", "run_nom")
#
#   DBI::dbExecute(connexion,
#                  paste0("DROP TABLE if exists temp.", table_destination, "_temp"))
#
#   table_isa_id <- DBI::Id(schema = "temp",
#                           table = paste0(table_destination, "_temp"))
#
#   DBI::dbWriteTable(connexion, table_isa_id, dataframe_a_enr)
#   tryCatch(
#     {
#       retour<-DBI::dbGetQuery(
#         connexion,
#         paste0(
#           "INSERT INTO ",
#           schema_destination,
#           ".",
#           table_destination,
#           " (",
#           paste(names(dataframe_a_enr), collapse = ","),
#           ") SELECT ",
#           paste(names(dataframe_a_enr), collapse = ","),
#           " FROM temp.",
#           table_destination,
#           "_temp RETURNING ",
#           paste(variables_a_retourner, collapse = ","),
#           ";"
#         )
#       )
#
#       cout_run_analytiques<-dplyr::inner_join(retour, cout_run_analytiques, by=c("run_nom"))
#       dataframe_a_enr<-cout_run_analytiques%>%dplyr::select(prr_mar_id,
#                                                             run_id,
#                                                             pru_datedebut,
#                                                             pru_datefin,
#                                                             pru_valeur)
#
#       dataframe_a_enr<-dataframe_a_enr%>%dplyr::rename("prr_run_id"="run_id")
#
#       DBI::dbExecute(connexion,
#                      paste0("DROP TABLE if exists temp.", table_destination2, "_temp"))
#
#       table_isa_id <- DBI::Id(schema = "temp",
#                               table = paste0(table_destination2, "_temp"))
#
#       DBI::dbWriteTable(connexion, table_isa_id, dataframe_a_enr)
#
#       DBI::dbExecute(
#         connexion,
#         paste0(
#           "INSERT INTO ",
#           schema_destination2,
#           ".",
#           table_destination2,
#           " (",
#           paste(names(dataframe_a_enr), collapse = ","),
#           ") SELECT ",
#           paste(names(dataframe_a_enr), collapse = ","),
#           " FROM temp.",
#           table_destination2,
#           "_temp;"
#         )
#       )
#
#
#       pool::dbCommit(conn)
#     },
#     error = function(e) {
#       print(e$message)
#       pool::dbRollback(conn)
#     }
#   )
#
#   ##### ENREGISTREMENT DE LA TABLE DES PROGRAMMES ANNUELS #####
#
#   func_lit_le_fichier(fichier_prog=fichier_prog,"programme_annuel")
#
#   # récupération de la table des prestataires
#   conn<-  pool::poolCheckout(connexion)
#   on.exit(pool::poolReturn(conn))
#   pool::dbBegin(conn) # commence transaction SQL
#
#   prestataires<-DBI::dbGetQuery(connexion,
#                                 "SELECT * FROM refer.tr_prestataire_pre;"
#   )
#
#   BPU<-dplyr::left_join(BPU, prestataires, by=c("siret_prestataire"="pre_siret"))
#   BPU$prs_mar_id<-mar_id
#
#   table_a_enregistrer<-data.frame(
#     prs_mar_id=mar_id,
#     prs_pre_id=BPU$pre_id,
#     prs_nomprestation=BPU$nom_complet_prestation,
#     prs_label_prestation=BPU$label_prestation,
#     prs_natureprestation=BPU$nature_prestation,
#     prm_unitedoeuvre=BPU$unite_facturation)
#
#   func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
#                                 table_destination="t_prestation_prs",
#                                 schema_destination="sqe",
#                                 connexion=connexion)
#
#
#   # enregistrement des prix BPU
#
#   # on récupère les prs_id
#   t_prestation_prs<-DBI::dbGetQuery(connexion, paste0("SELECT * FROM sqe.t_prestation_prs WHERE prs_mar_id=",mar_id,";"))
#
#   BPU<-dplyr::left_join(BPU, t_prestation_prs, by=c("prs_mar_id",
#                                                     "pre_id"="prs_pre_id",
#                                                     "nom_complet_prestation"="prs_nomprestation"))
#
#
#   table_a_enregistrer<-data.frame(
#     pru_datedebut=BPU$date_debut_validite_prix,
#     pru_datefin=BPU$date_fin_validite_prix,
#     pru_valeur=BPU$prix_unitaire_HT,
#     prp_prs_id=BPU$prs_id
#   )
#
#
#   func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
#                                 table_destination="t_prixunitaireprestation_prp",
#                                 schema_destination="sqe",
#                                 connexion=connexion)
#
#
#   ##### ENREGISTREMENT INITIAL DE LA TABLE PROGRAMMES TYPES #####
#   func_lit_le_fichier(fichier_prog=fichier_prog,
#                       onglet="programmes_types")
#
#   # ajout de l'info run_id
#   programmes_types<-dplyr::left_join(programmes_types, retour, by=c("run_analytique"="run_nom"))
#
#   # ajout de l'info prs_id
#   programmes_types<-dplyr::left_join(programmes_types, t_prestation_prs, by=c("programme"="prs_label_prestation"))
#
#   table_a_enregistrer<-data.frame(
#     ppt_prs_id=programmes_types$prs_id,
#     ppt_mar_id=mar_id,
#     ppt_run_id=programmes_types$run_id,
#     ppt_par_cdparametre=programmes_types$code_parametre_sandre,
#     ppt_codetemporaireparametre=programmes_types$code_parametre_sandre,
#     ppt_fra_codefraction=programmes_types$code_fraction_sandre,
#     ppt_nomparametre=programmes_types$nom_parametre_sandre,
#     ppt_uni_codesandreunite=programmes_types$code_unite_sandre,
#     ppt_analyseinsitu=ifelse(tolower(programmes_types$analyse_in_situ)=="oui", TRUE, FALSE),
#     ppt_limitedetec=programmes_types$limite_detection_garantie,
#     ppt_limitequantif=programmes_types$limite_quantification_garantie,
#     ppt_incertitude=programmes_types$incertitude_garantie,
#     ppt_accreditation=ifelse(tolower(programmes_types$accreditation)=="oui", TRUE, FALSE),
#     ppt_commentaireparametre=programmes_types$commentaires_parametre)
#
#   func_enregistre_dataframe_bdd(dataframe_a_enr=table_a_enregistrer,
#                                 table_destination="t_parametreprogrammetype_ppt",
#                                 schema_destination="sqe",
#                                 connexion=connexion)
#
#
#


}
