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
#' @param frequence_bdc : fréquence d'émission des bons de commandes.
#' Peut prendre les valeurs suivantes : mensuelle (défaut), bimestrielle, trimestrielle, semestrielle, annuelle
#' @param prefixe : character à ajouter en début de la référence de chaque bon de commande ("" par défaut)
#'
#' @export
#' func_charge_marche(fichier_prog, connexion, mar_id, frequence_bdc="mensuelle")
func_charge_prog_annuelle <-
  function(fichier_prog, connexion, mar_id, annee, frequence_bdc="mensuelle", prefixe="")
  {

    frequence_bdc="mensuelle"
    prefixe<-"SQE2023"

    fichier_prog <-
      "C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB2023_version dev libreSQE.xlsx"
    mar_id = 18
    connexion <- pool::dbPool(
      RPostgres::Postgres(),
      dbname = "libresqe",
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = "postgres"
    )

    annee <- 2023

    if (!frequence_bdc %in% c("mensuelle",
                              "bimestrielle",
                              "trimestrielle",
                              "semestrielle",
                              "annuelle")) {
      stop(
        "La fréquence du bon de commande (frequence_bdc) doit avoir une des valeurs suivantes mensuelle, bimestrielle, trimestrielle, semestrielle, annuelle"
      )
    }

    if(!"character"%in%class(prefixe)){stop("chargement prog annuelle : prefixe doit être de classe character")}


    ##### ENREGISTREMENT INITIAL DE LA TABLE calendrier (type de station et date de mise en oeuvre de chaque prestatation) #####
    func_lit_le_fichier(fichier_prog = fichier_prog, "calendrier")


    mise_en_forme_calendrier <- function(calendrier, annee)
    {
      calendrier <- tidyr::pivot_longer(
        calendrier,
        cols = c(
          janvier,
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
          décembre
        ),
        names_to = "mois",
        values_to = "nb"
      )

      calendrier <- calendrier %>% subset(nb > 0)

      mois_numeric <- data.frame(
        mois = c(
          "janvier",
          "f\u00e9vrier",
          "mars",
          "avril",
          "mai",
          "juin",
          "juillet",
          "aout",
          "septembre",
          "octobre",
          "novembre",
          "d\u00e9cembre"
        ),
        num_mois = seq(1, 12, 1)
      )

      calendrier <-
        dplyr::left_join(calendrier, mois_numeric, by = "mois")


      if (any(!calendrier$nb %in% seq(1, 31, by = 1))) {
        stop("Le nombre de pr\u00e9l\u00e8vements mensuels doit \u00eatre un entier entre 1 et 31")
      }

      ecart_2prel <- function(n) {
        if (n == 1) {
          out <- 1
        } else{
          out <- seq(1, floor(31 / n) * n, by = floor(31 / n))
        }
        return(out)
      }

      tmp <- lapply(calendrier$nb, ecart_2prel)

      for (i in 1:nrow(calendrier))
      {
        calendrier1 <- data.frame(calendrier[i,], jour = tmp[[i]])

        if (i == 1) {
          calendrier_out <-
            calendrier1
        } else{
          calendrier_out <- rbind(calendrier_out, calendrier1)
        }
      }
      calendrier <- calendrier_out
      calendrier$cal_date <-
        as.Date(paste0(annee, "-", calendrier$num_mois, "-", calendrier$jour))
      calendrier <-
        calendrier %>% dplyr::select(-nb, -num_mois,-jour)

      calendrier$cal_refannee <- annee
      calendrier$cal_mar_id <- mar_id

      # faire join sur les id de prestation à partir du nom du programme
      # calendrier$cal_prs_id


      calendrier <- dplyr::rename(calendrier,
                                  "cal_typestation" = "type_station")

      return(calendrier)
    }

    calendrier <- mise_en_forme_calendrier(calendrier, annee)


    # récupération de la table des prestations
    conn <-  pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn))
    pool::dbBegin(conn) # commence transaction SQL

    prestatations <- pool::dbGetQuery(
      conn,
      paste0(
        "SELECT prs_id, prs_label_prestation FROM sqe.t_prestation_prs WHERE prs_mar_id=",
        mar_id,
        ";"
      )
    )


    calendrier <-
      dplyr::left_join(calendrier,
                       prestatations,
                       by = c("programme" = "prs_label_prestation"))



    if (any(is.na(calendrier$prs_id))) {
      stop(
        paste0(
          "Table calendrier : le(s) noms de programmes suivants ne correspondent pas \u00e0 des programmes de ce march\u00e9 dans libreSQE : ",
          paste(calendrier[is.na(calendrier$prs_id),]$programme %>%
                  unique(), collapse = " - ")
        )
      )
    }

    #renommage des colonnes avant intégration dans la base
    calendrier <- dplyr::rename(calendrier,
                                "cal_prs_id" = "prs_id",
                                "cal_rattachement_bdc" = "rattachement_bdc")

    # enregistrement du calendrier en bdd (t_calendrierprog_cal)
    table_a_enregistrer <-
      calendrier %>% dplyr::select(
        cal_refannee,
        cal_mar_id,
        cal_typestation,
        cal_date,
        cal_prs_id,
        cal_rattachement_bdc
      )

    func_enregistre_dataframe_bdd(
      dataframe_a_enr = table_a_enregistrer,
      table_destination = "t_calendrierprog_cal",
      schema_destination = "sqe",
      connexion = connexion
    )

    ##### ENREGISTREMENT DE LA PROG ANNUELLE #####
    func_lit_le_fichier(fichier_prog = fichier_prog, "programme_annuel")

    programme_annuel$pga_mar_id <- mar_id
    programme_annuel$pga_cal_refannee <- annee

    # on complète la colonne code_interne_station lorsque les données sont vides avec code_sandre_station
    programme_annuel[is.na(programme_annuel$code_interne_station),]$code_interne_station<-programme_annuel[is.na(programme_annuel$code_interne_station),]$code_sandre_station

    #renommage des colonnes et sélection des colonnes avant intégration dans la base
    programme_annuel <- dplyr::rename(
      programme_annuel,
      "pga_per_nom" = "perimetre_facturation",
      "pga_cal_typestation" = "type_station",
      "pga_stm_cdstationmesureauxsurface" = "code_sandre_station",
      "pga_stm_cdstationmesureinterne" = "code_interne_station"
    ) %>% dplyr::select(
      pga_cal_refannee,
      pga_mar_id,
      pga_per_nom,
      pga_cal_typestation,
      pga_stm_cdstationmesureauxsurface,
      pga_stm_cdstationmesureinterne
    )

    # Insertion dans la bdd
    func_enregistre_dataframe_bdd(
      dataframe_a_enr = programme_annuel,
      table_destination = "t_progannuelle_pga",
      schema_destination = "sqe",
      connexion = connexion
    )


 ##### Préparation des bons de commandes #####

prog_annuelle<-dplyr::left_join(programme_annuel, calendrier, by=c("pga_cal_typestation"='cal_typestation'))

    prog_annuelle$mois1<-as.factor(prog_annuelle$mois)%>%dplyr::recode_factor(
      "janvier"="1",
      "février"="2",
      "mars"="3",
      "avril"="4",
      "mai"="5",
      "juin"="6",
      "juillet"="7",
      "aout"="8",
      "septembre"="9",
      "octobre"="10",
      "novembre"="11",
      "décembre"="12")%>%as.numeric



if(frequence_bdc=="mensuelle"){prog_annuelle$num_bdc_tmp<-prog_annuelle$mois1}
if(frequence_bdc=="bimestrielle"){
  prog_annuelle$num_bdc_tmp<-cut(prog_annuelle$mois1,
                                breaks = seq(0,12, by=2),
                                labels=as.character(seq(1,6)))
  }
if(frequence_bdc=="trimestrielle"){
      prog_annuelle$num_bdc_tmp<-cut(prog_annuelle$mois1,
                                    breaks = seq(0,12, by=3),
                                    labels=as.character(seq(1,4)))
    }

if(frequence_bdc=="semestrielle"){
      prog_annuelle$num_bdc_tmp<-cut(prog_annuelle$mois1,
                                    breaks = seq(0,12, by=2),
                                    labels=as.character(seq(1,2)))
    }
if(frequence_bdc=="annuelle"){
      prog_annuelle$num_bdc_tmp<-"1"
}

prog_annuelle$num_bdc_tmp<-as.numeric(prog_annuelle$num_bdc_tmp)

    # calcul du numéro d'ordre
tmp<-prog_annuelle%>%
  dplyr::select(pga_per_nom,
                cal_rattachement_bdc,
                num_bdc_tmp)%>%
  unique()%>%
  dplyr::arrange(num_bdc_tmp)%>%
  dplyr::group_by(pga_per_nom, cal_rattachement_bdc)%>%
  dplyr::mutate(num_bdc=rank(num_bdc_tmp))%>%
  dplyr::ungroup()



prog_annuelle<-dplyr::left_join(prog_annuelle, tmp, by=c("pga_per_nom", "num_bdc_tmp", "cal_rattachement_bdc"))


    prog_annuelle$bco_refcommande <-
      paste0(
        prefixe,
        "_",
        prog_annuelle$pga_per_nom,
        "_",
        prog_annuelle$cal_rattachement_bdc,
        "_",
        prog_annuelle$num_bdc
      )

##### enregistrement des bons de commandes #####
# mise en forme du bon de commande
  bons_commande<-prog_annuelle%>%
      dplyr::select(pga_per_nom,
                    bco_refcommande)%>%
      unique()

bons_commande$bco_stp_nom<-"1-projet"
bons_commande$bco_mar_id<-mar_id

bons_commande<-dplyr::rename(
  bons_commande,
  "bco_per_nom"="pga_per_nom"
  )

# Insertion dans la bdd
func_enregistre_dataframe_bdd(
  dataframe_a_enr = bons_commande,
  table_destination = "t_boncommande_bco",
  schema_destination = "sqe",
  connexion = connexion
)

# calcul du nb de prestation par bdc

tmp<-prog_annuelle%>%
  dplyr::group_by(bco_refcommande, cal_prs_id)%>%
  dplyr::count(name="bcq_nbprestacom")%>%
  dplyr::ungroup()

# récupération de la table des bdc
conn <-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

liste_bdc <- pool::dbGetQuery(
  conn,
  paste0(
    "SELECT bco_id, bco_refcommande FROM sqe.t_boncommande_bco WHERE bco_mar_id=",
    mar_id,
    ";"
  )
)

tmp<-dplyr::left_join(tmp, liste_bdc, by="bco_refcommande")
tmp<-tmp%>%dplyr::select(-bco_refcommande)


tmp<-dplyr::rename(
  tmp,
  "bcq_bco_id"="bco_id",
  "bcq_prs_id" = "cal_prs_id"
)

# Insertion dans la bdd
func_enregistre_dataframe_bdd(
  dataframe_a_enr = tmp,
  table_destination = "t_boncommande_quantitatif_bcq",
  schema_destination = "sqe",
  connexion = connexion
)

##### Enregistrement de la prog annuelle #####
prog_annuelle<-dplyr::left_join(prog_annuelle, liste_bdc, by="bco_refcommande")
data_a_enregistrer<-prog_annuelle%>%dplyr::select(bco_id,cal_prs_id,cal_date,pga_stm_cdstationmesureinterne)
names(data_a_enregistrer)<-c("bcp_bco_id",
                             "bcp_prs_id",
                             "bcp_dateinterv",
                             "bcp_stm_cdstationmesureinterne")


# Insertion dans la bdd
func_enregistre_dataframe_bdd(
  dataframe_a_enr = data_a_enregistrer,
  table_destination = "t_boncommande_pgm_bcp",
  schema_destination = "sqe",
  connexion = connexion
)

##### création d'une table commande annuelle qui liste les r\u00e9sultats attendus annuellement (et leur attribue une r\u00e9f\u00e9rence d'analyse) #####
prog_annuelle$res_codeprel<-paste0(prog_annuelle$bco_id,"*",prog_annuelle$cal_date,"*",prog_annuelle$pga_stm_cdstationmesureinterne)
tmp<-prog_annuelle%>%dplyr::select(pga_stm_cdstationmesureinterne,
                                   )

names(prog_annuelle)


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
