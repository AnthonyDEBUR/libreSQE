#' func_genere_bdc_xlsx
#'
#' @description Génère un bon de commande au format xlsx
#'
#' @param bdc_id : identifiant du bon de commande à éditer
#' @param pre_id : identifiant du (des) prestataire(s)
#' @param connexion : connexion à la BDD
#' @param bdc_type : ficier xlsx, trame du bon de commande à générer
#' @param out_file_name : chemin d'enregistrement du fichier
#'
#' @return Un fichier xlsx
#'
#' @export
func_genere_bdc_xlsx <-
  function(bdc_id,
           pre_id,
           connexion,
           bdc_type,
           out_file_name)  {
    pru_datedebut <- pru_datefin <- pru_valeur <-
      prs_label_prestation <- NULL

    if (!(("numeric" %in% class(bdc_id)) |
          ("integer" %in% class(bdc_id)))) {
      stop("func_genere_bdc_xlsx le parametre bdc_id doit être de classe numeric")
    }
    if (!(("numeric" %in% class(pre_id)) |
          ("integer" %in% class(pre_id)))) {
      stop("func_genere_bdc_xlsx le parametre pre_id doit être de classe numeric")
    }
    if (!file.exists(bdc_type)) {
      stop("func_genere_bdc_xlsx le parametre bdc_type ne renvoie pas vers un fichier")
    }


    # on récupère le contenu du BDC - page 1
    conn <-  pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn))
    pool::dbBegin(conn) # commence transaction SQL

    bdc_quantitatif <- pool::dbGetQuery(
      conn,
      paste0(
        "SELECT * FROM sqe.view_bdc_quantif
                                      WHERE bco_id=",
        bdc_id,
        " AND pre_id IN (",
        paste(pre_id, collapse = ","),
        ");"
      )
    )

    pool::dbRollback(conn)

    # on récupère le tableau de prog du BDC - page 2
    conn <-  pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn))
    pool::dbBegin(conn) # commence transaction SQL

    bdc_quantitatif_station <- pool::dbGetQuery(
      conn,
      paste0(
        "SELECT * FROM sqe.view_bdc_quantif_par_staq
                                      WHERE bco_id=",
        bdc_id,
        " AND pre_id IN (",
        paste(pre_id, collapse = ","),
        ");"
      )
    )

    pool::dbRollback(conn)

    # page 1 : on ne conserve que les données qui correspondent à la période prélevée sur le bdc

    date_mini <- min(bdc_quantitatif_station$bcp_dateinterv, na.rm = TRUE)
    date_maxi <- max(bdc_quantitatif_station$bcp_dateinterv, na.rm = TRUE)
    bdc_quantitatif <-
      bdc_quantitatif %>% subset(pru_datedebut <= date_mini &
                                   pru_datefin >= date_maxi)

    # on teste s'il existe des doublons dans les prix unitaires (par ex si pb de plusieurs prix unitaires possibles pour une même prestation)
    if (nrow(bdc_quantitatif) != nrow(bdc_quantitatif %>% dplyr::select(-pru_datedebut,-pru_datefin,-pru_valeur))) {
      stop(
        paste0(
          "Il existe des ambiguités sur les prix unitaire dans le programme / bon de commande ",
          bdc_quantitatif$bco_refcommande[1],
          ". Vérifiez que le bon de commande ne couvre pas une période pendant laquelle les prix unitaires ont été révisés."
        )
      )
    }

    # Extraction des données d'intérêt

    id_reference_bdc <- bdc_quantitatif$bco_refcommande[1]
    id_marche <- bdc_quantitatif$mar_reference[1]
    id_titre_marche <- bdc_quantitatif$mar_nom_long[1]
    id_perimetre <- bdc_quantitatif$per_entite_gestionaire[1]
    id_projet <- bdc_quantitatif$bco_per_nom[1]
    bdc_quantitatif$totalHT <-
      bdc_quantitatif$pru_valeur * bdc_quantitatif$bcq_nbprestacom
    col_num_presta <-
      data.frame(col_num_presta = bdc_quantitatif$prs_idprestationdansbpu)
    col_nom_presta <-
      data.frame(col_nom_presta = bdc_quantitatif$prs_label_prestation)
    col_unite_presta <-
      data.frame(col_unite_presta = bdc_quantitatif$prm_unitedoeuvre)
    col_PU_HT <- data.frame(col_PU_HT = bdc_quantitatif$pru_valeur)
    col_qte <- data.frame(col_qte = bdc_quantitatif$bcq_nbprestacom)
    col_total_HT <- data.frame(col_total_HT = bdc_quantitatif$totalHT)
    id_totalHT <- sum(bdc_quantitatif$totalHT, na.rm = TRUE)
    id_TVA <- 0.2 * id_totalHT
    id_totalTTC <- id_totalHT + id_TVA
    mois_annee <-
      c(
        "Janvier",
        "Février",
        "Mars",
        "Avril",
        "Mai",
        'Juin',
        'Juillet',
        "Aout",
        "Septembre",
        "Octobre",
        "Novembre",
        "Décembre"
      )
    id_mois_debut <- mois_annee[format(date_mini, "%m") %>% as.numeric]
    id_mois_fin <- mois_annee[format(date_maxi, "%m") %>% as.numeric]

    # page 2 passage au format large
    bdc_quantitatif_station$qte <- 1
    id_nb_stations <-
      length(bdc_quantitatif_station[bdc_quantitatif_station$bcp_stm_cdstationmesureinterne !=
                                       "sans_objet", ]$bcp_stm_cdstationmesureinterne %>% unique())


    bdc_quantitatif_station <- bdc_quantitatif_station %>%
      tidyr::pivot_wider(
        id_cols = c(
          "bco_per_nom",
          "bcp_stm_cdstationmesureinterne",
          "stm_lbstationmesureeauxsurface",
          "bcp_dateinterv"
        ),
        names_from = prs_label_prestation,
        values_from = "qte",
        values_fill = 0
      )


  # #  bdc_quantitatif_station[1:35,] %>%
  #     bdc_quantitatif_station[1:35,] %>%
  #     tidyr::pivot_wider(
  #       id_cols = c(
  #         "bco_per_nom",
  #         "bcp_stm_cdstationmesureinterne",
  #         "stm_lbstationmesureeauxsurface",
  #         "bcp_dateinterv"
  #       ),
  #       names_from = prs_label_prestation,
  #       values_from = "qte",
  #       values_fill = 0
  #     )


    bdc_quantitatif_station <- bdc_quantitatif_station %>%
      dplyr::rename(
        "Périmètre de\nfacturation" = "bco_per_nom",
        "Code station" = "bcp_stm_cdstationmesureinterne",
        "Nom station" = "stm_lbstationmesureeauxsurface",
        "Date intervention" = "bcp_dateinterv"
      )


    tab_prog_coin_gauche <- bdc_quantitatif_station


    #chargement du modele de fichier
    tryCatch(
      wb_trame <- openxlsx::loadWorkbook(bdc_type),
      error = function(e)
        e
    )

    liste_sheet <- openxlsx::getSheetNames(bdc_type)
    if (!"trame" %in% liste_sheet) {
      stop(
        paste0(
          "Fichier bon de commande type : ",
          bdc_type,
          " onglet trame (décrivant les champs à insérer) manquant."
        )
      )
    }

    liste_sheet <- liste_sheet[liste_sheet != "trame"]
    if (length(liste_sheet) == 0) {
      stop("Aucun onglet à générer dans le fichier bon de commande type : ",
           bdc_type)
    }
    trame <- openxlsx::read.xlsx(bdc_type, sheet = "trame")


    for (i in 1:nrow(trame))
    {
      if (substr(trame$Champs[i], 1, 2) == "id") {
        assign("valeur", eval(parse(text = trame$Champs[i])))
        openxlsx::writeData(
          wb_trame,
          sheet = trame$Onglet[i],
          x = valeur,
          xy = c(
            gsub("[[:digit:]]", "", trame$Emplacement[i]),
            gsub("[^0-9.-]", "", trame$Emplacement[i])
          )
        )

      }
      if (substr(trame$Champs[i], 1, 3) == "tab") {
        assign("valeur", eval(parse(text = trame$Champs[i])))
        openxlsx::writeData(
          wb_trame,
          sheet = trame$Onglet[i],
          x = valeur,
          xy = c(
            gsub("[[:digit:]]", "", trame$Emplacement[i]),
            gsub("[^0-9.-]", "", trame$Emplacement[i])
          )
        )
      }

      if (substr(trame$Champs[i], 1, 3) == "col") {
        assign("valeur", eval(parse(text = trame$Champs[i])))
        openxlsx::writeData(
          wb_trame,
          sheet = trame$Onglet[i],
          x = valeur,
          xy = c(
            gsub("[[:digit:]]", "", trame$Emplacement[i]),
            gsub("[^0-9.-]", "", trame$Emplacement[i])
          ),
          colNames = FALSE
        )

      }

    }
    openxlsx::activeSheet(wb_trame) <- liste_sheet[1]


    # setColWidths(wb_trame, liste_sheet[1], cols = 1:100, widths = "auto")
    # setColWidths(wb_trame, liste_sheet[2], cols = 1:100, widths = "auto")
    openxlsx::removeWorksheet(wb_trame, "trame")
    options("openxlsx.dateFormat" = "dd/mm/yyyy")

    openxlsx::saveWorkbook(wb_trame, file = out_file_name, overwrite = TRUE)

    return(
      c(
        id_titre_marche,
        id_reference_bdc,
        id_perimetre,
        id_projet,
        id_mois_debut,
        id_mois_fin,
        id_totalHT,
        id_totalTTC
      )
    )

  }
