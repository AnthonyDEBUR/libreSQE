
#' func_charge_marche
#'
#' @description
#' Charge un fichier Excel de marché (BPU, cout_run_analytiques, programmes_types)
#' dans la base LibreSQE en une transaction unique (rollback intégral en cas d'erreur).
#' Valide au préalable la présence des codes (paramètre, fraction, unité) dans les référentiels.
#'
#' @param fichier_prog Chemin du fichier .xlsx
#' @param connexion    pool::dbPool (connexion à la BDD)
#' @param mar_id       identifiant du marché (sqe.t_marche_mar.mar_id)
#' @return (invisible) TRUE si succès
#' @export
func_charge_marche <- function(fichier_prog, connexion, mar_id) {

  # --- NSE (sécurité d'évaluation) ---
  run_analytique <- code_methode_sandre <- siret_prestataire_analyse <- NULL
  pre_id <- run_nom <- run_met_code <- run_prestataire <- NULL
  prix_unitaire_ht <- prix_unitaire_HT <- NULL
  pru_valeur <- pru_datedebut <- pru_datefin <- NULL
  prs_mar_id <- prs_pre_id <- prs_nomprestation <- prs_label_prestation <- NULL
  prs_natureprestation <- prm_unitedoeuvre <- prs_idprestationdansbpu <- NULL

  # --- 0) Coercitions de type ---
  mar_id <- suppressWarnings(as.integer(mar_id))
  if (is.na(mar_id)) stop("mar_id non valide (doit être un entier).")

  as_date_safe <- function(x) {
    if (inherits(x, "Date")) return(x)
    xd <- suppressWarnings(as.Date(x))
    if (all(is.na(xd))) xd <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
    xd
  }
  as_numeric_safe <- function(x) suppressWarnings(as.numeric(x))

  # --- 1) Lecture des onglets Excel ---
  func_lit_le_fichier(fichier_prog = fichier_prog, "cout_run_analytiques")
  func_lit_le_fichier(fichier_prog = fichier_prog, onglet = "programmes_types")
  func_lit_le_fichier(fichier_prog = fichier_prog, "BPU")

  # --- 2) Connexion unique ---
  conn <- pool::poolCheckout(connexion)
  on.exit(pool::poolReturn(conn), add = TRUE)

  # Référentiels
  prestataires <- DBI::dbGetQuery(conn, "SELECT * FROM refer.tr_prestataire_pre;")
  ref_param    <- DBI::dbGetQuery(conn, "SELECT par_cdparametre, par_nomparametre FROM refer.tr_parametre_par;")
  ref_fraction <- DBI::dbGetQuery(conn, "SELECT fra_codefraction, fra_nomfraction FROM refer.tr_fraction_fra;")
  ref_unite    <- DBI::dbGetQuery(conn, "SELECT uni_codesandreunite, uni_symbole FROM refer.tr_uniteparametre_uni;")

  # --- 3) Contrôles & préparation R ---

  # 3.1 RUN specs (méthode + prestataire par run)
  spec_run_ana <- programmes_types %>%
    dplyr::select(run_analytique, code_methode_sandre, siret_prestataire_analyse) %>%
    dplyr::distinct() %>%
    dplyr::left_join(prestataires, by = c("siret_prestataire_analyse" = "pre_siret"))

  if (any(is.na(spec_run_ana$pre_id))) {
    stop(paste0(
      "Programmes types : SIRET inconnu dans LibreSQE pour : ",
      paste(spec_run_ana[is.na(spec_run_ana$pre_id), ]$siret_prestataire_analyse %>% unique(), collapse = " - ")
    ))
  }

  verif <- spec_run_ana %>% dplyr::group_by(run_analytique) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  if (any(verif$n > 1)) {
    stop(paste0(
      "Doublons RUN (plusieurs couples méthode/SIRET pour un même nom) : ",
      paste(verif$run_analytique[verif$n > 1], collapse = " - ")
    ))
  }
  rm(verif)

  # 3.2 Compléter cout_run_analytiques
  cra <- dplyr::left_join(cout_run_analytiques, spec_run_ana, by = "run_analytique")
  if (any(is.na(cra$code_methode_sandre))) {
    stop(paste0(
      "Méthode SANDRE manquante pour run : ",
      paste(cra[is.na(cra$code_methode_sandre), ]$run_analytique, collapse = " - ")
    ))
  }
  if (any(is.na(cra$siret_prestataire_analyse))) {
    stop(paste0(
      "SIRET prestataire d'analyse manquant pour run : ",
      paste(cra[is.na(cra$siret_prestataire_analyse), ]$run_analytique, collapse = " - ")
    ))
  }

  # 3.3 Validation référentiels (codes)
  invalid_param <- programmes_types %>%
    dplyr::mutate(code_parametre_sandre = trimws(code_parametre_sandre)) %>%
    dplyr::filter(is.na(code_parametre_sandre) |
                    code_parametre_sandre == "" |
                    tolower(code_parametre_sandre) == "à codifier sandre" |
                    !(code_parametre_sandre %in% ref_param$par_cdparametre))
  if (nrow(invalid_param) > 0) {
    msg <- paste0(
      "Codes PARAMETRE invalides/absents (refer.tr_parametre_par).\n",
      paste(
        apply(invalid_param[, c("programme", "nom_parametre_sandre", "code_parametre_sandre")], 1,
              function(r) sprintf("programme='%s' | paramètre='%s' | code='%s'", r[1], r[2], r[3])),
        collapse = "\n"
      )
    )
    stop(msg)
  }

  invalid_fraction <- programmes_types %>%
    dplyr::filter(!(code_fraction_sandre %in% ref_fraction$fra_codefraction))
  if (nrow(invalid_fraction) > 0) {
    msg <- paste0(
      "Codes FRACTION absents (refer.tr_fraction_fra).\n",
      paste(
        apply(invalid_fraction[, c("programme", "nom_fraction_sandre", "code_fraction_sandre")], 1,
              function(r) sprintf("programme='%s' | fraction='%s' | code='%s'", r[1], r[2], r[3])),
        collapse = "\n"
      )
    )
    stop(msg)
  }

  invalid_unite <- programmes_types %>%
    dplyr::filter(!(code_unite_sandre %in% ref_unite$uni_codesandreunite))
  if (nrow(invalid_unite) > 0) {
    msg <- paste0(
      "Codes UNITE absents (refer.tr_unite_uni).\n",
      paste(
        apply(invalid_unite[, c("programme", "nom_parametre_sandre", "code_unite_sandre")], 1,
              function(r) sprintf("programme='%s' | paramètre='%s' | code='%s'", r[1], r[2], r[3])),
        collapse = "\n"
      )
    )
    stop(msg)
  }

  # 3.4 RUN + PRR (normalisation)
  prix_col_cra <- if ("prix_unitaire_HT" %in% names(cra)) "prix_unitaire_HT" else "prix_unitaire_ht"
  cra <- cra %>%
    dplyr::rename(
      run_nom         = run_analytique,
      run_met_code    = code_methode_sandre,
      run_prestataire = pre_id,
      pru_datedebut   = date_debut_validite_prix,
      pru_datefin     = date_fin_validite_prix
    ) %>%
    dplyr::mutate(
      prr_mar_id    = as.integer(mar_id),
      pru_valeur    = as_numeric_safe(.data[[prix_col_cra]]),
      pru_datedebut = as_date_safe(pru_datedebut),
      pru_datefin   = as_date_safe(pru_datefin)
    ) %>%
    dplyr::select(run_nom, run_met_code, run_prestataire,
                  prr_mar_id, pru_datedebut, pru_datefin, pru_valeur) %>%
    dplyr::distinct()

  # 3.5 BPU -> PRS/PRP
  bpu_prix <- if ("prix_unitaire_HT" %in% names(BPU)) BPU$prix_unitaire_HT else BPU$prix_unitaire_ht
  bpu_prix <- as_numeric_safe(bpu_prix)

  BPU <- BPU %>%
    dplyr::mutate(
      prs_mar_id               = as.integer(mar_id),
      date_debut_validite_prix = as_date_safe(date_debut_validite_prix),
      date_fin_validite_prix   = as_date_safe(date_fin_validite_prix)
    ) %>%
    dplyr::left_join(prestataires, by = c("siret_prestataire" = "pre_siret"))

  if (any(is.na(BPU$pre_id))) {
    tmp <- BPU[is.na(BPU$pre_id), c("siret_prestataire", "nom_prestataire")] %>% unique()
    stop(paste0("BPU : SIRET inconnu pour : ", paste(tmp[, 1], " - ", tmp[, 2], collapse = "; ")))
  }

  prs_df <- dplyr::tibble(
    prs_mar_id              = BPU$prs_mar_id,
    prs_pre_id              = BPU$pre_id,
    prs_nomprestation       = BPU$nom_complet_prestation,
    prs_label_prestation    = BPU$label_prestation,
    prs_natureprestation    = BPU$nature_prestation,
    prm_unitedoeuvre        = BPU$unite_facturation,
    prs_idprestationdansbpu = BPU$id_prestation
  ) %>% dplyr::distinct()

  # --- 4) Transaction ENGLOBANTE ---
  DBI::dbWithTransaction(conn, {

    # 4.1 RUNS + PRR
    schema_run <- "sqe"; table_run <- "t_runanalytique_run"
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s_temp;", table_run))

    runs_to_insert <- cra %>%
      dplyr::select(run_nom, run_met_code, run_prestataire) %>%
      dplyr::distinct()

    DBI::dbWriteTable(conn, DBI::Id(schema = "temp", table = paste0(table_run, "_temp")), runs_to_insert)

    retour_run <- DBI::dbGetQuery(
      conn,
      sprintf(
        paste0(
          "INSERT INTO %s.%s (run_nom, run_met_code, run_prestataire) ",
          "SELECT run_nom, run_met_code, run_prestataire FROM temp.%s_temp ",
          "RETURNING run_id, run_nom;"
        ),
        schema_run, table_run, table_run
      )
    )

    prr_df <- cra %>%
      dplyr::left_join(retour_run, by = "run_nom") %>%
      dplyr::rename(prr_run_id = run_id) %>%
      dplyr::select(prr_mar_id, prr_run_id, pru_datedebut, pru_datefin, pru_valeur)

    schema_prr <- "sqe"; table_prr <- "t_prixunitairerunanalytique_prr"
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s_temp;", table_prr))
    DBI::dbWriteTable(conn, DBI::Id(schema = "temp", table = paste0(table_prr, "_temp")), prr_df)

    DBI::dbExecute(
      conn,
      sprintf(
        paste0(
          "INSERT INTO %s.%s (prr_mar_id, prr_run_id, pru_datedebut, pru_datefin, pru_valeur) ",
          "SELECT CAST(prr_mar_id AS INTEGER), prr_run_id, ",
          "       CAST(pru_datedebut AS DATE), CAST(pru_datefin AS DATE), CAST(pru_valeur AS NUMERIC) ",
          "FROM temp.%s_temp;"
        ),
        schema_prr, table_prr, table_prr
      )
    )

    # 4.2 PRS + PRP
    schema_prs <- "sqe"; table_prs <- "t_prestation_prs"
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s_temp;", table_prs))
    DBI::dbWriteTable(conn, DBI::Id(schema = "temp", table = paste0(table_prs, "_temp")), prs_df)

    DBI::dbExecute(
      conn,
      sprintf(
        paste0(
          "INSERT INTO %s.%s ",
          "(prs_mar_id, prs_pre_id, prs_nomprestation, prs_label_prestation, ",
          " prs_natureprestation, prm_unitedoeuvre, prs_idprestationdansbpu) ",
          "SELECT CAST(prs_mar_id AS INTEGER), prs_pre_id, prs_nomprestation, prs_label_prestation, ",
          "       prs_natureprestation, prm_unitedoeuvre, prs_idprestationdansbpu ",
          "FROM temp.%s_temp;"
        ),
        schema_prs, table_prs, table_prs
      )
    )

    t_prestation_prs <- DBI::dbGetQuery(
      conn,
      sprintf("SELECT * FROM %s.%s WHERE prs_mar_id = %s;", schema_prs, table_prs, mar_id)
    )

    BPU2 <- BPU %>%
      dplyr::left_join(
        t_prestation_prs,
        by = c(
          "prs_mar_id"             = "prs_mar_id",
          "pre_id"                 = "prs_pre_id",
          "nom_complet_prestation" = "prs_nomprestation"
        )
      )

    prp_df <- dplyr::tibble(
      pru_datedebut = BPU2$date_debut_validite_prix,
      pru_datefin   = BPU2$date_fin_validite_prix,
      pru_valeur    = bpu_prix,
      prp_prs_id    = BPU2$prs_id
    ) %>% dplyr::distinct()

    schema_prp <- "sqe"; table_prp <- "t_prixunitaireprestation_prp"
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s_temp;", table_prp))
    DBI::dbWriteTable(conn, DBI::Id(schema = "temp", table = paste0(table_prp, "_temp")), prp_df)

    DBI::dbExecute(
      conn,
      sprintf(
        paste0(
          "INSERT INTO %s.%s (pru_datedebut, pru_datefin, pru_valeur, prp_prs_id) ",
          "SELECT CAST(pru_datedebut AS DATE), CAST(pru_datefin AS DATE), CAST(pru_valeur AS NUMERIC), prp_prs_id ",
          "FROM temp.%s_temp;"
        ),
        schema_prp, table_prp, table_prp
      )
    )

    # 4.3 PPT
    programmes_types2 <- programmes_types %>%
      dplyr::left_join(retour_run,        by = c("run_analytique" = "run_nom")) %>%
      dplyr::left_join(t_prestation_prs,  by = c("programme"      = "prs_label_prestation")) %>%
      dplyr::left_join(prestataires,      by = c("siret_prestataire_analyse" = "pre_siret"))

    if (any(is.na(programmes_types2$pre_id))) {
      tmp <- programmes_types2[
        is.na(programmes_types2$pre_id),
        c("siret_prestataire_analyse", "prestataire_analyse")
      ] %>% unique()
      stop(paste0(
        "Programmes types : SIRET inconnu pour : ",
        paste(tmp[, 1], " - ", tmp[, 2], collapse = "; ")
      ))
    }

    ppt_df <- dplyr::tibble(
      ppt_prs_id                = programmes_types2$prs_id,
      ppt_mar_id                = as.integer(mar_id),
      ppt_run_id                = programmes_types2$run_id,
      ppt_par_cdparametre       = programmes_types2$code_parametre_sandre,
      ppt_fra_codefraction      = programmes_types2$code_fraction_sandre,
      ppt_nomparametre          = programmes_types2$nom_parametre_sandre,
      ppt_uni_codesandreunite   = programmes_types2$code_unite_sandre,
      ppt_met_codesandremethode = programmes_types2$code_methode_sandre,
      ppt_pre_id                = programmes_types2$pre_id,
      ppt_analyseinsitu         = tolower(programmes_types2$analyse_in_situ) == "oui",
      ppt_limitedetec           = as_numeric_safe(programmes_types2$limite_detection_garantie),
      ppt_limitequantif         = as_numeric_safe(programmes_types2$limite_quantification_garantie),
      ppt_incertitude           = as_numeric_safe(programmes_types2$incertitude_garantie),
      ppt_accreditation         = tolower(programmes_types2$accreditation) == "oui",
      ppt_commentaireparametre  = programmes_types2$commentaires_parametre
    )

    schema_ppt <- "sqe"; table_ppt <- "t_parametreprogrammetype_ppt"
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s_temp;", table_ppt))
    DBI::dbWriteTable(conn, DBI::Id(schema = "temp", table = paste0(table_ppt, "_temp")), ppt_df)

    DBI::dbExecute(
      conn,
      sprintf(
        paste0(
          "INSERT INTO %s.%s (",
          "ppt_prs_id, ppt_mar_id, ppt_run_id, ",
          "ppt_par_cdparametre, ppt_fra_codefraction, ppt_nomparametre, ",
          "ppt_uni_codesandreunite, ppt_met_codesandremethode, ",
          "ppt_pre_id, ppt_analyseinsitu, ppt_limitedetec, ppt_limitequantif, ",
          "ppt_incertitude, ppt_accreditation, ppt_commentaireparametre",
          ") ",
          "SELECT ",
          "ppt_prs_id, CAST(ppt_mar_id AS INTEGER), ppt_run_id, ",
          "ppt_par_cdparametre, ppt_fra_codefraction, ppt_nomparametre, ",
          "ppt_uni_codesandreunite, ppt_met_codesandremethode, ",
          "ppt_pre_id, ppt_analyseinsitu, ",
          "CAST(ppt_limitedetec AS NUMERIC), CAST(ppt_limitequantif AS NUMERIC), CAST(ppt_incertitude AS NUMERIC), ",
          "ppt_accreditation, ppt_commentaireparametre ",
          "FROM temp.%s_temp;"
        ),
        schema_ppt, table_ppt, table_ppt
      )
    )

  }) # FIN dbWithTransaction (commit si OK)

  invisible(TRUE)
} # FIN func_charge_marche
