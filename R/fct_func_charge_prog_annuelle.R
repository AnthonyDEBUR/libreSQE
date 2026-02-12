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
func_charge_prog_annuelle <-
  function(fichier_prog, connexion, mar_id, annee,
           frequence_bdc = "mensuelle", prefixe = "",
           programmes_sans_dates = c("Transport Vilaine Est","Transport Vilaine Ouest",
                                     "Transport Vilaine aval","Transport SMGBO","Transport EPB"),
           strict_referentiel = TRUE) {

    if (!frequence_bdc %in% c("mensuelle","bimestrielle","trimestrielle","semestrielle","annuelle")) {
      stop("frequence_bdc doit être parmi mensuelle, bimestrielle, trimestrielle, semestrielle, annuelle")
    }
    if (!inherits(prefixe, "character")) stop("prefixe doit être de classe character")

    # Connexion unique + helpers
    conn <- pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn), add = TRUE)
    sql <- function(...) glue::glue_sql(..., .con = conn)

    # Helper : réparer silencieusement les noms de colonnes (évite “New names: ...”)
    repair_names_quiet <- function(df) {
      if (requireNamespace("vctrs", quietly = TRUE)) {
        names(df) <- vctrs::vec_as_names(names(df), repair = "unique", quiet = TRUE)
      }
      df
    }

    # ======================
    # 1) Lecture calendrier
    # ======================
    func_lit_le_fichier(fichier_prog = fichier_prog, "calendrier")
    calendrier <- repair_names_quiet(calendrier)

    calendrier_long <- tidyr::pivot_longer(
      calendrier,
      cols = c(janvier, fevrier, mars, avril, mai, juin, juillet, aout, septembre, octobre, novembre, decembre),
      names_to = "mois", values_to = "nb"
    ) %>% dplyr::filter(!is.na(nb) & nb > 0)

    cal_sans_dates <- calendrier_long %>% dplyr::filter(programme %in% programmes_sans_dates)
    cal_avec_dates <- calendrier_long %>% dplyr::filter(!programme %in% programmes_sans_dates)

    # Prestations avec dates : nb ENTIER 1..31
    if (nrow(cal_avec_dates) > 0) {
      non_entiers <- which(cal_avec_dates$nb %% 1 != 0)
      hors_plage  <- which(!(cal_avec_dates$nb %in% 1:31))
      if (length(non_entiers) || length(hors_plage)) {
        stop("Le nombre de prélèvements mensuels doit être un entier entre 1 et 31 (prestations avec dates).")
      }
    }

    # Numéro de mois
    mois_numeric <- data.frame(
      mois = c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre"),
      num_mois = 1:12
    )
    cal_sans_dates <- dplyr::left_join(cal_sans_dates, mois_numeric, by = "mois")
    cal_avec_dates <- dplyr::left_join(cal_avec_dates, mois_numeric, by = "mois")

    # Générer les dates pour les prestations avec dates
    if (nrow(cal_avec_dates) > 0) {
      ecart_2prel <- function(n) if (n == 1) 1 else seq(1, floor(31/n)*n, by = floor(31/n))
      lst_days <- lapply(cal_avec_dates$nb, ecart_2prel)
      cal_out <- NULL
      for (i in seq_len(nrow(cal_avec_dates))) {
        tmp <- data.frame(cal_avec_dates[i,], jour = lst_days[[i]])
        cal_out <- if (is.null(cal_out)) tmp else rbind(cal_out, tmp)
      }
      cal_avec_dates <- cal_out
      cal_avec_dates$cal_date <- as.Date(paste0(annee, "-", cal_avec_dates$num_mois, "-", cal_avec_dates$jour))
      cal_avec_dates <- cal_avec_dates %>% dplyr::select(-nb, -num_mois, -jour)
    }

    # Ajouter ref année/marché + renommer colonnes
    if (nrow(cal_sans_dates) > 0) {
      cal_sans_dates$cal_refannee <- annee
      cal_sans_dates$cal_mar_id   <- mar_id
      cal_sans_dates <- dplyr::rename(cal_sans_dates,
                                      "cal_typestation"      = "type_station",
                                      "cal_rattachement_bdc" = "rattachement_bdc")
    }
    if (nrow(cal_avec_dates) > 0) {
      cal_avec_dates$cal_refannee <- annee
      cal_avec_dates$cal_mar_id   <- mar_id
      cal_avec_dates <- dplyr::rename(cal_avec_dates,
                                      "cal_typestation"      = "type_station",
                                      "cal_rattachement_bdc" = "rattachement_bdc")
    }

    # Prestations du marché
    prestations <- DBI::dbGetQuery(conn, sql(
      "SELECT prs_id, prs_label_prestation FROM {`'sqe'`}.t_prestation_prs WHERE prs_mar_id = {mar_id};"
    ))
    if (nrow(cal_avec_dates) > 0) {
      cal_avec_dates <- dplyr::left_join(cal_avec_dates, prestations,
                                         by = c("programme" = "prs_label_prestation"))
      if (any(is.na(cal_avec_dates$prs_id))) {
        stop(paste0("Calendrier avec dates : programme(s) inconnus -> ",
                    paste(unique(cal_avec_dates$programme[is.na(cal_avec_dates$prs_id)]), collapse=" - ")))
      }
      cal_avec_dates <- dplyr::rename(cal_avec_dates, "cal_prs_id" = "prs_id")
    }
    if (nrow(cal_sans_dates) > 0) {
      cal_sans_dates <- dplyr::left_join(cal_sans_dates, prestations,
                                         by = c("programme" = "prs_label_prestation"))
      if (any(is.na(cal_sans_dates$prs_id))) {
        stop(paste0("Calendrier sans dates : programme(s) inconnus -> ",
                    paste(unique(cal_sans_dates$programme[is.na(cal_sans_dates$prs_id)]), collapse=" - ")))
      }
      cal_sans_dates <- dplyr::rename(cal_sans_dates, "cal_prs_id" = "prs_id")
    }

    # Enregistrement du calendrier (avec dates) -> t_calendrierprog_cal
    if (nrow(cal_avec_dates) > 0) {
      table_cal <- cal_avec_dates %>% dplyr::select(
        cal_refannee, cal_mar_id, cal_typestation, cal_date, cal_prs_id, cal_rattachement_bdc
      )
      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = table_cal,
        table_destination = "t_calendrierprog_cal",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    # ⚠️ AJOUT : “stub” calendrier pour les prestations SANS DATES
    #            (satisfait la vérification FK/trigger pga_cal_typestation ∈ calendrier)
    if (nrow(cal_sans_dates) > 0) {
      stub_cal <- cal_sans_dates %>%
        dplyr::group_by(cal_refannee, cal_mar_id, cal_typestation, cal_prs_id, cal_rattachement_bdc) %>%
        dplyr::summarise(cal_date = as.Date(paste0(annee, "-01-01")), .groups = "drop")
      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = stub_cal,
        table_destination = "t_calendrierprog_cal",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    # ===========================
    # 2) Lecture programme_annuel
    # ===========================
    func_lit_le_fichier(fichier_prog = fichier_prog, "programme_annuel")
    programme_annuel <- repair_names_quiet(programme_annuel)

    # Neutraliser les placeholders côté code SANDRE (aux surface) et ne pas l'utiliser pour FK
    placeholders <- c("sans_objet","sans objet","transport","TRANSPORT","", NA)
    programme_annuel$code_sandre_station <- trimws(programme_annuel$code_sandre_station)
    programme_annuel$code_sandre_station[
      programme_annuel$code_sandre_station %in% placeholders
    ] <- NA_character_

    # Fallback du code interne si manquant
    programme_annuel$code_interne_station <- trimws(programme_annuel$code_interne_station)
    need_fallback <- is.na(programme_annuel$code_interne_station) | programme_annuel$code_interne_station == ""
    programme_annuel$code_interne_station[need_fallback] <- programme_annuel$code_sandre_station[need_fallback]

    # Périmètres référentiel (FK per_nom)
    per_ref <- DBI::dbGetQuery(conn, sql("SELECT per_nom FROM {`'refer'`}.tr_perimetre_per;"))
    per_file <- unique(programme_annuel$perimetre_facturation %>% trimws())
    per_missing <- setdiff(per_file, per_ref$per_nom)
    if (strict_referentiel && length(per_missing)) {
      stop(paste0("Périmètres inexistants (refer.tr_perimetre_per) -> ",
                  paste(per_missing, collapse=" ; ")))
    }

    # Stations internes référentiel (FK stm_cdstationmesureinterne)
    sta_ref <- DBI::dbGetQuery(conn, sql(
      "SELECT stm_cdstationmesureinterne FROM {`'refer'`}.tr_stationmesure_stm;"
    ))
    sta_file <- unique(programme_annuel$code_interne_station[!is.na(programme_annuel$code_interne_station)])
    sta_missing <- setdiff(sta_file, sta_ref$stm_cdstationmesureinterne)
    if (strict_referentiel && length(sta_missing)) {
      stop(paste0(
        "Stations INTERNES inexistantes (refer.tr_stationmesure_stm.stm_cdstationmesureinterne) -> ",
        paste(sta_missing, collapse=" ; "),
        "\n➡️ Crée ces stations internes dans le référentiel ou corrige les libellés avant l’import."
      ))
    }

    # Cohérence type_station programme_annuel vs calendrier (avec ou sans dates)
    types_cal <- c(cal_avec_dates$cal_typestation, cal_sans_dates$cal_typestation) %>% unique()
    types_file <- unique(programme_annuel$type_station)
    manq_types <- setdiff(types_file, types_cal)
    if (length(manq_types)) {
      stop(paste0("type_station du programme_annuel absent du calendrier -> ",
                  paste(manq_types, collapse=" ; ")))
    }

    # Renommage & sélection : FK sur le code interne
    programme_annuel$pga_mar_id       <- mar_id
    programme_annuel$pga_cal_refannee <- annee
    programme_annuel <- dplyr::rename(
      programme_annuel,
      "pga_per_nom"                       = "perimetre_facturation",
      "pga_cal_typestation"               = "type_station",
      "pga_stm_cdstationmesureauxsurface" = "code_sandre_station",
      "pga_stm_cdstationmesureinterne"    = "code_interne_station"
    ) %>% dplyr::mutate(
      # colonne aux surface neutralisée
      pga_stm_cdstationmesureauxsurface = NA_character_
    ) %>% dplyr::select(
      pga_cal_refannee, pga_mar_id, pga_per_nom, pga_cal_typestation,
      pga_stm_cdstationmesureauxsurface,   # NA
      pga_stm_cdstationmesureinterne       # code interne -> FK
    )

    # Insertion programme annuel
    func_enregistre_dataframe_bdd(
      dataframe_a_enr   = programme_annuel,
      table_destination = "t_progannuelle_pga",
      schema_destination = "sqe",
      connexion = connexion
    )

    # =============================
    # 3) Préparation des BDC / BCQ
    # =============================
    prog_avec <- dplyr::left_join(
      programme_annuel, cal_avec_dates,
      by = c("pga_cal_typestation" = "cal_typestation"),
      multiple = "all", relationship = "many-to-many"
    )
    prog_sans <- dplyr::left_join(
      programme_annuel, cal_sans_dates,
      by = c("pga_cal_typestation" = "cal_typestation"),
      multiple = "all", relationship = "many-to-many"
    )

    recode_mois <- function(x) {
      as.numeric(dplyr::recode_factor(as.factor(x),
                                      "janvier"="1","fevrier"="2","mars"="3","avril"="4","mai"="5","juin"="6",
                                      "juillet"="7","aout"="8","septembre"="9","octobre"="10","novembre"="11","decembre"="12"))
    }
    if (nrow(prog_avec)) prog_avec$mois1 <- recode_mois(prog_avec$mois)
    if (nrow(prog_sans)) prog_sans$mois1 <- recode_mois(prog_sans$mois)

    set_num_bdc_tmp <- function(df) {
      if (nrow(df) == 0) return(df)
      if (frequence_bdc == "mensuelle")     df$num_bdc_tmp <- df$mois1
      if (frequence_bdc == "bimestrielle")  df$num_bdc_tmp <- cut(df$mois1, breaks = seq(0,12,by=2), labels = as.character(seq(1,6))) %>% as.numeric()
      if (frequence_bdc == "trimestrielle") df$num_bdc_tmp <- cut(df$mois1, breaks = seq(0,12,by=3), labels = as.character(seq(1,4))) %>% as.numeric()
      if (frequence_bdc == "semestrielle")  df$num_bdc_tmp <- cut(df$mois1, breaks = seq(0,12,by=6), labels = as.character(seq(1,2))) %>% as.numeric()
      if (frequence_bdc == "annuelle")      df$num_bdc_tmp <- 1
      df
    }
    prog_avec <- set_num_bdc_tmp(prog_avec)
    prog_sans <- set_num_bdc_tmp(prog_sans)

    calc_num_bdc <- function(df) {
      if (nrow(df) == 0) return(df)
      tmp <- df %>% dplyr::select(pga_per_nom, cal_rattachement_bdc, num_bdc_tmp) %>%
        unique() %>%
        dplyr::arrange(num_bdc_tmp) %>%
        dplyr::group_by(pga_per_nom, cal_rattachement_bdc) %>%
        dplyr::mutate(num_bdc = rank(num_bdc_tmp)) %>%
        dplyr::ungroup()
      dplyr::left_join(df, tmp, by = c("pga_per_nom","cal_rattachement_bdc","num_bdc_tmp"))
    }
    prog_avec <- calc_num_bdc(prog_avec)
    prog_sans <- calc_num_bdc(prog_sans)

    make_ref <- function(df) {
      if (nrow(df) == 0) return(df)
      df$bco_refcommande <- paste0(prefixe, "_", df$pga_per_nom, "_", df$cal_rattachement_bdc, "_", df$num_bdc)
      df
    }
    prog_avec <- make_ref(prog_avec)
    prog_sans <- make_ref(prog_sans)

    # BDC -> t_boncommande_bco
    bco <- dplyr::bind_rows(
      prog_avec %>% dplyr::select(pga_per_nom, bco_refcommande),
      prog_sans %>% dplyr::select(pga_per_nom, bco_refcommande)
    ) %>% unique()
    if (nrow(bco)) {
      bco$bco_stp_nom <- "1-projet"
      bco$bco_mar_id  <- mar_id
      bco <- dplyr::rename(bco, "bco_per_nom" = "pga_per_nom")
      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = bco,
        table_destination = "t_boncommande_bco",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    # Relecture bco_id
    liste_bdc <- DBI::dbGetQuery(conn, sql(
      "SELECT bco_id, bco_refcommande FROM {`'sqe'`}.t_boncommande_bco WHERE bco_mar_id = {mar_id};"
    ))
    ref_all <- unique(c(prog_avec$bco_refcommande, prog_sans$bco_refcommande))
    ref_not_found <- setdiff(ref_all, liste_bdc$bco_refcommande)
    if (length(ref_not_found)) {
      stop(paste0("BDC non retrouvés après insertion -> ", paste(ref_not_found, collapse=" ; ")))
    }

    # BCQ (quantitatif)
    bcq_avec <- NULL; bcq_sans <- NULL
    if (nrow(prog_avec)) {
      bcq_avec <- prog_avec %>%
        dplyr::group_by(bco_refcommande, cal_prs_id) %>%
        dplyr::count(name="bcq_nbprestacom") %>%
        dplyr::ungroup() %>%
        dplyr::left_join(liste_bdc, by="bco_refcommande") %>%
        dplyr::select(-bco_refcommande) %>%
        dplyr::rename("bcq_bco_id"="bco_id","bcq_prs_id"="cal_prs_id")
    }
    if (nrow(prog_sans)) {
      bcq_sans <- prog_sans %>%
        dplyr::group_by(bco_refcommande, cal_prs_id) %>%
        dplyr::summarise(bcq_nbprestacom = sum(nb), .groups = "drop") %>%
        dplyr::left_join(liste_bdc, by="bco_refcommande") %>%
        dplyr::select(-bco_refcommande) %>%
        dplyr::rename("bcq_bco_id"="bco_id","bcq_prs_id"="cal_prs_id")
    }
    bcq_all <- dplyr::bind_rows(bcq_avec, bcq_sans)
    if (nrow(bcq_all)) {
      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = bcq_all,
        table_destination = "t_boncommande_quantitatif_bcq",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    # BCP (plan daté) — uniquement "avec dates"
    if (nrow(prog_avec)) {
      prog_avec <- dplyr::left_join(prog_avec, liste_bdc, by = "bco_refcommande")
      if (any(is.na(prog_avec$bco_id))) {
        bad <- unique(prog_avec$bco_refcommande[is.na(prog_avec$bco_id)])
        stop(paste0("BCP : bco_id manquant pour refcommande -> ", paste(bad, collapse=" ; ")))
      }
      data_bcp <- prog_avec %>% dplyr::select(
        bco_id, cal_prs_id, cal_date, pga_stm_cdstationmesureinterne
      )
      names(data_bcp) <- c("bcp_bco_id","bcp_prs_id","bcp_dateinterv","bcp_stm_cdstationmesureinterne")

      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = data_bcp,
        table_destination = "t_boncommande_pgm_bcp",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    # REA (résultats attendus) — uniquement "avec dates"
    if (nrow(prog_avec)) {
      prog_avec$res_codeprel <- paste0(prog_avec$bco_id, "*", prog_avec$cal_date, "*", prog_avec$pga_stm_cdstationmesureinterne)
      tmp_rea <- prog_avec %>% dplyr::select(
        pga_stm_cdstationmesureinterne, res_codeprel, cal_date, bco_id, cal_prs_id
      )
      names(tmp_rea) <- c("res_stm_cdstationmesureinterne","res_codeprel","rea_dateprel_prev","res_bco_id","cal_prs_id")

      if (any(is.na(tmp_rea$res_bco_id))) {
        bad <- unique(prog_avec$bco_refcommande[is.na(tmp_rea$res_bco_id)])
        stop(paste0("REA : res_bco_id manquant (join BDC) -> ", paste(bad, collapse=" ; ")))
      }

      # Paramètres types
      ppt <- DBI::dbGetQuery(conn, sql(
        "SELECT ppt_prs_id, ppt_par_cdparametre, ppt_fra_codefraction, ppt_uni_codesandreunite,
                ppt_met_codesandremethode, ppt_pre_id, ppt_analyseinsitu, ppt_limitedetec,
                ppt_limitequantif, ppt_incertitude, ppt_accreditation
         FROM {`'sqe'`}.t_parametreprogrammetype_ppt WHERE ppt_mar_id = {mar_id};"
      ))
      tmp_rea <- dplyr::left_join(tmp_rea, ppt, by = c("cal_prs_id" = "ppt_prs_id"),
                                  multiple="all", relationship="many-to-many")

      # Prestataires
      prest <- DBI::dbGetQuery(conn, sql(
        "SELECT pre_id, pre_siret FROM {`'refer'`}.tr_prestataire_pre;"
      ))
      tmp_rea <- dplyr::left_join(tmp_rea, prest, by = c("ppt_pre_id" = "pre_id"))

      # Renommages
      tmp_rea <- dplyr::rename(
        tmp_rea,
        "rea_par_cdparametre"    = "ppt_par_cdparametre",
        "rea_cdfractionanalysee" = "ppt_fra_codefraction",
        "rea_cdunitemesure"      = "ppt_uni_codesandreunite",
        "rea_cdmethode"          = "ppt_met_codesandremethode",
        "rea_cdproducteur"       = "pre_siret",
        "rea_cdaccreanaprev"     = "ppt_accreditation",
        "rea_incertitudeprev"    = "ppt_incertitude",
        "rea_lqprev"             = "ppt_limitequantif",
        "rea_ldprev"             = "ppt_limitedetec"
      )
      tmp_rea$rea_cdinsituana <- ifelse(tmp_rea$ppt_analyseinsitu == "TRUE", "1", "2")
      tmp_rea <- tmp_rea %>% dplyr::select(-cal_prs_id, -ppt_pre_id, -ppt_analyseinsitu)

      func_enregistre_dataframe_bdd(
        dataframe_a_enr   = tmp_rea,
        table_destination = "t_resultatanalyse_rea",
        schema_destination = "sqe",
        connexion = connexion
      )
    }

    invisible(TRUE)
  }
