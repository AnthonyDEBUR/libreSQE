#' func_charge_prog_annuelle
#'
#'
func_charge_prog_annuelle <-
  function(fichier_prog,
           connexion,
           mar_id,
           annee,
           frequence_bdc = "mensuelle", prefixe = "",
           strict_referentiel = TRUE) {

    if (!frequence_bdc %in% c("mensuelle","bimestrielle","trimestrielle","semestrielle","annuelle")) {
      stop("frequence_bdc doit être parmi mensuelle, bimestrielle, trimestrielle, semestrielle, annuelle")
    }
    if (!inherits(prefixe, "character")) stop("prefixe doit être de classe character")

    # Connexion
    conn <- pool::poolCheckout(connexion)
    on.exit(pool::poolReturn(conn), add = TRUE)
    sql <- function(...) glue::glue_sql(..., .con = conn)

    # Helper noms propres
    repair_names_quiet <- function(df) {
      if (requireNamespace("vctrs", quietly = TRUE)) {
        names(df) <- vctrs::vec_as_names(names(df), repair = "unique", quiet = TRUE)
      }
      df
    }

    # ───────────────────────────────────────────────
    # 1) Lecture programme_annuel AVANT calendrier
    # ───────────────────────────────────────────────
    func_lit_le_fichier(fichier_prog, "programme_annuel")
    programme_annuel <- repair_names_quiet(programme_annuel)


    # Éléments FK référentiels
    per_ref <- DBI::dbGetQuery(conn, sql("SELECT per_nom FROM refer.tr_perimetre_per;"))
    per_file <- unique(trimws(programme_annuel$perimetre_facturation))
    per_missing <- setdiff(per_file, per_ref$per_nom)
    if (strict_referentiel && length(per_missing)) {
      stop("Périmètres inexistants : ", paste(per_missing, collapse=" ; "))
    }

    sta_ref <- DBI::dbGetQuery(conn, sql(
      "SELECT stm_cdstationmesureinterne FROM refer.tr_stationmesure_stm;"
    ))
    sta_file <- unique(programme_annuel$code_interne_station[!is.na(programme_annuel$code_interne_station)])
    sta_missing <- setdiff(sta_file, sta_ref$stm_cdstationmesureinterne)
    if (strict_referentiel && length(sta_missing)) {
      stop("Stations internes inexistantes : ", paste(sta_missing, collapse=" ; "))
    }

    # Renommage
    programme_annuel$pga_mar_id       <- mar_id
    programme_annuel$pga_cal_refannee <- annee
    programme_annuel <- programme_annuel %>%
      rename(
        pga_per_nom                    = perimetre_facturation,
        pga_cal_typestation            = type_station,
        pga_stm_cdstationmesureauxsurface = code_sandre_station,
        pga_stm_cdstationmesureinterne    = code_interne_station
      )   %>%
     # dplyr::mutate(pga_stm_cdstationmesureauxsurface = NA_character_) %>%
      dplyr::select(
        pga_cal_refannee, pga_mar_id, pga_per_nom, pga_cal_typestation,
        pga_stm_cdstationmesureauxsurface,
        pga_stm_cdstationmesureinterne
      )

    # ───────────────────────────────────────────────
    # 2) Lecture calendrier (APRÈS programme_annuel)
    # ───────────────────────────────────────────────
    func_lit_le_fichier(fichier_prog, "calendrier")
    calendrier <- repair_names_quiet(calendrier)

    calendrier_long <- tidyr::pivot_longer(
      calendrier,
      cols = c(janvier, fevrier, mars, avril, mai, juin, juillet, aout, septembre, octobre, novembre, decembre),
      names_to = "mois", values_to = "nb"
    ) %>% filter(!is.na(nb) & nb > 0)

    ### joindre le code interne station
    calendrier_long <- right_join(
      calendrier_long,
      programme_annuel %>%
        select(pga_cal_typestation, pga_stm_cdstationmesureinterne),
      by = c("type_station" = "pga_cal_typestation"),
      relationship = "many-to-many"
    ) %>%
      rename(code_interne_station = pga_stm_cdstationmesureinterne)

    # Séparer avec / sans dates selon la station (et non selon le programme)
    cal_sans_dates <- calendrier_long %>%
      filter(code_interne_station %in% c("sans_objet","sans objet","", NA))%>%
      distinct()

    cal_avec_dates <- calendrier_long %>%
      filter(!code_interne_station %in% c("sans_objet","sans objet","", NA)) %>%
      distinct()


    # Vérification nb 1..31
    if (nrow(cal_avec_dates) > 0) {
      if (any(cal_avec_dates$nb %% 1 != 0))
        stop("nb doit être entier")
      if (any(!(cal_avec_dates$nb %in% 1:31)))
        stop("nb doit être entre 1 et 31")
    }

    # Mois num
    mois_numeric <- data.frame(
      mois = c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre"),
      num_mois = 1:12
    )
    cal_sans_dates <- left_join(cal_sans_dates, mois_numeric, by = "mois")
    cal_avec_dates <- left_join(cal_avec_dates, mois_numeric, by = "mois")

    # Génération dates (TOUTES prestations sauf sans_objet)
    if (nrow(cal_avec_dates) > 0) {
      ecart_2prel <- function(n) if (n == 1) 1 else seq(1, floor(31/n)*n, by = floor(31/n))
      lst_days <- lapply(cal_avec_dates$nb, ecart_2prel)

      cal_out <- NULL
      for (i in seq_len(nrow(cal_avec_dates))) {
        tmp <- data.frame(cal_avec_dates[i,], jour = lst_days[[i]])
        cal_out <- if (is.null(cal_out)) tmp else rbind(cal_out, tmp)
      }
      cal_avec_dates <- cal_out
      cal_avec_dates$cal_date <- as.Date(
        paste0(annee, "-", cal_avec_dates$num_mois, "-", cal_avec_dates$jour)
      )
      cal_avec_dates <- cal_avec_dates %>%
        select(-nb, -num_mois, -jour)
    }

 # Ajout ref + renoms
    if (nrow(cal_sans_dates) > 0) {
      cal_sans_dates$cal_refannee <- annee
      cal_sans_dates$cal_mar_id   <- mar_id
      cal_sans_dates <- rename(cal_sans_dates,
                               cal_typestation = type_station,
                               cal_rattachement_bdc = rattachement_bdc)
    }
    if (nrow(cal_avec_dates) > 0) {
      cal_avec_dates$cal_refannee <- annee
      cal_avec_dates$cal_mar_id   <- mar_id
      cal_avec_dates <- rename(cal_avec_dates,
                               cal_typestation = type_station,
                               cal_rattachement_bdc = rattachement_bdc)
    }

    # Prestations du marché
    # Récupération des prestations du marché
    prestations <- DBI::dbGetQuery(conn, sql(
      "SELECT prs_id, prs_label_prestation
  FROM sqe.t_prestation_prs
  WHERE prs_mar_id = {mar_id};"
    ))

    # Programmes à ignorer car validés comme "sans analyses"
    prog_sans_analyses <- unique(trimws(
      readxl::read_xlsx(fichier_prog, sheet = "programmes_sans_analyses")$programme
    ))

    # Traitement des lignes avec dates
    if (nrow(cal_avec_dates) > 0) {

      # Jointure normale
      tmp <- left_join(
        cal_avec_dates,
        prestations,
        by = c("programme" = "prs_label_prestation")
      )

      # Identifier les programmes absents du marché
      absent <- tmp$programme[is.na(tmp$prs_id)]

      # Exclure les programmes sans analyses
      absent_reels <- setdiff(absent, prog_sans_analyses)

      # S'il reste des absents = vrai problème
      if (length(absent_reels) > 0) {
        stop("Programme(s) inconnus dans cal_avec_dates : ",
             paste(absent_reels, collapse = ", "))
      }

      # Remplacer les programmes sans analyses par NA contrôlé
      tmp$prs_id[is.na(tmp$prs_id) & tmp$programme %in% prog_sans_analyses] <- NA_integer_

      # Renommer
      cal_avec_dates <- rename(tmp, cal_prs_id = prs_id)
    }

    # Enregistrement calendrier + stub
    if (nrow(cal_avec_dates) > 0) {
      func_enregistre_dataframe_bdd(
        cal_avec_dates %>% select(cal_refannee, cal_mar_id, cal_typestation,
                                  cal_date, cal_prs_id, cal_rattachement_bdc),
        "t_calendrierprog_cal", "sqe", connexion
      )
    }


    # Traitement des lignes sans dates
    if (nrow(cal_sans_dates) > 0) {

      # Jointure normale
      tmp <- left_join(
        cal_sans_dates,
        prestations,
        by = c("programme" = "prs_label_prestation")
      )

      # Identifier les programmes absents du marché
      absent <- tmp$programme[is.na(tmp$prs_id)]

      # Exclure les programmes sans analyses
      absent_reels <- setdiff(absent, prog_sans_analyses)

      # S'il reste des absents = vrai problème
      if (length(absent_reels) > 0) {
        stop("Programme(s) inconnus dans cal_sans_dates : ",
             paste(absent_reels, collapse = ", "))
      }

      # Renommer
      cal_sans_dates <- rename(tmp, cal_prs_id = prs_id)
    }

    if (nrow(cal_sans_dates) > 0) {
      stub_cal <- cal_sans_dates %>%
        group_by(cal_refannee, cal_mar_id, cal_typestation, cal_prs_id, cal_rattachement_bdc) %>%
        summarise(cal_date = as.Date(paste0(annee, "-01-01")), .groups = "drop")
      func_enregistre_dataframe_bdd(
        stub_cal, "t_calendrierprog_cal", "sqe", connexion
      )
    }

    # Vérif cohérence typestation
    types_cal <- unique(c(cal_avec_dates$cal_typestation, cal_sans_dates$cal_typestation))
    types_file <- unique(programme_annuel$pga_cal_typestation)
    manq <- setdiff(types_file, types_cal)
    if (length(manq)) stop("type_station absent du calendrier : ", paste(manq, collapse=" ; "))

    # Insertion programme annuel
    func_enregistre_dataframe_bdd(
      programme_annuel,
      "t_progannuelle_pga",
      "sqe",
      connexion
    )




    # ────────
    # 3) BDC / BCQ / BCP / REA
    # ────────

    programme_annuel_clean <- programme_annuel %>%
      filter(!pga_stm_cdstationmesureinterne %in% c("sans_objet", "sans objet", "", NA))

    prog_avec <- left_join(
      programme_annuel_clean,
      cal_avec_dates,
      by = c("pga_cal_typestation" = "cal_typestation",
             "pga_stm_cdstationmesureinterne"="code_interne_station"),
      multiple = "all",
      relationship = "many-to-many"
    )

    prog_sans <- left_join(
      programme_annuel_clean,
      cal_sans_dates,
      by = c("pga_cal_typestation" = "cal_typestation",
             "pga_stm_cdstationmesureinterne"="code_interne_station"),
      multiple = "all",
      relationship = "many-to-many"
    )

    recode_mois <- function(x) as.numeric(dplyr::recode_factor(
      as.factor(x),
      "janvier"="1","fevrier"="2","mars"="3","avril"="4","mai"="5","juin"="6",
      "juillet"="7","aout"="8","septembre"="9","octobre"="10","novembre"="11","decembre"="12"
    ))
    if (nrow(prog_avec)) prog_avec$mois1 <- recode_mois(prog_avec$mois)
    if (nrow(prog_sans)) prog_sans$mois1 <- recode_mois(prog_sans$mois)

    set_num_bdc_tmp <- function(df) {
      if (nrow(df) == 0) return(df)
      if (frequence_bdc == "mensuelle")     df$num_bdc_tmp <- df$mois1
      if (frequence_bdc == "bimestrielle")  df$num_bdc_tmp <- cut(df$mois1, breaks=seq(0,12,2), labels=1:6) %>% as.numeric()
      if (frequence_bdc == "trimestrielle") df$num_bdc_tmp <- cut(df$mois1, breaks=seq(0,12,3), labels=1:4) %>% as.numeric()
      if (frequence_bdc == "semestrielle")  df$num_bdc_tmp <- cut(df$mois1, breaks=seq(0,12,6), labels=1:2) %>% as.numeric()
      if (frequence_bdc == "annuelle")      df$num_bdc_tmp <- 1
      df
    }
    prog_avec <- set_num_bdc_tmp(prog_avec)
    prog_sans <- set_num_bdc_tmp(prog_sans)

    calc_num_bdc <- function(df) {
      if (nrow(df)==0) return(df)
      tmp <- df %>% select(pga_per_nom, cal_rattachement_bdc, num_bdc_tmp) %>%
        unique() %>% arrange(num_bdc_tmp) %>%
        group_by(pga_per_nom, cal_rattachement_bdc) %>%
        mutate(num_bdc = rank(num_bdc_tmp)) %>% ungroup()
      left_join(df, tmp, by=c("pga_per_nom","cal_rattachement_bdc","num_bdc_tmp"))
    }
    prog_avec <- calc_num_bdc(prog_avec)
    prog_sans <- calc_num_bdc(prog_sans)

    make_ref <- function(df) {
      if (nrow(df)==0) return(df)
      df$bco_refcommande <- paste0(prefixe,"_",df$pga_per_nom,"_",df$cal_rattachement_bdc,"_",df$num_bdc)
      df
    }
    prog_avec <- make_ref(prog_avec)
    prog_sans <- make_ref(prog_sans) # bug : va créer un bdc avec NA

    # bco <- bind_rows(
    #   prog_avec %>% select(pga_per_nom, bco_refcommande),
    #   prog_sans %>% select(pga_per_nom, bco_refcommande)
    # ) %>% unique()

    bco <- prog_avec %>%
      select(pga_per_nom, bco_refcommande) %>%
      unique()

    if (nrow(bco)>0) {
      bco$bco_stp_nom <- "1-projet"
      bco$bco_mar_id  <- mar_id
      bco <- rename(bco, bco_per_nom = pga_per_nom)
      func_enregistre_dataframe_bdd(bco, "t_boncommande_bco", "sqe", connexion)
    }

    liste_bdc <- DBI::dbGetQuery(conn,
                                 sql(
      "SELECT *
  FROM sqe.t_boncommande_bco
  WHERE bco_mar_id = {mar_id};"
    ))

    # BCQ
    bcq_avec <- NULL
    bcq_sans <- NULL
    if (nrow(prog_avec)) {
      bcq_avec <- prog_avec %>%
        group_by(bco_refcommande, cal_prs_id) %>%
        count(name="bcq_nbprestacom") %>%
        ungroup() %>%
        left_join(liste_bdc,
                  by="bco_refcommande",
                  relationship = "many-to-many") %>%
        select(-bco_refcommande,
               -bco_mar_id,
               -bco_per_nom,
               -bco_stp_nom,
               -bco_commentaires ) %>%
        rename(bcq_bco_id=bco_id,
               bcq_prs_id=cal_prs_id)
    }
    if (nrow(prog_sans)) {
      bcq_sans <- prog_sans %>%
        group_by(bco_refcommande, cal_prs_id) %>%
        summarise(bcq_nbprestacom=sum(nb), .groups="drop") %>%
        left_join(liste_bdc,
                  by="bco_refcommande",
                  relationship = "many-to-many") %>%
        select(-bco_refcommande,
               -bco_mar_id,
               -bco_per_nom,
               -bco_stp_nom,
               -bco_commentaires ) %>%
        rename(bcq_bco_id=bco_id, bcq_prs_id=cal_prs_id)
    }
    # bcq_all <- bind_rows(bcq_avec, bcq_sans)
    bcq_all <- bcq_avec


    if (nrow(bcq_all)) func_enregistre_dataframe_bdd(bcq_all, "t_boncommande_quantitatif_bcq", "sqe", connexion)

    # BCP — uniquement prestations avec dates
    if (nrow(prog_avec)) {
      prog_avec <- left_join(prog_avec, liste_bdc, by="bco_refcommande")
      if (any(is.na(prog_avec$bco_id))) stop("BCP : bco_id manquant")

      data_bcp <- prog_avec %>%
        select(
        bco_id,
        cal_prs_id,
        cal_date,
        pga_stm_cdstationmesureinterne
      )
      names(data_bcp) <- c("bcp_bco_id",
                           "bcp_prs_id",
                           "bcp_dateinterv",
                           "bcp_stm_cdstationmesureinterne")

      data_bcp <- data_bcp %>%
        filter(!is.na(bcp_prs_id)) %>%                          # élimine prestations nulles
        filter(!bcp_stm_cdstationmesureinterne %in%
                 c("sans_objet", "sans objet", "", NA)) %>%      # élimine les stations sans_objet
        distinct()

      func_enregistre_dataframe_bdd(data_bcp, "t_boncommande_pgm_bcp", "sqe", connexion)
    }

    # REA — uniquement prestations avec dates
    if (nrow(prog_avec)) {
      prog_avec$res_codeprel <- paste0(prog_avec$bco_id,"*",prog_avec$cal_date,"*",prog_avec$pga_stm_cdstationmesureinterne)

      tmp_rea <- prog_avec %>%
        select(pga_stm_cdstationmesureinterne,
               res_codeprel,
               cal_date,
               bco_id,
               cal_prs_id)

      names(tmp_rea) <- c("res_stm_cdstationmesureinterne",
                          "res_codeprel",
                          "rea_dateprel_prev",
                          "res_bco_id",
                          "cal_prs_id")

      if (any(is.na(tmp_rea$res_bco_id)))
        stop("REA : bco_id manquant pour certaines lignes")

      # Paramètres types
      ppt <- DBI::dbGetQuery(conn, sql(
        "SELECT ppt_prs_id, ppt_par_cdparametre, ppt_fra_codefraction, ppt_uni_codesandreunite,
                ppt_met_codesandremethode, ppt_pre_id, ppt_analyseinsitu, ppt_limitedetec,
                ppt_limitequantif, ppt_incertitude, ppt_accreditation
         FROM sqe.t_parametreprogrammetype_ppt WHERE ppt_mar_id = {mar_id};"
      ))
      tmp_rea <- left_join(tmp_rea,
                           ppt,
                           by=c("cal_prs_id"="ppt_prs_id"),
                           multiple="all",
                           relationship = "many-to-many")

      # Prestataires
      prest <- DBI::dbGetQuery(conn, sql("SELECT pre_id, pre_siret FROM refer.tr_prestataire_pre;"))
      tmp_rea <- left_join(tmp_rea, prest, by=c("ppt_pre_id"="pre_id"))

      # Finalisation
      tmp_rea <- rename(tmp_rea,
                        rea_par_cdparametre    = ppt_par_cdparametre,
                        rea_cdfractionanalysee = ppt_fra_codefraction,
                        rea_cdunitemesure      = ppt_uni_codesandreunite,
                        rea_cdmethode          = ppt_met_codesandremethode,
                        rea_cdproducteur       = pre_siret,
                        rea_cdaccreanaprev     = ppt_accreditation,
                        rea_incertitudeprev    = ppt_incertitude,
                        rea_lqprev             = ppt_limitequantif,
                        rea_ldprev             = ppt_limitedetec)

      tmp_rea$rea_cdinsituana <- ifelse(tmp_rea$ppt_analyseinsitu == "TRUE","1","2")

      tmp_rea <- tmp_rea %>% select(
        res_stm_cdstationmesureinterne, res_codeprel, rea_dateprel_prev,
        rea_par_cdparametre, rea_cdfractionanalysee, rea_cdunitemesure,
        rea_cdmethode, rea_cdproducteur, rea_cdaccreanaprev,
        rea_incertitudeprev, rea_lqprev, rea_ldprev, rea_cdinsituana,
        res_bco_id
      )

      tmp_rea <- tmp_rea %>%
        # filter(!is.na(cal_prs_id)) %>%
        filter(!res_stm_cdstationmesureinterne %in%
                 c("sans_objet", "sans objet", "", NA))

      func_enregistre_dataframe_bdd(tmp_rea, "t_resultatanalyse_rea", "sqe", connexion)
    }

    invisible(TRUE)
  }
