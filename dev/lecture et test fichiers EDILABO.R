library(LibreSQE)

#####Fichier a tester #####
# file.choose()
fichier <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\05_livrables\\01_janvier\\RA_LABOCEAQ_EPTB_230307104729001.xml"

##### Connexion bdd #####
connexion <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

##### Chargement données de référence #####
table_stat_analyses <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses.rds"
  )
table_stat_analyses_toutes_staq <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses_toutes_staq.rds"
  )


# initialisation du rapport d'import
Rapport <- list()

##### Import et mise en forme du fichier à tester #####
test <- func_importe_edilabo(fichier)
Analyses <- test$Analyses
Stations <- test$Stations
Res_env <- test$Res_env
Intervenants <- test$Intervenants
Echantillon <- test$Echantillon
Operation <- test$Operation
Commemoratifs <- test$Commemoratifs
Demande <- test$Demande
rm(test)

##### Synthèse rapport #####
Rapport$fichier <- fichier
Rapport$nb_lignes <- data.frame(c("Analyses", "Stations", "ResEnv"),
                                c(nrow(Analyses), nrow(Stations), nrow(Res_env)))


###### conversion des données ######
# Analyses
Analyses <- dplyr::rename(
  Analyses,
  cdstationmesureinterne = CdStationPrelevement,
  codeprel = CdPrelevement,
  dateprel = DatePrel,
  cdsupport = CdSupport,
  cdfractionanalysee = FractionAnalysee_CdFractionAnalysee,
  heureprel = HeurePrel,
  dateana = DateAna,
  heureana = HeureAna,
  cdparametre = Parametre_CdParametre,
  rsana = RsAna,
  cdunitemesure = UniteReference_CdUniteReference,
  cdrqana = RqAna,
  cdinsituana = InsituAna,
  profondeurpre = ProfondeurPrel,
  ldana = LDAna,
  lqana = LQAna,
  lsana = LSAna,
  cdmetfractionnement = MethFractionnement_CdMethode,
  cdmethode = Methode_CdMethode,
  rdtextraction = RdtExtraction,
  cdmethodeextraction = MethExtraction_CdMethode,
  cdaccreana = AccreAna,
  agreana = AgreAna,
  incertitude = IncertAna,
  commentairesana = CommentairesAna,
  cdrdd = commemo_123,
  cdpreleveur = CdPreleveur,
  cdlaboratoire = Laboratoire_CdIntervenant,
  LbParametre = Parametre_NomParametre,
  LbFraction = FractionAnalysee_LbFractionAnalysee,
  LbSupport = LbSupport,
  LbMethode = Methode_NomMethode,
  LbUnite = UniteReference_LbUniteReference,
  SymUnite = UniteReference_SymUniteReference
)


Analyses$rsana <- as.numeric(Analyses$rsana)
Analyses$lqana <- as.numeric(Analyses$lqana)
Analyses$lsana <- as.numeric(Analyses$lsana)
Analyses$ldana <- as.numeric(Analyses$ldana)
Analyses$incertitude <- as.numeric(Analyses$incertitude)

Analyses$cdproducteur <- Demande$Commanditaire_CdIntervenant[1]

# Res_env
Res_env <- dplyr::rename(
  Res_env,
  cdstationmesureinterne = CdStationPrelevement,
  codeprel = CdPrelevement,
  dateprel = DatePrel,
  cdparametre = Parametre_CdParametre,
  rsparenv = RsParEnv,
  cdunitemesure = UniteReference_CdUniteReference,
  cdrqparenv = RqParEnv,
  dateparenv = DateParEnv,
  met_code = MethodePrel_CdMethode,
  cdpreleveur = CdPreleveur,
  LbParametre = Parametre_NomParametre
)


Res_env$cdproducteur <- Demande$Commanditaire_CdIntervenant[1]


##### test cohérence code / nom station #####

station_str <-
  paste0("'", Stations$CdStationPrelevement, "'", collapse = ", ")

# Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
requete <-
  paste(
    "SELECT * FROM refer.tr_stationmesure_stm WHERE stm_cdstationmesureinterne IN (",
    station_str,
    ")",
    sep = ""
  )

# Exécuter la requête en utilisant la connexion à la base de données
refer_staq <- DBI::dbGetQuery(connexion, requete)
Stations <-
  dplyr::left_join(Stations,
                   refer_staq,
                   by = c("CdStationPrelevement" = "stm_cdstationmesureinterne"))
Stations$LbStationConforme <-
  ifelse(
    Stations$LbStationPrelevement == Stations$stm_lbstationmesureeauxsurface,
    TRUE,
    FALSE
  )
Stations <- dplyr::rename(
  Stations,
  cdstationmesureinterne = CdStationPrelevement,
  rendu_dans_xml = LbStationPrelevement,
  attendu = stm_lbstationmesureeauxsurface
)


Rapport$nom_cd_station <- Stations %>%
  subset(LbStationConforme == FALSE &
           !is.na(rendu_dans_xml) & rendu_dans_xml != "") %>%
  dplyr::select(cdstationmesureinterne,
                rendu_dans_xml,
                attendu)

##### test cohérence code / nom paramètre (analyses et Res_enc) #####
#####Analyses #####
Parametres <- Analyses %>%
  dplyr::select(cdparametre, LbParametre) %>%
  dplyr::distinct()
cdparametre <-
  paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
    cdparametre,
    ")",
    sep = ""
  )

refer_param <- DBI::dbGetQuery(connexion, requete)
Parametres <-
  dplyr::left_join(Parametres,
                   refer_param,
                   by = c("cdparametre" = "par_cdparametre"))
Parametres$LbParametreConforme <-
  ifelse(Parametres$LbParametre == Parametres$par_nomparametre,
         TRUE,
         FALSE)

Parametres <- dplyr::rename(Parametres,
                            rendu_dans_xml = LbParametre,
                            attendu = par_nomparametre,
                            statut_parametre_dans_SANDRE=par_statutparametre)


Rapport$nom_cd_parametre_analyses <- Parametres %>%
  subset((LbParametreConforme == FALSE &
            !is.na(rendu_dans_xml) &
            rendu_dans_xml != "") |
           statut_parametre_dans_SANDRE != "Validé") %>%
  dplyr::select(cdparametre,
                rendu_dans_xml,
                attendu,
                statut_parametre_dans_SANDRE)

#### Res_env #####
Parametres <- Res_env %>%
  dplyr::select(cdparametre, LbParametre) %>%
  dplyr::distinct()
cdparametre <-
  paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
    cdparametre,
    ")",
    sep = ""
  )

refer_param <- DBI::dbGetQuery(connexion, requete)
Parametres <-
  dplyr::left_join(Parametres,
                   refer_param,
                   by = c("cdparametre" = "par_cdparametre"))
Parametres$LbParametreConforme <-
  ifelse(Parametres$LbParametre == Parametres$par_nomparametre,
         TRUE,
         FALSE)

Parametres <- dplyr::rename(Parametres,
                            rendu_dans_xml = LbParametre,
                            attendu = par_nomparametre,
                            statut_parametre_dans_SANDRE=par_statutparametre)


Rapport$nom_cd_parametre_res_env <- Parametres %>%
  subset((LbParametreConforme == FALSE &
            !is.na(rendu_dans_xml) &
            rendu_dans_xml != "") |
           statut_parametre_dans_SANDRE != "Validé") %>%
  dplyr::select(cdparametre,
                rendu_dans_xml,
                attendu,
                statut_parametre_dans_SANDRE)



##### test cohérence code fraction / nom fraction #####
#####Analyses #####
Fraction <- Analyses %>%
  dplyr::select(cdfractionanalysee, LbFraction) %>%
  dplyr::distinct()
cdfraction <-
  paste0("'", unique(Fraction$cdfractionanalysee), "'", collapse = ", ")
requete <-
  paste("SELECT * FROM refer.tr_fraction_fra WHERE fra_codefraction IN (",
        cdfraction,
        ")",
        sep = "")

refer_frac <- DBI::dbGetQuery(connexion, requete)
Fraction <-
  dplyr::left_join(Fraction,
                   refer_frac,
                   by = c("cdfractionanalysee" = "fra_codefraction"))
Fraction$LbFractionConforme <-
  ifelse(Fraction$LbFraction == Fraction$fra_nomfraction,
         TRUE,
         FALSE)

Fraction <- dplyr::rename(Fraction,
                          rendu_dans_xml = LbFraction,
                          attendu = fra_nomfraction)


Rapport$nom_cd_fraction_analyses <- Fraction %>%
  subset(LbFractionConforme == FALSE &
           !is.na(rendu_dans_xml) &
           rendu_dans_xml!="")

##### test cohérence code unité / nom unité #####
Unites <- Analyses %>%
  dplyr::select(cdunitemesure, LbUnite, SymUnite) %>%
  dplyr::distinct()
cdunitemesure <-
  paste0("'", unique(Unites$cdunitemesure), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_uniteparametre_uni WHERE uni_codesandreunite IN (",
    cdunitemesure,
    ")",
    sep = ""
  )

refer_unit <- DBI::dbGetQuery(connexion, requete)
Unites <-
  dplyr::left_join(Unites,
                   refer_unit,
                   by = c("cdunitemesure" = "uni_codesandreunite"))
Unites$LbUniteConforme <-
  ifelse(Unites$LbUnite == Unites$uni_lblsandreunite,
         TRUE,
         FALSE)
Unites$SymUniteConforme <-
  ifelse(Unites$SymUnite == Unites$uni_symbole,
         TRUE,
         FALSE)

Unites <- dplyr::rename(
  Unites,
  Lb_rendu_dans_xml = LbUnite,
  Lb_attendu = uni_lblsandreunite,
  Sym_rendu_dans_xml = SymUnite,
  Sym_attendu = uni_symbole
)


Rapport$nom_cd_unites_analyses <- Unites %>%
  subset((LbUniteConforme == FALSE &
            !is.na(Lb_rendu_dans_xml) &
            Lb_rendu_dans_xml!=""
          )|
           (SymUniteConforme == FALSE &
              !is.na(Sym_rendu_dans_xml) &
              Sym_rendu_dans_xml!="")) %>%
  dplyr::select(cdunitemesure,
                Lb_rendu_dans_xml,
                Lb_attendu,
                Sym_rendu_dans_xml,
                Sym_attendu)


# test cohérence code / nom intervenants
cdintervenants <-
  paste0("'", unique(Intervenants$CdIntervenant), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_intervenantsandre_isa WHERE isa_codesandre IN (",
    cdintervenants,
    ")",
    sep = ""
  )

refer_interv <- DBI::dbGetQuery(connexion, requete)
Intervenants <-
  dplyr::left_join(Intervenants,
                   refer_interv,
                   by = c("CdIntervenant" = "isa_codesandre"))
Intervenants$NomIntervenantConforme <-
  ifelse(Intervenants$NomIntervenant == Intervenants$isa_nom,
         TRUE,
         FALSE)
Intervenants$MnIntervenantConforme <-
  ifelse(Intervenants$MnIntervenant == Intervenants$isa_mnemo,
         TRUE,
         FALSE)

Intervenants <- dplyr::rename(
  Intervenants,
  Nom_rendu_dans_xml = NomIntervenant,
  Nom_attendu = isa_nom,
  Mn_rendu_dans_xml = MnIntervenant,
  Mn_attendu = isa_mnemo
)


Rapport$nom_mnemo_intervenants <- Intervenants %>%
  subset((NomIntervenantConforme == FALSE &
            !is.na(Nom_rendu_dans_xml) &
            Nom_rendu_dans_xml!="") |
           (MnIntervenantConforme == FALSE &
              !is.na(Mn_rendu_dans_xml) &
              Mn_rendu_dans_xml!="" )) %>%
  dplyr::select(CdIntervenant,
                Nom_rendu_dans_xml,
                Nom_attendu,
                Mn_rendu_dans_xml,
                Mn_attendu)


# test cohérence valeur résultat, LQ, code remarque
incoherence_cd_rq <-
  (
    ifelse(
      Analyses$cdrqana == "10" &
        Analyses$rsana > Analyses$lqana,
      TRUE,
      FALSE
    ) |
      ifelse(
        Analyses$cdrqana == "1" &
          Analyses$rsana < Analyses$lqana,
        TRUE,
        FALSE
      )
  ) &
  ifelse(is.na(Analyses$lqana) | is.na(Analyses$rsana), FALSE, TRUE)

Rapport$cd_rq_ou_lq_incoherent <- Analyses[incoherence_cd_rq, ]



##### test cohérence O2 satO2 temp #####

analyses_a_tester <-
  Analyses %>% subset(cdparametre %in% c("1301", "1311", "1312") &
                        cdsupport == "3" &
                        cdinsituana == "1") %>% unique()
analyses_a_tester <-
  analyses_a_tester %>% tidyr::pivot_wider(
    id_cols = c(
      cdstationmesureinterne,
      dateprel,
      heureprel,
      codeprel,
      cdrdd,
      RefEchantillonCommanditaire,
      RefEchantillonPrel,
      RefEchantillonLabo
    ),
    names_from = cdparametre,
    names_prefix = "p",
    values_from = rsana,
    values_fn = max
  )

analyses_a_tester <-
  analyses_a_tester %>% subset(!is.na(p1301) &
                                 !is.na(p1311) & !is.na(p1312))


fct_test_O2 <-
  function(x) {
    func_test_metier_coherenceO2(O2 = analyses_a_tester[["p1311"]][x],
                                 satO2 = analyses_a_tester[["p1312"]][x],
                                 temp = analyses_a_tester[["p1301"]][x])
  }

fct_test_O2_val <-
  function(x) {
    func_test_metier_coherenceO2(
      O2 = analyses_a_tester[["p1311"]][x],
      satO2 = analyses_a_tester[["p1312"]][x],
      temp = analyses_a_tester[["p1301"]][x],
      export = "value"
    )
  }

analyses_a_tester$testO2 <-
  sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
         FUN = fct_test_O2)
analyses_a_tester$test_satO2_attendu <-
  sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
         FUN = fct_test_O2_val)

analyses_a_tester <-
  analyses_a_tester %>% dplyr::rename(O2 = p1311,
                                      satO2 = p1312,
                                      temperature = p1301)
analyses_a_tester$testO2 <- analyses_a_tester$testO2 %>%
  dplyr::case_match("1" ~ "correcte", "2" ~ "incertain", "3" ~ "incorrect")

Rapport$coherence_O2_temp <-
  analyses_a_tester %>% subset(testO2 != "correcte")

##### test cohérence Ptot PO4 #####

# Cd Sandre PO4 = 1433
# Cd Sandre Ptot = 1350

analyses_a_tester <-
  Analyses %>% subset((cdparametre == "1350" &
                         cdfractionanalysee == "23") |
                        (cdparametre == "1433" &
                           cdsupport == "3")
  ) %>% unique()


incertitudes_a_tester <-
  analyses_a_tester %>% tidyr::pivot_wider(
    id_cols = c(
      cdstationmesureinterne,
      dateprel,
      heureprel,
      codeprel,
      cdrdd,
      RefEchantillonCommanditaire,
      RefEchantillonPrel,
      RefEchantillonLabo
    ),
    names_from = cdparametre,
    names_prefix =
      "incer",
    values_from = incertitude,
    values_fn = max
  )

analyses_a_tester <-
  analyses_a_tester %>% tidyr::pivot_wider(
    id_cols = c(
      cdstationmesureinterne,
      dateprel,
      heureprel,
      codeprel,
      cdrdd,
      RefEchantillonCommanditaire,
      RefEchantillonPrel,
      RefEchantillonLabo
    ),
    names_from =
      cdparametre,
    names_prefix =
      "p",
    values_from = rsana,
    values_fn = max
  )

analyses_a_tester <- dplyr::left_join(
  analyses_a_tester,
  incertitudes_a_tester,
  by = c(
    "cdstationmesureinterne",
    "dateprel",
    "heureprel",
    "codeprel",
    "cdrdd",
    "RefEchantillonCommanditaire",
    "RefEchantillonPrel",
    "RefEchantillonLabo"
  )
)

analyses_a_tester <-
  analyses_a_tester %>% subset(!is.na(p1350) & !is.na(p1433))


fct_test_P <-
  function(x) {
    func_test_metier_coherenceP(
      Ptot = analyses_a_tester[["p1350"]][x],
      PO4 = analyses_a_tester[["p1433"]][x],
      incertPtot = analyses_a_tester[["incer1350"]][x],
      incertPO4 = analyses_a_tester[["incer1350"]][x]
    )
  }

fct_test_P_val <-
  function(x) {
    func_test_metier_coherenceP(
      Ptot = analyses_a_tester[["p1350"]][x],
      PO4 = analyses_a_tester[["p1433"]][x],
      incertPtot = analyses_a_tester[["incer1350"]][x],
      incertPO4 = analyses_a_tester[["incer1350"]][x],
      export = "value"
    )
  }

analyses_a_tester$testPtot_PO4 <-
  sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
         FUN = fct_test_P)
analyses_a_tester$PO4_en_PPO4 <-
  sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
         FUN = fct_test_P_val)

analyses_a_tester$pourcentage_PO4_sur_Ptot <-
  round(analyses_a_tester$PO4_en_PPO4 / analyses_a_tester$p1350 * 100,
        1)

analyses_a_tester <-
  analyses_a_tester %>% dplyr::rename(
    Ptot = p1350,
    PO4 = p1433,
    incertitude_Ptot = incer1350,
    incertitude_PO4 = incer1433
  )
analyses_a_tester$testPtot_PO4 <- analyses_a_tester$testPtot_PO4 %>%
  dplyr::case_match("1" ~ "correcte", "2" ~ "dans marge incertitude", "3" ~ "incorrect")

Rapport$coherence_Ptot_PO4 <-
  analyses_a_tester %>% subset(testPtot_PO4 != "correcte")


# test vraissemblance des résultats



