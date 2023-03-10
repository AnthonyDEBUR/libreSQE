library(LibreSQE)
library(openxlsx)

#####Fichier a tester #####
# file.choose()

# SQE2023_UGVA_calend_1
fichier <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\05_livrables\\01_janvier\\RA_LABOCEAQ_EPTB_230307104729001.xml"
bon_de_commande_id <- 57

# SQE2023_UGVE_calend_1
fichier<-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\05_livrables\\01_janvier\\RA_LABOCEAQ_EPTB_230307110458001.xml"
bon_de_commande_id <- 20

# SQE2023_UGVO_calend_1
fichier<-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\05_livrables\\01_janvier\\RA_LABOCEAQ_EPTB_230307111654001.xml"
bon_de_commande_id <- 32


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

##### chargement des analyses attendues pour le bon de commande #####
# Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
requete <-
  paste(
    "SELECT * FROM sqe.t_resultatanalyse_rea WHERE res_bco_id IN (",
    bon_de_commande_id,
    ")",
    sep = ""
  )

# Exécuter la requête en utilisant la connexion à la base de données
analyses_attendues <- DBI::dbGetQuery(connexion, requete)

analyses_attendues <- analyses_attendues %>%
  subset(res_stm_cdstationmesureinterne != "sans_objet")

analyses_attendues$nomparametre <- func_ajoute_nom_sandre(connexion,
                                                          code = analyses_attendues$rea_par_cdparametre,
                                                          out = "nom_parametre")

analyses_attendues$nomfraction <- func_ajoute_nom_sandre(connexion,
                                                         code = analyses_attendues$rea_cdfractionanalysee,
                                                         out = "nom_fraction")

analyses_attendues$nompreleveur <- func_ajoute_nom_sandre(connexion,
                                                          code = analyses_attendues$rea_cdpreleveur,
                                                          out = "nom_intervenant")

analyses_attendues$nomlabo <- func_ajoute_nom_sandre(connexion,
                                                     code = analyses_attendues$rea_cdlaboratoire,
                                                     out = "nom_intervenant")


##### référence bon de commande #####
# Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
requete <-
  paste(
    "SELECT bco_refcommande FROM sqe.t_boncommande_bco WHERE bco_id IN (",
    bon_de_commande_id,
    ")",
    sep = ""
  )

# Exécuter la requête en utilisant la connexion à la base de données
ref_bon_de_commande <- DBI::dbGetQuery(connexion, requete)


##### initialisation du rapport d'import #####
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
Rapport$bon_de_commande <- ref_bon_de_commande
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

# Ajout des noms paramètres, fraction, symboles unités, nom labo, noms Rdd
Analyses$nomparametre <-
  func_ajoute_nom_sandre(connexion = connexion,
                         code = Analyses$cdparametre,
                         out = "nom_parametre")

Analyses$unite <- func_ajoute_nom_sandre(connexion = connexion,
                                         code = Analyses$cdunitemesure,
                                         out = "nom_unite")

Analyses$nomfraction <- func_ajoute_nom_sandre(
  connexion = connexion,
  code = Analyses$cdfractionanalysee,
  out = "nom_fraction"
)

Analyses$nomlabo <- func_ajoute_nom_sandre(connexion = connexion,
                                           code = Analyses$cdlaboratoire,
                                           out = "nom_long_intervenant")

Analyses$nompreleveur <-
  func_ajoute_nom_sandre(connexion = connexion,
                         code = Analyses$cdpreleveur,
                         out = "nom_long_intervenant")

Analyses$insitu <- func_ajoute_nom_sandre(connexion = connexion,
                                          code = Analyses$cdinsituana,
                                          out = "insitu")

Analyses$station <- func_ajoute_nom_sandre(
  connexion = connexion,
  code = Analyses$cdstationmesureinterne,
  out = "nom_station"
)


func_noms_codes <- function(codesrdd_vecteur) {
  # Divise chaque élément du vecteur en entrée en un sous-vecteur de codes
  codes_separe <- strsplit(codesrdd_vecteur, "/")

  # Applique la fonction func_ajoute_nom_sandre à chaque sous-vecteur de codes
  noms_codes <-
    lapply(codes_separe, function(x)
      func_ajoute_nom_sandre(
        connexion = connexion,
        code =
          x,
        out =
          "nom_rdd"
      ))

  # Concatène les noms de code séparés par des slashs à nouveau
  noms_codes_concatenes <- sapply(noms_codes, paste, collapse = "/")

  return(noms_codes_concatenes)
}

Analyses$nomrdd <- func_noms_codes(Analyses$cdrdd)


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

Stations$stm_lbstationmesureeauxsurface <-
  func_ajoute_nom_sandre(connexion ,
                         code = Stations$CdStationPrelevement,
                         out = "nom_station")
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


Rapport$nom_cd_station <- unique(
  Stations %>%
    subset(
      LbStationConforme == FALSE &
        !is.na(rendu_dans_xml) & rendu_dans_xml != ""
    ) %>%
    dplyr::select(cdstationmesureinterne,
                  rendu_dans_xml,
                  attendu)
)

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
  ifelse(
    Parametres$LbParametre == Parametres$par_nomparametre |
      Parametres$LbParametre == Parametres$par_nomcourt,
    TRUE,
    FALSE
  )

Parametres <- dplyr::rename(
  Parametres,
  rendu_dans_xml = LbParametre,
  attendu = par_nomparametre,
  statut_parametre_dans_SANDRE = par_statutparametre
)


Rapport$nom_cd_parametre_analyses <- Parametres %>%
  subset((
    LbParametreConforme == FALSE &
      !is.na(rendu_dans_xml) &
      rendu_dans_xml != ""
  ) |
    statut_parametre_dans_SANDRE != "Validé"
  ) %>%
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
  ifelse(
    Parametres$LbParametre == Parametres$par_nomparametre |
      Parametres$LbParametre == Parametres$par_nomcourt,
    TRUE,
    FALSE
  )

Parametres <- dplyr::rename(
  Parametres,
  rendu_dans_xml = LbParametre,
  attendu = par_nomparametre,
  statut_parametre_dans_SANDRE = par_statutparametre
)


Rapport$nom_cd_parametre_res_env <- Parametres %>%
  subset((
    LbParametreConforme == FALSE &
      !is.na(rendu_dans_xml) &
      rendu_dans_xml != ""
  ) |
    statut_parametre_dans_SANDRE != "Validé"
  ) %>%
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
           rendu_dans_xml != "")

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
  subset((
    LbUniteConforme == FALSE &
      !is.na(Lb_rendu_dans_xml) &
      Lb_rendu_dans_xml != ""
  ) |
    (
      SymUniteConforme == FALSE &
        !is.na(Sym_rendu_dans_xml) &
        Sym_rendu_dans_xml != ""
    )
  ) %>%
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
  subset((
    NomIntervenantConforme == FALSE &
      !is.na(Nom_rendu_dans_xml) &
      Nom_rendu_dans_xml != ""
  ) |
    (
      MnIntervenantConforme == FALSE &
        !is.na(Mn_rendu_dans_xml) &
        Mn_rendu_dans_xml != ""
    )
  ) %>%
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
    print(analyses_a_tester[["cdstationmesureinterne"]][x])
    func_test_metier_coherenceP(
      Ptot = analyses_a_tester[["p1350"]][x],
      PO4 = analyses_a_tester[["p1433"]][x],
      incertPtot = analyses_a_tester[["incer1350"]][x],
      incertPO4 = analyses_a_tester[["incer1350"]][x],
      forceincert = TRUE
    )
  }

fct_test_P_val <-
  function(x) {
    func_test_metier_coherenceP(
      Ptot = analyses_a_tester[["p1350"]][x],
      PO4 = analyses_a_tester[["p1433"]][x],
      incertPtot = analyses_a_tester[["incer1350"]][x],
      incertPO4 = analyses_a_tester[["incer1350"]][x],
      export = "value",
      forceincert = TRUE
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


##### Test vraissemblance des résultats d'analyses #####
Analyses$cle_frac_unit <- paste0(
  Analyses$cdparametre,
  "_",
  Analyses$cdfractionanalysee,
  "_",
  Analyses$cdunitemesure
)

Analyses$cle_staq_frac_unit <-
  paste0(
    Analyses$cdstationmesureinterne,
    "_",
    Analyses$cdparametre,
    "_",
    Analyses$cdfractionanalysee,
    "_",
    Analyses$cdunitemesure
  )
verif <- Analyses %>%
  dplyr::left_join(table_stat_analyses, by = "cle_staq_frac_unit") %>%
  dplyr::left_join(table_stat_analyses_toutes_staq, by = "cle_frac_unit") %>%
  subset(cdrqana == "1")

##### qualif par station #####
verif$classement_par_station <- 99

# classe 1
verif$classement_par_station <-
  ifelse(
    verif$rsana >= verif$Q10ST &
      verif$rsana <= verif$Q90ST,
    1,
    verif$classement_par_station
  )

# classe 2
verif$classement_par_station <- ifelse(
  (verif$rsana >= verif$Q5ST & verif$rsana < verif$Q10ST) |
    (verif$rsana <= verif$Q95ST &
       verif$rsana > verif$Q90ST),
  2,
  verif$classement_par_station
)

# classe 3
verif$classement_par_station <- ifelse(
  (verif$rsana >= verif$minST & verif$rsana < verif$Q5ST) |
    (verif$rsana <= verif$maxST &
       verif$rsana > verif$Q95ST),
  3,
  verif$classement_par_station
)

# classe 7
verif$classement_par_station <- ifelse(
  (
    verif$rsana >= (verif$minST - verif$sdST) &
      verif$rsana < verif$minST
  ) |
    (
      verif$rsana <= (verif$maxST + verif$sdST) &
        verif$rsana > verif$maxST
    ),
  7,
  verif$classement_par_station
)

# classe 9
verif$classement_par_station <- ifelse(
  (
    verif$rsana >= (verif$minST - 2 * verif$sdST) &
      verif$rsana < (verif$minST - verif$sdST)
  ) |
    (
      verif$rsana <= (verif$maxST + 2 * verif$sdST) &
        verif$rsana > (verif$maxST + verif$sdST)
    ),
  9,
  verif$classement_par_station
)

# classe 10
verif$classement_par_station <-
  ifelse((verif$rsana <= (verif$minST - 2 *
                            verif$sdST)) |
           (verif$rsana >= (verif$maxST + 2 * verif$sdST)),
         10,
         verif$classement_par_station)

# classe 0
verif$classement_par_station <-
  ifelse(is.na(verif$classement_par_station),
         0,
         verif$classement_par_station)


##### qualif toutes stations

verif$classement_toutes_station <- 99

# classe 2
verif$classement_toutes_station <-
  ifelse(
    verif$rsana >= verif$Q10 &
      verif$rsana <= verif$Q90,
    2,
    verif$classement_toutes_station
  )

# classe 3
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$Q5 & verif$rsana < verif$Q10) |
    (verif$rsana <= verif$Q95 &
       verif$rsana > verif$Q90),
  3,
  verif$classement_toutes_station
)

# classe 4
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$Q1 & verif$rsana < verif$Q5) |
    (verif$rsana <= verif$Q99 &
       verif$rsana > verif$Q95),
  4,
  verif$classement_toutes_station
)

# classe 6
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$min & verif$rsana < verif$Q1) |
    (verif$rsana <= verif$max &
       verif$rsana > verif$Q99),
  6,
  verif$classement_toutes_station
)

# classe 8
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= (verif$min - verif$sd) &
     verif$rsana < verif$min) |
    (verif$rsana <= (verif$max + verif$sd) &
       verif$rsana > verif$max),
  8,
  verif$classement_toutes_station
)


# classe 10
verif$classement_toutes_station <-
  ifelse((verif$rsana <= (verif$min -
                            verif$sd)) |
           (verif$rsana >= (verif$max + verif$sd)),
         10,
         verif$classement_toutes_station)

# classe 0
verif$classement_toutes_station <-
  ifelse(is.na(verif$classement_toutes_station),
         0,
         verif$classement_toutes_station)

##### agrégation des vérifications
verif$cdqualana <-
  ifelse(verif$classement_toutes_station == 0 &
           verif$classement_par_station == 0,
         3,
         4)

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2, 3, 4) &
      verif$classement_par_station %in% c(0, 1, 2),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2, 3) &
      verif$classement_par_station %in% c(3),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(6) &
      verif$classement_par_station %in% c(3),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(6) &
      verif$classement_par_station %in% c(0, 1, 2, 3, 7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(4) &
      verif$classement_par_station %in% c(3, 7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(3) &
      verif$classement_par_station %in% c(7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2) &
      verif$classement_par_station %in% c(7, 9),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(verif$classement_toutes_station %in% c(8, 10),
         2,
         verif$cdqualana)

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(3, 4, 6) &
      verif$classement_par_station %in% c(9, 10),
    2,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2) &
      verif$classement_par_station %in% c(10),
    2,
    verif$cdqualana
  )


verif$cdqualana <- as.character(verif$cdqualana)
verif$nomclassement <- verif$cdqualana %>%
  dplyr::case_match(
    "0" ~ "non definissable",
    "1" ~ "correct",
    "2" ~ "incorrect",
    "3" ~ "incertain",
    "4" ~ "non qualifié"
  )

# export des resultats

Rapport$resultats_a_confirmer <- verif %>%
  dplyr::select(
    "nomclassement",
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  ) %>%
  subset(nomclassement %in% c("incorrect",
                              "incertain"))

##### ajout de la qualification à la table Analyses #####
verif <-
  verif %>% dplyr::select(
    "cdqualana",
    "cdstationmesureinterne",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "cdinsituana",
    "cdfractionanalysee",
    "cdrqana",
    "rsana",
    "cdunitemesure",
    "cdpreleveur",
    "cdlaboratoire",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire"
  )

Analyses <-
  Analyses %>% dplyr::left_join(
    verif,
    by = c(
      "cdstationmesureinterne",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "cdinsituana",
      "cdfractionanalysee",
      "cdrqana",
      "rsana",
      "cdunitemesure",
      "cdpreleveur",
      "cdlaboratoire",
      "RefEchantillonLabo",
      "RefEchantillonCommanditaire"
    )
  )


##### Comparaison bon de commande - réalisé #####

analyses_attendues$cle <-
  paste0(
    analyses_attendues$res_stm_cdstationmesureinterne,
    "_",
    analyses_attendues$rea_cdfractionanalysee,
    "_",
    analyses_attendues$rea_par_cdparametre,
    "_",
    analyses_attendues$rea_cdunitemesure,
    "_",
    analyses_attendues$rea_cdinsituana
  )

Analyses$cle <-
  paste0(
    Analyses$cdstationmesureinterne,
    "_",
    Analyses$cdfractionanalysee,
    "_",
    Analyses$cdparametre,
    "_",
    Analyses$cdunitemesure,
    "_",
    Analyses$cdinsituana
  )



##### Stations commandées et réalisées #####

Rapport$stations_commandees_analysees <- Analyses %>%
  subset(
    cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne &
      cdrqana != "0"
  ) %>%
  dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
  dplyr::summarise(nb_donnees = dplyr::n())

##### Stations non commandées et réalisées #####                                                                                                                  )
Rapport$stations_non_commandees_analysees <- Analyses %>%
  subset(
    !(
      cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne &
        cdrqana != "0"
    )
  ) %>%
  dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
  dplyr::summarise(nb_donnees = dplyr::n())

##### Stations commandées et non réalisées #####                                                                                                                  )
analyses_attendues$station <- func_ajoute_nom_sandre(connexion,
                                                     code = analyses_attendues$res_stm_cdstationmesureinterne,
                                                     out = "nom_station")

Rapport$stations_commandees_non_analysees <- analyses_attendues %>%
  subset(!(
    res_stm_cdstationmesureinterne %in% Analyses$cdstationmesureinterne
  )) %>%
  dplyr::group_by(res_stm_cdstationmesureinterne, station) %>%
  dplyr::summarise(nb_donnees = dplyr::n())


##### Analyses hors bon de commande (analyses en +) #####
Rapport$analyses_hors_bon_de_commande <- Analyses %>%
  subset((!cle %in% analyses_attendues$cle) &
           (
             !cdstationmesureinterne %in% Rapport$stations_commandees_non_analysees$cdstationmesureinterne
           )
  ) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  )

##### Analyses en doublon #####
doublons <- Analyses %>%
  dplyr::group_by(cle, dateprel) %>%
  dplyr::summarise(nb = dplyr::n()) %>%
  subset(nb > 1)

Rapport$analyses_en_doublon <- Analyses %>%
  dplyr::inner_join(doublons, by = c("cle", "dateprel")) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  )

##### Analyses manquantes #####
nb_analyses_attendues <- analyses_attendues %>%
  subset(
    !res_stm_cdstationmesureinterne %in%
      Rapport$stations_commandees_non_analysees$cdstationmesureinterne
  ) %>%
  unique %>%
  dplyr::group_by(cle) %>%
  dplyr::summarise(nb_attendu = dplyr::n()) %>%
  dplyr::ungroup()

nb_analyses_rendues <- Analyses %>%
  subset(cdrqana != "0") %>%
  dplyr::select(cle, dateprel) %>%
  unique %>%
  dplyr::group_by(cle) %>%
  dplyr::summarise(nb_rendu = dplyr::n()) %>%
  dplyr::ungroup()
nb_analyses_rendues$nb_rendu <-
  ifelse(is.na(nb_analyses_rendues$nb_rendu),
         0,
         nb_analyses_rendues$nb_rendu)


delta_analyses <- dplyr::left_join(nb_analyses_attendues,
                                   nb_analyses_rendues,
                                   by = "cle") %>%
  dplyr::mutate(delta = nb_attendu - ifelse(is.na(nb_rendu), 0, nb_rendu)) %>%
  subset(delta > 0)

extraire_lignes <- function(df, delta_analyses) {
  # Calculer le nombre d'occurrences de chaque clé dans le data.frame d'analyses
  df$occurrences <- ave(df$cle, df$cle, FUN = length)

  # Fusionner avec le data.frame delta pour récupérer les nombres d'occurrences souhaités
  df <- merge(df, delta_analyses, by = "cle", all = TRUE)

  # Extraire les lignes avec les clés et les nombres d'occurrences souhaités
  df <- subset(df, occurrences <= delta)

  # Retirer la colonne "occurrences" ajoutée précédemment
  df$occurrences <- NULL

  # Retourner le data.frame résultant
  return(df)
}




Rapport$analyses_manquantes <-
  extraire_lignes(analyses_attendues, delta_analyses) %>%
  dplyr::select(
    "res_stm_cdstationmesureinterne",
    "station",
    "rea_dateprel_prev",
    "rea_profondeurpre",
    "rea_par_cdparametre",
    "nomparametre",
    "rea_cdinsituana",
    "nomfraction",
    "rea_cdpreleveur",
    "nompreleveur",
    "rea_cdlaboratoire",
    "nomlabo",
    "rea_rdd_cdrdd"
  )

##### Vérification dispositif de collecte #####
Rapport$reseaux_de_mesures <- Analyses %>%
  dplyr::group_by(cdrdd, nomrdd) %>%
  dplyr::summarise(nb = dplyr::n(),
                   pourcent = 100 * dplyr::n() / nrow(Analyses))

##### Vérification respect des LQ contractuelles #####

compar_perf_anal <- dplyr::inner_join(Analyses,
                                      analyses_attendues,
                                      by = "cle",
                                      suffix = c("", ".attendu"))


Rapport$lq_non_conforme <- compar_perf_anal %>%
  subset((lqana > rea_lqprev) & cdrqana == "10") %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "rea_lqprev",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("lq_au_bpu" = "rea_lqprev")

##### Vérification respect des accréditations #####

Rapport$accreditation_non_conforme <- compar_perf_anal %>%
  subset(rea_cdaccreanaprev == "true" & cdaccreana != "1") %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "cdaccreana",
    "rea_cdaccreanaprev",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("accreditation_attendue" = "rea_cdaccreanaprev")


# calcul taux de résultats rendus sous accréditation par rapport à celui attendu

if (nrow(compar_perf_anal %>%
         subset(rea_cdaccreanaprev == "true")) > 0)
{
  Rapport$taux_result_accredites_sur_attendus <-
    nrow(compar_perf_anal %>%
           subset(rea_cdaccreanaprev ==
                    "true" & cdaccreana == "1")) / nrow(compar_perf_anal %>%
                                                          subset(rea_cdaccreanaprev ==
                                                                   "true")) * 100
}

##### Vérification des incertitudes contractuelles #####
Rapport$incertitude_non_conforme <- compar_perf_anal %>%
  subset(incertitude > rea_incertitudeprev) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "incertitude",
    "rea_incertitudeprev",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("incertitude_au_bpu" = "rea_incertitudeprev")


##### Vérification des méthodes analytiques contractuelles #####
compar_perf_anal$methode_au_bpu <- func_ajoute_nom_sandre(connexion,
                                                          code = compar_perf_anal$rea_cdmethode,
                                                          out = "nom_methode")

compar_perf_anal$methode_analyse <- func_ajoute_nom_sandre(connexion,
                                                           code = compar_perf_anal$cdmethode,
                                                           out = "nom_methode")

Rapport$methode_non_conforme <- compar_perf_anal %>%
  subset(cdmethode != rea_cdmethode) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "cdmethode",
    "methode_analyse",
    "rea_cdmethode",
    "methode_au_bpu",
    "CommentairesEchant",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "RefEchantillonLabo",
    "RefEchantillonCommanditaire",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("cd_methode_au_bpu" = "rea_cdmethode")


##### Test durée transport / réception échantillon labo #####

Rapport$duree_transport <- "Fonction à implémenter"



##### Test température réception échantillon #####

Rapport$temperature_reception_echantillon <-
  "Fonction à implémenter"


##### Génération d'un rapport xlsx #####
wb <- createWorkbook()
for (i in 1:length(Rapport)) {
  if (is.data.frame(Rapport[[i]])) {
    if (nrow(Rapport[[i]]) > 0) {
      if (nchar((names(Rapport))[i]) > 30) {
        nom_sheet <-
          paste0(substr((names(Rapport))[i], 1, 15), "_", substr((names(Rapport))[i], nchar((
            names(Rapport)
          )[i]) - 14, nchar((
            names(Rapport)
          )[i])))
      } else {
        nom_sheet <- (names(Rapport))[i]
      }
      addWorksheet(wb, sheetName = nom_sheet)
      writeData(wb, sheet = nom_sheet, x = Rapport[[i]])
    }
  }
}
saveWorkbook(wb, paste0(basename(Rapport$fichier), ".xlsx"), overwrite=TRUE)
