##### Export des résultats 2023 en QUESU v3 #####
library(LibreSQE)
library(tidyverse)
library(xml2)

annee <- "2023"
mois <- c(1, 12) # mois de debut et de fin de l'export
 mois<-1

nomfichier <- paste0(Sys.Date(),
                     "_Eaux_et_Vilaine_",
                     paste(unique(c(
                       min(mois), max(mois)
                     )), collapse = "_", sep = ""),
                     "_",
                     annee)
cd_emetteur <- "25440124300012"
nom_emetteur <-
  "ETABLISSEMENT PUBLIC TERRITORIAL DU BASSIN DE LA VILAINE (EPTB)"
cd_destinataire <- "22350001800013" #CD35
nom_destinataire <- "DEPARTEMENT D ILLE ET VILAINE"

# lecture du fichier de syntèse E&V
Analyses <-
  readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")
Res_env <-
  readRDS("~/R_Anthony/Naiades/bdd_locale/cond_env_Eaux_Vilaine.rds")
Operation <-
  readRDS("~/R_Anthony/Naiades/bdd_locale/operations_Eaux_Vilaine.rds")

##### Sélection station à pb #####
 # Analyses <- Analyses%>%subset(CdStationMesureEauxSurface%in%c("04376019"))
 # Res_env <- Res_env%>%subset(CdStationMesureEauxSurface%in%c("04376019") & CdParametreEnv=="1410")
 # Operation <- Operation%>%subset(CdStationMesureEauxSurface%in%c("04376019") )

#bdd lieux de qualité
lieux_qual <-
  readRDS("C:/workspace/LibreSQE/lieux_qual_1970_au_20240126.rds")

# on ne conserve que les données entre la date de début et de fin
mois_chr <-
  ifelse(min(mois) < 9, paste0("0", min(mois)), paste0(min(mois)))
mois_chr1 <-
  ifelse(max(mois) < 8, paste0("0", max(mois) + 1), paste0(max(mois) + 1))

date_debut <- as.Date(paste0(annee, "-", mois_chr, "-01"))
date_fin <- as.Date(ifelse(
  mois_chr1 == "13",
  paste0(as.numeric(annee) + 1, "-01-01"),
  paste0(annee, "-", mois_chr1, "-01")
))


Analyses <-
  Analyses %>% subset(DatePrel >= date_debut & DatePrel < date_fin)
Res_env <-
  Res_env %>% subset(DateParEnv >= date_debut & DateParEnv < date_fin)
Operation <-
  Operation %>% subset(DatePrel >= date_debut & DatePrel < date_fin)
Analyses$DatePrel <- as.character(Analyses$DatePrel)

##### recherche de doublons dans Operation #####
# Sélection des colonnes spécifiées
cols <- c("CdStationMesureEauxSurface", "DatePrel", "PointPrel")

# Trouver les lignes en doublons pour les colonnes spécifiées
doublons <-
  Operation[duplicated(Operation[, cols]) |
              duplicated(Operation[, cols], fromLast = TRUE),]
Operation <- Operation[!(duplicated(Operation[, cols])),]

##### recherche de doublons dans res_env #####
# Sélection des colonnes spécifiées
cols <-
  c("CdStationMesureEauxSurface", "DateParEnv", "CdParametreEnv")

# Trouver les lignes en doublons pour les colonnes spécifiées
doublons <-
  Res_env[duplicated(Res_env[, cols]) |
            duplicated(Res_env[, cols], fromLast = TRUE) &
            is.na(Res_env$HeureParEnv),]
Res_env <- Res_env[!(
  duplicated(Res_env[, cols]) |
    duplicated(Res_env[, cols],
               fromLast = TRUE) &
    is.na(Res_env$HeureParEnv)
),]



# recherche de doublons dans analyses
# Sélection des colonnes spécifiées
cols <-
  c(
    "CdStationMesureEauxSurface",
    "CdSupport",
    "CdFractionAnalysee",
    "CdPrelevement",
    "HeurePrel",
    "CdParametre",
    "RsAna",
    "CdRqAna",
    "CdInsituAna"
  )

# Trouver les lignes en doublons pour les colonnes spécifiées
doublons <- Analyses[duplicated(Analyses[, cols]) |
                       duplicated(Analyses[, cols], fromLast = TRUE),]

# Suppression des doublons à l'exemption de la colonne IncertitudeAna (on supprime les lignes ou incertitudeAna n'a aps été renseigné)
Analyses <- Analyses[!(((
  duplicated(Analyses[, cols]) &
    !duplicated(Analyses[, c(cols, "IncertAna")])
) |
  (
    duplicated(Analyses[, cols], fromLast = TRUE) &
      !duplicated(Analyses[, c(cols, "IncertAna")], fromLast = TRUE)
  )) &
  is.na(Analyses$IncertAna)),]

# Suppression des doublons à l'exemption de la colonne LqAna (on supprime les lignes ou LqAna n'a pas été renseigné)
Analyses <- Analyses[!(((
  duplicated(Analyses[, cols]) &
    !duplicated(Analyses[, c(cols, "LqAna")])
) |
  (
    duplicated(Analyses[, cols], fromLast = TRUE) &
      !duplicated(Analyses[, c(cols, "LqAna")], fromLast = TRUE)
  )) &
  is.na(Analyses$LqAna)),]


# suppression des doublons sur un suivi avec mauvais code réseau
# Trouver les lignes en doublons pour les colonnes spécifiées
Analyses <- Analyses[!((duplicated(Analyses[, cols]) |
                          duplicated(Analyses[, cols], fromLast = TRUE))
                       & Analyses$CdRdd == "0400000922/0400003223"),]

# recherche d'analyses avec date analyse < date prel
# on remplace dateAnal par datePrel
Analyses <- Analyses %>%
  mutate(HeureAna = ifelse(!is.na(HeureAna) &
                             HeureAna < HeurePrel, HeurePrel, HeureAna))

# recherche des Analyses pour lesquelles il n'existe pas d'Operation renseignée
Operation_manquante <- Analyses %>%
  select(names(Analyses)[names(Analyses) %in% c(names(Operation), "CdRdd")]) %>%
  subset(!HeurePrel %in% Operation$HeurePrel) %>%
  unique

# ajout des colonnes manquantes aux opérations manquantes :
colonnes_manquantes <-
  setdiff(names(Operation), names(Operation_manquante))

# Ajouter les colonnes manquantes à Operation avec des valeurs ""
Operation_manquante <- Operation_manquante %>%
  bind_cols(setNames(rep(list(""), length(
    colonnes_manquantes
  )), colonnes_manquantes))

Operation_manquante <- Operation_manquante %>%
  mutate_all(as.character)

# on complete les champs
Operation_manquante$FinalitePrel<-"0"
Operation_manquante$AgrePrel<-"0"
Operation_manquante$AccredPrel<-"2"

Operation <- Operation %>%
  mutate_all(as.character)

Operation <- bind_rows(Operation, Operation_manquante)

#on complete les CdRdd non renseignés d'Operation
tmp <- Analyses %>%
  select(names(Analyses)[names(Analyses) %in% names(Operation)]) %>%
  unique()

# %>%
#   bind_cols(setNames(rep(list(""), length(
#     colonnes_manquantes
#   )), colonnes_manquantes))

Operation <- merge(
  tmp,
  Operation,
  by = setdiff(names(tmp), "CdRdd"),
  all.x = TRUE,
  suffixes = c("", ".y")
) %>%
  unique()

# on complete les champs
Operation[is.na(Operation$FinalitePrel),]$FinalitePrel<-"0"
Operation[is.na(Operation$AgrePrel),]$AgrePrel<-"0"
Operation[is.na(Operation$AccredPrel),]$AccredPrel<-"2"



rm(tmp)

##### Ajout des codesRdd ResEnv à partir des CdRdd des operations #####
Corresp_Operation_RsEnv<-Operation%>%
  select(names(Operation)[names(Operation) %in% names(Res_env)],
         DatePrel,
         HeurePrel,
         CdRdd) %>%
  unique

Corresp_Operation_RsEnv$DateParEnv<-Corresp_Operation_RsEnv$DatePrel
Corresp_Operation_RsEnv$HeureParEnv<-Corresp_Operation_RsEnv$HeurePrel
Corresp_Operation_RsEnv<-Corresp_Operation_RsEnv%>%select(-c("DatePrel", "HeurePrel"))

Res_env<-merge(Res_env, Corresp_Operation_RsEnv,
                    by = intersect(names(Res_env), names(Corresp_Operation_RsEnv)),
                    all.x = TRUE,
                    suffixes = c("", ".y"))

Res_env<-Res_env%>%subset(!is.na(CdRdd))


##### correction des absences d'heure de prel #####
Analyses <- Analyses %>%
  mutate(HeurePrel = ifelse(
    substr(HeurePrel, nchar(HeurePrel), nchar(HeurePrel)) == "T",
    paste0(HeurePrel, "00:00:00"),
    HeurePrel
  ))



##### On ajoute une heure par défaut aux opération qui n'en ont pas #####
Operation <- Operation %>%
  mutate(HeurePrel = ifelse(
    substr(HeurePrel, nchar(HeurePrel), nchar(HeurePrel)) == "T",
    paste0(HeurePrel, "00:00:00"),
    HeurePrel
  ))

# Analyses dont le résultat n'est pas un nombrev alors qu'il devrait
Analyses <-
  Analyses %>% subset(!(is.na(as.numeric(RsAna)) & CdRqAna == "1"))

# Analyses dont le CdRq est 10 et le RsAna<Lq, alors RsAna = LQ
Analyses <- Analyses %>%
  mutate(RsAna = ifelse(CdRqAna == "10" &
                          RsAna < LqAna, LqAna, RsAna))

# Suppression des doublons à l'exception d'un des RsAna non renseigné
cols <-
  c(
    "CdStationMesureEauxSurface",
    "CdSupport",
    "CdFractionAnalysee",
    "CdPrelevement",
    "HeurePrel",
    "CdParametre",
    "CdRqAna",
    "CdInsituAna"
  )

# suppression des valeurs NA
Analyses <- Analyses[!(((
  duplicated(Analyses[, cols]) &
    !duplicated(Analyses[, c(cols, "RsAna")])
) |
  (
    duplicated(Analyses[, cols], fromLast = TRUE) &
      !duplicated(Analyses[, c(cols, "RsAna")], fromLast = TRUE)
  )) &
  (is.na(Analyses$RsAna))),]

# suppression des valeurs NA avec un CdRq=10
Analyses <- Analyses[!(Analyses$CdRqAna %in% c("10", "0") &
                         is.na(Analyses$RsAna)),]

# pour les lignes avec une valeur HeurePrel d'analyses qui ne serait pas dans Operation
#on remplace la valeur HeurePrel d'analyses par celle d'Operation
Analyses <- merge(
  Analyses,
  Operation %>% select(
    "CdStationMesureEauxSurface",
    "CdSupport",
    "DatePrel",
    "HeurePrel",
    "CdRdd"
  ),
  by = c(
    "CdStationMesureEauxSurface",
    "CdSupport",
    "DatePrel",
    "CdRdd"
  )
)

Analyses$HeurePrel <- Analyses$HeurePrel.x
Analyses <- Analyses %>% select(-c("HeurePrel.x", "HeurePrel.y"))

# Recherche et suppression des doublons générés par le remplacement de date avec celle d'Oper.Prel'
cols <-
  c(
    "CdStationMesureEauxSurface",
    "CdSupport",
    "CdFractionAnalysee",
    "CdPrelevement",
    "HeurePrel",
    "CdParametre",
    "CdRqAna",
    "CdInsituAna"
  )

# suppression des valeurs NA
doublons <-
  Analyses[duplicated(Analyses[, cols]) |
             duplicated(Analyses[, cols], fromLast = TRUE),]




# changement de qualification - passage en correct, validé niveau 1
summary(factor(Analyses$CdQualAna))
Analyses$CdQualAna <- "1" # correct
summary(factor(Analyses$CdStatutAna))
Analyses$CdStatutAna <- "2" # validé niveau 1

Res_env$CdQualParEnv <- "1"
Res_env$CdStatutParEn <- "2" # validé niveau 1

# remplacement des lieux de qualité
# sélection d'un lieu de qualité sur support eau par code station
lieux_qual_unique <- lieux_qual %>%
  filter(CdSupport == "3") %>%
  group_by(CdStationMesureEauxSurface) %>%
  slice(1) %>%
  ungroup() %>%
  select(CdStationMesureEauxSurface, CdPointEauxSurf)

try(Analyses <- Analyses %>% select(-CdPointEauxSurf))
Analyses <- Analyses %>%
  left_join(lieux_qual_unique, by = "CdStationMesureEauxSurface")

try(Res_env <- Res_env %>% select(-CdPointEauxSurf))
Res_env <- Res_env %>%
  left_join(lieux_qual_unique, by = "CdStationMesureEauxSurface")

try(Operation <- Operation %>% select(-CdPointEauxSurf))
Operation <- Operation %>%
  left_join(lieux_qual_unique, by = "CdStationMesureEauxSurface")

##### export de la BDD sous forme de quesu 3.0 #####

#méthode de prélèvement : par défaut Prélèvement eau brute (code 720)
code_methode_prelevement <- "720"

# # Créer le document XML
# doc <- xml_new_document(version = "1.0", encoding = "UTF-8")
# quesu <- xml_new_root("QUESU", doc = doc)

quesu <- xml_new_root("QUESU")

# Ajouter les attributs à la balise QUESU
xml_set_attr(quesu,
             "xmlns",
             "http://xml.sandre.eaufrance.fr/scenario/quesu/3")
xml_set_attr(quesu,
             "xmlns:xsi",
             "http://www.w3.org/2001/XMLSchema-instance")

# Créer l'élément Scenario
scenario <- xml_add_child(quesu, "Scenario")

# Ajouter les balises enfant de Scenario
xml_add_child(scenario, "CodeScenario", "QUESU_PHY")
xml_add_child(scenario, "VersionScenario", "3")
xml_add_child(
  scenario,
  "NomScenario",
  "Qualité des eaux superficielles continentales – Données physico-chimiques et microbiologiques"
)
xml_add_child(scenario,
              "DateCreationFichier",
              format(Sys.Date(), "%Y-%m-%d"))

# Ajouter les balises Emetteur et Destinataire sous l'élément Scenario
emetteur <- xml_add_child(scenario, "Emetteur")
xml_add_child(emetteur, "CdIntervenant", cd_emetteur) %>%
  xml_set_attr("schemeAgencyID", "SIRET")
xml_add_child(emetteur, "NomIntervenant", nom_emetteur)

destinataire <- xml_add_child(scenario, "Destinataire")
xml_add_child(destinataire, "CdIntervenant", cd_destinataire) %>%
  xml_set_attr("schemeAgencyID", "SIRET")
xml_add_child(destinataire, "NomIntervenant", nom_destinataire)

# Créer une série de balises StationMesureEauxSurface à partir des données dans le data.frame Analyses
stations <- unique(Analyses$CdStationMesureEauxSurface)

for (i in seq_along(stations)) {
  # <ResPC>
  respc <- xml_add_child(quesu, "ResPC")
  # <StationMesureEauxSurface>
  station <- xml_add_child(respc, "StationMesureEauxSurface")
  cd_station <-
    xml_add_child(station, "CdStationMesureEauxSurface", stations[i])
  xml_set_attr(cd_station, "schemeID", "STQ")
  xml_set_attr(cd_station, "schemeAgencyID", "AE")
  # <OperationPrel>
  oper <-
    Operation %>% subset(CdStationMesureEauxSurface == stations[i])
  oper$dateheure <- paste0(oper$HeurePrel)
  oper <- unique(oper)

  for (j in seq_along(oper$dateheure)) {
    operationprel <- xml_add_child(respc, "OperationPrel")
    xml_set_attr(operationprel, "Action", "A")
    xml_add_child(operationprel, "DateDebutOperationPrel", oper$DatePrel[j])

    if (oper$HeurePrel[j] != "")
    {
      xml_add_child(operationprel,
                    "HeureDebutOperationPrel",
                    paste0(oper$HeurePrel[j]))
    }

    prelevement <- xml_add_child(operationprel, "Prelevement")

    mesenv <- Res_env %>% subset(
      CdStationMesureEauxSurface == stations[i] &
        DateParEnv == oper$DatePrel[j] &
        HeureParEnv == oper$HeurePrel[j] &
        CdPreleveur == oper$CdPreleveur[j] &
        CdRdd == oper$CdRdd[j]
    )

    anal <- Analyses %>% subset(
      CdStationMesureEauxSurface == stations[i] &
        DatePrel == oper$DatePrel[j] &
        HeurePrel == oper$HeurePrel[j] &
        CdRdd == oper$CdRdd[j] &
        CdPreleveur == oper$CdPreleveur[j]
    )


    #<Prelevement>
    xml_add_child(
      prelevement,
      "CdPrelevement",
      ifelse(anal$CdPrelevement[1] != "",
             anal$CdPrelevement[1],
             "000000")
    )
    xml_add_child(prelevement, "DatePrel", oper$DatePrel[j])
    if (oper$HeurePrel[j] != "") {
      xml_add_child(prelevement,
                    "HeurePrel",
                    paste0(oper$HeurePrel[j]))
    }
    xml_add_child(prelevement, "DifficultePrel", 0) %>%
      xml_set_attr("listID", "67")
    xml_add_child(prelevement, "AccredPrel", oper$AccredPrel[j]) %>%
      xml_set_attr("listID", "333")
    xml_add_child(prelevement, "FinalitePrel", "0") %>%
      xml_set_attr("listID", "645")
    xml_add_child(prelevement, "AgrePrel", oper$AgrePrel[j])
    # methode_plt<-xml_add_child(prelevement, "MethodePrlvt")
    # xml_add_child(methode_plt, "CdMethode", code_methode_prelevement)%>%
    #   xml_set_attr("schemeID", "MET")


    pointprel <- xml_add_child(prelevement, "PointPrel")

    CdPointEauxSurf <-
      xml_add_child(pointprel,
                    "CdPointEauxSurf",
                    oper$CdPointEauxSurf[j])
    xml_set_attr(CdPointEauxSurf, "schemeID", "STM")
    xml_set_attr(CdPointEauxSurf, "schemeAgencyID", "AE")


    support <- xml_add_child(prelevement, "Support")
    xml_add_child(support, "CdSupport", oper$CdSupport[j]) %>%
      xml_set_attr("schemeID", "SUP")

    producprel <-
      xml_add_child(prelevement, "ProducteurPrelevement")
    product <-
      xml_add_child(producprel,
                    "CdIntervenant",
                    cd_emetteur)
    xml_set_attr(product, "schemeAgencyID", "SIRET")
    xml_set_attr(product, "schemeID", "INT")


    preleveur <- xml_add_child(prelevement, "Preleveur")
    prel <-
      xml_add_child(preleveur,
                    "CdIntervenant",
                    oper$CdPreleveur[j])
    xml_set_attr(prel, "schemeAgencyID", "SIRET")
    xml_set_attr(prel, "schemeID", "INT")


    # # Diviser la valeur en parties individuelles
    #  cdrdd_parts <- strsplit(anal$cdrdd[1], "/") %>% unlist()
    #
    #
    # # cdrdd <- xml_add_child(rdd, "CodeSandreRdd", anal$cdrdd[1])
    #
    # # Parcourir les parties et ajouter des éléments CodeSandreRdd
    # for (i in seq_along(cdrdd_parts)) {
    #   rdd <- xml_add_child(prelevement, "Rsx")
    #         xml_add_child(rdd, "CodeSandreRdd", cdrdd_parts[i])%>%xml_set_attr("schemeID", "RSX")
    # }


    try(oper[is.na(oper$CommentairesPrel), ]$CommentairesPrel <- "")

    if (oper$CommentairesPrel[j] != "") {
      xml_add_child(prelevement,
                    "CommentairesPrel",
                    oper$CommentairesPrel[j])
    }


    #<MesureEnvironnementale>

      for (k in seq_along(mesenv$CdParametreEnv))
      {
        if (!is.na(mesenv$CdParametreEnv[k]))
        {
        mesureenv <- xml_add_child(prelevement, "MesureEnvironnementale")

        xml_add_child(mesureenv, "DateParEnv", mesenv$DateParEnv[k])

        # si l'heure de mesure du paramètre environnemental a été renseignée alors
        # on retient la valeur, sinon on l'assimile à l'heure du prélèvement
        if (!is.null(mesenv$HeureParEnv[k]) &
            !is.null(mesenv$DateParEnv[k]))
        {
          xml_add_child(
            mesureenv,
            "HeureParEnv",
            ifelse(
              mesenv$DateParEnv[k] != "" & mesenv$HeureParEnv[k] != "",
              paste0(mesenv$HeureParEnv[k]),
              ""
            )
          )
        } else
        {
          xml_add_child(
            mesureenv,
            "HeureParEnv",
            ifelse(
              mesenv$DateParEnv[k] != "" & mesenv$HeureParEnv[k] != "",
              paste0(mesenv$HeureParEnv[k]),
              ""
            )
          )
        }

        cdparametre <- xml_add_child(mesureenv, "ParametreEnv")
        cd_param <-
          xml_add_child(cdparametre, "CdParametre", mesenv$CdParametreEnv[k])
        xml_set_attr(cd_param, "schemeID", "PAR")
        xml_set_attr(cd_param, "schemeAgencyID", "SANDRE")

        xml_add_child(mesureenv, "RsParEnv", mesenv$RsParEnv[k])

        cdunite <- xml_add_child(mesureenv, "UniteMesure")
        xml_add_child(cdunite, "CdUniteMesure", mesenv$CdUniteMesure[k]) %>%
          xml_set_attr("schemeID", "URF")

        cdrqenv <-
          xml_add_child(mesureenv, "RqParEn", mesenv$CdRqParEn[k])
        xml_set_attr(cdrqenv, "listID", "155")

        # on laisse les données environnementales comme non qualifiées
        xml_add_child(mesureenv, "QualParEnv", 4) %>%
          xml_set_attr("listID", "414")

        # on laisse le statut des paramètres environnementaux en brut
        xml_add_child(mesureenv, "StatutParEn", 1) %>%
          xml_set_attr("listID", "446")

        cdmethode <- xml_add_child(mesureenv, "Methode")
        xml_add_child(
          cdmethode,
          "CdMethode",
          ifelse(
            mesenv$CdMethodeParEnv[k] != "",
            mesenv$CdMethodeParEnv[k],
            0
          )
        ) %>%
          xml_set_attr("schemeID", "MET")


      }
    }

    #<Analyse>
    for (k in seq_along(anal$CdParametre))
    {
      analyse <- xml_add_child(prelevement, "Analyse")
      xml_add_child(analyse, "RefAnaProd", anal$RefAna[k])
      xml_add_child(analyse, "DateAna", as.character(anal$DateAna[k]))
      if (any(is.na(anal$HeureAna[k]))) {
        anal[is.na(anal$HeureAna[k]), ]$HeureAna <- ""
      }
      if (anal$HeureAna[k] != "") {
        xml_add_child(analyse,
                      "HeureAna",
                      paste0(anal$HeureAna[k]))
      }
      parametre <- xml_add_child(analyse, "Parametre")
      cdparametre <-
        xml_add_child(parametre, "CdParametre", anal$CdParametre[k])
      xml_set_attr(cdparametre, "schemeID", "PAR")
      xml_set_attr(cdparametre, "schemeAgencyID", "SANDRE")
      fractionanalysee <- xml_add_child(analyse, "FractionAnalysee")
      cdfraction <-
        xml_add_child(fractionanalysee,
                      "CdFractionAnalysee",
                      anal$CdFractionAnalysee[k])
      xml_set_attr(cdfraction, "schemeID", "FAN")
      xml_add_child(analyse, "RsAna", anal$RsAna[k])
      unitemesure <- xml_add_child(analyse, "UniteMesure")
      CdUniteMesure <-
        xml_add_child(unitemesure, "CdUniteMesure", anal$CdUniteMesure[k])
      xml_set_attr(CdUniteMesure, "schemeID", "URF")
      xml_add_child(analyse, "RqAna", anal$CdRqAna[k]) %>%
        xml_set_attr("listID", "155")
      xml_add_child(analyse,
                    "InsituAna",
                    anal$CdInsituAna[k]) %>%
        xml_set_attr("listID", "156")
      xml_add_child(analyse, "DifficulteAna", 0) %>%
        xml_set_attr("listID", "43")
      xml_add_child(analyse, "QualAna", ifelse(!is.na(anal$CdQualAna[k]),
                                               anal$CdQualAna[k],
                                               0)) %>%
        xml_set_attr("listID", "414")
      if (length(anal$CommentairesAna[k]) > 1) {
        xml_add_child(analyse, "CommentairesAna", anal$CommentairesAna[k])
      }
      xml_add_child(analyse,
                    "ComResultatAna",
                    "")
      xml_add_child(analyse, "StatutAna", anal$CdStatutAna[k]) %>%
        xml_set_attr("listID", "446")
      xml_add_child(analyse,
                    "AccreAna",
                    ifelse(anal$CdAccreAna[k] != "", anal$CdAccreAna[k], 0)) %>%
        xml_set_attr("listID", "299")
      if (!is.na(anal$LdAna[k])) {
        xml_add_child(analyse, "LDAna", anal$LdAna[k])
      }
      if (!is.na(anal$LqAna[k])) {
        xml_add_child(analyse, "LQAna", anal$LqAna[k])
      }
      if (!is.na(anal$LsAna[k])) {
        xml_add_child(analyse, "LSAna", anal$LsAna[k])
      }
      if (!is.na(anal$IncertAna[k])) {
        xml_add_child(analyse, "IncertAna", anal$IncertAna[k])
      }
      xml_add_child(analyse, "AgreAna", ifelse(anal[k,]$AgreAna, 1, 0))
      if (any(is.na(anal$CdMetFractionnement[k]))) {
        anal[is.na(anal$CdMetFractionnement[k]), ]$CdMetFractionnement <- ""
      }
      if (anal$CdMetFractionnement[k] != "") {
        metfra <- xml_add_child(analyse, "MetFractionnement")
        xml_add_child(metfra, "CdMethode", anal$CdMetFractionnement[k]) %>%
          xml_set_attr("schemeID", "MET")
      }
      if (any(is.na(anal$CdMethode[k]))) {
        anal[is.na(anal$CdMethode[k]), ]$CdMethode <- ""
      }
      if (anal$CdMethode[k] != "") {
        met <- xml_add_child(analyse, "Methode")
        xml_add_child(met, "CdMethode", anal$CdMethode[k]) %>% xml_set_attr("schemeID", "MET")
      } else
      {
        met <- xml_add_child(analyse, "Methode")
        xml_add_child(met, "CdMethode", "0") %>% xml_set_attr("schemeID", "MET")
      }

      produc <-
        xml_add_child(analyse, "Producteur")
      product <-
        xml_add_child(produc,
                      "CdIntervenant",
                      anal$CdProducteur[k])
      xml_set_attr(product, "schemeAgencyID", "SIRET")
      xml_set_attr(product, "schemeID", "INT")

      if (anal$CdLaboratoire[k] != "")
      {
        labo <-
          xml_add_child(analyse, "Laboratoire")
        product <-
          xml_add_child(labo,
                        "CdIntervenant",
                        anal$CdLaboratoire[k])
        xml_set_attr(product, "schemeAgencyID", "SIRET")
        xml_set_attr(product, "schemeID", "INT")
      }


      # Diviser la valeur en parties individuelles
      cdrdd_parts <- strsplit(anal$CdRdd[1], "/") %>% unlist()


      # Parcourir les parties et ajouter des éléments CodeSandreRdd
      for (m in seq_along(cdrdd_parts)) {
        rdd <- xml_add_child(analyse, "Rsx")
        xml_add_child(rdd, "CodeSandreRdd", cdrdd_parts[m]) %>% xml_set_attr("schemeID", "RSX")
      }
    }

  }
}

# Enregistrer le document XML dans un fichier
write_xml(quesu, paste0(nomfichier, "_quesu3.0.xml"))

tmp <- func_importe_quesu(paste0(nomfichier, "_quesu3.0.xml"))
Anal_rslt <- tmp$Analyses
Mes_Env_tmp <- tmp$Res_env

Analyses$cle <-
  paste0(Analyses$CdStationMesureEauxSurface,
         Analyses$DatePrel,
         Analyses$CdParametre)
Anal_rslt$cle <-
  paste0(Anal_rslt$CdStationMesureEauxSurface,
         Anal_rslt$DatePrel,
         Anal_rslt$CdParametre)

difference <- Analyses[!Analyses$cle %in% Anal_rslt$cle, ]
print(nrow(difference))

saveRDS(Anal_rslt, "Anal_rslt_annee.rds")
saveRDS(difference, "difference_annee.rds")

Analyses$cle2 <-
  paste0(
    Analyses$CdStationMesureEauxSurface,
    Analyses$DatePrel,
    Analyses$CdParametre,
    Analyses$RsAna,
    Analyses$CdRqAna
  )
Anal_rslt$cle2 <-
  paste0(
    Anal_rslt$CdStationMesureEauxSurface,
    Anal_rslt$DatePrel,
    Anal_rslt$CdParametre,
    Anal_rslt$RsAna,
    Anal_rslt$CdRqAna
  )

difference2 <- Analyses[!Analyses$cle2 %in% Anal_rslt$cle2, ]
difference3 <- Anal_rslt[!Anal_rslt$cle2 %in% Analyses$cle2, ]


print(nrow(difference2))
print(nrow(difference3))

tmp <-
  Anal_rslt %>% subset(cle %in% names(summary(as.factor(Anal_rslt$cle))[summary(as.factor(Anal_rslt$cle)) >
                                                                          1]))
tmp2 <-
  Anal_rslt %>% subset(cle2 %in% names(summary(as.factor(Anal_rslt$cle2))[summary(as.factor(Anal_rslt$cle2)) >
                                                                            1]))


# summary(Analyses$CdPointEauxSurf%>%as.factor)
# summary(Analyses0$CdRdd%>%as.factor)
# summary(Analyses0[Analyses0$CdStationMesureEauxSurface=="04212900",]$CdRdd%>%as.factor)


Res_env$cle <- paste0(
  Res_env$CdStationMesureEauxSurface,
  Res_env$DateParEnv,
  Res_env$HeureParEnv,
  Res_env$CdParametreEnv
)

Mes_Env_tmp$cle <- paste0(
  Mes_Env_tmp$CdStationMesureEauxSurface,
  Mes_Env_tmp$DateParEnv,
  Mes_Env_tmp$HeureParEnv,
  Mes_Env_tmp$CdParametreEnv
)

summary(Mes_Env_tmp$cle %>% as.factor)


CdRdd_DatePrel<-Operation%>%select(DatePrel, CdRdd)%>%unique()
CdRdd_DatePrel_heure_prel_staq<-Operation%>%select(DatePrel, HeurePrel, CdStationMesureEauxSurface, CdRdd)%>%unique()


