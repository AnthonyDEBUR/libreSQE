##### Export des résultats 2023 en QUESU v3 #####
library(LibreSQE)
library(tidyverse)
library(xml2)

nomfichier<-paste0(Sys.Date(),"_Eaux_et_Vilaine_2023")
cd_emetteur <- "25440124300012"
nom_emetteur <-
  "ETABLISSEMENT PUBLIC TERRITORIAL DU BASSIN DE LA VILAINE (EPTB)"
cd_destinataire <- "22350001800013" #CD35
nom_destinataire <- "DEPARTEMENT D ILLE ET VILAINE"

# lecture du fichier de syntèse E&V
Analyses <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")
Res_env <- readRDS("~/R_Anthony/Naiades/bdd_locale/cond_env_Eaux_Vilaine.rds")
Operation <- readRDS("~/R_Anthony/Naiades/bdd_locale/operations_Eaux_Vilaine.rds")

#bdd lieux de qualité
lieux_qual <- readRDS("C:/workspace/LibreSQE/lieux_qual_1970_au_20240126.rds")

# on ne conserve que l'année 2023
annee<-"2023"
date_debut<-as.Date(paste0(annee,"-01-01"))
date_fin<-as.Date(paste0(annee,"-12-31"))

Analyses<-Analyses%>%subset(DatePrel>=date_debut & DatePrel<=date_fin)%>%subset(CdStationMesureEauxSurface=="04376006")
Res_env<-Res_env%>%subset(DateParEnv>=date_debut & DateParEnv<=date_fin)%>%subset(CdStationMesureEauxSurface=="04376006")
Operation<-Operation%>%subset(DatePrel>=date_debut & DatePrel<=date_fin)%>%subset(CdStationMesureEauxSurface=="04376006")

# changement de qualification - passage en correct, validé niveau 1
summary(factor(Analyses$CdQualAna))
Analyses$CdQualAna<-"1" # correct
summary(factor(Analyses$CdStatutAna))
Analyses$CdStatutAna<-"2" # validé niveau 1

Res_env$CdQualParEnv<-"1"
Res_env$CdStatutParEn<-"2" # validé niveau 1

# remplacement des lieux de qualité
# sélection d'un lieu de qualité sur support eau par code station
lieux_qual_unique<- lieux_qual %>%
  filter(CdSupport == "3") %>%
  group_by(CdStationMesureEauxSurface) %>%
  slice(1) %>%
  ungroup()%>%
  select(CdStationMesureEauxSurface, CdPointEauxSurf)

try(Analyses<-Analyses%>%select(-CdPointEauxSurf))
Analyses<-Analyses%>%
  left_join(lieux_qual_unique, by="CdStationMesureEauxSurface")

try(Res_env<-Res_env%>%select(-CdPointEauxSurf))
Res_env<-Res_env%>%
  left_join(lieux_qual_unique, by="CdStationMesureEauxSurface")

try(Operation<-Operation%>%select(-CdPointEauxSurf))
Operation<-Operation%>%
  left_join(lieux_qual_unique, by="CdStationMesureEauxSurface")

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
  oper$dateheure <- paste0(oper$DatePrel, oper$HeurePrel)
  for (j in seq_along(unique(oper$dateheure))) {
    operationprel <- xml_add_child(respc, "OperationPrel")
    xml_set_attr(operationprel, "Action", "A")
    xml_add_child(operationprel, "DateDebutOperationPrel", oper$DatePrel[j])
    xml_add_child(
      operationprel,
      "HeureDebutOperationPrel",
      paste0(oper$DatePrel[j],
             "T",
             oper$HeurePrel[j])
    )



    prelevement <- xml_add_child(operationprel, "Prelevement")

    mesenv <- Res_env %>% subset(
      CdStationMesureEauxSurface == stations[i] &
        DateParEnv == oper$DatePrel[j] &
        HeureParEnv == oper$HeurePrel[j]
    )

    anal <- Analyses %>% subset(
      CdStationMesureEauxSurface == stations[i] &
        DatePrel == oper$DatePrel[j] &
        HeurePrel == oper$HeurePrel[j]
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
    xml_add_child(prelevement,
                  "HeurePrel",
                  paste0(oper$DatePrel[j], "T", oper$HeurePrel[j]))
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


    try(oper[is.na(oper$CommentairesPrel),]$CommentairesPrel<-"")

    if (oper$CommentairesPrel[j] != "") {
      xml_add_child(prelevement,
                    "CommentairesPrel",
                    oper$CommentairesPrel[j])
    }


    #<MesureEnvironnementale>
    for (k in seq_along(mesenv$CdParametreEnv))
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
            paste0(mesenv$DateParEnv[k],
                   "T",
                   mesenv$HeureParEnv[k]),
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
            paste0(mesenv$DateParEnv[k],
                   "T",
                   mesenv$HeureParEnv[k]),
            ""
          )
        )
      }

      cdparametre <- xml_add_child(mesureenv, "ParametreEnv")
      cd_param<-xml_add_child(cdparametre, "CdParametre", mesenv$CdParametreEnv[k])
      xml_set_attr(cd_param, "schemeID", "PAR")
      xml_set_attr(cd_param, "schemeAgencyID", "SANDRE")

      xml_add_child(mesureenv, "RsParEnv", mesenv$RsParEnv[k])

      cdunite <- xml_add_child(mesureenv, "UniteMesure")
      xml_add_child(cdunite, "CdUniteMesure", mesenv$CdUniteMesure[k])%>%
        xml_set_attr("schemeID", "URF")

      cdrqenv<-xml_add_child(mesureenv, "RqParEn", mesenv$CdRqParEn[k])
      xml_set_attr(cdrqenv, "listID", "155")

      # on laisse les données environnementales comme non qualifiées
      xml_add_child(mesureenv, "QualParEnv", 4)%>%
        xml_set_attr("listID", "414")

      # on laisse le statut des paramètres environnementaux en brut
      xml_add_child(mesureenv, "StatutParEn", 1)%>%
        xml_set_attr("listID", "446")

      cdmethode <- xml_add_child(mesureenv, "Methode")
      xml_add_child(cdmethode, "CdMethode", ifelse(mesenv$CdMethodeParEnv[k]!="",
                                                   mesenv$CdMethodeParEnv[k],
                                                   0))%>%
        xml_set_attr("schemeID", "MET")


    }


    #<Analyse>
    for (k in seq_along(anal$CdParametre))
    {
      analyse <- xml_add_child(prelevement, "Analyse")
      xml_add_child(analyse, "RefAnaProd", anal$RefAna[k])
      xml_add_child(analyse, "DateAna", as.character(anal$DateAna[k]))
      if(any(is.na(anal$HeureAna[k]))){anal[is.na(anal$HeureAna[k]),]$HeureAna<-""}
      if (anal$HeureAna[k] != "") {
        xml_add_child(analyse,
                      "HeureAna",
                      paste0(anal$DateAna[k],
                             "T",
                             anal$HeureAna[k]))
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
                    anal$CdInsituAna[k]
                      ) %>%
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
      xml_add_child(analyse, "AgreAna", ifelse(anal[k, ]$AgreAna, 1, 0))
      if(any(is.na(anal$CdMetFractionnement[k]))){anal[is.na(anal$CdMetFractionnement[k]),]$CdMetFractionnement<-""}
      if (anal$CdMetFractionnement[k] != "") {
        metfra <- xml_add_child(analyse, "MetFractionnement")
        xml_add_child(metfra, "CdMethode", anal$CdMetFractionnement[k]) %>%
          xml_set_attr("schemeID", "MET")
      }
      if(any(is.na(anal$CdMethode[k]))){anal[is.na(anal$CdMethode[k]),]$CdMethode<-""}
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

tmp<-func_importe_quesu(paste0(nomfichier, "_quesu3.0.xml"))
Anal_rslt<-tmp$Analyses
Mes_Env_tmp<-tmp$Res_env

Analyses$cle<-paste0(Analyses$CdStationMesureEauxSurface, Analyses$DatePrel, Analyses$CdParametre)
Anal_rslt$cle<-paste0(Anal_rslt$CdStationMesureEauxSurface, Anal_rslt$DatePrel, Anal_rslt$CdParametre)

difference<-Analyses[!Analyses$cle%in%Anal_rslt$cle,]
