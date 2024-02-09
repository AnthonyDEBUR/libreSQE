# reexporte_fichiers_quesu3.0

library(openxlsx)
library(LibreSQE)
library(xml2)

liste_fichier<-read.xlsx("C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\synthese bancarisation fichiers.xlsx",
                   startRow = 2)

liste_fichier<-liste_fichier$xml



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

for (z in 1:length(liste_fichier)){

##### Import et mise en forme du fichier à tester #####
test <- func_importe_edilabo(paste0("C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2023\\marché et commande\\05_livrables\\",basename(liste_fichier[z]),".xml"))
Analyses <- test$Analyses
Stations <- test$Stations
Res_env <- test$Res_env
Intervenants <- test$Intervenants
Echantillon <- test$Echantillon
Operation <- test$Operation
Commemoratifs <- test$Commemoratifs
Demande <- test$Demande
rm(test)


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

# conversion des données de fraction d'extraction en numérique
#si présence "%" --> convertir en numéric en divisant par 100
# si absence --> on conserve la valeur
conversion <- ifelse(grepl("%", Analyses$rdtextraction), 0.01, 1)
# Supprimer les virgules, espaces et %
try(Analyses$rdtextraction <-
      gsub(",", ".", Analyses$rdtextraction))
try(Analyses$rdtextraction <-
      gsub("\\s", "", Analyses$rdtextraction))
try(Analyses$rdtextraction <- gsub("%", "", Analyses$rdtextraction))
try(Analyses$rdtextraction <-
      conversion * as.numeric(Analyses$rdtextraction))

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


##### Sauvegarde des données correctes en QUESU v3.0 #####

names(Analyses)
cd_emetteur <- "25440124300012"
nom_emetteur <-
  "ETABLISSEMENT PUBLIC TERRITORIAL DU BASSIN DE LA VILAINE (EPTB)"
cd_destinataire <- "22350001800013" #CD35
nom_destinataire <- "DEPARTEMENT D ILLE ET VILAINE"
# On passe les analyses en contrôlées niveau 1
Analyses$statutana <- 2

# ces codes n'étant pas renseignés on passe tous les codes points de prélèvement à 031
code_pt_prelevement <- "031"

#méthode de prélèvement : par défaut Prélèvement eau brute (code 720)
code_methode_prelevement <- "720"

# Charger la bibliothèque xml2
library(xml2)

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
# xml_set_attr(quesu,
#              "xsi:schemalocation",
#              "http://xml.sandre.eaufrance.fr/scenario/quesu/3 http://xml.sandre.eaufrance.fr/scenario/quesu/3/sandre_sc_quesu.xsd")
# xml_set_attr(
#   quesu,
#   "xsi:schemalocation",
#   "http://xml.sandre.eaufrance.fr/scenario/quesu/3.1 http://xml.sandre.eaufrance.fr/scenario/quesu/3.1/sandre_sc_quesu.xsd"
# )

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
stations <- unique(Analyses$cdstationmesureinterne)
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
    Operation %>% subset(StationPrelevement_CdStationPrelevement == stations[i])
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
      cdstationmesureinterne == stations[i] &
        dateprel == oper$DatePrel[j] &
        HeurePrel == oper$HeurePrel[j]
    )

    anal <- Analyses %>% subset(
      cdstationmesureinterne == stations[i] &
        dateprel == oper$DatePrel[j] &
        heureprel == oper$HeurePrel[j]
    )

    #<Prelevement>
    xml_add_child(
      prelevement,
      "CdPrelevement",
      ifelse(anal$codeprel[1] != "",
             anal$codeprel[1],
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
                    code_pt_prelevement)
    xml_set_attr(CdPointEauxSurf, "schemeID", "STM")
    xml_set_attr(CdPointEauxSurf, "schemeAgencyID", "AE")


    support <- xml_add_child(prelevement, "Support")
    xml_add_child(support, "CdSupport", oper$Support_CdSupport[j]) %>%
      xml_set_attr("schemeID", "SUP")

    producprel <-
      xml_add_child(prelevement, "ProducteurPrelevement")
    product <-
      xml_add_child(producprel,
                    "CdIntervenant",
                    Demande$Commanditaire_CdIntervenant)
    xml_set_attr(product, "schemeAgencyID", "SIRET")
    xml_set_attr(product, "schemeID", "INT")


    preleveur <- xml_add_child(prelevement, "Preleveur")
    prel <-
      xml_add_child(preleveur,
                    "CdIntervenant",
                    oper$Preleveur_CdIntervenant[j])
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





    if (oper$CommentairesPrel[j] != "") {
      xml_add_child(prelevement,
                    "CommentairesPrel",
                    oper$CommentairesPrel[j])
    }


    #<MesureEnvironnementale>
    for (k in seq_along(mesenv$cdparametre))
    {
      mesureenv <- xml_add_child(prelevement, "MesureEnvironnementale")
      xml_add_child(mesureenv, "DateParEnv", mesenv$dateparenv[k])

      # si l'heure de mesure du paramètre environnemental a été renseignée alors
      # on retient la valeur, sinon on l'assimile à l'heure du prélèvement
      if (!is.null(mesenv$heureparenv[k]) &
          !is.null(mesenv$dateparenv[k]))
      {
        xml_add_child(
          mesureenv,
          "HeureParEnv",
          ifelse(
            mesenv$dateparenv[k] != "" & mesenv$heureparenv[k] != "",
            paste0(mesenv$dateparenv[k],
                   "T",
                   mesenv$heureparenv[k]),
            ""
          )
        )
      } else
      {
        xml_add_child(
          mesureenv,
          "HeureParEnv",
          ifelse(
            mesenv$dateprel[k] != "" & mesenv$HeurePrel[k] != "",
            paste0(mesenv$dateprel[k],
                   "T",
                   mesenv$HeurePrel[k]),
            ""
          )
        )
      }

      cdparametre <- xml_add_child(mesureenv, "ParametreEnv")
      cd_param<-xml_add_child(cdparametre, "CdParametre", mesenv$cdparametre[k])
      xml_set_attr(cd_param, "schemeID", "PAR")
      xml_set_attr(cd_param, "schemeAgencyID", "SANDRE")

      xml_add_child(mesureenv, "RsParEnv", mesenv$rsparenv[k])

      cdunite <- xml_add_child(mesureenv, "UniteMesure")
      xml_add_child(cdunite, "CdUniteMesure", mesenv$cdunitemesure[k])%>%
        xml_set_attr("schemeID", "URF")

      cdrqenv<-xml_add_child(mesureenv, "RqParEn", mesenv$cdrqparenv[k])
      xml_set_attr(cdrqenv, "listID", "155")

      # on laisse les données environnementales comme non qualifiées
      xml_add_child(mesureenv, "QualParEnv", 4)%>%
        xml_set_attr("listID", "414")

      # on laisse le statut des paramètres environnementaux en brut
      xml_add_child(mesureenv, "StatutParEn", 1)%>%
        xml_set_attr("listID", "446")

      cdmethode <- xml_add_child(mesureenv, "Methode")
      xml_add_child(cdmethode, "CdMethode", ifelse(mesenv$Methode_CdMethode[k]!="",
                                                   mesenv$Methode_CdMethode[k],
                                                   0))%>%
        xml_set_attr("schemeID", "MET")


    }


    #<Analyse>
    for (k in seq_along(anal$cdparametre))
    {
      analyse <- xml_add_child(prelevement, "Analyse")
      xml_add_child(analyse, "RefAnaProd", anal$RefAna[k])
      xml_add_child(analyse, "DateAna", anal$dateana[k])
      if (anal$heureana[k] != "") {
        xml_add_child(analyse,
                      "HeureAna",
                      paste0(anal$dateana[k],
                             "T",
                             anal$heureana[k]))
      }
      parametre <- xml_add_child(analyse, "Parametre")
      cdparametre <-
        xml_add_child(parametre, "CdParametre", anal$cdparametre[k])
      xml_set_attr(cdparametre, "schemeID", "PAR")
      xml_set_attr(cdparametre, "schemeAgencyID", "SANDRE")
      fractionanalysee <- xml_add_child(analyse, "FractionAnalysee")
      cdfraction <-
        xml_add_child(fractionanalysee,
                      "CdFractionAnalysee",
                      anal$cdfractionanalysee[k])
      xml_set_attr(cdfraction, "schemeID", "FAN")
      xml_add_child(analyse, "RsAna", anal$rsana[k])
      unitemesure <- xml_add_child(analyse, "UniteMesure")
      CdUniteMesure <-
        xml_add_child(unitemesure, "CdUniteMesure", anal$cdunitemesure[k])
      xml_set_attr(CdUniteMesure, "schemeID", "URF")
      xml_add_child(analyse, "RqAna", anal$cdrqana[k]) %>%
        xml_set_attr("listID", "155")
      xml_add_child(analyse,
                    "InsituAna",
                    ifelse(
                      anal$insitu[k] == "In situ",
                      1,
                      ifelse(
                        anal$insitu[k] == "Laboratoire",
                        2,
                        ifelse(anal$insitu[k] == "Sans objet", 3, 0)
                      )
                    )) %>%
        xml_set_attr("listID", "156")
      xml_add_child(analyse, "DifficulteAna", 0) %>%
        xml_set_attr("listID", "43")
      if (!is.null(anal$cdqualana[k]) && !is.na(anal$cdqualana[k])) {
        cdqualana_tmp <- anal$cdqualana[k]
      } else {
        cdqualana_tmp <- "0"
      }

      xml_add_child(analyse, "QualAna", cdqualana_tmp) %>%
        xml_set_attr("listID", "414")
      if (length(anal$commentairesana[k]) > 1) {
        xml_add_child(analyse, "CommentairesAna", anal$commentairesana[k])
      }
      xml_add_child(analyse,
                    "ComResultatAna",
                    "")
      xml_add_child(analyse, "StatutAna", anal$statutana[k]) %>%
        xml_set_attr("listID", "446")
      xml_add_child(analyse,
                    "AccreAna",
                    ifelse(anal$cdaccreana[k] != "", anal$cdaccreana[k], 0)) %>%
        xml_set_attr("listID", "299")
      if (!is.na(anal$ldana[k])) {
        xml_add_child(analyse, "LDAna", anal$ldana[k])
      }
      if (!is.na(anal$lqana[k])) {
        xml_add_child(analyse, "LQAna", anal$lqana[k])
      }
      if (!is.na(anal$lsana[k])) {
        xml_add_child(analyse, "LSAna", anal$lsana[k])
      }
      if (!is.na(anal$incertitude[k])) {
        xml_add_child(analyse, "IncertAna", anal$incertitude[k])
      }
      xml_add_child(analyse, "AgreAna", ifelse(anal[k, ]$agreana == "1", 1, 0))
      if (anal$cdmetfractionnement[k] != "") {
        metfra <- xml_add_child(analyse, "MetFractionnement")
        xml_add_child(metfra, "CdMethode", anal$cdmetfractionnement[k]) %>%
          xml_set_attr("schemeID", "MET")
      }
      if (anal$cdmethode[k] != "") {
        met <- xml_add_child(analyse, "Methode")
        xml_add_child(met, "CdMethode", anal$cdmethode[k]) %>% xml_set_attr("schemeID", "MET")
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
                      anal$cdproducteur[k])
      xml_set_attr(product, "schemeAgencyID", "SIRET")
      xml_set_attr(product, "schemeID", "INT")

      if (anal$cdlaboratoire[k] != "")
      {
        labo <-
          xml_add_child(analyse, "Laboratoire")
        product <-
          xml_add_child(labo,
                        "CdIntervenant",
                        anal$cdlaboratoire[k])
        xml_set_attr(product, "schemeAgencyID", "SIRET")
        xml_set_attr(product, "schemeID", "INT")
      }


      # Diviser la valeur en parties individuelles
      cdrdd_parts <- strsplit(anal$cdrdd[1], "/") %>% unlist()


      # Parcourir les parties et ajouter des éléments CodeSandreRdd
      for (i in seq_along(cdrdd_parts)) {
        rdd <- xml_add_child(analyse, "Rsx")
        xml_add_child(rdd, "CodeSandreRdd", cdrdd_parts[i]) %>% xml_set_attr("schemeID", "RSX")
      }


    }
  }
}

# Enregistrer le document XML dans un fichier
write_xml(quesu, paste0(basename(liste_fichier[z]), "_quesu3.0.xml"))


}
