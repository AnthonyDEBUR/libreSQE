#' func_importe_edilabo
#'
#' @description lecture de fichiers SANDRE xml aux formats d'échanges EDILABO 1 ou 1.1
#'
#' @param fichier : chemin d'accès au fichier à ouvrir
#' @param stations_a_traiter : paramètre optionnel listant les codes SANDRE des stations à importer
#'
#' @return liste avec 4 éléments : Analyses qui contient la liste des analyses importées,
#' Echantillon qui contient les caractéristiques de transport des échantillons,
#' Res_env qui contient les informations environnementales du prélèvement,
#  Operation qui détaille le contenu des opérations
#'
#' @export
func_importe_edilabo <- function(fichier, stations_a_traiter = NULL) {

  CdStationPrelevement<-StationPrelevement_CdStationPrelevement<-NULL


  if (!("character" %in% class(fichier))) {
    stop(
      "func_importe_edilabo : le paramètre fichier doit être un
                                          character spécifiant le chemin d'accès au fichier à importer"
    )
  }
  if (substr(fichier, nchar(fichier) - 3, nchar(fichier)) != ".xml") {
    stop("func_importe_edilabo : le fichier \u00e0 importer doit \u00eatre au format xml")
  }
  if(!(is.null(stations_a_traiter) |
       is.character(stations_a_traiter) |
       is.factor(stations_a_traiter))){stop("func_importe_quesu : stations_a_traiter
                                          doit être de type character ou factor")}
  n_lines <- 50000  # nombre de lignes à traiter à chaque itération
  file_in <-
    file(fichier, "r")  # ouverture du fichier en lecture

  ##### Initialisation des variables et tableaux de sortie #####
  bloc <- character(0)
  first_passage <- TRUE
  df_out_analyses<-df_out_analyses0<-data.frame()
  df_out_echant<-df_out_echant0<-data.frame()
  df_out_mesureenv<-df_out_mesureenv0<-data.frame()
  df_out_intervenant<-df_out_intervenant0<-data.frame()
  df_out_prelevement<-df_out_prelevement0<-data.frame()
  df_out_commemoratif<-df_out_commemoratif0<-data.frame()
  df_out_demande<-df_out_demande0<-data.frame()

#####sous fonctions #####
  ss_func_ajoute_colonne<-function(df, nom_colonne_valeur, nom_colonne_nb){
    lapply(seq_along(df[[nom_colonne_valeur]]),
           function(x){rep(df[[nom_colonne_valeur]][x],
                           df[[nom_colonne_nb]][x])})%>%unlist()
  }



  # fonction pour ajouter les colonnes de chaque commémoratif a df_out_prelevement
  create_new_cols <- function(df_out_prelevement, df_out_commemoratif_prel) {
    # obtenir les noms de colonnes distinctes dans df2
    col_names <- unique(df_out_commemoratif_prel$LbCommemoratif)
    col_values <- unique(df_out_commemoratif_prel$CdCommemoratif)
    if(length(col_names)!=length(col_values) | col_names==""){col_names <- unique(paste0("commemo_",df_out_commemoratif_prel$CdCommemoratif))}


    # initialiser une liste pour stocker les nouvelles colonnes
    new_cols <- list()

    # pour chaque nom de colonne, créer une nouvelle colonne dans df
    for (col in seq_along(col_values)) {
      # obtenir les valeurs correspondantes de df2
      values <- df_out_commemoratif_prel$ValCommemoratif[df_out_commemoratif_prel$CdCommemoratif == col_values[col]]

      # créer la nouvelle colonne
      new_col <- ifelse(df_out_prelevement$nb_Commemoratif == 1, values, NA)

      # ajouter la nouvelle colonne à la liste
      new_cols[[col_names[col]]] <- new_col
    }

    # combiner les nouvelles colonnes avec df
    new_df <- cbind(df_out_prelevement, new_cols)

    return(new_df)
  }


  ##### Traitement du fichier #####

  while (length(lines <-
                readLines(file_in, n = n_lines, warn = FALSE)) > 0) {
      # on fusionne les lignes en un seul bloc de character
    bloc <- c(bloc, lines)

    ##### si on est dans le premier passage, on recherche le scenario SANDRE #####
    if (first_passage) {
      # on extrait le bloc à traiter
      indice_debut <- grep("<CodeScenario>", bloc)[1]
      indice_fin <- grep("</CodeScenario>", bloc)[1]
      bloc_station <-
        bloc[indice_debut:indice_fin] %>% paste(collapse = "")
      CodeScenario <-
        gsub(".*<CodeScenario>(.+)</CodeScenario>*",
             "\\1",
             bloc_station)

      # dans bloc, suppression des lignes traitées
      bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                           seq(1:length(bloc)) > indice_fin)]

      indice_debut <- grep("<VersionScenario>", bloc)[1]
      indice_fin <- grep("</VersionScenario>", bloc)[1]
      bloc_station <-
        bloc[indice_debut:indice_fin] %>% paste(collapse = "")
      versionscenario <-
        gsub(".*<VersionScenario>(.+)</VersionScenario>*",
             "\\1",
             bloc_station)

      if (!paste0(CodeScenario) %in%
          c("LABO_DEST")) {
        stop(paste0("Sc\u00e9nario d\'\u00e9change ",CodeScenario,"
                    non support\u00e9"))
      }

      if (!paste0(CodeScenario, versionscenario) %in%
          c("LABO_DEST1", "LABO_DEST1.1")) {
        warning(paste0("L'import de fichier EDILABO n'a été testé que sur les
                       scénarios LABO_DEST v1 ou v1.1. La version importée est ",
                       versionscenario,
                       " vérifiez que l'import est cohérent avec les données
                       transmises."))

        # dans bloc, suppression des lignes traitées
        bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                             seq(1:length(bloc)) > indice_fin)]
      }

      first_passage <- FALSE
    }

    #####Traitement des balises intervenants #####
    # servira à affecter un nom d'intervenant face à un code intervenant et à vérifier la cohérence entre les 2 informations
    # on regarde si on trouve la balise </Intervenants> et dans ce cas on traite
    # les éléments entre les balises Intervenants et /Intervenants

    if (!all(is.na(stringr::str_locate(bloc, "</Intervenant>")[, 1])))
    {
      # on extrait le bloc à traiter
      indice_debut <- grep("<Intervenant>", bloc)[1]
      indice_fin <- grep("</Intervenant>", bloc)[length(grep("</Intervenant>", bloc))]

      bloc_intervenant <- bloc[indice_debut:indice_fin]

      # Lecture du contenu XML
      xml_content <- '<Root>'  # On doit ajouter une racine au contenu XML pour pouvoir le lire
      xml_content <- paste(xml_content, paste(bloc_intervenant, collapse=""), sep='')
      xml_content <- paste(xml_content, '</Root>', sep = '')  # On ferme la balise racine
      doc <- xml2::read_xml(xml_content)

      # extraire les balises Intervenant
      intervenants <- xml2::xml_find_all(doc, "//Intervenant")

      df_out_intervenant<-ss_func_extrat_from_xml(xml_nodes=intervenants,
                                        values=list("CdIntervenant",
                                                 "NomIntervenant",
                                                 "MnIntervenant"),
                                        scheme=list(list(c("CdIntervenant"),
                                                          "schemeAgencyID"))
                                        )

      # dans bloc, suppression des lignes traitées
      bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                           seq(1:length(bloc)) > indice_fin)]

    }

    #####Traitement des balises demande #####
    # servira à affecter les commémoratifs liés à demande

    if (!all(is.na(stringr::str_locate(bloc, "</Demande>")[, 1])))
    {
      # on extrait le bloc à traiter
      indice_debut <- grep("<Demande>", bloc)[1]
      indice_fin <- grep("</Demande>", bloc)[length(grep("</Demande>", bloc))]

      bloc_intervenant <- bloc[indice_debut:indice_fin]

      # Lecture du contenu XML
      xml_content <- '<Root>'  # On doit ajouter une racine au contenu XML pour pouvoir le lire
      xml_content <- paste(xml_content, paste(bloc_intervenant, collapse=""), sep='')
      xml_content <- paste(xml_content, '</Root>', sep = '')  # On ferme la balise racine
      doc <- xml2::read_xml(xml_content)

      # extraire les balises Demande
      intervenants <- xml2::xml_find_all(doc, "//Demande")

      df_out_demande<-ss_func_extrat_from_xml(xml_nodes=intervenants,
                                              values=list("CdDemandeCommanditaire",
                                                          c("Commanditaire", "CdIntervenant"),
                                                          "CdDemandePrestataire",
                                                          c("Prestataire", "CdIntervenant"),
                                                          "TypeDemande",
                                                          "DateDemande",
                                                          "LbDemande",
                                                          "ReferenceMarche",
                                                          "CommentairesCommanditaire",
                                                          c("DestinataireRsAna","CdIntervenant"),
                                                          c("Commemoratif", "CdCommemoratif")),
                                                  count=list("Commemoratif")
      )

      # recupération des résultats de Commemoratif directement sous demande

      # extraire les noeuds <Commemoratif> directement sous les noeuds <Demande>
      b1_nodes <- xml2::xml_find_all(doc, "//Demande/Commemoratif")
      if(length(b1_nodes)>0)
      {df_out_commemoratif<-ss_func_extrat_from_xml(xml_nodes=b1_nodes,
                                                   values=list("CdCommemoratif",
                                                               "LbCommemoratif",
                                                               "DsCommemoratif",
                                                               "ValCommemoratif"))}

      }




    #####Traitement des balises <Prelevement> #####
    # servira à affecter un nom d'intervenant face à un code intervenant et à vérifier la cohérence entre les 2 informations
    # on regarde si on trouve la balise </Prelevement> et dans ce cas on traite
    # les éléments entre les balises Prelevement et /Prelevement

    if(!all(is.na(stringr::str_locate(bloc, "</Prelevement>")[,1])))
    {
      # on extrait le bloc à traiter
      indice_debut <- grep("<Prelevement>", bloc)[1]
      indice_fin <- grep("</Prelevement>", bloc)[length(grep("</Prelevement>", bloc))]

      bloc_intervenant <- bloc[indice_debut:indice_fin]

      # Lecture du contenu XML
      xml_content <- '<Root>'  # On doit ajouter une racine au contenu XML pour pouvoir le lire
      xml_content <- paste(xml_content, paste(bloc_intervenant, collapse=""), sep='')
      xml_content <- paste(xml_content, '</Root>', sep = '')  # On ferme la balise racine
      doc <- xml2::read_xml(xml_content)

      # extraire les balises Prelevement
      intervenants <- xml2::xml_find_all(doc, "//Prelevement")
      if(length(intervenants)>0){
      df_out_prelevement<-ss_func_extrat_from_xml(xml_nodes=intervenants,
                                                  values=list(c("StationPrelevement","CdStationPrelevement"),
                                                              "CdPrelevement",
                                                              "ReferencePrel",
                                                              "DatePrel",
                                                              "HeurePrel",
                                                             "DureePrel",
                                                              "LocalExactePrel",
                                                              "ProfondeurPrel",
                                                              "ZoneVerticaleProspectee",
                                                              "CoordXPrel",
                                                              "CoordYPrel",
                                                              c("Support", "CdSupport"),
                                                              c("Support", "LbSupport"),
                                                              c("MethodePrel","CdMethode"),
                                                              c("MethodePrel","NomMethode"),
                                                              c("Preleveur","CdIntervenant"),
                                                             c("Payeur","CdIntervenant"),
                                                              "NumeroOrdrePrelevement",
                                                              "ConformitePrel",
                                                              "AccredPrel",
                                                              "PrelSousReserve",
                                                              "RealisePrel",
                                                              "AgrePrel",
                                                             "CommentairesPrel",
                                                             c("Commemoratif", "CdCommemoratif")
                                                              ),
                                                  scheme=list(list(c("CdPrelevement"),
                                                                   "schemeAgencyID"),
                                                              list(c("CdStationPrelevement"),
                                                                   "schemeAgencyID")),
                                                  count=list("MesureEnvironnementale",
                                                             "Echantillon",
                                                             "Commemoratif"))


      # extraire les noeuds <Commemoratif> directement sous les noeuds <Demande>
      b1_nodes <- xml2::xml_find_all(doc, "//Prelevement/Commemoratif")
      if(length(b1_nodes)>0){
      df_out_commemoratif_prel<-ss_func_extrat_from_xml(xml_nodes=b1_nodes,
                                                   values=list("CdCommemoratif",
                                                               "LbCommemoratif",
                                                               "DsCommemoratif",
                                                               "ValCommemoratif"))

      df_out_prelevement<-create_new_cols(df_out_prelevement, df_out_commemoratif_prel)}


      }else{df_out_prelevement<-data.frame()}



      # recupération des mesures environnementales
      mesenv <- xml2::xml_find_all(doc, "//MesureEnvironnementale")
      if(length(mesenv)>0)
      {df_out_mesureenv<-ss_func_extrat_from_xml(xml_nodes=mesenv,
                                                  values=list("RsParEnv",
                                                              "RqParEnv",
                                                             "DateParEnv",
                                                              c("Parametre", "CdParametre"),
                                                              c("Parametre", "NomParametre"),
                                                              c("Methode", "CdMethode"),
                                                              c("Methode", "NomMethode"),
                                                              c("UniteReference","CdUniteReference"),
                                                              c("UniteReference","LbUniteReference"),
                                                              c("UniteReference","SymUniteReference")
                                                  )
      )

      df_out_mesureenv$CdStationPrelevement<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                    "StationPrelevement_CdStationPrelevement",
                                                                    "nb_MesureEnvironnementale")

      df_out_mesureenv$DatePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                    "DatePrel",
                                                                    "nb_MesureEnvironnementale")

      df_out_mesureenv$HeurePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                        "HeurePrel",
                                                        "nb_MesureEnvironnementale")


      df_out_mesureenv$CdSupport<-ss_func_ajoute_colonne(df_out_prelevement,
                                                         "Support_CdSupport",
                                                         "nb_MesureEnvironnementale")

      df_out_mesureenv$ProfondeurPrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                         "ProfondeurPrel",
                                                         "nb_MesureEnvironnementale")

      df_out_mesureenv$ZoneVerticaleProspectee<-ss_func_ajoute_colonne(df_out_prelevement,
                                                              "ZoneVerticaleProspectee",
                                                              "nb_MesureEnvironnementale")

      df_out_mesureenv$MethodePrel_CdMethode<-ss_func_ajoute_colonne(df_out_prelevement,
                                                              "MethodePrel_CdMethode",
                                                              "nb_MesureEnvironnementale")

      df_out_mesureenv$CdPreleveur<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                     "Preleveur_CdIntervenant",
                                                                     "nb_MesureEnvironnementale")

      df_out_mesureenv$ConformitePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "ConformitePrel",
                                                           "nb_MesureEnvironnementale")

      df_out_mesureenv$AccredPrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "AccredPrel",
                                                           "nb_MesureEnvironnementale")

      df_out_mesureenv$PrelSousReserve<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "PrelSousReserve",
                                                           "nb_MesureEnvironnementale")

      df_out_mesureenv$RealisePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "RealisePrel",
                                                           "nb_MesureEnvironnementale")

      df_out_mesureenv$AgrePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "AgrePrel",
                                                           "nb_MesureEnvironnementale")

      df_out_mesureenv$CdPrelevement<-ss_func_ajoute_colonne(df_out_prelevement,
                                                        "CdPrelevement",
                                                        "nb_MesureEnvironnementale")
      }else{df_out_mesureenv<-data.frame()}


      # recupération des résultats d'Echantillon
      mesenv <- xml2::xml_find_all(doc, "//Echantillon")
      if(length(mesenv)>0){
      df_out_echant<-ss_func_extrat_from_xml(xml_nodes=mesenv,
                                                values=list("RefEchantillonCommanditaire",
                                                            "RefEchantillonPrel",
                                                            "RefEchantillonLabo",
                                                            "AcceptabiliteEchant",
                                                            "DateReceptionEchant",
                                                            "HeureReceptionEchant",
                                                            "CommentairesEchant",
                                                            c("MethodeTransport", "CdMethode"),
                                                            c("MethodeTransport", "NomMethode"),
                                                            "CompletEchant",
                                                            c("Methode", "NomMethode"),
                                                            c("UniteReference","CdUniteReference"),
                                                            c("UniteReference","LbUniteReference"),
                                                            c("UniteReference","SymUniteReference"),
                                                            c("Laboratoire","CdIntervenant"),
                                                            c("Payeur", "CdIntervenant"),
                                                            c("Commemoratif", "CdCommemoratif")
                                                ),
                                                count=list("Analyse",
                                                          "Commemoratif")
      )

      df_out_echant$CdStationPrelevement<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                   "StationPrelevement_CdStationPrelevement",
                                                                   "nb_Echantillon")

      df_out_echant$DatePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                 "DatePrel",
                                                                 "nb_Echantillon")

      df_out_echant$HeurePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                 "HeurePrel",
                                                                 "nb_Echantillon")

      df_out_echant$CdSupport<-ss_func_ajoute_colonne(df_out_prelevement,
                                                      "Support_CdSupport",
                                                      "nb_Echantillon")

      df_out_echant$LbSupport<-ss_func_ajoute_colonne(df_out_prelevement,
                                                         "Support_LbSupport",
                                                         "nb_Echantillon")

      df_out_echant$ProfondeurPrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                              "ProfondeurPrel",
                                                              "nb_Echantillon")

      df_out_echant$ZoneVerticaleProspectee<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                       "ZoneVerticaleProspectee",
                                                                       "nb_Echantillon")

      df_out_echant$MethodePrel_CdMethode<-ss_func_ajoute_colonne(df_out_prelevement,
                                                                     "MethodePrel_CdMethode",
                                                                     "nb_Echantillon")

      df_out_echant$CdPreleveur<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "Preleveur_CdIntervenant",
                                                           "nb_Echantillon")

      df_out_echant$ConformitePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                              "ConformitePrel",
                                                              "nb_Echantillon")

      df_out_echant$AccredPrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                          "AccredPrel",
                                                          "nb_Echantillon")

      df_out_echant$PrelSousReserve<-ss_func_ajoute_colonne(df_out_prelevement,
                                                               "PrelSousReserve",
                                                               "nb_Echantillon")

      df_out_echant$RealisePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                           "RealisePrel",
                                                           "nb_Echantillon")

      df_out_echant$AgrePrel<-ss_func_ajoute_colonne(df_out_prelevement,
                                                        "AgrePrel",
                                                        "nb_Echantillon")

      df_out_echant$CdPrelevement<-ss_func_ajoute_colonne(df_out_prelevement,
                                                     "CdPrelevement",
                                                     "nb_Echantillon")

      df_out_echant$commemo_123<-ss_func_ajoute_colonne(df_out_prelevement,
                                                     "commemo_123",
                                                     "nb_Echantillon")


      # extraire les noeuds <Commemoratif> directement sous les noeuds <Demande>
      b1_nodes <- xml2::xml_find_all(doc, "//Prelevement/Echantillon/Commemoratif")
      if(length(b1_nodes)>0){
      df_out_commemoratif_prel<-ss_func_extrat_from_xml(xml_nodes=b1_nodes,
                                                        values=list("CdCommemoratif",
                                                                    "LbCommemoratif",
                                                                    "DsCommemoratif",
                                                                    "ValCommemoratif"))

      df_out_echant<-create_new_cols(df_out_echant, df_out_commemoratif_prel)}

      }else{df_out_echant<-data.frame()}


      # recupération des résultats d'analyses
      mesenv <- xml2::xml_find_all(doc, "//Analyse")
      if(length(mesenv)>0)
     {
        df_out_analyses<-ss_func_extrat_from_xml(xml_nodes=mesenv,
                                                values=list("RefLaboAna",
                                                            "DateAna",
                                                            "HeureAna",
                                                            "RsAna",
                                                            "RqAna",
                                                            "LDAna",
                                                            "LQAna",
                                                            "LSAna",
                                                            "AccreAna",
                                                            "AgreAna",
                                                            "ConfirAna",
                                                            "ReserveAna",
                                                            "IncertAna",
                                                            "IncertTypeAna",
                                                            "IncertElarAna",
                                                            "RefAna",
                                                            "InsituAna",
                                                            "RdtExtraction",
                                                            "CommentairesAna",
                                                            c("Parametre", "CdParametre"),
                                                            c("Parametre", "NomParametre"),
                                                            c("FractionAnalysee","CdFractionAnalysee"),
                                                            c("FractionAnalysee","LbFractionAnalysee"),
                                                            c("Methode", "CdMethode"),
                                                            c("Methode", "NomMethode"),
                                                            c("UniteReference","CdUniteReference"),
                                                            c("UniteReference","LbUniteReference"),
                                                            c("UniteReference","SymUniteReference"),
                                                            c("Laboratoire", "CdIntervenant"),
                                                            c("Payeur", "CdIntervenant"),
                                                            c("MethFractionnement", "CdMethode"),
                                                            c("MethFractionnement", "NomMethode"),
                                                            c("MethExtraction", "CdMethode"),
                                                            c("MethExtraction", "NomMethode"),
                                                            c("Solvant", "CdParametre"),
                                                            c("Solvant", "NomParametre"),
                                                            "VolumeFiltre",
                                                            c("Commemoratif", "CdCommemoratif")
                                                            ),
                                                count=list("Commemoratif")
                                                  )




     df_out_analyses$CdStationPrelevement<-ss_func_ajoute_colonne(df_out_echant,
                                                                   "CdStationPrelevement",
                                                                   "nb_Analyse")

     df_out_analyses$CdPrelevement<-ss_func_ajoute_colonne(df_out_echant,
                                                                  "CdPrelevement",
                                                                  "nb_Analyse")

     df_out_analyses$DatePrel<-ss_func_ajoute_colonne(df_out_echant,
                                                                  "DatePrel",
                                                                  "nb_Analyse")

     df_out_analyses$HeurePrel<-ss_func_ajoute_colonne(df_out_echant,
                                                                  "HeurePrel",
                                                                  "nb_Analyse")

     df_out_analyses$CdSupport<-ss_func_ajoute_colonne(df_out_echant,
                                                       "CdSupport",
                                                       "nb_Analyse")

     df_out_analyses$LbSupport<-ss_func_ajoute_colonne(df_out_echant,
                                                        "LbSupport",
                                                        "nb_Analyse")

     df_out_analyses$ProfondeurPrel<-ss_func_ajoute_colonne(df_out_echant,
                                                             "ProfondeurPrel",
                                                             "nb_Analyse")

     df_out_analyses$ZoneVerticaleProspectee<-ss_func_ajoute_colonne(df_out_echant,
                                                                      "ZoneVerticaleProspectee",
                                                                      "nb_Analyse")

     df_out_analyses$MethodePrel_CdMethode<-ss_func_ajoute_colonne(df_out_echant,
                                                                    "MethodePrel_CdMethode",
                                                                    "nb_Analyse")

     df_out_analyses$CdPreleveur<-ss_func_ajoute_colonne(df_out_echant,
                                                          "CdPreleveur",
                                                          "nb_Analyse")

     df_out_analyses$ConformitePrel<-ss_func_ajoute_colonne(df_out_echant,
                                                             "ConformitePrel",
                                                             "nb_Analyse")

     df_out_analyses$AccredPrel<-ss_func_ajoute_colonne(df_out_echant,
                                                         "AccredPrel",
                                                         "nb_Analyse")

     df_out_analyses$PrelSousReserve<-ss_func_ajoute_colonne(df_out_echant,
                                                              "PrelSousReserve",
                                                              "nb_Analyse")

     df_out_analyses$RealisePrel<-ss_func_ajoute_colonne(df_out_echant,
                                                          "RealisePrel",
                                                          "nb_Analyse")

     df_out_analyses$AgrePrel<-ss_func_ajoute_colonne(df_out_echant,
                                                       "AgrePrel",
                                                       "nb_Analyse")

     df_out_analyses$RefEchantillonCommanditaire<-ss_func_ajoute_colonne(df_out_echant,
                                                            "RefEchantillonCommanditaire",
                                                            "nb_Analyse")

     df_out_analyses$RefEchantillonPrel<-ss_func_ajoute_colonne(df_out_echant,
                                                        "RefEchantillonPrel",
                                                        "nb_Analyse")

     df_out_analyses$RefEchantillonLabo<-ss_func_ajoute_colonne(df_out_echant,
                                                             "RefEchantillonLabo",
                                                             "nb_Analyse")

     df_out_analyses$AcceptabiliteEchant<-ss_func_ajoute_colonne(df_out_echant,
                                                         "AcceptabiliteEchant",
                                                         "nb_Analyse")

     df_out_analyses$DateReceptionEchant<-ss_func_ajoute_colonne(df_out_echant,
                                                      "DateReceptionEchant",
                                                      "nb_Analyse")

      df_out_analyses$HeureReceptionEchant<-ss_func_ajoute_colonne(df_out_echant,
                                                                         "HeureReceptionEchant",
                                                                         "nb_Analyse")

     df_out_analyses$CommentairesEchant<-ss_func_ajoute_colonne(df_out_echant,
                                                                "CommentairesEchant",
                                                                "nb_Analyse")

     df_out_analyses$MethodeTransport_CdMethode<-ss_func_ajoute_colonne(df_out_echant,
                                                                "MethodeTransport_CdMethode",
                                                                "nb_Analyse")

     df_out_analyses$Methode_NomMethode<-ss_func_ajoute_colonne(df_out_echant,
                                                                 "Methode_NomMethode",
                                                                 "nb_Analyse")

     df_out_analyses$Laboratoire_CdIntervenant<-ss_func_ajoute_colonne(df_out_echant,
                                                                 "Laboratoire_CdIntervenant",
                                                                 "nb_Analyse")

     df_out_analyses$CdPreleveur<-ss_func_ajoute_colonne(df_out_echant,
                                                                       "CdPreleveur",
                                                                       "nb_Analyse")

     df_out_analyses$commemo_123<-ss_func_ajoute_colonne(df_out_echant,
                                                         "commemo_123",
                                                         "nb_Analyse")



     # extraire les noeuds <Commemoratif> directement sous les noeuds <Demande>
     b1_nodes <- xml2::xml_find_all(doc, "//Prelevement/Echantillon/Analyse/Commemoratif")
     if(length(b1_nodes)>0){
       df_out_commemoratif_prel<-ss_func_extrat_from_xml(xml_nodes=b1_nodes,
                                                         values=list("CdCommemoratif",
                                                                     "LbCommemoratif",
                                                                     "DsCommemoratif",
                                                                     "ValCommemoratif"))

       df_out_analyses<-create_new_cols(df_out_analyses, df_out_commemoratif_prel)}

     } else
                                                  {df_out_analyses<-data.frame()}








      # dans bloc, suppression des lignes traitées
      bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                           seq(1:length(bloc)) > indice_fin)]

      if(!is.null(stations_a_traiter))
      {df_out_analyses<-df_out_analyses%>%subset(CdStationPrelevement%in%stations_a_traiter)
      df_out_echant<-df_out_echant%>%subset(CdStationPrelevement%in%stations_a_traiter)
      df_out_mesureenv<-df_out_mesureenv%>%subset(CdStationPrelevement%in%stations_a_traiter)
      df_out_prelevement<-df_out_prelevement%>%subset(StationPrelevement_CdStationPrelevement%in%stations_a_traiter)
      }


      # on agrege les fichiers
      df_out_analyses0<-dplyr::bind_rows(df_out_analyses0, df_out_analyses)
      df_out_echant0<-dplyr::bind_rows(df_out_echant0, df_out_echant)
      df_out_mesureenv0<-dplyr::bind_rows(df_out_mesureenv0, df_out_mesureenv)
      df_out_intervenant0<-dplyr::bind_rows(df_out_intervenant0, df_out_intervenant)
      df_out_prelevement0<-dplyr::bind_rows(df_out_prelevement0, df_out_prelevement)
      df_out_commemoratif0<-dplyr::bind_rows(df_out_commemoratif0, df_out_commemoratif)
      df_out_demande0<-dplyr::bind_rows(df_out_demande0, df_out_demande)

    }

    gc(verbose=FALSE)
}

  #####Traitement des balises stations de prélèvement #####
  # à traiter à la fin (hors boucle import des lignes en batch)
  # car la balise StationPrelevement peut être utilisée
  # pour décrire la station et dans le corps d'une balise <Demande>. Il faut traiter impérativement
  # les balies demmande avant
  # servira à affecter un nom de station face à un code station et à vérifier la cohérence entre les 2 informations
  # on regarde si on trouve la balise </StationPrelevement> et dans ce cas on traite
  # les éléments entre les balises StationPrelevement et /StationPrelevement

  if (!all(is.na(stringr::str_locate(bloc, "</StationPrelevement>")[, 1])))
  {
    # on extrait le bloc à traiter
    indice_debut <- grep("<StationPrelevement>", bloc)[1]
    indice_fin <- grep("</StationPrelevement>", bloc)[length(grep("</StationPrelevement>", bloc))]

     bloc_intervenant <- bloc[indice_debut:indice_fin]

      # Lecture du contenu XML
      xml_content <- '<Root>'  # On doit ajouter une racine au contenu XML pour pouvoir le lire
      xml_content <- paste(xml_content, paste(bloc_intervenant, collapse=""), sep='')
      xml_content <- paste(xml_content, '</Root>', sep = '')  # On ferme la balise racine
      doc <- xml2::read_xml(xml_content)

      # extraire les balises Intervenant
      intervenants <- xml2::xml_find_all(doc, "//StationPrelevement")

      df_out_stations<-ss_func_extrat_from_xml(xml_nodes=intervenants,
                                                  values=list("CdStationPrelevement",
                                                              "TypeStationPrelevement",
                                                              "LbStationPrelevement",
                                                              "AdresseStationPrelevement",
                                                              "CoordXStationPrelevement",
                                                              "CoordYStationPrelevement",
                                                              "ProjectStationPrelevement"),
                                                  scheme=list(list(c("CdStationPrelevement"),
                                                                   "schemeAgencyID"))
      )

      # dans bloc, suppression des lignes traitées
      bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                           seq(1:length(bloc)) > indice_fin)]

      if(!is.null(stations_a_traiter))
      {df_out_stations<-df_out_stations%>%subset(CdStationPrelevement %in% stations_a_traiter)}


  }

  return(
    list(
      Analyses = df_out_analyses0,
      Echantillon = df_out_echant0,
      Res_env = df_out_mesureenv0,
      Operation = df_out_prelevement0,
      Stations = df_out_stations,
      Intervenants = df_out_intervenant0,
      Commemoratifs = df_out_commemoratif0,
      Demande = df_out_demande0

    )
  )
}
