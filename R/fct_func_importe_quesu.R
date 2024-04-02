#' func_importe_quesu
#'
#' @description lecture de fichiers SANDRE xml aux formats d'échanges QUESU 3 ou 3.1
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
func_importe_quesu <- function(fichier, stations_a_traiter = NULL) {
    if (!("character" %in% class(fichier))) {
    stop(
      "func_importe_quesu : le paramètre fichier doit être un
                                          character spécifiant le chemin d'accès au fichier à importer"
    )
  }
  if (substr(fichier, nchar(fichier) - 3, nchar(fichier)) != ".xml") {
    stop("func_importe_quesu : le fichier \u00e0 importer doit \u00eatre au format xml")
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

  Operation <- data.frame(
    "CdStationMesureEauxSurface" = character(0),
    "CdPrelevement" = character(0),
    "DatePrel" = character(0),
    "HeurePrel" = character(0),
    "DifficultePrel" = character(0),
    "AccredPrel" = character(0),
    "PointPrel" = character(0),
    "CdSupport" = character(0),
    "LbSupport" = character(0),
    "CdProducteur" = character(0),
    "LbProducteur" = character(0),
    "CdPreleveur" = character(0),
    "LbPreleveur" = character(0),
    "ZoneVerticaleProspectee" = character(0),
    "ProfondeurPrel" = character(0),
    "FinalitePrel" = character(0),
    "AgrePrel" = character(0),
    "CommentairesPrel" = character(0),
    "CdMetPrlvt" = character(0),
    "LbMetPrlvt" = character(0)
  )



  Res_env <- data.frame(
    "CdStationMesureEauxSurface" = character(0),
    "CdPrelevement" = character(0),
    "DateParEnv" = character(0),
    "HeureParEnv" = character(0),
    "CdParametreEnv" = character(0),
    "LbParametreEnv" = character(0),
    "RsParEnv" = character(0),
    "CdUniteMesure" = character(0),
    "LbUniteMesure" = character(0),
    "CdRqParEn" = character(0),
    "CdStatutParEn" = character(0),
    "CdQualParEnv" = character(0),
    "ComParEnv" = character(0),
    "CdMethodeParEnv" = character(0),
    "CdProducteur" = character(0),
    "CdPreleveur" = character(0)
  )


  Echantillon <- data.frame(
    "CdStationMesureEauxSurface" = character(0),
    "CdPrelevement" = character(0),
    "DatePrel" = character(0),
    "HeurePrel" = character(0),
    "RefEchantillonCommanditaire" = character(0),
    "DateReceptionEchant" = character(0),
    "HeureReceptionEchant" = character(0),
    "CommentairesEchant" = character(0),
    "CdMethodeTransport" = character(0),
    "NomMethodeTransport" = character(0),
    "CdLaboratoireReceptionEchant" = character(0),
    "LbLaboratoireReceptionEchant" = character(0)
  )

  Analyses <- data.frame(
    "CdStationMesureEauxSurface" = character(0),
    "LbStationMesureEauxSurface" = character(0),
    "CdSupport" = character(0),
    "LbSupport" = character(0),
    "CdFractionAnalysee" = character(0),
    "LbFractionAnalysee" = character(0),
    "CdPrelevement" = character(0),
    "RefAnaProd" = character(0),
    "DatePrel" = character(0),
    "HeurePrel" = character(0),
    "DateAna" = character(0),
    "HeureAna" = character(0),
    "CdParametre" = character(0),
    "LbParametre" = character(0),
    "RsAna" = character(0),
    "CdUniteMesure" = character(0),
    "LbUniteMesure" = character(0),
    "CdRqAna" = character(0),
    "CdInsituAna" = character(0),
    "ProfondeurPrel" = character(0),
    "CdDifficulteAna" = character(0),
    "LDAna" = character(0),
    "LQAna" = character(0),
    "LSAna" = character(0),
    "IncertAna" = character(0),
    "CdMetFractionnement" = character(0),
    "LbMetFractionnement" = character(0),
    "CdMethode" = character(0),
    "LbMethode" = character(0),
    "RdtExtraction" = character(0),
    "CdMethodeExtraction" = character(0),
    "LbMethExtraction" = character(0),
    "CdAccreAna" = character(0),
    "AgreAna" = character(0),
    "CdStatutAna" = character(0),
    "CdQualAna" = character(0),
    "CommentairesAna" = character(0),
    "ComResultatAna" = character(0),
    "CdRdd" = character(0),
    "LbRdd" = character(0),
    "CdProducteur" = character(0),
    "LblProducteur" = character(0),
    "CdPreleveur" = character(0),
    "LblPreleveur" = character(0),
    "CdLaboratoire" = character(0),
    "LbLaboratoire" = character(0),
    "ZoneVerticaleProspectee" = character(0)
  )


  ###### fonction de lecture du contenu du xml #####
  f_lit_attributs <-
    function(Chemin,
             Chemin2 = NULL,
             node = nodes_mesures_env)
    {
      if (is.null(Chemin2)) {
        result <-
          lapply(xml2::xml_find_all(node, paste0("./", Chemin), flatten = FALSE),
                 xml2::xml_text) %>%
          lapply(function(x)
            if (identical(x, character(0)))
              NA_character_
            else
              x) %>%
          unlist()

      } else{
        result <-
          lapply(xml2::xml_find_all(node, paste0("./", Chemin, "/", Chemin2), flatten =
                                      FALSE),
                 xml2::xml_text) %>%
          lapply(function(x)
            if (identical(x, character(0)))
              NA_character_
            else
              x) %>%
          unlist()
        class(node)
      }

      if (rlang::is_empty(result)) {
        result <- NA
      }
      return(result)
    }


  ##### Traitement du fichier #####

  while (length(lines <-
                readLines(file_in, n = n_lines, warn = FALSE)) > 0) {
    # On déplace la balise </StationMesureEauxSurface> pour la placer avant celle </ResPC>
    lines <- gsub("</StationMesureEauxSurface>", "", lines)
    lines <-
      gsub("</ResPC>", "</StationMesureEauxSurface></ResPC>", lines)

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

      indice_debut <- grep("<VersionScenario>", bloc)[1]
      indice_fin <- grep("</VersionScenario>", bloc)[1]
      bloc_station <-
        bloc[indice_debut:indice_fin] %>% paste(collapse = "")
      versionscenario <-
        gsub(".*<VersionScenario>(.+)</VersionScenario>*",
             "\\1",
             bloc_station)

      if (!paste0(CodeScenario, versionscenario) %in%
          c("QUESU_PHY3", "QUESU_PHY3.1")) {
        stop("Sc\u00e9nario d\'\u00e9change non support\u00e9")
      }

      first_passage <- FALSE
    }

    ##### Traitement des balises ResPC #####
    # on regarde si on trouve la balise </ResPC> et dans ce cas on traite les éléments entre les balises ResPC et /ResPC
    while (!all(is.na(stringr::str_locate(bloc, "</ResPC>")[, 1])))
    {
      # on extrait le bloc à traiter
      indice_debut <- grep("<ResPC>", bloc)[1]
      indice_fin <- grep("</ResPC>", bloc)[1]

      bloc_station <- bloc[indice_debut:indice_fin]

      # traitement du bloc comme un fichier xml
      lit_bloc <-
        xml2::read_xml(paste(bloc_station, collapse = "")) %>% xml2::xml_children()
      CdStationMesureEauxSurface <-
        xml2::xml_find_first(lit_bloc, ".//CdStationMesureEauxSurface") %>%
        xml2::xml_contents() %>%
        as.character()
      print(CdStationMesureEauxSurface)
      #on traite les résultats seulement si on doit traiter toutes les stations ou bien si le code station figure parmi la liste des codes à traiter.
      if (is.null(stations_a_traiter) |
          CdStationMesureEauxSurface %in% stations_a_traiter)
      {
        LbStationMesureEauxSurface <-
          xml2::xml_find_first(lit_bloc, ".//LbStationMesureEauxSurface") %>%
          xml2::xml_contents() %>%
          as.character()
        if (rlang::is_empty(LbStationMesureEauxSurface)) {
          LbStationMesureEauxSurface <- NA
        }
        # traitement des opérations de prélèvement
        liste_prelevements_all <-
          xml2::xml_find_all(lit_bloc, ".//Prelevement")

        # on traite pour chaque opérations de prélèvement
        for (j in 1:length(liste_prelevements_all)) {
          liste_prelevements <- liste_prelevements_all[[j]]
          CdPrelevement <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//CdPrelevement"))
          DatePrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//DatePrel"))
          HeurePrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//HeurePrel"))
          DifficultePrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//DifficultePrel"))
          AccredPrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//AccredPrel"))
          PointPrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//PointPrel"))
          CdSupport <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//Support")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//Support"),
                  ".//CdSupport"
                )
              ),
              NA
            )
          LbSupport <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//Support")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//Support"),
                  ".//LbSupport"
                )
              ),
              NA
            )
          CdProducteur <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//ProducteurPrelevement")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//ProducteurPrelevement"),
                  ".//CdIntervenant"
                )
              ),
              NA
            )
          LbProducteur <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//ProducteurPrelevement")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//ProducteurPrelevement"),
                  ".//NomIntervenant"
                )
              ),
              NA
            )
          CdPreleveur <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//Preleveur")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//Preleveur"),
                  ".//CdIntervenant"
                )
              ),
              NA
            )
          LbPreleveur <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//Preleveur")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//Preleveur"),
                  ".//NomIntervenant"
                )
              ),
              NA
            )
          ZoneVerticaleProspectee <-
            xml2::xml_text(
              xml2::xml_find_first(liste_prelevements, ".//ZoneVerticaleProspectee")
            )
          ProfondeurPrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//ProfondeurPrelevement"))
          FinalitePrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//FinalitePrel"))
          AgrePrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//AgrePrel"))
          CommentairesPrel <-
            xml2::xml_text(xml2::xml_find_first(liste_prelevements, ".//CommentairesPrel"))
          CdMetPrlvt <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//MethodePrlvt")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//MethodePrlvt"),
                  ".//CdMethode"
                )
              ),
              NA
            )
          LbMetPrlvt <-
            ifelse(
              !is.na(
                xml2::xml_find_first(liste_prelevements, ".//MethodePrlvt")
              ),
              xml2::xml_text(
                xml2::xml_find_first(
                  xml2::xml_find_first(liste_prelevements, ".//MethodePrlvt"),
                  ".//NomMethode"
                )
              ),
              NA
            )

          out <-
            data.frame(
              "CdStationMesureEauxSurface" = CdStationMesureEauxSurface,
              "CdPrelevement" = CdPrelevement,
              "DatePrel" = DatePrel,
              "HeurePrel" = HeurePrel,
              "DifficultePrel" = DifficultePrel,
              "AccredPrel" = AccredPrel,
              "PointPrel" = PointPrel,
              "CdSupport" = CdSupport,
              "LbSupport" = LbSupport,
              "CdProducteur" = CdProducteur,
              "LbProducteur" = LbProducteur,
              "CdPreleveur" = CdPreleveur,
              "LbPreleveur" = LbPreleveur,
              "ZoneVerticaleProspectee" = ZoneVerticaleProspectee,
              "ProfondeurPrel" = ProfondeurPrel,
              "FinalitePrel" = FinalitePrel,
              "AgrePrel" = AgrePrel,
              "CommentairesPrel" = CommentairesPrel,
              "CdMetPrlvt" = CdMetPrlvt,
              "LbMetPrlvt" = LbMetPrlvt
            )

          Operation <- dplyr::bind_rows(Operation, out)

          ##### Traitement des mesures environnementales (QUESU_PHY 3 ou 3.1) #####
          nodes_mesures_env <-
            xml2::xml_find_all(liste_prelevements, ".//MesureEnvironnementale")

          DateParEnv <- f_lit_attributs("DateParEnv")
          HeureParEnv <- f_lit_attributs("HeureParEnv")
          CdParametreEnv <-
            f_lit_attributs("ParametreEnv", "CdParametre")
          LbParametreEnv <-
            f_lit_attributs("ParametreEnv", "LbParametre")
          RsParEnv <- f_lit_attributs("RsParEnv")
          CdUniteMesure <-
            f_lit_attributs("UniteMesure", "CdUniteMesure")
          LbUniteMesure <-
            f_lit_attributs("UniteMesure", "LbUniteMesure")
          CdRqParEn <- f_lit_attributs("RqParEn")
          CdStatutParEn <- f_lit_attributs("StatutParEn")
          CdQualParEnv <- f_lit_attributs("QualParEnv")
          ComParEnv <- f_lit_attributs("ComParEnv")
          CdStatutParEn <- f_lit_attributs("StatutParEn")
          CdMethodeParEnv <- f_lit_attributs("Methode", "CdMethode")
          LbMethodeParEnv <- f_lit_attributs("Methode", "LbMethode")

          if(!all(is.na(c(DateParEnv, CdParametreEnv)))){
          out <-
            data.frame(
              "CdStationMesureEauxSurface" = CdStationMesureEauxSurface,
              "CdPrelevement" = CdPrelevement,
              "DateParEnv" = DateParEnv,
              "HeureParEnv" = HeureParEnv,
              "CdParametreEnv" = CdParametreEnv,
              "LbParametreEnv" = LbParametreEnv,
              "RsParEnv" = RsParEnv,
              "CdUniteMesure" = CdUniteMesure,
              "LbUniteMesure" = LbUniteMesure,
              "CdRqParEn" = CdRqParEn,
              "CdStatutParEn" = CdStatutParEn,
              "CdQualParEnv" = CdQualParEnv,
              "ComParEnv" = ComParEnv,
              "CdMethodeParEnv" = CdMethodeParEnv,
              "LbMethodeParEnv" = LbMethodeParEnv,
              "CdProducteur" = CdProducteur,
              "CdPreleveur" = CdPreleveur
            )

          Res_env <- dplyr::bind_rows(Res_env, out)}

          ##### Traitement des échantillons (QUESU_PHY 3 ou 3.1) #####
          nodes_echantillon <-
            xml2::xml_find_all(liste_prelevements, ".//Echantillon")

          RefEchantillonCommanditaire <-
            f_lit_attributs("RefEchantillonCommanditaire", node = nodes_echantillon)
          DateReceptionEchant <-
            f_lit_attributs("DateReceptionEchant", node = nodes_echantillon)
          HeureReceptionEchant <-
            f_lit_attributs("HeureReceptionEchant", node = nodes_echantillon)
          CommentairesEchant <-
            f_lit_attributs("CommentairesEchant", node = nodes_echantillon)
          CdMethodeTransport <-
            f_lit_attributs("MethodeTransport", "CdMethode", node = nodes_echantillon)
          NomMethodeTransport <-
            f_lit_attributs("MethodeTransport", "NomMethode", node = nodes_echantillon)
          CdLaboratoireReceptionEchant <-
            f_lit_attributs("Laboratoire", "CdIntervenant", node = nodes_echantillon)
          LbLaboratoireReceptionEchant <-
            f_lit_attributs("Laboratoire", "NomIntervenant", node = nodes_echantillon)

          out <-
            data.frame(
              "CdStationMesureEauxSurface" = CdStationMesureEauxSurface,
              "CdPrelevement" = CdPrelevement,
              "DatePrel" = DatePrel,
              "HeurePrel" = HeurePrel,
              "RefEchantillonCommanditaire" = RefEchantillonCommanditaire,
              "DateReceptionEchant" = DateReceptionEchant,
              "HeureReceptionEchant" = HeureReceptionEchant,
              "CommentairesEchant" = CommentairesEchant,
              "CdMethodeTransport" = CdMethodeTransport,
              "NomMethodeTransport" = NomMethodeTransport,
              "CdLaboratoireReceptionEchant" = CdLaboratoireReceptionEchant,
              "LbLaboratoireReceptionEchant" = LbLaboratoireReceptionEchant
            )

          Echantillon <- dplyr::bind_rows(Echantillon, out)

          ###### Traitement des réseaux de mesure (QUESU_PHY 3 ou 3.1) #####
          nodes_rsx <-
            xml2::xml_find_all(liste_prelevements, ".//Rsx")

          CdRdd <- f_lit_attributs("CodeSandreRdd", node = nodes_rsx)
          CdRdd <- if (!all(is.na(CdRdd))) {
            paste(CdRdd, collapse = "/")
          }

          NomRdd <- f_lit_attributs("NomRdd", node = nodes_rsx)
          NomRdd <-
            ifelse(!all(is.na(NomRdd)), paste(NomRdd, collapse = "/"), NA)

          ###### Traitement des résultats d'analyses (QUESU_PHY 3 ou 3.1) #####
          nodes_analyses <-
            xml2::xml_find_all(liste_prelevements, ".//Analyse")
          if(length(nodes_analyses)>0){
          RefAnaProd <-
            f_lit_attributs("RefAnaProd", node = nodes_analyses)
          DateAna <- f_lit_attributs("DateAna", node = nodes_analyses)
          HeureAna <-
            f_lit_attributs("HeureAna", node = nodes_analyses)
          CdParametre <-
            f_lit_attributs("Parametre", "CdParametre", node = nodes_analyses)
          LbParametre <-
            f_lit_attributs("Parametre", "NomParametre", node = nodes_analyses)
          CdFractionAnalysee <-
            f_lit_attributs("FractionAnalysee", "CdFractionAnalysee", node = nodes_analyses)
          LbFractionAnalysee <-
            f_lit_attributs("FractionAnalysee", "LbFractionAnalysee", node = nodes_analyses)
          RsAna <- f_lit_attributs("RsAna", node = nodes_analyses)
          CdUniteMesure <-
            f_lit_attributs("UniteMesure", "CdUniteMesure", node = nodes_analyses)
          LbUniteMesure <-
            f_lit_attributs("UniteMesure", "LbUniteMesure", node = nodes_analyses)
          CdRqAna <- f_lit_attributs("RqAna", node = nodes_analyses)
          CdInsituAna <-
            f_lit_attributs("InsituAna", node = nodes_analyses)
          CdDifficulteAna <-
            f_lit_attributs("DifficulteAna", node = nodes_analyses)
          CdQualAna <-
            f_lit_attributs("QualAna", node = nodes_analyses)
          CommentairesAna <-
            f_lit_attributs("CommentairesAna", node = nodes_analyses)
          ComResultatAna <-
            f_lit_attributs("ComResultatAna", node = nodes_analyses)
          CdStatutAna <-
            f_lit_attributs("StatutAna", node = nodes_analyses)
          CdAccreAna <-
            f_lit_attributs("AccreAna", node = nodes_analyses)
          LDAna <- f_lit_attributs("LDAna", node = nodes_analyses)
          LQAna <- f_lit_attributs("LQAna", node = nodes_analyses)
          LSAna <- f_lit_attributs("LSAna", node = nodes_analyses)
          IncertAna <-
            f_lit_attributs("IncertAna", node = nodes_analyses)
          AgreAna <- f_lit_attributs("AgreAna", node = nodes_analyses)
          CdMetFractionnement <-
            f_lit_attributs(Chemin = "MetFractionnement",
                            Chemin2 = "CdMethode",
                            node = nodes_analyses)
          LbMetFractionnement <-
            f_lit_attributs("MetFractionnement", "NomMethode", node = nodes_analyses)
          CdMethode <-
            f_lit_attributs("Methode", "CdMethode", node = nodes_analyses)
          LbMethode <-
            f_lit_attributs("Methode", "NomMethode", node = nodes_analyses)
          CdMethExtraction <-
            f_lit_attributs("MethExtraction", "CdMethode", node = nodes_analyses)
          LbMethExtraction <-
            f_lit_attributs("MethExtraction", "NomMethode", node = nodes_analyses)
          RdtExtraction <-
            f_lit_attributs("RdtExtraction", node = nodes_analyses)
          CdProducteur <-
            f_lit_attributs("Producteur", "CdIntervenant", node = nodes_analyses)
          LbProducteur <-
            f_lit_attributs("Producteur", "NomIntervenant", node = nodes_analyses)
          CdLaboratoire <-
            f_lit_attributs("Laboratoire", "CdIntervenant", node = nodes_analyses)
          LbLaboratoire <-
            f_lit_attributs("Laboratoire", "NomIntervenant", node = nodes_analyses)
          # CdRddAna <-
          #   f_lit_attributs("Rsx", "CodeSandreRdd", node = nodes_analyses)

# si plusieurs codes Rdd on les fusionne en 1 seul séparé par des /
          for (k in 1:length(nodes_analyses)) {
            liste_analyse <- nodes_analyses[[k]]

            # Traitement des balises CodeSandreRdd
            CdRddNodes <- xml2::xml_find_all(liste_analyse, ".//CodeSandreRdd")
            if (length(CdRddNodes) > 1) {
              CdRddValues <- sapply(CdRddNodes, function(x) xml2::xml_text(x))
              CdRddAna <- paste(CdRddValues, collapse = "/")
            } else if (length(CdRddNodes) == 1) {
              CdRddAna <- xml2::xml_text(CdRddNodes[[1]])
            } else {
              # si pas de code réseau dans les balises Analyse, on les remplace par celles au niveau de Prélèvement
                            CdRddAna <- CdRdd
              }
            }



          # si plusieurs nom Rdd on les fusionne en 1 seul séparé par des /
          for (k in 1:length(nodes_analyses)) {
            liste_analyse <- nodes_analyses[[k]]

            # Traitement des balises CodeSandreRdd
            CdRddNodes <- xml2::xml_find_all(liste_analyse, ".//NomRdd")
            if (length(CdRddNodes) > 1) {
              CdRddValues <- sapply(CdRddNodes, function(x) xml2::xml_text(x))
              NomRddAna <- paste(CdRddValues, collapse = "/")
            } else if (length(CdRddNodes) == 1){
              NomRddAna <- xml2::xml_text(CdRddNodes[[1]])
            } else
            {NomRddAna <- NomRdd}
          }



          out <-
            data.frame(
              "CdStationMesureEauxSurface" = CdStationMesureEauxSurface,
              "LbStationMesureEauxSurface" = LbStationMesureEauxSurface,
              "CdSupport" = CdSupport,
              "LbSupport" = LbSupport,
              "CdFractionAnalysee" = CdFractionAnalysee,
              "LbFractionAnalysee" = LbFractionAnalysee,
              "CdPrelevement" = CdPrelevement,
              "RefAnaProd" = RefAnaProd,
              "DatePrel" = DatePrel,
              "HeurePrel" = HeurePrel,
              "DateAna" = DateAna,
              "HeureAna" = HeureAna,
              "CdParametre" = CdParametre,
              "LbParametre" = LbParametre,
              "RsAna" = RsAna,
              "CdUniteMesure" = CdUniteMesure,
              "LbUniteMesure" = LbUniteMesure,
              "CdRqAna" = CdRqAna,
              "CdInsituAna" = CdInsituAna,
              "ProfondeurPrel" = ProfondeurPrel,
              "CdDifficulteAna" = CdDifficulteAna,
              "LDAna" = LDAna,
              "LQAna" = LQAna,
              "LSAna" = LSAna,
              "IncertAna" = IncertAna,
              "CdMetFractionnement" = CdMetFractionnement,
              "LbMetFractionnement" = LbMetFractionnement,
              "CdMethode" = CdMethode,
              "LbMethode" = LbMethode,
              "RdtExtraction" = RdtExtraction,
              "CdMethodeExtraction" = CdMethExtraction,
              "LbMethExtraction" = LbMethExtraction,
              "CdAccreAna" = CdAccreAna,
              "AgreAna" = AgreAna,
              "CdStatutAna" = CdStatutAna,
              "CdQualAna" = CdQualAna,
              "CommentairesAna" = CommentairesAna,
              "ComResultatAna" = ComResultatAna,
              "CdRdd" = CdRddAna,
              "LbRdd" = NomRddAna,
              "CdProducteur" = CdProducteur,
              "LbProducteur" = LbProducteur,
              "CdPreleveur" = CdPreleveur,
              "LbPreleveur" = LbPreleveur,
              "CdLaboratoire" = CdLaboratoire,
              "LbLaboratoire" = LbLaboratoire,
              "ZoneVerticaleProspectee" = ZoneVerticaleProspectee
            )

          Analyses <- dplyr::bind_rows(Analyses, out)


}
        }
      }

      # dans bloc, suppression des lignes traitées
      bloc <- bloc[which(seq(1:length(bloc)) < indice_debut |
                           seq(1:length(bloc)) > indice_fin)]


    }

  }

  close(file_in)  # fermeture du fichier d'entrée
  return(
    list(
      Analyses = Analyses,
      Echantillon = Echantillon,
      Res_env = Res_env,
      Operation = Operation
    )
  )
}
