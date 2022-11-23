#' func_charge_ref_sandre_parametres
#'
#' @description Charge le référentiel SANDRE des paramètres
#'
#'@param date_maj : date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_charge_referentiel_SANDRE_parametres <-function(date_maj = "1950-01-01")
  {
    param.xml.gz <- tempfile()

    downloader::download(
      paste0(
        "https://api.sandre.eaufrance.fr/referentiels/v1/par.xml?outputSchema=SANDREv4&compress=true&derniereDateDeMAJ=",
        date_maj
      ),
      param.xml.gz,
      mode = "wb",
      cacheOK = T
    )
    file <- xml2::read_xml(param.xml.gz)
    file.remove(param.xml.gz)

    liste_parametres <-
      xml2::xml_name(file %>%  xml2::xml_child(2) %>% xml2::xml_contents())

    NbOccurrences <- file %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(grep("NbOccurrences", liste_parametres)[1]) %>%
      xml2::xml_text() %>% as.numeric()

    if (NbOccurrences > 0) {
      Parametres <- NA

      recupere_parametres <- function(i)
      {
        divs <- file %>%  xml2::xml_child(2) %>% xml2::xml_child(i)

        liste_rubriques <- divs %>%
          xml2::xml_children() %>%
          xml2::xml_name()

        CdParametre <- divs %>%
          xml2::xml_child(grep("CdParametre", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        NomParametre <- divs %>%
          xml2::xml_child(grep("NomParametre", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        StParametre <- divs %>%
          xml2::xml_child(grep("StParametre", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        LbCourtParametre <- divs %>%
          xml2::xml_child(grep("LbCourtParametre", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        CdCASSubstanceChimique <- NA

        if (any(grepl("ParametreChimique", liste_rubriques))) {
          liste_rubriques0 <- divs %>%
            xml2::xml_child(grep("ParametreChimique", liste_rubriques)) %>%
            xml2::xml_children() %>%
            xml2::xml_name()

          if (any(grepl("CdCASSubstanceChimique", liste_rubriques0))) {
            CdCASSubstanceChimique <- divs %>%
              xml2::xml_child(grep("ParametreChimique", liste_rubriques)) %>%
              xml2::xml_child(grep("CdCASSubstanceChimique", liste_rubriques0)) %>%
              xml2::xml_contents() %>%
              xml2::xml_text()
          }
        }


        ajout <-
          data.frame(
            CdParametre = CdParametre,
            NomParametre = NomParametre,
            StParametre = StParametre,
            LbCourtParametre = LbCourtParametre,
            CdCASSubstanceChimique = CdCASSubstanceChimique
          )
      }

      parametres_sandre <-
        lapply(grep("Parametre", liste_parametres), recupere_parametres) %>% dplyr::bind_rows()


      parametres_sandre <-
        parametres_sandre %>% dplyr::rename(
          "par_cdparametre" = "CdParametre",
          "par_nomparametre" = "NomParametre",
          "par_statutparametre" = "StParametre",
          "par_nomcourt" = "LbCourtParametre",
          "par_codecas" = "CdCASSubstanceChimique"
        ) %>% dplyr::select(
          par_cdparametre,
          par_nomparametre,
          par_statutparametre,
          par_nomcourt,
          par_codecas
        )
    } else{
      parametres_sandre <- NULL
    }


    return(parametres_sandre)

  }
