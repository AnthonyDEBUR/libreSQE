#' func_charge_ref_sandre_intervenants
#'
#'
#' @description Charge le référentiel SANDRE des inetervenants
#'
#'@param date_maj date à partir de laquelle telecharger les mises à jour
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
func_charge_ref_sandre_intervenants <-
  function(date_maj = "1950-01-01")
  {
    intervenants.xml.gz <- tempfile()

    downloader::download(
      paste0(
        "https://api.sandre.eaufrance.fr/referentiels/v1/int.xml?outputSchema=SANDREv2&compress=true&derniereDateDeMAJ=",
        date_maj
      ),
      intervenants.xml.gz,
      mode = "wb",
      cacheOK = T,
      extra = options(timeout = 600)
    )


    # choisir le fichier téléchargé pour le dézipper et le lire
    file <- xml2::read_xml(intervenants.xml.gz)

    file.remove(intervenants.xml.gz)

    liste_intervenants <-
      xml2::xml_name(file %>%  xml2::xml_child(2) %>% xml2::xml_contents())
    intervenants <- NA

    NbOccurrences <- file %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(grep("NbOccurrences", liste_intervenants)[1]) %>%
      xml2::xml_text() %>% as.numeric()


    if (NbOccurrences > 0) {
      recupere_intervenant <- function(i)
      {
        tmp <- file %>%
          xml2::xml_child(2) %>%
          xml2::xml_child(i)

        liste_rubriques <-  tmp %>%
          xml2::xml_children() %>%
          xml2::xml_name()

        CdIntervenant <- tmp %>%
          xml2::xml_child(grep("CdIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        NomIntervenant <- tmp %>%
          xml2::xml_child(grep("NomIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        StIntervenant <- tmp %>%
          xml2::xml_child(grep("StIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        DateCreationIntervenant <- tmp %>%
          xml2::xml_child(grep("DateCreationIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        DateMajIntervenant <- tmp %>%
          xml2::xml_child(grep("DateMajIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        AuteurIntervenant <- tmp %>%
          xml2::xml_child(grep("AuteurIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        MnIntervenant <- tmp %>%
          xml2::xml_child(grep("MnIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        BpIntervenant <- tmp %>%
          xml2::xml_child(grep("BpIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        ImmoIntervenant <- tmp %>%
          xml2::xml_child(grep("ImmoIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        RueIntervenant <- tmp %>%
          xml2::xml_child(grep("RueIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        LieuIntervenant <- tmp %>%
          xml2::xml_child(grep("LieuIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        VilleIntervenant <- tmp %>%
          xml2::xml_child(grep("VilleIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        DepIntervenant <- tmp %>%
          xml2::xml_child(grep("DepIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        ActivitesIntervenant <- tmp %>%
          xml2::xml_child(grep("ActivitesIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        CPIntervenant <- tmp %>%
          xml2::xml_child(grep("CPIntervenant", liste_rubriques)) %>%
          xml2::xml_contents() %>%
          xml2::xml_text()

        fct_ajoute_si <- function(x) {
          ifelse(length(x) > 0, x, NA)
        }

        ajout <-
          data.frame(
            CdIntervenant = fct_ajoute_si(CdIntervenant),
            NomIntervenant = fct_ajoute_si(NomIntervenant),
            StIntervenant = fct_ajoute_si(StIntervenant),
            DateCreationIntervenant = fct_ajoute_si(DateCreationIntervenant),
            DateMajIntervenant = fct_ajoute_si(DateMajIntervenant),
            AuteurIntervenant = fct_ajoute_si(AuteurIntervenant),
            MnIntervenant = fct_ajoute_si(MnIntervenant),
            BpIntervenant = fct_ajoute_si(BpIntervenant),
            ImmoIntervenant = fct_ajoute_si(ImmoIntervenant),
            RueIntervenant = fct_ajoute_si(RueIntervenant),
            LieuIntervenant = fct_ajoute_si(LieuIntervenant),
            VilleIntervenant = fct_ajoute_si(VilleIntervenant),
            DepIntervenant = fct_ajoute_si(DepIntervenant),
            ActivitesIntervenant = fct_ajoute_si(ActivitesIntervenant),
            CPIntervenant = fct_ajoute_si(CPIntervenant)
          )

        return(ajout)
      }

      intervenants <-
        lapply(grep("Intervenant", liste_intervenants),
               recupere_intervenant) %>% dplyr::bind_rows()





      colnames(intervenants) <-
        stringi::stri_trans_general(colnames(intervenants), "Latin-ASCII")

      # if(nrow(intervenants)>0){intervenants$isa_siret<-ifelse(intervenants$`CdIntervenant@schemeAgencyID`=="SIRET",
      #                                                         intervenants$CdIntervenant,
      #                                                         NA)} else {intervenants$isa_siret<-character(0)}

      intervenants <-
        intervenants %>%
        dplyr::select(
          CdIntervenant,
          NomIntervenant,
          StIntervenant,
          DateCreationIntervenant,
          DateMajIntervenant,
          MnIntervenant,
          ImmoIntervenant,
          BpIntervenant,
          RueIntervenant,
          LieuIntervenant,
          VilleIntervenant,
          DepIntervenant,
          CPIntervenant
        )


      names(intervenants) <- c(
        "isa_codesandre",
        "isa_nom",
        "isa_statut",
        "isa_datecreation",
        "isa_datemaj",
        "isa_mnemo",
        "isa_ensembleimmobilier",
        "isa_bp",
        "isa_rue",
        "isa_lieudit",
        "isa_ville",
        "isa_departementpays",
        "isa_codepostal"
      )
    } else{
      intervenants <- NULL
    }

    return(intervenants)

  }
