#' func_ajoute_nom_sandre
#'
#' @description A function to add name according to french reference SANDRE corresponding to a code
#'
#' @param connexion : DBI connexion to active LibreSQE database
#' @param code : a vector containing the code to convert
#' @param out Name of the required output. Possible values:
#' \describe{
#'   \item{\code{nom_parametre}}{To add parameter short name.}
#'   \item{\code{nom_long_parametre}}{To add parameter long name.}
#'   \item{\code{code_cas}}{To add cass number of parameter.}
#'   \item{\code{nom_fraction}}{To add fraction name.}
#'   \item{\code{nom_intervenant}}{To add intervenant short name.}
#'   \item{\code{nom_long_intervenant}}{To add intervenant full name.}
#'   \item{\code{nom_methode}}{To add methode name.}
#'   \item{\code{nom_qualif_ana}}{To add name of the qualification of analysis.}
#'   \item{\code{nom_rdd}}{To add name of the dispositif de collecte.}
#'   \item{\code{nom_station}}{To add name of the measurement station.}
#'   \item{\code{nom_statut}}{To add name of statut of analysis.}
#'  \item{\code{nom_unite}}{To add unit symbol.}
#'   \item{\code{insitu}}{To add signification of insitu code.}
#' @return vector of same length as input with corresponding name.
#' If code is missing, value return is NA
#'
#' @export
func_ajoute_nom_sandre <- function(connexion = connexion,
                                   code,
                                   out = "nom_parametre") {
  valeurs_out <- c(
    "nom_parametre",
    "nom_long_parametre",
    "code_cas",
    "nom_fraction",
    "nom_intervenant",
    "nom_long_intervenant",
    "nom_methode",
    "nom_qualif_ana",
    "nom_rdd",
    "nom_station",
    "nom_statut",
    "nom_unite",
    "insitu",
    "nom_support"
  )
  out <- match.arg(out, valeurs_out)

  # fonction qui envoi requête adaptée au parametre out
  ss_fct_requete <- function(nom_table,
                             nom_schema,
                             nom_colonne_in,
                             nom_colonne_out,
                             liste_des_codes) {
    codes_sql <-
      paste0("'", unique(liste_des_codes), "'", collapse = ", ")

    # Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
    requete <-
      paste(
        "SELECT ",
        nom_colonne_out,
        ", ",
        nom_colonne_in,
        " FROM ",
        nom_schema,
        ".",
        nom_table,
        " WHERE ",
        nom_colonne_in,
        " IN (",
        codes_sql,
        ")",
        sep = ""
      )
    # Exécuter la requête en utilisant la connexion à la base de données
    DBI::dbGetQuery(connexion, requete)
  }

  # Execution des requêtes selon la valeur de out
  if (out == "nom_parametre") {
    corresp <- ss_fct_requete(
      nom_table = "tr_parametre_par",
      nom_schema = "refer",
      nom_colonne_in = "par_cdparametre",
      nom_colonne_out = "par_nomcourt",
      liste_des_codes = code
    )
  }


  if (out == "nom_long_parametre") {
    corresp <- ss_fct_requete(
      nom_table = "tr_parametre_par",
      nom_schema = "refer",
      nom_colonne_in = "par_cdparametre",
      nom_colonne_out = "par_nomparametre",
      liste_des_codes = code
    )
  }

  if (out == "code_cas") {
    corresp <- ss_fct_requete(
      nom_table = "tr_parametre_par",
      nom_schema = "refer",
      nom_colonne_in = "par_cdparametre",
      nom_colonne_out = "par_codecas",
      liste_des_codes = code
    )
  }

  if (out == "nom_fraction") {
    corresp <- ss_fct_requete(
      nom_table = "tr_fraction_fra",
      nom_schema = "refer",
      nom_colonne_in = "fra_codefraction",
      nom_colonne_out = "fra_nomfraction",
      liste_des_codes = code
    )
  }

  if (out == "nom_intervenant") {
    corresp <- ss_fct_requete(
      nom_table = "tr_intervenantsandre_isa",
      nom_schema = "refer",
      nom_colonne_in = "isa_codesandre",
      nom_colonne_out = "isa_mnemo",
      liste_des_codes = code
    )
  }

  if (out == "nom_long_intervenant") {
    corresp <- ss_fct_requete(
      nom_table = "tr_intervenantsandre_isa",
      nom_schema = "refer",
      nom_colonne_in = "isa_codesandre",
      nom_colonne_out = "isa_nom",
      liste_des_codes = code
    )
  }

  if (out == "nom_methode") {
    corresp <- ss_fct_requete(
      nom_table = "tr_methode_met",
      nom_schema = "refer",
      nom_colonne_in = "met_code",
      nom_colonne_out = "met_nom",
      liste_des_codes = code
    )
  }

  if (out == "nom_qualif_ana") {
    corresp <- ss_fct_requete(
      nom_table = "tr_qualificationana_qal",
      nom_schema = "refer",
      nom_colonne_in = "qal_code",
      nom_colonne_out = "qal_mnemo",
      liste_des_codes = code
    )
  }

  if (out == "nom_rdd") {
    corresp <- ss_fct_requete(
      nom_table = "tr_rdd_rdd",
      nom_schema = "refer",
      nom_colonne_in = "rdd_cdrdd",
      nom_colonne_out = "rdd_nomrdd",
      liste_des_codes = code
    )
  }

  if (out == "nom_station") {
    corresp <- ss_fct_requete(
      nom_table = "tr_stationmesure_stm",
      nom_schema = "refer",
      nom_colonne_in = "stm_cdstationmesureinterne",
      nom_colonne_out = "stm_lbstationmesureeauxsurface",
      liste_des_codes = code
    )
  }

  if (out == "nom_statut") {
    corresp <- ss_fct_requete(
      nom_table = "tr_statutanalyse_san",
      nom_schema = "refer",
      nom_colonne_in = "san_cdstatutana",
      nom_colonne_out = "san_mnemostatutana",
      liste_des_codes = code
    )
  }

  if (out == "nom_unite") {
    corresp <- ss_fct_requete(
      nom_table = "tr_uniteparametre_uni",
      nom_schema = "refer",
      nom_colonne_in = "uni_codesandreunite",
      nom_colonne_out = "uni_symbole",
      liste_des_codes = code
    )
  }


  if (out == "insitu") {
    corresp <- data.frame(nom_insitu=c("Localisation inconnue", "In situ", "Laboratoire", "Sans objet"),
                          code_insitu=c("0","1","2","3"))
  }



  # mise en forme du résultat
  names(corresp) <- c("nom_out", "nom_in")
  df <- data.frame(code = code)
  df<-df %>%
    dplyr::left_join(corresp, by = c("code" = "nom_in")) %>%
    dplyr::select(nom_out) %>%
    as.vector
  return(df$nom_out)

}
