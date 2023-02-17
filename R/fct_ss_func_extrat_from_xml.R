#' ss_func_extrat_from_xml
#'
#' @description Function which convert xml to dataframe.
#' @param xml_nodes : nodes to treat
#' @param values : list of values to insert in result(xml tag names and xml tag path if necessary)
#' @param scheme : list of xml attribute to insert in result (xml tag names and xml tag path if necessary)
#' @param count : list of xml tag to count
#'
#' syntax for the lists values and count:
#' - First element and following ones = xml tag path,
#' - last element = xml tag name
#'
#' syntax for scheme :
#' list(c(list(path), scheme_name))
#'
#' @return The function return a dataframe with required data as column name
#'
#' @noRd
ss_func_extrat_from_xml<-function(xml_nodes,
                                  values=NULL,
                                  scheme=NULL,
                                  count=NULL){


# xml_nodes<-intervenants
# values<-list(c("MesureEnvironnementale", "RsParEnv"),
#              c("MesureEnvironnementale", "RqParEnv"),
#              c("MesureEnvironnementale", "DateParEnv"),
#              c("MesureEnvironnementale", "Methode", "CdMethode"),
#              c("MesureEnvironnementale", "Methode", "NomMethode")
#             )
#   values<-list(c("DatePrel"),
#                c("HeurePrel"),
#                c("AccredPrel"),
#                c("CommentairesPrel"),
#                c("Support", "CdSupport"),
#                c("StationPrelevement", "CdStationPrelevement")
#               )
#
# scheme<-list(list(c("StationPrelevement", "CdStationPrelevement"),
#                   "schemeAgencyID"))
# count<-list("MesureEnvironnementale",
#             "Echantillon",
#             "Analyse",
#             "StationPrelevement",
#             "StationBidon",
#             "CdStationPrelevement")


  if(!("xml_nodeset"%in%class(xml_nodes))){stop("ss_func_extrat_from_xml : xml_nodes should be a validate xml_nodeset")}
  if(!(is.list(values) | is.null(values))){stop("ss_func_extrat_from_xml : values should be a list")}
  if(!(is.list(scheme)| is.null(scheme))){stop("ss_func_extrat_from_xml : scheme should be a list")}
  if(!(is.list(count)| is.null(count))){stop("ss_func_extrat_from_xml : count should be a list")}
  if(all(is.null(values),
         is.null(scheme),
         is.null(count))){stop("ss_func_extrat_from_xml : scheme, count and values can't be all NULL at the same time")}


#initialisation du data_frame de sortie
nm_values<-sapply(values, paste, sep="", collapse="_")
if(length(nm_values)==0){nm_values<-NULL}

nm_scheme <- list()
for (i in seq_along(scheme)) {
  nm_scheme[[i]] <- scheme[[i]][[1]]
}
nm_scheme<-sapply(nm_scheme, paste, sep="", collapse="_")
if(length(nm_scheme)==0){nm_scheme<-NULL}else{nm_scheme<-paste0("type_", nm_scheme)}
nm_count<-sapply(count, paste, sep="", collapse="_")
if(length(nm_count)==0){nm_count<-NULL}else{nm_count<-paste0("nb_", nm_count)}


colonnes<- c(nm_values,
             nm_scheme,
             nm_count)

longueur<-length(xml_nodes)

  # Créer un data.frame vide avec les noms de colonnes
  df_out <- data.frame(matrix(ncol = length(colonnes), nrow = longueur))
  names(df_out) <- colonnes

  # Convertir toutes les colonnes en caractères
  df_out[] <- lapply(df_out, as.character)

#####Lecture / remplissage des Values #####
  # fonction pour lire les contenus d'une balise
  extract_node <- function(xml_nodes, chemin) {
    # Commencer à partir de la racine du XML
    node <- xml_nodes

    # Parcourir le chemin en appliquant la fonction xml_find_first() à chaque étape
    for (i in chemin) {
      for(k in i)
      {node <- xml2::xml_find_first(node, paste0("./", k))}
    }

    # Extraire le texte du noeud final
    output <- xml2::xml_text(node)

    return(output)
  }
# remplissage du data frame avec les noms de la liste
remplissage_df<-function(i, liste_values){
  extract_node(xml_nodes, liste_values[i])
}

liste_values<-values
chemin<-liste_values[4]

# remplissage du dataframe avec les values
df_out[,nm_values]<-lapply(seq_along(nm_values), function(x)remplissage_df(x,values))%>%as.data.frame


#####Lecture / remplissage des Schemes #####
# fonction pour lire les contenus d'une balise
extract_node_scheme <- function(xml_nodes, chemin) {

  chemins_scheme <- list()
  for (i in seq_along(chemin)) {
    chemins_scheme[[i]] <- chemin[[i]][[1]]
  }
  chemins_attr <- list()
  for (i in seq_along(chemin)) {
    chemins_attr[[i]] <- chemin[[i]][[2]]
  }

    # Commencer à partir de la racine du XML
  node <- xml_nodes

  # Parcourir le chemin en appliquant la fonction xml_find_first() à chaque étape
  for (i in chemins_scheme) {
    node <- xml2::xml_find_first(node, paste0("./", i))
  }

  # Extraire l'e texte'attribut du noeud final
  output <- xml2::xml_attr(node, as.character(chemins_attr))

  return(output)
}

# remplissage du data frame avec les noms de la liste
remplissage_df_scheme<-function(i, liste_values){
  extract_node_scheme(xml_nodes, liste_values[i])
}

# remplissage du dataframe avec les values
df_out[,nm_scheme]<-lapply(seq_along(sapply(scheme, head, n = 1)%>%as.character),
                           function(x)remplissage_df_scheme(x,scheme))%>%as.data.frame



#####Lecture / remplissage des count #####
compter_balises <- function(balises_a_lire, xml_nodes, base) {
    res <- data.frame(matrix(0,
                             nrow = length(xml2::xml_find_all(xml_nodes, paste0("//", base))),
                             ncol = length(balises_a_lire))
                      )
  colnames(res) <- balises_a_lire
  i <- 1
  for (node in xml2::xml_find_all(xml_nodes, paste0("//", base))) {
    for (balise in balises_a_lire) {
      res[i, balise] <- length(xml2::xml_find_all(node, paste0(".//", balise)))
    }
    i <- i + 1
  }
  return(res)
}

base<-xml2::xml_name(xml_nodes[[1]])
df_out[,nm_count]<-compter_balises(count, xml_nodes, base)

  # Afficher le data.frame créé
  return(df_out)


}

