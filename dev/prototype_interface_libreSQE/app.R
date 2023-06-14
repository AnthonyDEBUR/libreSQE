#
# Prototype de l'interface de libreSQE

library(shiny)
library(shinipsum)
library(DT)
library(tidyverse)
library(shinybusy)
library(tools4DCE)
library(readxl)

# limite fichier uploadable 120 Mo
options(shiny.maxRequestSize = 120*1024^2)


connexion <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

tableau_maj_ref <-
  DBI::dbReadTable(connexion, DBI::Id(schema = "sqe",
                                      table = "ts_suivi_maj_refer"))

tableau_per_gest <-
  DBI::dbReadTable(connexion, DBI::Id(schema = "refer",
                                      table = "tr_perimetre_per"))


fichier_prog <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\R_Anthony\\libreSQE\\dev\\v2 prog EPTB Est_Ouest 2022 - commande_3 derniers trimestres_ajout suivis Captages_version dev libreSQE.xlsx"


# param_perimetre_facturation <- "UGVE"
# param_rattachement_bdc <- "pluie"
# param_mois <- "aout"

prog_annuelle <- read_xlsx(fichier_prog,
                           sheet = "programme_annuel",
                           col_types = "text")


calendrier <- read_xlsx(fichier_prog,
                        sheet = "calendrier")


programmes_types <- read_xlsx(fichier_prog,
                              sheet = "programmes_types")


BPU <- read_xlsx(fichier_prog,
                 sheet = "BPU")


cout_run_analytiques <- read_xlsx(fichier_prog,
                                  sheet = "cout_run_analytiques")


prog_previsionnelle <-
  left_join(prog_annuelle,
            calendrier,
            by = c("Type station" = "type de station"))


prog_previsionnelle <- pivot_longer(prog_previsionnelle,
                                    janvier:décembre,
                                    names_to = "mois",
                                    values_to = "quantite_commandee")

prog_previsionnelle <-
  prog_previsionnelle %>% subset(quantite_commandee > 0)


table_stat_analyses <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses.rds"
  )
table_stat_analyses_toutes_staq <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses_toutes_staq.rds"
  )



# Define UI for application that draws a histogram
ui <- navbarPage(
 title=div(img(src="favicon.ico"), "Prototype de l'interface de libreSQE"),
  theme = bslib::bs_theme(
    version = 5,
    primary = "#00218f",
    success = "#33b5ff",
    info = "#00C9C4",
    warning = "#ffb428",
    base_font = "Segoe UI Symbol",
    heading_font = "Georgia",
    font_scale = NULL,
    `enable-gradients` = TRUE,
    bootswatch = "cerulean"
  ),
  tabPanel("Synthèse"),
  navbarMenu(
    ##### UI ANALYSES #####
    "Analyses",
    tabPanel("Mes analyses à qualifier",
             fluidRow(
               column(
                 2,
                 "Sélection du jeu de données",
                 selectInput(
                   "select_marche_prog_annuelle_a_importer",
                   "Sélectionnez le marché concerné",
                   choices = c(
                     "Marché labo n°1 - 2022",
                     "Marché labo n°2 - 2022",
                     "Marché labo n°1 - 2023-2025",
                     "Marché labo n°3 - 2023"
                   )
                 ),
                 selectInput(
                   "select_bdc",
                   "Sélectionnez le bon de commande concerné",
                   choices = c(
                     "2022-3_UGVO_pluie_avril_2022",
                     "2022-3_UGVO_calendaire_avril_2022"
                   )
                 ),
                 selectInput(
                   "select_depot",
                   "Sélectionnez le dépôt concerné",
                   choices = c("2022-12-01_dépôt1_carso")
                 ),
                 checkboxGroupInput(
                   "check_statut_depot_a_afficher",
                   label = "Statut des dépôts à afficher",
                   choices = c("en cours", "clôs"),
                   selected = 'en cours'
                 ),
                 checkboxGroupInput(
                   "check_qualification_analyses_a_afficher",
                   label = "Qualification des analyses à afficher",
                   choices = c("Correcte",
                               "Incorrecte",
                               "Incertaine",
                               "Non qualifié"),
                   selected = c("Incorrecte", "Incertaine")
                 ),
                 checkboxGroupInput(
                   "check_statut_analyses_a_afficher",
                   label = "Statut des analyses à afficher",
                   choices = c("Donnée brute",
                               "Niveau 1",
                               "Niveau 2")
                 )
               ),
               column(
                 8,
                 DTOutput("dt_analyses_a_qualifier"),
                 actionButton(
                   "btn_valide_selection",
                   "Valider l'ensemble des données affichées"
                 ),
                 actionButton(
                   "btn_invalide_selection",
                   "Invalider l'ensemble des données affichées"
                 ),
                 p(
                   "On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "
                 ),
                 p(
                   "S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"
                 ),
                 p(
                   "bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"
                 ),
                 p(
                   "boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé"
                 )

               )
             )),
    tabPanel("Rechercher / éditer des analyses",
             fluidRow(
               column(
                 2,
                 "Sélection du jeu de données",
                 p(
                   "Constructeur de requête SQL avec choix (ou pas si non renseigné)
                        sur code station, code paramètre, code fraction, code unité,
                        date début, date fin prélèvement, dispositif de collecte, laboratoire,
                        préleveur, qualification, ..."
                 )
               ),
               column(
                 8,
                 DTOutput("dt_analyses_a_qualifier2"),
                 p(
                   "possibilité d'éditer certaines valeurs (résultat d'analyses, LQ, LD, code unité, ... dans le DT)"
                 ),
                 actionButton(
                   "btn_valide_selection",
                   "Valider l'ensemble des données affichées"
                 ),
                 actionButton(
                   "btn_invalide_selection",
                   "Invalider l'ensemble des données affichées"
                 ),
                 p(
                   "On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "
                 ),
                 p(
                   "S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"
                 ),
                 p(
                   "bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"
                 ),
                 p(
                   "boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé"
                 )

               )
             ))
  ),
  ##### UI LIVRABLES #####
  navbarMenu(
    "Livrables",
    tabPanel(
      "Déposer un fichier xml / QUESU / EDILABO",
      selectInput(
        "select_bdc",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "2022-3_UGVO_pluie_avril_2022",
          "2022-3_UGVO_calendaire_avril_2022"
        )
      ),
      selectInput(
        "select_prestataire_emetteur_xml",
        "Emetteur xml",
        choices = c(
          "Prestataire en charge du marché 1",
          "Prestataire en charge du marché 2"
        )
      ),
      selectInput("select_mois",
                  "provisoire : mois à afficher",
                  choices = c(unique(
                    prog_previsionnelle$mois
                  ))),
      selectInput(
        "select_rattachement",
        "provisoire : rattachement devis",
        choices = c(unique(prog_previsionnelle$rattachement_devis))
      ),
      selectInput(
        "select_perimetre_fact",
        "provisoire : perimetre facturation",
        choices = c(unique(
          prog_previsionnelle$perimetre_facturation
        ))
      ),
      fileInput("import_xml",
                label = "Sélectionner fichier xml de résultats"),
      actionButton("btn_import_xml", "Déposer le fichier sélectionné sur le serveur"),
      actionButton("btn_actualise_bdc", "Actualiser traitement avec nlle réference bon de commande"),
      p(
        "formats acceptés : EDILABO, QUESU v2, QUESU v3, QUESU v3.1.
               Une fois importé le fichier fait l'objet des scripts de tests"
      )
    ),
    tabPanel(
      "Rapport de dépôt",
      selectInput(
        "select_bdc",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "2022-3_UGVO_pluie_avril_2022",
          "2022-3_UGVO_calendaire_avril_2022"
        )
      ),
      selectInput(
        "select_depot",
        "Sélectionnez le dépôt concerné",
        choices = c("2022-12-01_dépôt1_carso")
      ),
      h1("Synthèse du dépôt"),
      p(
        "Nb de lignes déposées, nb de stations concernées, tableau du nb de résultat classé correct, incertain , incorrect,
               nb de station manquant, nb de run analytiques incomplets, ..."
      ),
      h1("onglets qui détaillent les non conformités"),
      actionButton("btn_emettre_rapport_depot", "Emettre rapport de dépôt"),
      p(
        "boutton qui actionne la génèse d'un fichier excel qui liste les non conformités"
      ),
      actionButton("btn_clore_depot", "Clôtre le dépôt"),
      p(
        "Clore le dépôt ouvre une fenêtre qui indique qu'en clôturant le
               dépôt toutes les analyses qualifiée en niveau 1 (automatique)
               basculent en niveau 2. S'il reste des analyses incertaines,
               fenêtre d'alerte demandant de confirmer la clôture"
      ),
      tabsetPanel(
        tabPanel("nb données par station",
                 DTOutput("tbl_data_par_staq")),
        tabPanel("stations manquantes",
                 DTOutput("tbl_staq_missing")),
        tabPanel(
          "stations manquantes (cond env)",
          DTOutput("tbl_staq_missing_cond_env")
        ),
        tabPanel(
          "stations manquantes (opér)",
          DTOutput("tbl_staq_missing_oper")
        ),

        tabPanel(
          "Résultats analyses manquants",
          DTOutput("tbl_result_missing")
        ),
        tabPanel("Résultats analyses en +",
                 DTOutput("tbl_result_en_trop")),
        tabPanel(
          "Vérif du code dispositif de collecte",
          DTOutput("tbl_verif_rdd")
        ),
        tabPanel("Vérif du respect des LQ",
                 DTOutput("tbl_LQ_ko")),
        tabPanel("Vérif des accréditations",
                 DTOutput("tbl_accred_ko")),
        tabPanel("Vérif des incertitudes",
                 DTOutput("tbl_incert_ko")),
        tabPanel("Vérif des méthodes",
                 DTOutput("tbl_methode_ko"))
      )

    )
  ),
  ###### UI BDC #####
  navbarMenu(
    "Commandes",
    tabPanel(
      "Créer / éditer un bon de commande / émettre fichier EDILABO",
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_perimetre_fact",
        "Sélectionnez le périmètre de facturation concerné",
        choices = c("UGVO",
                    "UGVE",
                    "UGVE-Pont Billon",
                    "UGVE Vallière")
      ),
      selectInput(
        "select_annee_facturation",
        "Sélectionnez l'année",
        choices = c("2022",
                    "2023",
                    "2024"),
        multiple = FALSE
      ),
      selectInput(
        "select_mois_facturation",
        "Sélectionnez le ou les mois concernés",
        choices = c(
          "janvier",
          "février",
          "mars",
          "avril",
          "mai",
          "juin",
          "juillet",
          "août",
          "septembre",
          "octobre",
          "novembre",
          "décembre"
        ),
        multiple = TRUE
      ),
      actionButton("btn_generer_bdc", "Générer le bon de commande"),
      p(
        "générer le bon de commande regarde si le bon de commande n'est pas préexistant.
               S'il n'est pas préexistant alors un tableau de synthèse est créé qui
               liste les prestations et le nombre attendu sur le bon de commande ainsi qu'un
               tableau détaillé qui liste par station chaque prélèvement attendu
               avec une date prévisionnelle (fixée au 1er du mois si une presta / mois,
               au 1er et au 15 si 2 prestas, au 1er, au 10 et au 20 si 3 prestas,
               au 1er, 7, 14 et 28 si 4 prestas, ...
               "
      ),
      h1("tableau de synthèse"),
      p(
        "En plus des informations affichées, le bon de commande comprends également le montant des prestations correspondantes.
               En dessous est affiché le montant total."
      ),
      DTOutput("DT_synthese_bdc"),
      p(
        "Si besoin on peut ajouter des prestations non directement validables : la fourniture de flacons,
               les réunions, les prélèvements d'eau calendaires ou pluie, ..."
      ),
      actionButton(
        "btn_ajouter_presta",
        "Ajouter des prestataions au bon de commande"
      ),
      h1("détail par station"),
      DTOutput("DT_detail_bdc"),
      actionButton(
        "btn_éditer_tableau de prestation",
        "Editer tableau de prestations"
      ),
      p(
        "le bouton permet d'éditer le tableau de prestation s'il faut ajouter ou supprimer une ou des stations,
               modifier des dates, ...
               En cas d'activation, la prog annuelle est également modifiée en conséquent"
      ),
      actionButton("btn_générer_bdc_pdf", "Générer le bon de commande en pdf"),
      actionButton(
        "btn_générer_demande_edilabo",
        "Générer demande prestation EDILABO"
      )
    ),
    tabPanel(
      "Avancement du bon de commande / facturation",
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      p(
        "les choix du sélecteur de bon de commande affiche déjà les bdc non clôs puis ceux programmés et enfin ceux clôs "
      ),
      selectInput(
        "select_marche_bdc_a_importer",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "UGVE - Pont-Billon - Pluie - août 2022 (en cours)",
          "UGVE - Calendaire - avril 2022 (en cours)",
          "UGVE - Calendaire - mai 2022 (en cours)",
          "UGVE - Calendaire juin 2022 (programmé)",
          "UGVE - Calendaire janvier 2022 (clôs)"
        )
      ),
      checkboxGroupInput(
        "check_statut_bdc_a_afficher",
        label = "Statut des bons de commande à afficher",
        choices = c("programmé", "en cours", "clôs")
      ),
      p("Sortie texte qui affiche le statut du bon de commande"),
      actionButton("modifie_statut", "Modifier le statut du bon de commande"),
      textInput("commentaire_sur_bdc", "Commentaire lu dans la base bdc"),
      actionButton(
        "modifie_commentaire",
        "Modifier le commentaire du bon de commande"
      ),
      p(
        "tableau avec l'avancement du bon de commande par prestation (1 ligne par station / prestation avec un avancement)"
      ),
      p(
        "tableau de synthèse par prestation avec nombre commandé / en cours / validé"
      )

    )
  ),
  ###### UI Marchés #######
  navbarMenu(
    "Marchés",
    tabPanel(
      "Créer / éditer un marché",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      textInput("nom_nouveau_marche", label = "Nom du marché"),
      textInput("code_nouveau_marche", label = "Référence du marché (ex. 2022-65)"),
      dateInput("date_debut_nouveau_marche", label = "Date début validité du marché"),
      dateInput("date_fin_nouveau_marche", label = "Date fin de validité du marché"),
      selectInput(
        "prestataire_marche",
        "Titulaire du marché",
        choices = c("Liste des intervenants", "Intervenant 1", "Intervenant 2", "...")
      ),
      p(
        "Ajouter tableau avec la liste des marchés en cours, leur titulaire, ..."
      )
    ),
    tabPanel(
      "Importer BPU et programmes types",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      p(
        "le BPU comporte 3 onglets Excel : un avec le prix unitaire de chaque prestation,
               un avec le cout de chaque run analytique et un avec les programmes types"
      ),
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      fileInput("import_bpu",
                label = "Importer fichier xlsx bpu_prog_type"),
      actionButton("btn_import_bpu", "Importer le fichier sélectionné"),
      p(
        "une fois importé faire un rapport d'import qui indique si le fichier
               a bien été importé ou bien, s'il n'est pas conforme, les raisons de sa non conformité."
      )
    ),
    tabPanel(
      "Modifier BPU",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      selectInput(
        "select_marche_bpu_a_editer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_annee_bpu_a_editer",
        "Sélectionnez l'année concernée (choix parmi les années entre le début et la fin du marché)",
        choices = c("2022")
      ),
      p(
        "Affichage les 3 tableaux BPU, BPU des run analytiques et programmes types.
        Possibilité d'éditer certains champs (et en cascade de mettre  à jour les tables correspondantes) :
        code / nom paramètres, dates, nb de presta commandées, ..."
      ),
      DTOutput("DT_bpu"),
      DTOutput("DT_bpu_run_analytiques"),
      DTOutput("DT_prog_types")
    ),
    tabPanel(
      "Importer programmation annuelle",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      p(
        "la programmation annuelle comporte 2 onglets Excel : un avec la liste des stations et le code de leur programme annuel,
               et un avec le code des programmes annueles et les prestations et dates d'intervention correspondant"
      ),
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      fileInput("import_pgm_annuelle",
                label = "Importer fichier xlsx programmation annuelle"),
      actionButton("btn_import_pgm_annuelle", "Importer le fichier sélectionné"),
      p(
        "une fois importé faire un rapport d'import qui indique si le fichier
               a bien été importé ou bien, s'il n'est pas conforme, les raisons de sa non conformité."
      )
    ),
    tabPanel(
      "Editer programmation annuelle",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      selectInput(
        "select_marche_prog_annuelle_a_editer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_annee_prog_a_editer",
        "Sélectionnez l'année concernée (choix parmi les années entre le début et la fin du marché)",
        choices = c("2022")
      ),
      p("Affichage de la prog annuelle"),
      DTOutput("DT_prog_annuelle_stations"),
      DTOutput("DT_prog_annuelle_programmation")
    ),
    tabPanel(
      "Avancement du marché",
      selectInput(
        "select_marche_avancement",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      p(
        "graph en barres avec montant max marché, montant prévu, montant commandé, montant payé"
      ),
      plotOutput("graph_avancement_financier", width = "650px"),
      p(
        "Graph avec avancement des prélèvements (nb commandé, nb réalisé, nb annulé assec, ..."
      ),
      plotOutput("graph_avancement_prelevement", width = "650px")
    ),
    tabPanel(
      "Editer un jeu de fiches stations",
      selectInput(
        "select_marche_fiches_stations",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectizeInput(
        "select_liste_stations_marche",
        "Liste des stations dont il faut éditer les fiches (choix parmi celles du marché actif)",
        choices = c(
          "Station 1 du marché actif",
          "Station 2 du marché actif",
          "Station 3 du marché actif",
          "Station 4 du marché actif"
        ),
        multiple = TRUE
      ),
      actionButton(
        "select_liste_stations_marche_all",
        "Sélectionner toutes les stations du marché"
      ),
      actionButton(
        "generer_fiche_stations_de_la_liste",
        "Générer les fiches des stations sélectionnées en pdf (1 pdf par station)"
      ),
      actionButton(
        "generer_fiche_stations_de_la_liste_tout_1_coup",
        "Générer les fiches des stations sélectionnées en pdf (1 pdf pour ensemble des stations)"
      )

    )
  ),
  navbarMenu(
    "Référentiels",
    tabPanel(
      "Référentiels SANDRE",
      DTOutput("table_derniere_maj_SANDRE"),
      selectInput(
        "referentiel_a_maj",
        "Sélectionnez le référentiel SANDRE à actualiser",
        choices = tableau_maj_ref$ts_nom_referentiel
      ),
      actionButton(
        "btn_charge_ref",
        "Actualiser le référentiel sélectionné",
        icon = icon("check")
      ),
      actionButton(
        "btn_charge_tous_ref",
        "Actualiser tous les référentiels SANDRE",
        icon = icon("check")
      )
    ),
    tabPanel(
      "Visualiser / éditer fiches stations de mesures",
      selectInput(
        "selecteur_fiche_staq",
        "Fiche station à visualiser / éditer",
        choices = c(
          "04123456 - LA VILAINE A TRIFOUILLIS LES OIES",
          "04234567 - L'OUST AU PAYS DES BISOUNOURS"
        )
      ),
      actionButton(
        "btn_editer_fiche_terrain_pt_prel",
        "Editer fiche terrain en pdf",
        icon = icon("check")
      ),
      verbatimTextOutput("text_code_interne_station"),
      verbatimTextOutput("date_last_maj_SANDRE"),
      verbatimTextOutput("date_last_maj_EV"),
      h1("Carte de localisation"),
      p("Localisation selon le SANDRE et selon E&V + point de stationnement"),
      plotOutput("carto_station", width = "650px"),
      verbatimTextOutput("text_distance_station_station"),
      actionButton(
        "btn_deplacer_pt_prel",
        "Déplacer le point de prélèvement",
        icon = icon("check")
      ),
      actionButton(
        "btn_deplacer_pt_stationnement",
        "Déplacer le point de stationnement",
        icon = icon("check")
      ),
      p(
        "Si on appuie sur les boutons de déplacement, ouverture d'une fenêtre
        demandant les nouveaux X Y avec bouton valider / annuler déplacement
        + carte de visualisation des XY saisis."
      ),
      h1("accès station"),
      verbatimTextOutput("text_acces_station"),
      actionButton(
        "btn_editer_text_acces",
        "Editer texte accès station",
        icon = icon("check")
      ),
      h1("Précisions sur prélèvement"),
      verbatimTextOutput("text_prelevement"),
      actionButton(
        "btn_editer_prelevement",
        "Editer précisions prélèvement",
        icon = icon("check")
      ),
      h1("Commentaires généraux sur la station"),
      verbatimTextOutput("text_commentaire_station"),
      actionButton(
        "btn_editer_commentaires",
        "Editer commentaires",
        icon = icon("check")
      ),
      h1("Photos"),
      p("1 photo + 1 légende par photo de la station"),
      actionButton(
        "btn_ajout_rempl_photo",
        "ajouter/ remplacer photo",
        icon = icon("check")
      ),
      p(
        "Si on appuie sur le bouton ajout / remplacer photo, ouverture d'une fenetre pour :
        - charger photo (jpg ou png),
        - ajouter légende,
        - indiquer photo à remplacer le cas échéant (liste déroulante + mini image),
        - bouton valider / annuler
        "
      )
    ),
    tabPanel(
      "Créer une nouvelle fiche stations de mesures",
      h3("Cours d'eau"),
      h3("Positionnement"),
      p("Selon le positionnement, afficher commune de localisation"),
      h3("Précisions sur accès"),
      h3('Précisions sur modes de prélèvements'),
      h3("Finalité de la station / autres commentaires"),
      h3("photos"),
      h3("Valider / annuler / Editer fiche en pdf"),
      h3("Préparer fiche demande création station AELB")
    ),
    tabPanel(
      "Périmètres de gestion",
      tabPanel(
        p(
          "Page visible uniquement si la personne connectée a le statut administrateur de marché."
        ),
        h1("Périmètres de gestion"),
        p(
          "Prévoir d'ajouter à la table un statut du périmètre (actif / inactif).
                 Seuls les périmètres actifs pourront être intégrés à un nouveau bon de commande."
        ),
        DTOutput("table_perimetres_de_gestion"),
        actionButton(
          "btn_maj_perimetre_gestion",
          "Modifier un périmètre de gestion",
          icon = icon("check")
        ),
        actionButton(
          "btn_ajouter_per_gest",
          "Ajouter un périmètre de gestion",
          icon = icon("check")
        ),
        actionButton(
          "btn_changer_statut_per_gest",
          "Changer statut périmètre de gestion",
          icon = icon("check")
        ),
      )
    ),
    tabPanel(
      "Rôles des intervenants",
      p(
        "Page visible uniquement si la personne connectée a le statut administrateur de marché."
      ),
      h1("Tableau des intervenants sur le marché"),
      p("Possibilité de définir les labos et les préleveurs")
    )
  ),
  tabPanel(
    "Connexion",
    textInput("identifiant", "Saisir votre identifiant"),
    passwordInput("password", "Saisir votre mot de passe"),
    actionButton("btn_valide_mdp", "Valider", icon =
                   icon("check")),
    p(
      "En cas d'oubli du mot de passe, envoyer un mail à anthony.deburghrave@eaux-et-vilaine.fr"
    ),
    p(
      "Si l'opérateur est connecté, masquer les éléments de connexion et afficher les éléments suivants"
    ),
    verbatimTextOutput("text_connecte_comme"),
    actionButton("btn_deconnecter", "Se déconnecter", icon =
                   icon("xmark")),
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {


  # bon de commande
  output$DT_synthese_bdc <-
    renderDT(
      bdc %>% select(programme:quantite_commandee) %>% group_by(programme) %>% dplyr::summarise(TOTAL :=
                                                                                                  sum(quantite_commandee, na.rm = TRUE))
    )
  output$DT_detail_bdc <-
    renderDT(bdc %>% select(
      `CODE SANDRE STATION`:`NOM STATION`,
      programme:quantite_commandee
    ))


  # BPU

  output$DT_bpu <- renderDT(BPU)
  output$DT_bpu_run_analytiques <- renderDT(cout_run_analytiques)

  output$DT_prog_types <-
    renderDT(programmes_types %>% select(-`LIMITE DE QUANTIFICATION PRECONISEE`, -STATUT))


  # prog annuelle

  output$DT_prog_annuelle_stations <-
    renderDT(
      prog_annuelle %>% select(
        perimetre_facturation,
        `CODE SANDRE STATION`,
        `CODE INTERNE STATION`,
        `NOM STATION`,
        `COMMENTAIRES`,
        `Type station`
      )
    )
  output$DT_prog_annuelle_programmation <-
    renderDT(calendrier %>% dplyr::mutate(total = rowSums(across(janvier:décembre)),
                                          .before =
                                            janvier))


  ##### LIVRABLES #####

  fichier_xml <- reactiveValues(
    analyses_xml = data.frame(),
    cond_environ_xml = data.frame(),
    operations_xml = data.frame(),
    tbl_data_par_staq = data.frame(),
    tbl_staq_missing = data.frame(),
    tbl_staq_missing_cond_env = data.frame(),
    tbl_staq_missing_oper = data.frame(),
    tbl_result_missing = data.frame(),
    tbl_result_en_trop = data.frame(),
    tbl_verif_rdd = character(0),
    tbl_LQ_ko = data.frame(),
    tbl_accred_ko = data.frame(),
    tbl_incert_ko = data.frame(),
    tbl_methode_ko = data.frame(),
    tbl_rs_anal = data.frame()
  )


  output$tbl_data_par_staq <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_data_par_staq),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$tbl_staq_missing <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_staq_missing),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$tbl_result_missing <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_result_missing),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE
    )
  output$tbl_result_en_trop <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_result_en_trop),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)
  output$tbl_LQ_ko <- renderDT({
    datatable(
      req(fichier_xml$tbl_LQ_ko),
      extensions = c("Buttons"),
      options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                     rownames = FALSE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
      filter = 'top'
    )
  }, server = FALSE)
  output$tbl_accred_ko <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_accred_ko),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)
  output$tbl_incert_ko <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_incert_ko),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$tbl_methode_ko <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_methode_ko),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$tbl_staq_missing_cond_env <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_staq_missing_cond_env),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)
  output$tbl_staq_missing_oper <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_staq_missing_oper),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$tbl_verif_rdd <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_verif_rdd),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  output$dt_analyses_a_qualifier <-
    renderDT({
      datatable(
        req(fichier_xml$tbl_rs_anal),
        extensions = c("Buttons"),
        options = list(pageLength = 5, lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),

                       rownames = FALSE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        filter = 'top'
      )
    }, server = FALSE)

  observeEvent(input$btn_import_xml,
               {
                 gc()

                 if (!is.null(input$import_xml$datapath)) {
                   show_modal_spinner(text = "Import du fichier en cours")
                   tmp <- import_QESU_PHY_v3(input$import_xml$datapath)
                   fichier_xml$analyses_xml <- tmp$analyses
                   fichier_xml$cond_environ_xml <- tmp$cond_env
                   fichier_xml$operations_xml <- tmp$operations

                   update_modal_spinner(text = "Traitement du fichier en cours")
                   analyses_xml <- fichier_xml$analyses_xml
                   analyses_xml<-analyses_xml%>%subset(!is.na(RsAna))
                   cond_environ_xml <- fichier_xml$cond_environ_xml
                   operations_xml <- fichier_xml$operations_xml

                   devis <-
                     prog_previsionnelle %>% subset(
                       perimetre_facturation == input$select_perimetre_fact &
                         rattachement_devis == input$select_rattachement &
                         mois == input$select_mois
                     )

                   analyses_attendues_devis <-
                     left_join(devis, programmes_types, by = c("programme" = "PROGRAMME"))

                   analyses_attendues_devis <-
                     left_join(analyses_attendues_devis,
                               cout_run_analytiques,
                               by = c("RUN ANALYTIQUE"))


                   ##### Stations manquantes dans analyses #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!(
                       analyses_attendues_devis$`CODE SANDRE STATION` %in% analyses_xml$CdStationMesureEauxSurface
                     )] %>% unique()

                   station_manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   station_manquant <- ajoute_nom_station(station_manquant)

                   fichier_xml$tbl_staq_missing <- station_manquant

                   ##### Stations manquantes dans cond env #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!analyses_attendues_devis$`CODE SANDRE STATION` %in%
                                                                      cond_environ_xml$CdStationMesureEauxSurface] %>% unique()

                   manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   manquant <- ajoute_nom_station(manquant)

                   fichier_xml$tbl_staq_missing_cond_env <- manquant

                   ##### Stations manquantes dans opérations #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!analyses_attendues_devis$`CODE SANDRE STATION` %in%
                                                                      operations_xml$CdStationMesureEauxSurface] %>% unique()

                   manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   manquant <- ajoute_nom_station(manquant)

                   fichier_xml$tbl_staq_missing_oper <- manquant

                   ##### Analyses en + / en - #####
                   analyses_attendues_devis$cle <-
                     paste0(
                       analyses_attendues_devis$`CODE SANDRE STATION`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE FRACTION`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE PARAMETRE`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE UNITE`,
                       "_",
                       ifelse(
                         tolower(analyses_attendues_devis$`ANALYSES IN-SITU`) == "oui",
                         "1",
                         "2"
                       )
                     )

                   synthese_analyses_attendues <-
                     analyses_attendues_devis %>% group_by(cle) %>% dplyr::count(name = "n_prog")

                   analyses_xml <-
                     analyses_xml %>% ajoute_nom_fraction() %>% ajoute_nom_unite()
                   analyses_xml$cle <- paste0(
                     analyses_xml$CdStationMesureEauxSurface,
                     "_",
                     analyses_xml$CdFractionAnalysee,
                     "_",
                     analyses_xml$CdParametre,
                     "_",
                     analyses_xml$CdUniteMesure,
                     "_",
                     analyses_xml$CdInsituAna
                   )

                   cle_analyses_xml <-
                     analyses_xml %>% group_by(cle) %>% dplyr::count(name = "n_xml")


                   delta_rendu <- full_join(synthese_analyses_attendues,
                                            cle_analyses_xml,
                                            by = "cle")

                   delta_rendu <- delta_rendu %>% replace_na(list(n_xml = 0,
                                                                  n_prog = 0))
                   delta_rendu$manquant <-
                     delta_rendu$n_prog - delta_rendu$n_xml


                   # analyses attendues et absentes du rendu (hors station manquante)
                   delta_manquant <- delta_rendu %>% subset(manquant > 0)

                   fichier_xml$tbl_result_missing <-
                     analyses_attendues_devis %>%
                     subset(cle %in% delta_manquant$cle) %>%
                     subset(!(
                       `CODE SANDRE STATION` %in% station_manquant$CdStationMesureEauxSurface
                     ))%>%select(-COMMUNE, -`Coord_X (L93)`,
                                 -`Coord_Y (L93)`,
                                 -COMMENTAIRES,
                                 -`PT INSITU`,
                                 -`PT PC BASE`,
                                 -`PT PESTICIDES ET EROSION`,
                                 -`LIEU DIT`,
                                 -`PREL PRELEVEMENT TPS DE PLUIE REGIE`,
                                 -`PREL PRELEVEMENT TPS DE PLUIE EXTERNALISE`,
                                 -`PREL PRELEVEMENT CALENDAIRE EXTERNALISE`,
                                 -`Type station`,
                                 -`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE`,
                                 -`LIMITE DE QUANTIFICATION PRECONISEE`,
                                 -INCERTITUDE,
                                 -`TYPE D'INCERTITUDE (relative ou absolue)`,
                                 -`ACCREDITATION (oui / non)`,
                                 -`renseignements que le candidat souhaite porter à la connaissance du pouvoir adjudicateur pour ce paramètre (optionnel)`,
                                 -`Prix unitaire € HT`, -cle)

                   # analyses rendues et non attendues
                   delta_enplus <- delta_rendu %>% subset(manquant < 0)

                   analyses_enplus <-
                     analyses_xml %>%
                     subset(cle %in% delta_enplus$cle) %>%
                     subset(
                       !(
                         CdStationMesureEauxSurface %in% station_manquant$CdStationMesureEauxSurface
                       )
                     )

                   fichier_xml$tbl_result_en_trop <-
                     ajoute_nom_param(analyses_enplus)

                   #####nb données par station #####
                   nb_anal_par_station<-analyses_xml%>%dplyr::group_by(CdStationMesureEauxSurface)%>%dplyr::count()
                   nb_anal_par_station<-ajoute_nom_station(nb_anal_par_station)
                   fichier_xml$tbl_data_par_staq<-nb_anal_par_station

                   ##### Verif code Rdd #####
                   # ajout nom reseaux
                   anal_reseaux <-
                     analyses_xml %>% select(CdStationMesureEauxSurface, CdRdd)

                   # on détermine le nb max de codes réseaux pour une même analyse
                   tmp <-
                     gsub("[^//]+", "", anal_reseaux$CdRdd %>%
                            unique()) %>%
                     nchar() %>%
                     max(na.rm = T) + 1

                   # on split la colonne des codes reseaux pour séparer les codes multiples
                   anal_reseaux <-
                     separate(
                       anal_reseaux,
                       col = CdRdd,
                       sep = "[//]",
                       into = paste0("CdRdd", seq(1:tmp)),
                       extra = "drop"
                     )

                   # on transforme le resultat en format long
                   anal_reseaux <-
                     anal_reseaux %>% pivot_longer(
                       cols = paste0("CdRdd", seq(1:tmp)),
                       names_to = "numero",
                       values_to = "CdRdd",
                       values_drop_na = TRUE
                     )

                   # ajout des noms de reseaux
                   anal_reseaux <- anal_reseaux %>% ajoute_nom_cdreseaumesure()
                   fichier_xml$tbl_verif_rdd <-
                     anal_reseaux %>% group_by(NomRdd) %>% dplyr::count()



                   ##### Vérif LQ #####

                   analyses_attendues_devis$`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` <-
                     analyses_attendues_devis$`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` %>%
                     as.numeric()

                   analyses_attendues_devis$`ACCREDITATION (oui / non)` <-
                     tolower(analyses_attendues_devis$`ACCREDITATION (oui / non)`)

                   analyses_attendues_devis$INCERTITUDE <-
                     analyses_attendues_devis$INCERTITUDE %>% as.numeric()

                   # creation d'un tableau qui joint les analyses attendues avec celles rendues

                   comparatif <-
                     left_join(analyses_attendues_devis, analyses_xml, by = "cle") %>%
                     subset(!(
                       `CODE SANDRE STATION` %in% station_manquant$CdStationMesureEauxSurface
                     ))

                   # on extrait du tableau les analyses dont les limites de quantification sont non conformes aux engagements du prestataire
                   LQ_insuffisante <-
                     comparatif %>% subset(`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` <
                                             LqAna) %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       `LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE`,
                       LqAna
                     )

                   fichier_xml$tbl_LQ_ko <- LQ_insuffisante %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif Accreditation #####
                   # on extrait du tableau les analyses dont les accréditations sont non conformes aux engagements du prestataire

                   Accreditation_manquante <- comparatif %>%
                     subset(`ACCREDITATION (oui / non)` == "oui" &
                              CdAccreAna != 1) %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       CommentairesAna
                     )

                   fichier_xml$tbl_accred_ko <- Accreditation_manquante %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif incertitude #####
                   # on extrait du tableau les analyses dont les incertitudes sont non conformes aux engagements du prestataire

                   Incertitude_non_conforme <- comparatif %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       RsAna,
                       INCERTITUDE,
                       IncertAna,
                       CommentairesAna
                     ) %>% subset(IncertAna > INCERTITUDE)


                   fichier_xml$tbl_incert_ko <- Incertitude_non_conforme %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif méthodes #####
                   # on extrait du tableau les analyses dont les méthodes sont non conformes aux engagements du prestataire

                   Methode_non_conforme <- comparatif %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       `Code méthode SANDRE`,
                       Méthode,
                       CdMethode,
                       CommentairesAna
                     ) %>% subset(`Code méthode SANDRE` != CdMethode)

                   fichier_xml$tbl_methode_ko <- Methode_non_conforme %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite() %>%
                     ajoute_nom_methode()


                   ##### Qualification analyses #####
                   analyses_xml$cle_frac_unit <- paste0(
                     analyses_xml$CdParametre,
                     "_",
                     analyses_xml$CdFractionAnalysee,
                     "_",
                     analyses_xml$CdUniteMesure
                   )

                   analyses_xml$cle_staq_frac_unit <-
                     paste0(
                       analyses_xml$CdStationMesureEauxSurface,
                       "_",
                       analyses_xml$CdParametre,
                       "_",
                       analyses_xml$CdFractionAnalysee,
                       "_",
                       analyses_xml$CdUniteMesure
                     )
                   verif <- analyses_xml %>%
                     left_join(table_stat_analyses, by = "cle_staq_frac_unit") %>%
                     left_join(table_stat_analyses_toutes_staq, by = "cle_frac_unit") %>%
                     subset(CdRqAna == "1")

                   ##### qualif par station
                   verif$classement_par_station <- 99

                   # classe 1
                   verif$classement_par_station <-
                     ifelse(
                       verif$RsAna >= verif$Q10ST &
                         verif$RsAna <= verif$Q90ST,
                       1,
                       verif$classement_par_station
                     )

                   # classe 2
                   verif$classement_par_station <- ifelse(
                     (verif$RsAna >= verif$Q5ST & verif$RsAna < verif$Q10ST) |
                       (verif$RsAna <= verif$Q95ST &
                          verif$RsAna > verif$Q90ST),
                     2,
                     verif$classement_par_station
                   )

                   # classe 3
                   verif$classement_par_station <- ifelse(
                     (verif$RsAna >= verif$minST & verif$RsAna < verif$Q5ST) |
                       (verif$RsAna <= verif$maxST &
                          verif$RsAna > verif$Q95ST),
                     3,
                     verif$classement_par_station
                   )

                   # classe 7
                   verif$classement_par_station <- ifelse(
                     (
                       verif$RsAna >= (verif$minST - verif$sdST) &
                         verif$RsAna < verif$minST
                     ) |
                       (
                         verif$RsAna <= (verif$maxST + verif$sdST) &
                           verif$RsAna > verif$maxST
                       ),
                     7,
                     verif$classement_par_station
                   )

                   # classe 9
                   verif$classement_par_station <- ifelse(
                     (
                       verif$RsAna >= (verif$minST - 2 * verif$sdST) &
                         verif$RsAna < (verif$minST - verif$sdST)
                     ) |
                       (
                         verif$RsAna <= (verif$maxST + 2 * verif$sdST) &
                           verif$RsAna > (verif$maxST + verif$sdST)
                       ),
                     9,
                     verif$classement_par_station
                   )

                   # classe 10
                   verif$classement_par_station <-
                     ifelse((verif$RsAna <= (verif$minST - 2 *
                                               verif$sdST)) |
                              (verif$RsAna >= (verif$maxST + 2 * verif$sdST)),
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
                       verif$RsAna >= verif$Q10 &
                         verif$RsAna <= verif$Q90,
                       2,
                       verif$classement_toutes_station
                     )

                   # classe 3
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$Q5 & verif$RsAna < verif$Q10) |
                       (verif$RsAna <= verif$Q95 &
                          verif$RsAna > verif$Q90),
                     3,
                     verif$classement_toutes_station
                   )

                   # classe 4
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$Q1 & verif$RsAna < verif$Q5) |
                       (verif$RsAna <= verif$Q99 &
                          verif$RsAna > verif$Q95),
                     4,
                     verif$classement_toutes_station
                   )

                   # classe 6
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$min & verif$RsAna < verif$Q1) |
                       (verif$RsAna <= verif$max &
                          verif$RsAna > verif$Q99),
                     6,
                     verif$classement_toutes_station
                   )

                   # classe 8
                   verif$classement_toutes_station <- ifelse(
                     (
                       verif$RsAna >= (verif$min - verif$sd) &
                         verif$RsAna < verif$min
                     ) |
                       (
                         verif$RsAna <= (verif$max + verif$sd) &
                           verif$RsAna > verif$max
                       ),
                     8,
                     verif$classement_toutes_station
                   )


                   # classe 10
                   verif$classement_toutes_station <-
                     ifelse((verif$RsAna <= (verif$min -
                                               verif$sd)) |
                              (verif$RsAna >= (verif$max + verif$sd)),
                            10,
                            verif$classement_toutes_station)

                   # classe 0
                   verif$classement_toutes_station <-
                     ifelse(is.na(verif$classement_toutes_station),
                            0,
                            verif$classement_toutes_station)

                   ##### agrégation des vérifications
                   verif$classement <-
                     ifelse(verif$classement_toutes_station == 0 &
                              verif$classement_par_station == 0,
                            3,
                            4)

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2, 3, 4) &
                         verif$classement_par_station %in% c(0, 1, 2),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2, 3) &
                         verif$classement_par_station %in% c(3),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(6) &
                         verif$classement_par_station %in% c(3),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(6) &
                         verif$classement_par_station %in% c(0, 1, 2, 3, 7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(4) &
                         verif$classement_par_station %in% c(3, 7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(3) &
                         verif$classement_par_station %in% c(7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2) &
                         verif$classement_par_station %in% c(7, 9),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(verif$classement_toutes_station %in% c(8, 10),
                            2,
                            verif$classement)

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(3, 4, 6) &
                         verif$classement_par_station %in% c(9, 10),
                       2,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2) &
                         verif$classement_par_station %in% c(10),
                       2,
                       verif$classement
                     )


                   verif$classement <- as.character(verif$classement)

                   fichier_xml$tbl_rs_anal <-
                     verif %>% select(
                       CdStationMesureEauxSurface:CdParametre,
                       RsAna,
                       classement_par_station:last_col()
                     ) %>% ajoute_nom_param() %>% ajoute_nom_station() %>% ajoute_nom_cdqualana(col_qualif = "classement")

                   remove_modal_spinner()
                 }
               })

  observeEvent(input$btn_actualise_bdc,
               {
                 gc()

                 if (!is.null(input$import_xml$datapath)) {
                   show_modal_spinner(text = "Import du fichier en cours")

                   update_modal_spinner(text = "Traitement du fichier en cours")
                   analyses_xml <- fichier_xml$analyses_xml
                   analyses_xml<-analyses_xml%>%subset(!is.na(RsAna))
                   cond_environ_xml <- fichier_xml$cond_environ_xml
                   operations_xml <- fichier_xml$operations_xml

                   devis <-
                     prog_previsionnelle %>% subset(
                       perimetre_facturation == input$select_perimetre_fact &
                         rattachement_devis == input$select_rattachement &
                         mois == input$select_mois
                     )

                   analyses_attendues_devis <-
                     left_join(devis, programmes_types, by = c("programme" = "PROGRAMME"))

                   analyses_attendues_devis <-
                     left_join(analyses_attendues_devis,
                               cout_run_analytiques,
                               by = c("RUN ANALYTIQUE"))


                   ##### Stations manquantes dans analyses #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!(
                       analyses_attendues_devis$`CODE SANDRE STATION` %in% analyses_xml$CdStationMesureEauxSurface
                     )] %>% unique()

                   station_manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   station_manquant <- ajoute_nom_station(station_manquant)

                   fichier_xml$tbl_staq_missing <- station_manquant

                   ##### Stations manquantes dans cond env #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!analyses_attendues_devis$`CODE SANDRE STATION` %in%
                                                                      cond_environ_xml$CdStationMesureEauxSurface] %>% unique()

                   manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   manquant <- ajoute_nom_station(manquant)

                   fichier_xml$tbl_staq_missing_cond_env <- manquant

                   ##### Stations manquantes dans opérations #####
                   liste_cd_station_abs_analyses <-
                     analyses_attendues_devis$`CODE SANDRE STATION`[!analyses_attendues_devis$`CODE SANDRE STATION` %in%
                                                                      operations_xml$CdStationMesureEauxSurface] %>% unique()

                   manquant <-
                     data.frame(CdStationMesureEauxSurface = liste_cd_station_abs_analyses)
                   manquant <- ajoute_nom_station(manquant)

                   fichier_xml$tbl_staq_missing_oper <- manquant

                   ##### Analyses en + / en - #####
                   analyses_attendues_devis$cle <-
                     paste0(
                       analyses_attendues_devis$`CODE SANDRE STATION`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE FRACTION`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE PARAMETRE`,
                       "_",
                       analyses_attendues_devis$`CODE SANDRE UNITE`,
                       "_",
                       ifelse(
                         tolower(analyses_attendues_devis$`ANALYSES IN-SITU`) == "oui",
                         "1",
                         "2"
                       )
                     )

                   synthese_analyses_attendues <-
                     analyses_attendues_devis %>% group_by(cle) %>% dplyr::count(name = "n_prog")

                   analyses_xml <-
                     analyses_xml %>% ajoute_nom_fraction() %>% ajoute_nom_unite()
                   analyses_xml$cle <- paste0(
                     analyses_xml$CdStationMesureEauxSurface,
                     "_",
                     analyses_xml$CdFractionAnalysee,
                     "_",
                     analyses_xml$CdParametre,
                     "_",
                     analyses_xml$CdUniteMesure,
                     "_",
                     analyses_xml$CdInsituAna
                   )

                   cle_analyses_xml <-
                     analyses_xml %>% group_by(cle) %>% dplyr::count(name = "n_xml")


                   delta_rendu <- full_join(synthese_analyses_attendues,
                                            cle_analyses_xml,
                                            by = "cle")

                   delta_rendu <- delta_rendu %>% replace_na(list(n_xml = 0,
                                                                  n_prog = 0))
                   delta_rendu$manquant <-
                     delta_rendu$n_prog - delta_rendu$n_xml


                   # analyses attendues et absentes du rendu (hors station manquante)
                   delta_manquant <- delta_rendu %>% subset(manquant > 0)

                   fichier_xml$tbl_result_missing <-
                     analyses_attendues_devis %>%
                     subset(cle %in% delta_manquant$cle) %>%
                     subset(!(
                       `CODE SANDRE STATION` %in% station_manquant$CdStationMesureEauxSurface
                     ))%>%select(-COMMUNE, -`Coord_X (L93)`,
                                 -`Coord_Y (L93)`,
                                 -COMMENTAIRES,
                                 -`PT INSITU`,
                                 -`PT PC BASE`,
                                 -`PT PESTICIDES ET EROSION`,
                                 -`LIEU DIT`,
                                 -`PREL PRELEVEMENT TPS DE PLUIE REGIE`,
                                 -`PREL PRELEVEMENT TPS DE PLUIE EXTERNALISE`,
                                 -`PREL PRELEVEMENT CALENDAIRE EXTERNALISE`,
                                 -`Type station`,
                                 -`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE`,
                                 -`LIMITE DE QUANTIFICATION PRECONISEE`,
                                 -INCERTITUDE,
                                 -`TYPE D'INCERTITUDE (relative ou absolue)`,
                                 -`ACCREDITATION (oui / non)`,
                                 -`renseignements que le candidat souhaite porter à la connaissance du pouvoir adjudicateur pour ce paramètre (optionnel)`,
                                 -`Prix unitaire € HT`, -cle)

                   # analyses rendues et non attendues
                   delta_enplus <- delta_rendu %>% subset(manquant < 0)

                   analyses_enplus <-
                     analyses_xml %>%
                     subset(cle %in% delta_enplus$cle) %>%
                     subset(
                       !(
                         CdStationMesureEauxSurface %in% station_manquant$CdStationMesureEauxSurface
                       )
                     )

                   fichier_xml$tbl_result_en_trop <-
                     ajoute_nom_param(analyses_enplus)

                   #####nb données par station #####
                   nb_anal_par_station<-analyses_xml%>%dplyr::group_by(CdStationMesureEauxSurface)%>%dplyr::count()
                   nb_anal_par_station<-ajoute_nom_station(nb_anal_par_station)
                   fichier_xml$tbl_data_par_staq<-nb_anal_par_station

                   ##### Verif code Rdd #####
                   # ajout nom reseaux
                   anal_reseaux <-
                     analyses_xml %>% select(CdStationMesureEauxSurface, CdRdd)

                   # on détermine le nb max de codes réseaux pour une même analyse
                   tmp <-
                     gsub("[^//]+", "", anal_reseaux$CdRdd %>%
                            unique()) %>%
                     nchar() %>%
                     max(na.rm = T) + 1

                   # on split la colonne des codes reseaux pour séparer les codes multiples
                   anal_reseaux <-
                     separate(
                       anal_reseaux,
                       col = CdRdd,
                       sep = "[//]",
                       into = paste0("CdRdd", seq(1:tmp)),
                       extra = "drop"
                     )

                   # on transforme le resultat en format long
                   anal_reseaux <-
                     anal_reseaux %>% pivot_longer(
                       cols = paste0("CdRdd", seq(1:tmp)),
                       names_to = "numero",
                       values_to = "CdRdd",
                       values_drop_na = TRUE
                     )

                   # ajout des noms de reseaux
                   anal_reseaux <- anal_reseaux %>% ajoute_nom_cdreseaumesure()
                   fichier_xml$tbl_verif_rdd <-
                     anal_reseaux %>% group_by(NomRdd) %>% dplyr::count()



                   ##### Vérif LQ #####

                   analyses_attendues_devis$`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` <-
                     analyses_attendues_devis$`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` %>%
                     as.numeric()

                   analyses_attendues_devis$`ACCREDITATION (oui / non)` <-
                     tolower(analyses_attendues_devis$`ACCREDITATION (oui / non)`)

                   analyses_attendues_devis$INCERTITUDE <-
                     analyses_attendues_devis$INCERTITUDE %>% as.numeric()

                   # creation d'un tableau qui joint les analyses attendues avec celles rendues

                   comparatif <-
                     left_join(analyses_attendues_devis, analyses_xml, by = "cle") %>%
                     subset(!(
                       `CODE SANDRE STATION` %in% station_manquant$CdStationMesureEauxSurface
                     ))

                   # on extrait du tableau les analyses dont les limites de quantification sont non conformes aux engagements du prestataire
                   LQ_insuffisante <-
                     comparatif %>% subset(`LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE` <
                                             LqAna) %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       `LIMITE DE QUANTIFICATION GARANTIE PAR PRESTATAIRE`,
                       LqAna
                     )

                   fichier_xml$tbl_LQ_ko <- LQ_insuffisante %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif Accreditation #####
                   # on extrait du tableau les analyses dont les accréditations sont non conformes aux engagements du prestataire

                   Accreditation_manquante <- comparatif %>%
                     subset(`ACCREDITATION (oui / non)` == "oui" &
                              CdAccreAna != 1) %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       CommentairesAna
                     )

                   fichier_xml$tbl_accred_ko <- Accreditation_manquante %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif incertitude #####
                   # on extrait du tableau les analyses dont les incertitudes sont non conformes aux engagements du prestataire

                   Incertitude_non_conforme <- comparatif %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       RsAna,
                       INCERTITUDE,
                       IncertAna,
                       CommentairesAna
                     ) %>% subset(IncertAna > INCERTITUDE)


                   fichier_xml$tbl_incert_ko <- Incertitude_non_conforme %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite()


                   ##### Vérif méthodes #####
                   # on extrait du tableau les analyses dont les méthodes sont non conformes aux engagements du prestataire

                   Methode_non_conforme <- comparatif %>%
                     select(
                       UG,
                       perimetre_facturation,
                       `CODE SANDRE STATION`,
                       `CODE INTERNE STATION`,
                       `NOM STATION`,
                       programme,
                       `RUN ANALYTIQUE`,
                       CdSupport,
                       CdFractionAnalysee,
                       CdPrelevement,
                       DatePrel,
                       HeurePrel,
                       DateAna,
                       HeureAna,
                       CdParametre,
                       CdUniteMesure,
                       `Code méthode SANDRE`,
                       Méthode,
                       CdMethode,
                       CommentairesAna
                     ) %>% subset(`Code méthode SANDRE` != CdMethode)

                   fichier_xml$tbl_methode_ko <- Methode_non_conforme %>%
                     ajoute_nom_param() %>%
                     ajoute_nom_support() %>%
                     ajoute_nom_fraction() %>%
                     ajoute_nom_unite() %>%
                     ajoute_nom_methode()


                   ##### Qualification analyses #####
                   analyses_xml$cle_frac_unit <- paste0(
                     analyses_xml$CdParametre,
                     "_",
                     analyses_xml$CdFractionAnalysee,
                     "_",
                     analyses_xml$CdUniteMesure
                   )

                   analyses_xml$cle_staq_frac_unit <-
                     paste0(
                       analyses_xml$CdStationMesureEauxSurface,
                       "_",
                       analyses_xml$CdParametre,
                       "_",
                       analyses_xml$CdFractionAnalysee,
                       "_",
                       analyses_xml$CdUniteMesure
                     )
                   verif <- analyses_xml %>%
                     left_join(table_stat_analyses, by = "cle_staq_frac_unit") %>%
                     left_join(table_stat_analyses_toutes_staq, by = "cle_frac_unit") %>%
                     subset(CdRqAna == "1")

                   ##### qualif par station
                   verif$classement_par_station <- 99

                   # classe 1
                   verif$classement_par_station <-
                     ifelse(
                       verif$RsAna >= verif$Q10ST &
                         verif$RsAna <= verif$Q90ST,
                       1,
                       verif$classement_par_station
                     )

                   # classe 2
                   verif$classement_par_station <- ifelse(
                     (verif$RsAna >= verif$Q5ST & verif$RsAna < verif$Q10ST) |
                       (verif$RsAna <= verif$Q95ST &
                          verif$RsAna > verif$Q90ST),
                     2,
                     verif$classement_par_station
                   )

                   # classe 3
                   verif$classement_par_station <- ifelse(
                     (verif$RsAna >= verif$minST & verif$RsAna < verif$Q5ST) |
                       (verif$RsAna <= verif$maxST &
                          verif$RsAna > verif$Q95ST),
                     3,
                     verif$classement_par_station
                   )

                   # classe 7
                   verif$classement_par_station <- ifelse(
                     (
                       verif$RsAna >= (verif$minST - verif$sdST) &
                         verif$RsAna < verif$minST
                     ) |
                       (
                         verif$RsAna <= (verif$maxST + verif$sdST) &
                           verif$RsAna > verif$maxST
                       ),
                     7,
                     verif$classement_par_station
                   )

                   # classe 9
                   verif$classement_par_station <- ifelse(
                     (
                       verif$RsAna >= (verif$minST - 2 * verif$sdST) &
                         verif$RsAna < (verif$minST - verif$sdST)
                     ) |
                       (
                         verif$RsAna <= (verif$maxST + 2 * verif$sdST) &
                           verif$RsAna > (verif$maxST + verif$sdST)
                       ),
                     9,
                     verif$classement_par_station
                   )

                   # classe 10
                   verif$classement_par_station <-
                     ifelse((verif$RsAna <= (verif$minST - 2 *
                                               verif$sdST)) |
                              (verif$RsAna >= (verif$maxST + 2 * verif$sdST)),
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
                       verif$RsAna >= verif$Q10 &
                         verif$RsAna <= verif$Q90,
                       2,
                       verif$classement_toutes_station
                     )

                   # classe 3
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$Q5 & verif$RsAna < verif$Q10) |
                       (verif$RsAna <= verif$Q95 &
                          verif$RsAna > verif$Q90),
                     3,
                     verif$classement_toutes_station
                   )

                   # classe 4
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$Q1 & verif$RsAna < verif$Q5) |
                       (verif$RsAna <= verif$Q99 &
                          verif$RsAna > verif$Q95),
                     4,
                     verif$classement_toutes_station
                   )

                   # classe 6
                   verif$classement_toutes_station <- ifelse(
                     (verif$RsAna >= verif$min & verif$RsAna < verif$Q1) |
                       (verif$RsAna <= verif$max &
                          verif$RsAna > verif$Q99),
                     6,
                     verif$classement_toutes_station
                   )

                   # classe 8
                   verif$classement_toutes_station <- ifelse(
                     (
                       verif$RsAna >= (verif$min - verif$sd) &
                         verif$RsAna < verif$min
                     ) |
                       (
                         verif$RsAna <= (verif$max + verif$sd) &
                           verif$RsAna > verif$max
                       ),
                     8,
                     verif$classement_toutes_station
                   )


                   # classe 10
                   verif$classement_toutes_station <-
                     ifelse((verif$RsAna <= (verif$min -
                                               verif$sd)) |
                              (verif$RsAna >= (verif$max + verif$sd)),
                            10,
                            verif$classement_toutes_station)

                   # classe 0
                   verif$classement_toutes_station <-
                     ifelse(is.na(verif$classement_toutes_station),
                            0,
                            verif$classement_toutes_station)

                   ##### agrégation des vérifications
                   verif$classement <-
                     ifelse(verif$classement_toutes_station == 0 &
                              verif$classement_par_station == 0,
                            3,
                            4)

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2, 3, 4) &
                         verif$classement_par_station %in% c(0, 1, 2),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2, 3) &
                         verif$classement_par_station %in% c(3),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(6) &
                         verif$classement_par_station %in% c(3),
                       1,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(6) &
                         verif$classement_par_station %in% c(0, 1, 2, 3, 7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(4) &
                         verif$classement_par_station %in% c(3, 7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(3) &
                         verif$classement_par_station %in% c(7),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2) &
                         verif$classement_par_station %in% c(7, 9),
                       3,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(verif$classement_toutes_station %in% c(8, 10),
                            2,
                            verif$classement)

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(3, 4, 6) &
                         verif$classement_par_station %in% c(9, 10),
                       2,
                       verif$classement
                     )

                   verif$classement <-
                     ifelse(
                       verif$classement_toutes_station %in% c(2) &
                         verif$classement_par_station %in% c(10),
                       2,
                       verif$classement
                     )


                   verif$classement <- as.character(verif$classement)

                   fichier_xml$tbl_rs_anal <-
                     verif %>% select(
                       CdStationMesureEauxSurface:CdParametre,
                       RsAna,
                       classement_par_station:last_col()
                     ) %>% ajoute_nom_param() %>% ajoute_nom_station() %>% ajoute_nom_cdqualana(col_qualif = "classement")

                   remove_modal_spinner()
                 }
               })



  # avancement marché
  graph_avancement_financier <-
    renderPlot(random_ggplot(type = "bar"))

  graph_avancement_prelevement <-
    renderPlot(random_ggplot(type = "bar"))


  # stations
  output$carto_station <- renderPlot(random_ggplot(type = "raster"))

  output$date_last_maj_SANDRE <-
    renderText("date dernière maj SANDRE : xxxxxx")
  output$date_last_maj_EV <-
    renderText("date dernière maj Eaux & Vilaine : xxxxxx")

  output$text_code_interne_station <-
    renderText("Code interne station = Vilaine 17")
  output$text_distance_station_station <-
    renderText(
      "Texte qui s'affiche seulement si distance pt SANDRE / pt E&V>20m : affiche la distance entre les 2 points.
      Dans le rapport pdf seul le positionnement E&V apparait et ce commentaire n'est pas affiché."
    )
  output$text_acces_station <-
    renderText(
      "Commentaires sur l'accès à la station (point de stationnement, points de vigilance, ..."
    )
  output$text_prelevement <-
    renderText(
      "Commentaires sur le mode de prélèvement préférentiel (seau, .. et éventuel point alternatif si crue, étiage, ...)"
    )
  output$text_commentaire_station <-
    renderText(
      "Commentaires généraux sur la station (finalité de son positionnement, station qui remplace station...)"
    )
  output$date_last_maj_SANDRE <-
    renderText("date dernière maj SANDRE")
  output$date_last_maj_SANDRE <- renderText("date dernière maj E&V")

  # Périmètres de gestion
  output$table_perimetres_de_gestion <- renderDT(tableau_per_gest)


  # référentiels SANDRE
  output$table_derniere_maj_SANDRE <-
    renderDT(tableau_maj_ref %>% select(ts_nom_referentiel, ts_date))


  # connexion
  output$text_connecte_comme <-
    renderText("Vous etes connecte comme xxx")

}

# Run the application
shinyApp(ui = ui, server = server)
