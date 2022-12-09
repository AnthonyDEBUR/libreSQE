#
# Prototype de l'interface de libreSQE

library(shiny)
library(shinipsum)
library(DT)
library(tidyverse)

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

library(tools4DCE)
# pluie UGVE aout
fichier <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2022\\marché et commande\\05_livrables\\QUESU3_EPTB_VILAINE-CAB_2022-08-02_0400003224\\QUESU3_EPTB_VILAINE-CAB_2022-08-17_0400003223.xml"
if (!exists("fichier_xml")) {
  fichier_xml <- import_QESU_PHY_v3(fichier)
}
analyses_xml <- fichier_xml$analyses
cond_environ_xml <- fichier_xml$cond_env
operations_xml <- fichier_xml$operations

library(readxl)

fichier_prog <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\R_Anthony\\libreSQE\\dev\\v2 prog EPTB Est_Ouest 2022 - commande_3 derniers trimestres_ajout suivis Captages_version dev libreSQE.xlsx"

param_perimetre_facturation <- "UGVE"
param_rattachement_bdc <- "pluie"
param_mois <- "aout"

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


bdc <-
  prog_previsionnelle %>% subset(
    perimetre_facturation == param_perimetre_facturation &
      rattachement_devis == param_rattachement_bdc &
      mois == param_mois
  )

analyses_attendues_bdc <-
  left_join(bdc, programmes_types, by = c("programme" = "PROGRAMME"))

analyses_attendues_bdc <-
  left_join(analyses_attendues_bdc,
            cout_run_analytiques,
            by = c("RUN ANALYTIQUE"))

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Prototype de l'interface de libreSQE",
  tabPanel("Synthèse"),
  navbarMenu(
    "Analyses",
    tabPanel(
      "Mes analyses à qualifier",
      fluidRow(
        column(2,
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
        column(8,
        DTOutput("dt_analyses_a_qualifier"),
        actionButton(
          "btn_valide_selection",
          "Valider l'ensemble des données affichées"
        ),
        actionButton(
          "btn_invalide_selection",
          "Invalider l'ensemble des données affichées"
        ),
        p("On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "),
        p("S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"),
        p("bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"),
        p("boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé")

        )
      )

    ),
    tabPanel("Rechercher / éditer des analyses",
             fluidRow(
               column(2,
                      "Sélection du jeu de données",
                      p("Constructeur de requête SQL avec choix (ou pas si non renseigné)
                        sur code station, code paramètre, code fraction, code unité,
                        date début, date fin prélèvement, dispositif de collecte, laboratoire,
                        préleveur, qualification, ...")
               ),
               column(8,
                      DTOutput("dt_analyses_a_qualifier2"),
                      p("possibilité d'éditer certaines valeurs (résultat d'analyses, LQ, LD, code unité, ... dans le DT)"),
                      actionButton(
                        "btn_valide_selection",
                        "Valider l'ensemble des données affichées"
                      ),
                      actionButton(
                        "btn_invalide_selection",
                        "Invalider l'ensemble des données affichées"
                      ),
                      p("On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "),
                      p("S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"),
                      p("bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"),
                      p("boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé")

               )
             ))
  ),
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
      fileInput("import_xml",
                label = "Sélectionner fichier xml de résultats"),
      actionButton("btn_import_xml", "Déposer le fichier sélectionné sur le serveur"),
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
      )

    )
  ),
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
        "select_perimetre_facturation",
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
        "Affichage les 3 tableaux BPU, BPU des run analytiques et programmes types"
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
  # qualif analyses

  output$dt_analyses_a_qualifier <- renderDT(analyses_xml)



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
    renderDT(programmes_types %>% select(-`LIMITE DE QUANTIFICATION PRECONISEE`,-STATUT))


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
