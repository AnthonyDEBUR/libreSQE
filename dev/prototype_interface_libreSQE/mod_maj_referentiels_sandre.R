
# --- Module : Mise à jour des référentiels SANDRE ----------------------------

majReferentielsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Référentiels SANDRE"),
    shiny::p("Dernières mises à jour et actions par référentiel."),
    shiny::actionButton(ns("maj_all"), "Mettre à jour l'ensemble des référentiels", icon = shiny::icon("sync")),
    shiny::tags$hr(),
    shiny::uiOutput(ns("rows"))
  )
}

majReferentielsServer <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Lecture du tableau SQE + rafraîchissement --------------------------
    reload_data <- function() {
      DBI::dbReadTable(con, DBI::Id(schema = "sqe", table = "ts_suivi_maj_refer"))
    }
    ref_rv <- shiny::reactiveVal(reload_data())
    
    # --- Affichage des lignes (2 colonnes visibles + bouton d'action) --------
    output$rows <- shiny::renderUI({
      df <- ref_rv()
      shiny::req(nrow(df) > 0)
      # Nous n'affichons que ts_nom_referentiel et ts_date, avec un bouton par ligne
      shiny::tagList(
        lapply(seq_len(nrow(df)), function(i) {
          shiny::fluidRow(
            class = "mb-2",
            shiny::column(5, shiny::strong(df$ts_nom_referentiel[i])),
            shiny::column(3, as.character(df$ts_date[i])),
            shiny::column(4, shiny::actionButton(
              ns(paste0("maj_", df$ts_table[i])),
              "Mettre à jour", icon = shiny::icon("rotate-right")
            ))
          )
        })
      )
    })
    
    # --- Mapping ts_table -> fonctions LibreSQE ------------------------------
    ref_funcs <- list(
      "tr_stationmesure_stm"   = function(date_maj) LibreSQE::func_charge_ref_sandre_staq(date_maj = date_maj),
      "tr_fraction_fra"        = function(date_maj) LibreSQE::func_charge_ref_sandre_fractions(date_maj = date_maj),
      "tr_uniteparametre_uni"  = function(date_maj) LibreSQE::func_charge_ref_sandre_unites(date_maj = date_maj),
      "tr_methode_met"         = function(date_maj) LibreSQE::func_charge_ref_sandre_methodes(date_maj = date_maj),
      "tr_intervenantsandre_isa"= function(date_maj) LibreSQE::func_charge_ref_sandre_intervenants(date_maj = date_maj),
      "tr_parametre_par"       = function(date_maj) LibreSQE::func_charge_referentiel_SANDRE_parametres(date_maj = date_maj),
      "tr_rdd_rdd"             = function(date_maj) LibreSQE::func_charge_ref_sandre_dispocollecte(date_maj = date_maj)
    )
    
    # Eviter de multiplier les observeEvent lors des rafraîchissements
    created_ids <- shiny::reactiveVal(character())
    
    shiny::observe({
      df <- ref_rv()
      shiny::req(nrow(df) > 0)
      
      to_create <- setdiff(paste0("maj_", df$ts_table), created_ids())
      if (length(to_create) == 0) return(NULL)
      
      # Création des observateurs pour les nouveaux boutons seulement
      for (i in seq_len(nrow(df))) {
        tbl <- df$ts_table[i]
        btn_id <- paste0("maj_", tbl)
        if (!(btn_id %in% to_create)) next
        
        nom <- df$ts_nom_referentiel[i]
        d   <- as.Date(df$ts_date[i]) - 1
        
        shiny::observeEvent(input[[btn_id]], {
          if (!tbl %in% names(ref_funcs)) {
            shiny::showNotification(paste0("Aucune fonction définie pour '", tbl, "'."), type = "error", duration = 10)
            return(NULL)
          }
          
          shiny::withProgress(message = paste0("Mise à jour : ", nom), value = 0, {
            tryCatch({
              # Appel avec date_maj = ts_date - 1 jour
              ref_funcs[[tbl]](date_maj = d)
              shiny::showNotification(
                paste0("Référentiel '", nom, "' mis à jour (date_maj = ", d, ")."),
                type = "message"
              )
            }, error = function(e) {
              shiny::showNotification(
                paste0("Échec de la mise à jour '", nom, "' : ", e$message),
                type = "error", duration = 10
              )
            })
          })
          
          # Rafraîchir le tableau après mise à jour
          ref_rv(reload_data())
        }, ignoreInit = TRUE)
      }
      
      # Enregistrer les IDs déjà créés
      created_ids(unique(c(created_ids(), to_create)))
    })
    
    # --- Bouton global : mise à jour de toutes les tables --------------------
    shiny::observeEvent(input$maj_all, {
      shiny::withProgress(message = "Mise à jour de l'ensemble des référentiels SANDRE...", value = 0, {
        tryCatch({
          LibreSQE::func_maj_referentiels_sandre()
          shiny::showNotification("Tous les référentiels ont été mis à jour.", type = "message")
        }, error = function(e) {
          shiny::showNotification(paste0("Échec mise à jour globale : ", e$message), type = "error", duration = 10)
        })
      })
      ref_rv(reload_data())
    })
  })
}
