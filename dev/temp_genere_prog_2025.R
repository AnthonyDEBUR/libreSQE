# temporaire - génère BDC 2025

# prévoir dans le tableau de synthèse de pouvoir avoir l'info sur les coûts
# de prélèvement d'une part et les coûts d'analyse d'autre aprt car demandé par AELB

library(LibreSQE)

id_marche<-23
# dossier où sont enregistrés les bdc créés
output<-"c:\\workspace\\bdc2025\\"


frequence_bdc="mensuelle"
prefixe<-"SQE2025"

fichier_prog <-
  "C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB2025_version dev libreSQE.xlsx"
mar_id = id_marche
connexion <- pool::dbPool(
  RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

annee <- 2025

# A exécuter si nouveau marché
# func_charge_marche(fichier_prog, connexion, mar_id)

# a exécuter si nlle prog annuelle
 # func_charge_prog_annuelle(fichier_prog, connexion, mar_id, annee, frequence_bdc="mensuelle", prefixe=prefixe)



# prestataire(s) pour lesquels émettre les bdc
pre_id<-c(1,6,7)


#table correspondance périmètre / bdc vierge
connexion <- pool::dbPool(RPostgres::Postgres(),
                          dbname="libresqe",
                          host="localhost",
                          port=5432,
                          user= "postgres",
                          password= "postgres")


# on récupère le contenu du BDC - page 1
conn<-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

bdc<-pool::dbGetQuery(conn,
                      paste0("SELECT * FROM sqe.t_boncommande_bco
                                      WHERE bco_mar_id IN (",
                             id_marche,");")
)

pool::dbRollback(conn)

# ajout d'une colonne avec le fichier du bon de commande concerné
bdc$fichier<-paste0("C:\\workspace\\LibreSQE\\dev\\modèle bon de commande vierge\\modele_bdc_vierge_",
                    bdc$bco_per_nom,".xlsx")

# on vérifie que tous les fichiers modèle existent
verifier_existence_fichiers <- function(chemins) {
  chemins_uniques <- unique(chemins)
  fichiers_manquants <- chemins_uniques[!sapply(chemins_uniques, file.exists)]

  if (length(fichiers_manquants) > 0) {
    stop(paste("Les fichiers suivants n'existent pas :", paste(fichiers_manquants, collapse = ", ")))
  } else {
    message("Tous les fichiers existent.")
  }
}

# Exemple d'utilisation avec un dataframe
verifier_existence_fichiers(bdc$fichier)


liste_bdc<-data.frame(id=bdc$bco_id, nom=bdc$bco_refcommande, bdc$fichier)%>%unique

tableau_synthese<-data.frame(rep(character(),8))

# genere les bdc
for(i in 1:nrow(liste_bdc))
{
  print(i)
  out_file_name<-paste0(output,liste_bdc$nom[i],".xlsx")

  tableau_synthese<-rbind(tableau_synthese,
        func_genere_bdc_xlsx(bdc_id=liste_bdc$id[i],
                       pre_id,
                       connexion,
                       bdc_type=liste_bdc$bdc.fichier[i],
                       out_file_name)
        )


}

names(tableau_synthese)<-c("marche", "bon de commande","perimetre","projet", "mois_debut", "mois_fin", "montant total HT", "montant total TTC")
tableau_synthese$`montant total HT`<-as.numeric(tableau_synthese$`montant total HT`)
tableau_synthese$`montant total TTC`<-as.numeric(tableau_synthese$`montant total TTC`)
openxlsx::write.xlsx(tableau_synthese, paste0(output, "synthese_bdc.xlsx"))
