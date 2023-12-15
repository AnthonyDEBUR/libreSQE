# chargement d'un nouveau marché dans la bdd

library(LibreSQE)

# fichier prog
fichier_prog<-"C:\\workspace\\LibreSQE\\dev\\fichier_exemple_commande\\v2 prog EPTB2024_version dev libreSQE.xlsx"

# id marche selon table t_marche_mar du schéma sqe
mar_id=20
connexion <- pool::dbPool(RPostgres::Postgres(),
                          dbname="libresqe",
                          host="localhost",
                          port=5432,
                          user= "postgres",
                          password= "postgres")


func_charge_marche(fichier_prog, connexion, mar_id)


# chargement de la prog annuelle

# tables à mettre à jour si changement
# sqe.t_resultat_res (cascade) --> t_resultatanalyse_rea
# t_boncommande_pgm_bcp
# t_boncommande_quantitatif_bcq (cascade) -> view_bdc_quantitatif et view_bdc_quantif_par_staq
# t_boncommande_bco
# t_progannuelle_pga
# t_calendrierprog_cal
#

connexion <- pool::dbPool(RPostgres::Postgres(),
                          dbname="libresqe",
                          host="localhost",
                          port=5432,
                          user= "postgres",
                          password= "postgres")

annee <- 2024
func_charge_prog_annuelle(fichier_prog, connexion, mar_id, frequence_bdc="mensuelle", prefixe="SQE2024")


