##### Compare prog et realise pour compléter fichiers MOD AELB #####

library(LibreSQE)
library(tidyverse)

id_marche<- 17 # 18 #20
annee<-2022 #2023

connexion <- pool::dbPool(
  RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# reference du marché
mar_reference <- "2022-3"
  # "2022-65"

# périmètre de bons de commandes concernés par le bilan
bco_per_nom <-c("UGVE")


# on récupère le contenu des périmètres de bons de commandes concernés
conn<-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

bdc<-pool::dbGetQuery(conn,
                      paste0("SELECT * FROM sqe.view_bdc_quantif_par_staq WHERE mar_reference = '",
                             mar_reference,"' AND bco_per_nom IN ('",paste(bco_per_nom, collapse="','"),"');")
)

pool::dbRollback(conn)

# on récupère l'id du marche concerne
conn<-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

marche<-pool::dbGetQuery(conn,
                         paste0("SELECT mar_id, mar_reference
                                FROM sqe.t_marche_mar WHERE mar_reference = '",mar_reference,"';")
)

pool::dbRollback(conn)

# on récupère l'id des prestations concernees
conn<-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

presta_id<-pool::dbGetQuery(conn,
                         paste0("SELECT prs_id, prs_idprestationdansbpu
                                FROM sqe.t_prestation_prs WHERE prs_mar_id IN ('",
                                paste(marche$mar_id, collapse="','"),
                                "');")
)

pool::dbRollback(conn)

# on ajoute les informations de marche et prestation id à la table bdc
bdc<-dplyr::left_join(bdc, marche, by="mar_reference")
bdc<-dplyr::left_join(bdc, presta_id, by="prs_idprestationdansbpu")


# on récupère le contenu des programmes analytiques concernés

prog_prs_id<-unique(bdc$prs_id)

conn<-  pool::poolCheckout(connexion)
on.exit(pool::poolReturn(conn))
pool::dbBegin(conn) # commence transaction SQL

presta<-pool::dbGetQuery(conn,
                      paste0("SELECT * FROM sqe.t_parametreprogrammetype_ppt WHERE ppt_mar_id IN ('",
                      paste(marche$mar_id, collapse="','"),
                      "') AND ppt_prs_id IN ('",
                      paste(prog_prs_id, collapse="','"),"');")
)

pool::dbRollback(conn)

# volumetrie de chaque parametre attendu par station et prog
attendu<-inner_join(bdc,
                    presta,
                    by=c("prs_id"="ppt_prs_id"),
                    relationship = "many-to-many")%>%
  dplyr::group_by(bcp_stm_cdstationmesureinterne,
                                   stm_lbstationmesureeauxsurface,
                                   ppt_par_cdparametre,
                                   ppt_fra_codefraction,
                                   ppt_analyseinsitu,
                  prs_label_prestation)%>%
  dplyr::summarise(count = n())


# realise
analyses_Eaux_Vilaine <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")
analyses_Eaux_Vilaine<-analyses_Eaux_Vilaine%>%
  subset(DatePrel>=as.Date(paste0(annee,"-01-01")) &
         DatePrel<=as.Date(paste0(annee,"-12-31"))
         )

realise<-analyses_Eaux_Vilaine%>%
  dplyr::group_by(CdStationMesureEauxSurface,
                  CdParametre,
                  CdFractionAnalysee,
                  CdInsituAna)%>%
  dplyr::summarise(count = n())

realise$CdInsituAna<-ifelse(realise$CdInsituAna=="1", TRUE, FALSE)

# comparaison realise attendu
comparaison<-dplyr::left_join(attendu,
                              realise,
                              by=c("bcp_stm_cdstationmesureinterne"="CdStationMesureEauxSurface",
                                   "ppt_par_cdparametre"="CdParametre",
                                   "ppt_fra_codefraction"="CdFractionAnalysee",
                                   "ppt_analyseinsitu"="CdInsituAna"),
                              suffix=c(".attendu",".realise"))%>%
  dplyr::mutate(pc_realise=count.realise/count.attendu)
