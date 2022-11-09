
library("RPostgres")
library("DBI")
library("getPass")
con <- dbConnect(Postgres(),     
    dbname="libresqe",    
    host="localhost",
    port=5432,    
    user= getPass(msg="user"),    
    password= getPass(msg="password"))

# script de creation temp_isa voir creer_temp_isa.R
source("creer_temp_isa.R")

# ecriture de la table dans le schema temp
DBI::dbExecute(con, "CREATE SCHEMA temp AUTHORIZATION grp_eptbv_planif_dba")

# par defaut dbWriteTable �crit dans public ici une astuce pour �crire dans le schema temp il faut cr�er un id
table_isa_id<-DBI::Id(
    schema= "temp",
    table="temp_isa"
)
# �criture de la table
DBI::dbWriteTable(con, table_isa_id, temp_isa)

# il vaut mieux faire la suite dans postgres

DBI::dbGetQuery(con, statement= "SELECT * FROM temp.temp_isa")
DBI::dbExecute(con, 
"ALTER TABLE refer.tr_prestataire_pre DROP CONSTRAINT IF EXISTS c_fk_pre_codesandre;
DELETE FROM refer.tr_intervenantsandre_isa;
INSERT INTO refer.tr_intervenantsandre_isa
(isa_codesandre, isa_siret, isa_nom, isa_statut, isa_datecreation, isa_datemaj, isa_mnemo, isa_ensembleimmobilier, isa_bp, isa_rue, isa_lieudit, isa_ville, isa_departementpays, isa_codepostal)
SELECT isa_codesandre, isa_siret, isa_nom, isa_statut, isa_datecreation::date, isa_datemaj::date, isa_mnemo, isa_ensembleimmobilier, isa_bp, isa_rue, isa_lieudit, isa_ville, isa_departementpays, isa_codepostal::INTEGER
FROM
temp.temp_isa; -- 158842
ALTER TABLE refer.tr_prestataire_pre ADD CONSTRAINT c_fk_pre_codesandre FOREIGN KEY (pre_isa_codesandre) 
REFERENCES refer.tr_intervenantsandre_isa(isa_codesandre) 
ON DELETE RESTRICT ON UPDATE CASCADE;")




#Pour avoir le schema
