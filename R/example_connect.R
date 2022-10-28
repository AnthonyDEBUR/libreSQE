
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

# par defaut dbWriteTable écrit dans public ici une astuce pour écrire dans le schema temp il faut créer un id
table_isa_id<-DBI::Id(
    schema=temp,
    table="temp_isa"
)
# écriture de la table
DBI::dbWriteTable(con, table_isa_id, temp_isa)

# il vaut mieux faire la suite dans postgres

DBI::dbGetQuery(con, statement= "SELECT * FROM ....")





#Pour avoir le schema
