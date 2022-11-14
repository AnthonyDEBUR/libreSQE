# script de récupération des géométries postgis
# Author: cedricbriandgithub
###############################################################################

# setwd("c:/workspace/libreSQE")
library(sf)
library(RPostgres)
library()
con=dbConnect(Postgres(),
    dbname="geogwilen",
    host="mamut.lavilaine.local",
    port=5432,
    user= 'xxxxxxx',
    password= 'xxxxxxxxxxxxx')

read_sf
perimetre <- sf::st_read(con,query=paste("select *  FROM r300_territoire_sage.operateurs_bv"))
save(perimetre, file = str_c(getwd(),"/data/perimetre.Rdata"))

# TODO write in temporary table
con=dbConnect(Postgres(),
    dbname="geogwilen",
    host="mamut.lavilaine.local",
    port=5432,
    user= 'xxxxxxx',
    password= 'xxxxxxxxxxxxx')