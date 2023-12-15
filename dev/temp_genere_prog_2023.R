# temporaire - génère BDC 2023


# parametres temporaires
bdc_type<-"C:\\workspace\\LibreSQE\\dev\\modèle bon de commande vierge\\modele_bdc_vierge_2023.xlsx"

pre_id<-c(3)
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

tmp<-pool::dbGetQuery(conn,
                                  paste0("SELECT * FROM sqe.view_bdc_quantif
                                      WHERE pre_id IN (",
                                         paste(pre_id, collapse=","),");")
)

pool::dbRollback(conn)

# sélection du marché 2023
tmp<-tmp[tmp$mar_reference=="2022-65",]

liste_bdc<-data.frame(id=tmp$bco_id, nom=tmp$bco_refcommande)%>%unique

# genere les bdc
for(i in 1:nrow(liste_bdc))
{
  print(i)
  out_file_name<-paste0("c:\\workspace\\bdc2023\\",liste_bdc$nom[i],".xlsx")
  func_genere_bdc_xlsx(bdc_id=liste_bdc$id[i], pre_id, connexion, bdc_type, out_file_name)

}
