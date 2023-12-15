library(LibreSQE)

fichier<-"C:\\workspace\\LibreSQE\\dev\\fichier exemple EDILABO\\RA_LABOCEAQ_EPTB_230210144022001.xml"
test<-func_importe_edilabo(fichier)

anal<-test$Analyses
dema<-test$Demande
prel<-test$Operation
commemor<-test$Commemoratifs
