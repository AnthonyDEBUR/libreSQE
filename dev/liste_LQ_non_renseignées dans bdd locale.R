# liste les données sans LQ renseignée dans la bdd locale

library(tidyverse)
library(tools4DCE)
library(openxlsx)


analyses_Eaux_Vilaine <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")

anal_pb_LQ<-analyses_Eaux_Vilaine%>%
  subset(DatePrel>=as.Date("2023-01-01") &
           is.na(LqAna) &
           !CdInsituAna=="1")



anal_pb_LQ<-ajoute_nom_param(anal_pb_LQ)
anal_pb_LQ<-ajoute_nom_station(anal_pb_LQ)
anal_pb_LQ<-ajoute_nom_unite(anal_pb_LQ)

write.xlsx(anal_pb_LQ, "C://workspace//LibreSQE//analyses_avec_absence_LQ.xlsx")
