library(tools4DCE)
library(LibreSQE)
analyses_Eaux_Vilaine <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")

# test des cohérences température, O2 dissous, sat O2
# chargement des paramètres concernés

#n°1312 Taux de saturation en oxygène à partir des paramètres :
  #- n°1301 Température de l'Eau
  #- n°1311 Oxygène dissous

analyses_a_tester<-analyses_Eaux_Vilaine%>%subset(CdParametre%in%c("1301", "1311", "1312") &
                                                    CdSupport=="3" &
                                                    CdInsituAna=="1")%>% unique()
analyses_a_tester<-analyses_a_tester%>%tidyr::pivot_wider(id_cols=c(CdStationMesureEauxSurface, DatePrel, HeurePrel, CdPreleveur, CdRdd),
                                                          names_from=CdParametre,
                                                          names_prefix="p",
                                                          values_from = RsAna,
                                                          values_fn=max)

analyses_a_tester<-analyses_a_tester%>%subset(!is.na(p1301) & !is.na(p1311) & !is.na(p1312))


  fct_test_O2<-function(x){func_test_metier_coherenceO2(O2=analyses_a_tester[["p1311"]][x],
                                                        satO2=analyses_a_tester[["p1312"]][x],
                                                        temp=analyses_a_tester[["p1301"]][x])}

  fct_test_O2_val<-function(x){func_test_metier_coherenceO2(O2=analyses_a_tester[["p1311"]][x],
                                                        satO2=analyses_a_tester[["p1312"]][x],
                                                        temp=analyses_a_tester[["p1301"]][x],
                                                        export="value")}

  analyses_a_tester$testO2<-sapply(seq_along(analyses_a_tester$CdStationMesureEauxSurface),
            FUN=fct_test_O2)
  analyses_a_tester$testO2_attendu<-sapply(seq_along(analyses_a_tester$CdStationMesureEauxSurface),
                                   FUN=fct_test_O2_val)



