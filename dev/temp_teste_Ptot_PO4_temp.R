library(tools4DCE)
library(LibreSQE)
analyses_Eaux_Vilaine <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")

# test des cohérences Ptot, PO4
# chargement des paramètres concernés

# Cd Sandre PO4 = 1433
# Cd Sandre Ptot = 1350

analyses_a_tester<-analyses_Eaux_Vilaine%>%subset((CdParametre=="1350" &CdFractionAnalysee=="23") |
                                                    ( CdParametre=="1433" &CdSupport=="3"))%>% unique()
analyses_a_tester<-analyses_a_tester%>%tidyr::pivot_wider(id_cols=c(CdStationMesureEauxSurface, DatePrel, HeurePrel, CdPreleveur, CdRdd),
                                                          names_from=CdParametre,
                                                          names_prefix="p",
                                                          values_from = RsAna,
                                                          values_fn=max)

analyses_a_tester<-analyses_a_tester%>%subset(!is.na(p1350) & !is.na(p1433))


  fct_test_P<-function(x){func_test_metier_coherenceP(Ptot =analyses_a_tester[["p1350"]][x],
                                                      PO4 =analyses_a_tester[["p1433"]][x],
                                                      incertPtot = 30,
                                                      incertPO4 = 36)}

  fct_test_P_val<-function(x){func_test_metier_coherenceP(Ptot =analyses_a_tester[["p1350"]][x],
                                                          PO4 =analyses_a_tester[["p1433"]][x],
                                                          incertPtot = 30,
                                                          incertPO4 = 36,
                                                          export="value")}

  analyses_a_tester$testPtot_PO4<-sapply(seq_along(analyses_a_tester$CdStationMesureEauxSurface),
            FUN=fct_test_P)
  analyses_a_tester$testPPO4<-sapply(seq_along(analyses_a_tester$CdStationMesureEauxSurface),
                                   FUN=fct_test_P_val)

  analyses_a_tester$fractPPO4<-round(analyses_a_tester$testPPO4/analyses_a_tester$p1350*100,1)

