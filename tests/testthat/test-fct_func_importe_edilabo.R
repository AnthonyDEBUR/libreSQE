test_that("Import EDILABO fonctionne", {
  fichier <- system.file("exemple_EDILABO.xml", package = "LibreSQE")
  test<-func_importe_edilabo(fichier,
                             stations_a_traiter = c("04304001", "04304000", "04304011"))

#   test<-func_importe_edilabo(fichier)
#
#   op<-test$Operation
#   ana<-test$Analyses
#   res_env<-test$Res_env
#   commem<-test$Commemoratifs
#   echant<-test$Echantillon
#   test$Stations
#   test$Intervenants
# test$Demande
# test$Commemoratifs

  expect_equal(nrow(test$Analyses), 48)
  expect_equal(nrow(test$Echantillon), 3)
  expect_equal(nrow(test$Res_env), 44)
  expect_equal(nrow(test$Operation), 3)
  expect_true(all(c(test$Analyses[20,]$DateAna=="2020-12-08",
                    test$Analyses[20,]$Parametre_CdParametre=="1319",
                    test$Analyses[20,]$CdStationPrelevement=="04304000",
                    test$Analyses[20,]$DatePrel=="2020-12-02",
                    test$Analyses[20,]$RsAna==1.9)))


})
