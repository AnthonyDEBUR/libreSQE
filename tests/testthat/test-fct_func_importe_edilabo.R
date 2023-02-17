test_that("Import EDILABO fonctionne", {
  fichier <- system.file("exemple_EDILABO.xml", package = "LibreSQE")
  test<-func_importe_edilabo(fichier)
  expect_equal(nrow(test$Analyses), 108)
  expect_equal(nrow(test$Echantillon), 6)
  expect_equal(nrow(test$Res_env), 84)
  expect_equal(nrow(test$Operation), 6)
})
