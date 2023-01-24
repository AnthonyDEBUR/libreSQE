test_that("Import QUESU fonctionne", {
  datafile <- system.file("exemple_QUESU3.xml", package = "LibreSQE")
  test<-func_importe_quesu(datafile)
  expect_equal(nrow(test$Analyses), 108)
  expect_equal(nrow(test$Echantillon), 6)
  expect_equal(nrow(test$Res_env), 84)
  expect_equal(nrow(test$Operation), 6)
})
