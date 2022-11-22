test_that("charge parametres sandre format OK", {
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-06-01")
  expect_type(charge_referentiel_SANDRE_parametres(date_maj=date_maj_test), "list")
})

test_that("charge parametres sandre nb de lignes OK", {
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-06-01")
  tmp<-charge_referentiel_SANDRE_parametres(date_maj=date_maj_test)
  expect_true(nrow(tmp)>0)
})



