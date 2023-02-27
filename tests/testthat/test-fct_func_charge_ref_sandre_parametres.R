test_that("func charge parametres sandre nb de lignes OK", {
  Sys.sleep(3)

  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-06-01")
  tmp<-func_charge_referentiel_SANDRE_parametres(date_maj=date_maj_test)
  expect_type(tmp, "list")
  expect_true(nrow(tmp)>0)
})



