test_that("func charge unite sandre format OK", {
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-06-01")
  expect_type(func_charge_ref_sandre_unites(date_maj=date_maj_test), "list")
})

test_that("func charge parametres sandre nb de lignes OK", {
  date_maj_test<-"2020-06-01"
  tmp<-func_charge_ref_sandre_unites(date_maj=date_maj_test)
  expect_true(nrow(tmp)>0)
})
