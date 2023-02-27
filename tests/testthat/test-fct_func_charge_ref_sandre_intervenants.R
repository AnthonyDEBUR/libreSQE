test_that("func charge intervenants sandre format OK", {
  Sys.sleep(3)
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-12-01")
  expect_type(func_charge_ref_sandre_intervenants(date_maj=date_maj_test), "list")
})

Sys.sleep(3)
test_that("func charge intervenants sandre nb de lignes OK", {
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-12-01")
  tmp<-func_charge_ref_sandre_intervenants(date_maj=date_maj_test)
  expect_true(nrow(tmp)>0)
})
