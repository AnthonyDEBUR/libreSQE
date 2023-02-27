test_that("func charge stations sandre nb de lignes OK", {
  Sys.sleep(3)
  date_maj_test<-paste0((as.numeric(format(Sys.Date(),"%Y"))-1),
                        "-12-01")
  tmp<-func_charge_ref_sandre_staq(date_maj=date_maj_test)
  expect_type(tmp, "list")
  expect_true(nrow(tmp)>0)
})
