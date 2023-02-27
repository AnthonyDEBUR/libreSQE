test_that("func charge methodes sandre nb de lignes OK", {
  Sys.sleep(3)

  date_maj_test<-"2021-06-01"
  tmp<-func_charge_ref_sandre_methodes(date_maj=date_maj_test)
  expect_type(tmp, "list")
  expect_true(nrow(tmp)>0)
})
