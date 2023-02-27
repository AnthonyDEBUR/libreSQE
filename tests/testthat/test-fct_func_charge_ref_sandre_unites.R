test_that("func charge unites sandre nb de lignes OK", {
  Sys.sleep(3)

  date_maj_test<-"2020-06-01"
  tmp<-func_charge_ref_sandre_unites(date_maj=date_maj_test)
  expect_type(tmp, "list")
  expect_true(nrow(tmp)>0)
})
