#' func_test_metier_coherenceP
#'
#'
#' @description test that total phosphorus is greater or equal to orthophosphates
#'
#' @param Ptot : total phosphorus in mg P/L
#' @param PO4 : orthophosphates in mg PO4 / L
#' @param incertPtot : Measurement uncertainty on Ptot measurement (in % of value) - default = 20 (%)
#' @param incertPO4 : Measurement uncertainty on PO4 measurement (in % of value) - defaut = 20 (%)
#' @param forceincert : boolean. If TRUE, an incorrect value of incertPtot or of incertPO4 will be
#' automaticaly replaced by 20 (in %)
#' @param export : "code" (default) : return code if relation beween the two parameters is OK or not,
#' "value" return PO4 value in unit P-PO4.
#'
#' @return The return value, is either a numeric code :
#'  0 = impossible to do the test
#'  1 = OK PO4<=Ptot without considering uncertainty
#'  2 = OK only by considering uncertainty
#'  3 = PO4 measurement to high versus Ptot measurement
#'
#'or the transfromed value of PO4 as P-PO4
#'
#'
#' @export
func_test_metier_coherenceP <- function(Ptot = NULL,
                                        PO4 = NULL,
                                        incertPtot = NULL,
                                        incertPO4 = NULL,
                                        forceincert = FALSE,
                                        export = "code") {
  if (!all(is.numeric(Ptot), is.numeric(PO4))) {
    stop("func_test_metier_coherenceP : Ptot and PO4 should be numeric")
  }
  if (!all(length(Ptot) == 1, length(PO4) == 1)) {
    stop("func_test_metier_coherenceP : Ptot and PO4 should have a length of 1")
  }
  if (!is.logical(forceincert))  {
    stop("func_test_metier_coherenceP : forceincert should be a logical")
  }
  if (!forceincert) {
    if (!all(is.numeric(incertPtot), is.numeric(incertPO4))) {
      stop("func_test_metier_coherenceP : incertPtot and incertPO4 should be numeric")
    }
  }
  # on initialise les incertitudes si non fournies et si forceincert=TRUE
  if (forceincert) {
    if (!is.numeric(incertPtot) | is.na(incertPtot)) {
      incertPtot <- 20
    }
    if (!is.numeric(incertPO4)| is.na(incertPO4)) {
      incertPO4 <- 20
    }
  }
  if (incertPtot < 0) {
    if (forceincert) {
      incertPtot <- 20
      warning(
        "func_test_metier_coherenceP : incertPtot<0 ==> automatically converted to incertPtot=20"
      )

    } else {
      stop("func_test_metier_coherenceP : incertPtot must be >0")
    }
  }

  if (incertPO4 < 0) {
    if (forceincert) {
      incertPO4 <- 20
      warning(
        "func_test_metier_coherenceP : incertPO4<0 ==> automatically converted to incertPO4=20"
      )
    }
    else {
      stop("func_test_metier_coherenceP : incertPO4 must be >0")
    }
  }
  if (!(export %in% c("code", "value"))) {
    stop("func_test_metier_coherenceP : unknown export value")
  }


  # Cd Sandre PO4 = 1433
  # Cd Sandre Ptot = 1350

  # converting PO4 to P-PO4
  PPO4 <- PO4 * 31 / 95

  if (export == "code")
  {
    out <- ifelse(PPO4 <= Ptot,
                  "1",
                  ifelse((PPO4 - PPO4 * incertPO4 / 100) <= (Ptot + Ptot * incertPtot /
                                                               100),
                         "2",
                         ifelse((PPO4 - PPO4 * incertPO4 / 100) > (Ptot + Ptot * incertPtot /
                                                                     100),
                                "3",
                                "0"
                         )
                  ))
  } else{
    out <- PPO4
  }
  return(out)
}
