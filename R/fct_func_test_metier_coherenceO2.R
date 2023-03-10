#' func_test_metier_coherenceO2
#'
#' @description In pure water under 1atm of pressure, the parameters of temperature,
#' dissolved oxygen content, and oxygen saturation are inherently correlated by definition.
#' This function test if this is the case.
#'
#' @param O2 : dissolved oxygen content in mg O2/L
#' @param satO2 : oxygen saturation in % (value between 0 and 250, more than 100% = oversaturated)
#' @param temp : temperature in °C
#' @param export : "code" (default) : return code if correlation is OK or not,
#' "value" return oxygen saturation calculated for given O2 and temp values.
#'
#' @return The return value, is either a numeric code :
#'  0 = impossible to do the test
#'  1 = correlation OK (difference between provide satO2 value and calculated satO2value respect
#'  satO2 value in [satO2 calculated - 2, sat O2 calculated + 2])
#'  2 = correlation not exactly respected (satO2 value in [satO2 calculated - 5, sat O2 calculated - 1[ or
#'  satO2 value in ]satO2 calculated +1, sat O2 calculated + 5] )
#'  3 = error for sure
#'
#' or the calculated value of oxygen saturation for given temperature and dissolved O2.
#'
#' @export
func_test_metier_coherenceO2<-function(O2=NULL, satO2=NULL, temp=NULL, export="code")
{
  if(!all(is.numeric(O2), is.numeric(satO2), is.numeric(temp))){stop("func_test_metier_coherenceO2 : O2, satO2 and temp must be numeric")}
  if(length(O2)!=1){stop("func_test_metier_coherenceO2 : O2 must be of length 1")}
  if(length(satO2)!=1){stop("func_test_metier_coherenceO2 : satO2 must be of length 1")}
  if(length(temp)!=1){stop("func_test_metier_coherenceO2 : temp must be of length 1")}
  if(satO2<0 | satO2>250){stop("func_test_metier_coherenceO2 : satO2 should be a numeric between 0 and 250")}
  # la valeur d'O2 dissous doit être entre 0 et la concentration de saturation à 100°C (27 mg O2/L)
  if(O2<0 | O2>27){stop("func_test_metier_coherenceO2 : O2 should be between 0 and 27 mg O2/L")}
  #la valeur de température prise en compte doit être entre 0 et 100 °C
  if(temp<0 | temp>100){stop("func_test_metier_coherenceO2 : temp should be between 0 and 100°C")}
  if(!(export %in%c("code", "value"))){stop("func_test_metier_coherenceO2 : unknown export value")}

 # équation de calcul selon le SANDRE https://mdm.sandre.eaufrance.fr/node/414781
 # La formule de calcul du paramètre Sandre [1312] sat O2 à partir des 2 paramètres Sandre
#   [1301] température en °C et [1311] O2 dissous est la suivante :
#  [1312] = ([1311] / (14.64 - (0.4227 * [1301]) + (0.009937 * [1301]^2) - (0.0001575 * [1301]^3) + (0.000001125 * [1301]^4)) * 100
# Commentaires sur la méthode:Méthode pour calculer le paramètre n°1312 Taux de saturation en oxygène à partir des paramètres :
#- n°1301 Température de l'Eau
#- n°1311 Oxygène dissous
# Les valeurs Cs(t) de solubilité de l'oxygène dissous à 1013 mbars et salinité nulle peuvent être obtenues
# avec une erreur très inférieure à 1% au moyen de la formule suivante :
# Cs(t) = 14,64 - 0,4227*[1301] + 0,009937*[1301]^2 - 0,0001575*[1301]^3 + 0,000001125*[1301]^4
# Le taux de saturation proprement dit n°1312 est obtenu en fonction de cette solubilité Cs(t)
# et de la teneur en Oxygène dissous réellement mesurée n°1311 par la formule suivante : [1312] = ([1311] * 100) / Cs(t)

  # temp<-19.8
  # O2<-10.6
  # satO2<-116
  Csat<-14.64-0.4227*temp+0.009937*temp^2-0.0001575*temp^3+0.000001125*temp^4
  satO2theorique<-100*O2/Csat

  if(export=="code")
 { delta<-abs(satO2-satO2theorique)

  out <- ifelse(delta < 2, "1",
                ifelse(delta < 5, "2",
                       ifelse(delta >= 5, "3", "0")))}
  if(export=="value"){out<-satO2theorique}

  return(out)
}

