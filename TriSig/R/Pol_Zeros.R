#' calcul des poles et des zeros de la fonction de transfert
#'
#' @param R Résistance(en ohms)
#' @param L Inductance(en henrys)
#' @param C Capacité(en farads)
#'
#' @return number
#' @export
#'
#' @examples
poles_zeros_RLC <- function(R, L, C) {
  # Coefficients du numérateur et du dénominateur
  num <- c(1)  # Numérateur : 1

  # Calcul des zéros
  zeros <- polyroot(num)

  # Calcul des pôles
  a <- L*C
  b <- R*C
  c <- 1
  # Calculer le discriminant
  discriminant <- b^2 - 4 * a * c

  # Vérifier la nature des racines en fonction du discriminant
  if (discriminant > 0) {
    # Deux racines réelles distinctes
    poles <- (-b + sqrt(discriminant)) / (2 * a)
    poles <- (-b - sqrt(discriminant)) / (2 * a)
  } else if (discriminant == 0) {
    # Une racine réelle double
    poles <- -b / (2 * a)
  } else {
    # Racines complexes
    poles <- (-b + sqrt(as.complex(discriminant))) / (2 * a)
    poles <- (-b - sqrt(as.complex(discriminant))) / (2 * a)
    }
  return(list(poles = poles, zeros = zeros))
}
