#' fontion transfert d'un circuit RLC
#'
#' @param R Résistance(en ohms)
#' @param L Inductance(en henrys)
#' @param C Capacité(en farads)
#'
#' @return fonction
#' @export
#'
#' @examples
fonction_de_transfert_RLC <- function(R, L, C) {
  # Définir la variable complexe s
  s <- complex(real = 0, imaginary = 1)  # s = j*omega, ici on peut essayer avec j*1 pour un exemple

  # Calculer la fonction de transfert H(s)
  H <- 1 / (L * C * s^2 + R * C * s + 1)

  return(H)
}
