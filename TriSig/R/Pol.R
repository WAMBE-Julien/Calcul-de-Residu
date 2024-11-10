#' Calcul des poles d'une fonction rationelle
#'
#' @param a numbers
#' @param b numbers
#' @param c numbers
#'
#' @return number
#' @export
#'
#' @examples
poles <- function(a,b,c){
  # Calculer le discriminant
  discriminant <- b^2 - 4 * a * c

  # Vérifier la nature des racines en fonction du discriminant
  if (discriminant > 0) {
    # Deux racines réelles distinctes
    pole1 <- (-b + sqrt(discriminant)) / (2 * a)
    pole2 <- (-b - sqrt(discriminant)) / (2 * a)
    return(c(pole1, pole2))
  } else if (discriminant == 0) {
    # Une racine réelle double
    pole <- -b / (2 * a)
    return(pole)
  } else {
    # Racines complexes
    pole1 <- (-b + sqrt(as.complex(discriminant))) / (2 * a)
    pole2 <- (-b - sqrt(as.complex(discriminant))) / (2 * a)
    return(c(pole1, pole2))
  }


return(poles)
}
