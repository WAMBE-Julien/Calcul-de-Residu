#' calcul integrale de la fonction de transfert
#'
#' @param R Resistance(en ohms)
#' @param L Inductance(en henrys)
#' @param C Capacité(en farads)
#' @param s a number
#'
#' @return number
#' @export
#'
#' @examples
calculer_residus_RLC <- function(R, L, C) {
  # Coefficients du numérateur et du dénominateur
  num <- c(1)  # Numérateur : 1
  denom <- c(L * C, R * C, 1)  # Dénominateur : L*C*s^2 + R*C*s + 1

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

  # Fonction de transfert H(s)
  H <- function(R,L,C,s) {
    return(1 /((L*C*s^2)+(R*C*s)+1))
  }

  # Calcul des résidus
#'
#'
#' @param s a number
#'
#' @return a function
#' @export
#'
#' @examples
  f <- function(s){
    return((s-k)*H(R,L,C,s))
    }
  residues <- numeric(length(poles))
  for (i in seq_along(poles)) {
    k <- poles[i]
    epsilon <- 1e-10
    residues[i] <- mean(c(f(k-epsilon),f(k+epsilon)))# Calcul du résidu
    residues <- sum(residues[i])
  }
  integral <- 2*pi*1i*residues
  return(integral)
}
