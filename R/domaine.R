#' Calculer le domaine de validité d'une fonction
#'
#' Cette fonction détermine le domaine de validité d'une fonction pour une gamme de valeurs.
#'
#' @param f La fonction dont on veut déterminer le domaine de validité.
#' @param borne_inf La borne inférieure de la gamme de valeurs à tester.
#' @param borne_sup La borne supérieure de la gamme de valeurs à tester.
#'
#' @return Un vecteur contenant toutes les valeurs de x pour lesquelles f peut être évalué sans erreur.
#' @export
#'
#' @examples
#' f <- function(x) sqrt(x)
#' borne_inf <- -10
#' borne_sup <- 10
#' domaine(f, borne_inf, borne_sup)
calcul_domaine <- function(f, borne_inf = -100, borne_sup = 100) {
  x <- seq(borne_inf, borne_sup, by = 0.01)
  domaine <- sapply(x, function(val) {
    resultat <- tryCatch({
      f(val)
    }, warning = function(w) {
      return(NA)
    }, error = function(e) {
      return(NA)
    })
    !is.na(resultat)
  })
  return(x[domaine])
}


