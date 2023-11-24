#' Vérifier si une fonction est dérivable
#'
#' Cette fonction tente de calculer la dérivée d'une fonction pour vérifier si elle est dérivable.
#' @param f Une fonction à vérifier.
#' @param h Un petit nombre utilisé pour le calcul de la différence finie. Par défaut, h = 1e-7.
#' @return `TRUE` si la fonction est dérivable, `FALSE` sinon.
est_derivale <- function(f, h = .Machine$double.xmin) {
  tryCatch({
    derivee(f, h)
    TRUE
  }, error = function(e) {
    FALSE
  })
}
