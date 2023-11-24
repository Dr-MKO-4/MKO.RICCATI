#' Calcul de la dérivée numérique d'une fonction
#'
#' Cette fonction calcule la dérivée numérique d'une fonction à un point donné.
#' Elle utilise une approximation de la dérivée basée sur une différence centrée,
#' (f(x + h) - f(x - h)) / (2 * h), où h est une petite valeur.
#'
#' @param f La fonction dont on veut calculer la dérivée.
#' @param x Le point où la dérivée doit être calculée.
#' @param h La valeur de h utilisée dans la formule de la différence centrée.
#'          Par défaut, elle est fixée à .Machine$double.eps^0.25.
#' @return La dérivée de la fonction f au point x.
#' @examples
#' derivee_numerique(function(x) x^2, 2)
derivee_numerique <- function(f, x, h = 1e-5) {
  (f(x + h) - f(x - h)) / (2 * h)
}

