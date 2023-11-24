#' Calculer la limite d'une fonction
#'
#' Cette fonction utilise le package Ryacas pour calculer la limite d'une fonction à un point donné.
#'
#' @param expression L'expression de la fonction dont on veut déterminer la limite.
#' @param variable La variable de la fonction dont on veut déterminer la limite.
#' @param point Le point où on calcule la limite.
#'
#' @return La limite de la fonction.
#' @export
#'
#' @examples
#' expression <- "x^2 - 3*x + 2"
#' variable <- "x"
#' point <- 1
#' calcul_limite(expression, variable, point)

calcul_limite <- function(expression, variable, point) {
  if (!require(Ryacas0)) {
    install.packages("Ryacas0")
    library(Ryacas0)
  }
  library(Ryacas0)
  # Définir la variable comme une variable symbolique
  x <- yacas(paste(variable))
  # Calculer la limite de l'expression lorsque la variable tend vers le point
  limite <- Limit(expression, variable, point)
  return(as.character(limite))
}
