#' 'calculer_derivee' est une fonction qui calcule la dérivée d'une fonction donnée.
#'
#' Args:
#' @param expr L'expression mathématique à dériver
#' @param var La variable par rapport à laquelle dériver
#'  @return Un objet représentant la dérivée de l'expression par rapport à la variable.
#'  @example
#'  derive(quote(x^2))
#'  derive(quote(sin(x)))
#' Calculer la dérivée d'une expression mathématique
#'
#'
#' @return La dérivée de l'expression
derive <- function(expr, var = "x") {
  # Utiliser la fonction D pour calculer la dérivée
  deriv_ <- D(expr, var)
  # Retourner l'expression de la dérivée
  return(deriv_)
}

