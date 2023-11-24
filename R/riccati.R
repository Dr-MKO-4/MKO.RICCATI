#' 'riccati' est une fonction qui represente les parametre de l'quation de riccati.
#'
#' y' = a + b*y + c*y^2
#'
#' @param y La fonction y
#' @param x La variable x
#' @param a La fonction a(x) ecrit sous forme de chaine de caractère
#' @param b La fonction b(x) ecrit sous fprme de chaine de caractère
#' @param c La fonction c(x) écrit sous forme de chaine de caractère
#' @param y0 La fonction y0(x) description qui est la solution particulère de l'équation différentielle de bernouilli ecrit sous forme de chaine de c
#' @return La valeur de l'équation de Riccati.
#' @examples
#' a <- function(t) { return(sin(t)-exp(-t^2)) }
#' b <- function(t) { return(cos(t^3)) }
#' d <- function(t) { return(tan(t)) }
#' y_particular <- function(t) { return(exp(t)) }
#' riccati(a, b, c,y_particular)
#'



declare_riccati <- function(a, b, c, y0) {
  return(paste("y' = ", deparse(substitute(a)), "(x) + ", deparse(substitute(b)), "(x)*y + ", deparse(substitute(c)), "(x)*y^2, où y0 = ", deparse(substitute(y0)), "(x) est une solution particulière de l'équation différentielle de Bernoulli"))
}




