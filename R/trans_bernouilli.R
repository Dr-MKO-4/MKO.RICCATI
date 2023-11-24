#' 'riccati_to_bernoulli' est une fonction qui transforme une équation de Riccati en une équation de Bernoulli.
#'
#' y' = a + b*y + c*y^2
#' Si y_particular est une solution particulière de l'équation de Riccati,
#' alors z = 1/(y - y_particular) est une solution de l'équation de Bernoulli
#' z' + (b - c*y_particular)*z = c
#' y' = a + b*y + c*y^2
#'
#' @param a La fonction a(x)
#' @param b La fonction b(x)
#' @param d La fonction d(x)
#' @param y_particular Une solution particulière de l'équation de Riccati
#' @return Les paramètres de l'équation de Bernoulli associée.
#' @examples
#' a <- function(t) { return(sin(t)-exp(-t^2)) }
#' b <- function(t) { return(cos(t^3)) }
#' d <- function(t) { return(tan(t)) }
#' y_particular <- function(t) { return(exp(t)) }
#' riccati_to_bernoulli(a, b, d, y_particular)
riccati_to_bernoulli <- function(a, b, d, y_particular) {
  return(list(a = deparse(substitute(a)),
              b = paste(deparse(substitute(b)), " - ", deparse(substitute(d)), "*", deparse(substitute(y_particular))),
              d = deparse(substitute(d))))
}


