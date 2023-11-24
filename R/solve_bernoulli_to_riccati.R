#' 'bernoulli_to_riccati' est une fonction qui transforme une solution de l'équation de Bernoulli en une solution de l'équation de Riccati.
#'
#' z' + (b - c*y_particular)*z = c
#'
#' @param z La solution de l'équation de Bernoulli
#' @param y_particular Une solution particulière de l'équation de Riccati
#' @return La solution de l'équation de Riccati.
#' @examples
#' # Définir les paramètres de l'équation de Riccati
#' a <- function(t) { return(sin(t)) }
#' b <- function(t) { return(cos(t^3)) }
#' d <- function(t) { return(t) }
#' y_particular <- function(t) { return(sin(t)) }
#' # Transformer l'équation de Riccati en une équation de Bernoulli
#' params_bernoulli <- riccati_to_bernoulli(a(t), b(t), c(t), y_particular(t))
#' # Résoudre l'équation de Bernoulli
#' t <- 10
#' y_init <- 1 # y(0) = 1
#' solution_bernoulli <- solve_bernoulli(t, y_init, params_bernoulli)
#' # Transformer la solution de l'équation de Bernoulli en une solution de l'équation de Riccati
#' solution_riccati <- bernoulli_to_riccati(solution_bernoulli, y_particular,t)
#' # Afficher la solution de l'équation de Riccati
#' plot(solve_bernoulli(t, y_init, params), type = "l", col = couleurs[1],ylim=c(-6,12))
#' plot(solution_riccati[!is.infinite(solution_riccati[,1]),],col=couleurs[2],type="l")
#' print(solution_riccati)
bernoulli_to_riccati <- function(z, y_particular,t) {
  # y = 1/z + y_particular
  y <- 1/z + y_particular(t)
  return(y)
}
