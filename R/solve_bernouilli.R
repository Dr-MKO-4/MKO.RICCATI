#' 'solve_bernoulli' est une fonction qui résout une équation différentielle de Bernoulli.
#'
#' dy/dt = a(t)*y + b(t)
#'
#' @param t L'intervalle de temps pour lequel nous voulons la solution
#' @param y La condition initiale pour y
#' @param params Une liste contenant les fonctions a(t) et b(t)
#' @return La solution de l'équation différentielle pour chaque point dans l'intervalle de temps.
#' @examples
#' a <- function(t) { return(sin(t)-exp(-t^2)) }
#' b <- function(t) { return(cos(t^3)) }
#' d <- function(t) { return(tan(t)) }
#' y_particular <- function(t) { return(exp(t)) }
#' t <- 10
#' y_init <- 1 # y(0) = 1
#' y_init
#' params <- riccati_to_bernoulli(a(t),b(t),d(t),y_particular(t))
#' solve_bernoulli(t, y_init, params)
#' couleurs <- rainbow(10)
#' plot(solve_bernoulli(t, y_init, params), type = "l", col = couleurs[1])
#' for (i in 2:10) {
#'   for (m in 1:10) {
#'       lines(solve_bernoulli(i, m, params), col = couleurs[m])
#'         }
#'         }
#'




solve_bernoulli <- function(t, y_init, params, f) {
  # Installer et charger le package deSolve
  if (!require(deSolve)) {
    install.packages("deSolve")
    library(deSolve)
  }

  # Définir la fonction pour l'équation différentielle
  equation_diff <- function(t, y, params) {
    a <- params[["a"]]
    b <- params[["b"]]
    dy <- a(t) * y + b(t)
    list(dy)
  }

  # Définir les conditions initiales
  y_init <- c(y = y_init)

  # Définir l'intervalle de temps
  times <- seq(0, t, by = 0.01)
  # Résoudre l'équation différentielle
  solution <- lsoda(y = y_init, times = times, func = equation_diff, parms = params)

  # Retourner la solution
  return(solution)
}


