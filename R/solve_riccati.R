#' Résoudre une équation de Riccati
#'
#' Cette fonction résout une équation de Riccati en la transformant en une équation de Bernoulli.
#' Elle renvoie la solution de l'équation de Riccati et le domaine de validité des paramètres d'entrée.
#'
#' @param a Une fonction de t pour le paramètre a de l'équation de Riccati.
#' @param b Une fonction de t pour le paramètre b de l'équation de Riccati.
#' @param d Une fonction de t pour le paramètre d de l'équation de Riccati.
#' @param y_particular Une fonction de t pour le paramètre y_particular de l'équation de Riccati.
#' @param t Le temps auquel résoudre l'équation.
#' @param y_init La condition initiale y(0).
#' @param borne_inf La borne inférieure du domaine de validité des paramètres d'entrée (par défaut -100).
#' @param borne_sup La borne supérieure du domaine de validité des paramètres d'entrée (par défaut 100).
#' @return Une liste contenant la solution de l'équation de Riccati et le domaine de validité des paramètres d'entrée.
#' @examples
#' a <- function(x) { return(sin(x)-cos(-x^2)) }
#' b <- function(x) { return(cos(x^3)) }
#' d <- function(x) { return(x) }
#' y_particular <- function(x) {return((x^3-2 * x+1)/(2*x^2+1))}
#' t <- 1
#' y_init <- 1
#' sol <- solve_riccati(a, b, d, y_particular,y_init,t)
#' print(sol)
#' plot(sol[!is.infinite(sol[,1]),],type='l')
solve_riccati <- function(a, b, d, y_particular, y_init, t) {
  # Vérifier que les fonctions sont de classe C1
  domaine_a <- construire_domaine_C1(a)
  domaine_b <- construire_domaine_C1(b)
  domaine_d <- construire_domaine_C1(d)
  domaine_y_particular <- construire_domaine_C1(y_particular)

  # Trouver le plus petit domaine commun
  #domaine_commun <- intersect(intersect(domaine_a, domaine_b), intersect(domaine_d, domaine_y_particular))

  # Transformer l'équation de Riccati en une équation de Bernoulli
  params_bernoulli <- riccati_to_bernoulli(a, b, d, y_particular)

  # Résoudre l'équation de Bernoulli
  solution_bernoulli <- solve_bernoulli(t, y_init, params_bernoulli)

  # Transformer la solution de l'équation de Bernoulli en une solution de l'équation de Riccati
  solution_riccati <- bernoulli_to_riccati(solution_bernoulli, y_particular,t)

  # Retourner la solution et le domaine
  return(solution_riccati)
}

