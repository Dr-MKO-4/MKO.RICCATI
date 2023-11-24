#' 'construire_domaine_C1' est une fonction qui construit un domaine où tous les paramètres sont de classe C1.
#'
#' @param f La fonction dont on veut déterminer le domaine de validité.
#' @param borne_inf La borne inférieure de la gamme de valeurs à tester.
#' @param borne_sup La borne supérieure de la gamme de valeurs à tester.
#' @return Un vecteur contenant toutes les valeurs de x pour lesquelles f et sa dérivée peuvent être évaluées sans erreur.
#' @examples
#' f <- function(x){return(sin(x))}
#' construire_domaine_C1(f)
#' library(ggplot2)
construire_domaine_C1 <- function(f, borne_inf = -10, borne_sup = 10) {
  x <- seq(borne_inf, borne_sup, by = 0.001)
  domaine <- sapply(x, function(val) {
    resultat <- tryCatch({
      f(val)
      derivee_numerique(f, val)
    }, warning = function(w) {
      return(NA)
    }, error = function(e) {
      return(NA)
    })
    !is.na(resultat)
  })
  return(x[domaine])
}
