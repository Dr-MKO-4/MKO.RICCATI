#' Vérifier la continuité d'une fonction
#'
#' Cette fonction détermine si une fonction est continue sur son domaine.
#'
#' @param f La fonction à vérifier.
#' @param borne_inf La borne inférieure de la gamme de valeurs à tester.
#' @param borne_sup La borne supérieure de la gamme de valeurs à tester.
#'
#' @return Un vecteur logique indiquant si la fonction est continue pour chaque point dans son domaine.
#' @export
#'
#' @examples
#' f <- function(x) sqrt(x)
#' borne_inf <- -10
#' borne_sup <- 10
#' verifier_continuite(f, borne_inf, borne_sup)
verifier_continuite <- function(f, borne_inf = -100, borne_sup = 100, epsilon = 1e-6) {
  # Définir le domaine comme une séquence de nombres entre borne_inf et borne_sup
  domaine <- seq(borne_inf, borne_sup, length.out = 1000)

  # Vérifier si f est définie sur tout le domaine
  if (any(is.nan(f(domaine)))) {
    stop("la fonction n'est pas définie sur cet intervalle")
  }

  # Initialiser une liste pour stocker les points de discontinuité
  discontinuite <- c()

  # Calculer la limite à gauche et à droite pour chaque point du domaine
  for (a in domaine) {
    # Définition de la fonction g qui est la valeur absolue de la différence entre f(x) et f(a)
    g <- function(x) {abs(f(x) - f(a))}

    # Vérifier si g(x) est inférieur à epsilon pour tous les x dans un voisinage ouvert de a
    voisinage <- seq(a - epsilon, a + epsilon, length.out = 100)
    if (any(g(voisinage) >= epsilon)) {
      # Si g(x) est supérieur ou égal à epsilon pour un x dans le voisinage, ajouter a à la liste des points de discontinuité
      discontinuite <- c(discontinuite, a)
    }
  }

  # Si la liste des points de discontinuité est vide, la fonction est continue sur tout le domaine
  if (length(discontinuite) == 0) {
    print(paste("La fonction est continue sur l'intervalle [", borne_inf, ",", borne_sup, "]"))
    return(TRUE)
  } else {
    # Sinon, imprimer les points de discontinuité
    print("La fonction n'est pas continue aux points suivants :")
    return(discontinuite)
  }
}


