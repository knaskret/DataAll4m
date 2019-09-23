#' Model opisujący liczbę sprzedanych sztuk na osobę
#'
#' Funkcja vmodelSzt() dla podanego wektora cen zwraca wektor przybliżych liczb sprzedanych sztuk na osobę.
#' Jest to zwektoryzowana wersja funkcji modelSzt()
#' \deqn{y = a + b \cdot \frac{x^{-0,5} - 1}{-0,5},} gdzie
#' \deqn{a = \left\{ \begin{array}{ll}
#' 2,970639 & \textrm{gdy $x<100$},\\
#' 2,248757 & \textrm{gdy $x \in [100, 300)$},\\
#' 1 & \textrm{gdy $ x > 300 $},
#' \end{array} \right.}
#' @param ceny wektor cen produktów (double).
#'
#' @examples
#' vmodelSzt(c(35, 27, 1, 8))
#' vmodelSzt(aukcje$Cena)
#' 
#' @export

vmodelSzt <- function(ceny){
  sapply(ceny, modelSzt)
}