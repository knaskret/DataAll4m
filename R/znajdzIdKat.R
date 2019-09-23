#' Id interesujacej nas kategorii Allegro
#'
#' Funkcja znajdzIdKat() przeszukuje baze kategorieAllegro i wypisuje wszystkie id (pod)kategorii, ktore
#' mieszcza sie w interesujacej nas kategorii
#'
#' @param nazwa string - nazwa (pod)kategorii ktora nas interesuje, np "Instrumenty", "Bebny i bebenki"
#' (bez przecinkow i polskich znakow);
#' @param nr_podkat integer - w drzewie kategorii Allegro mamy 7 mozliwych poziomow. nr_podkat
#' okresla poziom, ktory chcemy przeszukac
#'
#' @examples
#' znajdzIdKat("Instrumenty perkusyjne", 3)
#' znajdzIdKat("Kapodastry", 5)
#'
#' @export

znajdzIdKat <- function(nazwa, nr_podkat){
  kategorieAllegro$Id_kategorii[kategorieAllegro[nr_podkat+1] == nazwa]
}