#' Napis przedstawiajay kategorie
#'
#' Funkcja napiszKat() przeszukuje baze kategorii Allegro i wypisuje wszystkie kategorie i podkategorie,
#' polaczone znakiem strzalki
#'
#' @param nazwa id_kat integer - id interesujacej nas kategorii
#' @param symbol symbol łączący kolejne gałęzie kategorii
#'
#' @examples napiszKat(67307)
#' @examples napiszKat(67307, symbol = "-")
#'
#' @export

napiszKat <- function(id_kat, symbol = " -> "){
  co <- naszeKategoriePL %>% filter(Id_kategorii == id_kat)
  ktore <- min(which(co == ""))
  napis <- co[[2]]
  for(i in 3:(ktore-1)){
    napis <- paste(napis, co[[i]], sep = symbol)
  }
  napis <- paste(co[[1]], napis, sep = ": ")
  return(napis)
}