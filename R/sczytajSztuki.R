#' Pobranie danych o liczbie sprzedanych sztuk z aukcji Allegro
#'
#' Funkcja sczytajSztuki() sczytuje ze strony aukci Allegro liczbe osob, ktore kupily ten produkt
#' oraz liczbe sprzedanych sztuk.
#'
#' @param nr_aukcji numer interesujacej nas aukcji.
#'
#' @examples 
#' sczytajSztuki(7387674525)
#' 
#' @export


sczytajSztuki <- function(nr_aukcji){
  adres <- paste("https://allegro.pl/oferta/", nr_aukcji, sep = "")
  oferta_html <- html(adres)
  wez_tekst <- oferta_html %>%
    html_node("div ._9a071_RRpJq") %>%
    html_text()
  slowa <- strsplit(wez_tekst, " ")
  liczba_osob <- as.numeric(slowa[[1]][1])
  liczba_sztuk <- as.numeric(slowa[[1]][4])
  return(c(liczba_osob, liczba_sztuk))
}