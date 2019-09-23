#' Przygotowanie danych do narysowania z nich wykresu
#'
#' Funkcja przygotujDoWykresu() sama w sobie nie robi nic ciekawego. Ma ona za zadanie przetworzyc
#' dane tak, zeby potem wygodnie bylo z niej rysowac wykres obrazujacy ranking.
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategoria liczba (wektor) bedaca id interesujacej nasz kategorii
#' @param k liczba osob, ktore chcemy zawrzec na wykresie; domyslnie k = 30
#' @param id id sprzedawcy, ktorego chcemy wyroznic; domyslnie nasze id, czyli 169890
#' 
#' @examples
#' przygotujDoWykresu(aukcje, 122815)
#'
#' @export




przygotujDoWykresu <- function(dane, kategoria, k = 30, id = 169890){
  d <- stworzRanking(dane, kategoria, k)
  nm <- which(d$Id_sprzedawcy == id)
  prom <- d$Wartosc_promowanych
  reg <- d$Wartosc_zwyklych
  if(!identical(nm, integer(0))){
    p <- c(rep("promoted", nm-1), "npromoted", rep("promoted", length(prom) - nm))
    r <- c(rep("regular", nm-1), "nregular", rep("regular", length(prom) - nm))
  }else{
    p <- c(rep("promoted", length(prom)))
    r <- c(rep("regular", length(reg)))
  }
  d2 <- data.frame(Id_sprzedawcy = rep(d$Id_sprzedawcy, 2),
                   Wartosc = c(prom, reg),
                   Promowanie = c(p,r))
  return(d2)
}