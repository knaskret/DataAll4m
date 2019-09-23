#' Ranking w podanej kategorii Allegro
#'
#' Funkcja stworzRanking() tworzy tabele, w ktorej dla interesujacej nas kategorii Allegro podane jest
#' pierwszych k sprzedawcow o najwiekszej wartosci z informacja o ich wartosci, udziale aukcji
#' promowanych oraz niepromowanych. Jest rowniez mozliwe stworzenie rankingu
#' dla szerszej kategorii, np. "Instrumenty".
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategoria liczba (wektor) bedaca id interesujacej nasz kategorii
#' @param k liczba osob, ktore chcemy zawrzec w rankingu; domyslnie k = 30
#' 
#' @examples
#' stworzRanking(aukcje, 122815)
#' naszeWartosci(aukcje, kategoria = c(122815, 122816, 122818, 122819))
#' naszeWartosci(aukcje, 122819, k = 10)
#'
#' @export

stworzRanking <- function(dane, kategoria, k = 30){
  prom <- dane %>%
    filter(Id_kategorii %in% kategoria, Promowanie == 'promoted') %>%
    group_by(Id_sprzedawcy) %>%
    summarise(Liczba_promowanych = n(), Wartosc_promowanych = sum(Wartosc)) %>%
    arrange(Id_sprzedawcy)
  reg <- dane %>%
    filter(Id_kategorii %in% kategoria, Promowanie == 'regular') %>%
    group_by(Id_sprzedawcy) %>%
    summarise(Liczba_zwyklych = n(), Wartosc_zwyklych = sum(Wartosc)) %>%
    arrange(Id_sprzedawcy)
  if(dim(prom)[1] < dim(reg)[1]){
    d <- left_join(reg, prom)
  }else{
    d <- left_join(prom, reg)
  }
  d[is.na(d)] <- 0
  d <- d %>%
    mutate(Liczba_aukcji = Liczba_promowanych + Liczba_zwyklych,
           Wartosc_sprzedawcy = Wartosc_zwyklych + Wartosc_promowanych) %>%
    arrange(desc(Wartosc_sprzedawcy)) %>%
    head(k) %>%
    mutate(Miejsce = 1:min(k, dim(d)[1]))
  d <- d %>%
    select(Miejsce, Id_sprzedawcy, Wartosc_sprzedawcy, Liczba_aukcji, Liczba_promowanych,
           Wartosc_promowanych, Liczba_zwyklych, Wartosc_zwyklych)
  return(d)
}