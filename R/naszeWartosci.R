#' Wartosci naszej sprzedazy dla wybranych kategorii
#'
#' Funkcja naszeWartości() tworzy tabele, w ktorej dla kazdej interesujacej nas kategorii Allegro podana 
#' jest wartosc naszej sprzedazy. Podaje rowniez liczbe naszych aukcji promowanych oraz ich wartosc.
#' Jest rowniez mozliwe obliczenie naszej wartosci dla szerszej kategorii, np. "Instrumenty".
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategorie wektor interesujących nas kategorii. Domyslnie sa wziete wszystkie mozliwe kategorie
#' w bazie dnaych dane
#' @param group wartosc logiczna okreslajaca, czy chcemy podane wczesniej kategorie scalic w jedna 
#' szersza kategorie
#' @param id liczba bedaca identyfikatorem interesujacego nas uzytkownika Allegro.
#' Domyslnie id = 169890 (czyli my)
#' @param info_promowanie wartosc logiczna okreslajaca, czy chcemy wyswietlic udzial aukcji promowanych
#' 
#' @examples
#' naszeWartosci(aukcje)
#' naszeWartosci(aukcje, kategorie = c(122815, 122816, 122818, 122819), group = TRUE)
#' naszeWartosci(aukcje, kategorie = 122815, info_promowania = FALSE)
#'
#' @export


naszeWartosci <- function(dane, kategorie = unique(dane$Id_kategorii), group = FALSE,
                          id = 169890, info_promowania = TRUE){
  prom <- dane %>%
    filter(Id_kategorii %in% kategorie, Id_sprzedawcy == id, Promowanie == "promoted")
  reg <- dane %>%
    filter(Id_kategorii %in% kategorie, Id_sprzedawcy == id, Promowanie == "regular")
  if(!group){
    prom <- prom %>%
      group_by(Id_kategorii)
    reg <- reg %>%
      group_by(Id_kategorii)
  }
  prom <- prom %>% 
    summarise(Liczba_promowanych = n(), Wartosc_promowanych = sum(Wartosc))
  reg <- reg %>% 
    summarise(Liczba_zwyklych = n(), Wartosc_zwyklych = sum(Wartosc))
  if(group){
    prom <- prom %>%
    mutate(Id_kategorii = 1)
    reg <- reg %>%
    mutate(Id_kategorii = 1)
  }
  if(dim(prom)[1] < dim(reg)[1]) d <- left_join(reg, prom)
  else d <- left_join(prom, reg)
  d[is.na(d)] <- 0
  d <- d %>%
    mutate(Liczba_aukcji = Liczba_promowanych + Liczba_zwyklych, 
           Nasza_wartosc = Wartosc_promowanych + Wartosc_zwyklych)
  d <- d %>%
      select(Id_kategorii, Liczba_aukcji, Nasza_wartosc, Liczba_promowanych, Wartosc_promowanych)
  
  if(info_promowania) return(d)
  else return(d %>% select(Id_kategorii, Nasza_wartosc))
}
