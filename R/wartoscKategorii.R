#' Wartosc ogolna wybranej kategorii
#'
#' Funkcja wartoscKatgeorii() tworzy tabele, w ktorej dla interesujacej nas kategorii Allegro podana 
#' jest wartosc ogolna tej kategorii. Podana jest rowniez wartosc aukcji promowanych i zwyklych.
#' Jest rowniez mozliwe scalenie kilku kategorii do jednej szerszej kategorii, np. "Instrumenty".
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategorie wektor interesujacych nas kategorii. Domyslnie sa wziete wszystkie mozliwe kategorie
#' w bazie dnaych dane
#' @param group wartosc logiczna okreslajaca, czy chcemy podane wczesniej kategorie scalic w jedna 
#' szersza kategorie. Domyslnie group = FALSE
#' 
#' @examples
#' wartoscKategorii(aukcje)
#' wartoscKategorii(aukcje, kategorie = c(122815, 122816, 122818, 122819), group = TRUE)
#' wartoscKategorii(aukcje, 122815)
#'
#' @export

wartoscKategorii <- function(dane, kategorie = unique(dane$Id_kategorii), group = FALSE){
  d1 <- dane %>%
    filter(Id_kategorii %in% kategorie)
  d <- d1 %>%
    group_by(Id_kategorii, Promowanie) %>%
    summarise(Wartosc = sum(Wartosc))
  dp <- d %>%
    filter(Promowanie == "promoted") %>%
    mutate(Wartosc_promowanych = Wartosc) %>%
    select(Id_kategorii, Wartosc_promowanych)
  dr <- d %>%
    filter(Promowanie == "regular") %>%
    mutate(Wartosc_zwyklych = Wartosc) %>%
    select(Id_kategorii, Wartosc_zwyklych)
  do <- left_join(dp, dr) %>%
    mutate(Wartosc_kategorii = Wartosc_zwyklych + Wartosc_promowanych) %>%
    select(Id_kategorii, Wartosc_kategorii, Wartosc_promowanych, Wartosc_zwyklych) %>%
    arrange(desc(Wartosc_kategorii))
  if(group){
    do <- do %>%
      summarise(Wartosc_kategorii = sum(Wartosc_kategorii), Wartosc_promowanych = sum(Wartosc_promowanych),
                Wartosc_zwyklych = sum(Wartosc_zwyklych))
  }
  return(do)
}