#' Tabela ze statystykami podsumowującymi kategorię
#'
#' Funkcja podsumowanieKategorii() tworzy tabelę, w której wypisana jest liczba aukcji ogólem,
#' aktywnych, nieprowmowanych, ich wartości oraz rozkład cen dla wszystkich aukcji, tylko promowanych,
#' tylko nieprowmowanych i tylko naszych.
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategoria liczba (wektor) bedaca id interesujacej nasz kategorii

#' 
#' @examples
#' podsumowanieKategorii(aukcje, 260310)
#'
#' @export

podsumowanieKategorii <- function(dane, kategoria){
  dane <- dane %>%
    filter(Id_kategorii %in% kategoria)
  pods_og <- dane %>%
    summarise(l.aukcji = n(), l.aukcji.akt = sum(Status == "active"),
              l.aukcji.zamk = sum(Status == "closed"), Wartosc = sum(Wartosc), 
              Wartosc.aktywnych = sum(dane$Wartosc*(dane$Status == "active")),
              Wartosc.zakmnietych = sum(dane$Wartosc*(dane$Status == "closed")),
              min.Cena = min(Cena), kw1.Cena = quantile(Cena)[[2]],
              median.Cena = median(Cena), mean.Cena = mean(Cena), kw3.Cena = quantile(Cena)[[4]],
              max.Cena = max(Cena))
  
  dane2 <- dane %>%
    filter(Promowanie == "regular")
  pods_zw <- dane2 %>%
    summarise(l.aukcji = n(), l.aukcji.akt = sum(Status == "active"),
              l.aukcji.zamk = sum(Status == "closed"), Wartosc = sum(Wartosc), 
              Wartosc.aktywnych = sum(dane2$Wartosc*(dane2$Status == "active")),
              Wartosc.zakmnietych = sum(dane2$Wartosc*(dane2$Status == "closed")),
              min.Cena = min(Cena), kw1.Cena = quantile(Cena)[[2]],
              median.Cena = median(Cena), mean.Cena = mean(Cena), kw3.Cena = quantile(Cena)[[4]],
              max.Cena = max(Cena))
  dane3 <- dane %>%
    filter(Promowanie == "promoted") 
  pods_pr <- dane3 %>%
    summarise(l.aukcji = n(), l.aukcji.akt = sum(Status == "active"),
              l.aukcji.zamk = sum(Status == "closed"), Wartosc = sum(Wartosc), 
              Wartosc.aktywnych = sum(dane3$Wartosc*(dane3$Status == "active")),
              Wartosc.zakmnietych = sum(dane3$Wartosc*(dane3$Status == "closed")),
              min.Cena = min(Cena), kw1.Cena = quantile(Cena)[[2]],
              median.Cena = median(Cena), mean.Cena = mean(Cena), kw3.Cena = quantile(Cena)[[4]],
              max.Cena = max(Cena))
  dane4 <- dane %>%
    filter(Id_sprzedawcy == 169890)
  pods_nasz <- dane4 %>%
    summarise(l.aukcji = n(), l.aukcji.akt = sum(Status == "active"),
              l.aukcji.zamk = sum(Status == "closed"), Wartosc = sum(Wartosc), 
              Wartosc.aktywnych = sum(dane4$Wartosc*(dane4$Status == "active")),
              Wartosc.zakmnietych = sum(dane4$Wartosc*(dane4$Status == "closed")),
              min.Cena = min(Cena), kw1.Cena = quantile(Cena)[[2]],
              median.Cena = median(Cena), mean.Cena = mean(Cena), kw3.Cena = quantile(Cena)[[4]],
              max.Cena = max(Cena))
  pods <- rbind(pods_og, pods_zw, pods_pr, pods_nasz)
  pods <- pods %>%
    round(1) %>%
    `rownames<-`(c("ogółem", "niepromowane", "promowane", "nasze"))
  pods[is.na(pods)] <- 0
  pods[pods == Inf] <- 0
  pods[pods == -Inf] <- 0
  pods <- pods %>% rename("liczba aukcji ogółem" = l.aukcji,
                          "liczba aukcji aktywnych" = l.aukcji.akt,
                          "liczba aukcji zakończonych" = l.aukcji.zamk,
                          "wartość aukcji ogólnie" = Wartosc,
                          "wartość aukcji aktywnych" = Wartosc.aktywnych,
                          "wartość aukcji zakończonych" = Wartosc.zakmnietych,
                          "cena - minimum" = min.Cena,
                          "cena - 1. kwantyl" = kw1.Cena,
                          "cena - mediana" = median.Cena,
                          "cena - średnia" = mean.Cena,
                          "cena - 3. kwantyl" = kw3.Cena,
                          "cena - maksimum" = max.Cena)
  return(pods)
}
  