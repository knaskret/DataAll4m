#' Wykres przedstawiajacy ranking sprzedawcow w konkretnej kategorii
#'
#' Funkcja rysujRanking() tworzy wykres, na ktorym dla interesujacej nas kategorii Allegro przedstawione
#' sa wartosci dla pierwszych k sprzedawcow oraz udzial aukcji promowanych oraz niepromowanych.
#' Slupek przedstawiajacy nasza wartosc wyroznia sie ciemniejszymi kolorami.
#' Jest rowniez mozliwe narysowanie rankingu dla szerszej kategorii, np. "Instrumenty".
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategoria liczba (wektor) bedaca id interesujacej nasz kategorii
#' @param k liczba osob, ktore chcemy zawrzec w rankingu; domyslnie k = 30
#' @param id id sprzedawcy, ktorego chcemy wyroznic; domyslnie nasze id, czyli 169890
#' 
#' @examples
#' rysujRanking(aukcje, 122815)
#' rysujRanking(aukcje, kategoria = c(122815, 122816, 122818, 122819))
#' rysujRanking(aukcje, 122819, k = 10)
#'
#' @export

rysujRanking <- function(dane, kategoria, k = 30, id = 169890){
  d <- przygotujDoWykresu(dane, kategoria, k, id)
  levels(d$Promowanie)[levels(d$Promowanie)=="npromoted"] <- "nasze aukcje promowane"
  levels(d$Promowanie)[levels(d$Promowanie)=="nregular"] <- "nasze aukcje niepromowane"
  levels(d$Promowanie)[levels(d$Promowanie)=="promoted"] <- "aukcje promowane"
  levels(d$Promowanie)[levels(d$Promowanie)=="regular"] <- "aukcje niepromowane"
  p <- ggplot(d, aes(x = reorder(Id_sprzedawcy, -Wartosc), y = Wartosc,
                     fill = Promowanie)) + 
    geom_bar(stat="identity", color = "black") +
    theme(legend.position = c(0.8, 0.9), axis.text.x = element_text(angle = 90),
          legend.title = element_blank(), legend.text = element_text(size = 12)) +
    scale_fill_manual(name = "",
                      values = c("aukcje promowane" = "lightcoral",
                                 "aukcje niepromowane" = "gray45",
                                 "nasze aukcje promowane" = "indianred4",
                                 "nasze aukcje niepromowane" = "black")) +
    ylab("Wartość") +
    xlab("Id sprzedawcy") +
    scale_y_continuous(labels=number)
  return(p)
}