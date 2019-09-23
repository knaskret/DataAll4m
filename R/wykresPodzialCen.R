#' Wykres przedstawiajacy sprzedaz z podzialem na klasy cenowe
#'
#' Funkcja wykresPodzialCen() tworzy wykres, na ktorym dla interesujacej nas kategorii Allegro przedstawione
#' sa wartosci lub ilosci sprzedazy w poszczegolnych klasach cenowych.
#' Sposob podzialu na klasy cenowe okreslamy sami.
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param kategoria liczba (wektor) bedaca id interesujacej nasz kategorii
#' @param podzial wektor liczb bedacych granicami poszczegolnych kategorii
#' @param typ zmienna okreslajaca czy chcemy na wykresie przedstawic wartosc ("W"), czy ilosc ("I")
#' @param zakres zakres cenowy nas interesujacy
#' 
#' @examples
#' WykresPodzialCen(aukcje, 260310, p, typ = "W", zakres= c(0, 2000))
#' WykresPodzialCen(aukcje, 260310, p, typ = "I")
#'
#' @export

wykresPodzialCen <- function(dane, kategoria, podzial, typ,
                             zakres = c(min(dane$Cena), max(dane$Cena)), s = 12, a = 90){
  d <- dane %>%
    filter(Id_kategorii %in% kategoria, Cena > zakres[1] & Cena < zakres[2])
  d <- d %>%
    mutate(Klasa_cenowa = rep(1, dim(d)[1]))
  l <- length(podzial)
  for(i in 1:l){
    d$Klasa_cenowa[d$Cena > podzial[i] & d$Cena <= podzial[i+1]] <- i
  }
  nazwy <- paste0(rep("[", l-1), podzial[1:l-1], ",", podzial[2:l], ")")
  n <- data_frame(Klasa_cenowa = 1:(l-1),
                  Nazwa = nazwy)
  if(typ == "W"){
    d <- d %>%
      group_by(Klasa_cenowa, Promowanie) %>%
      summarise(y = sum(Wartosc))
    nazwa = "Wartość"
  }else{
    d <- d %>%
      group_by(Klasa_cenowa, Promowanie) %>%
      summarise(y = sum(Sprzedane))
    nazwa = "Ilość"
  }
  d <- left_join(d, n) %>%
    arrange(Klasa_cenowa)
  ggplot(d, aes(x = reorder(Nazwa, Klasa_cenowa), y = y,
                fill = Promowanie)) + 
    geom_bar(stat="identity", color = "black") +
    theme(legend.position = c(0.8, 0.9), legend.title = element_blank(),
          legend.text = element_text(size = 12)) +
    scale_fill_manual(name = "", values = c("promoted" = "lightcoral", "regular" = "gray45"),
                      labels = c("aukcje promowane", "aukcje niepromowane")) +
    ylab(nazwa) +
    xlab("Klasa cenowa") +
    theme(axis.text.x = element_text(size = s, angle = a)) +
    scale_x_discrete(drop=F)
}


