#' Pobranie i przygotowanie danych z pliku csv
#'
#' Funkcja przygotujDaneCSV() sczytuje dane, osobno aukji aktywnych i zakończonych (format csv) 
#' oraz wstepnie przygotowuje je do pracy z kolejnymi funkcjami w tym pakiecie.
#'
#' @param adres_aktywnych sciezka do pliku w formacie csv.
#' @param adres_zakonczonych sciezka do pliku w formacie csv.
#' @param model wartość logiczna określająca, czy funkcja ma użyć modelu do przybliżenia liczby sprzedanych sztuk
#'
#' @examples
#' przygotujDaneCSV(".../active.csv", ".../closed.csv")
#' przygotujDaneCSV(".../active169890.csv", ".../closed169890.csv", model = FALSE)
#' 
#' @export

przygotujDaneCSV <- function(adres_aktywnych, adres_zakonczonych, model = TRUE){
  active <- read.csv(adres_aktywnych)
  closed <- read.csv(adres_zakonczonych)
  active <- active %>%
    mutate(Status = "active") %>%
    na.omit()
  closed <- closed %>%
    mutate(Status = "closed") %>%
    na.omit()
  aukcje <- rbind(active, closed) 
  aukcje <- aukcje %>%
    filter(Id_kategorii %in% naszeKategorie$Id_kategorii)
  aukcje <- aukcje %>%
    mutate(Sprz_sztuki = Sprzedane*(vmodelSzt(Cena)),
           Wartosc = Sprz_sztuki*Cena) %>%
  distinct()
  return(aukcje)
  
} 
