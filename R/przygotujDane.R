#' Pobranie i przygotowanie danych z pliku xlsx
#'
#' Funkcja przygotujDaneXLSX() sczytuje dane (format xlsx) oraz wstepnie przygotowuje je do pracy 
#' z kolejnymi funkcjami w tym pakiecie.
#'
#' @param adres_danych sciezka do pliku w formacie xlsx.
#'
#' @examples 
#' przygotujDaneXLSX(".../aukcje.xlsx")
#' 
#' @export

przygotujDaneXLSX <- function(adres_danych){
  dane <- read.xlsx(adres_danych)
  dane$Cena <- as.numeric(dane$Cena)
  dane$Id_sprzedawcy <- as.integer(dane$Id_sprzedawcy)
  dane <- dane %>% mutate(Wartosc = Sprzedane*Cena)
  dane <- distinct(dane)
  return(dane)
} 