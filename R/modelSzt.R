#' Model opisujący liczbę sprzedanych sztuk na osobę
#'
#' Funkcja modelSzt() dla podanej ceny oblicza przybliżoną liczbę sprzedanych sztuk na osobę.
#' Funkcja działa dla pojedynczej wartości, sama w sobie jest mało przydatna. Jest jedynie potrzebna
#' do działania funkcji vmodelSzt(), która jest zwektoryzowaną wersją tej funkcji, i to jej polecam
#' używać.
#'
#' @param cena double - cena produktu.
#'
#' @examples
#' modelSzt(35)
#' 
#' @export

modelSzt <- function(cena){
  l = -0.5
  if(cena<100){
    y = 2.970639 - 1.039033*boxcoxTransform(cena, l) 
  }else{
    if(cena < 300){
      y = 2.246757 - 0.6221353*boxcoxTransform(cena, l) 
    }else{
      y = 1
    }
  }
  return(y)
}

