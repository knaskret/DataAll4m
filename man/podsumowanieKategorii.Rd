% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/podsumowanieKategorii.R
\name{podsumowanieKategorii}
\alias{podsumowanieKategorii}
\title{Tabela ze statystykami podsumowującymi kategorię}
\usage{
podsumowanieKategorii(dane, kategoria)
}
\arguments{
\item{dane}{dane przygotowane przez funkcje przygotujDane();}

\item{kategoria}{liczba (wektor) bedaca id interesujacej nasz kategorii}
}
\description{
Funkcja podsumowanieKategorii() tworzy tabelę, w której wypisana jest liczba aukcji ogólem,
aktywnych, nieprowmowanych, ich wartości oraz rozkład cen dla wszystkich aukcji, tylko promowanych,
tylko nieprowmowanych i tylko naszych.
}
\examples{
podsumowanieKategorii(aukcje, 260310)

}
