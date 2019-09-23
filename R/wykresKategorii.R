#' Wartości kategorii Allegro
#'
#' Funkcja wykresKategorii() wykresy, na których dla każdej kategorii narysowana jest jej wartość,
#' z podziałem na aukcje promowane, niepromowane i nasze. Kategorie są posortowane według wartości
#' aukcji niepromowanych. Liczby na słupkach to średnie ceny aukcji w danej kategorii z tym samym podziałem.
#'
#' @param dane dane przygotowane przez funkcje przygotujDane();
#' @param id id sprzedawcy, ktorego chcemy wyroznic; domyslnie nasze id, czyli 169890
#' 
#' @examples
#' wk <- wykresKategorii(dane)
#' wk$wykres1
#'
#' @export


wykresKategorii <- function(dane, id = 169890){
  srednie_ceny_zwykle <- dane %>%
    filter(Id_sprzedawcy != id, Promowanie == "regular") %>%
    group_by(Id_kategorii) %>%
    summarise(Wartosc_zwyklych = mean(Cena))

  srednie_ceny_promowane <- dane %>%
    filter(Id_sprzedawcy != id, Promowanie == "promoted") %>%
    group_by(Id_kategorii) %>%
    summarise(Wartosc_promowanych = mean(Cena))

  srednie_ceny_nasze <- dane %>%
    filter(Id_sprzedawcy == id) %>%
    group_by(Id_kategorii) %>%
    summarise(Nasza_wartosc = mean(Cena))

  srednie_ceny <- full_join(srednie_ceny_nasze, srednie_ceny_zwykle)
  srednie_ceny <- full_join(srednie_ceny, srednie_ceny_promowane)

  srednie_ceny <- gather(srednie_ceny, Co, sr_wart_aukcji, -Id_kategorii)
  srednie_ceny[is.na(srednie_ceny)] <- 0

  srednie_ceny$Co <- as.factor(srednie_ceny$Co)
  srednie_ceny$Co <- factor(srednie_ceny$Co, levels=rev(levels(srednie_ceny$Co)))
  srednie_ceny$Co <- relevel(srednie_ceny$Co, "Nasza_wartosc")

  naszeWartosci <- dane %>%
    filter(Id_sprzedawcy == id) %>%
    group_by(Id_kategorii) %>%
    summarise(Nasza_wartosc = sum(Wartosc))

  wartosciPromowanych <- dane %>%
    filter(Id_sprzedawcy != id, Promowanie == "promoted") %>%
    group_by(Id_kategorii) %>%
    summarise(Wartosc_promowanych = sum(Wartosc))

  wartosciZwyklych <- dane %>%
    filter(Id_sprzedawcy != id, Promowanie == "regular") %>%
    group_by(Id_kategorii) %>%
    summarise(Wartosc_zwyklych = sum(Wartosc))


  wartosci <- left_join(wartosciZwyklych, naszeWartosci)
  wartosci <- left_join(wartosci, wartosciPromowanych)
  wartosci2 <- gather(wartosci, Co, Wartosc, -Id_kategorii)
  wartosci2$Wartosc[is.na(wartosci2$Wartosc)] <- 0

  wartosci2$Co <- as.factor(wartosci2$Co)
  wartosci2$Co <- factor(wartosci2$Co, levels=rev(levels(wartosci2$Co)))
  wartosci2$Co <- relevel(wartosci2$Co, "Nasza_wartosc")
  wartosci3 <- left_join(wartosci2, wartosciZwyklych)
  wartosci3 <- left_join(wartosci3, naszeWartosci)

  wartosci3[is.na(wartosci3)] <- 0

  wartosci3 <- wartosci3 %>% mutate(lacznie = Wartosc_zwyklych + Nasza_wartosc)
  id_kat <- unique(wartosci3$Id_kategorii)
  wartosci3 <- wartosci3 %>% arrange(desc(lacznie))
  idk <- unique(wartosci3$Id_kategorii)
  idk_nr <- data.frame(Id_kategorii = idk,
                     numer = 1:92) 
  wartosci3 <- left_join(wartosci3, idk_nr)
  wartosci3 <- wartosci3 %>%
    group_by(numer) %>%
    mutate(lacznie3 = sum(Wartosc)) %>%
    ungroup() %>%
    mutate(pozycja = if_else(Co == "Nasza_wartosc", Nasza_wartosc,
                           if_else(Co == "Wartosc_zwyklych", lacznie*(5/8), lacznie3)))
  wartosci3 <- left_join(wartosci3, srednie_ceny, by = c("Id_kategorii", "Co"))


  wartosci3_cz1 <- wartosci3 %>% filter(numer <= 31)
  wartosci3_cz2 <- wartosci3 %>% filter(numer > 31 & numer <= 62)
  wartosci3_cz3 <- wartosci3 %>% filter(numer > 62)

  nazwy_numery <- left_join(naszeKategorie, idk_nr)
  nazwy_numery_2 <- nazwy_numery %>%
    mutate(napis = paste(kat_1, kat_2, kat_3, kat_4, kat_5, kat_6, kat_7, sep = "-"))

  tabelka_nazw <- nazwy_numery_2 %>% select(numer, Id_kategorii, napis) %>% arrange(numer)
  tabelka_nazw_cz1 <- tabelka_nazw %>% filter(numer <= 31)
  tabelka_nazw_cz2 <- tabelka_nazw %>% filter(numer > 31 & numer <= 62)
  tabelka_nazw_cz3 <- tabelka_nazw %>% filter(numer > 62)

  p <- ggplot(wartosci3, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) + 
    theme(legend.position = "none") + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                            "Wartosc_zwyklych" = "gray45",
                                            "Nasza_wartosc" = "black"))+
    ylab("") +
    xlab("") +
    scale_y_continuous(labels=number) +
    geom_vline(xintercept=31.5, linetype="dashed", color = "black", size = 1) +
    geom_vline(xintercept=62.5, linetype="dashed", color = "black", size = 1)
  
  p1 <- ggplot(wartosci3_cz1, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    theme(legend.position = c(0.8, 0.9), legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                          "Wartosc_zwyklych" = "gray45",
                                          "Nasza_wartosc" = "black"),
                      labels = c("nasza wartość", "wartość niepromowanych", "wartość promowanych"))+
    ylab("Wartość") +
    xlab("Kategoria") +
    scale_y_continuous(labels=number) +
    geom_text(aes(y = pozycja, label=round(sr_wart_aukcji, 0)), vjust = -0.5, color= "black", size=4)
  
  p2.1 <- ggplot(wartosci3_cz2, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    theme(legend.position = "top") +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                            "Wartosc_zwyklych" = "gray45",
                                            "Nasza_wartosc" = "black"))+
    ylab("Wartość") +
    xlab("Kategoria") +
    scale_y_continuous(labels = number, limits = c(0, layer_scales(p1)$y$range$range[2]))
  
  p2 <- ggplot(wartosci3_cz2, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) +
    theme(legend.position = c(0.8, 0.9), legend.text = element_text(size = 12),
          legend.title = element_blank()) + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                            "Wartosc_zwyklych" = "gray45",
                                            "Nasza_wartosc" = "black"),
                      labels = c("nasza wartość", "wartość niepromowanych", "wartość promowanych"))+
    ylab("Wartość") +
    xlab("Kategoria") +
    scale_y_continuous(labels = number) +
    geom_text(aes(y = pozycja, label=round(sr_wart_aukcji, 0)), vjust = -0.5, color="black", size=4)
  
  p3.1 <- ggplot(wartosci3_cz3, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    theme(legend.position = "top") +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                            "Wartosc_zwyklych" = "gray45",
                                            "Nasza_wartosc" = "black"))+
    ylab("Wartość") +
    xlab("Kategoria") +
    scale_y_continuous(labels = number, limits = c(0, layer_scales(p1)$y$range$range[2]))
  
  p3 <- ggplot(wartosci3_cz3, aes(x = reorder(numer, -lacznie), y = Wartosc, fill = Co)) + 
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), color = "black") +
    theme(legend.position = c(0.8, 0.9), legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    scale_fill_manual(name = "", values = c("Wartosc_promowanych" = "lightcoral",
                                            "Wartosc_zwyklych" = "gray45",
                                            "Nasza_wartosc" = "black"),
                      labels = c("nasza wartość", "wartość niepromowanych", "wartość promowanych"))+
    ylab("Wartość") +
    xlab("Kategoria") +
    scale_y_continuous(labels = number, limits = c(0, 15500)) +
    geom_text(aes(y = pozycja, label=round(sr_wart_aukcji, 0)), vjust = -0.5, color="black", size=4.5)
  return(list("wykresCaly" = p,
              "wykres1" = p1, 
              "wykres2skala" = p2.1, 
              "wykres2" = p2, 
              "wykres3skala" = p3.1, 
              "wykres3" = p3,
              "nazwy1" = tabelka_nazw_cz1,
              "nazwy2" = tabelka_nazw_cz2,
              "nazwy3" = tabelka_nazw_cz3,
              "tabela" = wartosci3))
}

