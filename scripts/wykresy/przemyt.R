# Borys Piątek 331725


# Wczytanie bibliotek i funkcji ####

library(readxl)
library(scales)
library(tidyverse)
library(this.path)
library(treemapify)

source("scripts/wykresy/funkcje.R")


# Wczytanie danych ####

przemyt <- read_rds("data_clean/przemyt_2025.rds")

# Nazwa pliku
chart_dir <- basename(this.path()) %>% 
  str_remove("\\..*") %>% 
  str_trim()
chart_dir <- paste0("charts/", chart_dir)

# Paleta kolorów
paleta_sg <- c(
  "Towary handlowe" = "#2c3e50",
  "Pojazdy mechaniczne" = "#d35400",
  "Narkotyki" = "#c0392b"
)


# Wykresy ####

## -- Treemap dla całego przemytu ####

myplot <- ggplot(przemyt, aes(
  area = Wartość, 
  fill = Kategoria, 
  label = Rodzaj, 
  subgroup = Kategoria
)) + 
  
  geom_treemap(color = "white", size = 1) + 
  
  geom_treemap_text(
    color = "white", place = "centre", grow = FALSE, 
    fontface = "bold", size = 14
  ) + 
  
  geom_treemap_subgroup_border(color = "white", size = 3) + 
  
  geom_treemap_subgroup_text(
    color = "white", place = "bottom", grow = TRUE, 
    alpha = 0.2, fontface = "italic"
  ) + 
  
  scale_fill_manual(values = paleta_sg) + 
  
  labs(
    title = "Struktura wartości przechwyconego przemytu",
    subtitle = "Powierzchnia kafelka odpowiada szacunkowej wartości rynkowej (PLN)"
  ) + 
  
  theme_minimal() + 
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 16)
  )

mysaveplot(myplot, chart_dir, "1_drzewo_kafelki")
print(myplot)


## -- Wykres słupkowy dla top 10 towarów ####

top10 <- przemyt %>%
  slice_max(order_by = Wartość, n = 10, with_ties = FALSE)

myplot <- ggplot(top10, aes(x = Wartość, y = reorder(Rodzaj, Wartość), fill = Kategoria)) +
  geom_col(width = 0.7) + 
  
  # Etykiety z wartościami na końcu słupka
  geom_text(
    aes(label = label_number(suffix = " mln zł", scale = 1e-6, accuracy = 0.1)(Wartość)),
    hjust = -0.1, 
    size = 4, 
    color = "black", 
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = paleta_sg) +
  scale_x_continuous(
    labels = label_number(suffix = " mln", scale = 1e-6),
    expand = expansion(mult = c(0, 0.2)) # Poszerzenie osi z prawej strony na tekst
  ) +
  
  labs(
    title = "Top 10 najbardziej wartościowych towarów",
    subtitle = "Zestawienie całościowe z podziałem na kategorie",
    x = "Wartość (w milionach złotych)",
    y = NULL,
    fill = "Kategoria:"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

mysaveplot(myplot, chart_dir, "2_top_10_wartosciowe_slupkowy")
print(myplot)


## -- Wykres słupkowy dla narkotyków ####

narkotyki <- przemyt %>%
  filter(Kategoria == "Narkotyki") %>%
  # Usunięcie kategorii "inne", bo nie ma sprecyzowanej ilości i nie policzy się dla niej ceny
  filter(Rodzaj != "inne") %>% 
  mutate(
    # Wyciągnięcie z tekstu samej wartości liczbową
    Ilosc_num = readr::parse_number(Ilość, locale = locale(decimal_mark = ",")),
    
    # Przypisanie jednostki w zależności do zawartości tekstu
    Jednostka = case_when(
      str_detect(Ilość, "szt") ~ "szt.",
      str_detect(Ilość, "tabl") ~ "tabl.",
      TRUE ~ "kg"
    ),
    
    # Wyliczenie ceny jednostkowej
    Cena_za_jedn = Wartość / Ilosc_num,
    
    # Formatowanie wartości całkowitej 
    Wartość_tekst = ifelse(
      Wartość > 1000000, 
      label_number(scale = 1e-6, suffix = " mln zł", accuracy = 0.1)(Wartość),
      label_number(suffix = " zł", accuracy = 1, big.mark = " ")(Wartość)
    ),
    
    # Formowanie opisu na słupku
    Etykieta = paste0(
      Wartość_tekst, 
      "   (", 
      label_number(suffix = " zł", accuracy = 1, big.mark = " ")(Cena_za_jedn), 
      " / ", Jednostka, ")"
    )
  )

myplot <- ggplot(narkotyki, aes(x = Wartość, y = reorder(Rodzaj, Wartość))) +
  geom_col(fill = paleta_sg["Narkotyki"], width = 0.7) +
  
  # Tekst z dwiema informacjami (całość + cena jednostkowa)
  geom_text(aes(label = Etykieta), hjust = -0.05, size = 4, fontface = "bold") +
  
  scale_x_continuous(
    labels = label_number(scale = 1e-6, suffix = " mln"),
    expand = expansion(mult = c(0, 0.6)) # Duży margines na długie teksty z cenami
  ) +
  
  labs(
    title = "Przechwycone narkotyki wg szacunkowej wartości",
    subtitle = "Zawiera wyliczoną cenę rynkową za jednostkę (za 1 kg lub 1 sztukę)",
    x = "Całkowita wartość (w milionach złotych)",
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank()
  )

mysaveplot(myplot, chart_dir, "3_wartosc_narkotykow_slupkowy")
print(myplot)
