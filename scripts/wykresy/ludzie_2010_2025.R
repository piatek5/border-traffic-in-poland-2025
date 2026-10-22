# Borys Piątek 331725


# Wczytanie bibliotek i funkcji ####

library(readxl)
library(scales)
library(tidyverse)
library(this.path)

source("scripts/wykresy/funkcje.R")


# Wczytanie danych ####

sums_df = readRDS("data_clean/Sumy_2010_2025.rds")

# Nazwa pliku
chart_dir <- basename(this.path()) %>% 
  str_remove("\\..*") %>% 
  str_trim()
chart_dir <- paste0("charts/", chart_dir)

# Paleta kolorów dla liniowych
palette  <- RColorBrewer::brewer.pal(n = 5, name = "Dark2")
line_palette <- c(
  "Ruch lądowy" = palette[1],
  "Mały ruch graniczny (MRG)"  = palette[2],
  "Ruch lotniczy"        = palette[3],
  "Polacy"               = palette[4],
  "Cudzoziemcy"          = palette[5]
)


# Wykresy ####

## -- Wykres liniowy ruchu lądowego i lotniczego ####

myplot <- dual_line(
  data = sums_df,
  x = Rok,
  y_cols = c("Ruch lądowy" = Ladowy, 
             "Ruch lotniczy" = Lotniczy),
  title = "Porównanie sumy przekroczeń granicy między ruchem lotniczym a lądowym (bez MRG)",
  subtitle = "W latach 2010-2025",
  palette = line_palette
) + 
  
  geom_vline(xintercept = 2023.5, colour = "#2c3e50", 
             linetype = "dashed", linewidth = 1)

mysaveplot(myplot, chart_dir, "1_ladowy_vs_lotniczy_liniowy")
print(myplot)


## -- Wykres liniowy ruchu lądowego i MRG ####

myplot <- dual_line(
  data = sums_df,
  x = Rok,
  y_cols = c("Ruch lądowy" = Ladowy, 
    "Mały ruch graniczny (MRG)" = MRG),
  title = "Porównanie sumy przekroczeń granicy dla MRG oraz pozostałego ruchu lądowego",
  subtitle = "W latach 2010-2025",
  palette = line_palette
) + 
  
  geom_vline(xintercept = 2016, 
   colour = line_palette["Mały ruch graniczny (MRG)"], 
   linetype = "dashed", linewidth = 1) +
  
  annotate("text", x = 2016, y = 27000000, 
   colour = line_palette["Mały ruch graniczny (MRG)"], 
   label = "Zamknięcie MRG dla obw. Król.", 
   vjust = 0, hjust = 1.02, 
   fontface = "bold")

mysaveplot(myplot, chart_dir, "2_ladowy_vs_mrg_liniowy")
print(myplot)


## Wykres liniowy ruchu Polaków i cudzoziemców ####

myplot <- dual_line(
  data = sums_df,
  x = Rok,
  y_cols = c(Polacy, Cudzoziemcy),
  title = "Porównanie sumy przekroczeń granicy między Polakami a cudzoziemcami (bez MRG)",
  subtitle = "W latach 2010-2025",
  palette = line_palette
)

mysaveplot(myplot, chart_dir, "3_polacy_vs_obcy_liniowy")
print(myplot)
