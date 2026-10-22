# Borys Piątek 331725


# Wczytywanie bibliotek i funkcji ####

library(sf)
library(readxl)
library(magrittr)
library(tidyverse)
library(this.path)
library(rnaturalearth)

source("scripts/wykresy/funkcje.R")


# Wczytanie danych ####

ruch_2025 <- read_rds("data_clean/Baza_Ruchu_Granicznego_2025.rds")
placowki_sf <- read_rds("data_clean/Placowki_sf.rds")
ruch_2025_cudz <- read_rds("data_clean/Baza_Ruchu_Granicznego_2025_cudzoziemcy.rds")

palette  <- RColorBrewer::brewer.pal(n = 5, name = "Dark2")
single_col_palette <- c(
  "Polacy" = palette[4],
  "Cudzoziemcy"  = palette[5]
)

# Mapa Polski i świata
poland_map <- ne_countries(scale = "large", country = "Poland", returnclass = "sf")
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Nazwa pliku
chart_dir <- basename(this.path()) %>% 
  str_remove("\\..*") %>% 
  str_trim()
chart_dir <- paste0("charts/", chart_dir)


# Wykresy ####

## -- Poziomy wykres kolumnowy dla sumy ruchu dla rodzajów przejść ####

transport_grouped <- ruch_2025 %>%
  group_by(Rodzaj_przejscia) %>%
  summarise(suma_przejsc = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>%
  mutate(procent = suma_przejsc / sum(suma_przejsc))

myplot <- hcol_labels(
  data = transport_grouped,
  x = Rodzaj_przejscia,
  y = suma_przejsc,
  title = "Ruch graniczny odnotowany przez kontrole Straży Granicznej w 2025 roku",
  subtitle = "Według różnych rodzajów przejść granicznych",
  fill = "dodgerblue",
  x_label = "Rodzaj przejścia",
  y_label = "Liczba przekroczeń granicy" 
)

mysaveplot(myplot, chart_dir, "1_rodzaje_przejsc_slupkowy")
print(myplot)


## -- Poziomy wykres kolumnowy dla najczęstszych przekroczeń granicy wschodniej ####

poles_2025_air <- ruch_2025 %>% 
  filter(Kto == "RP",
         Rodzaj_przejscia == "lotnicze") %>% 
  summarize(suma_przejsc = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
  pull(suma_przejsc)

top_10_air <- ruch_2025_cudz %>%
  filter(Rodzaj_przejscia == "lotnicze") %>% 
  group_by(`Obywatelstwo (nazwa)`) %>%
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE)) %>%
  slice_max(order_by = suma_przejsc, n = 10, with_ties = FALSE)

myplot <- hcol_labels(
  data = top_10_air,
  x = `Obywatelstwo (nazwa)`,
  y = suma_przejsc,
  title = paste0("Czołówka 10 państw pochodzenia obywateli o największej sumie\n",
    "przekroczeń graniczy lotniczej"),
  subtitle = "Stan na rok 2025 dla wszystkich przejść lotniczych",
  fill = "cadetblue3",
  x_label = "Obywatelstwo",
  y_label = "Suma przejść"
)

mysaveplot(myplot, chart_dir, "2_top_10_lotnicze_slupkowy")
print(myplot)


## -- Poziomy wykres kolumnowy dla najczęstszych przekroczeń granicy lotniczej ####

poles_2025_land_east <- ruch_2025 %>% 
  filter(Kto == "RP",
         Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
         `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                             "Podlaski", "Bieszczadzki")
  ) %>%  
  summarize(suma_przejsc = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
  pull(suma_przejsc)

top_10_land_east <- ruch_2025_cudz %>%
  filter(
    Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
    `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                        "Podlaski", "Bieszczadzki")
  ) %>% 
  group_by(`Obywatelstwo (nazwa)`) %>%
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE)) %>%
  slice_max(order_by = suma_przejsc, n = 10, with_ties = FALSE)

myplot <- hcol_labels(
  data = top_10_land_east,
  x = `Obywatelstwo (nazwa)`,
  y = suma_przejsc,
  title = paste0("Czołówka 10 państw pochodzenia obywateli o największej liczbie\n",
                 "przekroczeń lądowej wschodniej granicy Polski"),
  subtitle = "Stan na rok 2025 dla przejść drogowych i kolejowych z Rosją, Białorusią i Ukrainą",
  fill = "aquamarine4",
  x_label = "Obywatelstwo",
  y_label = "Suma przejść"
)

mysaveplot(myplot, chart_dir, "2_top_10_ladoweo_wchod_slupkowy")
print(myplot)


## -- Poziomy wykres kolumnowy dla najrzadszych przekroczeń granicy ####

bottom_10 <- ruch_2025_cudz %>%
  group_by(`Obywatelstwo (nazwa)`) %>%
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE)) %>%
  slice_min(order_by = suma_przejsc, n = 10)

myplot <- hcol_labels(
  data = bottom_10,
  x = `Obywatelstwo (nazwa)`,
  y = suma_przejsc,
  title = paste0("10 państw pochodzenia obywateli o najmniejszej liczbie\n",
                 "przekroczeń granicy Polski"),
  subtitle = "Stan na rok 2025 dla wszytkich przejść odnotowanych przez Straż Graniczną",
  fill = "firebrick",
  x_label = "Obywatelstwo",
  y_label = "Suma przejść"
)

mysaveplot(myplot, chart_dir, "3_bottom_10_calosc")
print(myplot)


## -- Słupek udziału dla Polaków i cudzoziemców dla przejść lotniczych ####

pol_foreign_air <- ruch_2025 %>% 
  filter(Rodzaj_przejscia == "lotnicze") %>% 
  group_by(Kto) %>% 
  summarise(suma_przejsc = sum(c(Paszportowa, Pozasystemowa), na.rm = TRUE)) %>% 
  mutate(Kto = if_else(Kto == "C", "Cudzoziemcy", "Polacy"))

myplot <- hcol_single_stacked(
  data = pol_foreign_air,
  y = suma_przejsc,
  fill_cols = Kto,
  palette = single_col_palette,
  title = "Porównanie sumy przejść przez granicę lotniczą przez obywateli Polski oraz cudzoziemców",
  subtitle = "Stan na rok 2025"
)

mysaveplot(myplot, chart_dir, "4_polacy_vs_obcy_lotnicza")
print(myplot)

## -- Słupek udziału dla Polaków i cudzoziemców dla przejść wschodnich lądowych ####

pol_foreign_land_east <- ruch_2025 %>% 
  filter(Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
         `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                             "Podlaski", "Bieszczadzki")) %>% 
  group_by(Kto) %>% 
  summarise(suma_przejsc = sum(c(Paszportowa, Pozasystemowa), na.rm = TRUE)) %>% 
  mutate(Kto = if_else(Kto == "C", "Cudzoziemcy", "Polacy"))

myplot <- hcol_single_stacked(
  data = pol_foreign_land_east,
  y = suma_przejsc,
  fill_cols = Kto,
  palette = single_col_palette,
  title = "Porównanie sumy przejść przez wchodnią granicę lądową przez obywateli Polski oraz cudzoziemców",
  subtitle = "Stan na rok 2025"
)

mysaveplot(myplot, chart_dir, "5_polacy_vs_obcy_ladowa_wschodnia")
print(myplot)


## -- Bąbelkowy na mapie dla przejść lotniczych ####

all_air <- placowki_sf %>% 
  filter(Rodzaj_przejscia == 'lotnicze') %>% 
  group_by(`Placówka SG`) %>% 
  summarise(suma_przejsc = sum(c(Paszportowa, Pozasystemowa), na.rm = TRUE)) %>% 
  filter(suma_przejsc > 0)

myplot <- bubble(
  map = poland_map,
  data = all_air,
  data_col = suma_przejsc,
  scale_name = "Suma podróży przez granicę",
  color = "#242861",
  fill = "#7570b3",
  title = "Suma przejść przez granicę lotniczną",
  subtitle = "Stan na 2025 rok z pominięciem przejść o zerowej aktywności"
)

mysaveplot(myplot, chart_dir, "6_lotnicza_babelki_mapa")
print(myplot)


## -- Bąbelkowy na mapie dla przejść wschodnich lądowych ####

all_land_east <- placowki_sf %>% 
  filter(Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
         `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                             "Podlaski", "Bieszczadzki")) %>% 
  group_by(`Placówka SG`) %>% 
  summarise(suma_przejsc = sum(c(Paszportowa, Pozasystemowa), na.rm = TRUE)) %>% 
  filter(suma_przejsc > 0)

myplot <- bubble(
  map = poland_map,
  data = all_land_east,
  data_col = suma_przejsc,
  scale_name = "Suma podróży przez granicę",
  color = "#004d2d",
  fill = "#1b9e77",
  title = "Suma przejść przez wchodnią granicę lądową",
  subtitle = "Stan na 2025 rok z pominięciem przejść o zerowej aktywności"
)

mysaveplot(myplot, chart_dir, "7_ladowa_wschod_babelki_mapa")
print(myplot)


## -- Mapa świata dla przejść lotniczych ####

all_air <- ruch_2025_cudz %>% 
  filter(Rodzaj_przejscia == "lotnicze") %>% 
  group_by(`Obywatelstwo (kod)`) %>% 
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE))

world_all_air <- world_map %>% 
  left_join(all_air, join_by(iso_a2_eh == `Obywatelstwo (kod)`))

myplot <- map_world(
  data_map = world_all_air,
  y = suma_przejsc,
  title = "Suma przejść przez granicę lotniczą dla państw świata",
  subtitle = "Dane dla cudziemców w roku 2025",
  color = "gray40"
) +
  
  scale_fill_viridis_c(
    transform = "log10",
    labels = scales::comma_format(big.mark = " "),
    na.value = "gray90",
  )

mysaveplot(myplot, chart_dir, "8_lotnicza_swiat_mapa")
print(myplot)


## -- Mapa świata dla przejść lotniczych TĘCZA ####

all_air <- ruch_2025_cudz %>%
  filter(Rodzaj_przejscia == "lotnicze") %>%
  group_by(`Obywatelstwo (kod)`) %>%
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE))

world_all_air <- world_map %>%
  left_join(all_air, join_by(iso_a2_eh == `Obywatelstwo (kod)`))

myplot <- map_world(
  data_map = world_all_air,
  y = suma_przejsc,
  title = "Suma przejść przez granicę lotniczą dla państw świata",
  subtitle = "Dane dla cudziemców w roku 2025",
  color = "gray40"
) +
  
  scale_fill_gradientn(
    colours = rainbow(20),
    transform = "log10",
    labels = scales::comma_format(big.mark = " "),
    na.value = "white",
  )

mysaveplot(myplot, chart_dir, "8a_lotnicza_swiat_mapa_zla")
print(myplot)


## -- Mapa świata dla wschodnich przejść lądowych ####

all_land_east <- ruch_2025_cudz %>% 
  filter(Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
         `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                             "Podlaski", "Bieszczadzki")) %>% 
  group_by(`Obywatelstwo (kod)`) %>% 
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE))

world_all_land_east <- world_map %>% 
  left_join(all_land_east, join_by(iso_a2_eh == `Obywatelstwo (kod)`))

myplot <- map_world(
  data_map = world_all_land_east,
  y = suma_przejsc,
  title = "Suma przejść przez wschodnią granicę lądową dla państw świata",
  subtitle = "Dane dla cudziemców w roku 2025",
  color = "gray40"
) +
  
  scale_fill_viridis_c(
    transform = "log10",
    labels = scales::comma_format(big.mark = " "),
    na.value = "gray90",
  )

mysaveplot(myplot, chart_dir, "9_ladoa_wschod_swiat_mapa")
print(myplot)


## -- Mapa świata dla wschodnich przejść lądowych SKALA LINIOWA####

all_land_east <- ruch_2025_cudz %>% 
  filter(Rodzaj_przejscia %in% c("drogowe", "kolejowe"),
         `Oddział SG` %in% c("Warmińsko-Mazurski", "Nadbużański", 
                             "Podlaski", "Bieszczadzki")) %>% 
  group_by(`Obywatelstwo (kod)`) %>% 
  summarise(suma_przejsc = sum(Razem, na.rm = TRUE))

world_all_land_east <- world_map %>% 
  left_join(all_land_east, join_by(iso_a2_eh == `Obywatelstwo (kod)`))

myplot <- map_world(
  data_map = world_all_land_east,
  y = suma_przejsc,
  title = "Suma przejść przez wschodnią granicę lądową dla państw świata",
  subtitle = "Dane dla cudziemców w roku 2025",
  scale_title = "Liczba podróży (skala liniowa)",
  color = "gray40"
) +
  
  scale_fill_viridis_c(
    transform = "identity",
    labels = scales::comma_format(big.mark = " "),
    na.value = "white",
  )

mysaveplot(myplot, chart_dir, "9a_ladoa_wschod_swiat_mapa_zla")
print(myplot)
