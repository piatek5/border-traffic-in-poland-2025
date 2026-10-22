# Borys Piątek 331725


# Wczytywanie bibliotek ####

library(sf)
library(magrittr)
library(tidyverse)
library(tidygeocoder)


# Funkcje ####

read_csv_12 <- function(filename) {
  first_line <- readLines(filename, n = 1)
  
  if (grepl(";", first_line)) {
    sheet <- read_csv2(filename, show_col_types = FALSE)
  } else {
    sheet <- read_csv(filename, show_col_types = FALSE)
  }
  
  return(sheet)
}

format_headers <- function(df) {
  df %>% 
    rename(Rodzaj_przejscia = matches("^Rodzaj.*")) %>% 
    rename(Paszportowa = matches("^Paszportow.*"))
}


# Wczytanie i obróbka danych ####

filename <- "data_raw/Baza_Ruchu_Granicznego_2025.csv"
ruch_2025 <- read_csv_12(filename) %>% 
  format_headers()

filename <- "data_raw/Baza_Ruchu_Granicznego_2025_cudzoziemcy.csv"
ruch_2025_cudz <- read_csv_12(filename) %>% 
  format_headers()

filename <- "data_raw/przemyt.csv"
przemyt_2025 <- read_csv_12(filename)

## -- Ramka danych z sumami za lata 2010-2025

years <- 2010:2025

# Wynikowa ramka danych
sums_df <- data.frame(
  Rok = years # Jedna z kolumn
)

for (i in seq_along(years)) {
  curr_year <- years[i]
  
  filename <- paste0("data_raw/Baza_ruchu_granicznego_", curr_year, ".csv")
  
  if (file.exists(filename)) {
    sheet <- read_csv_12(filename)
    
    sheet %<>%
      format_headers()
    
    mrg <- sum(sheet$MRG, na.rm = TRUE)
    
    lot <- sheet %>% 
      filter(Rodzaj_przejscia == "lotnicze") %>% 
      summarise(suma_razem = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
      pull(suma_razem)
    
    lad <- sheet %>% 
      filter(Rodzaj_przejscia %in% c("drogowe", "kolejowe")) %>% 
      summarise(suma_razem = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
      pull(suma_razem)
    
    pol <- sheet %>% 
      filter(Kto == "RP") %>% 
      summarise(suma_razem = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
      pull(suma_razem)
    
    cudz <- sheet %>% 
      filter(Kto == "C") %>% 
      summarise(suma_razem = sum(Paszportowa, Pozasystemowa, na.rm = TRUE)) %>% 
      pull(suma_razem)
    
    # Pozostałe kolumny
    sums_df$MRG[i] <- mrg
    sums_df$Lotniczy[i] <- lot
    sums_df$Ladowy[i] <- lad
    sums_df$Polacy[i] <- pol
    sums_df$Cudzoziemcy[i] <- cudz
    
    print(years[i])
    
  } else {
    warning(paste("Brak pliku:", filename))
  }
}

saveRDS(sums_df, "data_clean/Sumy_2010_2025.rds")
saveRDS(ruch_2025, "data_clean/Baza_Ruchu_Granicznego_2025.rds")
saveRDS(ruch_2025_cudz, "data_clean/Baza_Ruchu_Granicznego_2025_cudzoziemcy.rds")
saveRDS(przemyt_2025, "data_clean/przemyt_2025.rds")


# Geokodowanie z cache ####

# Wzięcie unikalnych placówek Straży Granicznej
adresy_2025 <- ruch_2025 %>% select(`Placówka SG`)

unikalne_adresy <- adresy_2025 %>%
  distinct(`Placówka SG`) %>%
  drop_na(`Placówka SG`) %>% 
  mutate(adres_do_wyszukania = paste(`Placówka SG`, "Polska", sep = ", "))

# Definicja pliku cache
plik_cache <- "data_clean/geocache.rds"

# Wczytanie cache albo stworzenie pustego
if (file.exists(plik_cache)) {
  cache <- read_rds(plik_cache)
} else {
  cache <- tibble(
    adres_do_wyszukania = character(), 
    lat = numeric(), 
    lon = numeric()
  )
}

# Szukanie stacji, których nie ma jeszcze w cache
nowe_adresy <- anti_join(unikalne_adresy, cache, by = "adres_do_wyszukania")

# Odpytanie OSM tylko dla nowych stacji
if (nrow(nowe_adresy) > 0) {
  nowe_zgeokodowane <- nowe_adresy %>%
    geocode(address = adres_do_wyszukania, method = 'osm', lat = lat, long = lon) %>% 
    select(adres_do_wyszukania, lat, lon)
  
  # Aktualizacja i zapis cache
  cache <- bind_rows(cache, nowe_zgeokodowane)
  saveRDS(cache, plik_cache)
  message("Cache został zaktualizowany.")
} else {
  message("Wszystkie stacje są już w cache.")
}


## -- Tworzenie obiektów przestrzennych SF i zapis ####

# Placówki Straży Granicznej SF
placowki_sf <- ruch_2025 %>%
  mutate(adres_do_wyszukania = paste(`Placówka SG`, "Polska", sep = ", ")) %>%
  left_join(cache, by = "adres_do_wyszukania") %>% # Dołączenie współrzędnych
  drop_na(lat, lon) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Zapis ostatecznego pliku z geometrią
saveRDS(placowki_sf, "data_clean/Placowki_sf.rds")
