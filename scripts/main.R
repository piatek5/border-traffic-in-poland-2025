# Borys Piątek 331725


# Konfiguracja ####

# Czy bezwzględnie uruchamiać wczytanie i obróbkę danych
# (jeśli są pliki w data_clean - pomija)
zawsze_uruchamiaj_wczytanie <- FALSE

# Flaga sterująca zapisem wykresów
save_all <- TRUE


# Przygotowanie środowiska ####

# Czyszczenie pamięci z pominięciem flag konfiguracyjnych
rm(list = setdiff(ls(), c("zawsze_uruchamiaj_wczytanie", "save_all")))

# Ładowanie funkcji
source("scripts/wykresy/funkcje.R", encoding = "UTF-8")


# Warunkowe wczytanie i obróbka danych ####
folder_clean <- "data_clean"

# Sprawdzenie czy folder istnieje oraz czy zawiera jakiekolwiek pliki
czy_sa_pliki <- dir.exists(folder_clean) && length(list.files(folder_clean)) > 0

if (zawsze_uruchamiaj_wczytanie || !czy_sa_pliki) {
  message("Uruchamiam pełne wczytywanie i obróbkę danych...")
  source("scripts/wczytanie_obrobka.R", encoding = "UTF-8", echo = FALSE)
} else {
  message(paste0("Pominięto obróbkę danych. Znaleziono przetworzone ",
                 "pliki w katalogu '", folder_clean, "'."))
}


# Rysowanie wykresów ####

message("Generowanie wykresów: Ludzie 2010-2025...")
source("scripts/wykresy/ludzie_2010_2025.R", encoding = "UTF-8", echo = FALSE)

message("Generowanie wykresów: Ruch Graniczny 2025...")
source("scripts/wykresy/ruch_caly_2025.R", encoding = "UTF-8", echo = FALSE)

message("Generowanie wykresów: Przemyt...")
source("scripts/wykresy/przemyt.R", encoding = "UTF-8", echo = FALSE)


# Na koniec ####

save_all <- TRUE
