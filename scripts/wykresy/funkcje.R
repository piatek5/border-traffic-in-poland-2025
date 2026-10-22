# Borys Piątek 331725


# Wczytanie bibliotek ####

library(sf)
library(tidyverse)
library(shadowtext)


# Funkcje ####

# Funkcja dla zapisu wykresu
mysaveplot <- function(myplot, chart_dir = "charts", filename = "chart") {
  if ( !exists("save_all") || (exists("save_all") && save_all) ) {
    
    if(!dir.exists(file.path(chart_dir))) {
      dir.create(file.path(chart_dir), recursive = TRUE)
    }
    
    filename_no_ext = paste0(chart_dir, "/", filename)
    
    extension <- ".svg"
    
    final_filename = paste0(filename_no_ext, extension)
    
    if (filename == "chart"){
      counter <- 1
      while (file.exists(final_filename)) {
        final_filename = paste0(filename_no_ext, "(", counter, ")", extension)
        counter = counter + 1
      }
    }
    
    ggsave(final_filename, plot = myplot, width = 10, height = 6)
  }
}


# Funkcja dla rysowania wykresów liniowych
dual_line <- function(data, x, y_cols, title, subtitle, palette) {
  long_data <- data %>% 
    select({{ x }}, {{ y_cols }}) %>% 
    pivot_longer(
      cols = -{{ x }}
    )
  
  y_max <- long_data %>% 
    pull(value) %>% 
    max(na.rm = TRUE)
  
  x_vals <- data %>% 
    pull({{ x }})
  
  ggplot(long_data, aes(x = {{ x }}, y = value, color = name)) +
    geom_line(linewidth = 1.5) +
    
    scale_color_manual(values = palette) +
    
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = " mln"),
      limits = c(0, y_max * 1.1)
    ) +
    
    scale_x_continuous(breaks = min(x_vals, na.rm = TRUE):max(x_vals, na.rm = TRUE)) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Rok",
      y = "Liczba osób",
      color = ""
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16)
    )
}


# Funkcja dla poziomych wykresów kolumnowych z etykietami wartości
hcol_labels <- function(data, x, y, fill, title, subtitle,
                        x_label = NULL, y_label = NULL) 
{
  data_sorted <- data %>% 
    arrange({{ y }}) %>% 
    mutate({{ x }} := fct_inorder({{ x }}))
  
  max_y <- data_sorted %>% 
    pull({{y}}) %>% 
    max(na.rm = TRUE)
  
  ggplot(data_sorted, aes(x = {{ x }}, y = {{ y }})) +
    
    geom_col(fill = fill) +
    
    geom_text(
      aes(
        label = scales::label_number(
          big.mark = " ", decimal.mark = ",")(suma_przejsc),
        hjust = -0.1),
      color = "#2c3e50"
    ) +
    
    scale_y_continuous(
      limits = c(0, max_y * 1.1),
      labels = scales::label_number(big.mark = " ", decimal.mark = ",")
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 11),
      panel.grid.major.y = element_blank()
    ) +
    
    coord_flip() +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
}


# Funkcja dla rysownania jednego poziomego słupa w podziale
hcol_single_stacked <- function(data, y, fill_cols, title, subtitle, palette) {
  y_max <- data %>% 
    pull({{ y }}) %>% 
    sum(na.rm = TRUE) 
  
  ggplot(data, aes(x = "", y = {{ y }}, fill = {{ fill_cols }})) +
    geom_col(width = 0.35, color = "white", linewidth = 1) +
    
    geom_shadowtext(
      aes(label = scales::number({{ y }}, big.mark = " ", decimal.mark = ",")),
      position = position_stack(vjust = 0.5),
      
      color = "white",
      bg.color = "black",
      bg.r = 0.1,
      
      fontface = "bold",
      size = 4.5
    ) +
    
    scale_fill_manual(values = palette) +
    
    scale_y_continuous(
      limits = c(0, y_max),
      labels = scales::label_number(big.mark = " ", decimal.mark = ",")) +
    
    guides(fill = guide_legend(reverse = TRUE)) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.major.y = element_blank(),
      legend.position = "top",
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Suma przejść",
      fill = ""
    ) +
    
    coord_flip()
}


# Funkcja dla rysowania wykresu bąbelkowego na mapie
bubble <- function(map, data, data_col, scale_name, 
  title, subtitle, color, fill) 
{
  ggplot() +
    # Warstwa mapy
    geom_sf(data = map, fill = "gray95", color = "gray60") +
    
    # Warstwa okręgów
    geom_sf(
      data = data, 
      aes(size = {{ data_col }}),
      color = color, 
      fill = fill, 
      alpha = 0.6, 
      shape = 21
    ) +
    
    scale_size_continuous(
      range = c(4, 20), # Ustawienia skalowania dla kółek
      name = scale_name,
      labels = scales::comma_format(big.mark = " ")
    ) +
    
    # Kosmetyka wykresu
    theme_void() +
    labs(
      title = title,
      subtitle = subtitle
    )
}


# Prosta funkcja rysująca na mapie
map_world <- function(data_map, y, title, subtitle, 
  scale_title = "Liczba podróży (skala logarytmiczna)", ...)
  {
  ggplot(data_map) + 
    geom_sf(aes(fill = {{y}}), ...) +
    
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width  = unit(4, "lines"),
      legend.key.height = unit(1, "lines"),
      legend.title.position = "top"
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      fill = scale_title
    )
}
