#instalacja pakietów
install.packages("rvest")
install.packages("dplyr")
install.packages("ggplot2")
#Ładowanie pakietów
library(rvest)
library(dplyr)
library(ggplot2)

# Adres URL strony Wikipedii
url <- "https://pl.wikipedia.org/wiki/Opel_Astra"

#Pobieranie strony
html <- read_html(url)

#Wyodrębnienie wszystkich elementów <table wikitable>
wikitables <- html %>% html_nodes("table.wikitable") %>% html_table(fill = TRUE)

# Przetwarzanie danych
list_of_dataframes <- lapply(wikitables, as.data.frame)

# Przypisanie do my_df ostatniej tabeli "Sprzedaż w Polsce"
my_df <- tail(list_of_dataframes, 1)[[1]]

# Usunięcie odnośników do przypisów w kolumnie Rok
my_df$Rok <- gsub("\\[.*\\]", "", my_df$Rok)



#Wykres przedstawiający pozycję w czasie dla lat 2010 - 2020
ggplot(my_df[complete.cases(my_df) & my_df$Rok >= 2010 & my_df$Rok <= 2020, ], aes(x = Rok, y = as.numeric(Pozycja))) +
  geom_point() +
  geom_text(aes(label = Pozycja), vjust = -0.5, hjust = 1.5, size = 3) +  # Dodanie podpisów z numerami pozycji
  labs(title = "Pozycja Opla Astry w rankingu najlepiej sprzedających się samochodów",
       x = "Rok",
       y = "Pozycja w rankingu") +
  theme_minimal()


# Konwersja kolumny "Rok" na format liczbowy
my_df$Rok <- as.numeric(my_df$Rok)
#Filtracja danych dla ostatnich 13 lat 
my_df <- my_df[my_df$Rok >= (max(my_df$Rok) - 13), ]

# Konwersja kolumny "Liczba sprzedanych sztuk" na liczby
my_df$`Liczba sprzedanych sztuk` <- as.numeric(gsub("\\D", "", my_df$`Liczba sprzedanych sztuk`))

# Zamiana wartości "b.d." na NA
for (col in names(my_df)) {
  my_df[[col]][my_df[[col]] == "b.d."] <- NA
}

#Wykres słupkowy dla sprzedanych samochodów Opel Astra
ggplot(my_df, aes(x = Rok, y = `Liczba sprzedanych sztuk`, fill = `Liczba sprzedanych sztuk`)) +
  geom_col() +
  geom_text(aes(label = `Liczba sprzedanych sztuk`), vjust = -0.5, size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +  # Ustawienie gradientu kolorów
  labs(title = "Liczba sprzedanych Opli Astra w latach 2008 - 2021",
       x = "Rok",
       y = "Liczba sprzedanych sztuk")

print(my_df)
