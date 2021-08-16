library(dplyr)
library(readxl)
library(writexl)

df <- read_excel("2021-07-15g09.25.44.670_SPID582_ID1464.xlsx")

a <- 2
b <- 2

#liczba_iteracji <- 50

ilosc_rzedow <- df %>%
  count("Towar")

ilosc_rzedow <- ilosc_rzedow$n[[1]]

do_macierzy <- vector()

for (i in 1:50){
  
  rzad <- df %>%
    slice(a:a)
  
  final <- rzad %>%
    select(12:71)
  
  final[] <- lapply(final, gsub, pattern = "NULL", replacement = "0", fixed = TRUE)
  koncowy_wektor <- as.numeric(as.vector(t(final)))
  
  do_wyrzucenia <- unname(boxplot.stats(koncowy_wektor)$out)
  koncowy_wektor <- koncowy_wektor[! koncowy_wektor %in% do_wyrzucenia] #problem z operatorem != w pêtli (tutaj dzia³a dobrze)
  
  a <- a + 1 
  
  rzad_indeks <- df %>%
    slice(b:b)
  
  kolumna_indeks <- rzad_indeks %>%
    select(1:1)
  
  b <- b + 1
  
  do_macierzy <- append(do_macierzy, c(kolumna_indeks[[1,1]],mean(koncowy_wektor)))
  
}

gotowa_macierz <- matrix(do_macierzy, nrow = 50, ncol = 2, byrow = TRUE)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "srednie_automatyczne.xlsx")

