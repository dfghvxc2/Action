df <- read_excel("2021-07-13g14.42.38.680_SPID673_ID1464.xlsx")

a <- 12

ilosc_rzedow <- df %>%
  count("Towar")

ilosc_rzedow <- ilosc_rzedow$n[[1]]

sink(file = 'srednie_automatyczne.txt')

for (i in 1:30){

  rzad <- df %>%
    slice(a:a)
  
  final <- rzad %>%
    select(12:71)
  
  final[] <- lapply(final, gsub, pattern = "NULL", replacement = "0", fixed = TRUE)
  koncowy_wektor <- as.numeric(as.vector(t(final)))
  
  do_wyrzucenia <- unname(boxplot.stats(koncowy_wektor)$out)
  koncowy_wektor <- koncowy_wektor[! koncowy_wektor %in% do_wyrzucenia] #problem z operatorem != w pêtli (tutaj dzia³a dobrze)
  
  a <- a + 1 
  
  print(mean(koncowy_wektor))
  }
sink(file = NULL)

