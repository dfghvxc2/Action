df <- read_excel("2021-07-13g14.42.38.680_SPID673_ID1464.xlsx")

rzad <- df %>%
  slice(12:12)
  
final <- rzad %>%
  select(12:71)

final[] <- lapply(final, gsub, pattern = "NULL", replacement = "0", fixed = TRUE)

#przerobiony_wektor <- as.vector(t(final))
#koncowy_wektor <- as.numeric(przerobiony_wektor)

koncowy_wektor <- as.numeric(as.vector(t(final)))

#TUTAJ W£O¯YÆ WYP£ASZCZANIE WYKRESU
do_wyrzucenia <- unname(boxplot.stats(koncowy_wektor)$out)
koncowy_wektor <- koncowy_wektor[! koncowy_wektor %in% do_wyrzucenia] #problem z operatorem != w pêtli (tutaj dzia³a dobrze)
#print(koncowy_wektor)
mean(koncowy_wektor)

