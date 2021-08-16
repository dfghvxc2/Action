df <- read_excel("2021-07-13g14.42.38.680_SPID673_ID1464.xlsx")

a <- 12

ilosc_rzedow <- df %>%
  count("Towar")

ilosc_rzedow <- ilosc_rzedow$n[[1]]

sink(file = 'srednie_manualne.txt')


for (i in 1:30){
  
  rzad <- df %>%
    slice(a:a)
  
  final <- rzad %>%
    select(12:71)
  
  final[] <- lapply(final, gsub, pattern = "NULL", replacement = "0", fixed = TRUE)
  koncowy_wektor <- as.numeric(as.vector(t(final)))
  
  kwantyl_25 <- unname(quantile(koncowy_wektor,0.25))

  
  kwantyl_75 <- unname(quantile(koncowy_wektor,0.75))
 

  wartosc_IQR <- IQR(koncowy_wektor) * 1.5 
  
  Q3 <- wartosc_IQR + kwantyl_75
  Q1 <- kwantyl_25 - wartosc_IQR 
  
  wektor_z_kwantylami <- append(koncowy_wektor, c(Q3,Q1))
  
  posortowany <- sort(wektor_z_kwantylami)
  
  pre_final <- posortowany[posortowany < Q3]
  Final_flattened_vector <- pre_final[pre_final > Q1]
  #Mia³o zamieniac NaN na 0 ale NaN nie ma w wektorze tylko w princie #Final_flattened_vector <- replace(Final_flattened_vector, Final_flattened_vector == 'NaN', 0)
  
  a <- a + 1 
  
  print(mean(Final_flattened_vector))
}
sink(file = NULL)


