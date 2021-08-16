library(dplyr)
library(readxl)
library(writexl)
df_original <- read_excel("2021-07-15g09.25.44.670_SPID582_ID1464.xlsx")


df <- df_original[ -c(2:11, 72:80) ]
df <- slice(df, -c(1))
df_index <- df[-c(2:61)]
df <- df[-c(1)]

df[] <- lapply(df, gsub, pattern = "NULL", replacement = "0", fixed = TRUE)

ilosc_rzedow <- df %>%
  count("Towar")
ilosc_rzedow <- ilosc_rzedow$n[[1]]

do_macierzy <- vector()

b <- 2
c <- 1
d <- 1

for (i in 1:ilosc_rzedow){
  
  koncowy_wektor <- as.numeric(as.vector(t(slice(df,c(c)))))
  
  kwantyl_25 <- unname(quantile(koncowy_wektor,0.25))
  
  
  kwantyl_75 <- unname(quantile(koncowy_wektor,0.75))
  
  
  wartosc_IQR <- IQR(koncowy_wektor) * 1.5 
  
  Q3 <- wartosc_IQR + kwantyl_75
  Q1 <- kwantyl_25 - wartosc_IQR 
  
  wektor_z_kwantylami <- append(koncowy_wektor, c(Q3,Q1))
  
  posortowany <- sort(wektor_z_kwantylami)
  
  pre_final <- posortowany[posortowany < Q3]
  Final_flattened_vector <- pre_final[pre_final > Q1]
  
  do_macierzy <- append(do_macierzy, c(df_index[[d,1]], mean(Final_flattened_vector)))
  
  b <- b + 1
  c <- c + 1
  d <- d + 1
}
gotowa_macierz <- matrix(do_macierzy, nrow = ilosc_rzedow, ncol = 2, byrow = TRUE)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "srednie_manualne_szybkie.xlsx")
