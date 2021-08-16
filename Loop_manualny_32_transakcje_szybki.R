library(dplyr)
library(readxl)
library(writexl)
df_original <- read_excel("23.07.2021.xlsx")


df <- df_original[ -c(2:11, 132:138) ]
df_index <- df[-c(2:121)]


df <- df[-c(110:113,96:99,82:85,68:71,54:57,40:43,26:29,12,15)]# to sie zmienia wraz z data
df <- df[-c(1:27)] # to sie nie zmienia z data 

df[] <- lapply(df, gsub, pattern = "NULL", replacement = "NaN", fixed = TRUE)

ilosc_rzedow <- df_index %>%
  count("Towar")
ilosc_rzedow <- ilosc_rzedow$n[[1]]

do_macierzy <- vector()


d <- 1
e <- 1
counter_rzedow <- 1

for (i in 1:ilosc_rzedow){
  
  b <- 1
  c <- 2
  index <- df_index[[d,1]]
  
  wektor_sprzedazy <- vector()
  czysty_wektor <- vector()
  wektor_faktur <- vector()
  
  Pelny_wektor <- as.numeric(df[counter_rzedow,])
  
  wektor_sprzedazy <- Pelny_wektor[seq(1,length(Pelny_wektor),2)]
  wektor_sprzedazy <- wektor_sprzedazy[!is.na(wektor_sprzedazy)]
  wektor_faktur <- Pelny_wektor[seq(2,length(Pelny_wektor),2)]
  wektor_faktur <- wektor_faktur[!is.na(wektor_faktur)]
  
  if (length(wektor_sprzedazy) > 5) {
    
    kwantyl_25 <- unname(quantile(wektor_sprzedazy,0.25,na.rm = TRUE))
    
    
    kwantyl_75 <- unname(quantile(wektor_sprzedazy,0.75,na.rm = TRUE))
    
    
    wartosc_IQR <- IQR(wektor_sprzedazy) * 0.3255
    
    Q3 <- wartosc_IQR + kwantyl_75
    Q1 <- kwantyl_25 - wartosc_IQR 
    
    for (i in 1:(length(wektor_sprzedazy))){
      wektor_sprzedazy_pozycja <- wektor_sprzedazy[b]
      wektor_faktur_pozycja <- wektor_faktur[b]
      if (wektor_sprzedazy_pozycja < Q3 & wektor_sprzedazy_pozycja > Q1){
        czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
      } else if(wektor_faktur_pozycja / wektor_sprzedazy_pozycja * 100 > 20){
          czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
      }
      b <- b+1
    }
    do_macierzy <- append(do_macierzy, c(index, sum(czysty_wektor)/length(czysty_wektor)))
  }else {
    do_macierzy <- append(do_macierzy, c(index, '0'))
  }
  d <- d+1
  e <- e+1
  counter_rzedow <- counter_rzedow+1
}
gotowa_macierz <- matrix(do_macierzy, nrow = ilosc_rzedow, ncol = 2, byrow = TRUE)
colnames(gotowa_macierz) <- c("Indeks","Srednia")
gotowa_macierz <- ifelse(gotowa_macierz=="NaN",0,gotowa_macierz)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "Wyniki z 23.07.2021 z Loop manualny.xlsx")


# system.time(for (i in 1:100000){ b <- 240 + 15/30 * as.numeric(df[[1,5]])
#   + as.numeric(df[[1,5]]) - as.numeric(df[[1,5]])
#   })
# 
# 
# system.time(for (i in 1:100000){a <- as.numeric(df [[1,5]])
# b <- 240 + 15/30 * a
# + a - a
# })
# 
# a <- 1
# warunek1 <- c(a>25)
# 
# for (i in 1:10000){
#   warunek1 c(a>25)
#   if (warunek1){
#     cat('dog')
#   }
#   a+1
# }


