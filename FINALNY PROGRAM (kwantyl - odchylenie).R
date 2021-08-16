library(dplyr)
library(readxl)
library(writexl)
df_original <- read_excel("13.08.2021.xlsx")

df <- df_original[-c(1:11, 132:138)]# 132:140 bez obrobionego excela | 132:139 z usuniêciem spacji | 132:138 z usuniêciem spacji przesuniêciem produktów
df_original <- slice(df_original, -c(1))
df <- slice(df, -c(1))

x <- seq(Sys.Date()-60, Sys.Date()-1, by = 1)
y <- seq(Sys.Date()-60, Sys.Date()-1, by = 1)

licznik_dat <- 1
wektor <- vector()
for (i in 1:length(x)){
  wektor <- append(wektor,toString(x[licznik_dat]))
  wektor <- append(wektor,toString(x[licznik_dat]))
  licznik_dat <- licznik_dat+1
}

licznik_dat_df <- 1
for (i in 1:120){
  names(df)[licznik_dat_df] <- wektor[licznik_dat_df]
  licznik_dat_df <- licznik_dat_df+1
}

weekendy <- vector()
weekendy <- x[lubridate::wday(x) %in% c(1, 7)]

weekendy_czyste <- vector()
counter_weekendy <- 1
for(i in 1:length(weekendy)){
  element <- toString(weekendy[counter_weekendy])
  weekendy_czyste <- append(weekendy_czyste,element)
  counter_weekendy <- counter_weekendy+1
}

df <- df[ , !(names(df) %in% weekendy_czyste)]
ilosc_kolumn <- ncol(df)
ilosc_kolumn_delete <- ilosc_kolumn - 64
df <- df[-c(1:ilosc_kolumn_delete)]
df[] <- lapply(df, gsub, pattern = "NULL", replacement = "NaN", fixed = TRUE)

ilosc_rzedow <- nrow(df)

wektor_nazw <- pull(df_original, 1)
do_macierzy <- vector()


d <- 1
e <- 1
counter_rzedow <- 1

for (i in 1:ilosc_rzedow){
  
  b <- 1
  index <- wektor_nazw[d]
  
  wektor_sprzedazy <- vector()
  czysty_wektor <- vector()
  wektor_faktur <- vector()
  koncowy_wektor_faktur <- vector()
  koncowy_wektor_sprzedazy <- vector()
  
  Pelny_wektor <- as.numeric(df[counter_rzedow,])
  
  wektor_sprzedazy <- Pelny_wektor[seq(1,length(Pelny_wektor),2)]
  wektor_faktur <- Pelny_wektor[seq(2,length(Pelny_wektor),2)]
  
  for (i in 1:length(wektor_sprzedazy)){
    if(is.na(wektor_sprzedazy[i]) & is.na(wektor_faktur[i])){
    }else if (is.na(wektor_sprzedazy[i]) & is.numeric(wektor_faktur[i])){
      wektor_faktur[i] <- NaN
      koncowy_wektor_faktur <- append(koncowy_wektor_faktur, wektor_faktur[i])
      koncowy_wektor_sprzedazy <- append(koncowy_wektor_sprzedazy, wektor_sprzedazy[i])
    }else if (is.numeric(wektor_sprzedazy[i]) & is.na(wektor_faktur[i])){
      wektor_faktur[i] <- 0
      koncowy_wektor_faktur <- append(koncowy_wektor_faktur, wektor_faktur[i])
      koncowy_wektor_sprzedazy <- append(koncowy_wektor_sprzedazy, wektor_sprzedazy[i])
    }else if (is.numeric(wektor_sprzedazy[i]) & is.numeric(wektor_faktur[i])){
      koncowy_wektor_faktur <- append(koncowy_wektor_faktur, wektor_faktur[i])
      koncowy_wektor_sprzedazy <- append(koncowy_wektor_sprzedazy, wektor_sprzedazy[i])
    }
  }
  
  koncowy_wektor_sprzedazy <- koncowy_wektor_sprzedazy[!is.na(koncowy_wektor_sprzedazy)]
  koncowy_wektor_faktur <- koncowy_wektor_faktur[!is.na(koncowy_wektor_faktur)]
  
  kwantyl_25 <- unname(quantile(koncowy_wektor_sprzedazy,0.25,na.rm = TRUE))
  kwantyl_75 <- unname(quantile(koncowy_wektor_sprzedazy,0.75,na.rm = TRUE))
  
  if(is.na(kwantyl_25)){
    do_macierzy <- append(do_macierzy, c(index, '0'))
    d <- d+1
    counter_rzedow <- counter_rzedow+1
    b <- b+1
  }else{
    
    wartosc_IQR <- IQR(koncowy_wektor_sprzedazy) * 0.2412
    Q3 <- wartosc_IQR + kwantyl_75
    Q1 <- kwantyl_25 - wartosc_IQR
    
    if(kwantyl_25 == 0 & kwantyl_75 == 0){
      if(length(koncowy_wektor_sprzedazy)<5){
        do_macierzy <- append(do_macierzy, c(index, '0'))
        d <- d+1
        e <- e+1
        counter_rzedow <- counter_rzedow+1
      }else{
        srednia <- mean(koncowy_wektor_sprzedazy)
        odchylenie <- sd(koncowy_wektor_sprzedazy)
        
        granica_gorna <- srednia + odchylenie
        granica_dolna <- srednia - odchylenie
        
        for (i in 1:(length(koncowy_wektor_sprzedazy))){
          wektor_sprzedazy_pozycja <- koncowy_wektor_sprzedazy[b]
          wektor_faktur_pozycja <- koncowy_wektor_faktur[b]
          if (wektor_sprzedazy_pozycja <= granica_gorna & wektor_sprzedazy_pozycja >= granica_dolna){
            czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
          }else if(wektor_sprzedazy_pozycja == 0){
          }else if(wektor_faktur_pozycja / wektor_sprzedazy_pozycja * 100 > 20){
            czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
          }
          b <- b+1
        }
        if (length(koncowy_wektor_sprzedazy)<5){
          do_macierzy <- append(do_macierzy, c(index, '0'))
          d <- d+1
          e <- e+1
          counter_rzedow <- counter_rzedow+1
        }else{
          do_macierzy <- append(do_macierzy, c(index, sum(czysty_wektor)/(length(czysty_wektor))))
          d <- d+1
          e <- e+1
          counter_rzedow <- counter_rzedow+1
        }
      }
    }else{
      for (i in 1:(length(koncowy_wektor_sprzedazy))){
        wektor_sprzedazy_pozycja <- koncowy_wektor_sprzedazy[b]
        wektor_faktur_pozycja <- koncowy_wektor_faktur[b]
        if (wektor_sprzedazy_pozycja < Q3 & wektor_sprzedazy_pozycja > Q1){
          czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
        }else if(wektor_sprzedazy_pozycja == 0){
        }else if(wektor_faktur_pozycja / wektor_sprzedazy_pozycja * 100 > 20){
          czysty_wektor <- append(czysty_wektor,c(wektor_sprzedazy_pozycja))
        }
        b <- b+1
      }
      if(length(koncowy_wektor_sprzedazy)<5){
        do_macierzy <- append(do_macierzy, c(index, '0'))
        d <- d+1
        e <- e+1
        counter_rzedow <- counter_rzedow+1
      }else{
        do_macierzy <- append(do_macierzy, c(index, sum(czysty_wektor)/length(czysty_wektor)))
        d <- d+1
        e <- e+1
        counter_rzedow <- counter_rzedow+1
      }
    }
  }
}

gotowa_macierz <- matrix(do_macierzy, nrow = ilosc_rzedow, ncol = 2, byrow = TRUE)
colnames(gotowa_macierz) <- c("Indeks","Srednia")
gotowa_macierz <- ifelse(gotowa_macierz < 0,0,gotowa_macierz)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "Wynik.xlsx")