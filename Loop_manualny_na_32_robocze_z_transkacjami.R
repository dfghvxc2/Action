library(dplyr)
library(readxl)
library(writexl)
df_original <- read_excel("zestawienie do testów1.xlsx")


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


for (i in 1:ilosc_rzedow){
  a <- 1
  b <- 1
  c <- 2
  index <- df_index[[d,1]]
  
  koncowy_wektor <- vector()
  czysty_wektor <- vector()
  wektor_faktur <- vector()
  
  for (i in 1:32){ #Tutaj mozna tez wrzucic wszystko i wyrzucic nie numerical
    koncowy_wektor <- append(koncowy_wektor, as.numeric(df[[e,a]]))
    a <- a + 2
  }
  for (i in 1:32){
    wektor_faktur <- append(wektor_faktur, as.numeric(df[[e,c]]))
    c <- c + 2
  }
  
  koncowy_wektor <- koncowy_wektor[!is.na(koncowy_wektor)]
  wektor_faktur <- wektor_faktur[!is.na(wektor_faktur)]
  
  if (length(koncowy_wektor) > 5) {
    
    kwantyl_25 <- unname(quantile(koncowy_wektor,0.25,na.rm = TRUE))
    
    
    kwantyl_75 <- unname(quantile(koncowy_wektor,0.75,na.rm = TRUE))
    
    
    wartosc_IQR <- IQR(koncowy_wektor) * 0.3255
    
    Q3 <- wartosc_IQR + kwantyl_75
    Q1 <- kwantyl_25 - wartosc_IQR 

    for (i in 1:(length(koncowy_wektor))){
      koncowy_wektor_pozycja <- koncowy_wektor[b]
      wektor_faktur_pozycja <- wektor_faktur[b]
      if (koncowy_wektor_pozycja < Q3 & koncowy_wektor_pozycja > Q1){
        czysty_wektor <- append(czysty_wektor,c(koncowy_wektor_pozycja))
      } else{
        if(wektor_faktur_pozycja / koncowy_wektor_pozycja * 100 > 20){
          czysty_wektor <- append(czysty_wektor,c(koncowy_wektor_pozycja))}
        }
      b <- b+1
    }
    do_macierzy <- append(do_macierzy, c(index, sum(czysty_wektor)/length(czysty_wektor)))
  }else {
    do_macierzy <- append(do_macierzy, c(index, '0'))
  }
  d <- d+1
  e <- e+1
}
gotowa_macierz <- matrix(do_macierzy, nrow = ilosc_rzedow, ncol = 2, byrow = TRUE)
gotowa_macierz <- ifelse(gotowa_macierz=="NaN",0,gotowa_macierz)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "srednie_manualne_szybkie_na_32_robocze_z_transkacjami.xlsx")

 
 # system.time(for (i in 1:100000){ b <- 240 + 15/30 * as.numeric(df[[1,5]])
 #   + as.numeric(df[[1,5]]) - as.numeric(df[[1,5]])
 #   })
 # 
 # 
 # system.time(for (i in 1:100000){a <- as.numeric(df [[1,5]])
 # b <- 240 + 15/30 * a
 # + a - a
 # })

