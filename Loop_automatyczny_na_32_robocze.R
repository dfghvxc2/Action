library(dplyr)
library(readxl)
library(writexl)
df_original <- read_excel("2021-07-16g09.18.47.913_SPID700_ID1464.xlsx")


df <- df_original[ -c(2:11, 72:80) ]
df <- slice(df, -c(1))
df_index <- df[-c(2:61)]
df <- df[-c(1)]

df <- df[-c(57:58,50:51,43:44,36:37,29:30,22:23,15:16,8:9,2)]
df <- df[-c(1:11)]

df[] <- lapply(df, gsub, pattern = "NULL", replacement = "NaN", fixed = TRUE)

ilosc_rzedow <- df_index %>%
  count("Towar")
ilosc_rzedow <- ilosc_rzedow$n[[1]]

do_macierzy <- vector()

b <- 2
c <- 1
d <- 1

for (i in 1:ilosc_rzedow){
  
  
  koncowy_wektor <- as.numeric(as.vector(t(slice(df,c(c)))))
  koncowy_wektor <- koncowy_wektor[!is.na(koncowy_wektor)] 
  
  if (length(koncowy_wektor) > 5) {
  
    do_wyrzucenia <- unname(boxplot.stats(koncowy_wektor)$out)
    koncowy_wektor <- koncowy_wektor[! koncowy_wektor %in% do_wyrzucenia] #problem z operatorem != w p?tli (tutaj dzia?a dobrze)
    do_macierzy <- append(do_macierzy, c(df_index[[d,1]],mean(koncowy_wektor)))
    
  } else {
    do_macierzy <- append(do_macierzy, c(df_index[[d,1]], '0'))
  }
  
  
  b <- b + 1
  c <- c + 1
  d <- d + 1
}

gotowa_macierz <- matrix(do_macierzy, nrow = ilosc_rzedow, ncol = 2, byrow = TRUE)

macierz_as_dataframe <- as.data.frame(gotowa_macierz)
write_xlsx(macierz_as_dataframe, "srednie_automatyczne_szybkie_na_32_robocze.xlsx")
