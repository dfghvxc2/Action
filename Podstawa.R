srednia_sprzedaz <-c(20,25,35,45,150,34,22,12,34,35,23,34,24,24,35,32,45,65,23,34,32,32,34,35,1,35,36,35,38,39,34,35,36,41)
dni <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)

df <- data.frame(srednia_sprzedaz,dni)

p <- ggplot(df,aes(y = srednia_sprzedaz))
p + geom_boxplot()



#Instant calculation of outliers 
boxplot.stats(srednia_sprzedaz)$out 

#Manual calculation of outliers 

quantile(srednia_sprzedaz)
summary(srednia_sprzedaz)
IQR(srednia_sprzedaz)

a <- IQR(srednia_sprzedaz) * 1.5 

Q3 <- a + 35.75
Q1 <- 26.75 - a 

#Przyk³ady na danych 
srednia_sprzedaz1 <-c(30,34,35,36,38,38,385,654,34,32,23,25,1,26,27,25,45,34,45,45,46,47,48,49,43,23,24,23,24,25,26,24,23,25,26,27,35,34)

#boxplot.stats(srednia_sprzedaz1)$out

result1 <- unname(quantile(srednia_sprzedaz1,0.25))


result2 <- unname(quantile(srednia_sprzedaz1,0.75))


IQR <- IQR(srednia_sprzedaz1) * 1.5 

Q3 <- IQR + result2
Q1 <- result1 - IQR 

append <- append(srednia_sprzedaz1, c(Q3,Q1))

sorted <- sort(append)

pre_final <- sorted[sorted < Q3]
Final_flattened_vector <- pre_final[pre_final > Q1]

Final_flattened_vector  #Dzia³a 

mean(Final_flattened_vector)

#Gotowa funkcja wyniki 
#Dla srednia_sprzedazy 150, 65, 1 
#Dla srednia_sprzedazy1 385, 654

#Manualne liczenie wyniki 
#Dla srednia_sprzedazy 150, 65, 12, 1 
#Dla srednia_sprzedazy1 385, 654


wektor <- c(1,2,3,4,5)
wektor <- append(wektor,c(Q1,Q3))
