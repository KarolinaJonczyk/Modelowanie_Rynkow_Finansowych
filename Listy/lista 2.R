#Zadanie 2.1. (wyszukiwarka listów emisyjnych)
#Ile wyniosła cena ”brudna” obligacji PS0123 kupionej na GPW
#w dniu D = 12.02.2019 po kursie 102.40? Przyjmujemy tutaj, że dzień
#rozliczenia transakcji to D + 2.

#rozwiązanie
#dzień rozliczenia 14.02.2019, 25.01.2019 to koniec okresu rozliczeniowego
#cena= 102.4*10
#D = 20 #ilośc dni między tymi datami
#OD = 25*D / 365   #2.5%
#P = 1024 + OD


#obliczanie daty
d1=as.Date("2019-02-12")+2
#as.Date("12/02/2019"; format="%d/m%/y%")
d2=as.Date("2019-01-25")
d=as.numeric(d1-d2, units="days")
print(d)

#Zadanie 2.2
#Znajdź YTM obligacji o terminie wykupu za 8 lat
# i o wartości nominalnej PLN 10000, cenie PLN 980 oraz kuponach
# PLN 280 płatnych co pół roku.

#skala YTM: pół roku 

#$P = \frac{C}{YTM} + (FV - \frac{C}{YTM})(1+YTM)^{-n}$

#f-> function(YTM)
#C/YTM + (FV - C/YTM)*(1+YTM)1-n-p
#P= \sum\ displaylimits_k=0^n (\frac{C_k}{(1+YTM)^{t_k}), t_k=0, 1/2, 1, 3/2,...,8"

c=280
FV=10000
n=2*8
P=980
  
f1 <- function(x){-P + sum(C*(1+r)(-(1:16)/2))+ FV*(1+r)*(-8)}
print(f1)  
#YTM = ...



#Zadanie 2.3 Rozpatrzmy obligację z dwuletnim terminem wykupu, o wartości nominalnej 1000zł, 20% kuponie płatnym co pół roku i rentowności 15%.
#(a) Jaka jest jej bieżąca wartość (cena)?
#(b) Jaki jest czas trwania obligacji?
#(c) Jaki jest zmodyfikowany czas trwania obligacji?
#(d) Jak i w przybliżeniu o ile zmieni się cena
# obligacji gdy rentowność spadnie do 14%?

FV = 1000
Cr = 1000 * 0,2
YTM_1 = 0.15
YTM=0.01
#a
P = sum(100*(1 + YTM) *(-(1/4))+ 1000*(1+YTM_1)*(-2))

#b
MacD = (1/D) * sum((100*(1/4)*(1+YTM)(-(1:4))) + 1000*2(1+YTM_1)^n *(-2))

#c
ModD = MacD/(1+YTM_1)

#d
Pdelta = ModD/YTM



