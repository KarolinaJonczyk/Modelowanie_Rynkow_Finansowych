#zadanie 2.4
#Wiadomo, że YTM=3.5% dla obligcji o terminie wykupu za 5 lat,
#wartości nominalnej PLN 1000 i cenie PLN 950.10.
#Jaka jest wartość każdego z kuponów płatnych raz na rok?
a<-function(x){1000/(1+0.035)^5-950.1+sum(x/(1+0.035)^(1:5))}
s=uniroot(a,c(0,30))$root
print(s)
#zadanie 2.5
#Jedna sztuka obligacji OK0521 została kupiona na Giełdzie Papierów 
#Wartościowych (GPW) w dniu D=20.02.2019 po kursie 96,90. Jaka jest
#rentowność tej inwestycji dla kupującego (zakładając, że będzie trzymał
#obligację do dnia wykupu), jeśli prowizja maklerska jaką płaci kupujący
#wynosi 0.12% wartości transakcji??
#Uwaga: Przyjmujemy tutaj, że dzień rozliczenia transakcji to D.
F <- 1000

# Data wykupu 25.05.2021
# Data zakkupu 20.02.2019
d1=as.Date('2019-02-20')
d2=as.Date('2021-05-25')
liczba_dni = as.numeric(d2-d1)

P <- 96.90*(1+0.0012)/100 * F
t <- liczba_dni/365 #20.02.2019 - 25.05.2021
YTM <- (F/P)^(1/t)-1
print(YTM)

#zadanie 2.6
#Każda z osób A, B, C, D zaciąga kredyt w wysokości K na n okresów.
#Spłaty rat kredytu następują na koniec każdego z okresów. Oprocentowanie
#kredytu w skali okresu wynosi r przy czym:
#A spłaca kredyt w jednakowych ratach
#B spłaca kredyt metodą "jednakowych rat kapitałowych" tzn. każda
#rata spłaty jest równa K/n plus odestki od kwory pozostałej do 
#spłaty na początku okresu,
#C w pierwszych n-1 ratach spłaca jedynie odsetki za kończący się okres,
#a w ostatniej spłaca cały kapitał K plus odsetki za n-ty okres,
#D spłaca całość zobowiązań na koniec n-tego okresu, przy czym
#odsetki są kapitaliozowane na koniec każdego okresu.
#Napisz funkjcę OstatniaRata(TypeFlag=c("A)", "B", "C", "D"), K, n, r)
#zwracającą wektor wszystkich rat spłaty kredytu dla osób A, B, C, D.
OstatniaRataa=function(TypeFlag = c("A","B","C","D"), K, n, r) {
  #A
  p=(1+r)^(-(1:n))
  Ca=K/sum(p)
  va=rep(Ca,n)
  #B
  vb=rep(K/n,n)
  for(i in 1:n){
    vb[i]=vb[i]+r*(K-(i-1)*K/n)}
  #C
  vc=rep(K*r,n)
  vc[n]=vc[n]+K
  #D
  vd=rep(0,n)
  vd[n]=K*(1+r)^n
  switch(TypeFlag,
         "A"={return(va)},
         "B"={return(vb)},
         "C"={return(vc)},
         "D"={return(vd)}
  )
}
K = 1000
n = 4
r = 0.1
OstatniaRataa(TypeFlag=c("A"),K,n,r)
OstatniaRataa(TypeFlag=c("B"),K,n,r)
OstatniaRataa(TypeFlag=c("C"),K,n,r)
OstatniaRataa(TypeFlag=c("D"),K,n,r)

#lista 3

#zadanie 3.1
#Załóżmy, że funkcja stopy terminowej miała postać r(t)=0.04 + 0.001t
#gdy 8-letnia obligacja zerokuponowa została zakupiona. Pół roku później,
#gdy sprzedano obligację funkcja stopy terminowej miała postać
#r(t)=0.03 + 0.0013t. Jaka była stopa zwrotu tej inwestycji?

r1<-function(t){0.04+0.001*t}
r2<-function(t){0.03+0.0013*t}
B<-function(t,f){exp(-integrate(f,0,t)$val)}
buy=B(8,r1)
sell=B(7.5,r2)
stopa=(buy-sell)/buy
stopa

#zadanie 3.2
#Inwestor rozważa zakup obligacji zerokuponowych o termnie do wykupu
#jeden, trzy lub pięć lat. W tym momencie rentowność obligacji
#zerokuponowych 1-,2-,3-,4- oraz 5-letnich to odpowiednio
#3.1%, 3.5%, 4%, 4.2%, 4.3% przy czym rentowność jest wyrażona w skali
#roku z kapitalizacją co pół roku. Inwestro planuje sprzedaż kupowanych
#dziś obligacji za rok. Rozważa przy tym dwa następujące scenariusze:
#I. Stopy procentowe za rok będą takie same jak dziś i dlatego
#rentowność obligacji 1-,2-,3-,4- i 5-letnich wyniosą odpowiednio
#3.1%, 3,5%, 4%, 4.2%, 4.3%.
#II. Stopy procentowe (dla wszystkich okresów) za rok będa o 0.5 p.p.
#(punktu procentowego) wyższe niż dziś, tzn. rentowność obligacji 1-rocznych
#wyniesie 3.6%, dwuletnich 4% itd., przy czym rentownosć jest tu również
#wyrażona w skali roku z kapitalizacją co pół roku. Które z obligacji
#powinien zakupić inwestor w każdym z tych dwóch scenariuszy, aby 
#uzyskać jak najwyższą stopę zwrotu z tej rocznej inwestycji?
#scenariusz 1
p1=(1+(0.031/2))^(-2)
p1
p1_1 = p1*(1+(0.031/2))^(-2)
p1_1
p3 = (1+ (0.035/2))^(-2*2) #p1_3 = p2
p3
p1_3=p1*(1+ (0.035/2))^(-2*2)
p1_3
p5=(1+ (0.042/2))^(-2*4)
p5
p1_5=p1*(1+ (0.042/2))^(-2*4)
p1_5
z1=(p1_3 * p3)/p3
z1
z5=(p1_5 *p5)/p5
z5
#scenariusz 2
p2_1 = p1
p2_3 = p1*(1+(0.04/2))^(-4)
p2_3
p2_5 = p1*(1+(0.047/2)^(-8)
           