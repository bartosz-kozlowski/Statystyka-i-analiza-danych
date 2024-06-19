dane=read.csv("C:/Users/48663/Documents/laby statystyka/DwiePopulacje.csv", sep=";")
dane
reg1=na.omit(dane$cel1)
reg2=na.omit(dane$cel2)
#zad 1
#H0: mu1=mu2   ;  H1: mu1!=mu2
n1=length(reg1)
n2=length(reg2)
x_bar1=mean(reg1)
x_bar2=mean(reg2)
var1=var(reg1)
var2=var(reg2)
Sp2=((n1-1)*var1+(n2-1)*var2)/(n1+n2-2)
#1.  H0: mu1-mu2=0     H1: mu1-mu2!=0
#2. 
t=(x_bar1-x_bar2)/(sqrt(Sp2*(1/n1+1/n2)))
t
#t = -1.539823
#3. 
alfa = 0.02
qt(1-alfa/2, n1+n2-2)
#R=(-inf, -2.47266) suma (2.47266, inf)
#4. t nie nalezy do R -> brak podstaw do odrzucenia H0
#5. Na poziomie istotnosci 0.02 dane nie potwierdzaja hipotezy
#że przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
#zawartości celulozy dla regionu II 

#1  H0: mu1-mu2=0     H1: mu1-mu2!=0
#2
t.test(reg1, reg2, var.equal = TRUE, mu=0, alternative = "two.sided")
#3
#p-value = 0.1352
#4
#alfa = 0.02 < p-value = 0.1352 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotnosci 0.02 dane nie potwierdzaja hipotezy
#że przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
#zawartości celulozy dla regionu II 

#b. 
#1. H0: sig^2_1-sig^2_2=0    H1: sig^2_1-sig^2_2!=0
#2. 
F=var1/var2
F
#F = 0.4786012
#3.
qf(alfa/2, n1-1, n2-1)
qf(1-alfa/2, n1-1, n2-1)
#R=(0; 0.162458) suma (3.69874; inf)
#4. F nie należy do R -> brak podstaw do odrzucenia H0
#5. Na poziomie istotnosci 0.02 dane nie potwierdzaja hipotezy o roznosci wariancji.
# Zatem mozemy zalozyc ze wariancje sa jednorodne.

#1. H0: sig^2_1-sig^2_2=0    H1: sig^2_1-sig^2_2!=0
#2. 
library(PairedData)
var.test(reg1, reg2, alternative = "two.sided")
#3.
#alfa = 0.02
#4.
#alfa = 0.02 < p-value = 0.3225 -> brak podstaw do odrzucenia H0.
#5. Na poziomie istotnosci 0.02 dane nie potwierdzaja hipotezy o roznosci wariancji.
# Zatem mozemy zalozyc ze wariancje sa jednorodne.

#c
t.test(reg1, reg2, var.equal = TRUE, conf.level = 0.98)
#Na poziomie ufnosci 98% przedzial (-13.52; 3.15) pokrywa nieznana prawdziwa
#roznice srednich zawartosci celulozy w drewnie w dwoch regionach. 
#Poniewaz przedzial ufnosci pokrywa wartosc 0, zatem nie mamy podstaw
#do odrzucenia H0.

#zad 2
tradycyjna=na.omit(dane$tradycyjna)
nowa=na.omit(dane$nowa)

#1.  H0: sig^2_1-sig^2_2=0     H1: sig^2_1-sig^2_2!=0
#2.
var.test(tradycyjna, nowa, alternative = "two.sided")
#3. p-value = 0.36
#4. alfa = 0.1 < p-value = 0.36 -> nie odrzucamy H0
#5. Na poziomie istotności alfa = 0.1 dane nie potwierdzają hipotezy o różności
#wariancji.

#Założenie: rozklad normalny, rozne wariancje
#1. H0: mu1-mu2<=0  H1: mu1-mu2>0
#2. 
t.test(tradycyjna, nowa, var.equal = FALSE, mu = 0, alternative = "greater")
#p-value = 0.616
# alfa = 0.1 < p-value = 0.616 -> nie odrzucamy H0
#Na poziomie istotnosci 0.1 dane nie potwierdzają hipotezy, 
#że czas budowy metodą tradycyjną jest dłuższy niż czas
#budowy nową technologią.


#zad 3
publiczny=na.omit(dane$publiczny)
prywatny=na.omit(dane$prywatny)

#1.  H0: sig^2_1-sig^2_2=0     H1: sig^2_1-sig^2_2!=0
#2.
var.test(publiczny, prywatny, alternative = "two.sided")
#3. p-value = 0.08687 
#4. alfa = 0.1 > p-value = 0.08687 -> odrzucamy H0
#5. Na poziomie istotności alfa = 0.1 dane potwierdzają hipotezę o różności
#wariancji.

#Założenie: rozklad normalny, rozne wariancje
#1. H0: mu1-mu2>=0  H1: mu1-mu2<0
#2. 
t.test(publiczny, prywatny, var.equal = FALSE, mu = 0, alternative = "less")
#p-value = 0.023
# alfa = 0.1 > p-value = 0.023 -> odrzucamy H0
#Na poziomie istotnosci 0.1 dane potwierdzaja hipoteze ze publiczne zrodla 
#finansowania udzielają, przecietnie rzecz biorac, mniejszych kredytow.

#zad 4
zm1=na.omit(dane$zawodnik1)
zm2=na.omit(dane$zawodnik2)
#1. H0: sig^2_1-sig^2_2 >= 0     H1: sig^2_1-sig^2_2 < 0
#2.
var.test(zm1, zm2, alternative = "less")
#3. p-value = 0.2108
#4. alfa = 0.05 < p-value = 0.2108 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezę o większej
#regularnosci wynikow pierwszego zawodnika.

#zad 5
L1=na.omit(dane$L1)
L2=na.omit(dane$L2)

#1.  H0: sig^2_1-sig^2_2=0     H1: sig^2_1-sig^2_2!=0
#2.
var.test(L1, L2, alternative = "two.sided")
#3. p-value = 0.64
#4. alfa = 0.1 < p-value = 0.64 -> NIE odrzucamy H0
#5. Na poziomie istotności alfa = 0.1 dane  nie potwierdzają hipotezy o różności
#wariancji.

#Założenie: rozklad normalny, rozne wariancje
#1. H0: mu1-mu2<=0  H1: mu1-mu2>0
#2. 
t.test(L1, L2, var.equal = TRUE, mu = 0, alternative = "greater")
#p-value = 0.08
# alfa = 0.1 > p-value = 0.08 -> odrzucamy H0
#Na poziomie istotnosci 0.1 dane potwierdzaja hipoteze ze 
#sredni czas działania leku L1 jest istotnie dłuższy niż dla leku L2.

#zad 6
T1 = 0.78*1200
T2 = 0.8*2000
n1 = 1200
n2 = 2000
prop.test(c(T1, T2), c(n1, n2), conf.level = 0.9)
#Z ufnoscia 90% przedzial (-4.53%; 0.53%) pokrywa nieznana prawdziwa roznice
#proporcji osob zadowolonych z pracy w Polsce i w USA.

#b
#1.   H0:  p1-p2>=0     H1: p1-p2<0
#2.
prop.test(c(T1, T2), c(n1, n2), alternative = "less")
#3. p-value = 0.09583
#4. alfa = 0.1 > p-value = 0.09583 -> odrzucamy H0
#5. Na poziomie istotnosci alfa = 0.1 dane potwierdzaja hipoteze ze proporcja
#zadowolonych Polakow jest mniejsza niz zadowolonych Amerykanow.

#zad 7
naz = 313+28
naf = 145 + 56
Taz = 313
Taf = 145
#1
#H0: p1-p2=0
#H1: p1-p2!=0
#2
prop.test(c(Taz, Taf), c(naz, naf), alternative ="two.sided")
#3
#p-value = 0
#4
#4 alfa = 0.05 > p-value = 0 -> odrzucamy H0
#5 na poziomie istotnosci 0.05 dane potwierdzaja hipoteze że
# częstość występowania malarii typu A zależy od regionu.
#b
prop.test(c(Taz, Taf), c(naz, naf), conf.level = 0.95)
#Z ufnością 95% przedział (12,4%; 26,9%) pokrywa nieznaną prawdziwą różnice
#proporcji częstości występowania malarii typu A.


#zad 9
przed = c(15, 4, 9, 9, 10, 10, 12, 17, 14)
po = c(14, 4, 10, 8, 10, 9, 10, 15, 14)
roznica=przed-po

#1.     H0: mu=0   H1: mu!=0
#2. 
t.test(roznica, conf.level = 0.95)
#3. p-value = 0.08052
#4. alfa = 0.05 < p-value = 0.08052 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotnosci 0.05 dane nie potwierdzaja hipotezy ze
#dany rodzaj leku zmienia wartości określonego parametru biochemicznego

#zad 10
pietnascie = c(6.55, 5.98, 5.59, 6.17, 5.92, 6.18, 6.43, 5.68)
sto = c(6.78, 6.14, 5.80, 5.91, 6.10, 6.01, 8.18, 5.88)
roznica=pietnascie-sto
roznica
#a
#1.     H0: mu=0   H1: mu!=0
#2. 
t.test(roznica, conf.level = 0.9)
#3. p-value = 0.23
#4. alfa = 0.1 < p-value = 0.23 -> brak podstaw do odrzucenia H0
#5. Na poziomie istotnosci 0.05 dane nie potwierdzaja hipotezy ze
#pH wody zależy od głębokości.

#b
t.test(roznica, conf.level = 0.9)$conf.int
#90% przedział ufności dla różnicy średnich wartości pH na dwóch głębokościach 
#wynosi od -0.71 do 0.13. Ponieważ przedział ten zawiera 0, 
#nie mamy podstaw do odrzucenia H0.

#zad 8
n1 = 105
n2 = 110
T1 = 73
T2 = 102
#1
#H0: p1-p2=0
#H1: p1-p2!=0
#2
prop.test(c(T1, T2), c(n1, n2), alternative ="two.sided")
#3
#p-value = 0
#4
#4 alfa = 0.05 > p-value = 0 -> odrzucamy H0
#5 na poziomie istotnosci 0.05 dane potwierdzaja hipoteze że
# częstość występowania malarii typu A zależy od regionu.
#b
prop.test(c(T1, T2), c(n1, n2), conf.level = 0.95)
#Z ufnością 95% przedział (-34,1%; -12,3%) pokrywa nieznaną prawdziwą 
#różnicę proporcji przeżywalności w badanych temperaturach.