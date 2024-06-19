dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane_hip.csv", sep=";", dec=",")
wiatr=na.omit(dane$wiatr)
#zad 1 r. normalny, sigma nieznane
#procedura testowa
#1. H_0: mu <= 4 m/s  H_1: mu > 4m/s
#2.
mu = 4
x_bar = mean(wiatr)
sd=sd(wiatr)
n=length(wiatr)
t=(x_bar-mu)/(sd/sqrt(n))
t
#t = 2.418622
#3. 
alfa=0.05
qt(1-alfa, n-1)
#R = (1.795885, inf)
#4. t należy do R -> odrzucamy H0
#5. na poziomie istotnosci alfa = 0.05 dane potwierdzaja hipotezę, że średnia 
#prędkość wiatru przekracza 4m/s.
#Sensowna jest budowa elektrowni wiatrowej w okolicach Darłowa.

#procedura testowa
#1. H_0: mu <= 4 m/s  H_1: mu > 4m/s
#2.
t.test(wiatr, mu = 4, alternative="greater")
#3. p-value = 0.01705
#4. alfa = 0.05 > p-value -> odrzucamy H0
#5. na poziomie istotnosci alfa = 0.05 dane potwierdzaja hipotezę, że średnia 
#prędkość wiatru przekracza 4m/s.
#Sensowna jest budowa elektrowni wiatrowej w okolicach Darłowa.

#Zad 2 #r. normalny, sigma nieznane
pompa = na.omit(dane$pompa)
#1. H_1: mu < 3.5; H_0: mu >= 3.5
#2.
t.test(pompa, mu = 3.5, alternative="less")
#3. p-value = 0.1521
#4. alfa = 0.01 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotnosci alfa = 0.01 dane nie potwierdzaja hipotezy, że 
#wspolczynnik efektywnosci pompy cieplnej w gospodarstwie domowym jest mniejszy
#niz 3.5.

#Zad 3 #rozklad normalny, sigma znane
#1. H0: mu = 870   H1: mu != 870
morze = na.omit(dane$morze)
#2. 
library(BSDA)
z.test(morze, sigma.x = 5, mu = 870, alternative = "two.sided")
#3. p-value = 0.6547
#4. alfa = 0.05 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie alfa = 0.05 dane nie potwierdzają hipotezy,
# że średnia głębokość morza w tym rejonie jest różna od 870m.

#Zad 4 #proba duza n >= 30
b1 = na.omit(dane$blaszki)
#1. H0: mu <= 0.04; H1: mu > 0.04
#2.
library(BSDA)
zsum.test(mean(b1), sd(b1), length(b1), mu = 0.04, alternative="greater")
#3. p-value = 0.05041
#4. alfa = 0.02 < p-value -> brak podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.02 dane nie potwierdzają hipotezy, 
#że produkowane przez ten automat blaszki są grubsze niż nominalna grubość.

#Zad 5
mleko = na.omit(dane$mleko)
n=length(mleko)
alfa=0.05
#a
#1. H_1: mu != 1.7; H_0: mu = 1.7
#2.
t.test(mleko, mu = 1.7, alternative="two.sided")
#3. p-value = 0.1114
#4. alfa = 0.05 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotnosci alfa = 0.05 dane nie potwierdzaja hipotezy, że 
# średnia zawartość tłuszczu w mleku jest różna od 1,7 %.

#b
#1. H0: sig^2>=0.02   H1: sig^2<0.02
#2. 
Chi2 = (n-1)*var(mleko)/0.02
#Chi2 = 5.2
#3.
qchisq(alfa, n-1)
#R = (0; 3.325113)
#4. Chi2 nie należy do R -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
#że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0.02%^2.

#b 2 sposób
#1. H0: sig^2>=0.02   H1: sig^2<0.02
#2.
library(TeachingDemos)
sigma.test(mleko, sigmasq=0.02, alternative="less")
#3.p-value = 0.1835
#4. alfa = 0.02 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
#że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0.02%^2.

#Zad 8
n = 2500
T = 1600
phat = T/n
alfa=0.05
#1. H0: p=60%    H1: p!=60%
#2. 
Z=(phat-0.6)/sqrt(0.6*(1-0.6)/n)
#Z = 4.082483
qnorm(1-alfa/2)
#3. R = (-inf; -1.959964) suma (1.959964; inf)
#4. Z należy do R -> odrzucamy H0
#5. Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że 60% osób
# nie chce pójść na wybory.


#1. H0: p=60%    H1: p!=60%
#2. 
binom.test(T, n, p=0.6, alternative = "two.sided")
#3. p-value = 4.413e-05
#4. alfa = 0.05 > p-value -> odrzucamy H0
#5. Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że 60% osób
# nie chce pójść na wybory.

#zad 6
jaja = na.omit(dane$kukulki)
mu = 17
sig = 2.5
alfa = 0.05
#a
#1. H_1: mu = 17; H_0: mu != 17
#2.
t.test(jaja, mu = 17, alternative="two.sided")
#3. p-value = 0.01
#4. alfa = 0.05 > p-value -> odrzucamy H0
#5. Na poziomie istotnosci alfa = 0.05 dane potwierdzaja hipotezę, że 
#   średnia zawartość długości podrzuconych jaj wynosi 17 nm.

#1. H0: sig^2 = 6.25   H1: sig^2 != 6.25
#2.
library(TeachingDemos)
sigma.test(jaja, sigmasq = 6.25, alternative="two.sided")
#3. p-value = 0.4984
#4. alfa = 0.05 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
#   że wariancja długości podrzuconych jaj jest różna od 6.25.

#b
library(BSDA)
z.test(jaja, sd = sig, conf.level = 0.95)
srednia = mean(jaja)
liczebnosc = length(jaja)
odchylenie = sd(jaja)
alfa = 0.05
Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
Lt
Pt
#Przedział ufności to (17.36; 19.36)
#µ0 nie należy do przedziału ufności, w związku z czym odrzucamy H0, zgadza się
#zad 7
#1
# H0: mu <= 55
# H1: mu > 55
sr = 60
sd = 20
len = 100
#2
library(BSDA)
zsum.test(sr, sd, len, mu = 55, alternative="greater")
#3
#p-value = 0.00621
#4
#alfa=0.01
#alfa > p-value =>  odrzucamy hipotezę H0

#5
#Na poziomie istotności alfa 0.01 dane potwierdzają hipotezę, że fabryka działa nielegalnie.

#b
#1
# H0: sig^2 = 18^2
# H1: sig^2 != 18^2
#2
Chi2 = ((len-1)*20^2)/(18^2)
Chi2
#Chi2 = 122.22
#3.
alfa = 0.01
len = 100
qchisq(alfa / 2, len - 1)
qchisq(1 - alfa/2,len - 1)
#R= (0, 66.5) suma (139, inf)
#4. Chi2 nie należy do R -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotności alfa = 0.01 dane nie potwierdzają hipotezy,
#że wariancja pomiarów jest różna od 18.

#zad 9
n=1200
T=16
alfa=0.05
phat=T/n
p0=0.02
#1
#H0: p>=0.02
#H1: p<0.02
#2
z=(phat-p0)/sqrt(p0*(1-p0)/n)
#z = -1.65
#3. 
qnorm(1-alfa)
#R=(-inf;-1.644854)
#4. Z należy do R -> odrzucamy H0
#5. Na poziomie istotności 0.05, dane nie potwierdzają hipotezy, że 
#frakcja jaj złej jakości w badanej fermie jest mniejsza niż 2%.

#1
#H0: p>=0.02
#H1: p<0.02
#2
binom.test(T, n, p = p0, alternative="less")
prop.test(T, n, p = p0, alternative="less")
#3
#p-value = 0.055 
#4
# p-value = 0.055 > alfa = 0.05 -> nie odrzucamy H0
#5 Na poziomie istotności 0.05, dane nie potwierdzają hipotezy, 
#że frakcja jaj złej jakości w badanej fermie jest mniejsza niż 2%.

#zad 10
n=1100 
T=1000
alfa=0.05
phat=T/n
p0=0.90

#1
#H0: p<=0.90
#H1: p>0.90

#2.
z=(phat-p0)/sqrt(p0*(1-p0)/n)
#z = 1.005038

#3
qnorm(1-alfa)
#R=(1.644854; inf)

#4. Z nie należy do R -> nie odrzucamy H0
#5. Na poziomie istotności 0.05, dane nie potwierdzają hipotezy, że 
#że procent Polaków, którzy nie przeczytali żadnej książki jest większy niż 90%.

#1
#H0: p<=0.90
#H1: p>0.90

#2
binom.test(T, n, p = p0, alternative="greater")
prop.test (T, n, p = p0, alternative="greater")

#3
#p-value = 0.1701 

#4
# p-value = 0.1701 > alfa = 0.05 -> nie odrzucamy H0

#5. Na poziomie istotności 0.05, dane nie potwierdzają hipotezy, że 
#że procent Polaków, którzy nie przeczytali żadnej książki jest większy niż 90%.
