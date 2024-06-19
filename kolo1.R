dane=read.csv("C:/Users/48663/Documents/laby statystyka/opisowa.csv", sep=";")
#zad 1
egzamin = na.omit(dane$egzamin)
egzamin
#a
srednia = mean(egzamin)
srednia
##Sredni wynik z egzaminu ze statystyki wynosi 35 pkt.
sig = sd(egzamin)
sig
#Przecietnie wynik egzaminu ze statystyki odchylal sie od sredniej o 3 pkt.
#b
przedzial = seq(25, 50, 5)
przedzial
szereg = table(cut(egzamin, przedzial))
szereg
#c
pie(szereg)
#zad 2
n = 6
p = (0.8+9/100)
#a
U = c(0:6)
pr=dbinom(U,n,p) #gestosc a pbinom dystrybuanta
tabela=rbind(U,pr)
tabela
#b
#P(U>=2)
1-pbinom(1,n,p)
#Prawdopodobieństwo, że liczba urządzeń wymagających
#interwencji będzie nie mniejsza niż 2 wynosi 99,99%
#c
expected = sum(U*pr)
expected
#Przeciętnie możemy spodziewać się że spośród 6 urządzeń 
#5 będzie wymagało interwencji automatyka.
#zad 3
n = 10000
mu = 257
sig = 159
#a rozklad calkowitego rocznego rozszczenia wszystkich 10 000 ubezpieczonych T~N(n*mu, sig*sqrt(n))
#b
#P(X<2.55)
pnorm(2550000, n*mu, sig*sqrt(n))
#zad 4
cisnienie = na.omit(dane$egzamin)
conf = 0.99
library(TeachingDemos)
pr = sigma.test(cisnienie, conf.level = 0.99)$conf.int
L = sqrt(pr[[1]])
P = sqrt(pr[[2]])
L
P
# Z ufnością 99% przedział (2.71, 4.39) pokrywa nieznaną prawdziwą wartość odchylenia standardowego
# maksymalnego ciśnienia badanej mieszanki.