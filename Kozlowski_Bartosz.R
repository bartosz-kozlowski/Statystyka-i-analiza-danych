dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
dane

#zad 1
egzamin = na.omit(dane$egzamin)
#a
Q3 = quantile(egzamin)[4]
Q3
# 75% z grupy 80 studentów miało wynik egzaminu ze statystyki 
#mniejszy lub równy 38 pkt i reszta grupy studentów (25% z 80 studentów)
# miała wynik większy lub równy 38 pkt.
odchylenie = sd(egzamin)
odchylenie
#Przeciętny wynik egzaminu ze statystyki w grupie 80 studentów
#odchyla się od średniej o 7 pkt.
#b
przedzial = seq(10, 25, length = 7)
szereg = table(cut(egzamin, przedzial))
szereg
#c
hist(egzamin, przedzial, freq=TRUE)
#zad 2
h = 9
lambda = 1/h
#a
curve(dexp(x,lambda), 0, 100)
#b
# P(C<=10) = F(10)
pexp(10, lambda)

#zad 3
mu = (0.7+0.0)
sig = 0.01
n = 50
#a
#rozklad sredniej ilosci lemoniady w 50 wylosowanych butelkach, 
#rozkład normalny:avr~N(mu, sigma/sqrt(N))
#b P(0.71<avr<0.75)
pnorm(0.75, mu, sig/sqrt(n)) - pnorm(0.71, mu, sig/sqrt(n))

#zad 4
azot = na.omit(dane$azot)
n = 20
conf = 0.99
t.test(azot, conf.level=0.99)
# Z ufnością 99% możemy powiedzieć, że przedział od 1.67 mg NH4/l do
# 2.1 mg NH4/l pokrywa nieznaną prawdziwą średnią zawartość wyniku stężeń azotu
# amonowego w ściekach.

alfa = 1 - conf
blad = 0.15
n = 20
wielk_proby = ceiling((qt(1-alfa/2, n-1)*sd(azot)/blad)^2)
wielk_proby
#Próba powinna wynosić 40, żeby mieć 99% pewności, że błąd estymacji wynosi 0.15 mg NH4/l.

