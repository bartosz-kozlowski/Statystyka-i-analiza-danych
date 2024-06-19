dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
czas = na.omit(dane$czas)
#zad 1
mediana = quantile(czas)[3]
mediana
#25 klientów zostało obsłużonych przy kasie sklepowej dokładnie w 31s lub mniej sekund oraz pozostali klienci (25) zostali
# obsłużeni dokładnie w 31 sekund lub więcej sekund.
#b
przedzial = seq(15, 60, length = 6)
szereg = table(cut(czas, przedzial))
szereg
#c
hist(czas, przedzial, freq=FALSE)
#zad 2
h = 9
lambda=1/h
#a
curve(dexp(x,lambda), 0, 100)
#b
#P(S>=2)
1-pexp(2, lambda)
#zad 3
mu = 35+9
sig = 2
n = 75
#a rozklad calkowitego max cisnienia T ~ N(mu*n, sqrt(n)*sig)
#b P <= 2750
pnorm(2750, mu*n, sqrt(n)*sig)
#zad 4
n = 130
T = 40
conf = 0.99
binom.test(T, n, conf.level = conf)
#z ufnością 99% przedział (0.17, 0.46) pokrywa nieznaną 
#prawdziwą proporcję Polaków uważających 30-dniowy
#urlop za zbyt dług.
#Nie można uznać, że Polacy i Amerykanie mają taki sam pogląd dot. urlopu,
#ponieważ wartość 0.55 nie zawiera się w przedziale.