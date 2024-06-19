dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
energia = na.omit(dane$energia)
#zad 1
#a
Q3 = quantile(energia)[4]
Q3
# W 75% domach miesięczne zużycie energii było mniejsze lub równe 317.75 kWh i w reszcie domów (25%)
# miesięczne zużycie energii było większe lub równe 317.75 kWh.
sig = sd(energia)
sig
# Przeciętne zużycie energii w domach odchylało się od średniej o 53 kWh.
#b
przedzial = seq(180, 420, length = 6)
przedzial
szereg = table(cut(energia, przedzial))
#c
pie(szereg)
#zad 2
h = 9
lamda = 1/h
#a
curve(dexp(x,lambda), 0, 100)
#b
# P(X<=3) = F(3)
pexp(3, lambda)
#zad 3
mu = 165+9
sig = 5
n = 40
#a
#rozklad sredniego wzrostu 40 wylosowanych kobiet N(mu, sig/sqrt(n))
#b
pnorm(167, mu, sig/sqrt(n)) - pnorm(164, mu, sig/sqrt(n))
#zad 4
n = 16
moc = na.omit(dane$moc)
conf = 0.99
t.test(moc, conf.level=0.99)
#Z ufnością 99% przedział (3.65, 4.42) pokrywa nieznaną prawdziwą średnią
# moc ze wszystkich elektrowni wiatrowych.
blad = 0.1 
wielk_proby = ceiling((qt(1-(1-conf)/2, n-1)*sd(moc)/blad)^2)
wielk_proby 
#Próba powinna wynosić 233, żeby mieć pewność, że błąd estymacji wynosi 0.1 setek kW.