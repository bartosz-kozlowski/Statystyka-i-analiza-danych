dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
wiek = na.omit(dane$wiek)
#zad 1
#a
zm = sd(wiek)/mean(wiek)*100
zm
#Współczynnik zmienności mieści się w przedziale (0-20%), więc jest to słabe
#zróżnicowanie danych.
sd(wiek)
#Przecietnie wiek uczestników pewnego kursu odchylal sie od sredniej o 5 lat.
#b
przedzial = seq(25, 45, length = 4)
table(cut(wiek, przedzial))
#c
boxplot(wiek)
#zad 2
mu = 174
sig = 9
pnorm(175, mu, sig) - pnorm(170, mu, sig)
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
#zad 3
p = 0.55
T = 100
n = 150
# > 100
1-pnorm(T/n,p,sqrt(p*(1-p)/n))
#zad 4
n = 20
azot = na.omit(dane$azot)
conf = 0.99
library(TeachingDemos)
e=sigma.test(azot, conf.level=0.99)$conf.int
L=sqrt(e[1])
P=sqrt(e[2])
L
P
# Z ufnością 99% przedział (0.23, 0.56) pokrywa nieznaną prawdziwą wartość odchylenia standardowego
# stężeń azotu amonowego)