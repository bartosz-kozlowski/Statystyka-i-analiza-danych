dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
wiek = na.omit(dane$wiek)
#zad 1
srednia = mean(wiek)
srednia
#Sredni wiek 32 uczestnikow pewnego kursu wynosi 36 lat.
Q1=quantile(wiek)[2]
Q1
# 25% uczestników pewnego kursu ma wiek mniejszy lub równy 36 lat a reszta uczestników (75%)
# ma wiek większy lub równy 36 lat.
#b
przedzial = seq(25, 45, length = 5)
przedzial
table(cut(wiek, przedzial))
#c
hist(dane$wiek, przedzial, freq=TRUE)
#zad 2
h = 9
lambda = 1/h
#a
curve(dexp(x,lambda), 0, 100)
#b
#P(X>=4)
1-pexp(4, lambda)
#zad 3
p = 0.1
n = 150
#rozklad dwumianowy przyblizony rozkladem normalnym  P~N(n*p, sqrt(n*p(1-p)))
#P(P<20)
#Przyblizenie rozkladem normalnym
pnorm(20,n*p,sqrt(n*p*(1-p)))
#zad 4
n = 200
mu = 0.35
sig = 0.05
conf = 0.99
alpha = 0.01
library(BSDA)
zsum.test(mu, sig, n, conf.level= 0.99)

sqrt(((n-1)*sig^2)/qchisq(1-alpha/2, n-1))
sqrt(((n-1)*sig^2)/qchisq(alpha/2, n-1))
L = 0.34+0.04
P = 0.36+0.06
#Z ufnością 99% przedział (0.34, 0.36) pokrywa prawdziwą nieznaną wartość
#średniej zawartości witaminy C w soku pomarańczowym tej firmy.
#Właściciel firmy może podawać prawdziwą informację, ponieważ niewiele brakuje
#biorąc pod uwagę powyższy przedział, ale biorąc pod uwagę odchylenie standardowe,
#które możemy dodać, może to być jak najbardziej prawdą.

#Właściciel firmy niewiele się myli, gdyż wartość 0.37 niby nie pokrywa się
#z przedziałem, lecz jest bardzo blisko.
