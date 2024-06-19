dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
#zad 1
chlorki = dane$chlorki
n=40
srednia=mean(na.omit(chlorki))
sd1=sd(na.omit(chlorki))
#Przecietna zawartosc chlorkow w wodociagu miejskim odchyla sie od sredniej o 7.47 mg/l
wz=(sd1/srednia)*100
#Wspolczynnik zmiennosci wynosi 9%, wiec jest to slabe zroznicowanie danych.
#b
przedzial=seq(65, 100, length=7)
przedzial
fiu=table(cut(chlorki,przedzial))
fiu
#c
boxplot(chlorki)
#zad 2
sig = 2.2
mu = 13.7
pnorm(15,mu,sig)-pnorm(13,mu,sig)
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
#zad 3
#jest to rozklad dwumianowy
n=100
p=0.45
beta=c(0:100) 
pr=dbinom(beta, n,p) #gestosc a pbinom dystrybuanta
rbind(beta,pr)
#P(X<40)
pbinom(40, n, p)

#zad 4
n = 16
h = 9
conf = (90+h)/100
wiatraki = na.omit(dane$moc)
#n
pr= sigma.test(wiatraki, conf.level = conf)$conf.int
sqrt(pr[[1]])
sqrt(pr[[2]])

# Z ufnością 99% przedział (0.34 ; 0.94) pokrywa nieznaną prawdziwą wartosc
# odchylenia standardowego uzyskanej mocy

