dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
chlorki = na.omit(dane$chlorki)
#zad 1
#a
sd(chlorki)/mean(chlorki) *100
#Wspolczynnik zmiennosci wynosi 9% co oznacza, że dane są słabo zróżnicowane.
sd(chlorki)
#Przeciętne odchylenie zaawartości chlorków w wodociągu miejskim odchyla się od średniej o 7.47 mg/l
#b
przedzial = seq(65, 100, length = 6)
table(cut(chlorki,przedzial))
#c
boxplot(chlorki)
#zad 2
mu = 13.7
sig = 2.2
pnorm(15, mu, sig) - pnorm(13, mu, sig)
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
#zad 3
p = 0.45
n = 100
T = 40
#jest to rozklad dwumianowy
#P(B<=40)
#pbinom(40, 100, 0.45)
pnorm(T/n,p,sqrt(p*(1-p)/n))
#zad 4
n = 16
moc = na.omit(dane$moc)
conf = 0.99
lol=sigma.test(moc, conf.level=0.99)$conf.int
lol
L=sqrt(lol[1])
L
P=sqrt(lol[2])
P
#Z ufnoscia 99% przedzial (0.34, 0.94) pokrywa prawdziwą nieznana 
#wartość odchylenia standardowego uzyskanej mocy.

