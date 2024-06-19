dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
energia=na.omit(dane$energia)
Q3=quantile(energia)[4]
Q3
#W 75% domow zuzycie enrgii bylo mniejsze lub rowne 317.75 kWh 
# i w pozostalych domach zuzycie energii bylo wieksze lub rowne 317.75 kwh
#b
przedzial = seq(180,420, l=5)
przedzial
table(cut(energia,przedzial))
#c
pie(table(cut(energia,przedzial)))
#zad 2
lambda=1/9
#a
curve(dexp(x,lambda), 0, 100)
#b
pexp(3, lambda)
#zad 3
mu = 165+9
sig = 5
n = 40
pnorm(167,mu,sig/sqrt(n))-pnorm(164,mu,sig/sqrt(n))
#zad 4
alpha = 1 - 0.99
blad = 0.1
n = 16 
p = 2.8
conf = 0.99
moc= na.omit(dane$moc)
#przedzial ufnosci dla wariancji 
t.test(moc, conf.level = 0.99)$conf.int
#z ufnoscia 99% przedzial (3.65, 4.42) zawiera prawdziwa nieznana 
#srednia moc ze wszystkich elektrowni wiatrowych typu 

#b
wielk_proby = ceiling((qt(1-(1- conf)/2, n - 1)*sd(moc)/blad)^2)
wielk_proby
