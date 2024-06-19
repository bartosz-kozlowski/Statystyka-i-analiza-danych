dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane.csv", sep=";", dec=",")
energia = na.omit(dane$energia)
#Zad 1
#a
median = quantile(energia)[3]
median
#W 50% domach zużyto dokładnie 277 kWh energii lub mniej oraz w pozostałcyh 50%
# zużyto równo 277kWh lub więcej energii.
#b
przedzial = seq(190, 400, length = 5)
szereg = table(cut(energia, przedzial))
#c
##????
hist(dane$energia, przedzial, freq=FALSE)
#Zad 2
p = 0.4
h = 9
#a
X=c(0:9)
pr=dbinom(X, h, p)
tabela = rbind(X, pr)
#b
#P(X>1)
1-pbinom(1, h, p)
#c
expected=sum(X*pr)
expected
# Średnio w czasie rocznej eksploatacji w pewnej firmie informatycznej liczba uszkodzonych
# laptopów to 4.
#zad 3
#a rozklad calkowitego wzrostu T ~ N(mu*n, sqrt(n)*sig)
#b
mu = 179
sig = 6
n = 50
#P(X<8550)
pnorm(8550, mu*n, sqrt(n) *sig)
#zad 4
n = 120
T = 23
conf = 0.99
binom.test(T,n, conf.level=0.99)
#z ufnością 99% przedział (0.10, 0.3) pokrywa nieznaną 
#prawdziwą proporcję zbiorników wodnych skażonych bakterią coli.
#Zasadne jest wdrozenie systemow poprawy jakosci wody,
#gdyż wartość 20% zawiera sie w przedziale.
