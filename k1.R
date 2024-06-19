dane=read.csv("C:/Users/48663/Documents/laby statystyka/opisowa.csv", sep=";")
egzamin = na.omit(dane$egzamin)
egzamin
#a
mean(egzamin)
#Sredni wynik z egzaminu wynosi 35 pkt
sd(egzamin)
#Przeciętnie wynik z egzaminu ze statystyki odchylał się od średniej o 3 pkt
#b
a=seq(25, 50, 5)
fiu=table(cut(egzamin, a))
#c
pie(fiu)
#zad 2
#a
p=(0.8+9/100)
n=6
U=c(0:6)
pr=dbinom(U,n,p) #gestosc a pbinom dystrybuanta
tabela=rbind(U,pr)
tabela
#b
#P(U>=2)
1-pbinom(1,n,p)
#Prawdopodobieństwo, że liczba urządzeń wymagających
#interwencji będzie nie mniejsza niż 2 wynosi 99,99%
#c
expected=sum(U*pr)
expected
#przeciętnie możemy spodziewać się że spośród 6 urządzeń 5 będzie wymagało interwencji automatyka.
#zad 3
n = 10000
mu=257
sd=250+9
#b
pnorm(2550000, mu*n, sd*sqrt(n))
#zad 4
cisnienie = na.omit(dane$cisnienie)

conf = (90 + 9) / 100 
pr = sigma.test(cisnienie, conf.level = 0.99)$conf.int

L = sqrt(pr[[1]])
P = sqrt(pr[[2]])
L
P

# z ufnością 99% przedział (1.71, 4.77) pokrywa prawdziwą nieznaną wartość odchylenia standardowego maksymalnego ciśnienia badanej mieszanki 

