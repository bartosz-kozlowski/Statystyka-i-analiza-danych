#K1

#Zad 1
slonecznik=read.csv("C:/Users/48663/Documents/laby statystyka/dane_K2.csv", sep=";", dec=",")
slonecznik=slonecznik$slonecznik

#1. H0: p>=2.3    H1: mu<2.3
#2.
t.test(slonecznik, mu = 2.3, alternative="less")
#3. p-value = 0.8366
#4. alfa = 0.09 < p-value -> nie mamy podstaw do odrzucenia H0
#5. Na poziomie istotnosci alfa = 0.09 dane nie potwierdzaja hipotezy,
# że przeciętna długość łodygi słonecznika zwyczajnego jest mniejsza niż 2.3 cm.

#Zad 2

powietrze=read.csv("C:/Users/48663/Documents/laby statystyka/ANOVA_powietrze.csv", sep=";", dec=",")
powietrze

obiekty = rep(names(powietrze), c(length(na.omit(powietrze$Stare_Miasto)), length(na.omit(powietrze$Nowe_Miasto)), 
                                  length(na.omit(powietrze$Wilda)), length(na.omit(powietrze$Jezyce)), 
                                  length(na.omit(powietrze$Grunwald))))
wyniki = c(na.omit(powietrze$Stare_Miasto), na.omit(powietrze$Nowe_Miasto), na.omit(powietrze$Wilda),
           na.omit(powietrze$Jezyce), na.omit(powietrze$Grunwald))
#a
powietrzeTest=data.frame(obiekty, wyniki)
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2=sig_5^2    H1: ~H0
#H0  wariancje są jednorodne   H1: ~H0
bartlett.test(wyniki~obiekty, powietrzeTest)
alfa = 0.01
#alfa = 0.01 < p-value = 0.6144 -> zatem brak podstaw do odrzucenia H0
#Na poziomie istotności 9% nie mamy podstaw do odrzucenia H0.
#Zatem zakladamy ze wariancje są jednorodne i możemy przeprowadzić ANOVE.

#b
#H0: mu1=mu2=mu3=mu4=mu5 H1=~H0

anova(lm(wyniki~obiekty))
#alfa = 0.01 > p-value = 0 -> odrzucamy H0
#na poziomie istotnosci 1% odrzucamy H0
#stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi średnimi
#zawartościami dwutlenku siarki w dzielnicach Poznania.

#Zad 3
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_odpady.csv", sep=";", dec=",")
odpady_x = dane$wytworzone
odpady_y = dane$wykorzystane

# a)
cov(odpady_x, odpady_y) 
# Kowariancja jest różna od zera, więc istnieje liniowa zależność
# między roczną wielkością wytworzonych odpadów w PL w mln ton wg GUS, a ilością
#odpadów wykorzystywanych wtórnie w ciągu roku w mln ton.
# Ponieważ kowariancja jest dodatnia, zatem wraz ze zwiększeniem
#rocznej wielkości wytworzonych odpadów wzrasta ilość
#odpadów wykorzystywanych wtórnie w ciągu roku w mln ton.

#b)
prosta = lm(odpady_y ~ odpady_x)
prosta
#y = -193.72 + 2.15*x
#równanie regresji liniowej między wytworzonymi odpadami, a wykorzystanymi wtórnie.

#c
#Jeśli roczna wielkosc wytworzonych odpadow w PL wzrośnie o 1 mln tony to końcowa wielkość
#odpadów wykorzystywanych wtórnie w ciągu roku w mln ton wzrośnie o 2.15.
#(interpretacja współczynnika regresji liniowej)

#c)
summary(prosta)
#współczynnik determinacji R-squared=0.804*100%=80,4%
#Prosta regresji liniowej jest dobrze dopasowana do danych
#Końcowa wielkość odpadów wykorzystywanych wtórnie w ciągu roku w mln ton 
#jest wyjaśniona w ok 80% przez roczną wielkosc wytworzonych odpadow w PL.

#Zad 4

#H0: odsetek pasazerow ktorzy zgubili bagaz w trakcie lotu nie zalezy od linii lotniczej
#H1: ~H0
TAK=c(9,7,5)
NIE=c(91,93,95)
TK=data.frame(TAK, NIE)

chisq.test(TK)
#alfa = 0.09 < p-value = 0.5409 -> brak podstaw do odrzucenia H0
#Na poziomie istotności 9% dane nie potwierdzają hipotezy
#że odsetek pasazerow ktorzy zgubili bagaz w trakcie lotu zalezy od linii lotniczej.
