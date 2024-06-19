#zad1

#H0: rozkład częstotliwości emerytów, którzy wrócili do pracy w hrabstwie Allegheny
#odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reynolda
#H1: ~H0

#Emeryci: ZIO, S, F lub K, WF

observedf = c(122, 85, 76, 17) #częstość zaobserwowana
expectedp = c(0.38, 0.32, 0.23, 0.07) #oczekiwane prawdopodobieństwo

chisq.test(observedf, p=expectedp)
alfa = 0.1
# alfa = 0.1 < p-value = 0.3485 -> brak podstaw do odrzucenia H0
#Na poziomie istotności 10% dane nie potwierdzają hipotezy, że rozkład 
#częstotliwości emerytów, którzy wrócilli do pracy w hrabstwie Allegheny
#nie odpowiada ogólnemu rozkładowi podanemu przez stowarzyszenie Russela Reynolda

#zad2
#H0:  Odsetki poszczególnych rodzajów zgonów są zgodne z odsetkami przedstawionymi w artykule.
#H1: ~H0
#wypadki, zgony, samobójstwa
observedf =c(68, 27, 5)
expectedp = c(0.74, 0.16, 0.1)

chisq.test(observedf, p=expectedp)
alfa = 0.1
# alfa = 0.1 > p-value = 0.005 -> odrzucamy H0
# Na poziomie istotności 10% dane potwierdzaja hipotezę,
# że odsetki poszczególnych rodzajów zgonów nie są zgodne 
# z odsetkami przedstawionymi w artykule.

#zad3
#H0: rozkład smaków w cukierach Skittles wynosi 20%
#H1: ~H0

#Smaki: Cytrynowy, Limonkowy, Pomarańczowy, Truskawkowy, Winogronowy

observedf = c(43, 50, 44, 44, 52)
expectedp = c(0.2, 0.2, 0.2, 0.2, 0.2)

chisq.test(observedf, p = expectedp)
alfa=0.05
# alfa = 0.05 < p-value = 0.8369 -> brak podstaw do odrzucenia H0
# Na poziomie istotności 5% dane nie potwierdzaja hipotezy,
# że rozkład smaków cukerków Skittels nie wynosi 20%

#zad4
dane = read.csv("C:/Users/48663/Documents/laby statystyka/normalnosc_ozon.csv", sep = ";", dec = ",")
ozon=dane$ozon

#H0: stężenie ozonu ma rozkład normalny
#H1: stężenie ozonu NIE ma rozkładu normalnego
install.packages("nortest")
library(nortest)

alfa=0.05
pearson.test(ozon, adjust = FALSE)$p.value
#alfa=0.05 < p.value=0.2689 -> brak podstaw do odrzucenia H0
pearson.test(ozon, adjust = TRUE)$p.value
#alfa=0.05 < p.value=0.145 -> brak podstaw do odrzucenia H0
lillie.test(ozon)$p.value
#alfa=0.05 < p.value=0.277 -> brak podstaw do odrzucenia H0
shapiro.test(ozon)$p.value
#alfa=0.05 < p.value=0.109 -> brak podstaw do odrzucenia H0

#Na poziomie istotności 5% dane nie potwięrdzają hipotezy, 
#że stężenie ozonu NIE ma rozkładu normalnego.


#zad6
#H0: punkty uzyskane przez grupe maja rozklad normalny
#H1: ~H0
dane = read.csv("C:/Users/48663/Documents/laby statystyka/normalnosc_punkty.csv", sep = ";", dec = ",")
punkty = dane$punkty

alfa=0.01

pearson.test(punkty, adjust = FALSE)$p.value
#punkty uzyskane przezgrupe uczniów mają rozkład normalny
#H1: ~H0
pearson.test(punkty, adjust = TRUE)$p.value

lillie.test(punkty)$p.value

shapiro.test(punkty)$p.value
#alfa=0.01 > p.value = 0.00062 -> odrzucamy H0
#Na poziomie istotności 0.01 dane potwierdzają hipotezę że punkty uzyskane przez
#grupę uczniów nie mają rozkładu normalnego.

#zad7
#H0: poziom wykształcenia jest niezależny od miejsca zamieszkania
#H1: ~H0
miejski=c(15,12,8)
podmiejski=c(8,15,9)
wiejski=c(6,8,7)

TK=data.frame(miejski, podmiejski, wiejski)

chisq.test(TK)
alfa=0.05
#alfa = 0.05 < p-value = 0.5569 -> brak podstaw do odrzucenia H0
#Na poziomie istotności 5% dane nie potwierdzają hipotezy
#że poziom wykształcenia jest zależny od miejsca zamieszkania

#zad 8
#H0: Odsetek pasażerów, którzy zgubili bagaż, nie zależy od linii lotniczej.
#H1: ~H0
TAK=c(10, 7, 4)
NIE=c(90, 93, 96)

TN=data.frame(TAK, NIE)

chisq.test(TN)
alfa=0.05
#alfa = 0.05 < p-value = 0.251 -> brak podstaw do odrzucenia H0
#Na poziomie istotności 5% dane nie potwierdzają hipotezy
#że odsetek pasażerów, którzy zgubili bagaż, zależy od linii lotniczej.

#zad9
#H0: opinia nie zależy od wieku
#H1: opinia zależy od wieku
za = c(96,96,90,36)
przeciw = c(201,189,195,234)
nie_wiem = c(3,15,15,30)

TK=data.frame(za, przeciw, nie_wiem)
chisq.test(TK)
alfa=0.05
#alfa = 0.05 > p-value = 0 -> odrzucamy H0
#Na poziomie istotności 5% dane potwierdzają hipotezę
#że opinia zależy od wieku.



