
#WCZYTYWANIE:
dane = read.csv("C:/Users/adamp/Documents/Studia/Semestr 4/Statystyka/Laby/Do Kolosa 1/Kolosy rozw/dane.csv", sep=';', dec = ",")
#BIBLIOTEKI:
library("BSDA")
library("TeachingDemos")

#1 W pliku dane.csv, zmienna czas, znadują sie dane dotyczące czasu obsługi przy kasie sklepowej
# 50 losowo wybranych klientow(w s).
# a) Oblicz i zinterpretuj kwantyl trzeci i odchylenie standardowe czasu obsługi przy kasie 
#sklepowej
# b) Skonstruuj szereg rozdzielczy przedziałowy skladający się z 5 przedziałow, przy czym
# pierwszy przedział zaczyna sie od 10s, a ostatni kończy się na 60s
# c) Narysuj wykres kołowy czasu zgodnie z szeregiem rozdzielczym skonstruowanym w punkcie (b).
czas=na.omit(dane$czas)
#A
quantile(czas)[4]
#     W przypadku 75% z 50 klientów, (37 czy 38??)  czas obsługi był mniejszy lub równy 40s,
#     w przypadku 25% z 50 klientów czas obsługi był większy lub równy 40s.
#B
przedzial = seq(10, 60, length = 6)
przedzial
szereg = table(cut(czas, przedzial))
szereg
#C
pie(szereg)


#2 Pewien portier dużej uczelni publicznej dojeżdza samochodem do pracy i często się do niej spóźnia
# z powodu dużego porannego natężenia ruchu. Niech Y będzie zmienną losową zliczającą liczbe dni
# w których portier się spóźni. Jeśli prawdopodobieństwo spóźnienia się dowolnego dnia wynosi 0,3 to
# jakie jest prawdopodobieństwo, że w ciągu dwóch tygodni (10 dni roboczych) spóźni się nie więcej
# niż 7 razy? Podaj rozkład prawdopodobieństwa Y (przedstaw go w formie tabeli). Wyznacz i zinterpretuj
# wartość oczekiwaną zmiennej losowej Y.
p = 0.3
n = 10
wek = c(0:n)
#P(Y<=7):
pbinom(7, n, p)    
#po zaokrągleniu do 4 miejsc po przecinku
pr = round(dbinom(wek, n, p),4)
rbind(wek, pr)

expect = sum(pr*wek)
expect
#przeciętnie możemy spodziewać się, że w ciągu 10 dni roboczych portier spóźni się 3 razy.

#3 Maksymalne ciśnienia mieszanki betonu (kN/m^2) mające wpływ na wielkość dopuszczalnych obciążeń
# deskowań ścian i słupów w budownictwie jest zmienną losową o rozkladzie normalnym ze średnią 42 kN/m^2
# i odchyleniem standardowym 3 kN/m^2. Zmierzono maksymalne ciśnienie w próbie 50 mieszanek betonu.
# a) Podaj rozkład średniego maksymalnego ciśnienia 50 próbek mieszanek betonu
# b) Oblicz prawdopodobieństwo, że średnie maksymalne ciśnienie w próbie 50 mieszanek betonu
# mieści się w przedziale od 35 kN/m^2 do 40 kN/m^2
mu = 42
sig = 3
n=50
#A
#Rozkład średniego maksymalnego ciśnienia 50 próbek mieszanek betonu: avr~N(mu, sigma/sqrt(N))
#B
#P(35<avr<40)
pnorm(40,mu,sig/sqrt(n)) - pnorm(35,mu,sig/sqrt(n))

#4 Zmierzono zawartość witaminy C (w %) w losowo wybranoych 45 butelkach pewnego soku pomarańczowego.
# Wszystkie obserwacje znajdujące sie w pliku dance.csv, zmienna witaminaC. Zakładając normalność
# badanej zmiennej, wyznacz 97% przedział ufności dla prawdziwej średniej zawartości witaminy C
# we wszystkich sokach pomarańczowych. Podaj interpretację wyniku. Jak duża powinna być próba,
# żeby mieć 97% pewność, że błąd estymacji wynosi 0,2%?
witaminac=na.omit(dane$witaminaC)
witaminac
ufnosc=0.97
alfa=1-ufnosc
t.test(witaminac, conf.level=1-alfa)
#Z ufnością 97% możemy powiedzieć, że przedział od 26.16%  do
# 28.60% pokrywa nieznaną prawdziwą średnią zawartość witaminy C we wszystkich
# sokach pomarańczowych.

blad = 0.2
n = 45
wielk_proby = ceiling((qt(1-alfa/2, n - 1)*sd(witaminac)/blad)^2)
wielk_proby
#Próba powinna wynosić 1660, żeby mieć 97% pewności, że błąd estymacji wynosi 0.2%.
