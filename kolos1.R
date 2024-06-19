#K1

#Zad 1
p0 = 0.85
T = 360
n = 400

#1. H0: p=85%    H1: p!=85%
#2. 
binom.test(T, n, p=p0, alternative = "two.sided")
#3. p-value = 0.004023
#4. alfa = 0.09 > p-value -> odrzucamy H0
#Na poziomie istotności alfa = 0.09 dane potwierdzają hipoteze, 
#że procent opakowań nadajaych się do recyklingu wynosi nie 85%.

#Zad 2

powietrze=read.csv("C:/Users/48663/Documents/laby statystyka/ANOVA_powietrze.csv", sep=";", dec=",")
powietrze

obiekty = rep(names(powietrze), c(length(na.omit(powietrze$Stare_Miasto)), length(na.omit(powietrze$Nowe_Miasto)), 
                                  length(na.omit(powietrze$Wilda)), length(na.omit(powietrze$Jezyce)), 
                                  length(na.omit(powietrze$Grunwald))))
wyniki = c(na.omit(powietrze$Stare_Miasto), na.omit(powietrze$Nowe_Miasto), na.omit(powietrze$Wilda),
           na.omit(powietrze$Jezyce), na.omit(powietrze$Grunwald))
#a
#H0: mu1=mu2=mu3=mu4=mu5 H1=~H0

anova(lm(wyniki~obiekty))
#alfa = 0.05 > p-value = 0 -> odrzucamy H0
#na poziomie istotnosci 5% odrzucamy H0
#stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi średnimi
#zawartościami dwutlenku siarki w dzielnicach Poznania.
#b
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Grupy jednorodne.
#SM-G

#Zad 3
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_odpady.csv", sep=";", dec=",")
odpady_x = dane$wytworzone
odpady_y = dane$wykorzystane

# a)
cor(odpady_x, odpady_y) 
# współczynnik korelacji r = |0.9235867| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między wytworzonymi odpadami, a wykorzystanymi wtórnie.

#b)
prosta = lm(odpady_y ~ odpady_x)
prosta
# y = -193.718 + 2.145*x
# równanie regresji liniowej między wytworzonymi odpadami, a wykorzystanymi wtórnie.
plot(odpady_x, odpady_y); abline(prosta) #dodanie prostej regresji

#c)
predict(prosta, data.frame(odpady_x=125))
# Jeżeli liczba wytworzonych odpadów wyniesie 125 mln ton, to wykorzystanych wynisie 74.9 mln ton.

#Zad 4

#H0: rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat 
#rozkłada się jak opisano w artykule.
#H1: ~H0
#Możliwości: W, Z, S
observedf = c(67, 28, 6) #zaobserwowana częstość
expectedp = c(0.74, 0.16, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.09 > 0.003809 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.1 dane potwierdzają hipoteze, że 
#rozkład częstotliwości liczby zgonów związanych z bronią palną wśród osób w 
# wieku od 1 do 18 lat NIE rozkłada się jak opisano w artykule.
