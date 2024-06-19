#Zad 1
#1
#H0: p<=60
#H1: p>60
#2
T=140
n=200
binom.test(T, n, p = 0.6, alternative="greater")
#3
#p-value = 0.00213
#4
# p-value = 0.00213 < alfa = 0.09 -> odrzucamy H0
#5 Na poziomie istotności 9%, dane potwierdzają hipotezę, 
#że więcej niż 60% Polakow nie wie co to termin GMO.

#Zad 2
magnez=read.csv("C:/Users/48663/Documents/laby statystyka/ANOVA_magnez.csv", sep=";")

#a
obiekty=rep(names(magnez), each=length(magnez$Muszynianka))
obiekty

wyniki=c(na.omit(magnez$Muszynianka),
         na.omit(magnez$Muszynianka_plus),
         na.omit(magnez$Piwniczanka),
         na.omit(magnez$Galicjanka)
)

#H0: mu1=mu2=mu3=mu4 H1=~H0

anova(lm(wyniki~obiekty))
#alfa = 0.03 > p-value = 0 -> odrzucamy H0
#na poziomie istotnosci 3% odrzucamy H0
#stwierdzamy zatem, że sa istotne roznice miedzy rzeczywistymi srednimi
#zawartosciami magnezu w wodach mineralnych czterech rodzajow.
#b
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Grupy jednorodne.
#P-G

#Zad 3
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_lody.csv", sep=";", dec=",")
cena_x = dane$cena
lody_y = dane$sprzedaz

cena_x = c(4, 4.3, 4.5, 5, 5, 5.5)
lody_y = c(80, 73, 70, 61, 60, 50)

# a)
cor(cena_x, lody_y) 
# współczynnik korelacji r = |-0.998| > 0.8, zatem
# istnieje bardzo silna zależność liniowa między ceną lodów, a ilością sprzedanych sztuk.

#b)
prosta = lm(lody_y ~ cena_x) 
prosta
# y = 157.81 + -19.54*x
# równanie regresji liniowej między ceną lodów, a ilością sprzedanych sztuk.

plot(cena_x, lody_y); abline(prosta) #dodanie prostej regresji

#c)
predict(prosta, data.frame(cena_x = 5.2))
# Jeżeli cena lodów wyniesie 5.2 zł, to liczba sztuk sprzedanych lodów
# w ciągu godziny wyniesie 56.


Zad4.
#H0: odsetek poszczegółnych kobiet o różnych włosach rozkłada się jak opisano w artykule.
#H1: ~H0
#Możliwości: BR, SZ, BL, IN
observedf = c(15, 12, 20, 13) #zaobserwowana częstość
expectedp = c(0.38, 0.32, 0.20, 0.10) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.09 > 0.0002911 = p_value  ->  odrzucamy H0
# Na poziomie istotności 0.05 dane potwierdzają hipoteze, 
#że rozkład częstotliwości poszczególnych kobiet różni się od przedstawionego w artykule.



