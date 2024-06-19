#Bartosz Kozłowski 155869

#Zad 1
p0 = 0.2
T = 60
n = 200
#1. H0: p<=20%    H1: p>20%
#2. 
binom.test(T, n, p=p0, alternative = "greater")
#3. p-value = 0.0004977
#4. alfa = 0.09 > p-value = 0.0004977-> odrzucamy H0
#5. Na poziomie istotności alfa = 0.09 dane potwierdzają hipotezę, 
#że wadliwość regenerowanej turbosprężarki jest większa od 20%.

#Zad 2
energia=read.csv("C:/Users/48663/Documents/laby statystyka/ANOVA_energia.csv", sep=";", dec=",")
energia

obiekty = rep(names(energia), c(length(na.omit(energia$Opolskie)), length(na.omit(energia$Podlaskie)), 
                                  length(na.omit(energia$Lubuskie)), length(na.omit(energia$Malopolskie))))
wyniki = c(na.omit(energia$Opolskie), na.omit(energia$Podlaskie), na.omit(energia$Lubuskie),
           na.omit(energia$Malopolskie))

#Z polecenia zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.
#a
#H0: mu1=mu2=mu3=mu4 H1=~H0
anova(lm(wyniki~obiekty))
#alfa = 0.05 > p-value = 0.001694 -> odrzucamy H0
#Na poziomie istotnosci 5% odrzucamy H0. Stwierdzamy zatem, że 
#istnieją istotne różnice w przeciętnym udziale odnawialnych źródeł energii
#w tych województwach.

#b
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Grupy jednorodne -> Opolskie-Lubuskie, Podlaskie-Lubuskie, Opolskie-Malopolskie

#Zad 3
tlen = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_tlen.csv", sep=";", dec=",")
temperatura_x = tlen$temperatura
zaw_tlenu_y = tlen$tlen

# a)
cor(temperatura_x, zaw_tlenu_y) 
# współczynnik korelacji r = |-0.8675324| > 0.8
# Zatem istnieje bardzo silna zależność liniowa między temperaturą,
# a zawartością tlenu rozpuszczonego w wodzie destylowanej.

#b)
prosta = lm(zaw_tlenu_y ~ temperatura_x)
prosta
# y = b0+b1*x
# y = 14.504 - 0.251*x
# równanie regresji liniowej między zawartością tlenu rozpuszczonego w wodzie destylowanej,
# a temperaturą,
plot(temperatura_x, zaw_tlenu_y); abline(prosta) #dodanie prostej regresji

#c)
predict(prosta, data.frame(temperatura_x=10))
# Jeżeli temperatura wyniesie 10 stopni C, to przewidywana zawartość tlenu będzie
# wynosić 12 mgO2/dm3.

#Zad 4
#H0: zbiór danych dostarcza dowód na to, że wypadki są dwa razy bardziej 
#prawdopodobne w poniedziałki niż w inne dni
#H1: ~H0
#Możliwości: PON, WT, SR, CZW
observedf = c(73, 43, 48, 41) #zaobserwowana częstość
expectedp = c(0.4, 0.2, 0.2, 0.2) #oczekiwane prawdopodobieństwo
chisq.test(observedf, p=expectedp)
# alpha = 0.09 < 0.5163 = p-value  ->  nie odrzucamy H0
# Na poziomie istotności alfa = 0.09 dane potwierdzają hipotezę, że 
# zbiór danych dostarcza dowód na to, że wypadki są dwa razy bardziej prawdopodobne 
# w poniedziałki niż w inne dni.
