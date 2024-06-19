dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_chemikalia.csv", sep=";", dec=",")
surowiec_x = dane$surowiec
produkt_y = dane$produkt

#a
plot(surowiec_x, produkt_y)

#b
cov(surowiec_x, produkt_y)
# S_XY=138.4889
# Kowariancja jest różna od zera, więc istnieje liniowa zależność
# między ilością zużytego surowca, a końcową wielkością produkcji środków chemicznych
# Ponieważ kowariancja jest dodatnia, zatem wraz ze wzrostem ilości
# zużytego surowca wzrasta końcowa wielkośc produkcji środków chemicznych.

#c
cor(surowiec_x, produkt_y)
#r_XY=0.8953468
# współczynnik korelacji r=0.895>|0.8| zatem
# istnieje bardzo silny związek liniowy między ilością zużytego surowca
# a koncowa produkcja srodkow chemicznych

#d
prosta=lm(produkt_y~surowiec_x)
prosta
#y = b0+b1x
#y = 22.41 + 3.62*x - równanie prostej regresji liniowej
#między wielkością produkcji a ilością zużytego surowca

summary(prosta)

#e
plot(surowiec_x, produkt_y); abline(prosta) #dodanie prostej regresji

#f
# Jeśli ilość surowca wzrosnie o 1 litr to końcowa wielkość
#produkcji środków chemicznych wzrośnie o 3.62 kg.
#(interpretacja współczynnika regresji liniowej)

#g, h
predict(prosta, data.frame(surowiec_x=c(20, 15)))
# Jeśli zużyjemy do produkcji 20 litrów surowca to końcowa wielkość
# produkcji wyniesie 94.79 kg,
# a jeśli zużyjemy do produkcji 15 litrów surowca to
#końcowa wielkośc produkcji wyniesie 76.69 kg

#i
summary(prosta)
#współczynnik determinacji R-squared=0.8016*100%=80,16%
#Prosta regresji liniowej jest dobrze dopasowana do danych
#Końcowa wielkość produkcji środków chemicznych jest wyjaśniona
#w ok 80% przez ilość zużytego surowca.

#j
#H0: b1=0 (Regresja liniowa jest nieistotna)
#H1: b1!=0 (Regresja liniowa jest istotna)

anova(prosta)
#1 sposób
alfa=0.05
n=length(surowiec_x)
qf(1-alfa,1,n-2)
# F=32.332 > F_t=5.317655 -> odrzucamy H0

#2 sposob
#alfa=0.05 > p-value=0.0004617 -> odrzucamy H0

#Na poziomie istotności alfa=0.05 dane potwierdzają hipotezę że regresja liniowa jest istotna


#zadanie2
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_urzadzenie.csv", sep=";", dec=",")
efektywnosc_x = dane$efektywnosc
zywotnosc_y = dane$zywotnosc

#a
plot(efektywnosc_x, zywotnosc_y)

#b
cov(efektywnosc_x, zywotnosc_y)
# Kowariancja jest różna od zera, więc istnieje liniowa zależność
# między efektywnością a żywotnością pewnego urządzenia.
# Ponieważ kowariancja jest ujemna, zatem wraz ze zmniejszeniem efektywności
# urządzenia wzrasta żywotność pewnego urządzenia.
#c
cor(efektywnosc_x, zywotnosc_y)
# współczynnik korelacji r=-0.909>|0.8| zatem
# istnieje bardzo silny związek liniowy między efektywnością, a żywotnością
# pewnego urządzenia

#d
prosta = lm(zywotnosc_y~efektywnosc_x)
prosta
#y = 18.88 - 0.86x - równanie prostej regresji liniowej
summary(prosta)
#e
# Żywotność urządzenia jeśli efektywność wzrośnie o 1 element, zmaleje 
# o 0.86 miesiąca
plot(efektywnosc_x, zywotnosc_y); abline(prosta)

predict(prosta, data.frame(efektywnosc_x=c(11, 19)))
# Żywotność urządzenia przy efektywności 11 elementów, to 9.4 miesiaca.
# Żywotność urządzenia przy efektywności 19 elementów, to 2.5 miesiaca.

#h
summary(prosta)
#współczynnik determinacji R-squared=0.827*100%=82,7%
#Prosta regresji liniowej jest dobrze dopasowana do danych
#Wartość żywotności pewnego urządzenia jest wyjaśniona
#w ok 82.7% przez efektywność urządzenia.

#i
#H0: b1=0 (Regresja liniowa jest nieistotna)
#H1: b1!=0 (Regresja liniowa jest istotna)
anova(prosta)
alfa=0.01
n=length(efektywnosc_x)
qf(1-alfa,1,n-2)
#1 sposob
# F=32.471 > F_t=12.24638 -> odrzucamy H0
#2 sposob
#alfa=0.01 > p-value=0.0006735 -> odrzucamy H0
#Na poziomie istotności alfa=0.01 dane potwierdzają hipotezę,
#że regresja liniowa jest istotna

#zadanie3
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Reg_arszenik.csv", sep=";", dec=",")
pH_x = dane$pH
arszenik_y = dane$arszenik

#a
plot(pH_x, arszenik_y)

#b
cov(pH_x, arszenik_y)
# Kowariancja (-18.32) jest różna od zera, więc istnieje liniowa zależność
# między zakwaszeniem gleby oraz procentem ilości usuniętego przez proces arszeniku.
# Ponieważ kowariancja jest ujemna, zatem wraz ze wzrostem pH gleby
# maleje procent ilości usuniętego przez proces arszeniku.
cor(pH_x, arszenik_y)
# współczynnik korelacji r=-0.9504953>|0.8| zatem
# istnieje bardzo silny związek liniowy między zakwaszeniem gleby 
#oraz procentem ilości usuniętego przez proces arszeniku.

#c
prosta = lm(arszenik_y~pH_x)
prosta
#y = 190.27 - 18.03x - równanie prostej regresji liniowej
summary(prosta)
#d
#Ilość usuniętego przez proces arszeniku jeśli pH gleby wzrośnie o 1 zmaleje o 18.03%.

#e,f
predict(prosta, data.frame(pH_x=c(7.5, 9)))
# Jeśli pH gleby wyniesie 7.5, 55% arszeniku zostanie usunięte.
# Jeśli pH gleby wyniesie 9, 28% arszeniku zostanie usunięte.

#g
summary(prosta)
#współczynnik determinacji R-squared=0.9034*100%=90.34%
#Prosta regresji liniowej jest dobrze dopasowana do danych
#Wartość procentowe ilości usuniętego przez proces arszeniku 
#jest wyjaśniona w ok 90.34% przez zakwaszenie gleby (pH).

#i
#H0: b1=0 (Regresja liniowa jest nieistotna)
#H1: b1!=0 (Regresja liniowa jest istotna)
anova(prosta)
alfa=0.01
n=length(pH_x)
qf(1-alfa,1,n-2)
#1 sposob
# F=149.7 > F_t=8.53 -> odrzucamy H0
#2 sposob
#alfa=0.01 > p-value = 0 -> odrzucamy H0
#Na poziomie istotności alfa=0.01 dane potwierdzają hipotezę,
#że regresja liniowa jest istotna