cisnienie=read.csv("C:/Users/48663/Documents/laby statystyka/Anova_cisnienie.csv", sep=";")
#zad 1
obiekty = rep(names(cisnienie),  each=length(cisnienie$Niskie))
wyniki = c(na.omit(cisnienie$Niskie), na.omit(cisnienie$Srednie), na.omit(cisnienie$Silne), na.omit(cisnienie$BardzoSilne))
cisnienieTest=data.frame(obiekty, wyniki)
#srednie probkowe
srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty),mean)
srednie
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2    H1: ~H0
#H0  wariancje są jednorodne   H1: ~H0
bartlett.test(wyniki~obiekty, cisnienieTest)
alfa = 0.05
#alfa = 0.05 < p-value = 0.5009 -> zatem brak podstaw do odrzucenia H0
#Na poziomie istotności 5% nie mamy podstaw do odrzucenia H0.
#Zatem zakladamy ze wariancje są jednorodne i możemy przeprowadzić ANOVE.

#H0: mu1=mu2=mu3=mu4  H1: ~H0
anova(lm(wyniki~obiekty))

n=40
k=4
#1 sposob F = 2.2665 < F_t = 2.866266 -> brak podstaw do odrzucenia H0
qf(1-alfa, k-1, n-k)
#2 sposob: alfa = 0.05 < p-value = 0.09735 -> brak podstaw do odrzucenia H0
#Na poziomie istotnosci 5% nie mamy podstaw do odrzucenia H0 zatem 
#cisnienie nie ma wplywu na wielkosc produkcji.

#zad 3
dane = read.csv("C:/Users/48663/Documents/laby statystyka/Anova_mikrometr.csv", sep=";",dec=",");
dane
obiekty = rep(names(dane), c(length(na.omit(dane$mikrometrI)), length(na.omit(dane$mikrometrII)), length(na.omit(dane$mikrometrIII))))
wyniki = c(na.omit(dane$mikrometrI), na.omit(dane$mikrometrII), na.omit(dane$mikrometrIII))
mikroTest = data.frame(obiekty,wyniki)

#H0: mu1=mu2=mu3   H1: ~H0
anova(lm(wyniki~obiekty))
#alfa = 0.05 < p-value = 0.06859 -> brak podstaw do odrzcuenia H0
#Na poziomie istotnosci 5% nie mamy podstaw do odrzcuenia H0 zatem wybor mikrometru
#nie ma wplywu na uzyskane wyniki.
bartlett.test(wyniki~obiekty)
srednie = sapply(split(mikroTest$wyniki, mikroTest$obiekty), mean)

#zadanie 4
sportowcy=read.csv("C:/Users/48663/Documents/laby statystyka/Anova_sportowcy.csv",sep=";", dec=",")
obiekty=rep(names(sportowcy), each=length(sportowcy$Niepalacy))
obiekty

wyniki=c(na.omit(sportowcy$Niepalacy),
         na.omit(sportowcy$Lekkopalacy),
         na.omit(sportowcy$Sredniopalacy),
         na.omit(sportowcy$Duzopalacy)
)

sportowcytest=data.frame(obiekty,wyniki)
sportowcytest
#srednie probkowe
srednie=sapply(split(sportowcytest$wyniki, sportowcytest$obiekty), mean)
srednie
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2    H1: ~H0
#H0: czy wariancje sa jednorodne H1=~H0
bartlett.test(wyniki~obiekty, sportowcytest)
alfa=0.01
#alfa = 0.01 < p-value = 0.8517 -> brak podstaw do odrzucenia h0
#na poziomie istotnosci 1% nie mamy podstaw do odrzucenia h0
#zatem zakladamy jednorodnosc wariancji i mozemy przeprowadzic ANOVE.

#H0: mu1=mu2=mu3=mu4 H1=~H0
anova(lm(wyniki~obiekty))
#alfa = 0.01 > p-value = 0.003979 -> odrzucamy H0
#na poziomie istotnosci 1% odrzucamy H0
#stwierdzamy zatem, że palenie papierosow moze wplywac na rytm zatokowy serca
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#Lekkopalacy-Duzopalacy, Niepalacy-Duzopalacy, Sredniopalacy-Duzopalacy, 
#Sredniopalacy-Niepalacy.

#Grupy jednorodne:
# (L-D), (N-D), (Ś-D), (Ś-N)
#(N-D), (Ś-D), (Ś-N) -> grupa jednorodna (N-Ś-D)
#dwie grupy jednorodne:
#L-D, N-Ś-D
plot(TukeyHSD(aov(wyniki~obiekty)))

#zadanie 5
chomiki=read.csv("C:/Users/48663/Documents/laby statystyka/Anova_chomiki.csv",sep=";", dec=",")
obiekty=rep(names(chomiki), c(length(na.omit(chomiki$I)), length(na.omit(chomiki$II)), length(na.omit(chomiki$III)), length(na.omit(chomiki$IV))))
obiekty

wyniki=c(na.omit(chomiki$I),
         na.omit(chomiki$II),
         na.omit(chomiki$III),
         na.omit(chomiki$IV)
)
wyniki
chomikitest=data.frame(obiekty,wyniki)
chomikitest
#srednie probkowe
srednie=sapply(split(chomikitest$wyniki, chomikitest$obiekty), mean)
srednie
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2    H1: ~H0
#H0: czy wariancje sa jednorodne H1=~H0
bartlett.test(wyniki~obiekty, chomikitest)
alfa=0.05
#alfa = 0.05 < p-value = 0.2139 -> brak podstaw do odrzucenia h0
#na poziomie istotnosci 1% nie mamy podstaw do odrzucenia h0
#zatem zakladamy jednorodnosc wariancji i mozemy przeprowadzic ANOVE.

#H0: mu1=mu2=mu3=mu4 H1=~H0
anova(lm(wyniki~obiekty))
#alfa = 0.05 > p-value = 0.02398 -> odrzucamy H0
#na poziomie istotnosci 5% odrzucamy H0
#stwierdzamy zatem, że słuszne jest przypuszczenie, że masa gruczołu
#tarczycowego zależy od poziomu inbredu. 
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#II-I, III-I, III-II, IV-II, IV-III
#Grupy jednorodne:
#II-I, III-I, III-II, IV-II, IV-III
#II-I, II-III, I-III -> grupa jednorodna (I-II-III)
#IV-II, IV-III, II-III -> grupa jednorodna(II-III-IV)
#dwie grupy jednorodne:
#(I-II-III), (II-III-IV)

#zad 2
kopalnie=read.csv("C:/Users/48663/Documents/laby statystyka/Anova_kopalnie.csv", sep=";", dec=",")
kopalnie
obiekty = rep(names(kopalnie), c(length(na.omit(kopalnie$K1)),length(na.omit(kopalnie$K2)),
length(na.omit(kopalnie$K3)), length(na.omit(kopalnie$K4)), length(na.omit(kopalnie$K5))))
wyniki = c(na.omit(kopalnie$K1), na.omit(kopalnie$K2), na.omit(kopalnie$K3), na.omit(kopalnie$K4),
           na.omit(kopalnie$K5))
kopalnieTest=data.frame(obiekty, wyniki)
#srednie probkowe
srednie = sapply(split(kopalnieTest$wyniki, kopalnieTest$obiekty),mean)
srednie
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2    H1: ~H0
#H0  wariancje są jednorodne   H1: ~H0
bartlett.test(wyniki~obiekty, kopalnieTest)
alfa = 0.01
#alfa = 0.01 < p-value = 0.03 -> zatem brak podstaw do odrzucenia H0
#Na poziomie istotności 1% nie mamy podstaw do odrzucenia H0.
#Zatem zakladamy ze wariancje są jednorodne i możemy przeprowadzić ANOVE.

#H0: mu1=mu2=mu3=mu4  H1: ~H0
anova(lm(wyniki~obiekty))

n=20
k=5
#1 sposob F = 0.9563 < F_t = 4.89 -> brak podstaw do odrzucenia H0
qf(1-alfa, k-1, n-k)
#2 sposob: alfa = 0.01 < p-value = 0.4594 -> brak podstaw do odrzucenia H0
#Na poziomie istotnosci 1% nie mamy podstaw do odrzucenia H0 zatem 
#średnich zawartości popiołu dla ekogroszku produkowanego w pięciu kopalniach 
#nie można uznać za jednakowe

#zad 6
pulapki=read.csv("C:/Users/48663/Documents/laby statystyka/Anova_pulapki.csv",sep=";", dec=",")
pulapki
obiekty=rep(names(pulapki), each=length(pulapki$rozsiany))
obiekty

wyniki=c(na.omit(pulapki$rozsiany),
         na.omit(pulapki$skoncentrowany),
         na.omit(pulapki$roslina.zywicielka),
         na.omit(pulapki$powietrzny), na.omit(pulapki$gruntowy)
)

pulapkitest=data.frame(obiekty,wyniki)
pulapkitest
#srednie probkowe
srednie=sapply(split(pulapkitest$wyniki, pulapkitest$obiekty), mean)
srednie
#H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2    H1: ~H0
#H0: czy wariancje sa jednorodne H1=~H0
bartlett.test(wyniki~obiekty, pulapkitest)
alfa=0.05
#alfa = 0.05 < p-value = 0.06804 -> brak podstaw do odrzucenia h0
#na poziomie istotnosci 1% nie mamy podstaw do odrzucenia h0
#zatem zakladamy jednorodnosc wariancji i mozemy przeprowadzic ANOVE.

#H0: mu1=mu2=mu3=mu4 H1=~H0
anova(lm(wyniki~obiekty))
#alfa = 0.05 > p-value = 0 -> odrzucamy H0
#na poziomie istotnosci 5% odrzucamy H0
#stwierdzamy zatem, strategia lokalizacji może mieć wpływ na liczbę uwięzionych 
#ciem cygańskich.
#Chcemy sprawdzic ktore grupy sa do siebie podobne (nie roznia sie istotnie).
TukeyHSD(aov(wyniki~obiekty))
#Grupy ktore nie roznia sie miedzy soba istotnie:
#r-g, r.z-p, s-p, s-r.z

#Grupy jednorodne:
#r-g, r.z-p, s-p, s-r.z
#r.z-p, s-r.z, s-p -> grupa jednorodna (r.z-p-s)
#dwie grupy jednorodne:
#(r.z-p-s), (r-g)
plot(TukeyHSD(aov(wyniki~obiekty)))
