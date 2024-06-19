dane = read.csv("dane_est.csv", sep = ";", dec = ",")
diamenty= na.omit(dane$diamenty)
diamenty
#zadanie 1 
#populacja próba badana zmienna 
# populacja -  wszystkie diamenty wyprodukowane metodą syntetyczną
# próba - diamenty któe wzięły udziałw badaniu, 12 syntetycznych diamentów 
# badana zmienna -  wga syntetycznych diamnetów wybrodukowanych nową metodą  

proba = length(diamenty)
srednia = mean(diamenty) #srednia punktowa
wariancja = var(diamenty)
odchylenie = sd(diamenty)
sqrt(var(diamenty))

#przedział ufności, srednia, wielkosc proby,ufnosc, odchylenie standardowe, sigma
przedzial_ufn = function(srednia, wielkosc, ufnosc, odchylenie, sigma) 
{  
    alfa = 1 - ufnosc  
    up = 0
    down = 0
    ans = c(0,0)
    if (sigma == -1) {
    if (proba >=30){
      ans[1] = srednia - qnorm(1-alfa/2) * odchylenie/sqrt(wielkosc)
      ans[2] = srednia + qnorm(1-alfa/2) * odchylenie/sqrt(wielkosc)
    }else{
      ans[1] = srednia - qnorm(1-alfa/2,  wielkosc-1) * odchylenie/sqrt(wielkosc)
      ans[2] = srednia + qnorm(1-alfa/2, wielkosc-1) * odchylenie/sqrt(wielkosc)
    }
  }else{
    #znana sigma 
      ans[1] =  srednia - qt(1-alfa/2, wielkosc - 1 )*odchylenie/sqrt(wielkosc)
      ans[2] =  srednia + qt(1-alfa/2, wielkosc - 1)*odchylenie/sqrt(wielkosc)
  }
    
  return (ans)
} 

przedzial = przedzial_ufn(srednia, proba, 0.95, odchylenie, -1)
przedzial


przedzial2 = przedzial_ufn(srednia, proba, 0.95, odchylenie, 1)
przedzial2
#z ufnoscia 95% przedział pokrywa nieznaną prawdziwą średnią populacyjną mu (mi )

przedzial_wbudowane = t.test(diamenty, conf.level = 0.95 )
przedzial_wbudowane


#podpunkt e) 
wariancja_fun = function(wielkosc, odchylenie, ufnosc){
  ans = c(0,0)
  alfa = 1 - ufnosc
  if(wielkosc<30){
    ans[1] = ((wielkosc - 1) * odchylenie^2) / qchisq(1-alfa/2, wielkosc-1)
    ans[2] = ((wielkosc - 1) * odchylenie^2) / qchisq(alfa/2, wielkosc-1)
  }else{
    ans[1] = ((wielkosc - 1) * odchylenie^2) / (wielkosc - 1 + qnorm(1-alfa/2)*sqrt(2*wielkosc-1))
    ans[2] = ((wielkosc - 1) * odchylenie^2) / (wielkosc - 1 - qnorm(alfa/2)*sqrt(2*wielkosc-1))  
  }
  return (ans)
}
przedzial_w = wariancja_fun(proba, odchylenie, 0.95)
przedzial_w
#z ufnoscia 0.002 0.009 przedzial pokrywa nieznana prawdziwa wartosc wariancji dla populacji

chi = sigma.test(diamenty, conf.level = 0.95)
ci = chi$conf.int
L = ci[[1]]
P = ci[[2]]
l_od = sqrt(L)
p_od = sqrt(P)
l_od
p_od
#2 3 4 dom!!!!
#zadanie 2 
#populacja - wszystkie kobiety karmiace piersia
#probka -  20  wybranych kobiet ktore karmia piersia
#badana zmienna - poziom pcb
mleko = na.omit(dane$mleko)
proba = length(mleko)
srednia = mean(mleko)
wariancja = var(mleko)
odchylenie = sqrt(wariancja)

#przedzial_ufn(mean(mleko), sd(pcb), False, n ,0.95) 
#przedzial ufnosci zawsze lewy kraniec w dol prawy w gore 

przedzial_wbudowane = t.test(mleko, conf.level = 0.95 )
przedzial_wbudowane
#3.42, 8.18
# z ufnoscia 95% przedzial ten pokrywa nieznana srednia ilosci pcb w mleku wszystkich matek karmiacych piersia
#przedzial ufnosci i dla wszystkich matek 
przedzial_w = wariancja_fun(proba, odchylenie, 0.95)
przedzial_w

sd = sigma.test(mleko, conf.level=0.95)
sqrt(sd$conf.int)
#z ufnoscia 95% przedzial (3.86, 7,43) pokrywa nieznana prawdziwa wartość odchylennia standardowego ilosci pcb w mleku wszystkich matek karmiacych piersia 
#D) sigma test 
sredni = sigma.test(mleko, conf.level = 0.95)
sqrt(przedzial_w[1])
sqrt(przedzial_w[2])

#zadanie 3 
papierosy = na.omit(dane$papierosy)
proba = length(papierosy)
srednia = mean(papierosy)
wariancja = var(papierosy)
odchylenie = sqrt(wariancja)


przedzial_wbudowane = t.test(mleko, conf.level = 0.95 )
przedzial_wbudowane

przedzial_w = wariancja_fun(proba, odchylenie, 0.95)
przedzial_w

sqrt(przedzial_w[1])
sqrt(przedzial_w[2])

sigma = 0.7
#przedzialUFnosciMu(mean(cig), 0.7, true, n, 0.95)
# z ufnoscia 95% przedzial 1,45 2,17 pokrywa rzeczywista nieznana srednia zawartosc nikotyny we wszystkich papierosach
przedzial = z.test(papierosy, sigma, x=0.7 , conf.level = 0.95)
przedzial

#b)
kw = qnorm(1-0.05/2)
(2*kw*0.7/0.3)^2
#n = 84

sd(papierosy)
#zadanie 4 
wodorosty = na.omit(dane$wodorosty)
proba = length(wodorosty)
proba
srednia = mean(wodorosty)
srednia
wariancja = var(wodorosty)
wariancja 

przedzial_wbudowane = t.test(wodorosty, conf.level = 0.90 )
przedzial_wbudowane

przedzial_w = sigma.test(wodorosty, odchylenie, conf.level =0.90)
przedzial_w

#zadanie 5
sig = 3 
mean = 0 
mi

#zadanie 6 
polaczenia = na.omit(dane$sygnal)
mu = 4.7
n = 1200 
sig = 2.2 
alpha = 0.05 #1-conf
#wzor 17 
p1=zsum.test(mu, sig, n, conf.level=0.95)
#z ufnoscia 95$ przedzial ... pokrywa rzeczywista, nieznana srednia dlugosc trwania wszystkich takich polaczen 
sqrt(p1$conf.int)
sqrt((n-1)*sig^2/qchisq(1-alpha/2,n-1))
sqrt((n-1)*sig^2/qchisq(alpha/2,n-1))

#zadanie 7
n = 365 
mu = 102
var = 81
sig = sqrt(var)
conf = 0.98 
alpha = 1 - conf
zsum.test(mu, sig, n, conf.level=0.98)
# z ufnoscia 98%  przedizal... pokrywa prawdziwe, nieznana srednie zuzycie wody w fabryce

#nie ponieważ, przedzial ufnosci jest znaczenie wezszy i z 98% ufnoscia to sie nie wydazy 

#zadanie 8 
var = 25 
#n=? 
conf = 0.95
alfa = 1 - conf 
sigma = 5
(sigma*qnorm(1-alfa/2))^2
# n = 97 

#zadanie 9 
sig = 0.3
conf = 0.99
alfa = 1 - conf
(sig*qnorm(alfa/2))^2


#zadanie 10
T =  4 
n = 100 
binom.test(T, n, conf.level = 1 - 0.95)
prop.test(T, n, conf.level = 1 - 0.95)
#wzór - inne wyniki  

#zadanie 11 
n = 120 
T = 24 
conf = 0.9 
binom.test(T, n,conf.level=0.9)
prop.test(T, n, conf.level =0.9)

#zadanie 12 
conf = 0.98 
#A) 0,03

#kolokwium całe zajęcia 
