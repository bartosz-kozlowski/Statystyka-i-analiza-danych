#Zad 5
#P(phat<=232/1000)
T=232
n=1000
p=0.25

pnorm(T/n,p,sqrt(p*(1-p)/n))


#zad 1
dane_est=read.csv("C:/Users/48663/Documents/laby statystyka/dane_est.csv", sep=";", dec=",")
diamenty = na.omit(dane_est$diamenty)

#populacja - wszystkie syntwtyczne diamenty wyprodukowane nową metodą
#próba - 12 syntetycznych diamentów wyprodukowanych nową metodą
#Badana zmienna - waga syntetycznych diamentow wyprodukowanych nowa metoda
n=12
#pkt b)
srednia=mean(diamenty)
var(diamenty)
odchylenie=sd(diamenty)
#pkt c)(FUNKCJA)
PrzedzialufnosciMU=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  Pz=srednia+qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  return(
    if(liczebnosc<30){
      if(sigma==FALSE){print(paste("(",Lt,":",Pt,")"))}
      else {print(paste("(",Lz,":",Pz,")"))}
    }
    else{print(paste("(",Lz,":",Pz,")"))})
}
PrzedzialufnosciMU(srednia,odchylenie,FALSE,12,0.95)
#z ufnoscia 0.95 przedzial(0.499;0.569) pokrywa rzeczywista nieznana srednia populacyjna mu
#lub tez
t.test(diamenty, conf.level=0.95)


Przedzialufnoscisig2=function(odchylenie, liczebnosc, ufnosc){
  alfa=1-ufnosc
  Lchi=(liczebnosc-1)*odchylenie^2/qchisq(1-alfa/2,liczebnosc-1)
  Pchi=(liczebnosc-1)*odchylenie^2/qchisq(alfa/2,liczebnosc-1)
  Lz=(liczebnosc-1)*odchylenie^2/(liczebnosc-1+qnorm(1-alfa/2)*sqrt(2*(liczebnosc-1)))
  Pz=(liczebnosc-1)*odchylenie^2/(liczebnosc-1-qnorm(1-alfa/2)*sqrt(2*(liczebnosc-1)))
  return(
    if(liczebnosc<30){print(paste("(",Lchi,":",Pchi,")"))}
    else{print(paste("(",Lz,":",Pz,")"))}
  )
}
Przedzialufnoscisig2(odchylenie, 12, 0.95)
#z ufnosci 0.95 przedzial (0.002; 0.009) pokrywa rzeczywista nieznana wariancje populacyjna sig^2
install.packages("TeachingDemos")
library(TeachingDemos)
chi=sigma.test(diamenty, conf.level=0.95)
ci=chi$conf.int
L=sqrt(ci[[1]])
P=sqrt(ci[[2]])
#Z ufnoscia 0.95 przedzial (0.039; 0.094)

#zad 3 lab 4
dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane_est.csv", sep=";", dec=",")
papierosy=na.omit(dane$papierosy)
srednia=mean(papierosy)
#a)
PrzedzialufnosciMU=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  Pz=srednia+qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  return(
    if(liczebnosc<30){
      if(sigma==FALSE){print(paste("(",Lt,":",Pt,")"))}
      else {print(paste("(",Lz,":",Pz,")"))}
    }
    else{print(paste("(",Lz,":",Pz,")"))})
}
PrzedzialufnosciMU(srednia,0.7,TRUE,15,0.95)
#z ufnoscia 95% przedzial (1.455, 2.164) pokrywa nieznana rzeczywista wartosc nikotyny we wszystkich
# papierosach
library(BSDA)
z.test(papierosy, sigma.x=0.7, conf.level=0.95)


#b)
PrzedzialufnosciMURet=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  Pz=srednia+qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  if(liczebnosc < 30) {
    if(!sigma) {
      return(c(Lt, Pt))
    } else {
      return(c(Lz, Pz))
    }
  } else {
    return(c(Lz, Pz))
  }
}

liczebnosc = 2  # Początkowa liczebność próby
for (liczebnosc in 2:100) {
  przedzial = PrzedzialufnosciMURet(srednia, 0.7, TRUE, liczebnosc, 0.95)
  if (przedzial[2] - przedzial[1] <= 0.3) {
    break
  }
}
liczebnosc

#Jak duża próbka jest potrzebna, aby długość 95% przedziału ufności była nie większa niż 0,3 mg?
kw=qnorm(1-0.05/2)
(2*kw*0.7/0.3)^2

#c)
odchylenie=sd(papierosy)
#to odchylenie jest mniejsze od podanego



#zad 4 4 lab 
dane=read.csv("C:/Users/48663/Documents/laby statystyka/dane_est.csv", sep=";", dec=",")
wodorosty=na.omit(dane$wodorosty)
#a)
srednia=mean(wodorosty)
wariancja=var(wodorosty)
odchylenie=sd(wodorosty)
#b)
PrzedzialufnosciMU=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  Pz=srednia+qnorm(1-alfa/2)*(odchylenie/sqrt(liczebnosc))
  return(
    if(liczebnosc<30){
      if(sigma==FALSE){print(paste("(",Lt,":",Pt,")"))}
      else {print(paste("(",Lz,":",Pz,")"))}
    }
    else{print(paste("(",Lz,":",Pz,")"))})
}
PrzedzialufnosciMU(srednia,odchylenie,FALSE,18,0.90)
#lub
t.test(wodorosty, conf.level=0.90)
#c)
Przedzialufnoscisig2=function(odchylenie, liczebnosc, ufnosc){
  alfa=1-ufnosc
  Lchi=(liczebnosc-1)*odchylenie^2/qchisq(1-alfa/2,liczebnosc-1)
  Pchi=(liczebnosc-1)*odchylenie^2/qchisq(alfa/2,liczebnosc-1)
  Lz=(liczebnosc-1)*odchylenie^2/(liczebnosc-1+qnorm(1-alfa/2)*sqrt(2*(liczebnosc-1)))
  Pz=(liczebnosc-1)*odchylenie^2/(liczebnosc-1-qnorm(1-alfa/2)*sqrt(2*(liczebnosc-1)))
  return(
    if(liczebnosc<30){print(paste("(",Lchi,":",Pchi,")"))}
    else{print(paste("(",Lz,":",Pz,")"))}
  )
}
Przedzialufnoscisig2(odchylenie, 18, 0.90)

#zad 2
pcb= c(16, 0, 0, 2, 3, 6, 8, 2, 5, 0, 12, 10, 5, 7, 2 , 3, 8, 17, 9, 1)
n=length(pcb)
PrzedzialUfnosciMU(mean(pcb),sd(pcb), FALSE, n, 0.95)
#z ufnoscia 95% (3.42, 8.18) pokrywa nieznana rzeczywista srednia poziomu pcb w mleku 
#wszystkich matek karmiacych piersia
mean(pcb)
var(pcb)
sd(pcb)
t.test(pcb, conf.level=0.95)
library(TeachingDemos)
sig=sigma.test(pcb, conf.level=0.95)
sig$conf.int
sqrt(sig$conf.int)
#z ufnoscia 95% przedzial (3,86; 7,43) pokrywa nieznana rzeczywista wartosc odchylenia standardowego zawartości pcb
# w mleku wszystkich matek karmiacych piersia

#zad 6
library(BSDA)
n=1200
sig=2.2
mu=4.7
zsum.test(mu, sig, n, conf.level=0.95)
#z ufnoscia 95% przedzial (4.57, 4.83) pokrywa nieznana prawdziwa srednia dlugosc trwania wszystkich
#polaczen telefonicznych 
sqrt((n-1)*sig^2/qchisq(1-alpha/2,n-1))
sqrt((n-1)*sig^2/qchisq(alpha/2,n-1))
#z ufnoscia 95% przedzial 2.11;2,3 pokrywa nieznana prawdziwa wartosc ochylenia standrardowego dlugosci trwania wszystkich
#polaczen tel

#zad 7
library(BSDA)
mu=102
n=365
sig=sqrt(81)
zsum.test(mu, sig, n, conf.level=0.98)
# z ufnoscia 98% przedzial (100.9; 103.1)
sqrt((n-1)*sig^2/qchisq(1-alpha/2,n-1))
sqrt((n-1)*sig^2/qchisq(alpha/2,n-1))
#nie, przedzial ufnosci jest mniejszy, ponadto dodajac odchylenie standardowe, tez to jest niemozliwe
# JEST TO NIEMOZLIWE!!!

#zad 8
var=28
sig=5
kw=((qnorm(1-0.05/2)*sig)/1)^2
ceiling(kw)

#zad 10
n=100
T=4
phat=T/n
alfa=0.05
L=phat-qnorm(1-alfa/2)*sqrt(phat*(1-phat))/sqrt(n)
P=phat+qnorm(1-alfa/2)*sqrt(phat*(1-phat))/sqrt(n)

# z ufnoscia 95% przedial 0.15%; 7.84 pokrywa nieznana rzeczywista proporcje wszystkich niedopelnionych puszek

binom.test(4, 100, conf.level = 0.95)
prop.test(4, 100, conf.level = 0.95)

#zad 11
n=120
T=24
binom.test(T, n, conf.level = 0.9)
#przedzial 0.141 do 0.27
# Z ufnością 90% przedział (14.1% ; 27.0%) pokrywa nieznaną prawdziwą proporcję
# monterów nieprzestrzegających BHP

#zad 12
#a phat 0.3
#b phat 0.5
# (a)
phat = 0.3
n = qnorm(1-0.02/2) * sqrt(phat*(1-phat)) / 0.05
n = n*n
# Należy zbadać 455 osób


# (b)
phat = 0.5
n = qnorm(1-0.02/2) * sqrt(phat*(1-phat)) / 0.05
n = n*n

# Należy zbadać 541 osób

#zad 5

avg = 4.7
sd = 2.2
n = 1200

# Średni czas trwania, ufność 95%
zsum.test(avg, sd, 1200, conf.level = 0.95)

# Z ufnością 95% przedział (4.57 ; 4.83) pokrywa nieznany prawdziwy średni czas
# trwania WSZYSTKICH połączeń

# Średnie odchylenie standardowe
L = sqrt((n-1)*sd*sd/qchisq(1-0.05/2, n-1))
R = sqrt((n-1)*sd*sd/qchisq(0.05/2, n-1))
L
R

# Z ufnością 95% przedział (2.115 ; 2.292) pokrywa nieznane prawdziwe odchylenie
# standardowe czasu trwania WSZYSTKICH połączeń

#zadanie 9 
sig = 0.3
conf = 0.90
alfa = 1 - conf
(sig*qnorm(alfa/2))^2

odchylenie = 0.3
alfa = 0.1
kw=qnorm(1-alfa/2)
(kw*odchylenie/0.1)^2

alfa2 = 0.01
kw2=qnorm(1-alfa2/2)
(kw2*odchylenie/0.1)^2


##

phat = 0.46
n = qnorm(1-0.05/2) * sqrt(phat*(1-phat)) / 0.03
n = n*n
n
