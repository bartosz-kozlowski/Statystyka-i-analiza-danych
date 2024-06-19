##lab 1
#Zad 1 
sin(2*pi)
cos(3/4)
tan(pi)
log(100)
log(15, exp(1))
log(1/7, 7)
exp(1)^3
64^(1/3)

#Zad 2
wektor = seq(1, 10, 1)
sum(wektor)

#Zad 3
x = seq(2, 20, 2)
y = rev(x)
x*x
x^2
sqrt(sum(x^2)) ## euklides
t(x)%*%y
x%*%t(y)

#Zad 4
wektor4 = seq(5, 10, length = 13)

#Zad 5
z = c(1,2)
z1 = rep(z, times = 5)
z2 = rep(z, each = 5)
z1 = z1+4
z3 = z2[-c(length(z2))]
wektor_c = z1+z3
d = z1[z1>1]

#Zad 6
A=cbind(c(2,1,1),c(3,-1,1),c(0,2,-1))
A=rbind(c(2,3,0),c(1,-1,2),c(1,1,-1))
A^2
A%*%A
t(A)
det(A)
solve(A)
A[3,]

sum(diag(A)) ## slad macierzy!!!

#Zad 7
x = seq(1, 10, 1)
y = seq(2, 20, 2)
plot(x,y)
dane = data.frame(x,y)
plot(dane)
dane_2 = cbind(x,y) 
dane_3 = rbind(x,y)
plot(dane_3)

#Zad 8
curve(x^2+3*x-5, from=-3, to=4, col="orange")

##############lab 2

loty=read.csv("C:/Users/48663/Documents/laby statystyka/loty.csv", sep=";")
class(loty) ##DATA FRAME


nazwy = names(loty)
for (i in 1:6){
  print("średnia w roku")
  print(nazwy[i])
  print(mean(loty[,i]))
}


quantile(loty[,2])[1]
Q1=quantile(loty[,2])[2] #kwantyl 1 # W trzech miesiacach w roku 1956 byla mniejsza lub rowna 301 osob 
## i w 9 miesiacach liczba pasazerow byla wieksza lub rowna 301 osob
quantile(loty[,2])[3] # W 6 miesiacach liczba pasazerow pewnej lini lotniczej w roku 1956 byla 
#mniejsza lub rowna 315 i w pozostalcyh 6 miesiacach liczba pasazerow byla wieksza lub rowna 315 osob!!!
quantile(loty[,2])[4]
quantile(loty[,2])[5]


summary(loty[,2])

średnia=mean(loty[,2]) ##Srednia liczba pasazerow w roku 1956 wynosila 328 osob.
średnia
sd=sd(loty[,2]) ## przecietnie liczba pasazerow odchyla sie od sredniej o 48 osób
sd
wz=(sd/średnia)*100 ## wspolczynnik zmiennosci, slabe zroznicowanie liczby pasazerow w roku 1956
var(loty[,2])

min(loty)
max(loty)

przedzialy=seq(200, 650, length=10)
par(mfrow=c(2,3)) ## kilka wykresow na jednym okienku
kolory=c("red","green","orange", "pink", "yellow", "blue")


for (i in 1:6){
  hist(loty[,i], main=paste('loty w', nazwy[i]), xlab="liczba pasazerow", breaks=przedzialy, col=kolory[i])
}

boxplot(loty[,1], loty[,2],loty[,3],loty[,4],loty[,5],loty[,6])



#2 zadanie

oceny=read.csv("C:/Users/48663/Documents/laby statystyka/oceny.csv", sep=";", dec=",")

class(oceny)
apply(na.omit(oceny), 2, mean)
apply(na.omit(oceny), 2, quantile)
apply(na.omit(oceny), 2, sd)

install.packages("arm")
library(arm)

par(mfrow=c(2,2))
grupy=names(oceny)
for (j in 1:4){
  discrete.histogram(oceny[,j], freq=TRUE, main = paste('oceny', grupy[j]))
}

boxplot(oceny[,1], oceny[,2],oceny[,3],oceny[,4])

table(oceny[,1])
par(mfrow=c(2,2))
for (j in 1:4){
  title=paste("wykres kolowy", grupy[j])
  pie(table(oceny[,j]),main=title)
}

#zad 3

truskawki=read.csv("C:/Users/48663/Documents/laby statystyka/truskawki.csv", sep=";", dec=",")
class(truskawki)
summary(na.omit(truskawki$plon.2000))
plon2000=truskawki$plon.2000
plon2010=na.omit(truskawki$plon.2010)
print(plon2010)

tr=truskawki$plon.2000
table(cut(tr,breaks=4))

par(mfrow=c(2,1))
pie(table(tr))

lata=c(names(truskawki))
par(mfrow=c(1,2))
for (j in 1:2){
  title=paste0("histogram w roku", lata[j])
  hist(truskawki[,j], breaks=4, freq=FALSE, main=title)
}

#########lab 3

#zad 1 - rozklad dwumianowy bin(n,p)
#s|0|1|2|3|4|5
#p
n=5
p=0.3
s=c(0:5) #przypisujemy wartosci zmiennym losowym S
pr=dbinom(s,n,p) #gestosc a pbinom dystrybuanta
rbind(s,pr)
plot(s,pr, type = "h", lwd=4, xlab = "x", ylab = "f(x)", main = "Histogram prawdopodobieństwa Binomial(n,p)", col="magenta")
#P(S=3)
dbinom(3,n,p)
#P(S>=3)=P(S>2)
1-pbinom(2,n,p)
#P(S<3) = P(S<=2)
pbinom(2,n,p)

#zad 2
n=8
p=0.9
x=c(0:8)
pr=dbinom(x,n,p)
rbind(x,pr)
plot(x,pr, type = "h")
#PB=8
dbinom(8,n,p)
#PB>5
1-pbinom(5,n,p)
#E(B)
expect=sum(x*pr)
expect=n*p
#Przecietnie mozemy spodziewac sie ze 7 zarowek przekroczy zywotnosc 500 godzin.
SD=sqrt(n*p*(1-p))
#Możemy spodziewać się, że przeciętne odchylenie od średniej wynosi 1 żarówka.

#zad 3 rozklad wykladniczy
lambda=0.01
curve(dexp(x,lambda), 0, 1000)
#P(X>=200)=P(X>200)
1-pexp(200, lambda)
#P(X<100)=P(X<=100)
pexp(100,lambda)

#zad 4
#E(X)=2.4    lambda=1/E(X)
lambda=1/2.4
curve(dexp(x, lambda), 0, 50)
#P(X>3)
1-pexp(3,lambda)
#P(2<X<3)
pexp(3,lambda)-pexp(2,lambda)
f=function(x){x*dexp(x,lambda)}
integrate(f,0,Inf)

#Zad 5
mu=0.13
sig=0.005
curve(dnorm(x,mu,sig),0.1,0.16)
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig) ##99,7% znajduje sie w tej obserwacji
pnorm(0.14,mu,sig)-pnorm(0.12,mu,sig)

#Zad 8 rozklad dwumianowy przyblizony rozkladem normalnym // rozklad dokladny - dwumianowy
n=100
p=0.25
#P(X<15)
pbinom(15,n,p)
#Przyblizenie rozkladem normalnym
pnorm(15,n*p,sqrt(n*p*(1-p)))

#Zad 9
n=25
mu=200
sig=10
#P(199<X<202)
pnorm(202,mu, sig/sqrt(n))-pnorm(199,mu,sig/sqrt(n))

#T=X1+X2+...+X25, T ma rozklad N(n*mu, sqrt(n)*sig)
#P(T<=5100)
pnorm(5100, n*mu, sqrt(n)*sig)

#Zad 6
mu=120
sig=15
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
pnorm(135,mu,sig)-pnorm(111,mu,sig)

#Zad 7
mu = 46.8
sig = 1.75
#(X<=50) 
pnorm(50,mu,sig)
#X>=48
1-pnorm(48,mu,sig)

#Zad 10
mu=202
sig=14
n=64
#P(198<x<206)
pnorm(206,mu,sig/sqrt(n))-pnorm(198,mu,sig/sqrt(n))

#Zad 11
mu=0.5
sig=0.2
n=100
#P(X>=47)
1-pnorm(47, n*mu, sqrt(n)*sig)

######lab 4

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


###jakis test

x = seq(0,3)
parm = 0.6
size = 3
plot(x, dbinom(x, size, prob = prob), type = "h")

