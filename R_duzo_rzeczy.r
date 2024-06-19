sin(2*pi)
cos(3/4)
log(100, base=10)
e^3
exp(1)^3
c=seq(1,10,by=1)
sum(c)
a=seq(2,20,by=2)
length(a)
a2=rev(a)
a*a
a^2
sqrt(a^2+a^2)
t(a)*a2
t(a)
t(a)%*%a2
a%*%t(a2)  
t(a)%*%a2
c2=seq(5,10,length=13)
z1=rep(z,times=5)
g=c(1,2)
g1=rep(g,times=5)
g2=rep(g,each=5)
g1+4
g3=g2[-c(10)]
g4=g1+g3
g4=z1[z1>1]
a1=c(2,3,0)
a=rbind(a1,a2,a3)
a^2
a%*%a
tr=a[[1,1]]+a[[2,2]]+a[[3,3]]
x=c(1,4,3,7,2,8,9,2,3,5)
y=c(6,3,7,8,9,1,2,3,5,4)
sum(diag(a))
x1=data.frame(x,y)
plot(x,y)
plot(x1)
x2=rbind(x,y)
plot(x2)
x3=cbind(x,y)
plot(x3)
f=seq(-3,4,length=20)
plot(f)
f1=f*f+3*f-5
plot(f1)
curve(x^2+3*x-5,-3,4)



#zajecia 2
#zad1
#wczytanie
loty=read.csv("loty(1).csv",sep=";")
#a
class(loty)
#b
lab=names(loty)
for (i in 1:length(loty))
{
print(paste0("w roku ",lab[i]," z lini lotniczej skorzystalo:  ",round(mean(loty[,i]),3), " tysiecy pasazerow"))
}
dim(loty)
#mediana
print("w roku przez conajmniej 6 miesiecy liczba pasazerow nie przekraczala tej wartosci i przez 6 miesiecy byla wieksza niz")
quantile(loty[,1],probs=0.5)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 6 miesiecy liczba pasazerow nie przekraczala, ale byla tez niemniejsza niz  ",round(quantile(loty[,i],probs=0.5),3)))
}


#kwartyl pierwszy
quantile(loty[,1],probs=0.25)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 3 miesiecy liczba pasazerow nie przekraczala, oraz przez co najmniej 9 miesiecy byla niemniejsza  ",round(quantile(loty[,i],probs=0.25),3)))
}

#trzeci kwartyl
quantile(loty[,1],probs=0.75)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 9 miesiecy liczba pasazerow nie przekraczala, oraz przez co najmniej 3 miesiecy byla niemniejsza  ",round(quantile(loty[,i],probs=0.75),3)))
}

# odchylenie standardowe

sd(loty[,1])

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," liczba pasazerow odchylała sie od sredniej o  ",round(sd(loty[,i]),3), " tysiecy osob ",
        round(sd(loty[,i])/mean(loty[,i])*100), " to wspolczynnik zmiennosci"))
}

#histogram{
#wektor kolorow
colors=c(42,4,6,14,10,12,54)
#podział ekranu
par(mfrow=c(2,3))
  

  #nazwa
  lab=names(loty)
  #wektor breakow
  br=seq(220,670,50)
  
    for (i in 1:length(loty))
    {
    hist(loty[,i],main=lab[i],breaks=br,xlab="liczba pasazerow", col=colors[i])
    }
#}koniec histogramu
##naprawa podzialu
  par(mfrow=c(1,1))
  
#te takie pudełkowe

boxplot(loty,col=4)


#zadanie 2
#wczytanie z przecinkiem zamiast kropki 
oceny=read.csv("oceny(1).csv",sep=";",dec=",",)

lab=names(oceny)
for (i in 1:length(oceny))
{
  print(paste0("w ",lab[i]," srednia ocen to ",round(mean(na.omit(oceny[,i])),3)))
}


#histogram{

#podział ekranu
par(mfrow=c(2,2))


#nazwa
lab=names(oceny)

for (i in 1:length(oceny))
{
  discrete.histogram(oceny[,i],main=lab[i],xlab="srednia ocen", col=i, freq=TRUE)
}
#}koniec histogramu
##naprawa podzialu


#szereg rozdzielczy punktowy dla danych
par(mfrow=c(2,3))
for (i in 1:length(oceny))
{
  print(table(oceny[,i]))
}

#wykres kołowy
for (i in 1:length(oceny))
{
  pie(table(oceny[,i]))
}


boxplot(oceny)


#zajecia3

#Zadanie 1 
#a)rozkład dwumianowy Bin(n,p)
n=5
p=0.3
s=c(0:5) # przypisujemy wartości zmiennym losowym S
pr=dbinom(s,n,p) #gęstość rozkładu
tp =rbind(s,pr)

dys=pbinom(s,n,p)#dystrybuanta
td = rbind(s,dys)

plot(s,pr,type="h",lwd = 4, main = "Histogram prawdopodobieństwa", xlab ="s", ylab = "p")

#P(s = 3)
dbinom(3,n,p)

#P(s >= 3)
1-pbinom(2,n,p)

#P(S < 3)
pbinom(2,n,p)

#Zadanie 2
n2 = 8 #liczba żarówek
p2 = 0.9 #p że bedzie zyc 500h
b = c(0:8)
pr2 = dbinom(b,n2,p2)
tp2 = rbind(b,pr2)
tp2

#P(B=8)
dbinom(8,n2,p2)
#P(B=7)
dbinom(7,n2,p2)
#P(B>5)
1- pbinom(5,n2,p2)
#E(B)
E = sum(b*pr2) # 7,2 żarówek przekrozczy żywotność 500h
#SD(B)
SD =  sqrt(sum(b^2*pr2)-E^2)
SD = sqrt(n2*p2*(1-p2)) # możemy się spodziewać że przeciętne odchylenie od średniej wynosi 1 żarówkę
SD
#Zadanie 3
lambda = 0.01
curve(dexp(x,lambda),0,1000)#rozkład wykładniczy
#P(X >= 200) = P(X > 200)
1 - pexp(200,lambda)
#P(X<100)
pexp(100,lambda)
#P(X<500)
pexp(500,lambda)

#Zadanie 4
E2 = 2.4
lambda2 = 1/E2
curve(dexp(x,lambda2),0,30)
#P(X > 3)
1 - pexp(3,lambda2)
#P(2 < X < 3)
pexp(3,lambda2) - pexp(2,lambda2)
f = function(x){x*dexp(x,lambda2)}
integrate(f,lower=0,upper= Inf)

#Zadanie 5
mu = 0.13 # średnia 
sig=0.005 # odchylenie
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)##99,7% obserwacji w tym przedziale
#P(0.14 > X > 0.12)
pnorm(0.14,mu,sig) - pnorm(0.12,mu,sig)


#Zadanie 6
mu = 120
sig = 15
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
#P(111 < X < 135)
pnorm(135,mu,sig) - pnorm(111,mu,sig)

#Zadanie 7
mu=46.8
sig = 1.75
#P(X<50)
pnorm(50,mu,sig)
#P(X > 48)
1 - pnorm(48,mu,sig)
#Zadanie 8

n=100
p=0.25

#rozkład dokładny - dwumianowy
#P(X <= 15)
pbinom(15,n,p)

#Przybliżenie rozkładem normalnym

pnorm(15,n*p,sqrt(n*p*(1-p)))

#Zadanie 9

mu = 200
sig = 10
n = 25
#avR ma rozkład N(mu,sig/sqrt(n))
#P(199 < X < 202)
pnorm(202,mu,sig/sqrt(n)) - pnorm(199,mu,sig/sqrt(n))

#T=X1+X2+...+X25, T ma rozkład normalny N(n*mu,sqrt(n)*sig)
#P(T<5100)
pnorm(5100,n*mu,sqrt(n)*sig)

#Zadanie 10
mu = 202
sig = 14
n = 64
#P(198 < X < 206)
pnorm(206,mu,sig/sqrt(n)) - pnorm(198,mu,sig/sqrt(n))

#Zadanie 11
mu = 0.5
sig = 0.2
n = 100
#P(T > 47)
1 - pnorm(47,n*mu,sqrt(n)*sig)



#zad1
#wczytanie
loty=read.csv("loty(1).csv",sep=";")
#a
class(loty)
#b
lab=names(loty)
for (i in 1:length(loty))
{
print(paste0("w roku ",lab[i]," z lini lotniczej skorzystalo:  ",round(mean(loty[,i]),3), " tysiecy pasazerow"))
}
dim(loty)
#mediana
print("w roku przez conajmniej 6 miesiecy liczba pasazerow nie przekraczala tej wartosci i przez 6 miesiecy byla wieksza niz")
quantile(loty[,1],probs=0.5)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 6 miesiecy liczba pasazerow nie przekraczala, ale byla tez niemniejsza niz  ",round(quantile(loty[,i],probs=0.5),3)))
}


#kwartyl pierwszy
quantile(loty[,1],probs=0.25)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 3 miesiecy liczba pasazerow nie przekraczala, oraz przez co najmniej 9 miesiecy byla niemniejsza  ",round(quantile(loty[,i],probs=0.25),3)))
}

#trzeci kwartyl
quantile(loty[,1],probs=0.75)

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," przez co najmniej 9 miesiecy liczba pasazerow nie przekraczala, oraz przez co najmniej 3 miesiecy byla niemniejsza  ",round(quantile(loty[,i],probs=0.75),3)))
}

# odchylenie standardowe

sd(loty[,1])

for (i in 1:length(loty))
{
  print(paste0("w roku ",lab[i]," liczba pasazerow odchylała sie od sredniej o  ",round(sd(loty[,i]),3), " tysiecy osob ",
        round(sd(loty[,i])/mean(loty[,i])*100), " to wspolczynnik zmiennosci"))
}

#histogram{
#wektor kolorow
colors=c(42,4,6,14,10,12,54)
#podział ekranu
par(mfrow=c(2,3))
  

  #nazwa
  lab=names(loty)
  #wektor breakow
  br=seq(220,670,50)
  
    for (i in 1:length(loty))
    {
    hist(loty[,i],main=lab[i],breaks=br,xlab="liczba pasazerow", col=colors[i])
    }
#}koniec histogramu
##naprawa podzialu
  par(mfrow=c(1,1))
  
#te takie pudełkowe

boxplot(loty,col=4)


#zadanie 2
#wczytanie z przecinkiem zamiast kropki 
oceny=read.csv("oceny(1).csv",sep=";",dec=",",)

lab=names(oceny)
for (i in 1:length(oceny))
{
  print(paste0("w ",lab[i]," srednia ocen to ",round(mean(na.omit(oceny[,i])),3)))
}


#histogram{

#podział ekranu
par(mfrow=c(2,2))


#nazwa
lab=names(oceny)

for (i in 1:length(oceny))
{
  discrete.histogram(oceny[,i],main=lab[i],xlab="srednia ocen", col=i, freq=TRUE)
}
#}koniec histogramu
##naprawa podzialu


#szereg rozdzielczy punktowy dla danych
par(mfrow=c(2,3))
for (i in 1:length(oceny))
{
  print(table(oceny[,i]))
}

#wykres kołowy
for (i in 1:length(oceny))
{
  pie(table(oceny[,i]))
}


boxplot(oceny)


#side info, w formie komentarzy na kolosie jak najwiecej komentarzy
#pomoze to podniesc ocene
#ZAD1
#rozklad prawdopodobiensta
#30% studni zanieczyszczone, 5 studni wybieramy losowo, nie wiemy ile jest studni
# S ~bin(n,p)
n=5
p=0.3
s=0:n
#s-ile studni, jest zanieczyszone, szansa na to ze tyle studni jest zanieczyszczone
prob = dbinom(s,n,p)
rbind(s,prob)
plot(s,prob,type="h")

#dokladnie 3 studnie brudne
prob[s==3]

#wiecej 3 lub wiecej brudnych
prob[s>=3]
sum(prob[s>=3])

#mniej niz 3
prob[s<3]
sum(prob[s<3])

#zadanie 2 B ilosc zarowek dobrych, 0.9 na to ze bedzie dobra, 8 zarowek
#co ma być na kolokwium: B ~ bin(n,p)
p=0.9
n=8
b=0:n
prob=dbinom(b,n,p)
#8 dobrych
sum(prob[b==8])
#7 dobrych
sum(prob[b==7])
#wiecej niz 5 dobrych
sum(prob[b>5])
#wartość oczekiwana
sum(b*prob)
#ale jest to dyskretna, zaokraglamy zgodnie z matematyka
round(sum(b*prob))
#interpretacja ^: spodziewamy się że jak kupimy 8 zarowek to 7 bedzie git
#odchylenie standardowe sqrt(n*p*(1-p))
sqrt(n*p*(1-p))
#lub:
sqrt(sum(b^2*prob)-sum(b*prob)^2)
#liczba zarowek dobrych przecietnie odchyla sie od  7 o 1
#ergo zdroworozsadkowe bedzie zakladac ze dobre bedzie 6, 7 albo 8


#zadanie3
#czas ma rozklad wykladniczy szansa na awarie 0,01 -> T ~EXP(lambda)
#są dwa ogniwa
lambda=0.01
#x musi byc goly, usun go
curve(dexp(x,lambda))
#tak lepiej
curve(dexp(x,lambda),0,365)
#tak jeszcze lepiej
#4/lambda bo (1/lamda + 3*(1/lambda))/lambda
#0 bo nie moze byc ujemne
curve(dexp(x,lambda),0,(4/lambda))

#szansa na to ze satelita przezyje 200 dni lub wiecej
#tutaj trzeba umiec czerwone wzory (wyklad ze zmiennej losowej, to przy dystrybuancie, strona 21/36)
# P(T>=200)=1-F(200)
#F to dystrybuanta
1-pexp(200,lambda)

#P(T<100)
#UWAGA!!!! przy ciaglych zmiennych losowych nie ma znaczenie czy jest domkniete ostro czy nie
pexp(100,lambda)

#P(T<500)
pexp(500,lambda)


#ZAD4
#srednia to wartosc oczekiwana
#2,4 = 1/lambda
lambda=1/2.4

#R ~ EXP(Lambda)

#szansa na to ze przekracza 3
1-pexp(3,lambda)

#szansa ze miesci sie miedzy 2 a 3
pexp(3,lambda)-pexp(2,lambda)

#dodatkowo rysunek zeby to mialo sens
curve(dexp(x,lambda),0,(1/lambda + 3*(1/lambda))/lambda)

#opcja z calka jakas
f=function(x)(x*dexp(x,lambda))
integrate(f,0,Inf)
#najpierw funkcja(jakiej zmiennej)(jaka funkcja)
#integrate(funkcja,od,do)

#zad5
#zmienna o rozkladzie NORMALNY ze srednia 0,13, odchylenie 0,005
#wymagane jest od 0,12 do 0,14
#rozklad NORMALNY jest najwazniejszy ever
#oznacza sie go N
#N(mi,si)
#N(wartosc oczekiwana, odchylenie standradowe)
#R ~ norm(ni,si)
mi=0.13
si=0.005
#rysunel
curve(dnorm(x,mi,si),mi-3*si,mi+3*si)
#mi-3si,mi+3si to reguła 3 sigm, proste

#P(0.12<R<0.14)
pnorm(0.14,mi,si)-pnorm(0.12,mi,si)

#zad6
mi=2
si=0.25
curve(dnorm(x,mi,si),mi-3*si,mi+3*si)
pnorm(2.25,mi,si)-pnorm((111/60),mi,si)

#zad8
#25% studentow ma stypendium, niech X bedzie liczba studentow losowej probie 100 studentow
#ktorzy maja stypednium
p=0.25
n=100
X=0:n
prob=dbinom(X,n,p)
sum(prob[X<=15])

#policz sobie E i SD
#X ~ app norm (mi,si)
mi=n*p
si=sqrt(n*p*(1-p))
pnorm(15,mi,si)

#zajecia4

#zmienne losowe

#25kabli 200 omów 10 odchylenia
#a) srednia rezystancja miedzy 199 a 202
#rozklad normalny mi 200 sigma 10
mi=200
si=10
proba=25
#P(199<srednia<202) (ta srednia to R z kreseczka u gory)
#R z kreska ~ M(200, 10/sqrt(25))
#f(202)-f(199)
pnorm(202,mi,(si/sqrt(proba)))-pnorm(199,mi,(si/sqrt(proba)))


#b
#suma rezystancji nie przekracza 5100
#literka T jak Total
#T~N(200*25,10*5)
#T~N(Mi*proba,si*sqrt(proba))
pnorm(5100,mi*proba,si*sqrt(proba))


#nić 11
#zmienna losowa W
#n=100, srednia wytrzymalosc 0.5, odchylenie 0.2 
#centralne twierdzenie graniczne tu uzywamy
#T>=47
#T~app N(mi*proba,si*sqrt(proba))
mi=0.5
si=0.2
proba=100
#P(X>47)
#F=Dystrybuanta
1-pnorm(47,mi*proba,si*sqrt(proba))



#inne laby
#estymacja
#readcsv
#sessiom>set working directoty
dane=read.csv("dane_est.csv",sep=";",dec=",")
  
#estymacja diamenty
#sprawdzamy rozkładem normalny nowa metode produkcji, oceniamy waga
#populacja - wszystkie diamenty wyprodukowane dana metoda
#12 wybranych w zadaniu - proba
#zmienna losowa - waga wyrazona w karatach
#populacji nie znamy
#srednia dla populacji - mi
#Xbar - srednia z proby

prob=12
diamenty=dane[0:prob,1]
#ocena punktowa
xbar=mean(diamenty)

#warianja z proby s^2
war=var(diamenty)
print(war)
#odchyelenie-sigmadaszek
os=sd(diamenty)
print(os)

#przedział ufności
#oszacuj z pewnoscia 95%
#ufnosc to 1-alfa
alfa=0.05
#miH mi z haczykiem, mi oszacowane punktowo
kwantyl=qt(1-(alfa/2),prob-1)
lowerbound=xbar-kwantyl*(os/sqrt(prob))
upperbound=xbar+kwantyl*(os/sqrt(prob))
lowerbound=0.49
upperbound=0.57
#na 95% srednia nalezy do tego przedzialu
#srednia sie nie zmienia, to przedzial ja pokrywa

#od a-i
# LAB 1 - BASIC COMMANDS --------------------------------------------------------------------------------------------------------------

# ex. 1 --------------------
x = sin(2*pi)
round(x)
cos(3/4)
tan(pi)
log(100, base = 10)
log(15, base = exp(1)) # or just log(15)
?log
exp(3)
exp(1)*exp(1)*exp(1)
log(1/7, base = 7)
64^(1/3)

# ex. 2 --------------------
vec = seq(1, 10)
vec
sum(vec)

# ex. 3 --------------------
x = seq(2, 20, 2)
x
x*x
x^2
?rev
rev(x) # reverse order of elements
length(x)
len_of_vetor = sqrt(sum(x^2))
len_of_vetor

# ex. 4 --------------------
y = seq(5, 10, length = 13)
y
length(y)

# ex. 5 --------------------
z = c(1, 2)
z1 = rep(z, 5)
z1 #1 2 1 2 1 2 1 2 1 2
z2 = rep(z, each = 5)
z2 #1 1 1 1 1 2 2 2 2 2

z1*z2 #1 2 1 2 1 4 2 4 2 4
t(z1)%*%z2 # 23
z1%*%t(z2) # matrix 10x10

# ex. 6 --------------------
a = c(1, 3, 6, 2, 7, 4)
a
min(a)
which.min(a) # find index of min element
which(a<=4)
sum(a)
length(a)
a[3]
a + 4 
b = a[-4] # vector b equal to a without 4th component
b

c = a+b
c # 2 6 12 9 11 5
a # 1 3 6 2 7 4
b # 1 3 6 7 4

d = a[a>4]
d

# ex. 7 --------------------
?matrix
A = matrix(c(2, 1, 1, 3, -1, 1, 0, 2, -1), nrow = 3, ncol = 3)
A
B = t(A)
B
det(A)
A*A
#  4    9    0
#  1    1    4
#  1    1    1
A%*%A
# 7    3    6
# 3    6   -4
# 2    1    3

tr(A)
?tr
C = solve(A)
A*C
round(A%*%C)
a = A[,3]
a
b = A[2,]
b
A
a%*%b
a%*%t(b)

# ex. 8 --------------------
x = c(1, 2, 5, 6, 1, 2, 19, 2, 4, 7)
y = c(7, 13, 11, 3, 4, 16, 9, 8, 1, 10)
plot(x, y)
plot(data.frame(x, y))
plot(rbind(x, y))
points(x, y, type = "p")
curve(x^2 + 3*x - 5, from = -3, to = 4)


# LAB 2 - DESCRIPTIVE STATISTICS --------------------------------------------------------------------------------------------------------------

# ex. 1 --------------------

library("arm")
setwd("/home/farmerobot/pp/st")
data = read.csv("flights.csv", sep = ";")
class(data)
ncol(data)
labels = names(data)
par(mfrow = c(3,2))
for (i in 1:ncol(data)) {
  col = data[,i]
  hist(col, main = labels[i], breaks= seq(250,600, 50))
}

for (i in 1:ncol(data)) {
  col = data[,i]
  # mean
  print(paste( "Tn the year ",
               substr(labels[i], 2, 5),
               " the average amount of passengers across the airlines was ",
               mean(col),
               "."))
  # quantiles
  quant = quantile(col)
  idx = c(2, 3, 4)
  for (j in 1:3) {
    print(paste("In the year ",
                substr(labels[i], 2, 5),
                " at least ",
                (idx[j]-1) * 25,
                "% of the airlines had the number of passengers not greater than ",
                quant[idx[j]],
                ", and at least ",
                100 - ((idx[j]-1) * 25),
                "% of the airlines had the number of passengers not smaller than ",
                quant[idx[j]]
                ))
  }
  # standard deviation
  print(paste( "In the year ",
               substr(labels[i], 2, 5),
               " the number of passengers on average deviated from the mean by ",
               round(sd(col), 2)
               ))
  # variability index 
  print(paste( "In the year ",
               substr(labels[i], 2, 5),
               " the variability index was ",
               round(100*sd(col)/mean(col), 2), "%",
               " which indicates ",
               if (round(100*sd(col)/mean(col), 2) < 20) {
                 " weak variability, that is the data is very close to the mean."
               } else if (round(100*sd(col)/mean(col), 2) < 40) {
                 "medium variability, that is the data is moderatly close to the mean."
               } else if (round(100*sd(col)/mean(col), 2) < 60) {
                 "strong variability, that is the data is quite spread out from the mean."
               } else {
                 "very strong variability, that is the data is stronly spread out from the mean."
               }
               ))

}
quant = quantile(data[,1])
quant

boxplot(data)

# ex. 2 --------------------
library("arm")
data = read.csv("notes.csv", sep = ";", dec = ",")
par(mfrow = c(2, 2))
labels = names(data)
for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  print(table(col))
}

par(mfrow = c(2, 2))

for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  discrete.histogram(col, main = substr(labels[i], 7, 8))
}

for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  #mean
  print(paste( "In the group ",
               substr(labels[i], 7, 8),
               "the average of grades was ",
               mean(col)
               ))
  # quantiles
  quant = quantile(col)
  idx = c(2, 3, 4)
  for (j in 1:3){
    print(paste( "In the group ",
               substr(labels[i], 7, 8),
               " at least ",
               (idx[j]-1)*25,
               "% of the students got a grade not greater than ",
               quant[j],
               ", and at least ",
               100 - ((idx[j]-1)*25),
               "% of the students got a grade not smaller than ",
               quant[j]
               ))
  }
  
  # standard deviation
  print(paste("In the group ",
              substr(labels[i], 7, 8),
              " on average grades deviated from the mean by ",
              sd(col)
              ))
  
  #variability index
  print(paste( "In the group ",
               substr(labels[i], 7, 8),
               " the variability index was ",
               round(100*(sd(col)/mean(col)), 2),
               "%"
               ))
}

boxplot(data)
par(mfrow = c(2,2))
for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  print(table(col))
  pie(col)
}


# ex. 3 --------------------
data = read.csv("strawberries.csv", sep = ";")
class(data)

for (i in 1:ncol(data)){
  col = na.omit(data[,i])
  print(table(cut(col, 5)))
}

par(mfrow = c(2,1))
labels = names(data)
for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  hist(col, 
       freq = FALSE, 
       main = paste("Year", substr(labels[i], 6, 9)),
       breaks = seq(min(col), max(col), length.out = 6))
}

for (i in 1:ncol(data)) {
  print(paste("COLUMN ", i))
  col = na.omit(data[,i])
  # mean
  print(paste( "In the year ",
               substr(labels[i], 6, 9),
               " the average yeild of strawberries was ",
               round(mean(col), 2)
               ))
  
  # quantiles (quant[3] = median)
  quant = quantile(col)
  idx = c(2, 3, 4)
  for (j in 1:3) {
    print(paste("In the year ",
                substr(labels[i], 6, 9),
                " at least ",
                (idx[j]-1)*25,
                "% of the yeilds of strawberries was not greater than ",
                round(quant[j], 2),
                ", and at least ",
                100 - ((idx[j]-1)*25),
                "% of the yeilds of strawberries was not smaller than ",
                round(quant[j], 2)    
                ))
  }
  
  #standard deviation
  print(paste( "In the year ",
               substr(labels[i], 6, 9),
               " on average the yeild of strawberries deviated from the mean by ",
               round(sd(col), 2)
               ))
  
  #variability index
  print(paste( "In the year ",
               substr(labels[i], 6, 9),
               " the variability index was ",
               round(100 * (sd(col)/mean(col)), 2), "%"
               ))
}

boxplot(data)

par(mfrow = c(1,2))

for (i in 1:ncol(data)) {
  col = na.omit(data[,i])
  print(table(cut(col, 5)))
  pie_table = table(cut(col, 5))
  pie(pie_table)
}
  

# LAB 3 - RANDOM VARIABLES AND SAMPLING --------------------------------------------------------------------------------------------------------------

# ex. 1 --------------------
n = 30
p = 0.3
set.seed(156066)
rbinom(1, n, p)

# ex. 2 --------------------
n = 30
p = 0.6
set.seed(156066)
x = 0:30
r = dbinom(x, n, p)
plot(r, type = 'h')

# ex. 3 --------------------
# S - random variable denoting the number of contaminated wells
# 30% of wells are contaminated in the city
# 5 wells were chosen to verify
# S ~ bin(5, 0.3)
p = 0.3
n = 5
s = 0:n
prob = dbinom(s, n, p)
# P(S=3)
dbinom(3, n, p)
#P(S>=3)
sum(prob[s>=3])
#P(S<3)
sum(prob[s<3])

# ex. 4 --------------------
# B - random variable denoting number of bulbs that work for at least 500h
# p = 0.9
# n = 8
# B ~ bin(8, 0.9)
n = 8
p = 0.9
b = 0:n
prob = dbinom(b, n, p)
rbind(b, prob)

#P(B=8)
sum(prob[b==8])
#P(B=7)
sum(prob[b==7])
#P(B>5)
sum(prob[b>5])
#E(B)
# expected value = sum(b*p(b))
ex = sum(b*prob)
ex
# We can expect around 7 bulbs to be working for at leaast 500h.

#SD(B)
# standard deviation = sqrt(var)
# var = E(X^2) - EX^2
var = sum(b^2*prob) - ex^2
var
std = sqrt(var)
std
# On average the number of working bulbs deviates by 0.849 from the mean.


# ex. 5 --------------------
# P - power cell failures 
# P ~ exp(lambda = 0.01)
# there are two cells functioning, system functions as long as there is at least one cell functioning 
lambda = 0.01
curve(dexp(x, lambda), 0, 4/lambda)

# P(P>=200)
1 - pexp(200, lambda)
# P(P<=100)
pexp(100, lambda)
#P(P<500)
pexp(500,lambda)


# ex. 6 --------------------
# M - magnitudes of earthquakes
# M ~ exp(lambda=1/mean)
mean = 2.4
lambda = 1/mean
curve(dexp(x, lambda), 0, 4/lambda)
#P(M>3)
1-pexp(3, lambda)
#P(2<M<3)
pexp(3,lambda)-pexp(2, lambda)

# ex. 7 --------------------
# A - resistance of wires produced by company A
# A ~ N(mu=0.13, sig=0.005)
mu = 0.13
sig = 0.005
curve(dnorm(x, mu, sig), mu-3*sig, mu+3*sig)
#P(0.12<A<0.14)
pnorm(0.14, mu, sig) - pnorm(0.12, mu, sig)

# ex. 8 --------------------
# D - drying time
# mean = 2h = 120 min
# std = 15 min
# D ~ N(120, 15)
mu = 120
sig = 15
curve(dnorm(x,mu,sig), mu-3*sig, mu+3*sig)
#P(110<D<135)
pnorm(135,mu,sig)-pnorm(110,mu,sig)


# ex. 9 --------------------
# S - mopeds speed
# S ~ N(mu, sig)
mu = 46.8
sig = 1.75
#P(S<50)
pnorm(50, mu, sig)

#P(S>48)
1 - pnorm(48, mu, sig)

# ex. 10 --------------------
x = 0:500
n = 20
p = 0.2
set.seed(156066)
y = rbinom(x, n, p)
y
par(mfrow = c(1,3))
library("arm")
discrete.histogram(y, xlim = c(0,n)) # probability histogram of random gen. with binomal distribution
plot(dbinom(0:n, n, p), type = "h") # probability histogram of binomial distribution
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), 0, n) # curve of normal distribution


hist(y, xlim = c(0,n), freq=FALSE)
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), 0, n, add = TRUE)



# ex. 11 --------------------
# X - number of students receiving financial aid
# p = 0.25
# n = 100
# X ~ bin(n,p)
# P(X<=15)
n = 100
p = 0.25
pbinom(15, n, p)

# X ~ app N(n*p, sqrt(n*p*q))
pnorm(15, n*p, sqrt(n*p*(1-p)))


# ex. 12 --------------------
samples = c(0:199)
mu = 0
sig = 1
n = 30
for (i in 0:200) {
  samples[i] = mean(rnorm(30, 0, 1))
}
hist(samples, freq = F)
curve(dnorm(x, mu, sig/sqrt(n)), mu-3*sig/sqrt(n), mu+3*sig/sqrt(n), add=T)


# ex. 13 --------------------
samples = 1:200
length(samples)
n = 30
p = 0.4
for (i in 0:length(samples)) {
  samples[i] = mean(rbinom(10, n, p))
}
samples

hist(samples, freq = F)
curve(dnorm(x, n*p, sqrt(n*p*(1-p))/sqrt(10)), n*p - 3*sqrt(n*p*(1-p))/sqrt(10), n*p + 3*sqrt(n*p*(1-p))/sqrt(10), add=T)

# ex. 14 --------------------
# R - resistance in a curcuit
# mean = 200 resistance of a resistor
# std = 10 resistance of a resistor
mu = 200
std = 10
# R ~ N(200, 10)
# R_i ~ N(200, 10)

# P(199<Rbar<202) 
# Rbar ~ N(200, 10/sqrt(50))
sigbar = 10/sqrt(50)
pnorm(202,mu, sigbar) - pnorm(199, mu, sigbar)
# P(T<=10020)
# T - smaple total
muT = mu*50
sigT = sqrt(50) * std
pnorm(10020, muT, sigT)

# ex. 15 --------------------
# B - blood cholesterol lvls
# B ~ N(mu, sig)
mu = 202
sig = 14
n = 64
# P(198<B_bar<206)
mubar = mu 
sigbar = sig/sqrt(n)
pnorm(206, mubar, sigbar) - pnorm(198, mubar, sigbar)

# ex. 16 --------------------
# S - strength of a thread
# S ~ N(mu, sig)
mu = 0.5
sig = 0.2
# strength of a rope = sum of strengths of threads
n = 100
#P(T>=47)
muT = n*mu
sigT = sig * sqrt(n)
1-pnorm(47, muT, sigT)



# LAB 4 - ESTIMATION --------------------------------------------------------------------------------------------------------------

data = read.csv("data_est.csv", sep = ";")

# ex. 1 --------------------
diamonds = na.omit(data[,1])
# a
# Population: all synthetic diamonds produced in the new process
# Sample: 12 synthetic diamonds produced in the new process
# Random variable: W - weight of the synthetic diamonds

# b 
est_mean = mean(diamonds)
est_mean
# The mean of all synthetic diamonds can be estimated by the sample mean that is equal to 0.5341667.

# c
n = length(diamonds)
alpha = 0.05
est_std = sd(diamonds)
q = qt(1-alpha/2, n-1)
L = est_mean - q*est_std/sqrt(n)
U = est_mean + q*est_std/sqrt(n)
L
U
# We are 95% confident that the confidence interval
# (0.498899, 0.5694343)[carat] covers the true unknown mean
# of all weights of synthetic diamonds.

# e
est_var = est_std^2
est_std
est_var
# The standard deviation of all synthetic diamonds can be estimated by 0.0555073.
# The variance of all synthetic diamonds can be estimated by 0.003081061.

# f
alpha = 0.05 
var_int = sigma.test(diamonds, conf.level = 1-alpha)
sqrt(var_int$conf.int)
# We are 95 % confident that the interval (0.03932110 0.09424463)[carat]
# covers the true unknown standard deviation of all synthetic diamonds weights.
var_int$conf.int
# We are 95 % confident that the interval (0.001546149 0.008882050)[carat^2]
# covers the true unknown variance of all synthetic diamonds weights.


# ex. 2 --------------------
# M - random variable denoting amounts of PCB in nursing mothers' milk
# Population: milk of all nursing moms
# Sample: 20 nursing women selected
milk = na.omit(data[,2])
mean_est = mean(milk)
mean_est
# The average amount of PCB in milk can be estimated by 5.8.
std_est = sd(milk)
std_est
# The amount by which amounts of PCB in milk deviates from the mean on average
# can be estimated by 5.084548.
var_est = std_est^2
var_est
# The variance of PCB in milk can be estimated by 25.85263.

alpha = 0.05
mean_int = t.test(milk, conf.level = 1-alpha)
mean_int
# We are 95% confident that the interval (3.42, 8.18)[parts per milion]
# covers the true unknown mean of all amounts of PCB in nursing mothers' milks.

var_int = sigma.test(milk, conf.level = 1-alpha)
var_int$conf.int
# We are 95% confident that the interval (14.95, 55.15)[(parts per million)^2]
# covers the true unknown variance of all amounts of PCB in milks.
sqrt(var_int$conf.int)
# We are 95% confident that the interval (3.867, 7.426)[parts per million]
# covers the true unknown standard deviation of all amounts of PCB in milks.

# ex. 3 --------------------
cigs = na.omit(data[,3])
# Population: All cigarettes of the new brand.
# Sample: 15 cigs chosen to test
# N - random variable denoting the amount of nicotine in a cig
# N ~ N(mu, sig=0.7)

std = 0.7
alpha = 0.05
n = length(cigs)
mean_est = mean(cigs)
mean_int = z.test(cigs, sigma.x=std, conf.level = 1-alpha)
q = qnorm(1-alpha/2)
L = mean_est - q*std/sqrt(n)
U = mean_est + q*std/sqrt(n)
L
U
# we are 95% confident that the interval (1.455091, 2.163576)[nicotine contents]
# covers the true unknown mean of all nicotine contents in all cigarettes.

# How big should n be so that U-L <= 0.3.
# U - L = mean_est + q*std/sqrt(n) - (mean_est - q*std/sqrt(n))
# = 2*q*std/sqrt(n)
# 2*q*std/sqrt(n) <= 0.3
# 20/3 * q*std <= sqrt(n)
# (20/3 * q * std)^2 <= n
n = ((20/3) * q * std)^2
n #83.65844
# In order for the conf int to be equal or smaller than 0.3
# the sample size should be at least 84.

# ex. 4 --------------------
signal = na.omit(data[,4])
n = length(signal)
# S - intensity of signal recorded
# S ~ N(mu, sig=3)
sig = 3
mu_est = mean(signal)
mu_est
alpha=0.05
z.test(signal, sigma.x=sig, conf.level = 1-alpha)
# We are 95% confident that the interval (17.44, 21.16)[intensity of signal]
# covers the true unknown mean of all recorded signals intensities.


# ex. 5 --------------------
# Population: all phonecalls made during midday
# sample: 1200 phonecalls
# S - random variable san of phonecalls during midday 
est_mean = 4.7
est_std = 2.2
n = 1200
alpha = 0.05
mean_int = zsum.test(est_mean, est_std, n, conf.level = 1-alpha)
mean_int$conf.int
# We are 95% confident that the interval (4.58, 4.82)[min]
# covers the true unknown mean of the span of all phonecalls made during midday.

est_var = est_std^2
q1 = qchisq(1-alpha/2, n-1)
q2 = qchisq(alpha/2, n-1)
L = (n-1)*est_var/q1
U = (n-1)*est_var/q2
sqrt(L)
sqrt(U)
# We are 95% confident that the interval (2.115368, 2.291738)[min]
# covers the true unknown standard deviation of all spans of phonecalls mad during midday.


# ex. 6 --------------------
lifetimes = na.omit(data[,5])
n = length(lifetimes)
alpha = 0.05
est_mean = mean(lifetimes)
est_std = sd(lifetimes)
mean_int = zsum.test(est_mean, est_std, n, conf.level = 1-alpha)
mean_int$conf.int
# We are 95% confident that the interval (1163.695, 1230.185)[unit of time]
# covers the true unknown mean of alllifetimes of all transistors.

# ex. 7 --------------------
# C - setting time of cement
# C ~ N(mu, sig=sqrt(25))
var = 25
sig = sqrt(var)
# n = ? such that qnorm(1-alpha/2) * sig/sqrt(n) <= 1
# qnorm(1-alpha/2) * sig/sqrt(n) <= 1
# (qnorm(1-alpha/n)*sig)^2 <= n
n = (qnorm(1-alpha/2)*sig)^2
n # 96.03647
# Sample size should be at least 97


# ex. 8 --------------------
sig = 0.3
alpha = 0.01
# conf interval length = 0.2/estimation error = 0.1
# W - weights of salmon 
# W ~ N(mu, sig=0.3)
# qnorm(1-alpha/2)*sig/sqrt(n) <= 0.1
# (10*qnorm(1-alpha/2)*sig)^2 <= n
n = (10*qnorm(1-alpha/2)*sig)^2
n
# If we want to be 90% confident then sample of size at least 25 is needed.
# If we want to be 99% confident then sample of size at least 60 is needed.

# ex. 9 --------------------
n = 100
t = 3
alpha = 0.05
est_p = t/n
est_p
q = qnorm(1-alpha/2)
L = est_p - q*sqrt(est_p*(1-est_p)/n)
U = est_p + q*sqrt(est_p*(1-est_p)/n)
L*100
U*100
# We are 95% confident that the interval (0, 6.35%) covers 
# the true unknown population proportion.

# ex. 10 --------------------
# C - nr underfilled cans 
# p - proportion of underfilled cans
# p >= 0.015
n = 100
t = 4
est_p = t/n
alpha = 0.05
q = qnorm(1-alpha/2)
L = est_p - q*sqrt(est_p*(1-est_p)/n)
U = est_p + q*sqrt(est_p*(1-est_p)/n)
L*100
U*100
# we are 95% confident that the interval
# from 0.15% to 7.85% covers the true unknown
# population proportion of all underfilled cans.

# ex. 11 --------------------


# ex. 12 --------------------


# ex. 13 --------------------
# S - numnber of people having sight problems ina a group
# estimation err = 0.05
# conf lvl 0.98
alpha = 0.02
# nothing known about p -> assume p = 0.5
p = 0.5
# n = ?
# qnorm(1-alpha/2) * sqrt(p*(1-p)/n) =(aroud) 0.05
# 20 * qnorm(1-alpha/2) * sqrt(p*(1-p)) = sqrt(n)
# 20*20 * (qnorm(1-alpha/2))^2 * p * (1-p) = n
n = 20*20 * (qnorm(1-alpha/2))^2 * p * (1-p)
n # 541.1894
# 541 people should be examined to obtain estimation error +- 0.05

p = 0.3
n = 20*20 * (qnorm(1-alpha/2))^2 * p * (1-p)
n #454.5991
# 455 peole should be examined to obtain est err equal around 0.05


#od ai 

setwd("/home/farmerobot/pp/st")
data = read.csv("IrisSepalWidth.csv", sep=";", dec=",")

# task 1
names = colnames(data)
par(mfrow = c(1,3))
len = ncol(data)
colors = c("Red", "Green", "Blue")
for (i in 1:len) {
  hist(data[, i],
       main = names[i],
       xlab = "Width",
       col = colors[i])
  print(
    paste0(
      "Data for ",
      names[i],
      ":",
      " Mean: ",
      round(mean(data[, i]), 2),
      "; Median: ",
      round(median(data[, i]), 2),
      "; Standard deviation: ",
      round(sd(data[, i]), 2)
    )
  )
  cat("Quartiles: (", quantile(data[, i]), ")\n")
  cat("variability index:", round(sd(data[, i]) / mean(data[, i]) * 100, 2), "%\n")
}
for (i in 1:len) {
  boxplot(data[, i])
}

# task 2
# A - number of cars with mouldy air conditioning
# A ~ bin(10, 0.3)
n = 10
p = 0.3
a = 1:n

A = dbinom(a, n, p)
print(paste0("Probability of exactly 4 cars have mouldy air conditioning: ", round(A[a == 4], 2)))
print(paste0("Probability of at least 5 cars have mouldy air conditioning: ", round(sum(A[a >= 5 ]), 2)))
print(paste0("Probability of less than 3 cars have mouldy air conditioning: ", round(sum(A[a < 3]), 2)))

ex = sum(a * A)
print(paste0("Expected value of A: ", round(ex, 2)))
var = sum(a ^ 2 * A) - ex ^ 2
print(paste0("Standard deviation of A: ", round(sqrt(var), 2)))

# task 3
# D - number of damaged roller
# D ~ exp(lambda)
mean = 5.3
lambda = 1 / mean

par(mfrow=c(1,1))
curve(dexp(x, lambda), 0, 4/lambda, main="Density function of D", xlab = "number of damaged roller", ylab = "probability")

# P(D > 10)
print(paste0("Probability of number of damaged roller exceeding 10: ", round(1 - pexp(10, lambda), 2)))
#P (5 < D < 10)
print(paste0("Probability of number of damaged roller being between 5 and 10: ", round(pexp(10, lambda) - pexp(5, lambda), 2)))

# task 4
# D - delay of train
# D ~ norm(mu, sig)
mu = 5
sig = 2
sample_size = 50

# Dbar ~ norm(mu, sig/sqrt(50))
sigbar = sig/sqrt(sample_size)

# P(5.1 < Dbar < 4.8)
print(paste0("Probability of the average train delay among 50 being between 4.8 and 5.1 minutes: ", round(pnorm(5.1, mu, sigbar) - pnorm(4.8, mu, sigbar), 2)))

# T - sample total
mu_T = mu * sample_size
sig_T = sig * sqrt(sample_size)
# P(T < 4 * 60)
print(paste0("The probability of the total dalay among 50 trains not exceeding 4 hours: ", round(pnorm(4 * 60, mu_T, sig_T), 2)))

means = 1:200
sums = 1:200
for (i in 1:length(means)) {
  sample = rnorm(50, mu, sig)
  means[i] = mean(sample)
  sums[i] = sum(sample)
}

par(mfrow = c(1,2))

# mean
hist(means, freq = F, col="Blue")
sig_m = sig / sqrt(sample_size)
curve(dnorm(x, mu, sig_m), mu - sig_m * 3, mu + sig_m * 3, col="Red", add=T)

# T
hist(sums, freq = F, col="Blue")
curve(dnorm(x, mu_T, sig_T), mu_T - sig_T * 3, mu_T + sig_T * 3, col="Red", add=T)

library("BSDA")
library("TeachingDemos")
# task 5
# population - all of the newly produced concrete
# sample - the 15 measured examples
# measurement - the drying time of a single example in hours
concrete = c(30, 29, 24, 25, 26, 27, 29, 22, 24, 31, 28, 25, 27, 23, 25)
n = length(concrete)
est_mean = mean(concrete)
est_sd = sd(concrete)

# mean
alpha = 0.05
mean_int = zsum.test(est_mean, est_sd, n, conf.level = 1 - alpha)
mean_int$conf.int
print(paste0("We are 95% confident that the true mean of the drying time lies within: (24.96; 27.68)"))

# sd
alpha = 0.04
var_int = sigma.test(concrete, conf.level = 1 - alpha)
sqrt(var_int$conf.int)
print(paste0("We are 96% confident that the true standard deviation of the drying time lies within: (1.92; 4.30)"))

# task 6
n = 1000
t = 120
est_p = t / n

print(paste0("The estimated proportion of drivers who are unable to work is: ", round(est_p, 2)))

alpha = 0.1

q = qnorm(1 - alpha / 2)
lower = est_p - q * sqrt(est_p * (1 - est_p) / n)
upper = est_p + q * sqrt(est_p * (1 - est_p) / n)
print(paste0("We are 90% confident that the true proportion of unemployed workers lies within: (", round(lower, 2), "; ", round(upper, 2), ")"))

alpha = 0.05
# 0.04 >= qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)
# n >= (qnorm(1 - alpha / 2))^2 * p * (1 - p) * 25^2
n = ceiling((qnorm(1-alpha/2)^2 * est_p * (1 - est_p) * 25^2))
print(paste0("In order to obtain an estimation error of +-0.04 on confidence level 0.95 with proportion ", round(est_p, 2), " we need a polupation size of: ", n))



