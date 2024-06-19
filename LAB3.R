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
