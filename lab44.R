#ZAd przyklad 5 wyklad 3
#phat ma rozklad normalny N(p, sqrt(p*(1-p)/n))
T=232
p=0.25
n=1000
pnorm(T/n,p,sqrt(p*(1-p)/n))
----
#estymacja
dane = read.csv('dane_est.csv', sep=";", dec=",")
#zad1
#a populacja to wszystkie syntetyczne diamenty
#wyprdukowane nową metodą
#próba to 12 zbadanych diamentów
#badana zmienna to masa diamentów
diamenty = na.omit(dane$diamenty)
#b
mu=mean(diamenty)
sig2 = var(diamenty)
sd = sd(diamenty)
#c
PrzedzialuMu <- function(srednia,odchylenie,n,alfa,sigma=FALSE){
  if(sigma){
    return (c(srednia-qnorm(1-alfa/2)*odchylenie/sqrt(n), srednia+qnorm(1-alfa/2)*odchylenie/sqrt(n)))
  }else{
    if(n >= 30){
      return (c(srednia-qnorm(1-alfa/2)*odchylenie/sqrt(n), srednia+qnorm(1-alfa/2)*odchylenie/sqrt(n)))
    }
    else{
      return (c(srednia-qt(1-alfa/2, n-1)*odchylenie/sqrt(n), srednia+qt(1-alfa/2, n-1)*odchylenie/sqrt(n)))
    }
  }
}

t.test(diamenty, conf.level=0.95)

#e
PrzedzialuSig2 <- function(n, odchylenie, alfa){
  if(n>=30){
    return(c(((n-1)*odchylenie*odchylenie)/(n-1+qnorm(1-alfa/2)*sqrt(2*(n-1))), ((n-1)*odchylenie*odchylenie)/(n-1-qnorm(1-alfa/2)*sqrt(2*(n-1)))))
  }else{
    return(c((n-1)*odchylenie*odchylenie/qchisq(1-alfa/2, n-1), (n-1)*odchylenie*odchylenie/qchisq(alfa/2, n-1)))
  }
}
# z ufnoscia 95% przedzial (0.001;0.009) pokrywa nieznaną rzeczywistą wariancję masy wszystkich syntetycznych diamentow produkowanych nową metodą
chi2=sigma.test(diamenty, conf.level=0.95)
c=chi2$conf.int
L=sqrt(c[1])
P=sqrt(c[2])
#z ufnoscią 95% przedzial(0,039;0.095) pokrywa nieznaną
#rzeczywistą wartość odchylenia standardowego masy diamentow

#zad2,3,4 do zrobienia na next
#zad 2
#a populacja to wszystkie kobiety karmiace piersią
# próbka to 20 zbadanych, a zmienna to poziom pcb
#b
mleko=na.omit(dane$mleko)
mu = mean(mleko)
#c
var = var(mleko)
sd = sd(mleko)
n=20
#d
PrzedzialuMu(mu, sd, n, 1-0.95)
#e
x = PrzedzialuSig2(n, sd, 1-0.95)
sqrt(x)

#zad3
papierosy=na.omit(dane$papierosy)
sd=0.7
n=15
mu=mean(papierosy)
PrzedzialuMu(mu,sd,n,1-0.95, TRUE)
#b
x=((2*sd*qnorm(1-0.05/2))/0.3)**2
sd(papierosy)
#zad4
wodorosty=na.omit(dane$wodorosty)
var=var(wodorosty)
mu=mean(wodorosty)
sd=sd(wodorosty)
n=18
PrzedzialuMu(mu,sd,n,1-0.9)
PrzedzialuSig2(n,sd,1-0.9)
#zad 6
n=1200
mu=4.7
sd=2.2

zsum.test(mu,sd,n,conf.level=0.95)#wymagany pakiet bsda
x=c(((n-1)*sd*sd)/qchisq(1-0.05/2, n-1), ((n-1)*sd*sd)/qchisq(0.05/2, n-1))
sqrt(x)

#zad7
n=365
mu=102
var=81
zsum.test(mu, sqrt(var),n,conf.level = 0.98)
#zad8
var=25
sd=5
#błąd estymacji to połowo długości przedziału czyli n musi byc >= od linijki poniżej
((2*sd*qnorm(1-0.05/2))/2)**2
#zad10 przedzial ufnosci dla proporcji
binom.test(4,100,conf.level = 0.95)
prop.test(4,100,conf.level = 0.95)
c((4/100)-qnorm(1-0.05/2)*sqrt(((4/100)*(1-(4/100)))/100), (4/100)+qnorm(1-0.05/2)*sqrt(((4/100)*(1-(4/100)))/100))

#zad11
n=120
T=24
binom.test(T,n,conf.level = 0.9)
#zad12 #b p =0.5 zrob

