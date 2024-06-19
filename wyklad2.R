dane=read.csv("ozon.csv", sep=";")

oz=dane$ozon

length(oz)

min(oz)
max(oz)

przedzialy=seq(0,12,length=8)

hist(oz,breaks=(przedzialy))

sort(oz) #sortuje 

#średnia: mean(dane)
#jakis kwantyl 25,50 co to jest
#quantile(dane, probs = p)
#q1 - 25 kwantyl, q2 - 50 kwantyl, q3 - 75 kwantyl

a=c(4,5,7,100,101)
quantile(a)
summary(a)

b=c(21,30,37,88)
quantile(b)

#wariancja (z proby): var(dane)
#odchylenie standardowe(przeciętne odchylenie od średniej): sd(dane)

A=c(3,3,4,4.5,4.5)
B=c(2,3.5,4,4.5,5)

mean(A) #średnia ocen ze statystyki w grupie A wynosi 4
mean(B) #średnia ocen ze statystyki w grupie A wynosi 4

quantile(A)#Co najmniej 25% osób otrzymało ocenę niewiększą niż 3 
#i conajmniej 25% otrzymało ocene nie mniejszą niz 3
#Dominata=[3,4,5]

var(A) #wariancja
sd(A) #przecietne oceny odchylaja sie od sredniej o 1 stopien

R=max(A)-min(A)
RQ=quantile(A,prob=0.75)-quantile(A,prob=0.25)
v=sd(A)/mean(A)*100 #oceny są slabo zroznicowane

#wykres pudełkowy: boxplot(dane)
par(mfrow=c(1,3))
boxplot(A)
boxplot(B)

boxplot(A,B)


#WYKLAD CZESC 2

#zeminna losowa ciagle - czas, 
#zmienna losowa nie ciagla - rzut koscia

#rozklad dwumianowy:
dbinom(0,3,0.6)
#suma 2 prawdopodobieństw:
dbinom(2,3,0.6)+dbinom(3,3,0.6)
#dystrybuanta:
1-pbinom(1,3,0.6)

x=seq(0,3)
p=c(dbinom(0,3,0.6),dbinom(1,3,0.6),dbinom(2,3,0.6),dbinom(3,3,0.6))

rozklad=rbind(x,p)

expect=0
for (i in 1:4){
  expect=expect+rozklad[1,i]*rozklad[2,i]
}
print("expect")
print(expect)

essa=rbind(1,2,3)