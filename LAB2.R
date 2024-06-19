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