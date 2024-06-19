#estymacja
czas = c(8,12,26,10,23,21,16,22,18,17,36,9)
xbar=mean(czas)
xbar

# muhat = 18,17 - hat - oszacowane

# średni czas użytkowania użądzenia przez wszytskich pacjentow zostal
# oszacowany na 18h 10 min

#PREZENTACJA ESTYMACJA, SLAJD 11
n=length(czas)
s=sd(czas)

alpha = 0.05

L=xbar-qt(1-alpha/2,n-1)*s/sqrt(n)
L
U=xbar+qt(1-alpha/2,n-1)*s/sqrt(n)
U

time = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)
meanCI = t.test(time, conf.level = 0.95)
meanCI$conf.int