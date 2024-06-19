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

