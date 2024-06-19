n=3
x=0:n
p=0.6

prob=dbinom(x,n,p)

rbind(x,prob)

expect=sum(x*prob)
print(expect)

variance=sum(x^2*prob)-expect^2
print(variance)

std=sqrt(variance)
print(std)

rbinom(5,n,p) #losowe wyniki eksperymentu
plot(x,prob,type="h")

n=20
x=0:n
p=0.1 #0.9
prob=dbinom(x,n,p)
plot(x,prob,type="h")

rm(x)
curve(0.01*exp(-0.01*x),0,500)

#f=function(x){0.01*exp(-0.01*x)}
f=function(x){x*0.01*exp(-0.01*x)}

#integrate(f, 0, 50)
integrate(f, 0, Inf)

lam=0.01
pexp(50,lam)

# x ~ N(1, 0.001)
# P(X<1.0015)=F(1.0015)
mu=1
sig = 0.001
pnorm(1.0015,mu,sig)

# P(X>0.9995)=1-F(0.9995)
1-pnorm(0.9995,mu,sig)

# P(0.9998<X<1,0004)=F(1.0004)-F(0.9998)
pnorm(1.0004,mu,sig)-pnorm(0.9998,mu,sig)



