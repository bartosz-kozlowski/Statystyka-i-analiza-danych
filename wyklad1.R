5+9
8-6
9*7
6/4

rowery=c(2,3,3,2,3,0,2,1,3,7,6)

#?read.csv

sort(rowery)
table(rowery)

discrete.histogram(rowery,freq=T)

#plot(table(rowery)/length(rowery))
plot(table(rowery))


pie(table(rowery))

ozon=read.csv("C:/Users/48663/Documents/laby statystyka/ozon.csv", sep=";")

oz=ozon$ozon
oz

br=seq(0,12,by=2)
table(cut(oz,breaks = br))
hist(oz,breaks = br)

View(ozon)

