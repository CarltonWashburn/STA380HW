plane.df = read.csv('ABIA.csv')
cancelled.flights = plane.df[which(plane.df$Cancelled == 1),]

plot(plane.df$UniqueCarrier)
plot(cancelled.flights$uniqueCarrier)

total.counts = table(plane.df$UniqueCarrier)
cancelled.counts = table(cancelled.flights$UniqueCarrier)
propofcancelled = cancelled.counts/total.counts
plot(propofcancelled,xlab = "Carrier",ylab = "Proportion of Filghts Cancelled")

plane.df.date = transform(plane.df,date = interaction(Year,Month,DayofMonth, sep = ' '))
plane.df.date['date'] = as.Date(plane.df.date$date,"%Y %m  %d")

depart.austin = plane.df.date[which(plane.df$Origin== 'AUS'),]
arrive.austin = plane.df.date[which(plane.df$Dest == 'AUS'),]

agg.dep = aggregate(depart.austin$DepDelay, by=list(depart.austin$date),FUN=mean,na.rm=TRUE)
plot(agg.dep,type = 'l' ,xlab="Date",ylab='Average Departure Delay in minutes')
lines(agg.dep)

agg.arr = aggregate(arrive.austin$ArrDelay, by = list(arrive.austin$date),FUN=mean,na.rm=TRUE)
plot(agg.arr,type='l' ,xlab = "Date", ylab = "Average Delay of Arrival in minutes")
lines(agg.arr)

summary(depart.austin$ArrDelay)
summary(arrive.austin$ArrDelay)
boxplot(DepDelay~month,data=depart.austin)


