#prepare data for exploratory analysis
library(dplyr)
data<-read.table('~/R/household_power_consumption.txt',header = TRUE,sep=';',stringsAsFactors = FALSE)#read table
data$datetime<-paste(data$Date,data$Time)
data<-filter(data,Date %in% c('1/2/2007','2/2/2007')) #filter dates of interest
newdata<-data[,c(10,3,4,5,6,7,8,9)] #rearrange coloumn orders
newdata1<-transform(newdata,datetime=strptime(newdata$datetime,'%d/%m/%Y %H:%M:%S')) #convert to date time format
newdata1[2:8]=as.numeric(unlist(newdata1[2:8])) #convert variables to numeric data
newdata1$weekday<-weekdays.POSIXt(newdata1$datetime,abbreviate = TRUE) #confirm the dates are Thursdays and Fridays

#plot1
quartz()
hist(newdata1$Global_active_power,col = 'red', main ='Global Active Power',xlab='Global Active Power (kilowatts)',ylim=c(0,1200))
dev.copy(png,'~/R/plot1.png',height=480,width=480)
dev.off()

#plot2 (see plot1.R for data preparation steps)
quartz()
plot(newdata1$datetime,newdata1$Global_active_power,type='l',ann = FALSE)
title(ylab = 'Global Active Power (kilowatts)')
dev.copy(png,'~/R/plot2.png',width=480,height=480)
dev.off()

#plot3 (see plot1.R for data preparation steps)
quartz()
par(mar=c(8,6,4,2),bg='white') #set margins and backgorund color
plot(newdata1$datetime,newdata1$Sub_metering_1,type='l',ann=FALSE, ylim=c(0,40))
par(new=T) #tell R to build upon the previous plot
plot(newdata1$datetime,newdata1$Sub_metering_2,type='l',col='red',ylim=c(0,40),ann=FALSE,axes = FALSE) #set ylim the same as previous plot and remove annotation and axes so they don't overlap with
par(new=T)
plot(newdata1$datetime,newdata1$Sub_metering_3,type='l',col='blue' ,ylim=c(0,40),ann=FALSE,axes = FALSE)
title(ylab = 'Energy sub metering')
legend('topright',cex=1.2,y.intersp =1,text.width = strwidth('Sub_metering_1')*1.5,lty=1,col=c('black','red','blue'),legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'))#adjust lengend width and spacing
dev.copy(png,'~/R/plot3.png',width=480,height=480)
dev.off()

#plot4 (see plot1.R for data preparation steps)
quartz()
par(mar=c(8,6,2,2),oma=c(2,1,1,1),bg='white',mfrow=c(2,2),las=3) #set margins/outermargins,bg color and layout
# first plot
plot(newdata1$datetime,newdata1$Global_active_power,type='l',ylab='Global Active Power',xlab='')
# second plot
plot(newdata1$datetime,newdata1$Voltage,type='l',xlab = 'datetime',ylab='Voltage')
# third plot
plot(newdata1$datetime,newdata1$Sub_metering_1,type='l',ann=FALSE)
par(new=T)
plot(newdata1$datetime,newdata1$Sub_metering_2,type='l',col='red',ylim=c(0,40),ann=FALSE,axes = FALSE)
par(new=T)
plot(newdata1$datetime,newdata1$Sub_metering_3,type='l',col='blue' ,ylim=c(0,40),ann=FALSE,axes = FALSE)
title(ylab = 'Energy sub metering')
legend('topright',cex=0.4,y.intersp =1,text.width = strwidth('Sub_metering_1')/2,lty=1,col=c('black','red','blue'),legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'))
# fourth plot
plot(newdata1$datetime,newdata1$Global_reactive_power,type='l',yaxt='n',ylab='Global Reactive Power',xlab='datetime') #suppress y axis ticks
axis(2,at=seq(0,0.5,by=0.1),cex.axis=0.7) #add in y axis and make ticks smaller
dev.copy(png,'~/R/plot4.png',width=480,height=480)
dev.off()
