setwd("E:/Fall2020/ISL-1/Lab/Lab1")
print(getwd())
#1. a)
College=read.csv(file="College.csv", header=TRUE, sep=",")
print(College)
#1. b)
rownames(College)=College[,1]
fix(College)
#Eliminate first column in data
College=College[,-1]
fix(College)
#1. c)
#I.summary function to produce summary of a variables in dataset
summary(College)
#II. scatter plot of first ten columns
pairs(College[,1:10])
#III. plot of outstate vs private
plot(College$Private,College$Outstate,col=c("blue","green"))
#IV) creating new variable and dividing universities into two groups
Elite =rep ("No",nrow(College ))
Elite [College$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
College =data.frame(College ,Elite)
summary(College$Elite)
plot(College$Elite,College$Outstate,col=c("blue","green"))
# V) Histogram with bins for some variables
par(mfrow=c(2,2))
hist(College$Accept,main="Number of applications accepted",col="red",breaks=50)
hist(College$Enroll,main="Number of new students enrolled",col="green",bin=100)
hist(College$PhD,main="Percent of faculties with PhD",col="blue",breaks=20)
hist(College$perc.alumni,main="Percent of alumini donate",col="yellow")
summary(College$Apps)
# Vi) Exploring data and summary of discovery
College<-College[order(College$PhD),]
head(data.frame(College=row.names(College),College$PhD),n=15)
#2.
Auto=read.csv(file="Auto.csv", header=TRUE,na.strings = "?")
Auto=na.omit(Auto)
dim(Auto)
summary(Auto)
#a.qualitative and quantitative predictors
range(Auto$mpg)
range(Auto$weight)
summary(Auto[,c(7:9),])
summary(Auto[,c(1:6),])
#b.range of all quantitative predictor
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
#c. mean and SD of quantitative predictors
sapply(Auto[,1:6],function(x) signif(c(mean(x),sd(x),2)))
#d. removing 10-85th observations and range,mean,sd of remaining predictors
new.auto=subset(Auto[-c(10:85),])
sapply(new.auto[,-c(9)],range)
sapply(new.auto[,-c(9)],mean)
sapply(new.auto[,-c(9)],sd)
#e. Investigating predictors graphically
pairs(Auto,panel=panel.smooth,main="scatter plots of all pairs of variables")
plot(as.factor(Auto$cylinders),Auto$mpg)
plot(Auto$mpg,Auto$weight)
plot(factor(Auto$origin),Auto$mpg,names=(c("American","European","Japanese")))
#f.plots suggested for predicting mpg
plot(as.factor(Auto$cylinders),Auto$mpg)
pairs(~ mpg + horsepower + weight + displacement, data = Auto, panel = panel.smooth)
3.
#1.Boston dataset from MASS library
?Boston
data(Boston)
summary(Boston)
str(Boston)
#2.pairwise scatterplots of the predictors 
pairs(Boston)
par(mfrow=c(2,2))
plot(Boston$zn,Boston$indus)
plot(Boston$zn,Boston$dis)
plot(Boston$zn,Boston$rad)
#3.predictors associated with per capita crime rate
plot(as.factor(Boston$chas),Boston$crim)
plot(Boston$dis,Boston$crim)
pairs(Boston[Boston$crim<20,])
#d.
par(mfrow=c(2,2))
hist(Boston$crim,main="crime rates",breaks="FD")
hist(Boston$crim,main="crime rates with y-axis limited",ylim=c(0,40),breaks ="FD")
hist(Boston$tax,main="Tax rates",breaks="FD")
hist(Boston$ptratio,main="pupil-teach ratio",breaks="FD")

plot(as.factor(Boston$chas),Boston$tax)

range(Boston$crim)
hist(Boston$crim,breaks=50)
# e)
table(Boston$chas)
#f) 
median(Boston$ptratio)
#g)
plot(as.factor(Boston$chas),Boston$medv)
which.min(Boston$medv)
Boston[which.min(Boston$medv),]
summary(Boston$crim)
#h)
summary(Boston$rm)
table(Boston$rm > 7)
table(Boston$rm >8)
rooms8 = Boston[Boston$rm > 8, ]
summary(rooms8)
table(rooms8$chas)
summary(rooms8$black)
summary(Boston$black)