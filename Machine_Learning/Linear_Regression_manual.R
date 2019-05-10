#setting the working directory
print(getwd())
setwd("/home/basit/Desktop/Machine_Learning")
print(getwd())


#loading the data
data_set <- read.csv("USA_Housing.csv")
#head(data_set)


#selecting the theta0 = 0
#values for theta1
param <-seq(-10,10,0.1)

#selecting features in from dataset
#selecting average number of rooms as independent variable
x<-data_set$Avg..Area.Number.of.Rooms
#selecting price as dependent variable
y<-data_set$Price

#scaling features
x<-x/max(x)

print(range(y))
y<-y/max(y)

#computing number of samples
n <- length(x)

#my hypothesis  
hypotheses <-function(theta1,x){

  y <-theta1*x
  return(y)
}

#cost function
cost<- function(theta1){
  s = 0

  for (i in 0:n-1){
      temp = (hypotheses(theta1,x[i])-y[i])**2
      s <- sum(temp)
  }
  c = s/(2*n)
  return(c)
}

#computing cost values for each theta1
costs <- c()
for(i in param){
  costs <- append(costs,cost(i),length(costs))
}

#ploting graph of cost values vs theta1 values
plot(param,costs, col='red')

#sctter plot of depedent and independet variables 
plot(x,y,col = "blue")

#choosing appropriate theta-1(slope)
theta<-param[which.min(costs)]

#ploting regression line
abline(0.265,0.361,col= "red")
