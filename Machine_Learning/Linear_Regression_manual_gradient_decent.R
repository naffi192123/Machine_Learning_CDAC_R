#setting the working directory
print(getwd())
setwd("/home/basit/Desktop/Machine_Learning")
print(getwd())

#learning rate and number for epoches
alpha = 0.01
epoches = 1000
#loading the data
data_set <- read.csv("USA_Housing.csv")
#head(data_set)
#parameters
theta0 = 0.5
theta1 = 0.5


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
hypotheses <-function(theta0,theta1,x){

  y <- theta0+theta1*x
  return(y)
}
#partial derivatives
dtheta0<-function(theta0,theta1){
  s=0
  for(i in 1:n){
    t<-(hypotheses(theta0,theta1,x[i])-y[i])
    s<-s+t
  }
  c<-s/n
  return(c)
}
dtheta1<-function(theta0,theta1){
    s=0
    for(i in 1:n){
      t<-(hypotheses(theta0,theta1,x[i])-y[i])*x[i]
      s<-s+t
    }
    c<-s/n
    return(c)

}

#Gradient Decent
gradient<-function(alpha,epoches){
  for(i in 1:epoches){
    temp1<-theta0-alpha*(dtheta0(theta0,theta1))
    temp2<-theta1-alpha*(dtheta1(theta0,theta1))
    theta0<-temp1
    theta1<-temp2
  }
  theta_values<-c(theta0,theta1)
  return(theta_values)
}
# array of calculated parameters
a<-gradient(alpha,epoches)
