#setting the working directory
print(getwd())
setwd("/home/basit/Desktop/Machine_Learning")
print(getwd())

#learning rate and number for epoches
alpha = 0.01
epoches = 500
#loading the data
data_set <- read.csv("USA_Housing.csv")
#head(data_set)
#parameters
theta0 = 0.5
theta1 = 0.5
theta2 = 0.5
theta3 = 0.5


print(data_set)
typeof(data_set)
#selecting features in from dataset
data_set$
#selecting average number of rooms as independent variable
x1<-data_set$Avg..Area.Number.of.Rooms
#selecting average number of rooms as independent variable
x2<-data_set$Avg..Area.House.Age
#selecting average number of rooms as independent variable
x3<-data_set$Avg..Area.Number.of.Bedrooms
#selecting price as dependent variable
y<-data_set$Price
sub_set <-c(x1,x2,x3,y)
sub_set<-list(sub_set)
plot(data_set)
sub_set<-subset(data_set,select = c(Avg..Area.Number.of.Rooms,Avg..Area.House.Age,Avg..Area.Number.of.Bedrooms,Price))
#scaling features
x1<-x1/max(x1)
x2<-x2/max(x2)
x3<-x3/max(x3)
y<-y/max(y)

#computing number of samples
n <- length(x1)

#my hypothesis
hypotheses <-function(theta0,theta1,theta2,theta3,x1,x2,x3){
  
  y <- theta0+theta1*x1+theta2*x2+theta3*x3
  return(y)
}
#partial derivatives of cost function with respect to each parameter
dtheta0<-function(theta0,theta1,theta2,theta3){
  s=0
  for(i in 1:n){
    t<-(hypotheses(theta0,theta1,theta2,theta3,x1[i],x2[i],x3[i])-y[i])
    s<-s+t
  }
  c<-s/n
  return(c)
}
dtheta1<-function(theta0,theta1,theta2,theta3){
  s=0
  for(i in 1:n){
    t<-(hypotheses(theta0,theta1,theta2,theta3,x1[i],x2[i],x3[i])-y[i])*x1[i]
    s<-s+t
  }
  c<-s/n
  return(c)
  
}
dtheta2<-function(theta0,theta1,theta2,theta3){
  s=0
  for(i in 1:n){
    t<-(hypotheses(theta0,theta1,theta2,theta3,x1[i],x2[i],x3[i])-y[i])*x2[i]
    s<-s+t
  }
  c<-s/n
  return(c)
  
}
dtheta3<-function(theta0,theta1,theta2,theta3){
  s=0
  for(i in 1:n){
    t<-(hypotheses(theta0,theta1,theta2,theta3,x1[i],x2[i],x3[i])-y[i])*x3[i]
    s<-s+t
  }
  c<-s/n
  return(c)
  
}
#cost function
cost<- function(theta0,theta1,theta2,theta3){
  s = 0
  
  for (i in 1:n){
    t = (hypotheses(theta0,theta1,theta2,theta3,x1[i],x2[i],x3[i])-y[i])**2
    s <- s+t
  }
  c = s/(2*n)
  return(c)
}
#costs array
costs <- c()
#Gradient Decent

  for(i in 1:epoches){
    temp1<-theta0-alpha*(dtheta0(theta0,theta1,theta2,theta3))
    temp2<-theta1-alpha*(dtheta1(theta0,theta1,theta2,theta3))
    temp3<-theta2-alpha*(dtheta2(theta0,theta1,theta2,theta3))
    temp4<-theta3-alpha*(dtheta3(theta0,theta1,theta2,theta3))
    theta0<-temp1
    theta1<-temp2
    theta2<-temp3
    theta3<-temp4
    #calculating cost value for each iteration
    cost_value <-cost(theta0,theta1,theta2,theta3)
    
    costs <- append(costs,cost_value,length(costs))
  }
  #Array containing updated parameters
  theta_values<-c(theta0,theta1,theta2,theta3)
  no_ofiterations <- seq(1,epoches,1)
# array of calculated parameters
plot(no_ofiterations,costs,col="blue")
plot(x,y)
