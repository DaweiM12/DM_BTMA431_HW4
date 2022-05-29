library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(car)
library(stats)

########################################
################Question 1#############
########################################

#1a) 0.40
BTMA <- btma.431.736.f2018

Q1A.Model <- lm(BTMA$final.raw.score.excluding.bonus~BTMA$final.project
               +BTMA$post.retake.midterm
               +BTMA$textbook.quiz.average
               +BTMA$HW.average)

Q1A.Model$coefficients[2]

#1b) 
##HW.average coefficient estimate / HW.average standard error /
#textbook.quiz.average coefficient estimate / textbook.quiz.average 

BTMA.ELSE <- data.frame(midterm = BTMA[,2],
                         TB = BTMA[,3],
                         HW = BTMA[,4],
                         Project = BTMA[,1],
                         final = BTMA[,6])

BTMA.ELSE$HW.Percent <- (BTMA.ELSE[,3] / 20)*100 

BTMA.ELSE$TB.Percent <- (BTMA.ELSE[,2] / 15)*100 

BTMA.ELSE <- BTMA.ELSE[,-2:-3]

Q1B.Model <- lm(BTMA.ELSE$final~BTMA.ELSE$Project
                +BTMA.ELSE$midterm
                +BTMA.ELSE$TB
                +BTMA.ELSE$HW)
summary(Q1A.Model)
summary(Q1B.Model)

#1c) 0.36

Q1C <- lm(BTMA$final.raw.score.excluding.bonus~BTMA$final.project
          +BTMA$post.retake.midterm+BTMA$textbook.quiz.average+BTMA$HW.average+BTMA$BANA)

summary(Q1C)$coefficients[6,4]

#1d) 0.86

Q1D <- lm(BTMA$final.raw.score.excluding.bonus~BTMA$final.project
          +BTMA$textbook.quiz.average
          +BTMA$HW.average
          +BTMA$BANA*BTMA$post.retake.midterm)


summary(Q1D)$coefficients[7,4]

#when you look at the actual summary it is 0.86479, however when I only show the Pr(>|t|) it rounds the thousandth 
#which makes it look like it should be 0.87

#1e) 0.38

BTMA.Log <- BTMA[,-5]

BTMA.Log <- log(BTMA.Log)

Q1E <- lm(BTMA.Log$final.raw.score.excluding.bonus~BTMA.Log$final.project
          +BTMA.Log$post.retake.midterm
          +BTMA.Log$textbook.quiz.average
          +BTMA.Log$HW.average)

Q1E$coefficients[2]

########################################
################Question 2#############
########################################

#https://www.youtube.com/watch?v=ZYN0YD7UfK4
#https://www.youtube.com/watch?v=yLvwXccNw2o
#https://rstudio-pubs-static.s3.amazonaws.com/167310_92e4c90ca4284b5fb190e5cecce067b5.html

#2a) 5.50 / 101.25

p <- seq(from = 1, to = 9, by = 0.1)

table1 <- data.frame(Q = as.numeric(), p = as.numeric(), profit = as.numeric())

for(i in 1:length(p)) {
  table1[i,2] <- p[i]
  table1[i,1] <- 50 - 5*(p[i])
  table1[i,3] <- table1[i,1]*table1[i,2] - table1[i,1]
}

table1[which.max(table1$profit),]

#2b) 5.00 / 6.00

p <- seq(from = 1, to = 9, by = 0.1)

table1 <- data.frame(Q = as.numeric(), p = as.numeric(), profit = as.numeric())

for(i in 1:length(p)) {
  table1[i,2] <- p[i]
  table1[i,1] <- 45 - 5*(p[i])
  table1[i,3] <- table1[i,1]*table1[i,2] - table1[i,1]
}

table1[which.max(table1$profit),]

p <- seq(from = 1, to = 9, by = 0.1)

table1 <- data.frame(Q = as.numeric(), p = as.numeric(), profit = as.numeric())

for(i in 1:length(p)) {
  table1[i,2] <- p[i]
  table1[i,1] <- 55 - 5*(p[i])
  table1[i,3] <- table1[i,1]*table1[i,2] - table1[i,1]
}

table1[which.max(table1$profit),]

#2c) - upward sloping line - need to graph. 

opt.price <- data.frame("Q" = as.numeric(),"M" = as.numeric(), "K" = as.numeric(), 
                        "P" = as.numeric(), "profit" = as.numeric())
table2 <- data.frame("Q" = as.numeric(),"M" = as.numeric(), "K" = as.numeric(), 
                     "P" = as.numeric(), "profit" = as.numeric())
table3 <- data.frame()

M <- seq(40,60,1)
P <- seq(1,15,0.1)
K <- seq(2,8,1)

for(i in 1:length(M)) {
  M.temp <- M[i]
  
  for(l in 1:length(P)) {
    table2[l,1] <- M.temp - 5*P[l]
    table2[l,2] <- M.temp
    table2[l,3] <- 5
    table2[l,4] <- P[l]
    table2[l,5] <- table2[l,1]*table2[l,4] - table2[l,1]
  }
  table3 <- table2[which.max(table2$profit),]
  
  opt.price[i,1] <- table3$Q
  opt.price[i,2] <- table3$M
  opt.price[i,3] <- table3$K
  opt.price[i,4] <- table3$P
  opt.price[i,5] <- table3$profit
}

plot(opt.price$P,opt.price$M, type = "l", col = "red", 
     xlab = "Optimal Price",
     ylab = "Demand",
     main = "Optimal Price as a function of Demand") 


#2d) 5.00/6.00

opt.price <- data.frame("Q" = as.numeric(),"M" = as.numeric(), "K" = as.numeric(), 
                        "P" = as.numeric(), "profit" = as.numeric())
table2 <- data.frame("Q" = as.numeric(),"M" = as.numeric(), "K" = as.numeric(), 
                     "P" = as.numeric(), "profit" = as.numeric())
table3 <- data.frame()

P <- seq(1,15,0.1)
K <- seq(2,8,1)
M <- c(45,55)


for(i in 1:length(K)) {
  K.temp <- K[i]
  
  for(j in 1:length(P)) {
    table2[j,1] <- 45 - K.temp*P[j]
    table2[j,2] <- 45
    table2[j,3] <- K.temp
    table2[j,4] <- P[j]
    table2[j,5] <- table2[j,1]*table2[j,4] - table2[j,1]
    
  }
  
  table3 <- table2[which.max(table2$profit),]
  
  opt.price[i,1] <- table3$Q
  opt.price[i,2] <- table3$M
  opt.price[i,3] <- table3$K
  opt.price[i,4] <- table3$P
  opt.price[i,5] <- table3$profit
}

opt45 <- opt.price
opt45[(opt45$P == 5),3]


for(i in 1:length(K)) {
  K.temp <- K[i]
  
  for(j in 1:length(P)) {
    table2[j,1] <- 55 - K.temp*P[j]
    table2[j,2] <- 55
    table2[j,3] <- K.temp
    table2[j,4] <- P[j]
    table2[j,5] <- table2[j,1]*table2[j,4] - table2[j,1]
    
  }
  
  table3 <- table2[which.max(table2$profit),]
  
  opt.price[i,1] <- table3$Q
  opt.price[i,2] <- table3$M
  opt.price[i,3] <- table3$K
  opt.price[i,4] <- table3$P
  opt.price[i,5] <- table3$profit
}

opt55 <- opt.price
opt55[(opt55$K == 5),4]

plot(opt55$P,opt55$K, type = "l", lwd = 3, col = "blue", ylab = "Maringal Impact on D",
     xlab = "Optimal price",
     main = "Impact on optimal price as a result of K") 
lines(opt45$P,opt45$K, type = "l", lwd = 3, col = "red") 


#2e - 3.90

Price.Opt <- function(data, min.price, max.price) {
  
  #this is the polynomial regression where Quantity is the function of price. 
  model.fit <- lm(salesData$quantity ~ salesData$price + I(salesData$price^2))
 
  
  #this set the price range for your product. 
  p <- seq(min.price,max.price,0.01)
  
  
  table10 <- data.frame("p" = as.numeric(), "profit" = as.numeric())
  
  
  for(i in 1:length(p)) {
    table10[i,1] <- p[i]
    
    #this is the quantity equation that find what quantity level you would get based off the polynomial regression
    #and a given price. 
    q <- (model.fit$coefficients[1] + model.fit$coefficients[2] * p[i] + model.fit$coefficients[3] * p[i]^2)
    
    #this determines the profit based
    table10[i,2] <- q * p[i] - q
  }
  
  #returns the price that produced the greatest profit
  return(table10[which.max(table10$profit),])
  
}

Price.Opt(salesDates,1,15)












