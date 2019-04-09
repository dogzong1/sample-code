getwd()
setwd("D:\\SLU\\example")
getwd()
data = read.csv("card.csv")
data
data = data[,-1]
head(data)
library(leaps)
attach(data)

#method 1 stepwise
small = glm(default.payment.next.month~1)
full = glm(default.payment.next.month~LIMIT_BAL+EDUCATION+AGE+BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6)
step(small,scope=list(lower = small, upper = full),direction = "forward")
step(full,scope = list(lower = small, upper = full),direction = "backward")

out1 = glm(default.payment.next.month ~ LIMIT_BAL + PAY_AMT1 + 
             BILL_AMT5 + AGE + PAY_AMT4 + BILL_AMT1 + BILL_AMT2 + PAY_AMT2 + 
             PAY_AMT3 + EDUCATION + PAY_AMT5 + BILL_AMT6) 
p = out1$fitted.values
predict = rep(0,length(p))
predict[p>0.27]=1
predict
table(predict,default.payment.next.month)

out2 = glm(default.payment.next.month ~ LIMIT_BAL + EDUCATION + 
             AGE + BILL_AMT1 + BILL_AMT2 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + 
             PAY_AMT3 + PAY_AMT4 + PAY_AMT5) 
p = out2$fitted.values
predict = rep(0,length(p))
predict[p>0.25]=1
predict
table(predict,default.payment.next.month)

#method 2Mallow's Cp
x=cbind(LIMIT_BAL,EDUCATION,AGE,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)
colnames(x) <- c("LIMIT_BAL","EDUCATION","AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
out1=leaps(x,default.payment.next.month,method="Cp",nbest=5)
which(out1$Cp==min(out1$Cp))
out1$which[which(out1$Cp==min(out1$Cp)),]
x1 = x[,-c(6,7,8,14,15)]
head(x1)
out2 = glm(default.payment.next.month~x1)
out2
p = out2$fitted.values
p
predict = rep(0,length(p))
predict[p>0.26]=1
predict
table(predict,default.payment.next.month)

#method 3 knn

train = head(data,20000)
train
test = tail(data,10000) 
test

xtrain = train[,-c(2,4,5,6,7,8,9,10,11,14,15,16,22,23,24)]
xtrain = scale(xtrain)
ytrain = train[,24]
ytrain 

xtest = test[,-c(2,4,5,6,7,8,9,10,11,14,15,16,22,23,24)]
xtest = scale(xtest)
ytest = test[,24]
ytest

library(class)
yhat = knn(train=xtrain,  cl=ytrain, test=xtest, k = 1)
table(yhat, ytest)
yhat = knn(train=xtrain,  cl=ytrain, test=xtest, k = 5)
table(yhat, ytest)
yhat = knn(train=xtrain,  cl=ytrain, test=xtest, k = 10)
table(yhat, ytest)
yhat = knn(train=xtrain,  cl=ytrain, test=xtest, k = 20)
table(yhat, ytest)
error = rep(0,50)
error
for (i in 1:50) 
{error[i] = sum(ytrain != knn.cv(xtrain,ytrain,i))/length(ytrain)}
which(error == min(error))
yhat = knn(xtrain, xtest, ytrain, k = 31)
table(yhat, ytest)