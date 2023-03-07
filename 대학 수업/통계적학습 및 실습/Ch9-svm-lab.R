
# Lab: Support Vector Machines


## Support Vector Classifier

###
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))
###
dat <- data.frame(x = x, y = as.factor(y))
library(e1071)
svmfit <- svm(y ~ ., data = dat, kernel = "linear", 
    cost = 10, scale = FALSE)
aa<-colSums(c(svmfit$coefs)*svmfit$SV)
## beta 추정값  aa, rho는 intercept
aa 
bound_function <- function(x)-svmfit$rho+x%*%aa
pred<-as.numeric(levels(svmfit$fitted))[svmfit$fitted]
cbind(sign(bound_function(x)),-pred)
dx<-function(x)
abs(-svmfit$rho+x%*%aa)/sqrt(sum(aa^2))
margin = max(dx(svmfit$SV))
## 마진의 높이 h 
margin_h = margin*sin(atan(aa[1]/aa[2]))
### svm plot
plot(x,col=(3-y))
abline(a=svmfit$rho/aa[2],b=-aa[1]/aa[2])
abline(a=svmfit$rho/aa[2]+margin_h/aa[2],b=-aa[1]/aa[2],
       lty=2,col=2)
abline(a=svmfit$rho/aa[2]-margin_h/aa[2],b=-aa[1]/aa[2],
       lty=2,col=2)
###
plot(svmfit,dat)
### 몇번째 인덱스가 서포트 벡터로 사용되었는지 알수있다.
svmfit$index
###7개 서포트백터로 선택됨 빨간색4개 노란색3개
summary(svmfit)
### C값을 0.1로 줄이면 마진이 넓어진다.
svmfit <- svm(y ~ ., data = dat, kernel = "linear", 
    cost = 0.1, scale = FALSE)
plot(svmfit, dat)
### 서포트 벡터가 많아짐
svmfit$index
###  교차검증 함수 tune으로 c값을 다양하게 적합해본다
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", 
    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
### cost = 0.1 일때 가장 오분류율이 낮다
summary(tune.out)
###
bestmod <- tune.out$best.model
summary(bestmod)
###
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
###
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
##오분유율
3/20
###
svmfit <- svm(y ~ ., data = dat, kernel = "linear", 
    cost = .01, scale = FALSE)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
### 수업 안한내용
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)
###
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", 
    cost = 1e5)
summary(svmfit)
plot(svmfit, dat)
###
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

## Support Vector Machine

###비선형 데이터
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
###
plot(x, col = y)
### 추가적인 파라미터 감마 r 설정 radial 커널
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial",  
    gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
###
summary(svmfit)
### C를 크게하면 마진이 작아진다.
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", 
    gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])
### tune 함수로 하이퍼파라미터를 찾기위한 교차검증 
### 감마와 c 를 찾는다
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train, ], 
    kernel = "radial", 
    ranges = list(
      cost = c(0.1, 1, 10, 100, 1000),
      gamma = c(0.5, 1, 2, 3, 4)
    )
  )
summary(tune.out)
### best parameter cost=1, gamma = 0.5
table(
    true = dat[-train, "y"], 
    pred = predict(
      tune.out$best.model, newdata = dat[-train, ]
      )
    )
# 오분류율 12%
## ROC Curves

###
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
###
svmfit.opt <- svm(y ~ ., data = dat[train, ], 
    kernel = "radial", gamma = 2, cost = 1, 
    decision.values = T)
fitted <- attributes(
    predict(svmfit.opt, dat[train, ], decision.values = TRUE)
  )$decision.values
###
par(mfrow = c(1, 2))
rocplot(-fitted, dat[train, "y"], main = "Training Data")
###
svmfit.flex <- svm(y ~ ., data = dat[train, ], 
    kernel = "radial", gamma = 50, cost = 1, 
    decision.values = T)
fitted <- attributes(
    predict(svmfit.flex, dat[train, ], decision.values = T)
  )$decision.values
rocplot(-fitted, dat[train, "y"], add = T, col = "red")
###
fitted <- attributes(
    predict(svmfit.opt, dat[-train, ], decision.values = T)
  )$decision.values
rocplot(-fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(
    predict(svmfit.flex, dat[-train, ], decision.values = T)
  )$decision.values
rocplot(-fitted, dat[-train, "y"], add = T, col = "red")

###
svmfit.opt <- svm(yy ~ ., data = dat, kernel = "radial",  gamma = 2, cost = 1, probability = T)
probs <- attributes(predict(svmfit.opt, dat, probability = T))$probabilities[, 1]
###
par(mfrow = c(1, 2))
rocplot(probs, dat$yy, main = "Training Data")
###
svmfit.flex <- svm(yy ~ ., data = dat, kernel = "radial", gamma = 50,   cost = 1, probability = T)
probs <- attributes(predict(svmfit.flex, dat, probability = T))$probabilities[, 1]
rocplot(probs, dat$yy, add = T, col = "red")
###
dat.test <- data.frame(xx = x[-train, ], yy = as.factor(y[-train]))
probs <- attributes(predict(svmfit.opt, dat.test, probability = T))$probabilities[, 1]
rocplot(probs, dat.test$yy, main = "Test Data")
probs <- attributes(predict(svmfit.flex, dat.test, probability = T))$probabilities[, 1]
rocplot(probs, dat.test$yy, add = T, col = "red")

## SVM with Multiple Classes

### 다중분류 일대일방법
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))
###
svmfit <- svm(y ~ ., data = dat, kernel = "radial", 
    cost = 10, gamma = 1)
plot(svmfit, dat)

## Application to Gene Expression Data

###
library(ISLR2)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
###
table(Khan$ytrain)
table(Khan$ytest)
###
dat <- data.frame(
    x = Khan$xtrain, 
    y = as.factor(Khan$ytrain)
  )
out <- svm(y ~ ., data = dat, kernel = "linear", 
    cost = 10)
summary(out)
table(out$fitted, dat$y)
###
dat.te <- data.frame(
    x = Khan$xtest, 
    y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
###
