
# Lab: Linear Models and Regularization Methods


## Subset Selection Methods


### Best Subset Selection

###
library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
###
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
###
library(leaps)   ### 최상의 부분선택법 rss값을 기준으로 함
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)
###
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)  ## 최대 19개 까지 변수를 선택하라
reg.summary <- summary(regfit.full)
reg.summary
###
names(reg.summary)
###
reg.summary$rsq   ## 결정계수
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
    ylab = "Cp", type = "l")
cp.min<-which.min(reg.summary$cp)
points(cp.min, reg.summary$cp[cp.min], col = "red", cex = 2,
    pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
    ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
    pch = 20)
###
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
coef(regfit.full, 6)

### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="r2")
plot(regfit.fwd,scale="adjr2")
plot(regfit.fwd,scale="bic")
plot(regfit.fwd,scale="Cp")

regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)
###
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


### Choosing Among Models Using the Validation-Set Approach and Cross-Validation

###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
    replace = TRUE)
test <- (!train)
###
regfit.best <- regsubsets(Salary ~ .,
    data = Hitters[train, ], nvmax = 19)
###
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
###

val.errors <-c()
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 7)
### regsubset 에 predict함수가 적용되지않아 새로운 함수를 만든다.
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }
###  옳은 방법이아님
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(regfit.best, 7)

## 변수를 알고 다시 선형회귀를 한다.
##방법. 1
regfit7<-lm(Salary~ Hits+ Walks+CAtBat+CHits+CHmRun+Division+PutOuts,data=Hitters)
coef(regfit7)
##방법.2
xvars<-names(coef(regfit.best,7)[-1])
xvars[6]<-"Division"
xvars<-c(xvars,"Salary")
ind<-sapply(1:length(xvars),function(i) which(xvars[i] == names(Hitters)))
regfit72<-lm(Salary~.,data=Hitters[,ind])
coef(regfit72)


###
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
folds
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))
###
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
       data = Hitters[folds != j, ],
       nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
         mean((Hitters$Salary[folds == j] - pred)^2)
   }
}
cv.errors
###
mean.cv.errors <- apply(cv.errors, 2, mean)  ## 1은 행별로 계산 2 열별로 계산 
mean.cv.errors
mean.cv.errors <- colMeans(cv.errors)
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

###좋은 방법은 아님
reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)
###


## Ridge Regression and the Lasso

###
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

### Ridge Regression

###
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
grid
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)  ## alpha=0 ridge모델
###
dim(coef(ridge.mod))
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
###
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
###
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
###
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
mean((mean(y[train]) - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
    exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train])[1:20, ]
### 가장 좋은 방법 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

### The Lasso

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = bestlam)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]

## PCR and PLS Regression


### Principal Components Regression

###
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE,
    validation = "CV")
###
summary(pcr.fit)
###
validationplot(pcr.fit, val.type = "MSEP")
###
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train,
    scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
###
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)
mean((pcr.pred - y.test)^2)
###
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 5)
names(pcr.fit)
summary(pcr.fit)

### Partial Least Squares

###
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test)^2)
###
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE,
    ncomp = 1)
summary(pls.fit)
###
