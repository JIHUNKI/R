
# Lab: Decision Trees


## Fitting Classification Trees

###
library(tree)
###
library(ISLR2)
attach(Carseats)
##Sales는 연속형 변수이므로 이진변수로 기록
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
###데이터 병합
Carseats <- data.frame(Carseats, High)
###
tree.carseats <- tree(High ~ . - Sales, Carseats)
### summary에서 내부노드변수, 터미널노드 수, 훈련 오차율을 보여준다
## 훈련 오차율 9% ,잔차평균이탈도는 이탈도를 (n-터미널노드)로 나눈 값
summary(tree.carseats)
### pretty = 0 을 사용하면 각 카테고리에 대한 문자를 질적 설명변수이름 포함
plot(tree.carseats)
text(tree.carseats, pretty = 0)
###
tree.carseats
###
set.seed(2)
train <- sample(1:nrow(Carseats), 200)  # 200개 트레인 셋
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats,
    subset = train)
tree.pred <- predict(tree.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(104 + 50) / 200  ## 정분류율 77%
### 가지치기 (pruning),FUN=prune.misclass 이탈도가 아닌 분류오류율을사용교차검증
## size=터미널노드, 오분류율, 사용된 비용-복잡성 매개변수({alpha})에 대응하는 k
set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
### dev값이 가장 작은 9개의 터미널 노드에서 가장 작은 교차검증 오차율은 얻음
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
### prune.misclass함수로 터미널 노드가 9개일때로 가지치기
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
### 가자치기된 나무에 다시 예측
tree.pred <- predict(prune.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(97 + 58) / 200  ## 정분류율 0.775
###
prune.carseats <- prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(102 + 52) / 200

## Fitting Regression Trees

###  사용된 변수가 단지 4개임을 알 수 있다.
### 회귀트리에서 이탈도는 단순히 그 트리에 대한 오차 제곱합이다.
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
### rm이 클수록 lstat이 낮을수록 비싼 집에 해당
plot(tree.boston)
text(tree.boston, pretty = 0)
###
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
###
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
### 검정셋 mse는 35.29, 이다 
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)
### 조금 더 복잡한 트리를 적합 시키려면 
tree.boston_t1 <- tree(medv ~ ., Boston, subset = train,
                       control = tree.control(nobs = length(train), mindev = 0))
plot(tree.boston_t1)
text(tree.boston_t1, pretty = 0)

## Bagging and Random Forests

### 배깅은 단순히 m=p인 랜덤포레스트의 특별한 경우이다.
## 배깅을 적합하기 위해 m=p=12를 적용
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, importance = TRUE)
bag.boston
### 배깅 회귀 트리에 관련된 검정셋의 mse는 23.41로 단일 트리에서 얻은 mse의 2/3
### 이다
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)
### ntree인자를 통해 배깅에 사용되는 트리의 수를 변경할 수 있다.
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)
### 기본적으로 randomForest()는 회귀 트리로 랜덤 포리스트를 만들 때 p/3개의 변수
### 사용하고 분류 트리로의 랜덤 포리스트를 만들 때 sqrt(p) 변수를 사용한다. 
### 여기서는 mtry = 6을 사용한다.
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)
### 배깅 보다 너 낮은 mse를 얻음을 확인 할 수 있다.
importance(rf.boston)
### 변수의 중요도 두가지 척도가 제공된다. 첫 번째는 주어진 변수가 빠졌을 때
### OOB 샘플에 대한 예측의 평균 정확도 감소에 기초한 값.
### 두 번째는 해당 변수에 대한 분할로 인해 발생되는 노드 불순도의 총감소에 대한
### 측정값으로, 모든 트리에 대해 평균한 것이다. 회귀트리는 노드불순도가 RSS에
### 의해 측정되고 분류트리는 이탈도에 의해 측정된다.
varImpPlot(rf.boston)
### lstat,rm변수가 단연 가장 중요한 변수임을 알 수 있다.

## Boosting

### 회귀트리를 다룰 것 이기때문에 분포를 "gaussian"을 지정 
### 이진분류문제는 "bernulli", n.tree=5000,트리의 깊이 4
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4)
### 부스팅 모델에서도 lstat,rm이 가장 중요한 변수임을 알 수 있다.
summary(boost.boston)
###부분 종속성 그래프
## 중위주택가격은 rm이 증가하면 증가하고 ,lstat이 증가하면 감소한다
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
### 검정셋 mse =18.39
yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
### 축소파라미터는 기본값이 0.001 이다 이번에 0.2를 사용
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4, shrinkage = 0.2, verbose = F,cv.folds = 10)
yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

## Bayesian Additive Regression Trees

###  안배운 내용 
library(BART)
x <- Boston[, 1:12]
y <- Boston[, "medv"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
set.seed(1)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)
###
yhat.bart <- bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2)
###
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]
###
