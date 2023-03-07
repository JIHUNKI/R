### resamplng methods

## Cross Validation and Bootstrap

library(ISLR2)
set.seed(1)
train<-sample(392,196)
test<-setdiff(1:392,train)

lm.fit<-lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
mean((mpg[-train]-predict(lm.fit,Auto[-train,]))^2)

lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,
            subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto,
            subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

### seed 를 바꿔 적용 해본다.
set.seed(2)
train<-sample(392,196)
lm.fit<-lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2<-lm(mpg~poly(horsepower,2),data=Auto,
            subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto,
            subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


### LOOCV
library(boot)
glm.fit<-glm(mpg~horsepower,data=Auto,)
coef(glm.fit)
cv.err<-cv.glm(Auto,glm.fit)
cv.err$K    ## 교차검증회수 k=defalt Loocv ,k=10 10-foldcv
cv.err$delta ## 첫값은 일반전익 cverr 2두번째값은 adj err

cv.error<-c()
tic<-Sys.time()
for(i in 1:10){
  glm.fit<-glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]<-cv.glm(Auto,glm.fit)$delta[1]
}
toc<-Sys.time()
toc-tic         ## 코딩이 돌아가는 시간이 나옴
cv.error  

##10-fold vaildation
cv.error.10<-c()
tic<-Sys.time()
for(i in 1:10){
  glm.fit<-glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]<-cv.glm(Auto,glm.fit,K=10)$delta[1]
}
toc<-Sys.time()
toc-tic         ## 코딩이 돌아가는 시간이 나옴
cv.error.10  
 
###
loocv<-function(formula,data){
  lm.fit<-lm(formula,data)
  y<-lm.fit$model[,1]
  yhat<-lm.fit$fitted.values
  h<-hatvalues(lm.fit)
  m<-(y-yhat)/(1-h)
  return(mean(m^2))
}
cv.error2<-c()
for(i in 1:10){
  cv.error2[i]<-loocv(mpg~poly(horsepower,i),data=Auto)
}
cv.error2

### BOOTSTRAP

alpha.fn<-function(data,index){
  X<-data$X[index]
  Y<-data$Y[index]
  (var(Y)-cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y))
}
alpha.fn(Portfolio,1:100)

set.seed(12)
alpha.fn(Portfolio,sample(100,100,replace = T))

boot(Portfolio,alpha.fn,R=1000)

##
boot2<-function(data,fun1,R,seed=1){
  set.seed(seed)
  n = dim(data)[1]
  original = fun1(data,1:n)
  boot_res = replicate(R,fun1(data,sample(n,n,replace = T)))
  bias = mean(original-boot_res)
  std.err = sd(boot_res)
  result= c(original,bias,std.err)
  names(result) = c("original","bias","std.err")
  return(result)
  }

boot2(Portfolio,alpha.fn,R=1000,seed=12)


##
boot.fn<-function(data,index)
  coef(lm(mpg~horsepower,data=data,subset=index))
boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)

## 이차항까지 적용
boot.fn.2<-function(data,index)
  coef(lm(mpg~poly(horsepower,2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn.2,1000)
summary(lm(mpg~poly(horsepower,2),data=Auto))$coef
