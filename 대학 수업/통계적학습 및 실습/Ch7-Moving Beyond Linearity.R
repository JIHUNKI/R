
# Lab: Non-linear Modeling

###
library(ISLR2)
attach(Wage)

## Polynomial Regression and Step Functions

###
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
###
fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))
###
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
            data = Wage)
coef(fit2a)
###
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4),
            data = Wage)
coef(fit2b)
### se=TRUE 표준오차도 제공하도록 명시
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid),
                 se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit,
                  preds$fit - 2 * preds$se.fit)
### 4차 다항식 적합을 추가
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
### 기저함수들의 직교집합이 poly()함수에서 생성된 것인지의 여부는 
### 모델에 별 영향을 미치지않음을 확인 할 수 있다.
preds2 <- predict(fit2, newdata = list(age = age.grid),
                  se = TRUE)
max(abs(preds$fit - preds2$fit))
### 다항식회귀 적합 시 차수결정 하는 방법 1.가설검정
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
### 3차또는 2차다항식은 데이터에 합리적 적합을 할 것같다.
coef(summary(fit.5))
###
(-11.983)^2  ## t 검정통계량 제곱 anova에서 f 검정통계량
### 하지만, ANOVA 방법은 직교 다항식의 사용 여부와 상관없이 동작한다.
### 이 방법은 또한 모델에 다른 항이 있을 때에도 동작한다.
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

###다항식로지스틱 회귀모델 

fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage,
           family = binomial)
### 
preds <- predict(fit, newdata = list(age = age.grid), se = T)
### glm은 type="link"이며 여기서도 로짓계산된다.
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit,
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

### type= "response"를 선택하면 확률을 직접계산할 수 있다.
preds <- predict(fit, newdata = list(age = age.grid),
                 type = "response", se = T)
### 그러나 계산된 확률이 음수가 되므로 해당 신뢰구간은 적절하지 않다.
plot(age, I(wage > 250), xlim = agelims, type = "n",
     ylim = c(0, .2))
## 사전 확률이 wage>250 이 0.2이므로 나타내어준다.
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

###계단함수를 적합하기위해서는cut()함수를 이용한다. 순서형 범주형변수 반환
## 적절한 절단점으로 33.5, 49, 64.5를 선택 (breaks=c(),직접 선택가능)

table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

## Splines\label{Ch7:sec8.2}

### 기저함수들의 적절한 행령을 구성함으로써 회귀 스플라인 적합
### bs()함수는 지정된 매듭집합을 가진 스플라인들에 대한 기저함수들의 행렬 생성
### 기본적으로는 삼차 스플라인이 생성
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
matlines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
matlines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")
### 여기서 매듭은 25,40,60 이것은 6개의 기저함수를 가진 스플라인 제공
### 3개의 매듭을 가진 삼차 스플라인 자유도는 7 (k+4)
### df옵션을 사용하여 데이터의 균등분위수에 매듭을 가지는 스플라인 생성 가능
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

###   ns (자연 스플라인 ) 매듭의수 k = 자유도 = 4 

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid),
                 se = T)
plot(age, wage,col = "gray")
lines(age.grid, pred2$fit, col = "red", lwd = 2)
###  평활스플라인을 적합하기 위해 smooth.spline() 을 사용
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
## df=16으로 지정하면 자유도가 16이 되는 lambda값을 결정
## 교차검증에 의해 평뢀도 수준을 선택 이결과 자유도 6.8이되는 lambda값이 선택
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


## 극소회귀를 수행하기 위해서는 loess()를 사용한다.
### 극소회귀 주변 비율(span)을 설정해야 한다. 주변 비율이 크면 더 부드러워짐
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

## GAMs (일반화 가법 모형) 첫 두변수는 자연스플라인으로 edu변수는 일반적으로

### 첫 두 함수는 자유도가 각각 4, 5인 year와 age의 자연 스플라인이다.

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education,
           data = Wage)
### 자연스플라인이 아닌  gam라이브러리의 s()함수를 사용한다.(자연 스플라인)
###  첫 두 함수는 자유도가 각각 4, 5인 year와 age의 평활 스플라인이다
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
              data = Wage)
### 여기서 gam으로 만들어진 모델은 plot.Gam을 사용해야함을 유념
### year의 함수는 그래프에서 다소 선형적으로 보인다.
par(mfrow = c(1, 3))
plot.Gam(gam1, se = TRUE, col = "red")
###
plot(gam.m3, se = TRUE, col = "blue")

# year를 제외한 M1, year의 선형함수를 사용한 M2,
# 또는 year의 스플라인 함수를 사용한 M3 중 어느 것이 최고인지 결정할 수 있다. 
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education,
              data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
### year변수를 선형적으로나타낸  m2모델이 선호되는 것으로 보인다
summary(gam.m3)
### year변수가 비선형적이라는 p값 또한 0.3537로 결론을 뒷받침해준다.
### 하지만 age변수는 비선형 항이 필요하다는 명백한 증거를 보여준다.
preds <- predict(gam.m2, newdata = Wage)
###  스플라인과   lo()극소회귀를 함께 적용
gam.lo <- gam(
  wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
  data = Wage
)
plot(gam.lo, se = TRUE, col = "green")

# lo()함수를 사용하면 gam()함수를 호출하기 전에 상호작용항들을 만들 수 도 있다.
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education,
                data = Wage)
###
library(akima)
plot(gam.lo.i)

### Wage 자료에서 이진 반응변수 I(wage > 250)에 대한 로지스틱 회귀 GAM
gam.lr <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")
###
table(education, I(wage > 250))
###결과를 보면 < HS 범주에는 소득이 높은 사람이 없다는것을 알수있다(TRUE0)
### 따라서 <HS를 제외한 모든 범주를 사용하여 로지스틱회귀GAM을 적합한다.
gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial, data = Wage,
  subset = (education != "1. < HS Grad")
)
plot(gam.lr.s, se = T, col = "green")

