## 결측치에 대한 계산
## 결측치는 연산이 되지 않는다.
## 필터를 걸게되면 무조건 출력.

c1 = c(1,2,NA,NA,5)
c2 = c(1,2,3,4,5)
c3 = c(NA,2,3,4,5)
df = data.frame(c1,c2,c3)
df

## 결측치가 존재하는지 채크
is.na(df)
table(is.na(df))
is.na(df$c1)


## 결측치를 제거하는 방법
library(dplyr)
df %>% filter(is.na(c1))
df %>% filter(!is.na(c1))
na.omit(df)

## 결측치를 제외하고 연산

mean(df$c1)
mean(df$c1,na.rm=TRUE)

exam = read.csv("./csv/csv_exam.csv")
exam


exam[c(5,7),3] = NA

table(is.na(exam))
dim(exam)
str(exam)

## 수학 성적 평균 출력(NA)
##결측치인 값을 수학 성적의 평균으로 대체
## ifelse (조건식, 참일때 값 , 거짓일때 값)
## 조건식을 결측치인 경우 , 결측치를 제외한 수학평균, 자기자신의 값

exam$math = ifelse(is.na(exam$math),mean(exam$math,na.rm=T),exam$math)


## 이상치 제거

outlier = data.frame(gender=c(1,2,1,2,3),
                     score= c(80,90,70,80,50))
outlier
table(outlier$gender)

## gender 3이라는 값은 이상치
## 성별이 3인 데이터를 결측치로 변경
## 결측치 제거
outlier[outlier$gender==3,]$gender = NA
na.omit(outlier)

## 성별이 1,2,가 아닌경우 NA
ifelse(outlier$gender %in% c(1,2), outlier$gender,NA)

outlier$gender = ifelse(outlier$gender==1 | outlier$gender==2,outlier$gender,NA)

## 성별로 그룹화
## 결측치 제거
## score변수의 평균값
## dplyr 패키지를 이용하여
outlier |>
  filter(!is.na(gender))|>
  group_by(gender)|>
  summarize(score_mean = mean(score,na.rm=T))

library(ggplot2)
data(mpg)

## 극 단치 (시각화)
ggplot(data=mpg)+
  stat_boxplot(mapping = aes(hwy))+
  coord_flip()
## 극단치 확인(수치화)
boxplot(mpg$hwy)$stats

## 극단치 37초과이거나 12의 미만인 경우 결측치로 대체
## 결측치 제거
## manufacturer 기준으로 그룹화
## hwy의 평균 값을 출력
mpg$hwy <- ifelse(mpg$hwy>37 | mpg$hwy <12,NA,mpg$hwy)
mpg <-na.omit(mpg)
mpg |>  
  group_by(manufacturer)|>
  summarise(hwy_mean = mean(hwy,na.rm=T))|>
  arrange(desc(hwy_mean))|>
  head(5)

mpg = ggplot2 :: mpg
## 파생변수 추가
## total -> (도심연비 + 고속도로 연빈)/2
## 파생변수 추가
## test -> total의 값이 30 이상이면"A"
## 20이상이면 "B" 그 외에는 "C"

mpg_1 <- mpg |>
        mutate(total = (cty + hwy)/2)

mpg_1["test"] <- ifelse(mpg_1$total>=30,"A",ifelse(mpg_1$total>=20,"B","C"))
table(mpg_1$test)

## 간단한 시각화
qplot(mpg_1$test)


## ggplot2 midwest라는 샘플로드
data("midwest")
head(midwest)

## 컬럼의 이름 변경
## rename(df,세컬럼의 이름 = 변경이 될 컬럼의 이름)
## popasian 컬럼을 -> asian, poptotla -> total
midwest <- rename(midwest,asian = popasian,total = poptotal)

## 파생변수 ratio
## 전체 인구수 대비 아시아의 인구수 -> 백분율 표시
## (asian / total)*100 -> ratio
midwest |> 
  mutate(ratio  = (asian / total)*100)|>
  select(total,asian,ratio)

midwest= midwest |> 
  mutate(ratio  = (asian / total)*100)

## 파생변수 생성
## group변수는 ratio의 평균값 보다 ratio의 깂이 초과면 'large' 이하면 'small'

midwest <- midwest|> 
    mutate(group = ifelse(ratio > mean(ratio),"large","small"))
table(midwest$group)
qplot(midwest$group)
