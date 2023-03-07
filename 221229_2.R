## csv 파일 로드
## csv폴더 이동 ->  csv파일
df = read.csv("./csv/csv_exam.csv")

head(df)
head(df,2)
tail(df)
tail(df,2)

View(df)

## 데이터프레임의 크기를 출력
dim(df)

# 데이터프레임의 속성값 출력
str(df)

# 통계 요약 정보 촐력
summary(df)
summary(df$math)

## dplyr 패키지
library(dplyr)
head(df,1)
## class 컬럼의 이름을 grade 변경
rename(df,grade = class)

## 새로운 파생변수 전체 점수의 합계
df <-df %>% mutate(total = math+english+science)
head(df,1)

# mean 이라는 컬럼을 생성하여 점수의 평균 삽입
df <- df%>% mutate(mean = round(total/3,2))
head(df)
df$mean

## 성적의 평균 점수가 60점 이상이면 pass, 아니면 fail
## res 컬럼 생성
df$res <- ifelse(df$mean >=60,'pass','fail')

## dplyr 에 있는 내장 함수 사용
## python을 기준으로 라이브러리의 함수 사용하려면?
## 라이브러리명.함수명 ()
## R에서는 패키지 내의 함수를 어떻게 사용하는가?

df = read.csv("./csv/csv_exam.csv")
## filter()
df %>% filter(class==1)
## arrange() -기본 값 오름차순 정렬  , 내림차순 arrange(desc(col))
df%>%arrange(math)
df%>%arrange(desc(math))
df%>%arrange(-math)

## 정렬 기준을 여러개
df%>%arrange(class,math)

## class를 기준으로 오름차순, 영어성적을 기준으로 내림차순
df %>% arrange(class,desc(english))

## 특정 컬럼만 추렭
df %>%select(class)
df %>% select (class,math,english)

df%>%select(!class)

## 컬럼의 범위를 지정
df%>% select(math : science)

# 클래스를 기준으로 그룹화 , 수학과 영어의 평균
df %>%group_by(class)%>%summarise(mean_math = mean(math,na.rm=T),
                                  mean_english = mean(english,na.rm=T))

## 컬럼 생성
df %>% mutate(total = math+english+science,
              mean = (math+english+science)/3)

## class를 기준으로 오름차순 정렬 
## class 부터 english까지의 데이터를 출력
df%>% arrange(class)%>%select(class:english)

## 내장함수로 표현
## 정렬
order(df$class)
df[order(df$class),2:4]
df[order(df$class),][c("class","math","english")]


## join 함수
df_1 = data.frame(id = 1:5,score= c(80,70,80,90,85))
df_2 = data.frame(id = 1:5,weight=c(89,70,75,65,60))
df_3 = data.frame(id = 1:3,class=c(1,1,2))

inner_join(df_1,df_2,by='id')
inner_join(df_1,df_3,by='id')
left_join(df_1,df_3,by='id')
right_join(df_1,df_3,by='id')

##bind_row() 데이터프레임에 행을 추가하는 함수
## 데이터프레임간의 유니언 결합

bind_rows(df_1,df_2)
cbind(df_1,df_2)
bind_cols(df_1,df_2)

