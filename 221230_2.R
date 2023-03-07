## sav확장자 파일을 로드 패캐지
library(foreign)
library(readxl)
library(ggplot2)
library(tidyverse)
welfare =read.spss(file="./csv/Koweps_hpc10_2015_beta1.sav",to.data.frame = T)
dim(welfare)

welfare_copy = welfare

## 컬럼의 이름을 변경
welfare_copy = rename(welfare_copy,
                      gender = h10_g3,
                      birth = h10_g4,
                      marriage= h10_g10,
                      religion = h10_g11,
                      income = p1002_8aq1,
                      code_job = h10_eco9,
                      code_region = h10_reg7)
# 사용할 컬럼둘만 선택하여 데이터프레임 수정
welfare_copy = welfare_copy %>%select(gender,birth,marriage,religion,
                                      income,code_job,code_region)

# 성별 컬럼의 데이터 개수를 확인

table(welfare_copy$gender)
table(is.na(welfare_copy$gender))

## gender 컬럼의 데이터를 1은 male 2는 female
welfare_copy$gender <-ifelse(welfare_copy$gender==1,"male","female")

## income이 0이면 수익이 존재하지 않는다. -> 결측치
## income이 9999이면 극단치로 생각하여 결측차로 변경
#case1
welfare_copy$income <- ifelse(welfare_copy$income==9999 | welfare_copy$income==0,NA,welfare_copy$income)
#case2
ifelse(welfare_copy$income %in% c(0,9999),NA,welfare_copy$income)
## 결측치 확인
table(is.na(welfare_copy$income))

## 성별을 기준으로 월급의 차이가 존재하는가?
## 시각화를 하여 표현
## 결측치를 제외
gender_income <-welfare_copy |>filter(!is.na(income))|>
  group_by(gender)|>
  summarise(income_mean = mean(income))

## 데이터를 시각화
ggplot(gender_income,aes(x=gender,y=income_mean))+geom_col()

qplot(welfare_copy$gender,welfare_copy$income,xlab = "gender",ylab="income")


ggplot(data=welfare_copy)+
  geom_boxplot(mapping=aes(x=gender,y=income,color=gender))+
  labs(title="income by gender")

## age 파생변수 생성
## age -> 현재년도 - birth
## 나이별 월급이 어떻게 되는가?
## ageg 파생변수 생성
## age 컬럼의 값이 30세미만이면 'young',30이상 60미만이면 'middle',60이상이면 'old'
## 연령대별 월급의 어떻게 되는가?


welfare_copy <-welfare_copy%>%
  mutate(age = 2022-birth,
         ageg = ifelse(age<30,'young',ifelse(age>=30 & age <60,"middle","old")))


(age_income <- welfare_copy%>%
    filter(!is.na(income))%>%
    group_by(age)%>%
    summarise(income_mean = mean(income))%>%
    arrange(desc(age)))


ggplot(data=age_income)+
  geom_col(mapping=aes(x=age,y=income_mean))

ageg_income <-welfare_copy%>%
  filter(!is.na(income))%>%
  group_by(ageg)%>%
  summarise(income_mean=mean(income))

ggplot(data=ageg_income)+
  geom_col(mapping=aes(x=ageg,y=income_mean))


## 코드북 파일 로드
list_job <- read_excel("./csv/Koweps_Codebook.xlsx",sheet = 2)
list_job

# welfare_copy 와 list_job 데이터프레임을 결합
## 두 데이터프레임에 공통인 컬럼의 이름은?
# code_job
# code_job을 기준으로 결합(join 결합)


join_data <- left_join(welfare_copy,list_job,by="code_job")
table(is.na(join_data$job))


##직업별 월급의 차이가 어느정도 나는가?
##직업군 중에 가장 월급을 많이 받는 직군은 어디인가 상위 10개

# 결측치 제거(income,job 두 컬럼의 결측치)
# 직업별 그룹화 
# 수익의 평균

job_income <-join_data%>%filter(!(is.na(income)) & !(is.na(job)))%>%
  group_by(job)%>%
  summarise(mean_income = mean(income))%>%
  arrange(desc(mean_income))

job_income_2 <-join_data%>%filter(!(is.na(income)) & !(is.na(job)))%>%
  group_by(job)%>%
  summarise(mean_income = mean(income))%>%
  arrange(desc(mean_income))%>%
  top_n(10)

ggplot(job_income_2)+
  geom_col(mapping=aes(x=reorder(job,mean_income),y=mean_income))+
  coord_flip() 

ggplot(job_income)+
  geom_col(mapping=aes(x=reorder(job,mean_income),y=mean_income))+
  coord_flip() ->job_income_graph


## plotly 패키지 설치
library(plotly)
ggplotly(job_income_graph)
## dygragh -> 시계열 데이터를 시각화
library(dygraphs)
library(xts)
library(lubridate)
economics
eco = xts(economics$unemploy,order.by = economics$date)

dygraph(eco)%>%
  dyRangeSelector()




