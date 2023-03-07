## 지도 시각화
## 패키지 설치
library(ggiraphExtra)
library(ggplot2)

## 미국 주별 강력 범죄율 정보
head(USArrests)
str(USArrests)
library(tibble)
crime <- rownames_to_column(USArrests,var='state')
head(crime)

## state컬럼을 전부 소문자로
crime$state<-str_to_lower(crime$state)

## 지도 데이터 패키지
library(maps)

states_map <-map_data('state')
View(states_map)

ggChoropleth(
  data=crime,
  aes(fill=Murder,
      map_id = state),
  map = states_map,
  interactive = TRUE
)



