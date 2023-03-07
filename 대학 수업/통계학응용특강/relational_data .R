####
library(tidyverse)
library(nycflights13)

flights %>% print(n = 8)

airlines %>% print(n = 8)

airports %>% print(n = 8)

planes %>% print(n = 8)

weather %>% print(n = 8)


### key 확인
planes %>%
  count(tailnum) %>%
  filter(n > 1)  ## 없다 즉 tailnum 은 pk이다

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n>1)   #2인 경우는 데이터 식별에 주의해야한다.

flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1) ## pk가 존재하지않는다
flights %>%
  count(year, month, day, tailnum, flight) %>%
  filter(n > 1) ## n=2 일때 주의해야한다.

#### row_num을 정렬함을 통하여 대체 key를 만든다.

#################### 
####mutating jion
(x <- tribble( ~key, ~val_x,
               1, "x1",
               2, "x2",
               3, "x3" ))
(y <- tribble( ~key, ~val_y,
               1, "y1",
               2, "y2",
               4, "y3"))
###• inner join excludes unmatched rows.
x %>%
  inner_join(y, by = "key")

###• left join keeps all observations in x.
x %>%
  left_join(y, by = "key")

###• right join keeps all observations in y.
x %>%
  right_join(y, by = "key")

###• full join keeps all observations in x and y.
x %>%
  full_join(y, by = "key")

#### n=>2 일때 key 처리방법
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

x%>%
  left_join(y,by="key")

####
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")


### Mutating joins
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2 %>% print(n = 8)

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

flights2 %>%
  left_join(weather)  ## by= defalt (Null) natural join 을 하게된다.

flights2 %>%
  left_join(planes, by = "tailnum")  ## 연도 변수가 다르게 식별됨

flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

fl
### Filtering join
#• semi_join(x, y) keeps all observations in x that have a match in y.
#• anti_join(x, y) drops all observations in x that have a match in y.

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

flights %>%
  filter(dest %in% top_dest$dest)

##join을 이용한결과와  같다.
flights %>%
  semi_join(top_dest)

####
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

######
# Identify the variables that form the primary key in each table.
# Check that none of the variables in the primary key are missing.
# Check that foreign keys match parimary keys in another table (with anti_join()).


##집합 연산
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
intersect(df1, df2)  ## 교집합
union(df1, df2)      ## 합집합
setdiff(df1, df2)    ## 차집합   A-B
setdiff(df2, df1)    ## 차집합   B-A

