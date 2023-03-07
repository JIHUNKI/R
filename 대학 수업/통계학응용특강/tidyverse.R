library(nycflights13)
library(tidyverse)
aa<-flights
aa
str(aa)

filter() ## subset()
arrange() ##oreder() sort()
select() ## data[,c()]
mutate() ## transform()
summarise() ## aggregate()

##filter()
jan1<-filter(flights,month==1,day==1)
(dec25<-filter(flights,month==12,day==25))  ## 괄호 바로 프린팅

a1<-filter(flights,month==11|month==12)

a2<-filter(flights,month %in% c(11,12))

a3 <- filter(flights, !(arr_delay > 120 | dep_delay > 120))

(a4 <- filter(flights, arr_delay <= 120, dep_delay <= 120))



df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)


####arrange()
arrange(flights,year,month,day)
arrange(flights, desc(dep_delay))


##select()
flights[,c("year","month","day")]
flights[,1:3]
select(flights,year:day)
select(flights,-(year:day))

##starts_with("abc"): matches names that begin with “abc”.
#ends_with("xyz"): matches names that end with “xyz”.
#contains("ijk"): matches names that contain “ijk”.
#matches("(.)\\1"): selects variables that match a regular expression. This one
#matches any variables that contain repeated characters. You’ll learn more about regular
#expressions later.
# num_range("x", 1:3): matches x1, x2 and x3.
# everything() : matches all variables.
iris
select(iris,starts_with("Petal"))
select(iris,starts_with("petal")|ends_with("Width"))
select(iris,starts_with("Petal") & !ends_with("Width"))
#rename(flights, tail_num = tailnum)  변수 이름을 바꿀 때
select(flights, time_hour, air_time, everything())



## mutate()
(flights_sml<-flights%>%
    select(year:day, ends_with("delay"), air_time)%>%
    mutate(gain = dep_delay - arr_delay,
           hours = air_time * 60,
           gain_per_hour = gain / hours)
)

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

flights%>%
  select(year:day, ends_with("delay"), air_time, everything())%>%
  mutate(gain = dep_delay - arr_delay,
         hours = air_time * 60,
         gain_per_hour = gain / hours,
         .before = 1)  ## .before = 1 을 나타내어 파생변수를 앞으로 나타냄

flights%>%
  select(year:day, ends_with("delay"), air_time, everything())%>%
  mutate(gain = dep_delay - arr_delay,
         hours = air_time * 60,
         gain_per_hour = gain / hours,
         .after = day) ## .after= day day변수 뒤로 파생변수를 위치함 

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  ) ## .keep= c(“all”, “used”, “unused”, “none”) 

##summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

##group_by
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

##
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")


flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))


##
not_cancelled<- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]),
    med_delay1 = median(arr_delay),
    med_delay2 = median(arr_delay[arr_delay > 0])
  )

not_cancelled %>%
group_by(dest) %>%
  summarise(distance_sd = sd(distance), 
            distance_mad = mad(distance), 
            count = n()
            )%>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time),
    Q1 = quantile(dep_time,0.25),
    Q3 = quantile(dep_time,0.75),
    IQR= IQR(dep_time,na.rm=T),
    n_distinct=n_distinct(dep_time),
    count=n()
)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    second_dep = nth(dep_time,2),
    last_dep = last(dep_time)
  )

not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier), n = n()) %>%
  arrange(desc(carriers))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500), hour_prop = mean(arr_delay > 60))


##
daily<-group_by(flights,year,month,day)
(per_day<-summarise(daily,flights=n()))
per_month<-summarise(per_day,flights=sum(flights))
per_month[1:3,]
(per_year <- summarise(per_month, flights = sum(flights)))


##
daily %>%
  ungroup() %>% # no longer grouped by date
  summarise(flights = n()) # all flights


flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

flights %>%
  group_by(dest) %>%
  filter(n() > 10000) %>%
  select(dest) %>%
  summarise(count = n())

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dests

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
