library(tidyverse)
table1

table2

table3

table4a

table4b

table5
 
#compute rate per 10.000
table1 %>%
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>%
  count(year, wt=cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))

##gater
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

##pivot_logner()
table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

##combine
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

##spread
spread(table2, key = type, value = count)

##pivot_wider
table2 %>%
  pivot_wider(names_from = type, values_from = count)

##separate  sep defalt 값
table3 %>%
  separate(rate, into = c("cases", "population"),sep="/")

##convert argument  chr 에서 int로 변형해준다.
table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)

##sep num  문자열의 순서
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

##unite

table5 %>%
  unite(new, century, year) #By default underscore(_) is placed between the values from different columns.
table5 %>%
  unite(new, century, year, sep="")


##################
year<-c(2015,2015,2015,2015,2016,2016,2016)
qtr<-c(1,2,3,4,2,3,4)
return<-as.double(c(1.88,0.59,0.35,NA,0.92,0.17,2.66))
(stocks<-tibble(year,qtr,return))

##implicit missing to Explicit missing  암묵적인 미씽값을 명시적으로 NA처리한다.
stocks %>%
  pivot_wider(names_from = year, values_from = return)

stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )
##complete
stocks %>%
  complete(year, qtr)

##fill
person<-c("Derrick whitmore",NA,NA,"katherine Burke")
treatment<-c(1,2,3,1)
response<-c(7,10,9,4)
(treatment<-tibble(person,treatment,response))

treatment %>%
  fill(person)

##################################

tidyr::who
view(who)
who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1

who1 %>%
  count(key)

who2 <- who1 %>%   ###stringr::str_replace("newrel"값을 "new_rel"로 바꾸라)
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

who3 %>%
  count(new)  ##new만 있으므로 지워도 되는변수

who4 <- who3 %>%
  select(-new, -iso2, -iso3)
who4

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who5

who %>%
  gather(key, cases, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"),sep="_") %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

