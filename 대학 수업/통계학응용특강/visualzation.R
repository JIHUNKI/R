library(tidyverse)
data(mpg)
head(mpg)
##
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))
 
ggplot(data=mpg)+   #색깔 구분
  geom_point(mapping=aes(x=displ,y=hwy,color=class))

ggplot(data=mpg)+   #크기 구분
  geom_point(mapping=aes(x=displ,y=hwy,size=class))

ggplot(data=mpg)+    ## 투명도 구분
  geom_point(mapping=aes(x=displ,y=hwy,alpha=class))

ggplot(data=mpg)+   ##모양 구분분(6개의 모양까지 사용)
  geom_point(mapping=aes(x=displ,y=hwy,shape=class))

#aes, 레이어가 사용하는 심미성을 나타내는 함수

ggplot(data=mpg)+    ##바람직하지않음 
  geom_point(mapping=aes(x=displ,y=hwy,color="blue"))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy),color="blue")

##클래스별 분활 

ggplot(data=mpg)+  ##(facet_wrap)
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_wrap(~class,nrow=2)

ggplot(data=mpg)+  ##(facet_wrap)
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(~class)

ggplot(data=mpg)+  ##(facet_grid)  
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)

ggplot(data=mpg)+ 
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(.~cyl)
#### geom의 종류
ggplot(data=mpg)+  #point
  geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+  #smooth
  geom_smooth(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+   #linetype=변수 (변수별 선을 만듬)
  geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv))

ggplot(data=mpg)+  
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv))

## group
ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy,group=drv))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy,color=drv),
              show.legend = F)

##
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_smooth(mapping=aes(x=displ,y=hwy))


ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
         geom_point()+
         geom_smooth(se=F)

ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth(
    data=filter(mpg,class=="subcompact"),se=F)

## bar

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))

ggplot(data=diamonds)+
  stat_count(mapping=aes(x=cut))


##
demo<-tribble(
  ~cut, ~freq,
  "fair",1610,
  "Good",4906,
  "Very Good",12082,
  "Premium",13791,
  "Ideal",21551)
ggplot(data=demo)+
  geom_bar(mapping=aes(x=cut,y=freq),stat="identity")

##
ggplot(data=diamonds)+  ## 새로만든 변수는 ..변수.. 표시ㅣ
  geom_bar(
    mapping=aes(x=cut,y=..prop..,group=1)
  )

ggplot(data = diamonds) + ## 새로만든 변수 after_stat표시
  geom_bar(
    mapping = aes(x = cut, y = after_stat(prop), group = 1)
  )

ggplot(data=diamonds)+  ## fun 요약통계량으로 max,min,median을 나타내준다.
  stat_summary(
    mapping=aes(x=cut,y=depth),
               fun.min=min,fun.max=max,fun=median)

diamonds%>%group_by(cut)%>%
  summarise(max(depth),min(depth),median(depth))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,color=cut))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=cut))

ggplot(data=diamonds)+  #등급에 따라서 누적된 막대 fill=변수
  geom_bar(mapping=aes(x=cut,fill=clarity))
## 겹치는 막대그리기 alpha값을 조정하여 겹치는정도 조정
ggplot(data=diamonds,mapping=aes(x=cut,fill=clarity))+
  geom_bar(alpha=1/5,position="identity")
## 빈 막대 그리기 투명
ggplot(data=diamonds,mapping=aes(x=cut,color=clarity))+
  geom_bar(fill=NA,position="identity")
# 막대의 높이를 맞춰줌
ggplot(data = diamonds) + geom_bar(
  mapping = aes(x = cut, fill = clarity), position = "fill" )
# 겹쳐지는 막대가 서로 옆에 따로 나타남
ggplot(data = diamonds) + geom_bar(
  mapping = aes(x = cut, fill = clarity), position = "dodge" )
#####
#겹쳐지게 나오는 점을 조금 변화하여 모든점이 나타남
ggplot(data=mpg)+
  geom_point(
    mapping=aes(x=displ,y=hwy),position="jitter")
  


###boxplot
#변수명이겹침
ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  geom_boxplot()
#coord_flip으로 가로로 나타내 변수명이 모두 나타남
ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  geom_boxplot()+coord_flip()
#변수 순서를 변경
ggplot(data=mpg,mapping=aes(x=hwy,y=class))+
  geom_boxplot()

###공간 데이터  nz데이터 누락 ,,

nz <- ggplot2::map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

##coord_polar()
bar<-ggplot(data=diamonds)+
  geom_bar(
    mapping=aes(x=cut,fill=cut),
    show.legend=FALSE,
    width=1)+
  theme(aspect.ratio=1)+  ## 화면비율 aspect.ratio
  labs(x=NULL,y=NULL)

bar+coord_flip()
bar+coord_polar()  ## 원형 그래프


