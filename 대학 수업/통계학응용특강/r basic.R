a=5
b=6
c=8
sum_1<-a+b+c
sum_1

ls()
rm(list=c("sum_1"))
rm(list=ls())

getwd()
#setwd()

TRUE<-1
True<-1
True

typeof(5)
typeof(5L)
typeof(5i)
0xff
0xf+1 
'example'
typeof("5")

LETTERS
letters
month.name
month.abb

v1<-TRUE
class(v1)
v2<-23.5
class(v2)
v3<-2L
class(v3)
v4<-2+4i
class(v4)
v5<-"TURE"
class(v5)


x1<-c(1,2,3)
x2<-c(1L,2L,3L)
x3<-c(TRUE,FALSE,TRUE,FALSE)
x4<-c("Aton","Bob","Cain","Duke")
typeof(x1)
length(x2);class(x3);str(x4)

x1<-c(x1,4)
1:10
seq(10)
seq(1,10,by=0.5)

class(y1<-TRUE+2L)
class(y2<-y1+3)
class(y3<-y2+1i)
class(y4<-c(y3,"Hi"))

x<-1:5
as.numeric(x)
as.logical(x)
as.character(x)
as.complex(x)

x<-c("a","b")
as.numeric(x)
as.logical(x)
1<"2"
"1">2
1<"a"

m<-matrix(1:4,2,2)
m
din(m)
attributes(m)
m<-1:10
dim(m)<-c(2,5)
m

x<-1:3
y<-4:6
cbind(x,y)
rbind(x,y)

x<-list(a=1,b="g",c=TRUE,d=1+4i)
x
class(x)
class(x[1])
class(x[[1]])
str(x)
temp <- list(a=list(b =list(c=list())), b = list(d=1))
temp
temp[1]
temp[[1]]

x<-factor(c("yes","no","no","yes","yes"))
x
table(x)
unclass(x)

x<-factor(c("yes","no","yes"),levels=c("yes","no"))
x

df<-data.frame(id=letters[1:10],x=1:10,y=rnorm(10))
df
cbind(df,z=4)
#head(df) # first 6 rows
#tail(df) # last 6 rows
#dim(df) # dimensions
#nrow(df) # number of rows
#ncol(df) # number of columns
#str(df) # structure of each colmun
#names(df)# list column names

x<-c(1,2,NULL,NA,0/0)
length(x)
is.na(x)
is.nan(x)

sessionInfo()
options()

x<-5;y<-18
x+y;x-y;x*y;y/x;y%/%x;y%%x;y^x
x<y;x>y;x<=5;y>=20;y==17;x!=5

x<-c(2,4,5);y<-c(6,4,3)
x+y;x-y;x*y;x/y;x>y

x<-c(2,4,5,8);y<-c(6,4)
x+y;x-1;x+c(1,2,3)

x <- c(TRUE, FALSE, 5, 8); y<- c(FALSE, TRUE, FALSE, TRUE)
!x ;x&y ;x&&y ;x|y ;x||y

x<-5
y=9
10->z



print("Hello World")
print("Hello World",quote=F) #quote  인용구 ""#
print(paste("How","are","you"))
a<-"how"
b<-"are"
c<-"you"
print(paste(a,b,c))


my.name <- readline(prompt="Enter name: ")
## Enter name:
my.age <- readline(prompt="Enter age: ") # convert character into integer
## Enter age:
my.age <- as.integer(my.age)
print(paste("Hi,", my.name, "next year you will be", my.age+1, "years old."))


num = as.integer(readline(prompt = "Enter a number: "))
for(i in 1:10) { print(paste(num,'x', i, '=', num*i)) }
for(i in 1:10) { print(paste(num,'^', i, '=', num^i)) }

x<-1:7;x
y<-2:-2;y
seq(1,3,by=.2)
seq(1,5,length.out=4)
seq(from=1,by=2,length.out=10)
seq(f=1,b=5,leng=10)
seq(0,1,length.out=11)
seq(11)

rep(1,10)
rep(1:10,2)
rep(1:10,each=2)

x<-seq(0,10,by=2)
x[3]
x[c(2,4)]
x[-1]
x[c(2,-4)]
x[c(1.45,2.52)]
x[c(TRUE,FALSE,FALSE,TRUE)]
x[x<3]
x[x%%2!=1]

x<-c("first"=3,"second"=0,"third"=9)
names(x)
x["second"]
x[c("first","third")]

x<--3:2
x
x[2]<-0
x[x<0]<-5
x<-x[1:4]
x
x<-NULL
x
x+1
rm(x)
x
set.seed(1)
x<-sample(1:10,10,replace=T)
x
sort(x)
sort(x,decreasing = T)
order(x) # 오름차순의 순위를 나타냄
order(x,decreasing = T)

min(x)
max(x)
range(x)
which.min(x)
which.max(x)
x[which.min(x)] # min(x)

a<-matrix(1:9,nr=3,nc=3)
a
class(a)
attributes(a)
dim(a)
list(attributes(a))

a<-matrix(1:9,nr=3)
a
b<-matrix(1:9,nc=3)
b
c<-matrix(1:9,nr=3,byrow=T)
c


x<-matrix(1:9,nrow=3,dimnames=list(c("x","Y","Z"),c("A","B","C")))
x
colnames(x)
rownames(x)
colnames(x)<-c("C1","C2","C3")
rownames(x)<-c("R1","R2","R3")
x
rownames(x)<-tail(LETTERS,3)
colnames(x)<-head(LETTERS,3)
x

x<-1:3;y<-4:6
cbind(x,y)
rbind(x,y)
x<-1:6
class(x)
dim(x)<-c(2,3)
x
class(x)

x<-matrix(1:9,nrow=3)
x
x[c(1,2),c(2,3)]
x[c(3,2),]
x[-1,]
x[-2,]
x[1,]
class(x[1,])
x[,1,drop=FALSE]   # class를 메트릭스로 유지
x<-matrix(1:9,nr=3)
x
class(x[,1,drop=FALSE])

set.seed(1)
x<-matrix(sample(1:10,9,replace=T),nr=3)
x
as.vector(x)[1:4]
x[1:4]
x[c(3,5,7)]
which(x>5)%/%3  ##cor : 0->1, 3->3
which(x>5)%%3  ##row : 0 ->3
(a<-which(x>5))
(r<-which(x>5)%%3)
(c<-which(x>5)%/%3)
c[r!=0]<-c[r!=0]+1
r[r==0]<-3
r;c;x

x[c(T,F,T),c(T,T,F)]
x[c(T,F),c(2,3)]    ##c(T,F,T,)가 됨
as.vector(x)[c(T,F)]

colnames(x)<-LETTERS[1:3]
x[,"A"]
x[TRUE, c("A","C")]

x<-matrix(1:9,nr=3)
x[2,2]<-10;x
x[x<5]<-0;x
t(x)
diag(x)
diag(diag(x))  ##대각행렬화

cbind(x,1:3)
rbind(x,4:6)
x<-x[1:2,];x


x<-list(a=2.5,b=TRUE,c=1:3);x
typeof(x)
length(x)
str(x)

x<-list(2.5,TRUE,1:3);x
x<-list(names="Jack",age=17,speaks=c("English","Korean"))
x
x[1:2]
x[-2]
x[c(T,F,T)]
x[c("age","speaks")]
x["age"]
typeof(x["age"])
x[["age"]]
typeof(x[["age"]])
x$names
x$a
x$speaks[1]
x[["speaks"]][2]
x$names<-"Sun";x
x$married<-F
x
x$age<-NULL
x

x<-data.frame(SN=1:2,Age=c(21,25),Name=c("Jack","Sun"))
class(x$SN)
class(x$Age)
class(x$Name)
x
typeof(x)
str(x)
names(x)
colnames(x)
ncol(x);nrow(x);length(x)
str(x)

x["Name"]
x$Name
x[1,"Age"]<-20;x
x$Age[1]<-20;x

rbind(x,list(1,16,"Paul"))
cbind(x, State=c("NY", "FL"))
x$Sex<-factor(c("M", "F")); x

x$Sex<-NULL;x
x<-x[-1,];x


x<-factor(c("single","married","married","single"));x
x<-factor(c("single","married","married","single"),levels=c("single","married","divorced"))
class(x)
levels(x)
x[2]<-"divorced";x
x[4]<-"widwed";x # can't sign values outside levels
levels(x)<-c(levels(x),"widwed")
x[4]<-"widwed";x

###  array(data=NA,dim=length(data),dimnames=NULL)
x<-array(1:9);x
x<-array(1:9,c(3,3));x
x<-array(1:24)
dim(x)<-c(2,3,4);x  ##2행 3열 4개의 테이블 
x[2,3,4]
x[1,,]
x[1,2,]

vector1<-c(5,9,3)
vector2<-c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2") # Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim=c(3,3,2),dimnames = list(column.names,row.names,matrix.names))
result
result[3,,2]
result[1,3,1]
result[,,2]

apply(result,c(1),sum)
apply(result,c(1,2),sum)
apply(result,c(1,3),sum)


pow <- function(x, y) {
  result <- xˆy
  print(paste(x,"raised to the power", y, "is", result))
}
pow(8,2)

check<-function(x){
  if(x>0){result <- "positive"
  }else if (x<0){
    result<-"Negative"
  }else{
    result<-"Zero"
  }
  return(result)
}
check(-1);check(10);check(0)



multi_return <- function() {
  my_list <- list("color" = "red", "size" = 20, "shape" = "round") return(my_list)
}
a<-multi_return()
a

# Recursive function to find factorial
recursive.factorial <- function(x) {
  if (x == 0) return (1)
  else return (x * recursive.factorial(x-1))
}
recursive.factorial(6)
factorial(6)
