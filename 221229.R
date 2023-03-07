# 벡터 데이터의 타입 생성
a <- c(1,2,3,4,5)
a
b <- 1:5
b
c(1,2)->c
a = c(1,'d')
a

# 행렬
d = array(1:20,dim=c(4,5))
d
e = matrix(1:10,nrow=2)
e

# 리스트 (python에서 dict 형태와 흡사)
f = list(name="test",age=20,phone="01012341234")
f
f["name"]

## 데이터프레임
df = data.frame(name=c("test","test2"),
                age = c(30,20),
                phone = c('01012341234','01012345678'))
df


#if 문

a=10
if (a >5){
  print('a는 5보다 크다')
}

if (a >15){
  print("a는 15보다 크다")
}else if(a>=10){
  print("a는 10보다 크거나 같거나 15보다 작거나 같다.")
}else {
  print("a는 10 보다 작다")
}

## which문 (python에서는 index() )
name = c("test","test2","test3")
which(name=="test2")
which(name=="test5")
which(name != "test2")

## 함수 (function 키워드로 함수를 선언)
# 매개변수가 없는 경우
func_1 = function(){
  return("Hell R")
}
func_1()  

# 매개변수가 있는 경우
func_2 = function(x,y){
  result = x^y
  return(result)
}
func_2(5,2)

'%s%' = function(x,y){
  result = x^y
  return(result)
}
5%s%2

# 매개변수의 기본값 설정
func_4 = function(x,y=5){
  result = x^y
  return(result)
}
func_4(2);func_4(2,10)

## 매개변수의 개수가 가변인 경우
## python def func(x,*y)
func_5 = function(x,...){
  print(x)
  print(c(...))
}
func_5(1,2,3,4,5,6)

## 백터형 데이터를 가지고 데이터프레임 생성
## 벡터데이터의 길이가 동일 해야 가능
name = c("test","test2","test3","test4")
grade = c(1,3,2,1)
student = data.frame(name,grade)
student

midturn = c(70,80,60,90)
final = c(60,90,80,90)
scores = cbind(midturn,final)
scores

students = data.frame(student,scores)
students

gender = c("M","F","F","M")
students = cbind(students,gender)
students

## 중간성적과 기말성적의 합을 새로운 컬럼을 추가
## 새로운 컬럼의 이름은 total_score
## case1
total_score = midturn + final
cbind(students,total_score)
# 특정 컬럼의 데이터를 추출
students[["midturn"]]
students$midturn
students[[3]]
students["total_score"] = students$midturn+students$final
students

##행을 추가하는 함수 rbind()
##rbind()는 데이터프레임의 형태와 같은형태의 
## 데이터프레임을 추가
new_student = data.frame(
    name = "test5",
    grade = 3,
    midturn=50,
    final = 60,
    gender = 'F',
    total_score = 110)
new_student
students <- rbind(students,new_student) 

## 데이터 프레임의  필터림
## 데이터프레임명[행의기준, 열의기준]
students[c(2,4),]
students[1:3,]

## 해당 인덱스만 제외하고 출력
students[-1,]
students[-c(2,4),]

# 조건을 이용해서 필터링
# 학년이 2학년이상인 경우
students[students$grade >=2,]

# 정렬- 기본값 오름차순
order(students$grade)
students[order(students$grade),]

## 내림차순
order(students$grade,decreasing = TRUE)
students[order(students$grade,decreasing = TRUE),]
order(-students$grade)




