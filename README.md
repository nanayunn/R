# R과 통계

(1월 13일)

- R과 파이썬?

  - 비슷하지만 R은 통계 패키지로부터 시작

    > 파이썬은 Language로 시작, 통계 라이브러리가 추가
    >
    > R은 순수하게 통계 데이터 분석을 위한 것

  - 통계처리에 강한 모습을 보임

- R 표기법

  > x=3 ==> x`<-`3
  >
  > ex)
  >
  > ```R
  > x<-2:7
  > x
  > 결과값 : [1] 2 3 4 5 6 7
  > ```

- R Studio

  > R 프로그램과 달리 `Ctrl+Enter`로 프로그램을 실행

https://thebook.io/006723/ 4,5장 참고 예정

- 리눅스 환경에서 설치 시 리눅스를 최신 버전으로 업데이트 해줘야함.

- 개념 공부 순서...

  > 1. Scalar
  > 2. Factor
  > 3. Vector
  > 4. List
  > 5. Matrix
  > 6. Array
  > 7. Data Frame

***

(1월 14일)

### R의 기초

```R
a <- 10
#b <- 20
result <- a + b
print(result)
```

- `a<-10`

  > 변수에 값을 할당해주는 방법
#### 변수

- 알파벳, 숫자, _(언더스코어), .(마침표)
- -(하이픈)은 사용할 수 없다.
- 첫 글자는 알파벳 또는 .
-  .으로 시작한다면 . 뒤에는 숫자가 올 수 없다


- `#(주석처리)`

  > R에서 주석 처리 시 이전에 값을 할당해준 이후 실행한 이력이 있다면 R에 해당 주석 처리 된 변수의 값이 남아있기 때문에 주석처리를 해도 값이 출력되게 된다. 

- `install.packages("randomForest")`

  > 패키지 설치 방법
  >
  > 설치 후에는 코드 창의 맨 상단에 library 선언을 통해 이용

- ```R
  #1차원 행렬
  x_r <- mean(x <- c(1,2,3))
  #'=' 표시 사용 시 변수 값 인식이 되지 않음
  y_r <- mean(y=c(1,2,3))
  ```

- ```R
  af <- function(a1, a2, a3, a4){
    v1 <- a1;
    v2 <- a2;
    v3 <- a3;
    v4 <- a4;
    result <- v1+v2+v3+v4;
    return(result);
  }
  ```

  > **R에서 함수 설정하기**
  >
  > `source` 실행 시 함수가 드라이브에 저장된다. 
  >
  > 콘솔 창에서 함수 값을 넣어 실행하면 값이 출력됨.

  

  ### 데이터 타입

  1. 스칼라

     > 스칼라란 단일 차원의 값을 뜻하는 것으로 숫자 1, 2, 3, …을 예로 들 수 있다. 반면 좌표 평면 위에 있는 점인 (1, 2)는 2차원 값이므로 이 절에서 설명하는 스칼라에 해당하지 않는다.
     >
     > R에서 데이터 타입의 기본은 벡터Vector다. 따라서 스칼라 데이터는 길이가 1인 벡터(즉, 길이가 1인 배열)와 같은 것으로 볼 수 있다.
     >
     > > EX)) 숫자
     > >
     > > a<-3
     > >
     > > 벡터
     > >
     > > a<-c(1:10)

     - 정수, 부동소수 등의 숫자형 데이터 타입

     - NA(Not Available) 상수

       > 데이터 값이 없음을 뜻함
       >
       > 변수에 NA 값이 저장되어 있는지는 is.na( ) 함수로 확인
       >
       > ```R
       > > is.na(four)
       > [1] TRUE
       > ```

     - NULL 

       > 변수가 초기화되지 않았을 때 사용
       >
       >  NA와 구분해서 생각해야 함 
       >
       > 어떤 변수에 NULL이 저장되어 있는지는 is.null( )을 사용해 판단

       ##### NA와 NULL의 차이

       > NA= 값이 빠져 있는 경우
       >
       > > *결측치가 존재하는 이유*
       > >
       > > 1. 데이터 입력 중 실수로 값을 입력하지 않은 경우, 
       > > 2. 값을 어떤 이유로든 관찰되지 못한 경우
       > > 3. 해당 항목에 적절한 값이 없어서 값이 입력되지 않은 경우
       >
       > NULL= 프로그래밍의 편의를 위해 미정(undefined) 값을 표현하는 데 사용

       - 문자열

       R에는 C 등의 언어에서 볼 수 있는 한 개 문자에 대한 데이터 타입(예를 들면, C의 char 데이터 타입)이 없다. 대신 문자열로 모든 것을 표현한다.

     - 진릿값

     TRUE, T는 모두 참 값을 의미한다. FALSE, F는 거짓 값을 의미한다. 진릿값에는 &(AND), |(OR), !(NOT) 연산자를 사용할 수 있다.

     - 팩터

       > 팩터Factor는 범주형Categorical 데이터(자료)를 표현하기 위한 데이터 타입이다.
       >
       > ```R
       > sex <- factor("m", c("f","m")
       > ```
       >
       > 성별에는 m과 f가 있는데 그 중에서 m에 해당한다.

     - 벡터

       > 배열의 개념
       >
       > 한 가지 스칼라 데이터 타입의 데이터를 저장
       >
       > R의 벡터는 슬라이스Slice를 제공
       >
       > > *슬라이스?*
       > >
       > > 배열의 일부를 잘라낸 뒤 이를 또 다시 배열처럼 다루는 개념

       > 벡터의 각 셀에는 이름을 부여할 수 있다. 

벡터 데이터 변환 순서

> NULL < raw < logical < integer < double < complex < character < list < expression

#### 벡터

```R
v1 <- c(1:3)
names(v1) <-c("a","b","c")
length(v1)
#객체의 길이를 반환한다
NROW(v1)
#배열의 행 또는 열의 수를 반환한다.
v1[2]
#v1의 두번째 값
v1["b"]
#위와 같은 값
v1[-2]
#v1에서 2번째 값만 빼고 가져와!
v2 <- v1[1:2]
#v1의 1에서 2의 값을 새로운 변수 v2에게 지정해 주겠다.
```

```R
#x3 <- c(1:10)
#x4 <- x3[-c(4:7)]
#x3에게 할당된 C 값 중 4~7번째 순서의 숫자들을 빼고 가져오겠다.
#==>>x4 =[1,2,3,8,9,10]
x3 <- c(11:20)
x4 <- x3[c(1,3,5,7)]
#x3에게 할당된 c 값 중 1,3,5,7 번째의 숫자들만 가져오겠다.
#==>>x4 =[11,13,15,17]
```

**벡터 값 출력, 선택 시 위치를 지정해 가져오는 것 기억**

- lengths와 length는 다르다

  > > lengths(v1)
  > > a b c 
  > > 1 1 1 
  > > length(v1)
  > > [1] 3

- NROW와 nrow는 다르다

  > ```R
  > > x <- c("a", "b", "c")
  > > length(x)
  > [1] 3
  > > nrow(x)  # nrow()는 행렬만 가능
  > NULL
  > > NROW(x)  # NROW()는 벡터와 행렬 모두 사용 가능
  > [1] 3
  > ```

- sequence 숫자 생성

  ```R
  w4 <-seq(1,10,3)
  #w4
  #[1]  1  4  7 10
  ```
***
#### 행렬 데이터 접근

> 행렬은 색인 또는 행과 열의 이름을 통해 접근할 수 있다.

- 위치로 접근하기

```R
data <- c(1:12)

m1 <- matrix(data=data, nrow=3, byrow = TRUE)
colnames(m1) <-c("A","B","C","D")
rownames(m1) <-c("R1","R2","R3")
m1[c(1),]
m1[c(-1),]
m1[,c(-2,-4)]
```

(결과값)

```R
> m1[1,1]
[1] 1
> m1[1,3]
[1] 3
> m1[c(1:3),]
   A  B  C  D
R1 1  2  3  4
R2 5  6  7  8
R3 9 10 11 12
> m1[c(1:2),]
   A B C D
R1 1 2 3 4
R2 5 6 7 8
> m1[c(1),]
A B C D 
1 2 3 4
> m1[c(-1),]
   A  B  C  D
R2 5  6  7  8
R3 9 10 11 12
> m1[,c(-2,-4)]
   A  C
R1 1  3
R2 5  7
R3 9 11
```

| 문법          | 의미                                                  |
| ------------- | ----------------------------------------------------- |
| A[ridx, cidx] | 행렬 A (ridx행, cidx열.                               |
|               | ridx나 cidx에 벡터를 사용해 여러 값을 지정            |
|               | ridx나 cidx 중 하나를 생략하면 전체 행 또는 열을 의미 |

- 열과 행의 이름으로 값 가져오기

  ```R
  m1["R1",]
  m1[, c("A","C")]
  ```

  (결과값)

```R
> m1["R1",]
A B C D 
1 2 3 4 
> m1[, c("A","C")]
   A  C
R1 1  3
R2 5  7
R3 9 11
```

- 행렬 간 연산 가능



#### 배열

> 다차원 데이터
>
> ex)행렬 = 2×3 차원의 데이터
>
> 배열=2×3×4 차원의 데이터

 

#### 데이터 프레임

> 

```R
e <- c(90,90,98,89)
m <- c(98,89,90,90)
k <- c(100,100,91,92)
s <- c(99,99,88,87)

c1<-data.frame(en=e,ma=m,ko=k,si=s)
rownames(c1)<- c("ha","na","mi","hy")

c1[c(1,4),]
c1[c("ha","hy"),]
```

```R
> c1
  en ma  ko si
1 90 98 100 99
2 90 89 100 99
3 98 90  91 88
4 89 90  92 87

> c1<-data.frame(en=e,ma=m,ko=k,si=s)
> rownames(c1 )<- c("ha","na","mi","hy")
> c1
   en ma  ko si
ha 90 98 100 99
na 90 89 100 99
mi 98 90  91 88
hy 89 90  92 87
> c1[c("ha","hy"),]
   en ma  ko si
ha 90 98 100 99
hy 89 90  92 87
> c1[c(1,4),]
   en ma  ko si
ha 90 98 100 99
hy 89 90  92 87
```

사실 이건 매트릭스..

```R
n<-c("na","hy","mi","ha")
e <- c(90,90,98,89)
m <- c(98,89,90,90)
k <- c(100,100,91,92)
s <- c(99,99,88,87)

c1<-data.frame(name = n,en=e,ma=m,ko=k,si=s)

```

```R
> n<-c("na","hy","mi","ha")
> 
> e <- c(90,90,98,89)
> m <- c(98,89,90,90)
> k <- c(100,100,91,92)
> s <- c(99,99,88,87)
> 
> c1<-data.frame(name = n,en=e,ma=m,ko=k,si=s)
> c1
  name en ma  ko si
1   na 90 98 100 99
2   hy 90 89 100 99
3   mi 98 90  91 88
4   ha 89 90  92 87
```

> 요게 데이터 프레임

- 데이터 타입 알아보기

  ```R
  c1$en[c(1,2)]
  class(c1[c(1:3),c(1:4)])
  ```

```R
> class(c1[1,2])
[1] "numeric"
> class(c1[c(1:3),c(1:4)])
[1] "data.frame"
```

- 특정 컬럼이나 row의 평균값 계산이 불가능 하다면

- **타입 변환** 필요!

  `mean(as.numeric(c2[2,]))`

***

1차원 (Vector)

2차원(Matrix)

3차원(Data Frame)

***

```R
c1<-data.frame(name = n,en=e,ma=m,ko=k,si=s, stringsAsFactors = FALSE)
c1[5,]<-c("yu",NA,NA,NA,NA)
```

> 데이터 프레임의 row에 새로운 컬럼을 추가하고 싶을 때
>
> 그냥 하면 오류남!
>
> `stringsAsFactors = FALSE` 을 데이터 프레임 셋에 추가해주면
>
> name의 데이터 타입 변화
>
> (Factor => Character)

***

# R 프로그래밍

- R의 함수를 Java에서 실행한다?!

- `rm(list=ls())`

  > 변수 값을 저장해두는 Global Environment를 clear하는 명령어

#### 조건문 이용하기

```R
f1 <- function(a){
  result <- NULL;
  if(a%%2 == 0){
    result = "even";
  }else{
    result = "odd";
  }
  return(result);
}

me.f2 <- function(n){
  result <- 0;
  for(i in c(1:n)){
    print(i);
    result <- result + i;
  }
  return(result);
}
```

```R
#n:1 국가별 평균 날씨를 Vector로 리턴한다.
#n:2 분기별 평균 날씨를 Vector로 리턴한다. 
#단, 2개 모두 컬럼 명칭을 정확히 셋팅해서 리턴한다.

re <- function(n){
  #날짜 데이터?
  result<-0;
  w <- data.frame(nation=c("ko","jp","ch"),
                  Q1=c(30,20,20),
                  Q2=c(31,18,19),
                  Q3=c(28,18,12),
                  Q4=c(22,16,20),
                  stringsAsFactors = FALSE);  
   
  if(n == 1){
    result <- rowMeans(w[,-1]);
      #colMeans와 달리 rowMeans의 결과값은 컬럼을 포함하지 않는다.
    names(result)<- c("ko","jp","ch")
      #그래서 이름 추가해줌
  }
  if(n == 2){
    result<- colMeans((w[,-1]));
  }
    return(result);
  }
```
