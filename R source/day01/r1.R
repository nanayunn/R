library(randomForest)
a <- 10
#b <- 20
result <- a + b
print(result)

.1<-109
a-b<-10

x <- c(1,2,3)
#1차원 행렬
x_r <- mean(x <- c(1,2,3))
#'=' 표시 사용 시 변수 값 인식이 되지 않음
y_r <- mean(y=c(1,2,3))

af <- function(a1, a2, a3, a4){
  v1 <- a1;
  v2 <- a2;
  v3 <- a3;
  v4 <- a4;
  result <- v1+v2+v3+v4;
  return(result);
}