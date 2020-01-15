# MPG 데이터분석 연습문제

Property: Jan 15, 2020 9:12 AM
Property 1: pg.112,133,138,141,144,150

library(ggplot2)
    
    #View(mpg)
    mpg_dupl<-rename(mpg, city = cty, highway = hwy)

    library(ggplot2)
    
    
    #Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아볼것
    #displ이 4이하인 자동차와 5이상인 자동차 중 어떤 자동차의 hwy가 평균적으로 더 높은지 알아보자.
    
    #mpg 조회
    View(mpg)
    
    #모델과 배기량 조회
    mpg[mpg$displ<=4, c(2,3)]
    mpg[mpg$displ>=5, c(2,3)]
    
    
    #배기량을 기준으로 hwy 값을 추출, 
    #matrix로 형변환 후 평균값 구함
    mean(as.matrix(mpg[mpg$displ<=4, 9]))
    #[1] 25.96319
    
    mean(as.matrix(mpg[mpg$displ>=5, 9]))
    #[1] 18.07895
    
    
    #Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아볼 것.
    #"audi"와 "toyota" 중 어느 manufacturer의 cty가 평균적으로 더 높은가?
    
    mpg[mpg$manufacturer=="audi", c(1,8)]
    mpg[mpg$manufacturer=="toyota", c(1,8)]
    
    mean(as.matrix(mpg[mpg$manufacturer=="audi", 8]))
    # [1] 17.61111
    mean(as.matrix(mpg[mpg$manufacturer=="toyota", 8]))
    #[1] 18.52941
    
    
    #Q3. "chevrolet","ford","honda" 자동차의 고속도로 연비 평균을 알아볼것. 이 회사들의 데이터 추출 후 hwy 전체 평균을 구하라.
    
    mpg[mpg$manufacturer=="chevrolet", c(1,9)]
    mpg[mpg$manufacturer=="ford", c(1,9)]
    mpg[mpg$manufacturer=="honda", c(1,9)]
    
    c<-mean(as.matrix(mpg[mpg$manufacturer=="chevrolet", 9]))
    #[1] 21.89474
    f<-mean(as.matrix(mpg[mpg$manufacturer=="ford", 9]))
    #[1] 19.36
    h<-mean(as.matrix(mpg[mpg$manufacturer=="honda", 9]))
    #[1] 32.55556
    
    mean(c,f,h)
    #[1] 21.89474

    library(ggplot2)
    
    
    #Q1. mpg 데이터에서 class, cty 변수를 추출해 새로운 데이터를 만들어라. 
    
    m1<-mpg[,c(8,11)]
    
    #Q2.
    m1[m1$class=="suv",c(1,2)]
    m1[m1$class=="compact",c(1,2)]
    
    s<-m1[m1$class=="suv",1]
    m<-m1[m1$class=="compact",1]
    
    
    mean(as.matrix(s))
    #[1] 13.5
    mean(as.matrix(m))
    #[1] 20.12766

    library(ggplot2)
    
    #Q1. "audi"에서 생산한 자동차 중 어떤 자동차 모델의 hwy가 높은지 알아보자.
    #hwy가 1~5위에 해당하는 자동차의 데이터를 출력하시오.
    
    mm<-mpg[mpg$manufacturer=="audi", c(1,9)]
    
    #mm의 목차 갯수 
    dim(mm)
    
    mm[order(mm$hwy,decreasing = T),]
    
    #mm의 순위를 보기 위해 내림차순 정렬
    hwy_rank<-mm[order(mm$hwy,decreasing = T),]
    
    #mm의 목차 갯수에 맞게 순위를 매겨줌
    hwy_rank$level <- c(1:18)
    #그 중 5위까지 조회
    hwy_rank[hwy_rank$level <= 5,]

    library(ggplot2)
    library(dplyr)
    #Q1. mpg() 데이터의 복사본을 만들고, cty와 hwy를 합산한 합산 연비 변수 추가하기
    c1[5,]<-c("yu",NA,NA,NA,NA)
    
    mpg2<- mpg
    mpg_new<-mpg
    mpg2[,"cty"]
    mpg_new$plus<-apply(mpg_new[,c(8,9)],1,sum)
    
    View(mpg_new)
    
    #Q2. 합산 연비변수를 2로 나누어 평균 연비변수 추가.
    
    mpg2$avg<-apply(mpg2[,12],1,mean)
    
    mpg_new$avg<-mpg_new$plus /2
    
    #Q3.
    dim(mpg_new)
    rank<-mpg_new[order(mpg_new$avg,decreasing = T),]
    rank$level <- c(1:234)
    
    rank[rank$level <= 3,]
    
    
    #Q4
    mpg%>%mutate(sum=mpg$cty+mpg$hwy,avg=(mpg$cty+mpg$hwy)/2)%>%arrange(desc(avg))%>%head(3)

    library(ggplot2)
    
    #Q1
    
    #(조건, 데이터, 실행할 함수)
    #aggregate(en~class,df1,mean)
    #class별 english의 평균 구할 것이라는 말.
    #aggregate((en+ma)/2~name,df1,mean)
    #name별 english와 match의 평균 구할 것이라는 말.
    
    mm<-aggregate(cty~class,mpg,mean)
    
    #Q2
    mm[order(mm$cty,decreasing = T),]
    
    #Q3
    m2<-aggregate(hwy~manufacturer,mpg,mean)
    m3<-m2[order(m2$hwy,decreasing = T),]
    dim(m2)
    m3$level<-c(1:15)
    m3[m3$level<=3,]
    
    #Q4
    #aggregate(class~manufacturer,mpg, )
    
    m3<-mpg[mpg$class=="compact",c(1,11)]
    View(m3)
    m4<-table(m3)
    df<-data.frame(m4)
    df2<-rename(df, count=Freq)
    df2[order(df2$count,decreasing = T),]