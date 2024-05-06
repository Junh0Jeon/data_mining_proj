# genie200_alive_hist2


# x축은 생존 주, y축은 빈도이다.
ggplot(tibble(alive_date), aes(x = alive_date/7)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "white", fill = "skyblue") +  # histogram 그리기
    geom_density(alpha = 0.5, fill = "orange") +  # density plot 그리기
    # geom_rug(aes(x=alive_date/7+runif(length(alive_date), 0.01, 0.1))) +  # rug plot 그리기 (하단에)
    labs(title = "Histogram and Density Plot", x = "Value", y = "Density") # 제목과 축 레이블 설정
## ㄴ 왼쪽에 거의 쏠려있고, 꽉 채운 노래가 살짝 많은 그래프 이다.

ggplot(tibble(alive_date)) +
    geom_bar(aes(x=alive_date/7), fill = "skyblue", color = "white")
## ㄴ 좀 더 detail하게 그린 plot이다. 0,1,2,118(max)가 거의 대부분의 portion을 차지하는 것 처럼 보인다.

table_alive_date <- table(alive_date/7) ; table_alive_date
df_alive_date <- tibble(freq=factor(names(table_alive_date),levels = as.numeric(names(table_alive_date))),
                        count=as.numeric(table_alive_date))
df_alive_date %>% arrange(desc(count)) ; (177+100+53+44) ; (177+100+53+44)/966 ; 4/113 ; 0.387/0.035
## ㄴ table로 정리해보았다. 실제로 위의 4개가 전체의 38%를 차지하는 모습이다.

##############
## 118을 permanent music으로 정의하고, Y = ifelse(in200 term == 118, 1, 0)으로 정의하겠다.
## 그러면 이제 X로 사용할 데이터를 골라야하는데, 그것을 위한 EDA를 진행하자.


##############################
##############################
##############################
##############################
##############################
##############################

