# genie200_intro continue

# 1. 전체 구간에서 순위권 Drawing

df1 %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group))
## ㄴ 너무 많고 정신없다.
## 이 그래프를 그리게 된 최초 idea는
## x축을 date, y축을 rank로 하고 plot을 그리면 permanent와 non-permanent가 보이지 않을까?
## 대부분의 0,1,2,permanent이니까 세로줄, 가로줄 위주로 보이지 않을까? group이 보이지 않을까? 였다.
## 실제로 그렇게 보이기도 한다.
## 다만, line이 너무 많아서 visual base group을 유추하긴 어려워보인다.
## 한가지 특이한 점이라면, 연말연초 기간에 유난히 세로선이 진해보인다.
## 또, 25위권 이내는 세로선이 거의 안보인다. 변동량이 작다는 뜻이겠지.




# 2. rank diff

# 변동량이 작다에서 idea를 얻었다.
# 실제 "변동량"은 어떤 trend를 보일지 확인해보자
# expectation은 "low에 몰려있고, large에 몰려있다.", "large는 특정 숫자에 몰려있다"이다.
df1$rankingdiff %>% 
    gsub("(\\d+)(하강)|(상승)", "\\1", .) %>% gsub("유지", "0", .) %>% 
    gsub("new", "-1", .) %>% as.numeric() -> rank_diff
ggplot(tibble(rank_diff)) +
    geom_bar(aes(x=rank_diff), fill = "skyblue", color = "white")
## 극단적으로 right skewed, 즉 왼쪽으로 쏠린 분포이다.
## weibull 분포가 생각난다.
## 실제로는 low에 몰려있긴 하지만, large는 "몰려있다"라고 하기 어려워 보인다.





# 3. permanent vs. temporary

## 우리의 주 목적은 Y를 얻는것임을 잊으면 안된다.
## 따라서, 두 그룹의 차이 위주로 EDA를 하고자 한다.
permanent_musics <- titles[alive_date==max(alive_date)]
nonpermanent_musics <- titles[alive_date != max(alive_date)]
df_permanent <- df1 %>% mutate(rankingdiff = rank_diff) %>% filter(title %in% permanent_musics)
df_temporary <- df1 %>% mutate(rankingdiff = rank_diff) %>% filter(title %in% nonpermanent_musics)


df_permanent %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group)) -> p_p_r
df_temporary %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group)) -> p_t_r
p_p_r
p_t_r
grid.arrange(p_p_r, p_t_r, ncol=2)
## permanent의 경우, 비교적 횡보하는 것 같다(그렇게 주장하고 싶다)
## temporary의 경우, 최초의 그 정신없는 graph가 보인다.
## 특이한 점은, 횡보하는 그래프가 아래에 시간이 지날수록 차곡차곡 쌓이는 경향이 관측된다.
df_permanent %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") -> p_p_df
df_temporary %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") -> p_t_df
p_p_df
p_t_df
grid.arrange(p_p_df, p_t_df, ncol=2)
## 겉보기엔 distribution이 완전 똑같아 보인다.
par(mfrow=c(2,1))
boxplot(df_permanent$rankingdiff, horizontal=TRUE)
boxplot(df_temporary$rankingdiff, horizontal=TRUE)
par(mfrow=c(1,1))
## boxplot으로 보면, skewedness가 확연히 다름을 알 수 있다.
## range 자체도 다르고...
describe(df_permanent$rankingdiff) ; describe(df_temporary$rankingdiff)

quantile(df_permanent$rankingdiff, probs=seq(0, 1, 0.1)) ; mean(df_permanent$rankingdiff <= 10)
quantile(df_temporary$rankingdiff, probs=seq(0, 1, 0.1)) ; mean(df_temporary$rankingdiff <= 10)
## permanent는 diff 10 이하의값이 96%이고, temporary는 diff 10이하의 값이 82%이다.
## gap이 내가 생각했던 것보다 적게 나오긴 했지만, 해석하기 따라 유의미한 차이로 볼 수 있을 것 같다.





#