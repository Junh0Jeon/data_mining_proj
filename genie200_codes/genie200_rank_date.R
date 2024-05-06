# Eda Part

#####################################
########################################
########################################
# Data Loading
######################################
rm(list=ls())
setwd("C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj")
setwd("C:/Users/p5682/Desktop/R_cloud/R_2024/DataMining_proj")
df1 <- read.table("genie200.txt")
df1 <- tibble(df1) %>% mutate(date = as.Date(date))

titles <- unique(df1$title) ; length(titles)
alive_date <- c()
ranking_list <- list()

par(mfrow=c(2,2))
cnt <- 1 # cnt = 10
for(t in titles) {
  temp_df <- df1 %>% filter(title==t) # t=df1$title[15]
  alive_date <- c(alive_date, tibble(temp_df) %>% slice(c(1, n())) %>% .$date %>% diff.Date())
  ranking_list[[cnt]] <- temp_df$ranking
  # plot(ranking_list[[cnt]], type="l", main=titles[cnt])
  cnt = cnt+1
}
par(mfrow=c(1,1)) ; rm(cnt, t, temp_df)
#########################################
#########################################

# 1. 전체 구간에서 순위권 Drawing

df1 %>%
  group_by(title) %>%
  ggplot() +
  geom_line(aes(x=date, y=ranking, group=title))
# 너무 많고 정신없다. 그리고 그래프가 뭔가 이상하게 그려졌을 거란 생각이 든다.
# 범위를 좁혀보자.

df1 %>%
  group_by(title) %>% # group n 966
  filter(date > as.Date("2022-10-01")) %>%
  filter(date < as.Date("2022-12-01")) %>%
  ggplot() +
  geom_line(aes(x=date, y=ranking, group=title))
# 그냥 title이 너무 많아서 그런가? 그렇다기엔 저기 저 왔다갔다 저건 뭐지 이상한데.
df1 %>%
  group_by(title) %>%
  filter(date > as.Date("2022-10-01")) %>%
  filter(date < as.Date("2022-12-01")) %>%
  arrange(title) %>%
  ungroup() %>%
  select(ranking) %>% .$ranking %>% plot(., type="l")
# 300~500구간에 범인이 있어보인다.
df1 %>%
  group_by(title) %>%
  filter(date > as.Date("2022-10-01")) %>%
  filter(date < as.Date("2022-12-01")) %>%
  arrange(title) %>%
  ungroup() %>%
  slice(400:500) %>% .$ranking %>% plot(., type="l")
# 거의 찾았다.
df1 %>%
  group_by(title) %>%
  filter(date > as.Date("2022-10-01")) %>%
  filter(date < as.Date("2022-12-01")) %>%
  arrange(title) %>%
  ungroup() %>%
  slice(440:460) %>% .$ranking %>% plot(., type="l")
# 아.....


# 위에꺼 regroup해서 다시 plot을 그려보자.
df1 %>%
  mutate(title_artist = paste(title, "::", artist)) %>%
  ggplot() +
  geom_line(aes(x=date, y=ranking, group=title_artist))

## 전체 구간에서 순위권을 plot으로 보기엔 무리가 있어 보인다.
## 맨 처음에는 명확하게 grouping이 될 거라고 생각했는데 그렇지 않은 것 같다.
## 약간의 재밌는 점을 보자면, 1월에 그래프가 쏠려있는듯 해보인다. 진하다.
## 30위 이내는 평행선이 잘 보인다. 뭔가 안정적인, 콘크리트층? 혹은 내려가는 길밖에 그럴지도.
#########################################
#########################################
#########################################
#########################################
#########################################
#########################################

# 2. rank diff

# 변동량이 작다에서 idea를 얻었다.
# 실제 "변동량"은 어떤 trend를 보일지 확인해보자
# expectation은 "low에 몰려있고, large에 몰려있다.", "large는 특정 숫자에 몰려있다"이다.

unique(df1$rankingdiff)

df1$rankingdiff %>% 
    gsub("[하강]|[상승]", "", .) %>% gsub("유지", "0", .) %>% 
    gsub("new", "-1", .) %>% as.numeric() -> rank_diff
head(rank_diff) ; sample(rank_diff, 6, replace=FALSE)

hist(rank_diff)
ggplot(tibble(rank_diff)) +
    geom_bar(aes(x=rank_diff), fill = "skyblue", color = "white")
## 극단적으로 right skewed, 즉 왼쪽으로 쏠린 분포이다.
## weibull 분포가 생각난다.
# plot(seq(0, 10, length=100), dweibull(seq(0, 10, length=100), shape=3, scale=0.5),
#      type='l', lwd=2, col='blue', 
#      xlab='x', ylab='Density', main='Weibull Distribution PDF')
tibble(rank_diff) %>% group_by(rank_diff) %>%
    summarise(freq = n()) %>% arrange(desc(freq))
## plot으로 보면 25 이하 변화에서 거의 노는 모습을 볼 수 있다.
## 최초 등록시 급상승하는데, 이 "급상승"은 정해진 수치가 없고 range가 넓으므로 넓게 퍼지며,
## 이후 소량변동의 경우 range가 좁기 때문에 extremely skewed로 나타나는 것 같다.

##
## 단순 절댓값이므로, 이번엔 증감까지 보자.
df1$rankingdiff %>% 
    gsub("(\\d+)하강", "-\\1", .) %>%
    gsub("(\\d+)상승", "\\1", .) %>% gsub("유지", "0", .) %>% 
    gsub("new", "0", .) %>% as.numeric() -> rank_diff2
# hist(rank_diff2)
ggplot(tibble(rank_diff2)) +
    geom_bar(aes(x=rank_diff2), fill = "skyblue", color = "white")
## 경향은 거의 같아보인다. 다만, 범위가 다르다.
range(rank_diff) ; range(rank_diff2)
## 왜 231이 있지?
df1$rankingdiff[df1$rankingdiff=="231상승"]
df1[df1$rankingdiff=="231상승",]
## 상승값은 이전 위치까지 기억해서 표기했나보다.
## 하강값은 반드시 200 이내일 수 밖에 없겠다.

## 주 목표는 Y값 탐색이기 때문에, 일단 Y값을 두 그룹으로 나누자.
detach(package:MASS) # select겹친다.
permanent_musics <- titles[alive_date==max(alive_date)]
df1 %>%
    mutate(rankingdiff = rank_diff) %>%
    filter(title %in% permanent_musics) %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") +
    labs(title="permanent misic의 rank diff histogram")
## permanent music의 rank diff분포.
## 여전히 right skewed이긴 하지만, 덜 거시기하다. 50이 아니라 25안이 더 풍족해보인다.
nonpermanent_musics <- titles[alive_date != max(alive_date)]
df1 %>%
    mutate(rankingdiff = rank_diff) %>%
    filter(title %in% nonpermanent_musics) %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") +
    labs(title="nonpermanent misic의 rank diff histogram")
## 여전히 right skewed. 분포 자체가 차이가 있어보이긴 하지만, 그건 그냥 outlier때문일수도.
## outlier를 제거하면 비슷한 분포가 될 수도 있다.
## 사실 안그럴 것 같다. 꼬리가 명백하게 value를 가지는 느낌.
df1 %>%
    mutate(rankingdiff = rank_diff) %>%
    filter(title %in% nonpermanent_musics) %>%
    group_by(rankingdiff) %>%
    summarise(freq = n()) %>%
    filter(rankingdiff > 50)
## 0,1,2,3 뭐 이런 value가 아니라, 그것보단 크다.

## Y를 구분하는데 rankingdiff를 ranking_step_changed{0, 1}로 변화시켜 categorical로 이용할 수 있을 것 같다.





# perma vs nonperma
## 특징을 비교해야하니까, 그룹으로 나눠서 좀 보자.

df_permanent <- df1 %>% mutate(rankingdiff = rank_diff) %>% filter(title %in% permanent_musics)
df_temporary <- df1 %>% mutate(rankingdiff = rank_diff) %>% filter(title %in% nonpermanent_musics)
df_permanent %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group)) +
    labs(title="permanent misic의 rank plot")
## 내부적으로 왔다갔다 하는 모습이 보이긴 한다.
## 대체적으로 그래도 횡보한다..라고 주장하고 싶은데
## "횡보한다"==diff가 적다. 를 보이고 싶으니까 diff를 한번 보자.
df_permanent %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") # 사실 앞에서 봤던거.
boxplot(df_permanent$rankingdiff, horizontal=TRUE)
describe(df_permanent$rankingdiff)
quantile(df_permanent$rankingdiff, probs=seq(0, 1, 0.1))
mean(df_permanent$rankingdiff <= 10)
## 90%가 8 under이고, 신곡도 존재하며, 이정도면 횡보한다라고 할 수 있을 것 같다.
## 즉, notable한 rankingdiff가 없다라고 할 수 있다. 10 이하가 0.05니까...

df_temporary %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group))
## 최초에 봤던 정신없던 그 chart가 그려진다.
## 특이사항이 하나 보인다.
## permanent가 걸러지고 나니까, 횡보하는 describe(df_temporary$rankingdiff)그래프가 아래에 시간이 지날수록 차곡차곡
## 쌓이는 그런 경향이 관측된다.
df_temporary %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white") # 사실 앞에서 봤던거.
boxplot(df_temporary$rankingdiff, horizontal=TRUE)

quantile(df_temporary$rankingdiff, probs=seq(0, 1, 0.1))
mean(df_temporary$rankingdiff <= 10)
## 얘도 횡보하는 경향이 있는걸로 관측된다!
## permanent보다는 크지만, 그래도...
## 82%가 10 이하이다. 보기에 따라 "얘도 횡보다"라고 할만할 것 같다.
## 난 그래도 10이하의 비중이 60~70을 생각했었는데.





# 3. perm과 nonperm중 큰  비중을 갖는 애들 pareto
df_alive_date
df_alive_date$freq
df_alive_date <- df_alive_date %>% mutate(cumul = cumsum(count)/sum(count))
df_alive_date

ggplot(df_alive_date) +
    geom_bar(aes(x=freq, y=count), stat="identity", fill="skyblue") +
    geom_line(aes(x=freq, y=cumul*max(count), group=1), color="red") +
    scale_y_continuous(sec.axis = sec_axis(~./max(.), name = "Cumulative Percentage")) +  # 오른쪽 축 추가 (누적 백분율)
    labs(title = "Pareto Chart", x = "alive_week", y = "freq") +  # 제목과 축 레이블 설정
    theme_minimal() +  # 기본적인 테마 적용
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x 축 레이블 각도 조정
df_alive_date %>%
    filter(as.numeric(freq) < nrow(df_alive_date)*0.2) %>% print(n=22)
## 하위 20%가 전체 65%를 차지하는 모습. pareto ratio에 해당하진 않는다.





# 4.
df_permanent
## 사실, 얘들중 다수는 이미 "permanent"를 증명한 애들이라, new가 아니다. new를 찾아볼까.
df_permanent %>% filter(rankingdiff == -1) # 희재, 기억해줘요 내 모든날과 어쩌구
## 희재의 경우, 1년이 가까이 되어가기 때문에 permanent에 가까울 수 있다.
df1 %>% filter(title == "희재", artist == "이홍기 (FT아일랜드)")
df1 %>% filter(title == "희재", artist == "성시경") %>% slice(c(1, n()))
## 아니네? 성시경꺼만 살아있는거네
## 화곡동 청개구리를 보자.
df1 %>% filter(title == "기억해줘요 내 모든 날과 그때를")
df1 %>% filter(title == "기억해줘요 내 모든 날과 그때를", artist == "거미 (Gummy)")
df1 %>% filter(title == "기억해줘요 내 모든 날과 그때를", artist == "화곡동 청개구리")
## 이건 둘다 permanent네
## 딱 운 좋게 first line부터 permanent를 유지하는게 있다.

## 생각해보니까, permanent-candidate가 있다. 애초에 permanent를 시작과 끝으로 정했기때문에...
## permanent가 충분히 될 수 있는 애들을 구별해내야 한다.
candidate_musics <- df1 %>% filter(date == as.Date("2024-04-08")) %>% .$title %>% unique()
candidate_musics <- candidate_musics[!(candidate_musics %in% permanent_musics)]
## candidate는 permanent를 빼줘야함.
df_candidate <- df1 %>% mutate(rankingdiff = rank_diff) %>% filter(title %in% candidate_musics)
df_candidate %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group))
#(reference from genie200_intro2)
p_p_r
p_t_r
grid.arrange(p_p_r, p_t_r, ncol=2)
## 두 plot의 모습이 부분부분 보인다.
## 차곡차곡 쌓이는 경향 + 세로줄은 temporary에서 보이던 모습이고,
## 중간에 짤라서 본다고 치면 비교적 횡보하는 모습이 관측되는데 이건 permanent의 모습이다.

## 우리가 Y를 분류한다고 하면, 초점을 맞추게 될 것은 아마도
## "최초 rank의 변화"일 것이다. 그것만이 관측 가능하니까.
df_candidate %>%
    ggplot() +
    geom_bar(aes(x=rankingdiff), fill="skyblue", color="white")
#(reference from genie200_intro2)
p_p_df
p_t_df
grid.arrange(p_p_df, p_t_df, ncol=2)
## temporary보다 훨씬 더 skewed된 모습이 보인다.


# 5. candidate estimating
## 시작점을 모두 동일선상에 두고 시작해보자
## 일단, 가장 빈도가 많은 0, 1, 2를 넘어가면 temporary에 속할 확률이
## 낮을 거라고 예상할 수 있다.
## 이런 method로 접근한다 했을 때... 이런 방식도 가능하다.

## "특정 threshold를 넘어가면 temporary가 아니라 permanent로 속할 수 있지 않을까?
## 그런 threshold의 여부를 확인하기 위해, 시작점을 동일하게 둬보자.
## 그리고 candidate, temporary, permanent를 비교해보자.
df1 %>% # 실행에 시간이 좀 걸린다. 저장해놨으니 그걸로 보자.
    group_by(title, artist) %>%
    mutate(first_date = min(date)) %>%
    mutate(date_gap = (date-first_date)/7) %>%
    select(-date, -first_date) %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    mutate(type = ifelse(title %in% permanent_musics, 1, 0)) %>%
    mutate(type = ifelse(title %in% candidate_musics, 2, type)) %>%
    mutate(type = as.factor(type)) %>%
    ggplot() +
    geom_line(aes(x=date_gap, y=ranking, group=pri_group, color=type)) -> p_groupcol
p_groupcol
## 흠.. 느낌있는듯..?
## 1번(permanent, green)은 대체적으로 횡보한다. 크게 변화하는 구간이 적다.
## 2번(candidate, blue)은 ranking start point가 굉장히 높은 경향이 있다.
## 0번(temporary, orange)은 date gap이 굉장히 짧은곳에 몰려있고, 중간에 역주행 하는 곡도 보인다.
## 거의 끝까지 왔다가 permanent에 아쉽게 속하지 못한 곡도 보인다.

df1 %>%
    mutate(pri_group = paste(title, "::", artist)) %>%
    mutate(type = ifelse(title %in% permanent_musics, 1, 0)) %>%
    mutate(type = ifelse(title %in% candidate_musics, 2, type)) %>%
    mutate(type = as.factor(type)) %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=pri_group, color=type))

## 처음 색 없이 그린 plot을 보니까 다시 보인다.
## 분명히, group이 보일락 말락 한다.



alive_date
titles
permanent_musics
df1 %>%
    mutate(type = ifelse(title %in% permanent_musics, 1, 0)) %>%
    mutate(type = ifelse(title %in% candidate_musics, 2, type)) %>%
    mutate(type = as.factor(type)) %>%
    ungroup() %>% select(title, type) %>% unique() %>% .$type -> alive_type

ggplot(tibble(alive_date, alive_type), aes(x = alive_date/7, fill=alive_type)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "white") +  # histogram 그리기
    geom_density(alpha = 0.5, fill = "orange") +  # density plot 그리기
    # geom_rug(aes(x=alive_date/7+runif(length(alive_date), 0.01, 0.1))) +  # rug plot 그리기 (하단에)
    labs(title = "Histogram and Density Plot", x = "Value", y = "Density") # 제목과 축 레이블 설정



## Modeling 방향성

## 첫 k주차 순위와 순위diff를 보고 permanent인지, temporary인지 예측한다.
## 사실 엄밀히 따지면 permanent, unpredictable, temporary로 구분하는게 맞을 것 같다.
## 하지만 0과 1이므로 일단을 permanent와 temporary로 구분하고자 한다.

