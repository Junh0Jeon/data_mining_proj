# 중간고사 Review

# genie_basic_data로 data가 불러온 상태에서 실행바람

#### 모든 음악들의 생존 기간 plot ####
ggplot(tibble(alive_date), aes(x = alive_date/7)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "white", fill = "skyblue") +  # histogram 그리기
    geom_density(alpha = 0.5, fill = "orange") +  # density plot 그리기
    # geom_rug(aes(x=alive_date/7+runif(length(alive_date), 0.01, 0.1))) +  # rug plot 그리기 (하단에)
    labs(title = "Histogram and Density Plot", x = "연속 in200 시기(단위 : 주)", y = "Density")

#### 모든 음악들의 생존기간 table ####
table_alive_date <- table(alive_date/7) ; 
# table_alive_date
df_alive_date <- tibble("in200 기간"=factor(names(table_alive_date),levels = as.numeric(names(table_alive_date))),
                        count=as.numeric(table_alive_date))
df_alive_date %>% arrange(desc(count))

#### 음악 type 정의 ####
## permanent : 2년 이상
## temporary : 2년 미만
## candidate : 2년 미만 중에서, 지금까지 유지중인 곡들.
df_release_date <- genie200 %>%
    group_by(uniq_title) %>%
    mutate(release_date = min(date),
           lasting_date = max(date)) %>%
    select(uniq_title, release_date, lasting_date) %>%
    unique() %>%
    mutate(lasting_days = lasting_date - release_date)
# df_release_date
permanent_musics <- df_release_date %>% filter(lasting_days >= as.Date("2024-01-01") - as.Date("2022-01-01")) %>% .$uniq_title
temporary_musics <- df_release_date %>% filter(lasting_days < as.Date("2024-01-01") - as.Date("2022-01-01")) %>% .$uniq_title
candidate_musics <- df_release_date %>% 
    filter(lasting_days < as.Date("2024-01-01") - as.Date("2022-01-01")) %>%
    filter(lasting_date == as.Date("2024-04-08")) %>% .$uniq_title

#### 세 타입의 랭킹플롯 ####
genie200_typed <- tibble(genie200) %>%
    mutate(music_type = ifelse(uniq_title %in% permanent_musics, "P", "T")) %>%
    mutate(music_type = ifelse(uniq_title %in% candidate_musics, "C", music_type)) %>%
    mutate(music_type = as.factor(music_type))

genie200_typed %>%
    ggplot() +
    geom_line(aes(x=date, y=ranking, group=uniq_title, color=music_type))

## 초록(permanent)이 이상해서 초록만 빼서 봐봄.
genie200_typed %>%
    filter(music_type == "P") %>%
    ggplot() +
    geom_line(aes(x = date, y = ranking, group = uniq_title))
## 쟤 쟤 쟤는 뭐냐 혼자 위아래 왔다갔다 하는놈
genie200_typed %>%
    filter(music_type == "P") %>%
    filter(rankingdiff > 50)
## ㅋㅋ... 봄노래 크리스마스노래 유행노래구나. 좀 더 자세히 볼까?

#### 세 타입의 랭킹디프 플롯 ####
quantile_typed <- genie200_typed %>%
    group_by(music_type) %>%
    summarise(q01 = quantile(rankingdiff, 0.01),
              q05 = quantile(rankingdiff, 0.05),
              q95 = quantile(rankingdiff, 0.95),
              q99 = quantile(rankingdiff, 0.99))
quantile_typed
genie200_typed %>%
    ggplot() +
    geom_density(aes(x=rankingdiff, fill = music_type), alpha = 0.5) +
    geom_vline(data = quantile_typed, aes(xintercept = q05, color = music_type), linetype = "dotted") +
    geom_vline(data = quantile_typed, aes(xintercept = q95, color = music_type), linetype = "dotted") +
    geom_vline(data = quantile_typed, aes(xintercept = q01, color = music_type), linetype = "longdash") +
    geom_vline(data = quantile_typed, aes(xintercept = q99, color = music_type), linetype = "longdash") +
    labs(color = "music_type", 
         x = "rankingdiff", 
         y = "Density",
         title = "Density Plot with Quantiles (0.05, 0.95)") +
    theme_minimal()
## 각자의 type에서, 유난히 갭이 컸던 곡들을 확인해보자.
## 원래 상하위 5%만 볼랬는데,C랑 P가 10위 변동을 threshold로 잡아버렸다.
genie_rankingdiff_quantile <- genie200_typed %>%
    left_join(quantile_typed, by = "music_type") 
## 원래 목적은 너무 차이나는 (line48 plot)같은걸 보는거였기 때문에, 상하위 1%를 보기로 했다.
genie_rankingdiff_quantile %>%


genie200_typed %>%
    filter(music_type == "P") %>%
    ggplot() +
    geom_line(aes(x = date, y = ranking, group = uniq_title))
genie_rankingdiff_quantile %>%
    filter(rankingdiff < q01 | rankingdiff > q99) %>%
    filter(music_type == "P")
## P의 경우, 계절노래가 이러한 노래에 속했다.
## 재밌는점은, "계절노래"가 P에 속했다는 점이다. 즉, 항상 200위권 안에 있었다는 이야기이다.
genie200_typed %>%
    filter(music_type == "T") %>%
    ggplot() +
    geom_line(aes(x = date, y = ranking, group = uniq_title))
genie_rankingdiff_quantile %>%
    filter(rankingdiff < q01 | rankingdiff > q99) %>%
    filter(music_type == "T")
## T의 경우, 특징짓기 어려웠다.
genie200_typed %>%
    filter(music_type == "C") %>%
    ggplot() +
    geom_line(aes(x = date, y = ranking, group = uniq_title))
genie_rankingdiff_quantile %>%
    filter(rankingdiff < q01 | rankingdiff > q99) %>%
    filter(music_type == "C")
