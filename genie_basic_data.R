#### Data Load ####

##### Raw Data ####
df1 <- read.table("C:/Users/JeonJunho/Desktop/R_cloud/Git_R/data_mining_proj/genie200.txt")
df1 <- df1 %>% mutate(date = as.Date(date))
## 곡명을 uniq로 전처리
genie200 <- df1 %>% mutate(uniq_title = paste(title, " :: ", artist)) %>%
    select(uniq_title, ranking, rankingdiff, date)
rm(df1)
## rankingdiff를 숫자로 전처리
genie200$rankingdiff %>% 
    gsub("(\\d+)하강", "-\\1", .) %>%
    gsub("(\\d+)상승", "\\1", .) %>% gsub("유지", "0", .) %>% 
    gsub("new", "0", .) %>% as.numeric() -> ranking_diff
genie200 <- tibble(genie200) %>%
    mutate(rankingdiff = ranking_diff) %>% tibble()

##### Basic Data ####
alive_date <- c()
ranking_list <- list()
uniq_titles <- unique(genie200$uniq_title)
{# alive_date와 ranking_list를 채우는 process
    alive_date <- c()
    ranking_list <- list()
    cnt <- 1
    for(t in uniq_titles) { 
        temp_df <- genie200 %>% filter(uniq_title == t)
        alive_date <- c(alive_date, tibble(temp_df) %>% slice(c(1, n())) %>% .$date %>% diff.Date())
        ranking_list[[cnt]] <- temp_df$ranking
        # plot(ranking_list[[cnt]], type="l", main=titles[cnt])
        cnt = cnt+1
    }
    rm(cnt, t, temp_df)
}
##### 그 이외 추가되는 변수들... ####

###### 모든 음악들의 생존기간 table ####
table_alive_date <- table(alive_date/7) ; 
# table_alive_date
df_alive_date <- tibble("in200 기간"=factor(names(table_alive_date),levels = as.numeric(names(table_alive_date))),
                        count=as.numeric(table_alive_date))
###### 출시기간 및 마지막 기록날짜 ####
df_release_date <- genie200 %>%
    group_by(uniq_title) %>%
    mutate(release_date = min(date),
           lasting_date = max(date)) %>%
    select(uniq_title, release_date, lasting_date) %>%
    unique() %>%
    mutate(lasting_days = lasting_date - release_date)
###### 음악 타입 분류 ####
permanent_musics <- df_release_date %>% filter(lasting_days >= as.Date("2024-01-01") - as.Date("2022-01-01")) %>% .$uniq_title
temporary_musics <- df_release_date %>% filter(lasting_days < as.Date("2024-01-01") - as.Date("2022-01-01")) %>% .$uniq_title
candidate_musics <- df_release_date %>% 
    filter(lasting_days < as.Date("2024-01-01") - as.Date("2022-01-01")) %>%
    filter(lasting_date == as.Date("2024-04-08")) %>% .$uniq_title
###### 세 타입 적용시킨 데이터셋 ####
genie200_typed <- tibble(genie200) %>%
    mutate(music_type = ifelse(uniq_title %in% permanent_musics, "P", "T")) %>%
    mutate(music_type = ifelse(uniq_title %in% candidate_musics, "C", music_type)) %>%
    mutate(music_type = as.factor(music_type))
###### type, quantile 관련 ####
quantile_typed <- genie200_typed %>%
    group_by(music_type) %>%
    summarise(q01 = quantile(rankingdiff, 0.01),
              q05 = quantile(rankingdiff, 0.05),
              q95 = quantile(rankingdiff, 0.95),
              q99 = quantile(rankingdiff, 0.99))
genie_rankingdiff_quantile <- genie200_typed %>%
    left_join(quantile_typed, by = "music_type")
