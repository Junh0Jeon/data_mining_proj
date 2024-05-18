genie200

genie200 %>%
    select(date) %>%
    slice(1, n())


# 기존에 permanent를 730일 동안 생존한 음악으로 정의내렸다.
# 이건 근데 4월까지 얻은 데이터라... 2년짜리 데이터만 있다고 치자.
################ 2024 04까지 보자면... ####
genie200 %>%
    filter(date <= as.Date("2024-01-01")) -> genie200_2year

genie200_2year %>%
    group_by(uniq_title) %>%
    mutate(release_date = min(date),
           lasting_date = max(date)) %>%
    select(uniq_title, release_date, lasting_date) %>%
    unique() %>%
    mutate(lasting_days = lasting_date - release_date) %>%
    ungroup() -> df_release_date_2year

df_release_date_2year %>%
    filter(lasting_days < threshold_date & lasting_date == max(lasting_date)) %>%
    .$uniq_title -> candidate_musics_2year
df_release_date_2year %>%
    filter(lasting_days == threshold_date) %>%
    .$uniq_title -> permanent_musics_2year
df_release_date_2year %>%
    filter(lasting_days < threshold_date & lasting_date != max(lasting_date)) %>%
    .$uniq_title -> temporary_musics_2year
# 나온지 2년 안된 노래들에 대해선 판단을 보류해야함.
# 즉, temporary musics는 lasting_days < 2years but not lasting_date = latest

permanent_musics[!(permanent_musics %in% permanent_musics_2year)] # new permanent musics ( imported to permanent )
temporary_musics[temporary_musics %in% candidate_musics_2year] # new temporary musics ( imported to temporary )
