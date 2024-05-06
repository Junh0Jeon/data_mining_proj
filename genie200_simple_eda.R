# data mining

################ Data Crawling : don't need to execute again. Data already Saved.
###############################################
################################################
#################################################
setwd("C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj")
setwd("C:/Users/p5682/Desktop/R_cloud/R_2024/DataMining_proj")
dir()

x <- as.Date("2022-01-03")
x <- as.Date("2024-04-01")

page <- 1
week_cnt <- 118

for(i in 1:week_cnt){
  #print(x)
  x <- x+7
  print(weekdays(x))
}

rm(list=ls())
df1 <- data.frame()
x <- as.Date("2022-01-03")
week_cnt <- 118
for(i in 1:week_cnt) {
  title <- c()
  artist <- c()
  rankingdiff <- c()
  for(page in 1:4) { # page = 1
    url <- paste0("https://www.genie.co.kr/chart/top200?ditc=W&ymd=",
                  gsub("-", "", x),"&hh=13&rtm=N&pg=", page)
    url <- read_html(url)
    
    title <- c(title, html_elements(url, "#body-content .title") %>% html_text() %>% trimws())
    artist <- c(artist, html_elements(url, ".artist") %>% html_text())
    rankingdiff <- c(rankingdiff, html_elements(url, ".rank")[seq(1,100) %% 2 != 0] %>% html_text() %>% trimws())
    Sys.sleep((runif(1, 0.01, 0.05)))
  }
  x <- x+7
  
  df1 <- rbind(df1,
               data.frame(title, artist, ranking = 1:200, rankingdiff, date = rep(x, 200)))
}
tibble(df1)
write.table(df1, "C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj/genie200.txt")
dir()
#####################################
########################################
########################################
# Data Loading
######################################
rm(list=ls())
setwd("C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj")
df1 <- read.table("C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj/genie200.txt")
df1 <- df1 %>% mutate(date = as.Date(date))
tibble(df1)
View(df1)


df1$title %>% unique() %>% length() # 2년동안 966개의 신곡이 in200 했다.


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
par(mfrow=c(1,1))

# plot 보다가 이름이 왜 안나오지... 하면서 Fall In Love Alone 옆에 그래프 보고 찾아봄.
# 왜 이거 했냐면 그냥 얘가 자료가 많아 보여서.
which(titles=="Fall In Love Alone", arr.ind = TRUE) # 623
titles[620:625]
ranking_list[[624]]
df1 %>% filter(title==titles[624])


head(titles)
head(alive_date)
head(ranking_list)

###################################

artists <- unique(df1$artist)
head(artists, 10) ; length(artists)
artists[grepl("&", artists)]

artists2 <- strsplit(artists, " & ", fixed = T) %>% unlist() %>% unique()
head(artists2, 10) ; length(artists2)

artists[!(artists %in% artists2)]
artists2[!(artists2 %in% artists)]

#####################################

par(mfrow=c(2,2))
lapply(head(ranking_list, 4),function(x){plot(x, type="l")})
titles[alive_date==0]
df1 %>%
  filter(title == "고민중독")
tibble(df1) %>%
  filter(rankingdiff=="new") %>%
  filter(title %in% titles[alive_date==0]) # n=117
tibble(df1) %>% filter(rankingdiff == "new") -> new_titles #n=551
alive_date[titles %in% new_titles$title]

######

par(mfrow=c(1,1))
hist(alive_date/7)
plot(density(alive_date/7))

#######

ggplot(tibble(alive_date), aes(x = alive_date/7)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "black", fill = "skyblue") +  # histogram 그리기
  geom_density(alpha = 0.5, fill = "orange") +  # density plot 그리기
  # geom_rug(aes(x=alive_date/7+runif(length(alive_date), 0.01, 0.1))) +  # rug plot 그리기 (하단에)
  labs(title = "Histogram and Density Plot with Rug", x = "Value", y = "Density") # 제목과 축 레이블 설정

########

table(alive_date/7)
table_alive_date <- table(alive_date/7)
table_alive_date <- table_alive_date[order(table_alive_date, decreasing = TRUE)]  # 빈도 기준으로 내림차순 정렬
#########
ggplot(tibble(alive_date)) +
  geom_bar(aes(x=alive_date/7), fill = "skyblue", color = "white")


table_alive_date <- table(alive_date)
df_alive_date <- tibble(freq=factor(names(table_alive_date),levels = as.numeric(names(table_alive_date))),
                        count=as.numeric(table_alive_date))

df_alive_date
df_alive_date$freq
df_alive_date <- df_alive_date %>% mutate(cumul = cumsum(count)/sum(count)*max(count))
df_alive_date

ggplot(df_alive_date) +
  geom_bar(aes(x=freq, y=count), stat="identity", fill="skyblue") +
  geom_line(aes(x=freq, y=cumul, group=1), color="red") +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Cumulative Percentage")) +  # 오른쪽 축 추가 (누적 백분율)
  labs(title = "Pareto Chart", x = "Category", y = "Frequency") +  # 제목과 축 레이블 설정
  theme_minimal() +  # 기본적인 테마 적용
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x 축 레이블 각도 조정
#############

