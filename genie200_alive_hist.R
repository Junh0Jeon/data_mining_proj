# Eda Part

#####################################
########################################
########################################
# Data Loading
######################################
rm(list=ls())
setwd("C:/Users/JeonJunho/Desktop/R_cloud/R_2024/DataMining_proj")
setwd("C:/Users/p5682/Desktop/R_cloud/R_2024/DataMining_proj")
df1 <- read.table("genie200.txt") ;
df1 <- tibble(df1) %>% mutate(date = as.Date(date))

df1
DT::datatable(df1)

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

# 2. 곡들의 in200 기간 histogram

# hist(alive_date/7)
# plot(density(alive_date/7))

ggplot(tibble(alive_date), aes(x = alive_date/7)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "white", fill = "skyblue") +  # histogram 그리기
  geom_density(alpha = 0.5, fill = "orange") +  # density plot 그리기
  # geom_rug(aes(x=alive_date/7+runif(length(alive_date), 0.01, 0.1))) +  # rug plot 그리기 (하단에)
  labs(title = "Histogram and Density Plot", x = "Value", y = "Density") # 제목과 축 레이블 설정

ggplot(tibble(alive_date)) +
  geom_bar(aes(x=alive_date/7), fill = "skyblue", color = "white")

table_alive_date <- table(alive_date) ; table_alive_date
df_alive_date <- tibble(freq=factor(names(table_alive_date),levels = as.numeric(names(table_alive_date))),
                        count=as.numeric(table_alive_date))
df_alive_date

## 거의 다 