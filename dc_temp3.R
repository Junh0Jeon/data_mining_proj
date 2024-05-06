url = "https://gall.dcinside.com/board/lists/?id=leagueoflegends5&exception_mode=recommend"
page = read_html(url)
recommend = html_elements(page, ".us-post .gall_recommend") %>% html_text() %>% as.numeric()
hist(recommend)
recommend=c();views=c();title=c()
for(page_i in 1:6) {
  url = paste0("https://gall.dcinside.com/board/lists/?id=leagueoflegends6&page=", page_i, "&exception_mode=recommend")
  # 각 객체들은 크롤링시간 기준 15페이지 어치 정보를 가지고 있음.
  # time-var 객체
  page = read_html(url)
  notation_n = sum(html_elements(page, ".us-post .gall_subject") %>% html_text() %in% "공지")
  total_post_n = 50
  post_n = total_post_n - notation_n
  
  title <- c(title, html_elements(page, ".us-post a:nth-child(1)") %>% html_text() %>% gsub("[\r\n\t]+", "", .) %>% tail(post_n))
  recommend = c(recommend, html_elements(page, ".us-post .gall_recommend") %>% html_text() %>% as.numeric() %>% tail(post_n))
  views = c(views, html_elements(page, ".us-post .gall_count") %>% html_text() %>% as.numeric() %>% tail(post_n))
  Sys.sleep(runif(1, 1, 2))
}
hist(recommend)
plot(density(recommend))
plot(sort(recommend)~seq(length(recommend))) ; max(recommend)
plot(ecdf(recommend))
qqnorm(recommend)
qqline(recommend)

hist(views)
qqnorm(views)
qqline(views)
plot(sort(views)~seq(length(views)))

plot(views~recommend)
plot(views[views<10000 & recommend<300]~recommend[views<10000 & recommend<300])
title[views>15000]
title[recommend > 200]
