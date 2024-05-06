# trash code

# 념글 15페이지까지 크롤링 및 각 객체에 연속시켜서 담음.
for(page_i in 1:15) {
  # 각 객체들은 크롤링시간 기준 15페이지 어치 정보를 가지고 있음.
  # static 객체
  id <- c(id, html_elements(page, ".us-post .gall_num") %>% html_text() %>% as.numeric() %>% tail(post_n))
  subject <- c(subject, html_elements(page, ".us-post .gall_subject") %>% html_text() %>% tail(post_n))
  title <- c(title, html_elements(page, ".us-post a:nth-child(1)") %>% html_text() %>% gsub("[\r\n\t]+", "", .) %>% tail(post_n))
  writer <- c(writer, html_elements(page, ".ub-writer em") %>% html_text() %>% tail(post_n))
  write_time <- c(write_time, html_elements(page, ".us-post .gall_date")%>% substr(.,30, 48) %>% tail(post_n))
  # time-var 객체
  recommend = c(recommend, html_elements(page, ".us-post .gall_recommend") %>% html_text() %>% as.numeric() %>% tail(post_n))
  views = c(views, html_elements(page, ".us-post .gall_count") %>% html_text() %>% as.numeric() %>% tail(post_n))
  comments = c(comments, html_elements(page, ".reply_num") %>% html_text() %>% substr(., 2, nchar(.) - 1) %>% as.numeric() %>% tail(post_n))
  Sys.sleep(runif(1, 0.05, 0.1))
}

while(TRUE) {
  id <- c(id, html_elements(page, ".us-post .gall_num") %>% html_text() %>% as.numeric() %>% tail(post_n))
  subject <- c(subject, html_elements(page, ".us-post .gall_subject") %>% html_text() %>% tail(post_n))
  title <- c(title, html_elements(page, ".us-post a:nth-child(1)") %>% html_text() %>% gsub("[\r\n\t]+", "", .) %>% tail(post_n))
  writer <- c(writer, html_elements(page, ".ub-writer em") %>% html_text() %>% tail(post_n))
  write_time <- c(write_time, html_elements(page, ".us-post .gall_date")%>% substr(.,30, 48) %>% tail(post_n))
  
  in_day_index <- difftime(start_time, write_time, units="hour") <= 24
}