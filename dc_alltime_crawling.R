url <- "https://gall.dcinside.com/mgallery/board/lists/?id=projectmx&exception_mode=recommend"

webpage <- read_html(url)


static_database = tibble()


rm(list=ls())


time_memory <- 0
static_db <- tibble()
time_db <- tibble()
# static_db = rbind(static_db, tibble(id, subject, title, writer, write_time, start_time))

while(time_memory <= 3600) {
  start_time <- Sys.time()
  time_memory = time_memory + 5
  
  url <- "https://gall.dcinside.com/mgallery/board/lists/?id=projectmx&exception_mode=recommend"
  page <- read_html(url)
  notation_n = sum(html_elements(page, ".us-post .gall_subject") %>% html_text() %in% "공지")
  total_post_n = 50
  post_n = total_post_n - notation_n
  
  id <- html_elements(page, ".us-post .gall_num") %>% html_text() %>% as.numeric() %>% tail(post_n)
  subject <- html_elements(page, ".us-post .gall_subject") %>% html_text() %>% tail(post_n)
  title <- html_elements(page, ".us-post a:nth-child(1)") %>% html_text() %>% gsub("[\r\n\t]+", "", .) %>% tail(post_n)
  writer <- html_elements(page, ".ub-writer em") %>% html_text() %>% tail(post_n)
  write_time <- html_elements(page, ".us-post .gall_date")%>% substr(.,30, 48) %>% tail(post_n)
  if(nrow(static_db) == 0) {
    false_is_new = rep(FALSE, post_n)
  } else {
    false_is_new = (id %in% static_db$id)
  }
  # 새로 등록된 념글은 static_db에 담는다.
  for(i in 1:length(id)) {
    if(!false_is_new[i]) { # i is new recommend post index
      static_db = rbind(static_db,
                        tibble(id = id[i],
                               subject = subject[i],
                               title = title[i],
                               writer = writer[i],
                               write_time = write_time[i],
                               recommended_time = start_time,
                               updated_time = start_time))
    }
  }
  # 1시간 이내의 것만 남긴다.
  static_db = static_db[difftime(Sys.time(), static_db$recommended_time, units="hour") <= 1,]
  
  # static_db를 순회하면서 각 념글 time value를 기록한다.
  for(i in 1:nrow(static_db)) {
    recommend = html_elements(page, ".us-post .gall_recommend") %>% html_text() %>% as.numeric() %>% tail(post_n)
    views = html_elements(page, ".us-post .gall_count") %>% html_text() %>% as.numeric() %>% tail(post_n)
    comments = html_elements(page, ".reply_num") %>% html_text() %>% substr(., 2, nchar(.) - 1) %>% as.numeric() %>% tail(post_n)
    temp_db = tibble(
      now_time = rep(start_time, length(id)),
      id,
      recommend,
      views,
      comments
    )
  }
    # temp_db에는 
    # 15페이지어치 념글의 start_time기준 추천수, 조회수, 댓글수를 temp_db에 담는다.
    temp_db = tibble(
      now_time = rep(start_time, length(id)),
      id,
      recommend,
      views,
      comments
    )
    
  }


  time_db <- rbind(time_db, temp_db)
  
  end_time <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
  Sys.sleep(max(0, 5-end_time))
}

difftime(Sys.time(), start_time, units="hour") >= 1
static_db$start_time
head(time_db$now_time)
tail(time_db$now_time)
difftime(Sys.time(), time_db$now_time, units="hour") %>% tail()
# 알고리즘
# 최초 념글 등록 시간으로부터 24시간이 지난 념글은, 추적을 멈춘다.

# 확인해야 하는 것
# 1. 최초 념글 등록시간
# 2. 현재 시간
# 3. 이 념글 추적 멈추는지 여부

# static_db에는 최초 등록시간이 들어가 있다.
# 현재 시간 - 최초 등록시간을 받는다.
# 그 gap이 1이 넘는 애들만 id를 받는다.
# 

View(static_db)
time_db$id %>% unique() %>% length()
View(time_db)
searching_id = 10
time_db[time_db$id==time_db$id[searching_id],]$recommend %>% plot() ; static_db$title[static_db$id %in% time_db$id[searching_id]]


##########

url_test = "https://gall.dcinside.com/mgallery/board/view/?id=projectmx&no=10594987"
page_test = read_html(url_test)
page_i = 1
