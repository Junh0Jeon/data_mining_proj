url <- "https://gall.dcinside.com/mgallery/board/lists/?id=projectmx&page=2&exception_mode=recommend"

webpage <- read_html(url)
webpage

g_id <- html_elements(webpage, ".us-post .gall_num")
g_type <- html_elements(webpage, ".us-post .gall_subject")
g_recommend <- html_elements(webpage, ".us-post .gall_recommend")
g_type
html_text(g_type)
html_text(g_id)
html_text(g_recommend)

rm(list=ls())
i <- 4

temp_db = tibble()
for(i in 3:30) {
  Sys.sleep(runif(1, 0.25, 0.75))
  url <- paste0("https://gall.dcinside.com/mgallery/board/lists/?id=projectmx&page=",
  i, "&exception_mode=recommend")
  page <- read_html(url)
  
  id <- html_elements(page, ".us-post .gall_num") %>% html_text() %>% as.numeric()
  subject <- html_elements(page, ".us-post .gall_subject") %>% html_text()
  recommend <- html_elements(page, ".us-post .gall_recommend") %>% html_text() %>% as.numeric()
  md <- html_elements(page, ".us-post .gall_date") %>% substr(.,35, 39)
  title <- html_elements(page, ".us-post a:nth-child(1)") %>% html_text() %>% gsub("[\r\n\t]+", "", .)
  temp_db <- rbind(temp_db, tibble(id, subject, recommend, md, title))
}

temp_db
hist(temp_db$recommend)
t = 1:1400
plot(sort(temp_db$recommend)~t)
temp_db$title[temp_db$recommend >= 100]
sum(temp_db$recommend>=100)/1400
sort(temp_db$recommend)[1400*0.8]
temp_db$title[temp_db$recommend >= 87]
