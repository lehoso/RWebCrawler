library(rvest)
library(magrittr)
library(jiebaRD)
library(ggplot2)
#载入rvest包
url <- 'https://www.thepaper.cn/'
#需要爬取的网址
web <- read_html(url)
#新闻主页对应的html文档
news <- web %>% html_nodes('h2 a')
#读取html的节点
titles <- news %>% html_text()
#将new传递到text管道函数得到新闻的title
link <- news %>% html_attrs()

link1 <- c(1:length(link))
for (i in 1:length(link)) {
  link1[i] <- link[[i]][1]
}

#得到新闻标签的三项属性信息 href id target
link2 <- paste("https://www.thepaper.cn/", link1, sep = "")
#获取网页的详细信息，【1】代表第一条信息 href内的新闻
news_content <- c(1:length(link2))
for (i in 1:length(link2)) {
  x <-
    read_html(link2[i]) %>% html_nodes('div.video_txt_l p') %>% html_name()
  y <- 'p'
  if (identical(x, y) == TRUE)
    news_content[i] <-
    read_html(link2[i]) %>% html_nodes('div.video_txt_l p') %>% html_text()
  else
    news_content[i] <-
    read_html(link2[i]) %>% html_nodes('div.news_txt ') %>% html_text()
}
#获取新闻每篇上架时间
news_date <- c(1:length(link2))
for (i in 1:length(link2)) {
  news_date[i] <-
    (read_html(link2[i]) %>% html_nodes('div p') %>% html_text(''))[2]
}

date <- c(1:length(link2))
time <- c(1:length(link2))
for (i in 1:length(link2)) {
  date[i] <- strsplit(news_date, split = ' ')[[i]][21]
  time[i] <- strsplit(news_date, split = ' ')[[i]][22]
}

news_01 <- data.frame(titles, date, time, url = link2, news_content)
save(news_01, file = "thepaper1.Rdata")

#获取正文的函数，读取link2，传递到nodes里，仅需获取正文text
write.csv(news_01, file = "newspaperto.csv")

#中文分析分词
library(jiebaR)
wk <- worker()
words <-  segment(news_content, wk)
library(dplyr)
library(stringr)

#停用库测试
wktest <- worker(stop_word = 'stop_words.txt')
wordstest <- segment(news_content,wktest)
text_dftest <- tibble(line= c(1:length(wordstest)),word = wordstest)
text_dftest%>% count(word,sort=TRUE)


#画图
text_dftest %>%
count(word,sort=TRUE) %>%
  filter(n > 40) %>%
  mutate(word =reorder(word,n))%>%
  ggplot(aes(word , n)) +
  geom_col(fill="pink") +
  xlab(NULL)+
  coord_flip()