install.packages("pacman")  #先安装这个包，方便一键加载其他包
pacman::p_load(XML,rvest,dplyr,stringr)
house_inf <- data.frame()
#爬取前50页
for (i in 1:50) {
  #发现url规律，利用字符串函数进行url拼接并规定编码：
  web <- read_html(str_c("https://cq.lianjia.com/ershoufang/", 82), encoding = "UTF-8")
  #提取房名信息：
  house_name <- web %>% html_nodes(".item a") %>% html_text()
  house_name2 <- ifelse(nchar(house_name) > 8, house_name, NA)
  house_name2 <- na.omit(house_name2)
  #提取房名基本信息并消除空格
  house_basic_inf <- web %>% html_nodes(".houseInfo") %>% html_text()
  house_basic_inf <- str_replace_all(house_basic_inf, " ", "")
  #提取二手房地址
  house_address <- web %>% html_nodes(".positionInfo a") %>% html_text()
  house_address <- str_replace_all(house_address, " ", "")
  house_address <- data.frame(matrix(house_address, ncol = 2, nrow = 30, byrow = T))
  house_qu <- house_address[, 1]
  house_district <- house_address[, 2]

  #提取二手房总价
  house_totalprice <- web %>% html_nodes(".totalPrice") %>% html_text()
  #提取二手房单价
  house_unitprice <- web %>% html_nodes(".unitPrice span") %>% html_text()
  #创建数据框存储以上信息
  house <- data.frame(house_name2, house_basic_inf, house_qu, house_district, house_totalprice, house_unitprice)
  house_inf <- rbind(house_inf, house)
}
length(house_basic_inf)
length(house_name)

#将数据写入csv文档
#去除总价里除了数字以外的其他文字字符
house_inf <- na.omit(house_inf)
price <- house_inf$house_totalprice
price <- stringr::str_remove_all(as.character(price), "[^0-9]")
house_inf$price <- as.numeric(price)
#按房价总价进行排序
house_inf2 <- house_inf[order(house_inf[, 7]),]
#去掉重复的房价信息
house_inf2 <- house_inf2[!duplicated(house_inf2),]
#将结果存为csv格式，路径修改
write.csv(house_inf2, file = "price.csv", row.names = F)
hist(house_inf$price)