#載入套件
library(anytime)
library(lubridate)
library(dplyr)
library(reshape2)
library(recommenderlab)
######################讀取歷史車輛資料並將最新一筆二手車資訊資料併入歷史車輛資料########################
history_car = read.csv("D:/big_data_exchange/kpmg_output/history/car_list.csv", 
                       sep = ",",fileEncoding = "utf-8")
car_file = file.info(list.files("D:/big_data_exchange/acer_output/cardata"))
bid_car = read.csv(paste("D:/big_data_exchange/acer_output/cardata/", 
                         rownames(car_file)[order(car_file$mtime)][nrow(car_file)],sep = ""))
car_result <- rbind(history_car, bid_car)
car_result <- unique(car_result)
filename = file(paste0("D:/big_data_exchange/kpmg_output/history/", "car_list.csv"), encoding="UTF-8")
write.csv(car_result, filename, row.names = FALSE)
######################讀取歷史拍賣資料並將最新一筆拍賣結果資料併入歷史拍賣資料#########################
history_result = read.csv("D:/big_data_exchange/kpmg_output/history/result_list.csv", 
                       sep = ",",fileEncoding = "utf-8")
history_result$日期 = anydate(as.Date(history_result$日期,format="%Y-%m-%d"))
result_file = file.info(list.files("D:/big_data_exchange/acer_output/result"))
result = read.csv(paste("D:/big_data_exchange/acer_output/result/", 
                     rownames(result_file)[order(result_file$mtime)][nrow(result_file)], sep = ""))
result$日期 = anydate(as.Date(result$日期,format="%Y/%m/%d"))
result = select(result, -X)
result_result <- unique(rbind(history_result, result))
filename = file(paste0("D:/big_data_exchange/kpmg_output/history/", "result_list.csv"), encoding="UTF-8")
write.csv(result_result, filename, row.names = FALSE)
#read history bid
history_bid = read.csv("D:/big_data_exchange/kpmg_output/history/bid_list.csv", 
                       sep = ",",fileEncoding = "utf-8")
history_bid$日期 = anydate(history_bid$日期,"%Y/%m/%d")
bid_file = file.info(list.files("D:/big_data_exchange/acer_output/bidcard"))
bid = read.csv(paste("D:/big_data_exchange/acer_output/bidcard/", 
                     rownames(bid_file)[order(bid_file$mtime)][nrow(bid_file)], sep = ""))
bid = select(bid, -X)
bid$日期 <-  anydate(bid$日期, "%Y/%m/%d")
bid_bid <- unique(rbind(history_bid, bid))
filename = file(paste0("D:/big_data_exchange/kpmg_output/history/", "bid_list.csv"), encoding="UTF-8")
write.csv(bid_bid, filename, row.names = FALSE)
#合併舊和新拍賣資料
bid_result = merge(bid_bid, result_result, by.x = c("車號","日期"),
                   by.y = c("車號","日期"), all.x = TRUE)
bid_result = unique(select(bid_result, c("車號", "日期", "會員", "金額", "結果.y")))
#僅保留車號,日期,會員,金額,結果.y欄位並依照金額排列後保留最後一筆車輛
bid_result = unique(select(bid_result, c("車號", "日期", "會員", "金額", "結果.y"))) %>% 
  arrange(金額) %>% group_by(車號) %>% filter(row_number()==n())
#將missing的結果.y補為流標
bid_result$結果.y[is.na(bid_result$結果.y)] = "流標"
#資料前處理和合併拍賣結果、車輛資料
car_result$拍賣日期 <- anydate(as.Date(car_result$拍賣日期, format="%Y/%m/%d"))
car_result = car_result[car_result$里程數!=9999999,]
car_result$出廠年 <- substring(car_result$出廠年月, 1, 4)
car_result$車齡 <- year(car_result$拍賣日期)-as.integer(car_result$出廠年)
car_result$車號 <- as.character(car_result$車號)
bid_result$車號 <- as.character(bid_result$車號)
result_kpmg <- merge(bid_result, car_result,  by.x = c("車號", "日期"),
                          by.y = c("車號", "拍賣日期"))
#合併完car_data後仍有重複值,原因為cardata資料key錯因此再次篩選依照金額排列的最後一筆車號, 篩選重要變數、分箱、轉換名目變數
model_result = result_kpmg %>% select("車號", "日期", "會員", "金額", 
                                      "廠牌", "型式","委拍人類別","排氣量",
                                      "車體評價","內裝評價","里程數", "里程保證", 
                                      "車齡", "結果.y")  %>% 
  arrange(金額) %>% group_by(車號)  %>% filter(row_number()==n())

model_result$委拍人類別 = as.factor(as.character(model_result$委拍人類別))
model_result$排氣量 =cut(as.numeric(model_result$排氣量), 
                 breaks=c(0, 1600, 2000, 3000, Inf), 
                 labels = 1:4,include.lowest=TRUE)
model_result$里程數 =cut(as.numeric(model_result$里程數), 
                 breaks=c(0, 58932, 100046, 150234, Inf), 
                 labels = 1:4,include.lowest=TRUE)
model_result$委拍人類別 = as.factor(as.character(model_result$委拍人類別))
model_result$車體評價 = as.factor(as.character(model_result$車體評價))
model_result$內裝評價 = as.factor(as.character(model_result$內裝評價))
model_result$里程保證 = as.factor(as.character(model_result$里程保證))
####################挑選出成交率高之車子#####################
test1 = model_result %>% filter(委拍人類別%in% c("法拍車", "租賃車"))

test2 = model_result %>% filter(委拍人類別 == "會務委拍" ,
                                     車體評價 %in% c("A", "A+", "D.W", "N", "N.W"),
                                     排氣量 %in% c(1,2,3), 內裝評價 %in% c("A","C","D"),
                                     里程數 %in% c(1,2), 內裝評價 %in% c("C"), 車齡 == 1)
test3 = model_result %>% filter(委拍人類別 == "會務委拍" ,
                                     車體評價 %in% c("A", "A+", "D.W", "N", "N.W"),
                                     排氣量 %in% c(1,2,3), 內裝評價 %in% c("B", "N"),
                                     車體評價 %in% c("A+", "N"),
                                     里程保證 =="Y")
test4 = model_result %>% filter(委拍人類別 == "會務委拍" ,
                                     車體評價 %in% c("A", "A+", "D.W", "N", "N.W"),
                                     排氣量 %in% c(1,2,3), 內裝評價 %in% c("B", "N"),
                                     車體評價 %in% c("A+", "N"),
                                     里程保證 == "N", 里程數 == 1)
recommend_result = rbind(test1, test2, test3, test4)
#將最新拍賣車資訊做和先前一樣之轉換
bid_car$日期 <- anydate(as.Date(bid_car$拍賣日期, format="%Y/%m/%d"))
bid_car$編號 <- as.numeric(bid_car$編號)
bid_car$出廠年 <- substring(bid_car$出廠年月, 1, 4)
bid_car$車齡 <- year(bid_car$拍賣日期)-as.integer(bid_car$出廠年)
bid_car$車號 <- as.character(bid_car$車號)
bid_car$排氣量 =cut(bid_car$排氣量, 
                 breaks=c(0, 1600, 2000, 3000, Inf), 
                 labels = 1:4,include.lowest=TRUE)
bid_car$里程數 =cut(as.numeric(bid_car$里程數), 
                 breaks=c(0, 58932, 100046, 150234, Inf), 
                 labels = 1:4,include.lowest=TRUE)
bid_car = bid_car %>% select("車號", "委拍人類別","排氣量","車體評價", 
                             "內裝評價","里程數", "里程保證", "車齡"
) 


###########################從歷史資料挑選出當天拍賣才有車輛#################################
recommend_result$車體評價 = paste("body",recommend_result$車體評價, sep = " ")
bid_car$車體評價 = paste("body",bid_car$車體評價, sep = " ")
recommend_result$內裝評價 = paste("inner",recommend_result$內裝評價, sep = " ")
bid_car$內裝評價 = paste("inner",bid_car$內裝評價, sep = " ")
recommend_result$mix  = paste(recommend_result$委拍人類別, recommend_result$排氣量,
                         recommend_result$車體評價,recommend_result$內裝評價,
                         recommend_result$里程數, recommend_result$里程保證,
                         recommend_result$車齡)
bid_car$mix  = paste(bid_car$委拍人類別, bid_car$排氣量,
                     bid_car$車體評價, bid_car$內裝評價,
                     bid_car$里程數, bid_car$里程保證,
                     bid_car$車齡)
bid_car_mix = as.character(unique(bid_car$mix))
recommend_result = recommend_result %>% 
  filter( mix %in% bid_car_mix) 
#build model data
model_data = recommend_result %>% group_by(會員, mix) %>% 
  summarise(score = sum(n()))
names(model_data)[1] = c("user")
model_wide = dcast(model_data, user ~ mix, value.var = "score" , index="user")
model_wide[is.na(model_wide)] <- 0
vector_users <- model_wide[, "user"]
ratingDF <- model_wide[,2:ncol(model_wide)]
rownames(ratingDF) <- vector_users 
ratingMat <- Matrix(as.matrix(ratingDF), sparse = TRUE)
ratings_matrix_train <- as(ratingMat, "realRatingMatrix")

#build model
recc_model <- Recommender(data = ratings_matrix_train, method = "UBCF", param=list(method="cosine",nn= 5))
recc_predicted <- predict(object = recc_model,newdata = ratings_matrix_train, n = 20,
                          type = "topNList") 
recc_matrix <- sapply(recc_predicted@items, 
                      function(x){colnames(ratings_matrix_train)[x]})
recc_matrix_frame = data.frame(ID = rep(names(recc_matrix), sapply((recc_matrix), length)),
                  Obs = unlist(recc_matrix))
#####################產出推薦車輛######################
recc_matrix_frame2 = matrix(unlist(t(recc_matrix_frame$Obs)), byrow=F, 20, nrow(recc_matrix_frame)/20)
name = c()
recc_matrix_frame$ID = as.character(recc_matrix_frame$ID)
#挑選最新日期
last_day = gsub("_[a-z]+.[a-z]+", "",rownames(car_file)[order(car_file$mtime)][nrow(car_file)])
bid_day = gsub("-", "/",as.character(anydate(last_day)))
id_number <- nrow(recc_matrix_frame)/20
for(i in 1:id_number){
  id = recc_matrix_frame$ID[20*i-19]
  name = append(name,id)
}
colnames(recc_matrix_frame2) = name
output = c()
for(i in 1:ncol(recc_matrix_frame2)){
  output2 =c()
  a = c(rep(colnames(recc_matrix_frame2)[i],20))
  b =c(1:20)
  c = c(recc_matrix_frame2[1:20,i])
  d =c(rep(bid_day, 20))
  output2 = cbind(output2,a, b, c, d)
  output = rbind(output, output2)
}
output = as.data.frame(output)
bid_car$車號 = as.character( bid_car$車號)
output$c = NA
for(i in 1:nrow(output)){
  output$c[i] =  bid_car$車號[bid_car$mix == recc_matrix_frame$Obs[i]][1]
}
#########################add TOP20 to unrecommend##################
top20 = names(sort(table(output$c),decreasing=TRUE)[1:20])
recommend_id = as.character(unique(output$a))
total_id = na.omit(as.character(unique(history_bid$會員)))
index = total_id %in% recommend_id
poor_id = total_id[which(index == FALSE)]
poor_output = c()
for(i in 1:length(poor_id)){
  output3 =c()
  a = c(rep(poor_id[i],20))
  b =c(1:20)
  c = top20
  d =c(rep(bid_day, 20))
  output3 = cbind(output3,a, b, c, d)
  poor_output = rbind(poor_output, output3)
}
poor_output = as.data.frame(poor_output)
#############################輸出資料#################################
output = rbind(output, poor_output)
colnames(output) = c("id", "ser", "no", "date")
filename = file(paste0("D:/big_data_exchange/kpmg_output/list/", last_day,"_list.csv"), encoding="UTF-8")
write.csv(output, filename, row.names = FALSE, quote = FALSE)
