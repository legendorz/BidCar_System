# write.table(car_type_null, "~/Desktop/價格預測區間/型號遺失值_20180211.csv", sep = ",",
#             row.names = F, fileEncoding = "BIG-5")
library(dplyr)
library(lubridate)
library(anytime)
library(ggplot2)
#讀取資料（76290筆）
car_result = read.csv("~/Desktop/car_list_20180211.csv", 
                      sep = ",",fileEncoding = "utf-8", stringsAsFactors = FALSE)
#將廠牌Honda排氣量179改為1798
car_result$排氣量[car_result$廠牌 == "HONDA" & car_result$排氣量 == 179] = 1798
#將型式SIENNA排氣量31456改為3456
car_result$排氣量[car_result$型式 == "SIENNA" & car_result$排氣量 == 31456] = 3456
#將廠牌MITSUBISHI排氣量2型號為EM198（電動機車）
car_result$型式[car_result$廠牌 == "MITSUBISHI" & car_result$排氣量 == 2] = "EM198"
na_car = car_result[is.na(car_result$排氣量)==TRUE,]
#車號48288、264901排氣量為1991
car_result$排氣量[car_result$車號 == "48288" | car_result$車號 == "264901"] = 1991
#車號RAW-0821、RAW-0822排氣量為1198車齡3年
car_result$排氣量[car_result$車號 == "RAW-0821" | car_result$車號 == "RAW-0822"] = 1198
car_result$車齡[car_result$車號 == "RAW-0821" | car_result$車號 == "RAW-0822"] = 3
car_result = na.omit(car_result)
#拿掉廠牌為空值（76206筆）
car_result = car_result[car_result$廠牌 != "",]
#篩掉機車廠牌KYMCO,SYM,AEON,YAMAHA,KAWASAKI,,DUCATI,APRILIA,TRIUMPH
# SUZUKI GSX-R600, PIAGGIO VESPA GTS, HARLEY-DAVIDSON,BMW HP4,BMW K1200R(沒有成交過)
#拿掉機車廠牌及排氣量為107的車種（76002筆）
motor_type = c("KYMCO","SYM","AEON","YAMAHA","KAWASAKI","DUCATI","APRILIA","TRIUMPH")
car_result = car_result[!car_result$廠牌 %in% motor_type, ]
car_result  = car_result[car_result$排氣量 > 107, ]
#轉換變數
#拿掉法拍車(64575筆)
reg_var= car_result %>%  filter(委拍人類別 != "法拍車")
#仍然有104筆型式是空值
car_type_null = reg_var[reg_var$型式 == "",]
null_type = car_type_null$車號
#拿掉型式是空值及整筆遺失值(64471筆)
reg_var = reg_var[!reg_var$車號 %in% null_type, ]
reg_var = na.omit(reg_var)
#將里程保證為Y的挑出（61249筆）
# n_mile = reg_var[reg_var$里程保證 == "N",]
reg_var = reg_var[reg_var$里程保證 == "Y",]

# reg_var$排氣量 = factor(cut(as.numeric(reg_var$排氣量),
#                          breaks=c(0, 1600, 2000, 3000, Inf),
#                          labels = 1:4,include.lowest=TRUE))
# reg_var$排氣量 = factor(cut(as.numeric(reg_var$排氣量),
#                          breaks=c(0, 1700, Inf),
#                          labels = 1:2,include.lowest=TRUE))
reg_var$委拍人類別 = factor(reg_var$委拍人類別)


#改為factor
# reg_var$車體評價 = factor(reg_var$車體評價, 
#                       levels = c("N.W", "N", "E.W", "E", "D.W", "D", "C.W", "C", "C+.W", "C+", "B.W", "B", "A.W", "A", "A+"))
reg_var$車體評價 = factor(reg_var$車體評價)
# reg_var$車體評價NEW = as.numeric(reg_var$車體評價) 
#改為factor
# reg_var$內裝評價 = factor(reg_var$內裝評價, levels = c("N", "D", "C", "B", "A"))
reg_var$內裝評價 = factor(reg_var$內裝評價)
# reg_var$內裝評價NEW = as.numeric(reg_var$內裝評價)
reg_var$里程保證 = factor(reg_var$里程保證)
# #各顏色平均得標金額
# color_price = group_by(reg_var, 顏色轉換) %>% summarise(平均數 = mean(得標金額))
# color_price$顏色轉換[, order(color_price$平均數)]
# color_price$顏色轉換[order(color_price$平均數)]
reg_var$顏色轉換 = factor(reg_var$顏色轉換, levels = c("綠", "其他", "棕", "黃", "紫", "銀", "藍", "紅", "灰", "黑", "白"))
# reg_var$顏色轉換NEW = as.numeric(reg_var$顏色轉換)
reg_var$car_type = factor(paste(reg_var$廠牌, reg_var$型式))


#將car_type大於30的廠牌挑出
# sort_car_type = as.data.frame(sort(table(reg_var$car_type)))
# more_cartype = as.character(sort_car_type$Var1[sort_car_type$Freq>=30])
# reg_var = reg_var[reg_var$car_type %in% more_cartype, ]
#里程數超過1.5個標準差之車輛用該車款(car_type)之最大里程數取代
#計算1.5倍標準差
car_mile <- group_by(reg_var, car_type) %>%
  summarise(平均里程數 = round(mean(里程數)),
                 里程數標準差 = sd(里程數),
                 該車種1.5倍標準差 = 平均里程數 + 1.5*里程數標準差          
  )
#將計算好欄位合併回資料集
reg_mile = merge(reg_var, car_mile, by = "car_type", all.x = TRUE)
#將超過1.5倍標準差的里程數用1表示
reg_mile$超過標準差 = ifelse(reg_mile$里程數 > reg_mile$該車種1.5倍標準差, 1,  
                        0)
#將標準差算不出的里程數其超過標準差用0表示
reg_mile$超過標準差[which(is.na(reg_mile$里程數標準差) == TRUE)] = 0
#計算各車種里程數的最大值（排除超過標準差之車）
reg_mile_normal = reg_mile[reg_mile$超過標準差 == 0,]
reg_mile_normal <- group_by(reg_mile_normal, car_type) %>%
  summarise(最大里程數 = max(里程數)     
  )
#將計算好欄位合併回資料集
reg_mile = merge(reg_mile, reg_mile_normal, by = "car_type", all.x = TRUE)
reg_mile$轉換後里程數 = ifelse(reg_mile$超過標準差 == 1, reg_mile$最大里程數, reg_mile$里程數)

#盒鬚圖
# box1 = reg_mile %>% select(排氣量) %>% unique()
# par(family="STKaiti")
# boxplot(box1, main = c("排氣量盒鬚圖"))
# box2 = reg_mile %>% select(轉換後里程數) %>% unique()
# par(family="STKaiti")
# boxplot(box2, main = c("轉換後里程數盒鬚圖"))
# box3 = reg_mile %>% select(車齡) %>% unique()
# par(family="STKaiti")
# boxplot(box3, main = c("車齡盒鬚圖"))
# box4 = reg_mile %>% select(得標金額) %>% unique()
# par(family="STKaiti")
# boxplot(box4, main = c("得標金額盒鬚圖"))
# hist1 = reg_mile
# colnames(hist1)[c(5, 7, 9, 10)] = c("commit", "car_evaluate", "color", "inner_evaluate")
# hist1$commit =as.character(hist1$commit)
# hist1$car_evaluate =as.character(hist1$car_evaluate)
# hist1$color =as.character(hist1$color)
# ggplot(hist1) + geom_bar(mapping = aes(x = commit))+theme(text = element_text(family = 'STKaiti'))
# ggplot(hist1) + geom_bar(mapping = aes(x = car_evaluate))+theme(text = element_text(family = 'STKaiti'))
# ggplot(hist1) + geom_bar(mapping = aes(x = color))+theme(text = element_text(family = 'STKaiti'))
# ggplot(hist1) + geom_bar(mapping = aes(x = inner_evaluate))+theme(text = element_text(family = 'STKaiti'))

#regression
log_lmfit = lm(log(得標金額) ~ 排氣量+車體評價+內裝評價 + scale(轉換後里程數)+ 顏色轉換  + car_type*車齡, data = reg_mile)
log_summary = summary(log_lmfit)
# options(max.print=999999)
# capture.output(log_summary, file = "logfile_interaction_addcolor20180206.txt")
#predict
#參考連結:https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
#參考連結:https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
predict_log = predict(log_lmfit, reg_mile, interval= "predict", level=0.5)
log_predict = cbind(reg_mile, predict_log)
log_predict$fit = exp(log_predict$fit)
log_predict$lwr = exp(log_predict$lwr)
log_predict$upr = exp(log_predict$upr)
log_predict = log_predict[,c(1:7, 9:20, 8, 21:23)]



