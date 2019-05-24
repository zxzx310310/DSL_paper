#----時間紀錄(開始)----
startTime <- Sys.time()

#----資料初始化(本地端)----
sourceData <- read.csv(file = "assets/商品資料庫_s.csv") #讀取原始資料
preferenceTable <- read.csv(file = "assets/preferenceTable_s.csv") #讀取商品偏好表
sourceData <- sourceData[c(-1, -13)] #移除不必要的資料欄位
names(sourceData)[11] <- "重量" #重新命名欄位名稱
goodData <- sourceData #將原始資料複製一份
goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #新增被選擇欄位

#----環境參數設定----
maxVolume <- 13000 #最大箱子體積
maxWeight <- 16000 #最大重量(g)
userItemValues <- 10 #使用者需要的數量
maxPrice <- 550 #使用者金額

#----Function----
#偏好值與類別合併:
#將使用者對商品種類的偏好與原始商品資料進行合併成一個Data Frame, 使原始資料有使用者對每個商品的品項偏好
preference_match <- function(good_data, preference_table) {
  #gene_list: 被選擇出的基因清單
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #user_preference: 使用者對商品種類的偏好
  
  for (i in 1:dim(preference_table)[1]) {
    # good_data[good_data$種類==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])^2
    good_data[good_data$種類==preference_table$category[i],]$Preference <- as.numeric(preference_table$preference[i])
  }
  return(good_data)
}

#計算總重量
total_weight <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  
  for(i in 1:length(gene_list)) {
    sum_weight <- sum(gene_list[[i]][[1]]$'重量')
    gene_list[[i]]["totalWeight"] <- list(sum_weight)
  }
  return(gene_list)
}

#偏好的適應度方法(算式分母為偏好值1~偏好的最大值)
fitness_preference <- function(gene_list, require_goods, non_require_values, preference_table) {
  #gene_list: 被選擇出的基因清單
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #user_preference: 使用者對商品種類的偏好
  
  max_preference <- max(preference_table$preference)
  for(i in 1:length(gene_list)) {
    reuslt <- 1
    for (k in 1:sum(length(require_goods), non_require_values)) {
      temp_preferenced <- 1+as.numeric((gene_list[[i]][[1]]$'Preference'[k])^2 - 1) / sum((1:max_preference)^2) #偏好的計算公式
      sum_preferenced <- sum(gene_list[[i]][[1]]$'Preference')
      reuslt <- reuslt*temp_preferenced
    }
    gene_list[[i]]["fitPreference"] <- list(reuslt)
    gene_list[[i]]["totalPreference"] <- sum_preferenced
  }
  return(gene_list)
}

#體積的適應度方法(已加入懲罰值)
fitness_volume <- function(gene_list, bin_volume) {
  #gene_list: 被選擇出的基因清單
  #bin_volume: 箱子的乘積
  
  for (i in 1:length(gene_list)) {
    sum_volume <- sum(gene_list[[i]][[1]]$'體積') #將最大限制體積減去每個基因的總體積
    subtraction_volume <- bin_volume-sum_volume #容積上限與選擇商品之總體積的差額
    reuslt <- abs(subtraction_volume)/bin_volume #將體積適應度算出
    
    if (sum_volume >=(bin_volume*0.6) & sum_volume <=bin_volume) {
      if (subtraction_volume==0) {
        reuslt <- reuslt + 1 #若適應度等於0就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
      } else {
        reuslt <- reuslt + 2 #若適應度大於0就給予懲罰值2
      } 
    } else {
      reuslt <- reuslt + 3 #剩下結果將給予懲罰值3
    }
    gene_list[[i]]["fitVolume"] <- reuslt 
    gene_list[[i]]["totalVolume"] <- sum_volume 
  }
  return(gene_list)
}

#價格的適應度方法(已加入懲罰值)
fitness_price <- function(gene_list, limit_price) {
  #gene_list: 被選擇出的基因清單
  #limit_price: 價格最高限制
  
  for (i in 1:length(gene_list)) {
    sum_price <- sum(gene_list[[i]][[1]]$'單價') #將最大限制金額減去每個基因的總金額
    subtraction_price <- limit_price-sum_price #預算與商品組合之總價格的差額
    reuslt <- abs(subtraction_price)/limit_price #將價格適應度算出
    if (subtraction_price==0) {
      reuslt <- reuslt + 1
    } else if(subtraction_price>0){
      reuslt <- reuslt + 2
    } else {
      reuslt <- reuslt + 3
    }
    gene_list[[i]]["fitPrice"] <- reuslt
    gene_list[[i]]["totalPrice"] <- sum_price
  }
  return(gene_list)
}

#總體的適應度方法
fitness_total <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  
  sum_fit <- unlist(lapply(gene_list, function(x) x$fitVolume*x$fitPrice))
  
  for (i in 1:length(gene_list)) {
    sum_fit <- gene_list[[i]]$'fitVolume'*gene_list[[i]]$'fitPrice'*gene_list[[i]]$'fitPreference'
    gene_list[[i]]["totalFit"] <- sum_fit
  }
  return(gene_list)
}

filter_weight <- function(gene_list, limit_weight, limit_volume) {
  condition_pop <- list() 
  for (i in 1:length(gene_list)) {
    if(gene_list[[i]]$'totalWeight' <= limit_weight & gene_list[[i]]$'totalVolume' <= limit_volume & gene_list[[i]]$'totalVolume' >= (limit_volume*0.6)){
      condition_pop <- append(condition_pop, gene_list[i]) #將未超過限制重量的染色體放入新的群組
    }
  }
  condition_pop <- condition_pop[order(sapply(condition_pop, function(x) x$totalFit), decreasing=FALSE)] #將人口按照適應函數遞減排序
  return(condition_pop)
}


#---執行----
level <- levels(goodData$種類)
level <- level[order(nchar(level), level)]
requiredList <- level[1:6]
nonRequiredList <- level[-1:-length(requiredList)]

goodData <- preference_match(good_data = goodData, preference_table = preferenceTable)

goodKind <- list()
for (i in 1:length(level)) {
  goodKind[[i]] <- goodData[goodData$'種類' == level[i], ]
}

combination <- list()
result <- data.frame(產品代號 = factor(), 品名 = factor(), 單價 = integer(), 體積 = numeric(), 廠牌 = factor(), 長 = numeric(), 寬 = numeric(), 高 = numeric(), 種類 = factor(), 葷素 = factor(), 重量 = integer(), Selected = numeric(), Preference = numeric())

index <- 1
for (a1 in 1:nrow(goodKind[[1]])) {
  for (b1 in 1:nrow(goodKind[[2]])) {
    for (c1 in 1:nrow(goodKind[[3]])) {
      for (d1 in 1:nrow(goodKind[[4]])) {
        for (e1 in 1:nrow(goodKind[[5]])) {
          for (f1 in 1:nrow(goodKind[[6]])) {
            for (g1 in 1:nrow(goodKind[[7]])) {
              for (g2 in 1:nrow(goodKind[[8]])) {
                for (h1 in 1:nrow(goodKind[[9]])) {
                  for (i1 in 1:nrow(goodKind[[10]])) {
                    result <- rbind(result, goodKind[[1]][a1,])
                    result <- rbind(result, goodKind[[2]][b1,])
                    result <- rbind(result, goodKind[[3]][c1,])
                    result <- rbind(result, goodKind[[4]][d1,])
                    result <- rbind(result, goodKind[[5]][e1,])
                    result <- rbind(result, goodKind[[6]][f1,])
                    result <- rbind(result, goodKind[[7]][g1,])
                    result <- rbind(result, goodKind[[8]][g2,])
                    result <- rbind(result, goodKind[[9]][h1,])
                    result <- rbind(result, goodKind[[10]][i1,])
                    combination[[index]] <- list(result)
                    index = index +1
                    result <- data.frame(產品代號 = factor(), 品名 = factor(), 單價 = integer(), 體積 = numeric(), 廠牌 = factor(), 長 = numeric(), 寬 = numeric(), 高 = numeric(), 種類 = factor(), 葷素 = factor(), 重量 = integer(), Selected = numeric(), Preference = numeric())
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
  
#----時間紀錄(結束)----


combination <- total_weight(gene_list = combination)
combination <- fitness_preference(gene_list = combination, require_goods = requiredList, non_require_values =  nonRequiredValues, preference_table = preferenceTable)
combination <- fitness_volume(gene_list = combination, bin_volume = maxVolume)
combination <- fitness_price(gene_list = combination, limit_price = maxPrice)
combination <- fitness_total(gene_list = combination)
combination <- filter_weight(gene_list = combination, limit_weight = maxWeight, limit_volume = maxVolume)


endTime <- Sys.time()
resultTime <- endTime - startTime
print(resultTime)
  
  
  