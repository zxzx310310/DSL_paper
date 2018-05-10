#----資料初始化(本地端)----
sourceData <- read.csv(file = "assets/StoreData.csv") #讀取原始資料
goodData <- sourceData #將原始資料複製一份

goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #新增被選擇欄位


#----資料初始化(資料庫)----
library(RMySQL)

mydb = dbConnect(MySQL(), user='root', password='', dbname='product', host='127.0.0.1')
dbSendQuery(mydb,"SET NAMES big5")
result <- dbSendQuery(mydb, 'SELECT * FROM storedata')

sourceData <- fetch(result)
goodData <- sourceData #將原始資料複製一份
goodData <- cbind(goodData, "Selected" = 0, "Preference" = 1) #新增被選擇欄位

#----環境參數設定----
alpha <- 0.9 #體積鬆弛因子
requiredList <- c("油", "米", "醬油", "酒", "鹽", "糖", "麵食") #必需品商品
nonRequiredList <- c("沖泡", "罐頭", "飲品", "泡麵", "零食") #非必需品商品
#categoryList <- c("油", "米", "醬油", "酒", "鹽", "糖", "麵食", "沖泡", "罐頭", "飲品", "泡麵", "零食") #必需品商品
#Vegetarian <- 0 #是否為素食(0為否, 1為是, 3為不限)

#canQt <- 2 #罐頭數量
#drinkQt <- 3 #飲料數量
#InstNoodlesQt <- 3 #泡麵數量
#snackQt <- 4 #零食數量

popAmount <- 30 #人口數量

crossRate <- 0.7 #交配率
mutationRate <- 0.01 #突變率
eliteValues <- 1 #菁英數量
maxGen <- 500 #世代次數


#----使用者需輸入的參數(假設)----
maxVolume <- 52*38*28 #最大箱子體積
maxPrice <- 1500 #最大金額
maxWeight <- 13000 #最大重量
#exceptBrandList <- sample(c(levels(goodData$'廠牌'), NA), 2) #將要剔除的品牌
nonRequiredValues <- 14 #選擇性商品的數量
#dietHabit <- sample(c("素食", "葷食"), 1) #葷食與素食的選擇
dietHabit <- "素食"
userPreference <- sample(c(1:5), length(nonRequiredList), replace = TRUE)




#----單方測試----
#隨機選擇必要商品(單方執行)-1
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="油",]), size = 1)
# getIndex <- which(goodData$種類=="油")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="米",]), size = 1)
# getIndex <- which(goodData$種類=="米")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="醬油",]), size = 1)
# getIndex <- which(goodData$種類=="醬油")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="酒",]), size = 1)
# getIndex <- which(goodData$種類=="酒")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="糖",]), size = 1)
# getIndex <- which(goodData$種類=="糖")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="麵食",]), size = 1)
# getIndex <- which(goodData$種類=="麵食")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="沖泡",]), size = 1)
# getIndex <- which(goodData$種類=="沖泡")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# if(Vegetarian==0) {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(葷)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(葷)")[requiredGood]
#   goodData$Selected[getIndex] <- 1  
# } else if(Vegetarian==1) {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(素)")[requiredGood]
#   goodData$Selected[getIndex] <- 1  
# } else {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)",]), size = canQt)
#   getIndex <- which(goodData$種類=="罐頭(素)" | goodData$種類=="罐頭(葷)")[requiredGood]
#   goodData$Selected[getIndex] <- 1
# }
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="飲品",]), size = drinkQt)
# getIndex <- which(goodData$種類=="飲品")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="泡麵",]), size = InstNoodlesQt)
# getIndex <- which(goodData$種類=="泡麵")[requiredGood]
# goodData$Selected[getIndex] <- 1
# 
# requiredGood <- sample(1:nrow(goodData[goodData$種類=="零食",]), size = snackQt)
# getIndex <- which(goodData$種類=="零食")[requiredGood]
# goodData$Selected[getIndex] <- 1



#隨機選擇必要商品(單方執行)-2
# for (i in 1:length(requiredList)) {
#   requiredGood <- sample(1:nrow(goodData[goodData$種類==requiredList[i],]), size = 1) #隨機選擇一個每個種類的必要商品
#   getIndex <- which(goodData$種類==requiredList[i])[requiredGood] #抓取被選擇的必要商品
#   if(goodData$Selected[getIndex] != 1) {
#     goodData$Selected[getIndex] <- 1 #將被抓取的商品從0改為1  
#   } else {
#     i <- i-1 #如果該商品被選過, 則重新迴圈
#   }
# }

# for (i in 1:nonRequiredValues) {
#   categoryGoods <- sample(nonRequiredList, 1)
#   requiredGood <- sample(1:nrow(goodData[goodData$種類==categoryGoods,]), size = 1)
#   getIndex <- which(goodData$種類==categoryGoods)[requiredGood]
#   if(goodData$Selected[getIndex] != 1) {
#     goodData$Selected[getIndex] <- 1 
#   } else {
#     i <- i-1 #如果該商品被選過, 則重新迴圈
#   }
# }

# crossOverList <- fitnessAfter #先將資料移轉給另外一個變數
# for (i in 1:length(crossOverList)) {
#   crossOverList[[i]]["corssOverState"] <- 0 #因所有基因都尚未被交配, 所以給0作為代表
# }
# for (i in 1:2) {
#   selected <- sample(crossOverList, 2)
#   
#   selected[[1]]$corssOverState <- 1 #因被選擇的基因被交配, 所以給1作為代表
#   selected[[2]]$corssOverState <- 1 #因被選擇的基因被交配, 所以給1作為代表
# }


#交配測試-1
# getChromLength <- length(requiredList)+nonRequiredValues #取得染色體長度
# rnd <- sort(sample(getChromLength, 2)) #隨機選擇切割地方(採雙點交配)
# fitnessAfter[[1]]["crossState"] <- 0
# fitnessAfter[[2]]["crossState"] <- 0
# tempChrom_A <- fitnessAfter[[1]] #先將染色體給暫時變數A
# tempChrom_B <- fitnessAfter[[2]] #先將染色體給暫時變數B
# tempChrom_A$chromosome[c(rnd[1]+1):rnd[2]] <- fitnessAfter[[2]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
# tempChrom_B$chromosome[c(rnd[1]+1):rnd[2]] <- fitnessAfter[[1]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
# tempChrom_A[[1]][c(rnd[1]+1):rnd[2]] <- fitnessAfter[[2]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的商品給第一個基因
# tempChrom_B[[1]][c(rnd[1]+1):rnd[2]] <- fitnessAfter[[1]]$chromosome[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的商品給第二個基因
# fitnessAfter[[1]]["crossState"] <- 1
# fitnessAfter[[2]]["crossState"] <- 1


#交配測試-2
# geneList <- fitnessPriceAfter
# paste("交配率:", crossRate) #顯示交配率
# for (i in 1:length(fitnessPriceAfter)) {
#   #先給予交配狀態, 0表示未交配, 1表示已交配
#   geneList[[i]]["crossState"] <- 0
# }
# getChromLength <- length(requiredList)+nonRequiredValues #取得染色體長度
# 
# for(i in 1:(length(fitnessPriceAfter)/2)){
#   getCrossState <- unlist(lapply(geneList, function(x) x$crossState)) #給定目前交配狀態
#   paste("目前交配狀態為:", getCrossState) #顯示交配狀態
#   # if (length(getCrossState==1)==length(fitnessPriceAfter)) {
#   getIndex <- as.vector(sample(which(getCrossState!=1),2)) #抽取要被交配的基因
#   paste("被抽取到的基因為:", getIndex[1], ",", getIndex[2]) #顯示被抽取到基因的index
#   rndCrossRate <- round(runif(n = 1, min = 0, max = 1),3) #產生亂數
#   paste("亂數交配率:", rndCrossRate) #顯示亂數的交配率
#   
#   if(rndCrossRate<=crossRate) {
#     #亂數小於等於交配率, 則進行交配
#     
#     divideIndex <- sort(as.vector(sample(getChromLength, 2))) #隨機選擇切割地方(採雙點交配)
#     paste("分割染色體的位置為:", divideIndex[1], ",", divideIndex[2]) #顯示需要被分割的位置
#     
#     tempChrom_A <- geneList[[getIndex[1]]] #先將染色體給暫時變數A
#     tempChrom_B <- geneList[[getIndex[2]]] #先將染色體給暫時變數B
#     tempChrom_A$'chromosome'[(divideIndex[1]+1):divideIndex[2]] <- geneList[[getIndex[2]]]$'chromosome'[(divideIndex[1]+1):divideIndex[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
#     tempChrom_B$'chromosome'[(divideIndex[1]+1):divideIndex[2]] <- geneList[[getIndex[1]]]$'chromosome'[(divideIndex[1]+1):divideIndex[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
#     tempChrom_A[[1]][(divideIndex[1]+1):divideIndex[2],] <- geneList[[getIndex[2]]][[1]][(divideIndex[1]+1):divideIndex[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
#     tempChrom_B[[1]][(divideIndex[1]+1):divideIndex[2],] <- geneList[[getIndex[1]]][[1]][(divideIndex[1]+1):divideIndex[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
#     tempChrom_A$'crossState' <- 1
#     tempChrom_B$'crossState' <- 1
#     geneList[[getIndex[1]]]<- tempChrom_A
#     geneList[[getIndex[2]]]<- tempChrom_B
#     print("交配囉!")
#   } else {
#     #亂數大於交配率, 則不進行交配
#     geneList[[getIndex[1]]]$'crossState' <- 1
#     geneList[[getIndex[2]]]$'crossState' <- 1 
#     print("沒交配!")
#   }
#   # } else {
#   #   print("所有基因皆已交配!")
#   # }
# }


#突變測試
# paste("突變率:", mutationRate) #顯示突變率
# getChromLength <- length(requiredList)+nonRequiredValues #基因染色體的長度
# tempList <- crossAfter #先將資料給另外一個變數
# 
# for(i in 1:20) {
#   mutationIndex <- as.numeric(sample(getChromLength, 1)) #隨機取得要突變的位置(有問題的地方: 第二次執行後前面資料皆會覆蓋)
#   print(paste("突變的位置為:", mutationIndex)) #顯示突變位置
#   
#   rndMutationRate <- runif(n = 1, min = 0, max = 1) #產生亂數
#   print(paste("亂數突變率:", rndMutationRate)) #顯示亂數的突變率
#   if(rndMutationRate <= mutationRate){
#     mutationCategory <- as.character(tempList[[i]][[1]][mutationIndex,]$'種類') #取得基因中要被突變的染色體商品種類
#     mutationList <- goodData[goodData$種類==mutationCategory,] #取得原始資料中符合要被突變的商品種類
#     print(mutationCategory)
#     
#     repeat {
#       #重複篩選商品, 直到沒有與原本基因相同的商品
#       rndMutationValue <- sample(nrow(mutationList), 1) #隨機取得該種類的商品
#       print(paste(tempList[[i]][[1]][mutationIndex,]$'產品代號', "和", mutationList[rndMutationValue,]$'產品代號'))
#       if(tempList[[i]][[1]][mutationIndex,]$'產品代號'!=mutationList[rndMutationValue,]$'產品代號'){
#         #當商品兩者不同時, 則跳出, 表示已拿到非重複的商品
#         print(paste("結果是", tempList[[1]][[1]][mutationIndex,]$'產品代號', "和", mutationList[rndMutationValue,]$'產品代號'))
#         break
#       }
#     }
#     tempList[[i]][[1]][mutationIndex,] <- mutationList[rndMutationValue,] #將商品進行變異
#     tempList[[i]]$chromosome[mutationIndex] <- as.character(mutationList[rndMutationValue,]$產品代號) #將基因進行變異 
#     print("完成突變!")
#   } else {
#     print(paste(rndMutationRate, "<=", mutationRate)) #顯示目前突變率
#   }
#   print(paste("========第", i, "次========"))
# }


#選擇並菁英政策
# newPop <- fitnessTotalAfter #將此代基因放入新的變數
# newPop <- append(newPop, mutationAfter) #將下代基因加入變數
# 
# for (i in 1:length(newPop)) {
#   #產生菁英狀態
#   newPop[[i]]["elite"] <- 0 #將菁英狀態皆設定為0, 0表示不是菁英值; 1表示被挑選為菁英值
# }
# 
# resultPop <- list() #初始最終結果族群清單
# 
# lastList <- head(order(unlist(lapply(newPop, function(x) x$totalFit)), decreasing = FALSE), popAmount) #取得popAmount(數量)的族群
# for (z in 1:length(eliteValues)) {
#   #將最好的適應函數設定為精英值, 並放入新的群組
#   newPop[[lastList[z]]]$'elite' <- 1
#   resultPop[[z]] <- newPop[[lastList[z]]] #將菁英的基因放入新的族群中
# }
# 
# for (k in (length(eliteValues)+1):popAmount) {
#   #將剩下的基因加入到族群
#   resultPop[[k]] <- newPop[[lastList[k]]]
# }


#----檢驗區----


# goodData[goodData$'Selected'==1,] #檢驗已被選擇的商品
# 
# selectedGood <- goodData[goodData$'Selected'==1,]
# sum(selectedGood$'體積')
# sum(selectedGood$'單價')
# 
# allFitPrice <- unlist(lapply(fitnessAfter, function(x) x$fitPrice)) #拉出所有價格的fitness
# min(allFitPrice) #找出最小的數值
# which.min(allFitPrice) #找出最小數值的index
# 
# #檢測是否有違反體積限制而變為NA值的資料
# if(length(which(is.na(fitnessAfter))) != 0) {
#   paste("有違反體積限制的基因數為: ", length(which(is.na(fitnessAfter))))
# } else {
#   paste("所有基因完全符合體積限制, 共", length(which(!is.na(fitnessAfter))), "個")
# }
# 
# 
# #檢驗交配後的結果
# for(i in 1:20){
#   print(crossAfter[[i]]$'chromosome' == fitnessPriceAfter[[i]]$'chromosome')
# }
# 
# #檢驗突變後的結果
# for(i in 1:20) {
#   print(mutationAfter[[i]]$'chromosome'==mutationAfter[[i]]$'chromosome')
# }



#----Function----
#偏好值與類別合併, 並且將偏好值平方
preference_match <- function(good_data, require_goods, non_require_goods, user_preference) {
  total_list <- as.character(c(require_goods, non_require_goods)) #將必需性商品與選擇性商品類別合併
  good_preference <- data.frame(category = total_list, preference = c(sample(1, length(require_goods), replace = TRUE), user_preference)) #將商品類別和偏好值合併為DF型態
  
  for (i in 1:dim(good_preference)[1]) {
    good_data[good_data$種類==good_preference$category[i],]$Preference <- as.numeric(good_preference$preference[i])^2
  }
  
  return(good_data)
}



#剔除品牌的方法
# except_brand <- function(good_data, except_brand_list) {
#   #good_data: 原始商品資料集
#   #except_brand_list: 剔除品牌的名稱
#   
#   for (i in 1:length(except_brand_list)) {
#     good_data <- good_data[good_data$'廠牌'!=except_brand_list[i],] #將要剔除的廠牌移除
#   }
#   return(good_data)
# }



diet_select <- function(good_data, diet_habit_list) {
  #good_data: 原始商品資料集
  #diet_habit_list: 葷食或素食的選擇
  
  if(diet_habit_list=="素食") {
    good_data <- good_data[good_data$'葷素'==diet_habit_list,] #如果是素食就將屬於素食的產品篩選出來
  }
  
  return(good_data)
}


#初始人口方法(素食選擇)
# initial_pop <- function(good_data, category_list) {
#   for (i in 1:length(category_list)) {
#     #若為罐頭則判斷是否為素食
#     if(category_list[i]=="罐頭") {
#       if(Vegetarian==0) {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(葷)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(葷)")[requiredGood]
#         good_data$Selected[getIndex] <- 1  
#       } else if(Vegetarian==1) {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(素)")[requiredGood]
#         good_data$Selected[getIndex] <- 1  
#       } else {
#         requiredGood <- sample(1:nrow(good_data[good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)",]), size = canQt)
#         getIndex <- which(good_data$種類=="罐頭(素)" | good_data$種類=="罐頭(葷)")[requiredGood]
#         good_data$Selected[getIndex] <- 1
#       }
#     } else if(category_list[i]=="飲品") {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = drinkQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else if(category_list[i]=="泡麵") {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = InstNoodlesQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else if(category_list[i]=="零食") {  
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = snackQt)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     } else {
#       requiredGood <- sample(1:nrow(good_data[good_data$種類==category_list[i],]), size = 1)
#       getIndex <- which(good_data$種類==category_list[i])[requiredGood]
#       good_data$Selected[getIndex] <- 1 
#     }
#   }
#   #print(good_data[good_data$Selected==1,])
# }


#初始人口方法(無素食, 選擇性商品為參數)
# initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values) {
#   #good_data: 原始商品資料集
#   #require_goods: 必要性的商品清單
#   #non_require_goods: 不必要性的商品清單
#   #non_require_values: 不必要性的商品數量
#   
#   for (i in 1:length(require_goods)) {
#     requiredGood <- sample(1:nrow(good_data[good_data$種類==require_goods[i],]), size = 1) #依必要商品的每個類別去隨機挑選出商品
#     
#     repeat {
#       #重複篩選必要性商品, 直到沒有與原本商品相同
#       getIndex <- which(good_data$種類==require_goods[i])[requiredGood] #取得剛才隨機挑選的商品位置
#       if(good_data$Selected[getIndex] != 1) {
#         good_data$Selected[getIndex] <- 1 #將被選擇的欄位改為1  
#         break
#       }
#     }
#   }
#   
#   for (i in 1:non_require_values) {
#     categoryGoods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
#     requiredGood <- sample(1:nrow(good_data[good_data$種類==categoryGoods,]), size = 1) #依選擇性商品的類別去隨機挑選出商品
#     
#     repeat {
#       #重複篩選選擇性商品, 直到沒有與原本商品相同
#       getIndex <- which(good_data$種類==categoryGoods)[requiredGood] #取得剛才隨機挑選的商品位置
#       if(good_data$Selected[getIndex] != 1) {
#         good_data$Selected[getIndex] <- 1 #將被選擇的欄位改為1 
#         break
#       }
#     }
#   }
#   selectedGood <- good_data[good_data$Selected==1,] #將所有被選擇的欄位為1的資料拉出
#   return(selectedGood) #回傳結果
# }


#初始人口方法2(無素食, 選擇性商品為參數)
# initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values) {
#   #good_data: 原始商品資料集
#   #require_goods: 必要性的商品清單
#   #non_require_goods: 不必要性的商品清單
#   #non_require_values: 不必要性的商品數量
#   
#   for (i in 1:length(require_goods)) {
#     get_index <- sample(which(good_data$種類==require_goods[i] & good_data$Selected!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
#     good_data$Selected[get_index] <- 1 #將被選擇的欄位改為1    
#   }
#   
#   for (i in 1:non_require_values) {
#     category_goods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
#     get_index <- sample(which(good_data$種類==category_goods & good_data$Selected!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
#     good_data$Selected[get_index] <- 1 #將被選擇的欄位改為1 
#   }
#   
#   
#   selected_good <- good_data[good_data$Selected==1,] #將所有被選擇的欄位為1的資料拉出
#   return(selected_good) #回傳結果
# }


#初始人口方法3(無素食, 選擇性商品為參數, 重量為硬性條件) 
initial_pop <- function(good_data, require_goods, non_require_goods, non_require_values, limit_weight) {
  #good_data: 原始商品資料集
  #require_goods: 必要性的商品清單
  #non_require_goods: 不必要性的商品清單
  #non_require_values: 不必要性的商品數量
  #limit_weight: 最大重量限制
  
  temp_good <- good_data #先將原始資料暫時給另外一個變數使用
  
  for (i in 1:length(require_goods)) {
    get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
    temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1    
  }
  
  for (i in 1:non_require_values) {
    category_goods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
    get_index <- sample(which(temp_good$'種類'==category_goods & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
    temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1 
  }
  
  selected_good <- temp_good[temp_good$'Selected'==1,] #將被選擇的商品放入新變數中
  sum_weight <- sum(selected_good$'重量') #計算此基因的總重量
  
  while (sum_weight > limit_weight) {
    #重量的硬性條件
    #先開始判別第一次產生的初始人口總重量有沒有超過預先的最大值, 若有就繼續隨機選擇商品出來做初始人口, 直到總重量沒有大於最大值
    
    print(paste("重量超過:", sum_weight, ">", limit_weight))
    temp_good <- good_data #重新給一次原始資料, 因selected欄位有被修改過
    
    for (i in 1:length(require_goods)) {
      get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      while (dim(temp_good[temp_good$'種類'==require_goods[i],])[1]==1) {
        #若該種類的商品數量等於1, 則重新隨機選擇其他類別
        get_index <- sample(which(temp_good$'種類'==require_goods[i] & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      }
      temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1    
    }
    
    for (i in 1:non_require_values) {
      category_goods <- sample(non_require_goods, 1) #隨機挑選選擇性商品的類別
      while (dim(temp_good[temp_good$'種類'==category_goods,])[1]==1) {
        #如果取得到的位置該商品只有一個且已經被選擇, 沒有商品可選時, 再重新挑選種跟商品
        remove_index <- which(non_require_goods==category_goods)
        category_goods <- sample(non_require_goods[-remove_index], 1) #隨機挑選選擇性商品的類別
      }
      get_index <- sample(which(temp_good$'種類'==category_goods & temp_good$'Selected'!=1), 1) #隨機抓出符合種類並Selected欄位不等於1的列
      temp_good$'Selected'[get_index] <- 1 #將被選擇的欄位改為1 
    }
    
    selected_good <- temp_good[temp_good$'Selected'==1,] #將被選擇的商品重新放入變數中
    sum_weight <- sum(selected_good$'重量') #計算此基因的總重量
  }
  
  return(selected_good) #回傳結果
}



#編碼染色體
create_chromosome <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  for(i in 1:length(gene_list)) {
    chromosome <- as.vector(gene_list[[i]][[1]]$'產品代號')
    gene_list[[i]]["chromosome"] <- list(chromosome) 
  }
  return(gene_list)
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

#偏好的適應度方法(未完成)
fitness_preference <- function(gene_list) {
  #被選擇出的基因清單
  print("開始計算偏好適應函數...")
  for(i in 1:length(gene_list)) {
    sum_preference <- sum(gene_list[[i]][[1]]$'Preference')
    gene_list[[i]]["totalPreference"] <- list(sum_preference) 
  }
  return(gene_list)
}


#體積的適應度方法(已加入懲罰值)
fitness_volume <- function(gene_list, bin_volume, volume_alpha) {
  #gene_list: 被選擇出的基因清單
  #bin_volume: 箱子的乘積
  #volume_alpha: 箱子的體積鬆弛因子
  print("開始計算體積適應函數...")
  for (i in 1:length(gene_list)) {
    limit_volume <- bin_volume*volume_alpha #體積與鬆弛因子相乘
    sum_volume <- sum(gene_list[[i]][[1]]$'體積') #將最大限制體積減去每個基因的總體積
    reuslt <- (limit_volume-sum_volume)/limit_volume #將體積適應度算出
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #若適應度等於0或大於0並小於等於0.1就給予懲罰值1, e.g. (49795.2-27749.25)/49795.2=0.4427324, 愈接近0表示價格差距越小
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #若適應度大於0.1並小於等於0.5就給予懲罰值2
    } else {
      reuslt <- reuslt + 3 #剩下結果將給予懲罰值3
    }
    gene_list[[i]]["fitVolume"] <- reuslt 
    print(paste("體積適應函數:", reuslt))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#價格的適應度方法(已加入懲罰值)
fitness_price <- function(gene_list, limit_price) {
  #gene_list: 被選擇出的基因清單
  #limit_price: 價格最高限制
  print("開始計算價格適應函數...")
  for (i in 1:length(gene_list)) {
    sum_price <- sum(gene_list[[i]][[1]]$'單價') #將最大限制金額減去每個基因的總金額
    reuslt <- (limit_price-sum_price)/limit_price #將價格適應度算出
    if (reuslt==0 || reuslt>0 && reuslt<=0.1) {
      reuslt <- reuslt + 1 #若適應度等於0或大於0並小於等於0.1就給予懲罰值1, e.g. (1200-1123)/1200=0.06416667, 愈接近0表示價格差距越小
    } else if (reuslt>0.1 && reuslt<=0.5) {
      reuslt <- reuslt + 2 #若適應度大於0.1並小於等於0.5就給予懲罰值2
    } else {
      reuslt <- reuslt + 3 #剩下結果將給予懲罰值3
    }
    gene_list[[i]]["fitPrice"] <- reuslt
    print(paste("價格適應函數:", reuslt))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}

#總體的適應度方法
fitness_total <- function(gene_list) {
  #gene_list: 被選擇出的基因清單
  
  print("開始計算總體適應函數...")
  sum_fit <- unlist(lapply(fitnessPriceAfter, function(x) x$fitVolume*x$fitPrice))
  
  for (i in 1:length(gene_list)) {
    sum_fit <- gene_list[[i]]$'fitVolume'*gene_list[[i]]$'fitPrice'
    
    gene_list[[i]]["totalFit"] <- sum_fit
    print(paste("總體適應函數:", sum_fit))
    print(paste("========第", i, "個基因========"))
  }
  return(gene_list)
}


#交配(雙點交配)-錯誤, 未考慮到適應函數值及未採用菁英政策和交配率
# cross_over <- function(gene_list, require_goods, non_require_values) {
#   #gene_list: 被選擇出的基因清單
#   #require_goods: 必要性的商品清單
#   #non_require_values: 不必要性的商品數量
#   
#   for (i in 1:length(gene_list)) {
#     #先給予交配狀態, 0表示未交配, 1表示已交配
#     gene_list[[i]]["crossState"] <- 0
#   }
#   
#   get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度
# 
#   if (length(which(!is.na(gene_list)))%%2==0) {
#     #若基因數量為雙數時開始執行
# 
#     for (i in 1:length(gene_list)/2) {
#       get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState))
# 
#       if (length(get_cross_state==1)==length(gene_list)) {
#         get_index <- sample(which(get_cross_state!=1),2)
#         rnd <- sort(sample(get_chrom_length, 2)) #隨機選擇切割地方(採雙點交配)
#         tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
#         tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
#         tempChrom_A$'chromosome'[c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[2]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
#         tempChrom_B$'chromosome'[c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[1]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
#         tempChrom_A[[1]][c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[2]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第二個基因切割的商品給第一個基因
#         tempChrom_B[[1]][c(rnd[1]+1):rnd[2]] <- gene_list[[get_index[1]]]$'chromosome'[c(rnd[1]+1):rnd[2]] #開始進行交配, 將第一個基因切割的商品給第二個基因
#         gene_list[[get_index[1]]]$'crossState' <- 1
#         gene_list[[get_index[2]]]$'crossState' <- 1  
#       }
#     }
#   }
#   return(gene_list)
# }


#交配(雙點交配)-需考慮適應函數值(包含懲罰執)和交配率
# cross_over <- function(gene_list, require_goods, non_require_values, cross_rate) {
#   for (i in 1:length(gene_list)) {
#     #先給予交配狀態, 0表示未交配, 1表示已交配
#     gene_list[[i]]["crossState"] <- 0
#   }
#   get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度
#   print("開始交配")
#   
#   for(i in 1:c(length(gene_list)/2)){
#     get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState)) #給定目前交配狀態
#     # paste("目前交配狀態為:", get_cross_state) #顯示交配狀態
#     get_index <- as.vector(sample(which(get_cross_state!=1),2)) #抽取要被交配的基因
#     # paste("被抽取到的基因為:", get_index[1], ",", get_index[2]) #顯示被抽取到基因的index
#     rnd_cross_rate <- round(runif(n = 1, min = 0, max = 1),3) #產生亂數
#     # paste("亂數交配率:", rnd_cross_rate) #顯示亂數的交配率
# 
#     if(rnd_cross_rate<=cross_rate) {
#       #亂數小於等於交配率, 則進行交配
# 
#       divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #隨機選擇切割地方(採雙點交配)
#       # paste("分割染色體的位置為:", divide_index[1], ",", divide_index[2]) #顯示需要被分割的位置
# 
#       tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
#       tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
#       tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
#       tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
#       tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
#       tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
#       tempChrom_A$'crossState' <- 1
#       tempChrom_B$'crossState' <- 1
#       gene_list[[get_index[1]]] <- tempChrom_A
#       gene_list[[get_index[2]]] <- tempChrom_B
#       print("交配囉!")
#       print(paste("========第", i, "對========"))
#     } else {
#       #亂數大於交配率, 則不進行交配
#       gene_list[[get_index[1]]]$'crossState' <- 1
#       gene_list[[get_index[2]]]$'crossState' <- 1
#       print("沒交配!")
#       print(paste("========第", i, "對========"))
#     }
#   }
#   return(gene_list)
# }



#交配(雙點交配)-需考慮適應函數值(包含懲罰值)、交配率和重量限制
cross_over <- function(gene_list, require_goods, non_require_values, cross_rate, limit_weight) {

  for (i in 1:length(gene_list)) {
    #先給予交配狀態, 0表示未交配, 1表示已交配
    gene_list[[i]]["crossState"] <- 0
  }
  get_chrom_length <- length(require_goods)+non_require_values #取得染色體長度
  print("開始交配")
  
  for(i in 1:c(length(gene_list)/2)){
    get_cross_state <- unlist(lapply(gene_list, function(x) x$crossState)) #給定目前交配狀態
    rnd_cross_rate <- round(runif(n = 1, min = 0, max = 1),3) #產生亂數
    get_index <- as.vector(sample(which(get_cross_state!=1),2)) #抽取要被交配的基因
    
    loop_value <- 0
    
    if(rnd_cross_rate<=cross_rate) {
      #亂數小於等於交配率, 則進行交配
      
      divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #隨機選擇切割地方(採雙點交配)
      
      tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
      tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
      tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
      tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
      tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
      tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
      tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'重量') #重新計算總重量
      tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'重量') #重新計算總重量
      
      while (tempChrom_A$'totalWeight' > limit_weight || tempChrom_B$'totalWeight' > limit_weight && length(tempChrom_A$chromosome)!=length(unique(tempChrom_A$chromosome))) {
        loop_value <- loop_value+1
        
        print(paste("重量超過:", tempChrom_A$'totalWeight', "或", tempChrom_B$'totalWeight', ">", limit_weight, "或 有重複值"))
        
        get_index <- as.vector(sample(which(get_cross_state!=1),2)) #重新抽取要被交配的基因
        divide_index <- sort(as.vector(sample(get_chrom_length, 2))) #重新隨機選擇切割地方(採雙點交配)
        
        tempChrom_A <- gene_list[[get_index[1]]] #先將染色體給暫時變數A
        tempChrom_B <- gene_list[[get_index[2]]] #先將染色體給暫時變數B
        tempChrom_A$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[2]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第二個基因切割的染色體給第一個基因
        tempChrom_B$'chromosome'[(divide_index[1]+1):divide_index[2]] <- gene_list[[get_index[1]]]$'chromosome'[(divide_index[1]+1):divide_index[2]] #開始進行交配, 將第一個基因切割的染色體給第二個基因
        tempChrom_A[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[2]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第二個基因切割的商品給第一個基因
        tempChrom_B[[1]][(divide_index[1]+1):divide_index[2],] <- gene_list[[get_index[1]]][[1]][(divide_index[1]+1):divide_index[2],] #開始進行交配, 將第一個基因切割的商品給第二個基因
        tempChrom_A$'totalWeight' <- sum(tempChrom_A[[1]]$'重量') #重新計算總重量
        tempChrom_B$'totalWeight' <- sum(tempChrom_B[[1]]$'重量') #重新計算總重量
        
        # if(loop_value > 500) {
        #   print("已成無窮解, 不進行交配")
        #   break
        # }
        temp_log <- as.numeric(table(tempChrom_A$'chromosome'==tempChrom_B$'chromosome')["TRUE"]) 
        
        if(temp_log == get_chrom_length) {
          break
        }
      }
      
      tempChrom_A$'crossState' <- 1
      tempChrom_B$'crossState' <- 1
      gene_list[[get_index[1]]] <- tempChrom_A
      gene_list[[get_index[2]]] <- tempChrom_B
      
      print("交配囉!")
      print(paste("========第", i, "對========"))
    } else {
      #亂數大於交配率, 則不進行交配
      gene_list[[get_index[1]]]$'crossState' <- 1
      gene_list[[get_index[2]]]$'crossState' <- 1
      print("沒交配!")
      print(paste("========第", i, "對========"))
    }
  }
  return(gene_list)
}


#突變方法
# mutation_FN <- function(gene_list, mutation_rate, require_goods, non_require_values, soure_data) {
#   #gene_list: 已交配過的基因人口群
#   #mutation_rate: 交配率
#   #require_goods: 必要性商品清單
#   #non_require_values: 選擇性商品數量
#   #soure_data: 原始資料(未經過修改的)
#   
#   get_chrom_length <- length(require_goods)+non_require_values #基因染色體的長度
#   temp_list <- gene_list #先將資料給另外一個變數
#   print("開始突變")
#   
#   for(i in 1:length(gene_list)) {
#     mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置(有問題的地方: 第二次執行後前面資料皆會覆蓋)
#     rnd_mutation_rate <- runif(n = 1, min = 0, max = 1) #產生亂數
#     print(paste("亂數突變率:", rnd_mutation_rate)) #顯示亂數的突變率
#     
#     if(rnd_mutation_rate <= mutation_rate){
#       mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'種類') #取得基因中要被突變的染色體商品種類
#       mutation_list <- soure_data[soure_data$'種類'==mutation_category,] #取得原始資料中符合要被突變的商品種類
#       
#       repeat {
#         #重複篩選商品, 直到沒有與原本基因相同的商品
#         rnd_mutation_value <- sample(nrow(mutation_list), 1) #隨機取得該種類的商品
#         if(temp_list[[i]][[1]][mutation_index,]$'產品代號'!=mutation_list[rnd_mutation_value,]$'產品代號'){
#           #當商品兩者不同時, 則跳出, 表示已拿到非重複的商品
#           break
#         }
#       }
#       temp_list[[i]][[1]][mutation_index,] <- mutation_list[rnd_mutation_value,] #將商品進行變異
#       temp_list[[i]]$'chromosome'[mutation_index] <- as.character(mutation_list[rnd_mutation_value,]$'產品代號') #將基因進行變異 
#       print("完成突變!")
#     } else {
#       print(paste(rnd_mutation_rate, "<=", mutation_rate)) #顯示目前突變率
#     }
#     print(paste("========第", i, "次========"))
#   }
#   return(temp_list)
# }


#突變方法, 加入重量限制
mutation_FN <- function(good_data, gene_list, mutation_rate, require_goods, non_require_values, soure_data, limit_weight) {
  #good_data: 商品資料集
  #gene_list: 已交配過的基因人口群
  #mutation_rate: 交配率
  #require_goods: 必要性商品清單
  #non_require_values: 選擇性商品數量
  #soure_data: 原始資料(未經過修改的)
  
  get_chrom_length <- length(require_goods)+non_require_values #基因染色體的長度
  temp_list <- gene_list #先將資料給另外一個變數
  print("開始突變")
  
  for(i in 1:length(gene_list)) {
    mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置
    rnd_mutation_rate <- runif(n = 1, min = 0, max = 1) #產生亂數
    print(paste("亂數突變率:", rnd_mutation_rate)) #顯示亂數的突變率
    
    if(rnd_mutation_rate <= mutation_rate){
      mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'種類') #取得基因中要被突變的染色體商品種類
      mutation_list <- soure_data[soure_data$'種類'==mutation_category,] #取得原始資料中符合要被突變的商品種類
      temp_good_data <- good_data[good_data$'種類'==mutation_category, ] #取得資料集該種類的資料
      
      repeat {
        #重複篩選商品, 直到沒有與原本基因相同的商品
        rnd_mutation_value <- sample(nrow(mutation_list), 1) #隨機取得該種類的商品
        
        comput_weight_list <- temp_list[[i]][[1]] #暫時將處理中的資料給另一變數
        comput_weight_list[mutation_index,] <- mutation_list[rnd_mutation_value,] #將取得到突變的位置進行突變
        sum_weight <- sum(comput_weight_list$'重量') #計算突變後總重量
        
        if(temp_list[[i]][[1]][mutation_index,]$'產品代號'!=mutation_list[rnd_mutation_value,]$'產品代號' && sum_weight < limit_weight && dim(temp_good_data)[1]!=1){
          #當商品兩者不同時, 並突變後的重量不可大於最大重量限制, 且該突變位置的商品種類數量不等於1, 則跳出, 表示已拿到非重複的商品, 且總重量沒有超過限制, 及該商品種類在剔除品牌後數量不等於1 
          break
        }
        print(paste("商品重複且重量超過:", sum_weight, "<", limit_weight))
        mutation_index <- as.numeric(sample(get_chrom_length, 1)) #隨機取得要突變的位置
        mutation_category <- as.character(temp_list[[i]][[1]][mutation_index,]$'種類') #取得基因中要被突變的染色體商品種類
        mutation_list <- soure_data[soure_data$'種類'==mutation_category,] #取得原始資料中符合要被突變的商品種類
        temp_good_data <- good_data[good_data$'種類'==mutation_category, ] #取得資料集該種類的資料
      }
      temp_list[[i]][[1]][mutation_index,] <- mutation_list[rnd_mutation_value,] #將商品進行變異
      temp_list[[i]]$'chromosome'[mutation_index] <- as.character(mutation_list[rnd_mutation_value,]$'產品代號') #將基因進行變異
      temp_list[[i]]$'totalWeight' <- sum_weight
      
      print("完成突變!")
    } else {
      print(paste(rnd_mutation_rate, ">=", mutation_rate)) #顯示目前突變率
    }
    print(paste("========第", i, "次========"))
  }
  return(temp_list)
}


#此代與下代合併的方法, 並挑選出新的族群(採用菁英政策)
new_population <- function(first_gene, second_gene, elite_values, pop_amount) {
  #first_gene: 此代基因
  #second_gene: 下一代基因
  #elite_values: 菁英數量
  #pop_amount: 族群大小
  
  new_pop <- list()
  new_pop <- first_gene #將此代基因放入新的變數
  new_pop <- append(new_pop, second_gene) #將下代基因加入變數
  
  for (i in 1:length(new_pop)) {
    #產生菁英狀態
    new_pop[[i]]["elite"] <- 0 #將菁英狀態皆設定為0, 0表示不是菁英值; 1表示被挑選為菁英值
  }
  
  result_pop <- list() #初始最終結果族群清單
  
  last_list <- head(order(unlist(lapply(new_pop, function(x) x$totalFit)), decreasing = FALSE), pop_amount) #取得popAmount(數量)的族群
  for (z in 1:length(elite_values)) {
    #將最好的適應函數設定為精英值, 並放入新的群組
    new_pop[[last_list[z]]]$'elite' <- 1
    result_pop[[z]] <- new_pop[[last_list[z]]] #將菁英的基因放入新的族群中
    print(paste("菁英編碼:", unlist(result_pop[[z]]$'chromosome')))
    print(paste("菁英的總體適應函數:", result_pop[[z]]$'totalFit'))
  }
  
  for (k in (length(elite_values)+1):pop_amount) {
    #將剩下的基因加入到族群
    result_pop[[k]] <- new_pop[[last_list[k]]]
  }
  
  return(result_pop)
}


#----暫存區----




#----執行----
goodData <- preference_match(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, user_preference = userPreference)

#剔除掉不想要的品牌
#goodData <- except_brand(good_data = goodData, except_brand_list = exceptBrandList)

#葷素的方法
goodData <- diet_select(good_data = goodData, diet_habit_list = dietHabit)

#產生初始口(遵照popAmount數量)
geneList <- list()
for (i in 1:popAmount) {
  gene <- initial_pop(good_data = goodData, require_goods = requiredList, non_require_goods = nonRequiredList, non_require_values = nonRequiredValues, limit_weight = maxWeight)
  geneList[[i]] <- list(gene)
}

#編碼染色體
geneList <- create_chromosome(gene_list = geneList)

#計算每個基因總重量
geneList <- total_weight(gene_list = geneList)

# 計算總體積
# totalGene <- list()
# totalGene <- total_Volume(gene_list = geneList)

#計算偏好適應度(目前僅計算總偏好值)
fitnessPreference <- list()
fitnessPreference <- fitness_preference(gene_list = geneList)

#計算體積適應度
fitnessVolumeAfter <- list()
fitnessVolumeAfter <- fitness_volume(gene_list = fitnessPreference, bin_volume = maxVolume, volume_alpha = alpha)

#計算價格適應度
fitnessPriceAfter <- list()
fitnessPriceAfter <- fitness_price(gene_list = fitnessVolumeAfter, limit_price = maxPrice)

#計算總體適應度
fitnessTotalAfter <- list()
fitnessTotalAfter <- fitness_total(gene_list = fitnessPriceAfter)

#開始進行交配
crossAfter <- list()
crossAfter <- cross_over(gene_list = fitnessTotalAfter, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate, limit_weight = maxWeight)

#開始進行突變
mutationAfter <- list()
mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, soure_data = goodData, limit_weight = maxWeight)

#計算新一代基因的價格及體積的適應函數
mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume, volume_alpha = alpha) 
mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
mutationAfter <- fitness_total(gene_list = mutationAfter)


#到此位置已經有第一代與第二代的基因
#初始基因為fitnessPriceAfter
#第二代基因為mutationAfter

#合併此代與下一代基因, 並採用菁英政策和產出新的族群
newPopulation <- new_population(first_gene = fitnessTotalAfter, second_gene = mutationAfter, elite_values = eliteValues, pop_amount = popAmount)


gen_values_best <- vector() #紀錄最好的基因總體適應函數
gen_values_loss <- vector() #紀錄最差的基因總體適應函數
for (i in 1:maxGen) {
  #開始進行基因演算
  crossAfter <- list()
  crossAfter <- cross_over(gene_list = newPopulation, require_goods = requiredList, non_require_values = nonRequiredValues, cross_rate = crossRate, limit_weight = maxWeight)
  
  mutationAfter <- list()
  mutationAfter <- mutation_FN(good_data = goodData, gene_list = crossAfter, mutation_rate = mutationRate, require_goods = requiredList, non_require_values = nonRequiredValues, soure_data = goodData, limit_weight = maxWeight)
  
  mutationAfter <- fitness_volume(gene_list = mutationAfter, bin_volume = maxVolume, volume_alpha = alpha) 
  mutationAfter <- fitness_price(gene_list = mutationAfter, limit_price = maxPrice)
  mutationAfter <- fitness_total(gene_list = mutationAfter)
  
  newPopulation <- new_population(first_gene = newPopulation, second_gene = mutationAfter, elite_values = eliteValues, pop_amount = popAmount)
  
  gen_values_best[i] <- newPopulation[[1]]$totalFit
  gen_values_loss[i] <- newPopulation[[20]]$totalFit
  print(paste("============第", i, "代============"))
} 

plot(gen_values_best, main = "裝箱演算法", xlab = "世代次數", ylab = "總體適應函數") #畫圖來顯示總體適應函數的起伏

#resultDF<- newPopulation[[1]][[1]][,-11] #去除selected的欄位
#write.csv(resultDF, file = "outputList.csv", row.names = FALSE) #輸出最佳的裝箱清單
